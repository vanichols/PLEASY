#--this turns on renv for this project
# renv::init()

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes)
library(naniar)

#--this gets the most recent version of it
# remotes::install_github("vanichols/PesticideLoadIndex4Dummies")
library(PesticideLoadIndex4Dummies)

#--this will update all of the packages associated with this project
#renv:update()

#--run this when changing code and/or updating packages used
#renv::snapshot()

#--this one is a little more unclear to me, when to use it:
#renv::restore()


#used shiny app assistant: https://shiny.posit.co/blog/posts/shiny-assistant/

# data --------------------------------------------------------------------

# Use PLI dataset from package
the_data <- 
  as_tibble(pli_ppdb3plis) %>% 
  arrange(substance) %>% 
  select(id, substance, env_load, eco_load, hh_load) %>% 
  pivot_longer(env_load:hh_load) %>% 
  mutate(value2 = case_when(
    is.na(value) ~ "Unknown",
    value < 0.001 ~ "<0.001",
    TRUE ~ as.character(round(value, 3))
  ))

the_data %>% 
  filter(name == "hh_load") %>% 
  arrange(-value)

# ui ----------------------------------------------------------------------

ui <- page_sidebar(
  title = "PLEASY - Pesticide load estimations in an easily accessibly summary",
  sidebar = sidebar(
    selectizeInput(
      inputId = "selected_ai",
      label = "Select active ingredient:",
      choices = unique(the_data$substance),
      selected = "glyphosate"
    ),
    br(),
    p("Select an active ingredient to see the potential pesticide load in the three categories of impact. You can begin typing and it will show you available options.
      As an example for exploration, carbofuran has a very high ecological toxicity, and aldicarb has a very high human health load.")
  ),
  
  layout_columns(
    card(
      card_header("Environmental Persistence"),
      plotOutput("env_plot")
    ),
    card(
      card_header("Ecological toxicity"),
      plotOutput("eco_plot")
    ),
    card(
      card_header("Human health"),
      plotOutput("hum_plot")
    ),
    col_widths = c(4, 4, 4)
  )
)


# server ------------------------------------------------------------------

fig_theme <- 
  theme(axis.text.x = element_blank(), 
        axis.text = element_text(size = rel(1.25)),
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(hjust = 0.5, size = rel(2)))


#--practice, what if there are NAs
tst1 <- 
  the_data %>% 
  filter(substance == "glyphosine")

Make_Plot <- function(f.alldat = the_data, f.filt = tst1, f.imp = "env_load"){
  
  plot_data <- 
    f.alldat %>% 
    filter(name == f.imp)
  
  plot_filtdata <- 
    f.filt %>% 
    filter(name == f.imp)
  
  if(!is.na(plot_filtdata$value))
  {
    
    f1 <- 
      plot_data %>% 
      filter(!is.na(value)) %>% #--exclude NAs from the fig
      ggplot(aes(reorder(id, value), value)) + 
      geom_point(data = plot_filtdata, aes(id, value), fill = "gold", shape = 24, size = 8) +
      geom_point() +
      geom_label(data = plot_filtdata, aes(1, 1, label = substance, hjust = 0), size = 10) +
      geom_label(data = plot_filtdata, aes(1, 0.9, label = value2, hjust = 0), size = 10) +
      labs(x = "All available active ingredients\nordered by load",
           y = "Load") +
      scale_size_manual(values = c(1, 5)) +
      theme_minimal() +
      fig_theme
  } else {
    
    f1 <- 
      plot_data %>% 
      filter(!is.na(value)) %>% #--exclude NAs from the fig
      ggplot(aes(reorder(id, value), value)) + 
      geom_point() +
      geom_label(data = plot_filtdata, aes(1, 1, label = substance, hjust = 0), size = 10) +
      geom_label(data = plot_filtdata, aes(1, 0.9, label = "Unknown", hjust = 0), size = 10) +
      labs(x = "All available active ingredients\nordered by load",
           y = "Load") +
      scale_size_manual(values = c(1, 5)) +
      theme_minimal() +
      fig_theme
    }
    
  return(f1)
}
 
#--test function
tst1 <- 
  the_data %>% 
  filter(id == 225)

Make_Plot(f.alldat = the_data, f.filt = tst1, f.imp = "env_load")

server <- function(input, output, session) {
  
  # Reactive data based on selected category
  filtered_data <- reactive({
    the_data %>%
      filter(substance == input$selected_ai)
  })
  
  # Figure 1: Environmental persistence
  output$env_plot <- renderPlot({
    tst <- filtered_data()
    Make_Plot(f.alldat = the_data, 
              f.filt = tst, 
              f.imp = "env_load")
    
  })
  
  # Figure 2: Ecological toxicity
  output$eco_plot <- renderPlot({
    tst <- filtered_data()
    Make_Plot(f.alldat = the_data, 
              f.filt = tst, 
              f.imp = "eco_load")
    
  })
  
  # Figure 3: Human health
  output$hum_plot <- renderPlot({
    tst <- filtered_data()
    Make_Plot(f.alldat = the_data, 
              f.filt = tst, 
              f.imp = "hh_load")
    
  })
  
}

shinyApp(ui = ui, server = server)
