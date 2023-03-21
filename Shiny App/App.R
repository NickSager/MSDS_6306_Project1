library(shiny)
library(tidyverse)

beers_breweries <- read.csv("beers_breweries.csv")


# Define UI for random distribution app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Beers Case Study Explorer"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Select the metric of interest ----
      radioButtons("metric", "Metric of Interest:",
                   c("Alcohol by Volume (abv)" = "ABV",
                     "Bitterness (IBU)" = "IBU")),

      # br() element to introduce extra vertical spacing ----
      br(),

      # Input: Select the desired plot ----
      radioButtons("plot", "Distribution type:",
                   c("Histogram" = "geom_histogram",
                     "Boxplot" = "geom_boxplot")),

      # br() element to introduce extra vertical spacing ----
      br(),

      # Input: Show Linear Regression ----
      radioButtons("lm", "Show Linear Regression:",
                   c("Yes" = TRUE,
                     "No" = FALSE)),

      # br() element to introduce extra vertical spacing ----
      br(),
      
      # Input: Slider for the number of observations to generate ----
      textInput("state", "Filter by State (e.g., UT)"),

      #Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Individual Distribution", plotOutput("plot")),
                  tabPanel("Scatter Plot", plotOutput("scatter"))
      )

    )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {  
  # Reactive expression to generate the requested filter ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
  beers <- reactive({
      ifelse(nchar(input$state == 0),
          beers_breweries,
          beers_breweries %>% filter(state == input$state)
      )
  })
#   beers <- beers_breweries

  # Generate a plot of the data ----
  # Also uses the inputs to build the plot label. Note that the
  # dependencies on the inputs and the data reactive expression are
  # both tracked, and all expressions are called in the sequence
  # implied by the dependency graph.
  output$plot <- renderPlot({
    if(input$plot == "geom_histogram") {
      metric <- input$metric
      beers %>%
        ggplot(aes(x = metric)) +
        geom_histogram(bins = input$bins) +
        labs(
            title = paste("Distribution of ", metric, sep = ""),
            x = "ABV",
            y = "Count"
        )
    } else {
      metric <- input$metric
      beers %>%
        ggplot(aes(x = metric)) +
        geom_boxplot()+
        labs(
            title = paste("Distribution of ", metric, sep = ""),
            x = "ABV",
            y = "Count"
        )
    }   
  })

  # Generate a scatterplot of ABV vs IBU ----
  output$scatter <- renderPlot({
    if(input$lm == TRUE) {
      beers %>%
        ggplot(aes(x = ABV, y = IBU, color = Ale)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE, color = "black") +
        labs(
            x = "Alcohol Content (ABV)",
            y = "Bitterness (IBU)",
            title = "Alcohol Content vs. Bitterness"
        ) +
        scale_x_continuous(labels = scales::percent) +
        theme_minimal()
    } else {
      beers %>%
        ggplot(aes(x = ABV, y = IBU, color = Ale)) +
        geom_point() +
        labs(
            x = "Alcohol Content (ABV)",
            y = "Bitterness (IBU)",
            title = "Alcohol Content vs. Bitterness"
        ) +
        scale_x_continuous(labels = scales::percent) +
        theme_minimal()
    }
    # beers %>%
    #     ggplot(aes(x = ABV, y = IBU, color = Ale)) +
    #     geom_point() +
    #     geom_smooth(method = "lm", se = FALSE, color = "black") +
    #     labs(
    #         x = "Alcohol Content (ABV)",
    #         y = "Bitterness (IBU)",
    #         title = "Alcohol Content vs. Bitterness"
    #     ) +
    #     scale_x_continuous(labels = scales::percent) +
    #     theme_minimal()
  })
}

# Create Shiny app ----
shinyApp(ui, server)
