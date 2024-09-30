pacman::p_load(
  shiny, sf, tmap, tidyverse,
  sfdep, shinydashboard, shinythemes
)
# Load and join hunan county data
# from week 6 data
hunan <- st_read(
  dsn = "data/geospatial",
  layer = "Hunan"
)
data <- read_csv("data/aspatial/Hunan_2012.csv")
hunan_profile <- left_join(hunan, data)

# User Interface
# Controls the appearance and layout of the app
ui <- fluidPage(
  titlePanel("In-Class Exercise 7: Choropleth Mapping"),
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "variable",
        label = "Mapping variable",
        choices = list(
          "GDP" = "GDP",
          "GDP per capita" = "GDPPC",
          "Gross Industry Output" = "GIO",
          "Output Value of Agriculture" = "OVA",
          "Output Value of Service" = "OVS"
        ),
        selected = "GDPPC"
      ),
      sliderInput(
        inputId = "classes",
        label = "Number of classes",
        min = 5,
        max = 10,
        value = c(6)
      )
    ),
    mainPanel(
      tmapOutput(
        "mapPlot", width = "100%", height = 580
      )
    )
  )
)

# Server function
# Contains instructions needed to build the app
server <- function(input, output) {

}

# shinyApp()
# Creates the Shiny app object
shinyApp(ui = ui, server = server)
