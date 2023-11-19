library(shiny)
library(tidyverse)

## Define UI
ui <- fluidPage(

  ## Title and descriptive text
  titlePanel("🦷 Tooth Growth App 🦷"),
  h4("Use this app to explore the effect of vitamin c on
     tooth growth in guinea pigs"),

  ## Organize panels
  sidebarLayout(

    ## Sidebar to left of screen
    sidebarPanel(

      ## Slide to control range of tooth lengths
      sliderInput("id_slider",
                  "Select a tooth length range:",
                  min = min(ToothGrowth$len),
                  max = max(ToothGrowth$len),
                  value = c(10, 30),
                  post = "mm"),

      ## Feature 1: Checkbox Group to select levels displayed in supplements
      checkboxGroupInput("supp_levels",
                         "Select at least one supplement to display:",
                         choices = c("Orange Juice" = "OJ",
                                     "Ascorbi Acid" = "VC"),
                          selected = c("Orange Juice" = "OJ",
                                       "Ascorbi Acid" = "VC"))
    ),

    ## Main panel: boxplot and data table
    mainPanel(
      plotOutput("id_boxplot"),

      ## Feature 2: Use DT package to produce interactive table
      DT::dataTableOutput("id_table")
    )
  )
)


## Define server
server <- function(input, output) {
  #observe(print(input$id_slider)) ## Uncomment to test how slider works
  #observe(print(input$supp_levels)) ## Uncomment to test how checkbox works

  ## Filter data from users choice
  ToothGrowth_filtered <- reactive({
    ToothGrowth %>%
      filter(len < input$id_slider[2],
             len > input$id_slider[1],
             supp %in% input$supp_levels)
  })

  ## Create boxplot from filtered data
  output$id_boxplot <- renderPlot({
    ToothGrowth_filtered() %>%
      ggplot(aes(x=factor(dose), y=len)) +
        geom_boxplot() +
        facet_grid(.~supp)
  })

  ## Rander interactive data table
  output$id_table <- DT::renderDataTable({
    ToothGrowth_filtered()
  })
}


## Create shiny app
shinyApp(ui = ui, server = server)
