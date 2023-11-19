library(shiny)
library(tidyverse)


ui <- fluidPage(
  titlePanel("🦷 Tooth Growth App 🦷"),
  h4("Use this app to explore the effect of vitamin c on
     tooth growth in guinea pigs"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("id_slider",
                  "Select a tooth length range:",
                  min = min(ToothGrowth$len),
                  max = max(ToothGrowth$len),
                  value = c(10, 30),
                  post = "mm"),
      checkboxGroupInput("supp_levels",
                         "Select at least one supplement to display:",
                         choices = c("Orange Juice" = "OJ",
                                     "Ascorbi Acid" = "VC"),
                          selected = c("Orange Juice" = "OJ",
                                       "Ascorbi Acid" = "VC"))
    ),
    mainPanel(
      plotOutput("id_boxplot"),
      tableOutput("id_table")
    )
  )
)

server <- function(input, output) {
  #observe(print(input$id_slider)) ## Uncomment to test how slider works
  observe(print(input$supp_levels))

  ToothGrowth_filtered <- reactive({
    ToothGrowth %>%
      filter(len < input$id_slider[2],
             len > input$id_slider[1],
             supp %in% input$supp_levels)
  })

  output$id_boxplot <- renderPlot({
    ToothGrowth_filtered() %>%
      ggplot(aes(x=factor(dose), y=len)) +
        geom_boxplot() +
        facet_grid(.~supp)
  })
  output$id_table <- renderTable({
    ToothGrowth_filtered()
  })
}


shinyApp(ui = ui, server = server)
