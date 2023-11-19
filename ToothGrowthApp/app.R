#install.packages("hrbrthemes")
#install.packages("shinythemes")
library(shiny)
library(tidyverse)
library(hrbrthemes)

## Define UI
ui <- fluidPage(

  ## Feature 1: Theme selector allows for user to choose an theme most desirable for them.
  shinythemes::themeSelector(),

  ## Title and descriptive text
  titlePanel("🦷 Tooth Growth App 🦷"),
  h4("Use this app to explore the effect of vitamin c on
     tooth growth in guinea pigs."),

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

      ## Feature 2: Checkbox Group to select levels of supplements displayed.
      ## Allows users to narrow focus on a supplement of their choice.
      checkboxGroupInput("supp_levels",
                         "Select at least one supplement to display:",
                         choices = c("Orange Juice" = "OJ",
                                     "Ascorbi Acid" = "VC"),
                          selected = c("Orange Juice" = "OJ",
                                       "Ascorbi Acid" = "VC")),

      #img(src='figures/image.jpg', align = "right")
    ),

    ## Main panel: boxplot and data table
    mainPanel(

      ## Feature 3: Tabs presents a far more neat interface.
      ## Reduces clutter so the user may digest only the information they would like to see.
      tabsetPanel(
        type = "tabs",
        tabPanel("Plot", plotOutput("id_boxplot")),
        ## Feature 4: Summary Table provides a numerical summary of the data after filtering from slider and checkbox is applied.
        tabPanel("Summary", tableOutput("id_summary")),
        ## Feature 5: Use DT package to produce interactive table.
        ## Results in a more organized display given the users preference.
        tabPanel("Data", DT::dataTableOutput("id_table"))
      )


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
    if (nrow(ToothGrowth_filtered()) > 0) {
      ToothGrowth_filtered() %>%
        ggplot(aes(x=factor(dose), y=len, fill = factor(dose))) +
        geom_boxplot(alpha=0.4) +
        facet_grid(.~supp) +
        theme_ipsum() +
        theme(legend.position="none") +
        xlab("Dose (mg)") +
        ylab("Tooth Length (mm)")
    }
  })

  ## Render interactive data table
  output$id_table <- DT::renderDataTable({
    ToothGrowth_filtered()
  })

  ## Render data summary
  output$id_summary <- renderTable({
    ToothGrowth_filtered() %>%
      group_by(supp) %>%
      summarise(count = n(),
                across(c(len, dose),
                       c(mean = mean, sd = sd)))
  })
}


## Run shiny app
shinyApp(ui = ui, server = server)
