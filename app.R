library(shiny)
library(bslib)
library(ggplot2)

ui <- page_sidebar(
  title = "Shiny for R",
  sidebar = sidebar(
    sliderInput("sample", "Sample Size", 0, 1, value = 0.1, ticks = FALSE),
    checkboxInput("log", "Log Scale")
  ),
  class = "bslib-page-dashboard",
  card(
    card_header(textOutput("first_taxi_id")),
    plotOutput("tip_plot"),
  ),
  card(
    plotOutput("amount_histogram")
  )
)

server <- function(input, output, session) {
  dat <- reactive({
    read.csv("nyc-taxi.csv")
  })

  sampled_dat <- reactive({
    dplyr::slice_sample(dat(), prop = input$sample)
  })

  output$first_taxi_id <- renderText({
    paste("First taxi ID:", sampled_dat()$taxi_id[1])
  })

  output$tip_plot <- renderPlot({
    plot <- ggplot(sampled_dat(), aes(tip_amount, total_amount)) +
      geom_point(size = 3) +
      theme_bw(20)
    if (input$log) {
      plot <- plot +
        scale_x_log10() +
        scale_y_log10()
    }
    plot
  })

  output$amount_histogram <- renderPlot({
    plot <- ggplot(sampled_dat(), aes(x = total_amount)) +
      geom_histogram(binwidth = 5) +
      theme_bw(20)
    plot
  })
}

shinyApp(ui, server)
