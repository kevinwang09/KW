shinyUI(fluidPage(
  titlePanel("Simple data explorer"),
  plotOutput("plot1", brush = "plot_brush", height = 250),
  DT::dataTableOutput("info")
  )
)
