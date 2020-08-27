shinyServer(function(input, output, session) {
  output$plot1 <- renderPlot({
    uploaded_plot
  })

  # output$info <- renderPrint({
  output$info <- DT::renderDataTable({
    brushedPoints(df = uploaded_plot$data,
                  brush = input$plot_brush)
  })
})
