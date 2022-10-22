step7_table_proxy <- dataTableProxy(outputId = "step7_table")

observeEvent(input$heatMap_tutorial, ignoreInit = TRUE, {
  
  state_a_triangle_dev$COLOR_USE <- input$heatMap_tutorial
  
  replaceData(proxy = step7_table_proxy,
              data = state_a_triangle_dev,
              rownames = FALSE,
              clearSelection = FALSE)
  
})
