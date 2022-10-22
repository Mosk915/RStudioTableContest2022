step12_table_proxy <- dataTableProxy(outputId = "step12_table")

observeEvent(input$average_method_tutorial, ignoreInit = TRUE, {
  
  factors <- state_a_triangle_summ[state_a_triangle_summ$AVG_METHOD == input$average_method_tutorial,]
  losses_filled <- fill_triangle(data_triangle = state_a_triangle, factors = factors)
  
  replaceData(proxy = step12_table_proxy,
              data = losses_filled,
              rownames = FALSE)
  
})
