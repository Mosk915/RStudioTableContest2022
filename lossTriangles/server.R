function(input, output, session) {
  
  values <- reactiveValues()
  
  # Development Triangles
  
  # DT Proxies
  triangle_proxy <- dataTableProxy(outputId = "devTriangleFormatted")
  averages_proxy <- dataTableProxy(outputId = "averages")
  triangle_filled_proxy <- dataTableProxy(outputId = "lossTriangleFilled")
  
  output$rawData <- renderDT({
    DTLossData(state_a_losses)
  })
  
  output$lossTriangle <- renderDT({
    DTLossTriangle(state_a_triangle)
  })
  
  output$devTriangle <- renderDT({
    DTDevTriangle(state_a_triangle_dev)
  })
  
  output$devTriangleFormatted <- renderDT({
    
    values$data <- state_a_triangle_dev
    values$losses <- state_a_triangle
    DTDevTriangleFormatted(isolate(values$data))
    
  })
  
  output$averages <- renderDT({
    
    values$averages <- state_a_triangle_summ
    DTAverages(isolate(values$averages))
    
  })
  
  output$lossTriangleFilled <- renderDT({
    
    values$factors <- state_a_triangle_summ[state_a_triangle_summ$AVG_METHOD == "Two Year",]
    values$losses_filled <- fill_triangle(data_triangle = state_a_triangle, factors = isolate(values$factors))
    DTLossTriangleFilled(isolate(values$losses_filled))
    
  })
  
  observeEvent(input$state, ignoreInit = TRUE, {
    # when the state is changed, update all the reactive values appropriately
    
    if(input$state == "State A") {
      values$data <- state_a_triangle_dev
      values$losses <- state_a_triangle
      values$averages <- state_a_triangle_summ
    } else {
      values$data <- state_b_triangle_dev
      values$losses <- state_b_triangle
      values$averages <- state_b_triangle_summ
    }
    
    # make sure the current heat map selection persists
    values$data$COLOR_USE <- input$heatMap
    
    # reset any current cell selection
    values$clearSelection <- "all"
    
  })
  
  observeEvent(input$heatMap, ignoreInit = TRUE, {
    # when the heat map is toggled, update the last column in the values$data data frame appropriately
    
    values$data$COLOR_USE <- input$heatMap
    
    # do not reset the current cell selection
    values$clearSelection <- "none"
    
  })
  
  observeEvent(values$data, {
    # when the data is updated replace the data in the DT tables with the new data
    
    replaceData(proxy = triangle_proxy,
                data = values$data,
                rownames = FALSE,
                clearSelection = values$clearSelection)
    
    replaceData(proxy = averages_proxy,
                data = values$averages,
                rownames = FALSE,
                clearSelection = values$clearSelection)
    
  })
  
  observeEvent(input$devTriangleFormatted_cells_selected, {
    # when a cell is selected, output the two loss amounts that go into the development factor
    
    if(length(input$devTriangleFormatted_cells_selected) > 0) {
      
      # get the row and column index of the selected cell
      row <- input$devTriangleFormatted_cells_selected[1]
      col <- input$devTriangleFormatted_cells_selected[2]
      
      # determine the previous and current report numbers of the cell selected
      rpt_prev <- col
      rpt_curr <- col + 1
      
      # determine the loss amounts that make up the development factor
      losses_prev <- values$losses[row, col + 1]
      losses_curr <- values$losses[row, col + 2]
      
      output$factorDetail <- renderText(paste0("Selected Factor: ", round(losses_curr / losses_prev, 3),
                                               "\n",
                                               "Total Claim Payments @", ordinal(rpt_prev), ": ", format(losses_prev, big.mark = ","),
                                               "\n",
                                               "Total Claim Payments @", ordinal(rpt_curr), ": ", format(losses_curr, big.mark = ",")))
      
    } else {
      
      output$factorDetail <- renderText("Select a factor in the triangle to see loss details.")
      
    }
    
  })
  
  observeEvent(c(input$average_method, values$losses), {
    # when the average method is changed, recalculate the estimated loss values using the new averages
    # and replace the data in the DT table with the new data
    
    values$factors <- values$averages[values$averages$AVG_METHOD == input$average_method,]
    values$losses_filled <- fill_triangle(data_triangle = values$losses, factors = values$factors)
    
    replaceData(proxy = triangle_filled_proxy,
                data = values$losses_filled,
                rownames = FALSE)
    
  })
  
  # Tutorial
  
  output$step1 <- renderPrint({
    source("www/tutorial/step1.R", local = TRUE)
  })
  
  output$step2 <- renderPrint({
    source("www/tutorial/step2.R", local = TRUE)
  })
  
  output$step3 <- renderPrint({
    source("www/tutorial/step3.R", local = TRUE)
  })
  
  output$step4 <- renderPrint({
    invisible(capture.output(source("www/tutorial/step3.R", local = TRUE)))
    source("www/tutorial/step4.R", local = TRUE)
  })
  
  output$step5 <- renderPrint({
    source("www/tutorial/step5.R", local = TRUE)
  })
  
  output$step5_table <- renderDT({
    source("www/tutorial/step5.R", local = TRUE)
    table
  })
  
  output$step6 <- renderPrint({
    source("www/tutorial/step6.R", local = TRUE)
  })
  
  output$step6_table <- renderDT({
    source("www/tutorial/step6.R", local = TRUE)
    table
  })
  
  output$step7_table <- renderDT({
    source("www/tutorial/step6.R", local = TRUE)
    table
  })
  
  source("www/tutorial/step7_server.R", local = TRUE)
  
  output$step8 <- renderPrint({
    source("www/tutorial/step8.R", local = TRUE)
  })
  
  output$step9 <- renderPrint({
    source("www/tutorial/step9.R", local = TRUE)
  })
  
  output$step10 <- renderPrint({
    invisible(capture.output(source("www/tutorial/step9.R", local = TRUE)))
    source("www/tutorial/step10.R", local = TRUE)
  })
  
  output$step10_table <- renderDT({
    invisible(capture.output(source("www/tutorial/step9.R", local = TRUE)))
    source("www/tutorial/step10.R", local = TRUE)
    table
  })
  
  output$step11 <- renderPrint({
    invisible(capture.output(source("www/tutorial/step9.R", local = TRUE)))
    source("www/tutorial/step11.R", local = TRUE)
  })
  
  output$step11_table <- renderDT({
    invisible(capture.output(source("www/tutorial/step9.R", local = TRUE)))
    source("www/tutorial/step11.R", local = TRUE)
    table
  })
  
  output$step12_table <- renderDT({
    invisible(capture.output(source("www/tutorial/step9.R", local = TRUE)))
    source("www/tutorial/step11.R", local = TRUE)
    table
  })
  
  source("www/tutorial/step12_server.R", local = TRUE)
  
}