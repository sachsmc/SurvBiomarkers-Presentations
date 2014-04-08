shinyServer(function(input, output) {
  
  
  dataset <- reactive({
    
    meandiff <- as.numeric(input$quality)
    data.frame(D0 = rnorm(250, mean = 0), D1 = rnorm(250, mean = meandiff))
    
  })
  
  output$density_plot <- renderPlot({
    
    plot(density(dataset()$D0), xlab = "Biomarker Value", ylab = "Density", main = "", col = "red", lwd = 2, 
         xlim = c(min(dataset()), max(dataset())), ylim = c(0, .5))
    lines(density(dataset()$D1), col = "blue", lwd = 2)
    abline(v = input$thresh, lty = 2)
    
  })
  
  output$roc_plot <- renderPlot({
    
    cuts <- sort(unique(c(dataset()$D0, dataset()$D1, input$thres)))
    tpr <- sapply(cuts, function(x) mean(dataset()$D1 > x))
    fpr <- sapply(cuts, function(x) mean(dataset()$D0 > x))
    
    tpr.thresh <- mean(dataset()$D1 > input$thresh)
    fpr.thresh <- mean(dataset()$D0 > input$thresh)
    
    plot(tpr ~ fpr, type = 'l', xlab = "1 - Specificity", ylab = "Sensitivity", xlim = c(0, 1), ylim = c(0,1), lwd = 2)
    lines(tpr.thresh ~ fpr.thresh, type = 'p', col = "green",  pch = 20, cex = 2)
    text(fpr.thresh + .05, tpr.thresh, paste("c =", round(input$thresh, 1)), pos = 1)
    abline(0, 1, lty = 2)
    
  })
  
  output$mary <- renderUI({
    
    sens.thresh <- mean(dataset()$D1 > input$thresh)
    spec.thresh <- mean(dataset()$D0 < input$thresh)
    auc <- mean(outer(dataset()$D1, dataset()$D0, FUN = ">"))
    
    h5(list(sprintf("Threshold: %.2f", input$thresh), br(),
            sprintf("Sensitivity: %.2f", sens.thresh), br(),
            sprintf("Specificity: %.2f", spec.thresh), br(),
            sprintf("AUC: %.2f", auc)))
    
  })
  
})