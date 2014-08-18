library(shiny)
library(ggplot2)

emisN2O <- function(synt, manure, animal, quantity) {
      emis <- quantity * 0.01 * (44/28)
}

emisNH3 <- function(synt, manure, animal, quantity) {
      tman <- 0
      tsyn <- 0      
      if (synt & manure) {
            tman <- quantity / 2
            tsyn <- quantity /2
      }
      else if (synt) tsyn <- quantity
      else if (manure) tman <- quantity

      emisS <- tsyn * 0.081 * (17/14)
      
      if (animal == "Dairy Cattle") {emisM <- tman * 0.6* 0.55 * (17/14)}
      else if (animal == "Non-Dairy Cattle") {emisM <- tman * 0.6* 0.55 * (17/14)}
      else if (animal == "Swine") {emisM <- tman * 0.7* 0.35 * (17/14)}
      else {emisM <- tman * 0.5* 0.9 * (17/14)}
      
      emis <- emisS + emisM
}

emisNOX <- function(synt, manure, animal, quantity) {
      emis <- quantity * 0.026 * (30/14)
}

shinyServer(
      function(input, output) {
            
            output$anim <- renderText({input$animalInput})
            output$fertTonnes <- renderText({input$fertTonnesInput})
            
            ## N2O emission estimation
            output$emisN2O <- reactive({round(emisN2O(input$tSynth, input$tManur, input$animalInput, input$fertTonnesInput), 3)}) 
            ## NH3 emission estimation
            output$emisNH3 <- reactive({round(emisNH3(input$tSynth, input$tManur, input$animalInput, input$fertTonnesInput), 3)})      
            ## NOX emission estimation
            output$emisNOX <- reactive({round(emisNOX(input$tSynth, input$tManur, input$animalInput, input$fertTonnesInput), 3)})    
            
            output$emisPlot <- renderPlot({
                  emisionesN2O <- emisN2O(input$tSynth, input$tManur, input$animalInput, input$fertTonnesInput)
                  emisionesNH3 <- emisNH3(input$tSynth, input$tManur, input$animalInput, input$fertTonnesInput)
                  emisionesNOX <- emisNOX(input$tSynth, input$tManur, input$animalInput, input$fertTonnesInput)
                  emisiones <- data.frame(c("N2O", "NH3","NOX"), c(emisionesN2O, emisionesNH3, emisionesNOX))
                  names(emisiones) <- c("cont", "emis")
                  qplot(cont, emis, data=emisiones, geom="bar", stat="identity", fill=cont, ylab = "Emissions (t)", xlab = "gas")
            })            
})

