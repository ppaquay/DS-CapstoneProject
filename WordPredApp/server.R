library(shiny)

source("prediction.R", local = TRUE)


shinyServer(
    function(input, output) {
        
        withProgress(message = "Loading datasets :", value = 0, {
            incProgress(0.2, detail = "1-grams......")  
            load("onegrams-final.RData", .GlobalEnv)
            load("SGT-onegrams-final.RData", .GlobalEnv)
            incProgress(0.2, detail = "2-grams......")
            load("twograms-final.RData", .GlobalEnv)
            load("SGT-twograms-final.RData", .GlobalEnv)
            incProgress(0.2, detail = "3-grams......")
            load("threegrams-final.RData", .GlobalEnv)
            load("SGT-threegrams-final.RData", .GlobalEnv)
            incProgress(0.2, detail = "4-grams......")
            load("fourgrams-final.RData", .GlobalEnv)
            load("SGT-fourgrams-final.RData", .GlobalEnv)
            incProgress(0.2, detail = "vocabulary......")
            load("vocab-final.RData", .GlobalEnv)
        })
        
        output$sentence <- renderPrint({input$sentence})
        
        sentence.proc <- reactive({CleanSentence(input$sentence)})
        output$sentence.proc <- renderPrint({sentence.proc()})
        
        predictions <- reactive({PredictNextWordApprox(sentence.proc(), input$numwords)})
        output$prediction <- renderPrint({predictions()$Word})
        
        output$probs.plot <- renderPlot({
            preds <- as.data.frame(predictions())
            preds$Word <- reorder(preds$Word, -preds$Probas)
            p <- ggplot(preds, aes(y = Probas))
            p + geom_bar(aes(x = Word), stat = "identity", fill = "blue", colour = "black") + xlab("Word predicted") + ylab("Probability") + theme(axis.text.x=element_text(angle=0, hjust=1))
        })
    }
)
