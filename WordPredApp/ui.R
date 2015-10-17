library(shiny)


shinyUI(navbarPage(fluid = FALSE, em("Next Word Prediction App"),
    tabPanel("Application",
        titlePanel(title = strong(em("What is the next word in a sentence ?")), windowTitle = "Prediction of the next word"),
        sidebarLayout(
            sidebarPanel(
                h4("Sentence to be predicted"),
                textInput("sentence", em("Enter the beginning of a sentence"), value = "Just do"),
                h4("Number of prediction choices"),
                sliderInput("numwords", label = em("Select the number of prediction choices to be displayed"), min = 1, max = 5, value = 1),
                p(),
                p(),
                submitButton("Next word !")
                ),
            mainPanel(
                h4("Sentence entered :"),
                textOutput("sentence"),
                h4("Sentence completion predicted (sorted by decreasing likelihood) :"),
                #textOutput("sentence.proc"),
                strong(textOutput("prediction")),
                conditionalPanel(
                    condition = "input.numwords > 1",
                    h4("Plot of completion prediction probabilities :"),
                    plotOutput("probs.plot")
                )
            )
        )
    ),
    tabPanel("Documentation",
            titlePanel(title = strong(em("What is the next word in a sentence ?")), windowTitle = "Prediction of the next word"),
            h4("Synopsis"),
            p("This application predicts the next word to complete a sentence. This prediction is based on the most likely word to follow the beginning of a sentence computed by smoothing the n-grams MLE probabilities with the Simple Good-Turing and Katz Back-off algorithms."),
            h4("How it works"),
            p("Enter the beginning of a sentence in the left-hand panel and then click on the 'Next Word !' button. You may enter your sentence in lowercase or not, and also with punctuation or not."),
            p("You may also select the number of prediction choices to display; if you select more than one prediction, a bar plot will be displayed which shows the probabilities of the different choices."),
            p(" Please note that the first time the application is launched it may take some time for the application to load the different datasets. A progress bar is displayed to show the user what file the application is loading in the background."),
            h4("Results"),
            p("The choices for the predicted word completing the sentence you entered will be displayed in decreasing likelihood order in the main panel as will be a reminder of the sentence entered."),
            p("Below you will also find the bar plot of the probabilities of the different completion predictions (if you selected to predict multiple choices).")
        )
    )
)