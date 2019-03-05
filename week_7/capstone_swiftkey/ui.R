suppressMessages(library(shiny))

shinyUI(pageWithSidebar(
        headerPanel("Data Science Capstone Project"),
        sidebarPanel(
                h3("User Input"),
                br(),
                
                strong(""),
                textInput("impText", "Type few words of a sentence below:", value = "type here...")
                
        ),
        mainPanel(tabsetPanel(
                tabPanel(
                        "Predictions",
                        
                        h4('The text as interpreted:'),
                        verbatimTextOutput("clnText"),
                        
                        h4('The word predicted based on the phrase provided:'),
                        verbatimTextOutput("nxtWord")
                        
                )
                    
                )
        ))
)
