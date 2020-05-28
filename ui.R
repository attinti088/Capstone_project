#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyBS) 

library(shinythemes)
# Define UI for application that draws a histogram
shinyUI( navbarPage(
    
    "NLP Data Science SwiftKey Next Word Prediction",
    
    theme = shinytheme("flatly"),
    
    tabPanel(
        
        "Prediction",
        
        
        pageWithSidebar(
            
            headerPanel("Next Word Text Prediction"),
            
            sidebarPanel(
                
                textInput("text", label = h3("Text input"), 
                          value = "Enter Some Text"),
                
                h6(em("Note: Please do not use numbers and/or Special Characters ")),
                
            
                actionButton("goButton", "Predict")
            ),
            mainPanel(
                h4("Next word prediction"),
                verbatimTextOutput("pred1"),       
                verbatimTextOutput("pred2"),
                verbatimTextOutput("pred3")
                
            ))
    ),
    
    
    tabPanel(
        
        "Help",
        
        
        p("By entering a short line of text and hitting enter or the Predict button, the user can predict the next word."),
        
        p("Dataset used by the application is avilable in the below location"),
        a(href = "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", "Swiftkey Dataset"),
        br(),
        p("comprises of datasets in 3 different languages.i have considered the english language datasets")
    ),
    
    tabPanel(
        
        "About",
        
        h3("This application is a machine learning model which predicts the next word based on the input of the user "),
        
        br(),
        
        h4("Author: Venkatesh Attinti"),
        br(),
        h4("Published Date: 28-May-2020"),
        br(),
        
        p("Source code of this application is available at",
          a(href = "https://","https://github.com/"))
    )
))
