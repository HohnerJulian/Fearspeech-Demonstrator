library(shiny)
library(readr)

#setwd("C:/Users/Julian Arbeit/Documents/FS_App_Classification_en")
# Load your dataset here
posts <- read_csv("fs_data_anonym.csv")  




# Adjusted UI function
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
    body {
      font-family: 'Open Sans', sans-serif;
      background-color: #f5f5f5;
      margin: 0;
      padding: 0;
      overflow-x: hidden;
    }
    .main-container {
      margin-top: 10px;
      margin-left: 10%;
      margin-right: 10%;
      padding: 15px;
    }
    h1 {
      color: #333333;
      font-size: 36px;
      margin-top: 0;
    }
    h2 {
      color: #555555;
      font-size: 28px;
      margin-top: 5px;
    }
    h3 {
      color: #333333;
      font-size: 24px;
      margin-bottom: 15px;
    }
    .btn-primary {
      background-color: #3498db;
      border-color: #3498db;
      border-radius: 4px;
      font-weight: bold;
    }
    .btn-primary:hover {
      background-color: #2471a3;
      border-color: #2471a3;
    }
    .well {
      background-color: #fff;
      border: 1px solid #ddd;
      box-shadow: 0 1px 1px rgba(0,0,0,0.05);
      padding: 20px;
      margin-bottom: 30px;
      border-radius: 4px;
    }
    #classification-output .output-value {
      font-size: 20px;
      font-weight: bold;
    }
    .image-row {
      margin-top: 10px;
      margin-bottom: 10px;
    }
    .btn-lg {
      padding: 10px 16px;
      font-size: 18px;
      line-height: 1.33;
      border-radius: 6px;
      margin: 10px;
    }
    /* New CSS classes for output coloring */
    .output-yes {
      color: green;
      font-weight: bold;
      font-size: 20px; 
    }
    .output-no {
      color: red;
      font-weight: bold;
      font-size: 20px; 
    }
  "))
  ),
  
  
  #img(src = "bmi.png", width = "250px", height = "175px", style = "float: right; margin-right: 10px; margin-top: 40px;"),
  #img(src = "bmbf.png", width = "250px", height = "200px", style = "float: right; margin-right: 10px; margin-top: 10px;"),
  #img(src = "motra.png", width = "275px", height = "150px", style = "float: left; margin-left: 10px; margin-top: 10px;")
  # Additional row for images
  fluidRow(
    column(12,
           img(src = "bmi.png", width = "200px", height = "150px", style = "float: right; margin-right: 10px; margin-top: 40px;"),
           img(src = "bmbf.png", width = "200px", height = "175px", style = "float: right; margin-right: 10px; margin-top: 10px;"),
           img(src = "motra.png", width = "225px", height = "100px", style = "float: left; margin-left: 10px; margin-top: 40px;")
    )
  ),
  div(class="main-container",  # This div wraps your content and applies the margins
      titlePanel(uiOutput("pageTitle")),
      fluidRow(
        column(6,
               h3("Random Telegram Post"),
               wellPanel(
                 textOutput("randomPost")
               )
        ),
        column(6,
               h3("Definition of Fearspeech"),
               wellPanel(
                 uiOutput("definition")
               )
        )
      ),
      
      fluidRow(
        column(12, align = "center", style = "padding-bottom: 20px;",  # Space below the buttons
               actionButton("btnFear", "The post contains Fear Speech", class = "btn-primary btn-lg", style = "margin: 5px; background-color: darkred; border-color: darkred;"),
               actionButton("btnNoFear", "The post does not contain Fear Speech", class = "btn-primary btn-lg", style = "margin: 5px; background-color: darkred; border-color: darkred;")
        )
      ),
      
      # Additional row for the refresh button and text
      fluidRow(
        column(12, align = "center",
               actionButton("btnRefresh", "", icon = icon("refresh"), class = "btn-default", style = "margin: 5px;"),
               "Refresh for the next post."
        )
      ),
      
      # Conditional Panel for classifications
      conditionalPanel(
        condition = "output.userHasClassified",
        div(id = "classification-output",
            uiOutput("classifications")  # Output for displaying classifications horizontally
        )
      )
  )  
)





#### SERVER

server <- function(input, output, session) {
  
  output$pageTitle <- renderUI({
    HTML("<div style='text-align: center;'><h1 style='font-size: 60px; color: darkred; font-weight: bold;'>You vs. the Machine</h1><br><h2 style='font-size: 50px'>Do you recognize the use of fear speech in right-wing communication better than the machine?</h2></div>")
  })
  
  currentPost <- reactiveVal()
  
  updateCurrentPost <- function() {
    currentPost(posts[sample(nrow(posts), 1), ])
  }
  
  updateCurrentPost()
  
  userHasClassified <- reactiveVal(FALSE)
  userClassification <- reactiveVal(NA)
  
  observeEvent(input$btnFear, {
    userClassification("Contains Fear Speech")
  })
  
  observeEvent(input$btnNoFear, {
    userClassification("Does not contain Fear Speech")
  })
  
  output$userHasClassified <- reactive({ userHasClassified() })
  outputOptions(output, "userHasClassified", suspendWhenHidden = FALSE)
  
  output$randomPost <- renderText({
    currentPost()$post
  })
  
  output$definition <- renderUI({
    HTML(
      "<p>Fearspeech means that a post directly or indirectly represents a specific group or institution as dangerous or harmful (to one's own group), whether on a cultural, social, or existential level.</p>",
"<p><strong>Fearspeech is not coded if:</strong></p>",
"<p>No addressing of fear or anxieties</p>",
"<p>Concerns are expressed - but with little emotion</p>",
  "<p>Indirect Fearspeech - Threat visible, but not the main message of the post.</p>",
"<p>Hostile speech - Hate or anger are the main focus of the post.</p>",
  "<p><strong>Fearspeech is coded if:</strong></p>",
"<p>Direct Fearspeech - (elaborated) threat is the main focus of the post and enemy group is named.</p>",
  "<p>Effective Fearspeech - Direct Fearspeech + call to action (e.g.: 'hit the streets!')</p>"

    )
  })
  
  observeEvent(input$btnFear, {
    userHasClassified(TRUE)
    post <- currentPost()
    output$userClass <- renderUI({
      span(
        if (!is.na(userClassification()) && userClassification() == "Contains Fear Speech") {
          "Contains Fear Speech"
        } else {
          "Does not contain Fear Speech"
        }, 
        class = ifelse(post$fs_manual == 1 && userClassification() == "Contains Fear Speech", "output-yes", 
                       ifelse(post$fs_manual == 0 && userClassification() == "Does not contain Fear Speech", "output-yes", "output-no"))
      )
    })
    output$humanClass <- renderUI({
      span(
        ifelse(post$fs_manual == 1, "Yes", "No"), 
        class = ifelse(post$fs_manual == 1, "output-yes", "output-no")
      )
    })
    output$algorithmClass <- renderUI({
      span(
        ifelse(post$fs_auto == 1, "Yes", "No"), 
        class = ifelse(post$fs_auto == 1, "output-yes", "output-no")
      )
    })
    output$probNotFear <- renderUI({
      span(sprintf("%.2f", as.numeric(post$prob_0)), class = "output-value")
    })
    output$probFear <- renderUI({
      span(sprintf("%.2f", as.numeric(post$prob_1)), class = "output-value")
    })
  })
  
  observeEvent(input$btnNoFear, {
    userHasClassified(TRUE)
    post <- currentPost()
    output$userClass <- renderUI({
      span(
        if (!is.na(userClassification()) && userClassification() == "Contains Fear Speech") {
          "Contains Fear Speech"
        } else {
          "Does not contain Fear Speech"
        }, 
        class = ifelse(post$fs_manual == 0 && userClassification() == "Does not contain Fear Speech", "output-yes", 
                       ifelse(post$fs_manual == 1 && userClassification() == "Contains Fear Speech", "output-yes", "output-no"))
      )
    })
    output$humanClass <- renderUI({
      span(
        ifelse(post$fs_manual == 0, "No", "Yes"), 
        class = ifelse(post$fs_manual == 1, "output-yes", "output-no")
      )
    })
    output$algorithmClass <- renderUI({
      span(
        ifelse(post$fs_auto == 0, "No", "Yes"), 
        class = ifelse(post$fs_manual == 1, "output-yes", "output-no")
      )
    })
    output$probNotFear <- renderUI({
      span(sprintf("%.2f", as.numeric(post$prob_0)), class = "output-value")
    })
    output$probFear <- renderUI({
      span(sprintf("%.2f", as.numeric(post$prob_1)), class = "output-value")
    })
  })
  
  observeEvent(input$btnRefresh, {
    updateCurrentPost()
    userHasClassified(FALSE)
    userClassification(NA)
  })
  
  output$classifications <- renderUI({
    fluidRow(
      column(3, h4("Your Classification:"), uiOutput("userClass")),
      column(3, h4("Researcher Classification:"), uiOutput("humanClass")),
      column(3, h4("AI Classification:"), uiOutput("algorithmClass")),
      column(3, h4("Probability Not Fear Speech:"), uiOutput("probNotFear")),
      column(3, h4("Probability Fear Speech:"), uiOutput("probFear"))
    )
  })
}


# Run the app
shinyApp(ui = ui, server = server)



