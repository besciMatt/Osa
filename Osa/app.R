# Load required libraries
library(shiny)
library(httr)
library(stringr)
library(shinyjs)


# Define the API endpoint and your API key
api_url <- "https://api.openai.com/v1/chat/completions"
api_key <- "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"

# Define the system prompt
system_prompt <- "You are 'Osa: One Step Ahead', a supportive assistant to help users get unstuck from a current setback or frustration.
Your goal is to help users with weight loss and mental wellness challenges in a conversational manner. 
You are to apply the concepts of motivational interviewing and to inform their understanding of change and sustain language into your responses.

1. Begin by empathizing briefly by offering a reflection, summary or affirmation of the user’s statement. Ask a question to gain context to assess the challenge using the COM-B framework.
2. As soon as you can do so, identify the relevant COM-B element in a sentence or two but dont use COM-B jargon. Just describe the Capability, Opportunity, or Motivation element that is relevant to the user in plain terms.
3. Suggest a BCT intervention based on your COM-B assessment, using a friendly tone. Identify the BCT for the user in plain terms.
4. Offer a concise, actionable recommendation following the SMART criteria (specific, measurable, achievable, relevant, time-bound) based on your COM-B and BCT assessment. Do not mention SMART explicitly, just use plain language. 
5. Conclude with a brief word of encouragement and ask if they need further assistance.
6. If the user appears satisfied, suggest that the conversation end there and give the user one last motivational nudge.

Here are some general rules:
1. Always keep your messages short, no more than two to three sentences.
2. Always engage the user by asking for their thoughts or prompting a response when it seems appropriate. 
3. The goal is to reach the recommendation stage as soon as you have enough information to do so. Don't ask questions over and over, only ask questions to assess the challenge and get to the BCT asap.
4. Do not generate user inputs.
5. Treat short (single to a few word) responses as continuations of the current conversation, not as new conversation initiations. 
6. If the user expresses satisfaction with the recommendation, treat that as the end of the conversation and wrap up.
7. Never repeat yourself.
8. If the user input is extreme, suggestive of self harm, or suggestive of any condition that should be escalated to a support or care team, encourage the user to do so. 
9. You are a short term intervention assistant, not a long term behavior coach. The user comes to you in moments of need, to get an actionable next step. You are not the user's long term coach.
10. When having conversation with the user, always apply Grice's maxims of quality, quantity, relevance, and manner.
11. If the user responds negatively to three suggestions in a row, comment on their sustain talk start to dig into their motivation and confidence to make the desired change. Try to direct the user back to change talk (and away from sustain talk).  
When users share their challenges:
Remember, think of this as a chat with a expert coach or trusted friend, keeping it light, interactive, and to the point.  
Always consider the conversation as ongoing unless the user explicitly ends it with the end button."

ui <- fluidPage(
  useShinyjs(),
  div(
    titlePanel("Chat with Osa"),
    style = "color: white; background-color: #fb513b"
  ),
  sidebarLayout(
    sidebarPanel(
      h3("Welcome to Osa: One Step Ahead"),
      p("This application allows you to chat with Osa, a supportive assistant to help get you unstuck from a current setback or frustration and stay one step ahead on your wellness journey.
        Simply describe your challenge and get science-backed actionable advice to stay one step ahead."),
      style = "background-color: #f6f4ee",
      tags$hr(),
    ),
    mainPanel(
      tags$style(type = "text/css", ".shiny-output-error {visibility: hidden;}"),
      tags$style(type = "text/css", ".shiny-output-error:before {content: ' Check your inputs or API key';}"),
      tags$style(type = "text/css", "label {font-weight: bold;}"),
      fluidRow(
        column(12,tags$h3("Chat History"),tags$hr(),uiOutput("chat_history"),tags$hr())
      ),
      fluidRow(
        column(11,textAreaInput(inputId = "user_message", placeholder = NULL, label=NULL, width = "100%")),
        column(12,actionButton("send_message", "Send",icon = icon("play"),height = "350px")),
        column(12, actionButton(inputId = "end_conversation", icon = icon("stop"),label = "End Conversation", style = "margin-top: 10px;")),
        column(12, 
               tags$div(id = "rating_div", style = "display: none; margin-top: 15px;",  # Initially hidden
                        tags$h5("Was this chat helpful?"),
                        actionButton(inputId = "rating_yes", label = "Yes", style = "margin-right: 10px;"),
                        actionButton(inputId = "rating_no", label = "No")
               )
        )
      ),style = "background-color: #f6f4ee")
  ),style = "background-color:  #fb513b ")



server <- function(input, output, session) {
  chat_data <- reactiveVal(data.frame(source = "Osa", message = "Hello! I'm Osa, your helpful assistant from Noom.
                                      I can help you work through challenges or frustrations you might be facing so you can stay one step ahead on your wellness journey. 
                                      How can I help you today?", stringsAsFactors = FALSE))
  
  observeEvent(input$send_message, {
    # Add user's message to chat history
    new_data <- data.frame(source = "You", message = input$user_message, stringsAsFactors = FALSE)
    shinyjs::disable("send_message")
    chat_data(rbind(chat_data(), new_data))
    
    # Ensure the bot doesn't respond to its own messages
    if (new_data$source != "Osa") {
      # Call GPT-3 API with the chat history
      gpt_res <- call_gpt_api(api_key, chat_data(), system_prompt)
      
      # Add GPT-3's response to chat history
      chat_data(rbind(chat_data(), data.frame(source = "Osa", message = gpt_res, stringsAsFactors = FALSE)))
    }
    updateTextInput(session, inputId = "user_message", value = "")
    shinyjs::enable("send_message")
    
  })
  
  
  observeEvent(input$end_conversation, {
    new_data <- data.frame(source = "You", message = "User has ended the conversation.", stringsAsFactors = FALSE)
    chat_data(rbind(chat_data(), new_data))
  })
  
  observeEvent(input$end_conversation, {
    # Reveal the rating UI element when "End Conversation" is clicked
    shinyjs::show("rating_div")
  })
  
  call_gpt_api <- function(api_key, chat_history, system_prompt) {
    # Limit to the last 4 exchanges (8 rows: 4 user, 4 assistant)
    chat_history <- tail(chat_history, 8)
    
    # Prepare messages for GPT-3 based on chat history
    messages_list <- lapply(1:nrow(chat_history), function(i) {
      list(role = ifelse(chat_history[i, "source"] == "You", "user", "assistant"), content = chat_history[i, "message"])
    })
    
    # Append the system prompt (if needed)
    messages_list <- append(messages_list, list(list(role = "system", content = system_prompt)))
    
    # Call the API
    response <- httr::POST(
      url = api_url, 
      add_headers(Authorization = paste("Bearer", api_key)),
      content_type("application/json"),
      encode = "json",
      body = list(
        model = "gpt-4",
        messages = messages_list,
        temperature = 0.8,
        max_tokens = 2000
      )
    )
    return(str_trim(content(response)$choices[[1]]$message$content))
  }
  
  output$chat_history <- renderUI({
    chatBox <- lapply(1:nrow(chat_data()), function(i) {
      tags$div(class = ifelse(chat_data()[i, "source"] == "You", "alert alert-secondary", "alert alert-success"),
               HTML(paste0("<b>", chat_data()[i, "source"], ":</b> ", chat_data()[i, "message"])))
    })
    do.call(tagList, chatBox)
  })
}

shinyApp(ui = ui, server = server)
