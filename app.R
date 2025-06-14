library(shiny)
library(tidyllm)
library(jsonlite)
library(DT)

# Load the precomputed cubes object with embeddings
cubesEN <- readRDS("statcan_cubesEN_with_embeddings.rds")
cubesFR <- readRDS("statcan_cubesFR_with_embeddings.rds")

app_password <- Sys.getenv("correct_password")

# Prepare embedding matrices in advance
embedding_matrix_en1 <- do.call(
  rbind,cubesEN$embedding_en
)

embedding_matrix_en2 <- do.call(
  rbind,embedding_matrix_en1[[2]]
)

embedding_matrix_fr1 <- do.call(
  rbind,cubesFR$embedding_fr
)

embedding_matrix_fr2 <- do.call(
  rbind,embedding_matrix_fr1[[2]]
)

# UI
ui <- fluidPage(
  titlePanel("tablefinder"),

  # Password entry UI
  uiOutput("password_ui"),

  # Conditional content shown only after correct password
  conditionalPanel(
    condition = "output.authenticated == true",
    fluidRow(
      column(
        width = 12,
        tags$div(
          style = "margin-bottom: 20px;",
          tags$img(src = "canada_stats_leaf.ico", height = "200px", style = "left; margin-left: 20px;"),
          tags$p(
            "I'm tablefinder;  I identify search relevant Statistics Canada data tables using AI.",
            style = "font-size: 16px;"
          ),
          tags$p(
            "To use me, please type a question or keyword up to 140 characters (e.g., 'corporate bond issues in 2020'), and I will return the most relevant Statistics Canada tables.",
            style = "font-size: 14px; color: #555;"
          )
        )
      )
    ),
    
    sidebarLayout(
      sidebarPanel(
        textInput("query", "Enter your query:"),
        selectInput("language", "Language", choices = c("English", "French")),
        actionButton("submit", "Submit")
      ),
      
       tags$script(HTML("
        $(document).on('input', '#query', function() {
          if(this.value.length > 140) {
            this.value = this.value.substring(0, 140);
          }
          Shiny.setInputValue('query', this.value, {priority: 'event'});
        });
      "))
    ),
    mainPanel(
      tableOutput("similarityResults")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Track authentication status
  auth <- reactiveVal(FALSE)

  output$char_count <- renderText({
    n <- nchar(input$query %||% "")
    paste0(n, " / 140 characters")
  })

  output$password_ui <- renderUI({
    if (!auth()) {
      tagList(
        passwordInput("password", "Enter access password:"),
        actionButton("login", "Login")
      )
    }
  })

  observeEvent(input$login, {
    if (input$password == app_password) {
      auth(TRUE)
    } else {
      showModal(modalDialog(
        title = "Access Denied",
        "Incorrect password. Please try again.",
        easyClose = TRUE
      ))
    }
  })

  # Allow conditionalPanel to detect auth status
  output$authenticated <- reactive({ auth() })
  outputOptions(output, "authenticated", suspendWhenHidden = FALSE)

  output$similarityResults <- renderTable({
    req(input$submit)
    query_vec <- suppressWarnings(as.numeric(unlist(embed(substring(input$query,1,140), openai))))

    # Select embedding matrix and titles based on language
    if (input$language == "English") {
      embedding_matrix <- embedding_matrix_en2
      titles <- cubesEN$cubeTitleEn
    } else {
      embedding_matrix <- embedding_matrix_fr2
      titles <- cubesFR$cubeTitleFr
    }

    # Compute cosine similarity
    dot_products <- embedding_matrix %*% na.omit(as.numeric(query_vec))
    norm_query <- sqrt(sum(na.omit(query_vec^2)))
    norm_matrix <- sqrt(rowSums(embedding_matrix^2))
    similarities <- dot_products / (norm_query * norm_matrix)

    # Top 5 matches
    top_indices <- order(similarities, decreasing = TRUE)[1:5]
    results <- data.frame(
      Title = titles[top_indices],
      ProductId = cubesEN$productId[top_indices],
      Link = paste0(
        '<a href="https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=',
        cubesEN$productId[top_indices],
        '01" target="_blank">View Table</a>'
      ),
      Similarity = round(similarities[top_indices], 3),
      stringsAsFactors = FALSE
    )
    results
  }, sanitize.text.function = identity)
}

shinyApp(ui = ui, server = server)
