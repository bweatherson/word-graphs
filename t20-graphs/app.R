library(shiny)
library(ggplot2)
library(dplyr)
library(slider)

# Load your tibble (assuming you've saved it as an RDS)
word_data <- readRDS("word_data.rds")

word_index <- split(word_data, word_data$word)

word_list <- readRDS("word_list.rds")

ui <- navbarPage("Word Usage in Philosophy Journals",
                 tabPanel("App",
                          sidebarLayout(
                            sidebarPanel(
                              p("Select whether you'd like to see how often the word is used per 1000 words published, the percentage of articles the word appears in, or the percentage of articles in which the word appears at least 10 times."),
                              radioButtons("type", "Select data type:",
                                           choices = c("Frequency" = "frequency",
                                                       "Appearances" = "appear",
                                                       "10+ Appearances" = "topic"),
                                           selected = "frequency"),
                              textInput("word", "Enter a word:", value = "philosophy"),
                              p("The dots are the annual values, the line is a five year rolling average centered on that year."),
                              downloadButton("downloadPlot", "Download graph")
                              ),
                            mainPanel(
                              plotOutput("usagePlot")
                            )
                          )
                 ),
                 tabPanel("About",
                          fluidRow(
                            column(10, offset = 1,
                                   h2("About This App"),
                                   p("The data here shows how often words are used in the following 20 journals, from 1980–2019."),
                                   tags$table(
                                     class = "table table-striped",
                                     tags$thead(
                                       tags$tr(
                                         tags$th("Journal"), tags$th("Articles"), tags$th("Words")
                                       )
                                     ),
                                     tags$tbody(
                                       tags$tr(tags$td("American Philosophical Quarterly"), tags$td("1,170"), tags$td("9,192,943")),
                                       tags$tr(tags$td("Analysis     "), tags$td(" 2,225"), tags$td("  5,858,163")),
                                       tags$tr(tags$td("Australasian Journal of Philosophy "), tags$td(" 1,361"), tags$td(" 10,703,604")),
                                       tags$tr(tags$td("British Journal for the Philosophy of Science"), tags$td(" 1,060"), tags$td("10,667,948")),
                                       tags$tr(tags$td("Canadian Journal of Philosophy     "), tags$td(" 1,067"), tags$td(" 10,663,229")),
                                       tags$tr(tags$td("Ethics       "), tags$td(" 1,037"), tags$td(" 10,791,948")),
                                       tags$tr(tags$td("Journal of Philosophical Logic     "), tags$td(" 1,007"), tags$td(" 10,318,105")),
                                       tags$tr(tags$td("Journal of Philosophy   "), tags$td(" 1,181"), tags$td(" 10,623,752")),
                                       tags$tr(tags$td("Linguistics and Philosophy         "), tags$td("   711"), tags$td("10,079,432")),
                                       tags$tr(tags$td("Mind         "), tags$td(" 1,069"), tags$td(" 10,426,520")),
                                       tags$tr(tags$td("Monist       "), tags$td(" 1,255"), tags$td(" 10,201,234")),
                                       tags$tr(tags$td("Noûs         "), tags$td(" 1,152"), tags$td(" 12,224,560")),
                                       tags$tr(tags$td("Pacific Philosophical Quarterly    "), tags$td(" 1,011"), tags$td("  9,466,146")),
                                       tags$tr(tags$td("Philosophical Quarterly "), tags$td(" 1,080"), tags$td("  8,344,414")),
                                       tags$tr(tags$td("Philosophical Review    "), tags$td("   511"), tags$td("7,055,603")),
                                       tags$tr(tags$td("Philosophical Studies   "), tags$td(" 3,407"), tags$td(" 28,456,406")),
                                       tags$tr(tags$td("Philosophy & Public Affairs        "), tags$td("   521"), tags$td("5,978,222")),
                                       tags$tr(tags$td("Philosophy and Phenomenological Research  "), tags$td(" 2,146"), tags$td(" 17,993,432")),
                                       tags$tr(tags$td("Philosophy of Science   "), tags$td(" 1,961"), tags$td(" 15,216,385")),
                                       tags$tr(tags$td("Synthese     "), tags$td(" 3,653"), tags$td(" 36,568,335")))                            )
                                   ,
                                   p("Mostly the data comes from JSTOR. For AJP and the most recent issues of journals that are not yet on JSTOR, I downloaded the papers directly and used ",
                                     a(href="https://cran.r-project.org/web/packages/pdftools/index.html", "pdftools"), " to extract the text."),
                                   p("Only words that are:"),
                                   tags$ul(
                                     tags$li("At least three letters long"),
                                     tags$li("Appear at least 500 times across all journals"),
                                     tags$li("Appear at least 10 times in one article")
                                   ),
                                   p("Questions? Contact me at ", a(href="mailto:brian@weatherson.org", "brian@weatherson.org"))
                            )
                          )
                 )
)

server <- function(input, output, session) {
  # Update word list on startup
  
  # Filter data when user clicks Go
  # filtered_data <- eventReactive(input$go, {
  #   req(input$word, input$type)
  #   word_data %>%
  #     filter(type == input$type, word == tolower(input$word))
  # })
  
  filtered_data <- reactive({
    word_index[[tolower(input$word)]] %||% tibble() 
  })

  plot_reactive <- reactive(
    {    data <- filtered_data() 
    if (nrow(data) == 0) {
      plot.new(); text(0.5, 0.5, "Word not in dataset (see about page for details)", cex = 2)
    } else {
      data <- data %>% 
        filter(type == input$type) %>%
        mutate(rolling = slide_mean(value, before = 2, after = 2))
      y_label <- switch(input$type,
                        "frequency" = "Frequency per 1000 words",
                        "appear" = "Percentage of articles",
                        "topic" = "Percentage of articles")
      graph_title <- switch(input$type,
                            "frequency" = paste0("Usage of \"",input$word,"\""),
                            "appear" = paste0("Articles containing \"",input$word,"\""),
                            "topic" = paste0("Articles with 10 uses of \"",input$word,"\""))
      gg <- ggplot(data, aes(x = year, y = value)) +
        geom_line(aes(x = year, y = rolling),
                  color = "red",
                  linewidth = 0.5,
                  alpha = 0.5) +
        geom_point(size = 3, color = "red") +
        labs(title = graph_title, y = y_label, x = element_blank()) +
        theme_minimal() +
        theme(
          text = element_text(size = 16),           # All text (fallback)
          axis.title = element_text(size = 18),     # Axis titles
          axis.text = element_text(size = 14),      # Axis tick labels
          plot.title = element_text(size = 20, face = "bold")  # Plot title
        )
      gg
    }})
    
      
  output$usagePlot <- renderPlot({
      plot_reactive()
    }
  )
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0("plot_", input$word, ".png")
    },
    content = function(file) {
      ggsave(file, plot = plot_reactive() + theme(plot.background = element_rect(fill = "white", color = NA)), device = "png")
    }
  )

}

shinyApp(ui = ui, server = server)
