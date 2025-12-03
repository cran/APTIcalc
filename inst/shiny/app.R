library(shiny)
library(ggplot2)
library(DT)
library(APTIcalc)  # your compute_apti function here (comp_apti assumed)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("cerulean"),

  tags$head(
    tags$style(HTML("
      h2.title-custom {
        color: #1A5276;
        font-weight: bold;
        margin-bottom: 20px;
      }
      .well {
        background-color: #F0F8FF;
        border-left: 5px solid #1ABC9C;
        padding: 15px;
      }
      .shiny-download-link {
        margin-top: 10px;
      }
      /* Footer styling - fixed to bottom */
      .app-footer {
        position: fixed;
        left: 0;
        bottom: 0;
        width: 100%;
        background-color: #ffffff;
        border-top: 1px solid #e7e7e7;
        padding: 8px 15px;
        text-align: center;
        font-size: 13px;
        color: #555555;
        z-index: 1000;
      }
      /* add a little bottom padding to main content so footer doesn't overlap */
      .main-content-padding {
        padding-bottom: 55px;
      }
    "))
  ),

  titlePanel(div(h2("🌿 Air Pollution Tolerance Index (APTI) Calculator", class = "title-custom"))),

  sidebarLayout(
    sidebarPanel(
      tags$div(class = "well",
               fileInput("file", "📁 Upload CSV File", accept = ".csv"),
               helpText("📝 CSV must include columns: A, TC, P, R"),
               br(),
               downloadButton("download_data", "⬇️ Download Results", class = "btn-primary"),
               br(), br(),
               downloadButton("download_sample", "📄 Sample Data", class = "btn-info"),
               br(), br(),
               helpText("📊 Navigate to 'Visualizations' to view plots.")
      )
    ),

    mainPanel(
      div(class = "main-content-padding",
          tabsetPanel(
            tabPanel("📊 Data Preview",
                     br(),
                     DTOutput("data_table")),
            tabPanel("📈 Visualizations",
                     br(),
                     fluidRow(
                       column(6,
                              plotOutput("histogram", height = "360px"),
                              br(),
                              downloadButton("download_histogram", "⬇️ Download Histogram (PNG)", class = "btn-success")
                       ),
                       column(6,
                              plotOutput("scatter", height = "360px"),
                              br(),
                              downloadButton("download_scatter", "⬇️ Download Scatter (PNG)", class = "btn-success")
                       )
                     ),
                     br(),
                     helpText("Tip: Use download buttons below each plot to save the image.")
            )
          )
      )
    )
  ),

  # Footer (fixed)
  tags$div(class = "app-footer", "Developed By ICAR-NIASM, Baramati, Pune-413115")
)

server <- function(input, output, session) {

  data <- reactive({
    req(input$file)
    df <- tryCatch({
      read.csv(input$file$datapath)
    }, error = function(e) {
      stop("❌ Unable to read the file. Please upload a valid CSV.")
    })

    required_cols <- c("A", "TC", "P", "R")
    if (!all(required_cols %in% colnames(df))) {
      stop("❌ CSV must contain columns: A, TC, P, R")
    }

    # Assuming comp_apti returns a data.frame with APTI and Category columns
    comp_apti(df$A, df$TC, df$P, df$R)
  })

  # Ensure data() returns a data.frame for DT and plotting
  output$data_table <- renderDT({
    datatable(data(), options = list(pageLength = 10), rownames = FALSE)
  })

  # Create reactive ggplot objects to reuse for rendering and download
  histogram_plot <- reactive({
    df <- data()
    ggplot(df, aes(x = APTI, fill = Category)) +
      geom_histogram(binwidth = 1, color = "black", alpha = 0.8) +
      scale_fill_manual(values = c(
        "Sensitive" = "#e74c3c",
        "Intermediate" = "#f39c12",
        "Tolerant" = "#27ae60"
      )) +
      theme_minimal(base_size = 15) +
      labs(title = "🌡️ Distribution of APTI Values", x = "APTI", y = "Frequency") +
      theme(plot.title = element_text(face = "bold", hjust = 0.5))
  })

  scatter_plot <- reactive({
    df <- data()
    ggplot(df, aes(x = A, y = APTI, color = Category)) +
      geom_point(size = 4, alpha = 0.9) +
      scale_color_manual(values = c(
        "Sensitive" = "#e74c3c",
        "Intermediate" = "#f39c12",
        "Tolerant" = "#27ae60"
      )) +
      theme_minimal(base_size = 15) +
      labs(title = "📈 APTI vs Ascorbic Acid Content",
           x = "Ascorbic Acid (mg/g)", y = "APTI") +
      theme(plot.title = element_text(face = "bold", hjust = 0.5))
  })

  output$histogram <- renderPlot({
    print(histogram_plot())
  })

  output$scatter <- renderPlot({
    print(scatter_plot())
  })

  # Download handlers for data and sample (kept as you had)
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("APTI_Results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )

  output$download_sample <- downloadHandler(
    filename = function() {
      "Sample_APTI_Data.csv"
    },
    content = function(file) {
      sample_data <- data.frame(
        A = c(10.2, 12.5, 9.8, 11.1, 13.3),
        TC = c(9.5, 10.1, 8.9, 9.8, 10.5),
        P = c(6.1, 6.8, 5.9, 6.2, 7.0),
        R = c(0.3, 0.4, 0.2, 0.35, 0.5)
      )
      write.csv(sample_data, file, row.names = FALSE)
    }
  )

  # Download handlers for plot images (PNG). Uses ggsave for good resolution.
  output$download_histogram <- downloadHandler(
    filename = function() {
      paste0("APTI_Histogram_", Sys.Date(), ".png")
    },
    content = function(file) {
      # Save the reactive ggplot object using ggsave
      ggsave(filename = file, plot = histogram_plot(), device = "png",
             width = 10, height = 7, units = "in", dpi = 300)
    }
  )

  output$download_scatter <- downloadHandler(
    filename = function() {
      paste0("APTI_Scatter_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(filename = file, plot = scatter_plot(), device = "png",
             width = 10, height = 7, units = "in", dpi = 600)
    }
  )

}

shinyApp(ui, server)
