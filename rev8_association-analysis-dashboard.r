# Association Analysis Dashboard - Working Version
# Filters hidden by default, data table shows all data
# Fixed correlation matrix with proper variable selection
# Handles column names with spaces and missing values properly
# Light color palette for correlation matrix with download buttons
# User-customizable titles, subtitles, and captions for all plots
# Regression report download functionality


library(shiny)
library(DT)
library(dplyr)
library(readr)
library(readxl)
library(ggplot2)
library(corrplot)
library(broom)

# Set global theme for consistent plots
theme_set(theme_minimal(base_size = 16) +
          theme(
            plot.title = element_text(face = "bold", size = 24),
            plot.subtitle = element_text(face = "bold", size = 20),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            axis.text = element_text(face = "bold"),
            legend.position = "bottom",
            strip.text = element_text(face = "bold"),
            panel.spacing.x = unit(1.5, "cm"),
            panel.spacing.y = unit(1.5, "cm"),
            plot.margin = margin(20, 20, 20, 20, "pt"),
            plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA)
          )
)

# UI
ui <- fluidPage(
  # Add custom CSS for consistent styling
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
  # tags$head(
  #   tags$style(HTML("
  #     body {
  #       font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
  #       font-size: 16px;
  #     }
  #     .well {
  #       background-color: #f8f9fa;
  #       border: 1px solid #dee2e6;
  #       border-radius: 8px;
  #     }
  #     .btn {
  #       font-weight: bold;
  #       border-radius: 6px;
  #       padding: 8px 16px;
  #     }
  #     .selectize-input {
  #       font-weight: 500;
  #     }
  #     h1, h2, h3, h4 {
  #       font-weight: bold;
  #     }
  #     .shiny-input-container label {
  #       font-weight: bold;
  #     }
  #   "))
  # ),

  titlePanel("Association AnalyxeR"),

  # File upload row
  fluidRow(
    column(4,
      fileInput("file", "Upload CSV or Excel File",
                accept = c(".csv", ".xlsx", ".xls"))
    ),
    column(2,
      checkboxInput("header", "Header row", TRUE)
    ),
    column(3,
      conditionalPanel(
        condition = "output.file_uploaded",
        actionButton("show_filters", "Show/Hide Filters",
                     class = "btn-warning")
      )
    ),
    column(3,
      verbatimTextOutput("data_status")
    )
  ),

  # Collapsible filter section
  conditionalPanel(
    condition = "output.show_filter_ui",
    wellPanel(
      h4("Filters (Optional)"),
      p("Adjust sliders to filter data. Close this section when done."),
      uiOutput("filter_ui"),
      style = "background-color: #f0f0f0; margin: 20px 0;"
    )
  ),

  # Main tabs
  tabsetPanel(
    tabPanel("Data Table",
      br(),
      DT::dataTableOutput("data_table")
    ),

    tabPanel("Scatterplot",
      br(),
      fluidRow(
        column(4,
          selectInput("x_var", "X Variable:", choices = NULL)
        ),
        column(4,
          selectInput("y_var", "Y Variable:", choices = NULL)
        ),
        column(4,
          selectInput("color_var", "Color by (optional):",
                      choices = NULL)
        )
      ),
      fluidRow(
        column(4,
          textInput("scatter_title", "Title:",
                    value = "Scatterplot Analysis")
        ),
        column(4,
          textInput("scatter_subtitle", "Subtitle:",
                    value = "")
        ),
        column(4,
          textInput("scatter_caption", "Caption:",
                    value = "")
        )
      ),
      hr(),
      fluidRow(
        column(12, align = "center",
          downloadButton("download_scatter_png", "Download PNG", class = "btn-primary"),
          downloadButton("download_scatter_svg", "Download SVG", class = "btn-success"),
          downloadButton("download_scatter_pdf", "Download PDF", class = "btn-info")
        )
      ),
      br(),
      plotOutput("scatter_plot", height = "600px")
    ),

    tabPanel("Correlation Matrix",
      br(),
      fluidRow(
        column(8,
          selectInput("corr_vars", "Select Variables (choose 2-30):",
                      choices = NULL, multiple = TRUE),
          helpText("Select numeric variables to include in the correlation matrix. Variables with missing values will only use complete cases.")
        ),
        column(4,
          br(),
          actionButton("update_corr", "Create Matrix",
                       class = "btn-primary", style = "width: 100%;")
        )
      ),
      fluidRow(
        column(4,
          textInput("corr_title", "Title:",
                    value = "Correlation Matrix")
        ),
        column(4,
          textInput("corr_subtitle", "Subtitle:",
                    value = "")
        ),
        column(4,
          textInput("corr_caption", "Caption:",
                    value = "")
        )
      ),
      fluidRow(
        column(12,
          verbatimTextOutput("corr_info")
        )
      ),
      hr(),
      fluidRow(
        column(12, align = "center",
          downloadButton("download_corr_png", "Download PNG", class = "btn-primary"),
          downloadButton("download_corr_svg", "Download SVG", class = "btn-success"),
          downloadButton("download_corr_pdf", "Download PDF", class = "btn-info")
        )
      ),
      br(),
      plotOutput("corr_plot", height = "700px")
    ),

    tabPanel("Regression",
      br(),
      fluidRow(
        column(4,
          selectInput("dep_var", "Dependent Variable:",
                      choices = NULL)
        ),
        column(8,
          selectInput("indep_vars", "Independent Variables:",
                      choices = NULL, multiple = TRUE)
        )
      ),
      fluidRow(
        column(4,
          textInput("reg_title", "Title:",
                    value = "Regression Diagnostic Plots")
        ),
        column(4,
          textInput("reg_subtitle", "Subtitle:",
                    value = "")
        ),
        column(4,
          textInput("reg_caption", "Caption:",
                    value = "")
        )
      ),
      fluidRow(
        column(12,
          checkboxInput("download_reg_summary",
                        "Include regression summary in report download",
                        value = TRUE),
          conditionalPanel(
            condition = "input.download_reg_summary",
            downloadButton("download_reg_report", "Download Regression Report",
                           class = "btn-warning")
          )
        )
      ),
      hr(),
      br(),
      verbatimTextOutput("reg_summary"),
      br(),
      fluidRow(
        column(12, align = "center",
          downloadButton("download_reg_png", "Download PNG", class = "btn-primary"),
          downloadButton("download_reg_svg", "Download SVG", class = "btn-success"),
          downloadButton("download_reg_pdf", "Download PDF", class = "btn-info")
        )
      ),
      br(),
      plotOutput("reg_plots", height = "600px")
    )
  )
)

# Server
server <- function(input, output, session) {

  # Reactive values
  values <- reactiveValues(
    show_filters = FALSE,
    data = NULL,
    filtered_data = NULL
  )

  # Toggle filter visibility
  observeEvent(input$show_filters, {
    values$show_filters <- !values$show_filters
  })

  output$show_filter_ui <- reactive({
    values$show_filters
  })
  outputOptions(output, "show_filter_ui", suspendWhenHidden = FALSE)

  # File upload indicator
  output$file_uploaded <- reactive({
    !is.null(input$file)
  })
  outputOptions(output, "file_uploaded", suspendWhenHidden = FALSE)

  # Load data
  observeEvent(input$file, {
    req(input$file)

    ext <- tools::file_ext(input$file$datapath)

    df <- tryCatch({
      if(ext == "csv") {
        read_csv(input$file$datapath,
                 col_names = input$header,
                 show_col_types = FALSE)
      } else {
        read_excel(input$file$datapath,
                   col_names = input$header)
      }
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message),
                       type = "error")
      return(NULL)
    })

    if(!is.null(df)) {
      values$data <- df
      values$filtered_data <- df

      # Update all dropdowns
      num_cols <- names(df)[sapply(df, is.numeric)]
      all_cols <- names(df)

      if(length(num_cols) == 0) {
        showNotification("Warning: No numeric columns found in the data",
                         type = "warning", duration = 10)
      } else {
        updateSelectInput(session, "x_var", choices = num_cols,
                          selected = num_cols[1])
        updateSelectInput(session, "y_var", choices = num_cols,
                          selected = if(length(num_cols) > 1) num_cols[2] else num_cols[1])
        updateSelectInput(session, "dep_var", choices = num_cols,
                          selected = num_cols[1])
        updateSelectInput(session, "indep_vars", choices = num_cols,
                          selected = if(length(num_cols) > 1) num_cols[2] else NULL)

        # Update correlation variables selector
        # Select variables with least missing values
        missing_counts <- sapply(df[num_cols], function(x) sum(is.na(x)))
        sorted_cols <- num_cols[order(missing_counts)]
        default_corr_vars <- sorted_cols[1:min(length(sorted_cols), 5)]

        updateSelectInput(session, "corr_vars", choices = num_cols,
                          selected = default_corr_vars)
      }

      updateSelectInput(session, "color_var",
                        choices = c("None" = "", all_cols))
    }
  })

  # Update scatter title when variables change
  observeEvent(c(input$x_var, input$y_var), {
    if(!is.null(input$x_var) && !is.null(input$y_var) && !is.null(values$filtered_data)) {
      updateTextInput(session, "scatter_title",
                      value = paste("Scatterplot:", input$x_var, "vs", input$y_var))

      # Calculate complete cases for subtitle
      plot_data <- values$filtered_data %>%
        filter(!is.na(.data[[input$x_var]]) & !is.na(.data[[input$y_var]]))

      updateTextInput(session, "scatter_subtitle",
                      value = paste("n =", nrow(plot_data), "complete cases"))
    }
  })

  # Update regression title when variables change
  observeEvent(input$dep_var, {
    if(!is.null(input$dep_var)) {
      updateTextInput(session, "reg_title",
                      value = paste("Regression Diagnostics for", input$dep_var))
    }
  })

  # Update correlation subtitle with info when matrix is created
  observeEvent(corr_data(), {
    cd <- corr_data()
    if(!is.null(cd$n_complete)) {
      updateTextInput(session, "corr_subtitle",
                      value = paste("Based on", cd$n_complete, "complete cases"))
    }
  })

  # Data status
  output$data_status <- renderText({
    if(is.null(values$data)) {
      "No data loaded"
    } else {
      paste0("Loaded: ", nrow(values$filtered_data), " rows, ",
             ncol(values$data), " columns")
    }
  })

  # Dynamic filter UI
  output$filter_ui <- renderUI({
    req(values$data)

    num_cols <- names(values$data)[sapply(values$data, is.numeric)]

    # Create sliders in rows of 4
    filter_rows <- lapply(seq(1, length(num_cols), by = 4), function(i) {
      cols_in_row <- num_cols[i:min(i+3, length(num_cols))]

      fluidRow(
        lapply(cols_in_row, function(col) {
          col_data <- values$data[[col]]
          rng <- range(col_data, na.rm = TRUE)

          column(3,
            sliderInput(
              inputId = paste0("filter_", make.names(col)),
              label = if(nchar(col) > 15) paste0(substr(col, 1, 15), "...") else col,
              min = rng[1],
              max = rng[2],
              value = rng,
              step = (rng[2] - rng[1]) / 100
            )
          )
        })
      )
    })

    tagList(filter_rows)
  })

  # Apply filters
  observe({
    req(values$data)

    filtered <- values$data

    # Only apply filters if filter panel is shown
    if(values$show_filters) {
      num_cols <- names(values$data)[sapply(values$data, is.numeric)]

      for(col in num_cols) {
        filter_id <- paste0("filter_", make.names(col))
        if(!is.null(input[[filter_id]])) {
          filter_range <- input[[filter_id]]

          # Check if column exists and has valid range
          if(col %in% names(filtered) &&
             !is.na(filter_range[1]) &&
             !is.na(filter_range[2])) {
            filtered <- filtered %>%
              filter(.data[[col]] >= filter_range[1] &
                     .data[[col]] <= filter_range[2])
          }
        }
      }
    }

    values$filtered_data <- filtered
  })

  # Data table
  output$data_table <- DT::renderDataTable({
    req(values$filtered_data)

    DT::datatable(
      values$filtered_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      extensions = 'Buttons'
    )
  })

  # Scatterplot
  output$scatter_plot <- renderPlot({
    req(values$filtered_data, input$x_var, input$y_var)

    # Remove rows with NA in x or y variables
    plot_data <- values$filtered_data %>%
      filter(!is.na(.data[[input$x_var]]) & !is.na(.data[[input$y_var]]))

    if(nrow(plot_data) == 0) {
      plot(1, type = "n", xlab = "", ylab = "",
           main = "No complete cases for selected variables")
      return()
    }

    p <- ggplot(plot_data,
                aes(x = .data[[input$x_var]],
                    y = .data[[input$y_var]]))

    if(!is.null(input$color_var) && input$color_var != "") {
      p <- p + geom_point(aes(color = .data[[input$color_var]]),
                          size = 3, alpha = 0.7)
    } else {
      p <- p + geom_point(color = "steelblue", size = 3, alpha = 0.7)
    }

    # Use custom title, subtitle, and caption
    title_text <- if(!is.null(input$scatter_title) && input$scatter_title != "") {
      input$scatter_title
    } else {
      paste("Scatterplot:", input$x_var, "vs", input$y_var)
    }

    subtitle_text <- if(!is.null(input$scatter_subtitle) && input$scatter_subtitle != "") {
      input$scatter_subtitle
    } else {
      paste("n =", nrow(plot_data), "complete cases")
    }

    p + geom_smooth(method = "lm", se = TRUE, color = "red") +
      labs(x = input$x_var, y = input$y_var,
           title = title_text,
           subtitle = subtitle_text,
           caption = input$scatter_caption)
  })

  # Reactive for current scatter plot
  scatter_plot_reactive <- reactive({
    req(values$filtered_data, input$x_var, input$y_var)

    plot_data <- values$filtered_data %>%
      filter(!is.na(.data[[input$x_var]]) & !is.na(.data[[input$y_var]]))

    if(nrow(plot_data) == 0) {
      return(NULL)
    }

    p <- ggplot(plot_data,
                aes(x = .data[[input$x_var]],
                    y = .data[[input$y_var]]))

    if(!is.null(input$color_var) && input$color_var != "") {
      p <- p + geom_point(aes(color = .data[[input$color_var]]),
                          size = 3, alpha = 0.7)
    } else {
      p <- p + geom_point(color = "steelblue", size = 3, alpha = 0.7)
    }

    # Use custom title, subtitle, and caption
    title_text <- if(!is.null(input$scatter_title) && input$scatter_title != "") {
      input$scatter_title
    } else {
      paste("Scatterplot:", input$x_var, "vs", input$y_var)
    }

    subtitle_text <- if(!is.null(input$scatter_subtitle) && input$scatter_subtitle != "") {
      input$scatter_subtitle
    } else {
      paste("n =", nrow(plot_data), "complete cases")
    }

    p + geom_smooth(method = "lm", se = TRUE, color = "red") +
      labs(x = input$x_var, y = input$y_var,
           title = title_text,
           subtitle = subtitle_text,
           caption = input$scatter_caption)
  })

  # Download handlers for scatterplot
  output$download_scatter_png <- downloadHandler(
    filename = function() paste0("scatterplot_", Sys.Date(), ".png"),
    content = function(file) {
      ggsave(file, plot = scatter_plot_reactive(),
             device = "png", width = 10, height = 8, dpi = 300)
    }
  )

  output$download_scatter_svg <- downloadHandler(
    filename = function() paste0("scatterplot_", Sys.Date(), ".svg"),
    content = function(file) {
      ggsave(file, plot = scatter_plot_reactive(),
             device = "svg", width = 10, height = 8)
    }
  )

  output$download_scatter_pdf <- downloadHandler(
    filename = function() paste0("scatterplot_", Sys.Date(), ".pdf"),
    content = function(file) {
      ggsave(file, plot = scatter_plot_reactive(),
             device = "pdf", width = 10, height = 8)
    }
  )

  # Correlation matrix
  corr_data <- eventReactive(input$update_corr, {
    req(values$filtered_data, input$corr_vars)

    if(length(input$corr_vars) < 2) {
      return(list(error = "Please select at least 2 variables"))
    }

    # Get selected data
    selected_data <- values$filtered_data[input$corr_vars]

    # Check for complete cases
    complete_rows <- complete.cases(selected_data)
    n_complete <- sum(complete_rows)

    if(n_complete < 3) {
      return(list(error = paste("Not enough complete cases. Only",
                                n_complete, "rows without missing values.")))
    }

    # Calculate correlation
    cor_matrix <- cor(selected_data[complete_rows, ],
                      method = "pearson")

    list(matrix = cor_matrix,
         n_complete = n_complete,
         n_total = nrow(selected_data))
  }, ignoreNULL = FALSE)

  output$corr_info <- renderText({
    if(input$update_corr == 0) {
      "Select variables and click 'Create Matrix' to generate correlation plot"
    } else {
      cd <- corr_data()
      if(!is.null(cd$error)) {
        cd$error
      } else if(!is.null(cd$n_complete)) {
        paste("Using", cd$n_complete, "complete cases out of",
              cd$n_total, "total rows")
      }
    }
  })

  output$corr_plot <- renderPlot({
    if(input$update_corr == 0) return(NULL)

    cd <- corr_data()
    if(!is.null(cd$error)) return(NULL)
    req(cd$matrix)

    # Increase margins for title/subtitle/caption
    par(mar = c(6, 4, 6, 2))

    corrplot(cd$matrix,
             method = "color",
             type = "upper",
             order = "hclust",
             tl.col = "black",
             tl.srt = 45,
             tl.cex = 1.6,  # Doubled text size for row/column labels
             addCoef.col = "black",
             number.cex = 1.2,  # Larger numbers
             col = colorRampPalette(c("#FFB3BA", "#FFFFBA", "#FFFFFF",
                                      "#BAFFC9", "#BAE1FF"))(200))  # Light pastel palette

    # Add custom titles
    if(!is.null(input$corr_title) && input$corr_title != "") {
      mtext(input$corr_title, side = 3, line = 3, cex = 1.5, font = 2)
    }
    if(!is.null(input$corr_subtitle) && input$corr_subtitle != "") {
      mtext(input$corr_subtitle, side = 3, line = 1.5, cex = 1.2, font = 2)
    }
    if(!is.null(input$corr_caption) && input$corr_caption != "") {
      mtext(input$corr_caption, side = 1, line = 4, cex = 1, font = 3)
    }
  })

  # Download handlers for correlation matrix
  output$download_corr_png <- downloadHandler(
    filename = function() paste0("correlation_matrix_", Sys.Date(), ".png"),
    content = function(file) {
      png(file, width = 1200, height = 1200, res = 150)

      cd <- corr_data()
      if(!is.null(cd$matrix)) {
        par(mar = c(6, 4, 6, 2))

        corrplot(cd$matrix,
                 method = "color",
                 type = "upper",
                 order = "hclust",
                 tl.col = "black",
                 tl.srt = 45,
                 tl.cex = 1.6,  # Doubled text size for row/column labels
                 addCoef.col = "black",
                 number.cex = 1.2,
                 col = colorRampPalette(c("#FFB3BA", "#FFFFBA", "#FFFFFF",
                                          "#BAFFC9", "#BAE1FF"))(200))

        # Add custom titles
        if(!is.null(input$corr_title) && input$corr_title != "") {
          mtext(input$corr_title, side = 3, line = 3, cex = 1.5, font = 2)
        }
        if(!is.null(input$corr_subtitle) && input$corr_subtitle != "") {
          mtext(input$corr_subtitle, side = 3, line = 1.5, cex = 1.2, font = 2)
        }
        if(!is.null(input$corr_caption) && input$corr_caption != "") {
          mtext(input$corr_caption, side = 1, line = 4, cex = 1, font = 3)
        }
      }
      dev.off()
    }
  )

  output$download_corr_svg <- downloadHandler(
    filename = function() paste0("correlation_matrix_", Sys.Date(), ".svg"),
    content = function(file) {
      svg(file, width = 12, height = 12)

      cd <- corr_data()
      if(!is.null(cd$matrix)) {
        par(mar = c(6, 4, 6, 2))

        corrplot(cd$matrix,
                 method = "color",
                 type = "upper",
                 order = "hclust",
                 tl.col = "black",
                 tl.srt = 45,
                 tl.cex = 1.6,  # Doubled text size for row/column labels
                 addCoef.col = "black",
                 number.cex = 1.2,
                 col = colorRampPalette(c("#FFB3BA", "#FFFFBA", "#FFFFFF",
                                          "#BAFFC9", "#BAE1FF"))(200))

        # Add custom titles
        if(!is.null(input$corr_title) && input$corr_title != "") {
          mtext(input$corr_title, side = 3, line = 3, cex = 1.5, font = 2)
        }
        if(!is.null(input$corr_subtitle) && input$corr_subtitle != "") {
          mtext(input$corr_subtitle, side = 3, line = 1.5, cex = 1.2, font = 2)
        }
        if(!is.null(input$corr_caption) && input$corr_caption != "") {
          mtext(input$corr_caption, side = 1, line = 4, cex = 1, font = 3)
        }
      }
      dev.off()
    }
  )

  output$download_corr_pdf <- downloadHandler(
    filename = function() paste0("correlation_matrix_", Sys.Date(), ".pdf"),
    content = function(file) {
      pdf(file, width = 12, height = 12)

      cd <- corr_data()
      if(!is.null(cd$matrix)) {
        par(mar = c(6, 4, 6, 2))

        corrplot(cd$matrix,
                 method = "color",
                 type = "upper",
                 order = "hclust",
                 tl.col = "black",
                 tl.srt = 45,
                 tl.cex = 1.6,  # Doubled text size for row/column labels
                 addCoef.col = "black",
                 number.cex = 1.2,
                 col = colorRampPalette(c("#FFB3BA", "#FFFFBA", "#FFFFFF",
                                          "#BAFFC9", "#BAE1FF"))(200))

        # Add custom titles
        if(!is.null(input$corr_title) && input$corr_title != "") {
          mtext(input$corr_title, side = 3, line = 3, cex = 1.5, font = 2)
        }
        if(!is.null(input$corr_subtitle) && input$corr_subtitle != "") {
          mtext(input$corr_subtitle, side = 3, line = 1.5, cex = 1.2, font = 2)
        }
        if(!is.null(input$corr_caption) && input$corr_caption != "") {
          mtext(input$corr_caption, side = 1, line = 4, cex = 1, font = 3)
        }
      }
      dev.off()
    }
  )

  # Regression
  output$reg_summary <- renderPrint({
    req(values$filtered_data, input$dep_var, input$indep_vars)

    # Build formula with backticks for spaces
    dep_var_bt <- paste0("`", input$dep_var, "`")
    indep_vars_bt <- paste0("`", input$indep_vars, "`")
    formula_str <- paste(dep_var_bt, "~",
                         paste(indep_vars_bt, collapse = " + "))

    tryCatch({
      model <- lm(as.formula(formula_str), data = values$filtered_data)
      summary(model)
    }, error = function(e) {
      cat("Error fitting model:\n", e$message)
      cat("\n\nCheck for missing values or collinearity in selected variables.")
    })
  })

  output$reg_plots <- renderPlot({
    req(values$filtered_data, input$dep_var, input$indep_vars)

    dep_var_bt <- paste0("`", input$dep_var, "`")
    indep_vars_bt <- paste0("`", input$indep_vars, "`")
    formula_str <- paste(dep_var_bt, "~",
                         paste(indep_vars_bt, collapse = " + "))

    tryCatch({
      model <- lm(as.formula(formula_str), data = values$filtered_data)

      par(mfrow = c(2, 2), mar = c(5, 4, 4, 2), oma = c(2, 0, 3, 0))
      plot(model)

      # Add overall title
      if(!is.null(input$reg_title) && input$reg_title != "") {
        mtext(input$reg_title, side = 3, line = 1, outer = TRUE, cex = 1.5, font = 2)
      }
      if(!is.null(input$reg_subtitle) && input$reg_subtitle != "") {
        mtext(input$reg_subtitle, side = 3, line = -0.5, outer = TRUE, cex = 1.2, font = 2)
      }
      if(!is.null(input$reg_caption) && input$reg_caption != "") {
        mtext(input$reg_caption, side = 1, line = 0, outer = TRUE, cex = 1, font = 3)
      }
    }, error = function(e) {
      plot(1, type = "n", xlab = "", ylab = "",
           main = "Unable to create diagnostic plots")
      text(1, 1, paste("Error:", e$message), cex = 1.2)
    })
  })

  # Download handlers for regression plots
  output$download_reg_png <- downloadHandler(
    filename = function() paste0("regression_plots_", Sys.Date(), ".png"),
    content = function(file) {
      png(file, width = 1200, height = 900, res = 150)

      req(values$filtered_data, input$dep_var, input$indep_vars)

      dep_var_bt <- paste0("`", input$dep_var, "`")
      indep_vars_bt <- paste0("`", input$indep_vars, "`")
      formula_str <- paste(dep_var_bt, "~",
                           paste(indep_vars_bt, collapse = " + "))

      tryCatch({
        model <- lm(as.formula(formula_str), data = values$filtered_data)
        par(mfrow = c(2, 2), mar = c(5, 4, 4, 2), oma = c(2, 0, 3, 0))
        plot(model)

        # Add overall title
        if(!is.null(input$reg_title) && input$reg_title != "") {
          mtext(input$reg_title, side = 3, line = 1, outer = TRUE, cex = 1.5, font = 2)
        }
        if(!is.null(input$reg_subtitle) && input$reg_subtitle != "") {
          mtext(input$reg_subtitle, side = 3, line = -0.5, outer = TRUE, cex = 1.2, font = 2)
        }
        if(!is.null(input$reg_caption) && input$reg_caption != "") {
          mtext(input$reg_caption, side = 1, line = 0, outer = TRUE, cex = 1, font = 3)
        }
      }, error = function(e) {
        plot(1, type = "n", xlab = "", ylab = "",
             main = "Unable to create diagnostic plots")
      })

      dev.off()
    }
  )

  output$download_reg_svg <- downloadHandler(
    filename = function() paste0("regression_plots_", Sys.Date(), ".svg"),
    content = function(file) {
      svg(file, width = 12, height = 9)

      req(values$filtered_data, input$dep_var, input$indep_vars)

      dep_var_bt <- paste0("`", input$dep_var, "`")
      indep_vars_bt <- paste0("`", input$indep_vars, "`")
      formula_str <- paste(dep_var_bt, "~",
                           paste(indep_vars_bt, collapse = " + "))

      tryCatch({
        model <- lm(as.formula(formula_str), data = values$filtered_data)
        par(mfrow = c(2, 2), mar = c(5, 4, 4, 2), oma = c(2, 0, 3, 0))
        plot(model)

        # Add overall title
        if(!is.null(input$reg_title) && input$reg_title != "") {
          mtext(input$reg_title, side = 3, line = 1, outer = TRUE, cex = 1.5, font = 2)
        }
        if(!is.null(input$reg_subtitle) && input$reg_subtitle != "") {
          mtext(input$reg_subtitle, side = 3, line = -0.5, outer = TRUE, cex = 1.2, font = 2)
        }
        if(!is.null(input$reg_caption) && input$reg_caption != "") {
          mtext(input$reg_caption, side = 1, line = 0, outer = TRUE, cex = 1, font = 3)
        }
      }, error = function(e) {
        plot(1, type = "n", xlab = "", ylab = "",
             main = "Unable to create diagnostic plots")
      })

      dev.off()
    }
  )

  output$download_reg_pdf <- downloadHandler(
    filename = function() paste0("regression_plots_", Sys.Date(), ".pdf"),
    content = function(file) {
      pdf(file, width = 12, height = 9)

      req(values$filtered_data, input$dep_var, input$indep_vars)

      dep_var_bt <- paste0("`", input$dep_var, "`")
      indep_vars_bt <- paste0("`", input$indep_vars, "`")
      formula_str <- paste(dep_var_bt, "~",
                           paste(indep_vars_bt, collapse = " + "))

      tryCatch({
        model <- lm(as.formula(formula_str), data = values$filtered_data)
        par(mfrow = c(2, 2), mar = c(5, 4, 4, 2), oma = c(2, 0, 3, 0))
        plot(model)

        # Add overall title
        if(!is.null(input$reg_title) && input$reg_title != "") {
          mtext(input$reg_title, side = 3, line = 1, outer = TRUE, cex = 1.5, font = 2)
        }
        if(!is.null(input$reg_subtitle) && input$reg_subtitle != "") {
          mtext(input$reg_subtitle, side = 3, line = -0.5, outer = TRUE, cex = 1.2, font = 2)
        }
        if(!is.null(input$reg_caption) && input$reg_caption != "") {
          mtext(input$reg_caption, side = 1, line = 0, outer = TRUE, cex = 1, font = 3)
        }
      }, error = function(e) {
        plot(1, type = "n", xlab = "", ylab = "",
             main = "Unable to create diagnostic plots")
      })

      dev.off()
    }
  )

  # Download handler for regression report
  output$download_reg_report <- downloadHandler(
    filename = function() paste0("regression_report_", Sys.Date(), ".txt"),
    content = function(file) {
      req(values$filtered_data, input$dep_var, input$indep_vars)

      dep_var_bt <- paste0("`", input$dep_var, "`")
      indep_vars_bt <- paste0("`", input$indep_vars, "`")
      formula_str <- paste(dep_var_bt, "~",
                           paste(indep_vars_bt, collapse = " + "))

      # Capture the regression summary
      sink(file)
      cat("========================================\n")
      cat("REGRESSION ANALYSIS REPORT\n")
      cat("========================================\n\n")

      if(!is.null(input$reg_title) && input$reg_title != "") {
        cat("Title:", input$reg_title, "\n")
      }
      if(!is.null(input$reg_subtitle) && input$reg_subtitle != "") {
        cat("Subtitle:", input$reg_subtitle, "\n")
      }
      if(!is.null(input$reg_caption) && input$reg_caption != "") {
        cat("Caption:", input$reg_caption, "\n")
      }

      cat("\nDate:", Sys.Date(), "\n")
      cat("Time:", format(Sys.time(), "%H:%M:%S"), "\n\n")

      cat("Model Formula:\n")
      cat(formula_str, "\n\n")

      tryCatch({
        model <- lm(as.formula(formula_str), data = values$filtered_data)

        cat("REGRESSION SUMMARY:\n")
        cat("==================\n\n")
        print(summary(model))

        cat("\n\nANOVA TABLE:\n")
        cat("============\n\n")
        print(anova(model))

        cat("\n\nCOEFFICIENTS WITH CONFIDENCE INTERVALS:\n")
        cat("======================================\n\n")
        print(confint(model))

        cat("\n\nMODEL DIAGNOSTICS:\n")
        cat("==================\n\n")
        cat("AIC:", AIC(model), "\n")
        cat("BIC:", BIC(model), "\n")
        cat("Residual standard error:", sigma(model), "\n")
        cat("Degrees of freedom:", model$df.residual, "\n")

      }, error = function(e) {
        cat("\nError in regression analysis:\n")
        cat(e$message, "\n")
      })

      sink()
    }
  )
}

shinyApp(ui = ui, server = server)
