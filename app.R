# app.R
# Smoking & Blood Pressure Dashboard
# "Do smokers have higher blood pressure than non-smokers?"

library(shiny)
library(dplyr)
library(ggplot2)

# 1) Load dataset ---------------------------------------------------------
clinic_data <- read.csv("data/clinic_data.csv", stringsAsFactors = FALSE)

# ensure visit_date is Date
if (!inherits(clinic_data$visit_date, "Date")) {
  clinic_data$visit_date <- as.Date(clinic_data$visit_date)
}

# 2) UI -------------------------------------------------------------------
ui <- fluidPage(
  
  # ----- CSS for colored KPI cards + icons ------------------------------
  tags$head(
    tags$style(HTML("
      .kpi-card {
        color: white;
        border-radius: 12px;
        padding: 16px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.15);
        margin-bottom: 15px;
        text-align: center;
      }
      .kpi-title {
        font-size: 13px;
        opacity: 0.9;
        margin-bottom: 4px;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }
      .kpi-value {
        font-size: 22px;
        font-weight: bold;
      }
      .kpi-icon {
        font-size: 26px;
        margin-bottom: 6px;
        opacity: 0.95;
      }
      .kpi-blue   { background-color: #1e88e5; }
      .kpi-green  { background-color: #43a047; }
      .kpi-orange { background-color: #fb8c00; }
      .kpi-red    { background-color: #e53935; }
      .kpi-purple { background-color: #8e24aa; }
      .kpi-grey   { background-color: #455a64; }
      .kpi-teal   { background-color: #00897b; }
      .filter-summary {
        font-size: 13px;
        margin-bottom: 10px;
        color: #555555;
      }
    "))
  ),
  
  titlePanel("Smoking and Blood Pressure in Clinic Patients"),
  
  p("This dashboard is designed for clinicians and quality managers. It focuses on the relationship between smoking and blood pressure."),
  tags$ul(
    tags$li("How common is smoking among their patients?"),
    tags$li("Do smokers have higher blood pressure than non-smokers?"),
    tags$li("What proportion of smokers vs non-smokers have high blood pressure (SBP ≥ 140 or DBP ≥ 90)?"),
    
  ),
  tags$hr(),
  
  sidebarLayout(
    
    # -------- Sidebar filters -------------------------------------------
    sidebarPanel(
      width = 3,
      h4("Filters"),
      
      selectInput(
        inputId = "clinic",
        label   = "Clinic:",
        choices = c("All", sort(unique(clinic_data$clinic))),
        selected = "All"
      ),
      
      checkboxGroupInput(
        inputId = "sex",
        label   = "Sex:",
        choices = unique(clinic_data$sex),
        selected = unique(clinic_data$sex)
      ),
      
      sliderInput(
        inputId = "age_range",
        label   = "Age range:",
        min  = min(clinic_data$age, na.rm = TRUE),
        max  = max(clinic_data$age, na.rm = TRUE),
        value = c(min(clinic_data$age, na.rm = TRUE),
                  max(clinic_data$age, na.rm = TRUE))
      ),
      
      dateRangeInput(
        inputId = "date_range",
        label   = "Visit date range:",
        start = min(clinic_data$visit_date, na.rm = TRUE),
        end   = max(clinic_data$visit_date, na.rm = TRUE)
      ),
      
      radioButtons(
        inputId = "forest_by",
        label   = "Forest plot subgroup:",
        choices = c("Sex", "Clinic"),
        selected = "Sex"
      ),
      
      helpText("Smoking status itself is not a filter here, because the aim is to compare smokers vs non-smokers.")
    ),
    
    # -------- Main panel ------------------------------------------------
    mainPanel(
      width = 9,
      
      h4("Question: Do smokers have higher blood pressure than non-smokers in the selected patients?"),
      
      div(class = "filter-summary",
          strong("Current filter: "),
          htmlOutput("filter_summary")
      ),
      
      # KPIs row 1
      fluidRow(
        column(
          3,
          div(class = "kpi-card kpi-blue",
              div(class = "kpi-icon", icon("users")),
              div(class = "kpi-title", "Total patients"),
              div(class = "kpi-value", textOutput("kpi_n"))
          )
        ),
        column(
          3,
          div(class = "kpi-card kpi-red",
              div(class = "kpi-icon", icon("smoking")),
              div(class = "kpi-title", "Smokers (%)"),
              div(class = "kpi-value", textOutput("kpi_smoker_pct"))
          )
        ),
        column(
          3,
          div(class = "kpi-card kpi-teal",
              div(class = "kpi-icon", icon("user-plus")),
              div(class = "kpi-title", "n – Smokers"),
              div(class = "kpi-value", textOutput("kpi_n_yes"))
          )
        ),
        column(
          3,
          div(class = "kpi-card kpi-grey",
              div(class = "kpi-icon", icon("user")),
              div(class = "kpi-title", "n – Non-smokers"),
              div(class = "kpi-value", textOutput("kpi_n_no"))
          )
        )
      ),
      
      # KPIs row 2
      fluidRow(
        column(
          4,
          div(class = "kpi-card kpi-green",
              div(class = "kpi-icon", icon("heartbeat")),
              div(class = "kpi-title", "Mean SBP – Smokers"),
              div(class = "kpi-value", textOutput("kpi_mean_sbp_yes"))
          )
        ),
        column(
          4,
          div(class = "kpi-card kpi-orange",
              div(class = "kpi-icon", icon("heartbeat")),
              div(class = "kpi-title", "Mean SBP – Non-smokers"),
              div(class = "kpi-value", textOutput("kpi_mean_sbp_no"))
          )
        ),
        column(
          4,
          div(class = "kpi-card kpi-purple",
              div(class = "kpi-icon", icon("balance-scale")),
              div(class = "kpi-title", "SBP Difference (Yes − No)"),
              div(class = "kpi-value", textOutput("kpi_sbp_diff"))
          )
        )
      ),
      
      # KPIs row 3
      fluidRow(
        column(
          4,
          div(class = "kpi-card kpi-red",
              div(class = "kpi-icon", icon("exclamation-triangle")),
              div(class = "kpi-title", "High BP (%) – Smokers"),
              div(class = "kpi-value", textOutput("kpi_highbp_yes"))
          )
        ),
        column(
          4,
          div(class = "kpi-card kpi-grey",
              div(class = "kpi-icon", icon("exclamation-circle")),
              div(class = "kpi-title", "High BP (%) – Non-smokers"),
              div(class = "kpi-value", textOutput("kpi_highbp_no"))
          )
        ),
        column(
          4,
          div(class = "kpi-card kpi-blue",
              div(class = "kpi-icon", icon("chart-line")),
              div(class = "kpi-title", "t-test (SBP Yes vs No)"),
              div(class = "kpi-value", textOutput("kpi_ttest"))
          )
        )
      ),
      
      tags$hr(),
      
      # -------- Tabs for each graph -------------------------------------
      tabsetPanel(
        id = "plots_tabs",
        
        tabPanel(
          title = "SBP distribution",
          br(),
          h4("Systolic BP by Smoking Status"),
          plotOutput("plot_sbp_box", height = "400px")
        ),
        
        tabPanel(
          title = "High BP proportion",
          br(),
          h4("High Blood Pressure (%) by Smoking Status"),
          plotOutput("plot_highbp_bar", height = "400px")
        ),
        
        tabPanel(
          title = "SBP vs Age (scatter)",
          br(),
          h4("SBP vs Age by Smoking Status"),
          plotOutput("scatter_plot", height = "400px")
        ),
        
        tabPanel(
          title = "Forest plot",
          br(),
          h4("Difference in mean SBP (Smokers − Non-smokers) by subgroup"),
          plotOutput("forest_plot", height = "400px")
        ),
        
        tabPanel(
          title = "High BP risk over age",
          br(),
          h4("Predicted probability of high BP vs age (logistic model)"),
          plotOutput("logit_plot", height = "400px")
        )
      ),
      
      tags$hr(),
      
      h4("Interpretation for the current selection"),
      verbatimTextOutput("text_summary")
    )
  )
)

# 3) SERVER ---------------------------------------------------------------
server <- function(input, output, session) {
  
  # -------- Filtered dataset --------------------------------------------
  filtered_data <- reactive({
    df <- clinic_data
    
    if (input$clinic != "All") {
      df <- df %>% filter(clinic == input$clinic)
    }
    
    df <- df %>%
      filter(
        sex %in% input$sex,
        age >= input$age_range[1],
        age <= input$age_range[2],
        visit_date >= input$date_range[1],
        visit_date <= input$date_range[2]
      ) %>%
      filter(smoker %in% c("Yes", "No"))
    
    df
  })
  
  # -------- Filter summary ----------------------------------------------
  output$filter_summary <- renderUI({
    df <- filtered_data()
    if (nrow(df) == 0) {
      return(HTML("<em>No data for the current filters.</em>"))
    }
    
    clinic_str <- if (input$clinic == "All") {
      "Clinic: All clinics"
    } else {
      paste("Clinic:", input$clinic)
    }
    
    sex_str <- if (length(input$sex) == length(unique(clinic_data$sex))) {
      "Sex: All"
    } else {
      paste("Sex:", paste(input$sex, collapse = ", "))
    }
    
    age_str  <- paste0("Age: ", input$age_range[1], "–", input$age_range[2], " years")
    date_str <- paste0("Visits: ", format(input$date_range[1]), " to ", format(input$date_range[2]))
    
    HTML(paste(clinic_str, sex_str, age_str, date_str, sep = " | "))
  })
  
  # -------- Add high BP flag --------------------------------------------
  data_with_highbp <- reactive({
    df <- filtered_data()
    if (nrow(df) == 0) return(df)
    
    df %>%
      mutate(
        high_bp = ifelse(sbp >= 140 | dbp >= 90, "High BP", "Normal BP")
      )
  })
  
  # -------- Smoking stats -----------------------------------------------
  smoking_stats <- reactive({
    df <- data_with_highbp()
    if (nrow(df) == 0) return(NULL)
    
    overall_n  <- nrow(df)
    smoker_pct <- mean(df$smoker == "Yes", na.rm = TRUE) * 100
    
    group_stats <- df %>%
      group_by(smoker) %>%
      summarise(
        n           = n(),
        mean_sbp    = mean(sbp, na.rm = TRUE),
        high_bp_pct = mean(high_bp == "High BP", na.rm = TRUE) * 100,
        .groups     = "drop"
      )
    
    list(
      overall_n   = overall_n,
      smoker_pct  = smoker_pct,
      group_stats = group_stats
    )
  })
  
  # -------- t-test -------------------------------------------------------
  ttest_sbp <- reactive({
    df <- data_with_highbp()
    if (nrow(df) == 0) return(NULL)
    if (!all(c("Yes", "No") %in% unique(df$smoker))) return(NULL)
    
    df$smoker <- factor(df$smoker, levels = c("No", "Yes"))
    tt <- try(t.test(sbp ~ smoker, data = df), silent = TRUE)
    if (inherits(tt, "try-error")) return(NULL)
    
    diff_no_yes <- as.numeric(tt$estimate[1] - tt$estimate[2])
    ci_no_yes   <- tt$conf.int
    diff_yes_no <- -diff_no_yes
    ci_yes_no   <- sort(-ci_no_yes)
    
    list(
      diff     = diff_yes_no,
      ci_low   = ci_yes_no[1],
      ci_high  = ci_yes_no[2],
      pvalue   = tt$p.value
    )
  })
  
  # -------- KPIs outputs -------------------------------------------------
  output$kpi_n <- renderText({
    s <- smoking_stats(); if (is.null(s)) return("0"); s$overall_n
  })
  output$kpi_smoker_pct <- renderText({
    s <- smoking_stats(); if (is.null(s)) return("0%")
    paste0(round(s$smoker_pct, 1), "%")
  })
  output$kpi_n_yes <- renderText({
    s <- smoking_stats(); if (is.null(s)) return("0")
    gs <- s$group_stats; if (!"Yes" %in% gs$smoker) return("0")
    gs$n[gs$smoker == "Yes"]
  })
  output$kpi_n_no <- renderText({
    s <- smoking_stats(); if (is.null(s)) return("0")
    gs <- s$group_stats; if (!"No" %in% gs$smoker) return("0")
    gs$n[gs$smoker == "No"]
  })
  output$kpi_mean_sbp_yes <- renderText({
    s <- smoking_stats(); if (is.null(s)) return("-")
    gs <- s$group_stats; if (!"Yes" %in% gs$smoker) return("NA")
    round(gs$mean_sbp[gs$smoker == "Yes"], 1)
  })
  output$kpi_mean_sbp_no <- renderText({
    s <- smoking_stats(); if (is.null(s)) return("-")
    gs <- s$group_stats; if (!"No" %in% gs$smoker) return("NA")
    round(gs$mean_sbp[gs$smoker == "No"], 1)
  })
  output$kpi_sbp_diff <- renderText({
    s <- smoking_stats(); if (is.null(s)) return("-")
    gs <- s$group_stats
    if (!all(c("Yes", "No") %in% gs$smoker)) return("NA")
    m_yes <- gs$mean_sbp[gs$smoker == "Yes"]
    m_no  <- gs$mean_sbp[gs$smoker == "No"]
    round(m_yes - m_no, 1)
  })
  output$kpi_highbp_yes <- renderText({
    s <- smoking_stats(); if (is.null(s)) return("-")
    gs <- s$group_stats; if (!"Yes" %in% gs$smoker) return("NA")
    paste0(round(gs$high_bp_pct[gs$smoker == "Yes"], 1), "%")
  })
  output$kpi_highbp_no <- renderText({
    s <- smoking_stats(); if (is.null(s)) return("-")
    gs <- s$group_stats; if (!"No" %in% gs$smoker) return("NA")
    paste0(round(gs$high_bp_pct[gs$smoker == "No"], 1), "%")
  })
  output$kpi_ttest <- renderText({
    tt <- ttest_sbp(); if (is.null(tt)) return("NA")
    p <- tt$pvalue
    p_str <- if (p < 0.001) "< 0.001" else paste0("= ", round(p, 3))
    paste0("p ", p_str, "\n95% CI: [",
           round(tt$ci_low, 1), ", ", round(tt$ci_high, 1), "]")
  })
  
  # -------- cohort-level plots ------------------------------------------
  output$plot_sbp_box <- renderPlot({
    df <- data_with_highbp(); if (nrow(df) == 0) return(NULL)
    p <- ggplot(df, aes(x = smoker, y = sbp, fill = smoker)) +
      geom_boxplot(alpha = 0.8) +
      scale_fill_manual(values = c("No" = "#4caf50", "Yes" = "#e53935")) +
      labs(x = "Smoking status", y = "Systolic blood pressure (mmHg)") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
    if (length(unique(df$sex)) > 1) p <- p + facet_wrap(~ sex)
    p
  })
  
  output$plot_highbp_bar <- renderPlot({
    df <- data_with_highbp(); if (nrow(df) == 0) return(NULL)
    if (length(unique(df$sex)) > 1) {
      df_sum <- df %>%
        group_by(sex, smoker) %>%
        summarise(
          high_bp_pct = mean(high_bp == "High BP", na.rm = TRUE) * 100,
          .groups = "drop"
        )
      p <- ggplot(df_sum, aes(x = smoker, y = high_bp_pct, fill = smoker)) +
        geom_col(alpha = 0.85) +
        geom_text(aes(label = paste0(round(high_bp_pct, 1), "%")),
                  vjust = -0.4, size = 4) +
        scale_fill_manual(values = c("No" = "#4caf50", "Yes" = "#e53935")) +
        labs(x = "Smoking status", y = "High blood pressure (%)") +
        ylim(0, 100) +
        theme_minimal(base_size = 14) +
        theme(legend.position = "none") +
        facet_wrap(~ sex)
    } else {
      df_sum <- df %>%
        group_by(smoker) %>%
        summarise(
          high_bp_pct = mean(high_bp == "High BP", na.rm = TRUE) * 100,
          .groups = "drop"
        )
      p <- ggplot(df_sum, aes(x = smoker, y = high_bp_pct, fill = smoker)) +
        geom_col(alpha = 0.85) +
        geom_text(aes(label = paste0(round(high_bp_pct, 1), "%")),
                  vjust = -0.4, size = 5) +
        scale_fill_manual(values = c("No" = "#4caf50", "Yes" = "#e53935")) +
        labs(x = "Smoking status", y = "High blood pressure (%)") +
        ylim(0, 100) +
        theme_minimal(base_size = 14) +
        theme(legend.position = "none")
    }
    p
  })
  
  # -------- Scatter plot: SBP vs Age ------------------------------------
  output$scatter_plot <- renderPlot({
    df <- data_with_highbp(); if (nrow(df) == 0) return(NULL)
    
    p <- ggplot(df, aes(x = age, y = sbp, color = smoker)) +
      geom_point(alpha = 0.4, size = 1.8) +
      geom_smooth(se = FALSE) +
      labs(
        x = "Age (years)",
        y = "Systolic blood pressure (mmHg)",
        color = "Smoking"
      ) +
      theme_minimal(base_size = 14)
    
    if (length(unique(df$sex)) > 1) {
      p <- p + facet_wrap(~ sex)
    }
    
    p
  })
  
  # -------- Forest plot data --------------------------------------------
  forest_data <- reactive({
    df <- data_with_highbp(); if (nrow(df) == 0) return(NULL)
    by_var <- if (input$forest_by == "Sex") "sex" else "clinic"
    df <- df %>% filter(!is.na(.data[[by_var]]))
    if (nrow(df) == 0) return(NULL)
    
    fd <- df %>%
      group_by(subgroup = .data[[by_var]]) %>%
      group_modify(~{
        d <- .x
        if (!all(c("Yes", "No") %in% unique(d$smoker))) {
          return(tibble(diff = NA_real_, ci_low = NA_real_,
                        ci_high = NA_real_, n = nrow(d)))
        }
        d$smoker <- factor(d$smoker, levels = c("No", "Yes"))
        tt <- try(t.test(sbp ~ smoker, data = d), silent = TRUE)
        if (inherits(tt, "try-error")) {
          return(tibble(diff = NA_real_, ci_low = NA_real_,
                        ci_high = NA_real_, n = nrow(d)))
        }
        diff_no_yes <- as.numeric(tt$estimate[1] - tt$estimate[2])
        ci_no_yes   <- tt$conf.int
        diff_yes_no <- -diff_no_yes
        ci_yes_no   <- sort(-ci_no_yes)
        tibble(
          diff   = diff_yes_no,
          ci_low = ci_yes_no[1],
          ci_high= ci_yes_no[2],
          n      = nrow(d)
        )
      }) %>%
      ungroup() %>%
      filter(!is.na(diff))
    
    if (nrow(fd) == 0) return(NULL)
    fd
  })
  
  output$forest_plot <- renderPlot({
    fd <- forest_data(); if (is.null(fd)) return(NULL)
    ggplot(fd, aes(y = subgroup, x = diff)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
      geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.2) +
      geom_point(size = 3) +
      labs(
        x = "Difference in mean SBP (Smokers − Non-smokers) mmHg",
        y = if (input$forest_by == "Sex") "Sex" else "Clinic"
      ) +
      theme_minimal(base_size = 13)
  })
  
  # -------- Logistic model ----------------------------------------------
  logit_model <- reactive({
    df <- data_with_highbp()
    if (nrow(df) == 0) return(NULL)
    if (!all(c("Yes", "No") %in% unique(df$smoker))) return(NULL)
    df$y <- as.integer(df$high_bp == "High BP")
    m <- try(glm(y ~ age + smoker, data = df, family = binomial), silent = TRUE)
    if (inherits(m, "try-error")) return(NULL)
    m
  })
  
  output$logit_plot <- renderPlot({
    df <- data_with_highbp()
    m  <- logit_model()
    if (is.null(m) || nrow(df) == 0) return(NULL)
    
    age_seq <- seq(min(df$age, na.rm = TRUE), max(df$age, na.rm = TRUE), length.out = 60)
    newdat <- expand.grid(
      age    = age_seq,
      smoker = c("No", "Yes")
    )
    newdat$pred <- predict(m, newdata = newdat, type = "response")
    
    ggplot(newdat, aes(x = age, y = pred, color = smoker)) +
      geom_line(size = 1.1) +
      labs(
        x = "Age (years)",
        y = "Predicted probability of high BP",
        color = "Smoking"
      ) +
      ylim(0, 1) +
      theme_minimal(base_size = 14)
  })
  
  # -------- Text summary -------------------------------------------------
  output$text_summary <- renderText({
    s  <- smoking_stats()
    tt <- ttest_sbp()
    
    if (is.null(s)) {
      return("No data available for the current filters.")
    }
    
    sum_tab <- s$group_stats
    if (!all(c("Yes", "No") %in% sum_tab$smoker)) {
      return("One of the smoking groups (Yes/No) has no data under the current filters, so a comparison is not possible.")
    }
    
    n_total    <- s$overall_n
    smoker_pct <- s$smoker_pct
    
    m_yes <- sum_tab$mean_sbp[sum_tab$smoker == "Yes"]
    m_no  <- sum_tab$mean_sbp[sum_tab$smoker == "No"]
    d     <- m_yes - m_no
    
    hp_yes <- sum_tab$high_bp_pct[sum_tab$smoker == "Yes"]
    hp_no  <- sum_tab$high_bp_pct[sum_tab$smoker == "No"]
    
    line1 <- paste0(
      "For the current selection (n = ", n_total, 
      ", smokers: ", round(smoker_pct, 1), "%), ",
      "smokers have a mean systolic blood pressure of ",
      round(m_yes, 1), " mmHg compared with ",
      round(m_no, 1), " mmHg in non-smokers (difference: ",
      round(d, 1), " mmHg)."
    )
    
    line2 <- paste0(
      "High blood pressure (SBP ≥ 140 or DBP ≥ 90) is observed in ",
      round(hp_yes, 1), "% of smokers versus ",
      round(hp_no, 1), "% of non-smokers."
    )
    
    if (is.null(tt)) {
      line3 <- "A t-test could not be performed (both groups required)."
    } else {
      p <- tt$pvalue
      p_str <- if (p < 0.001) "< 0.001" else paste0("= ", round(p, 3))
      line3 <- paste0(
        "A two-sample t-test for SBP (smokers vs non-smokers) yielded p ",
        p_str, ", with a 95% confidence interval for the mean difference (Yes − No) of [",
        round(tt$ci_low, 1), "] to [", round(tt$ci_high, 1), "] mmHg."
      )
    }
    
    paste(line1, line2, line3, sep = "\n")
  })
}

# 4) Run the app ----------------------------------------------------------
shinyApp(ui = ui, server = server)
