# Install once (not on every run):
# install.packages(c("shiny", "bslib", "readxl", "dplyr", "ggplot2", "plotly"))

library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)

# ==================================================
# 1. LOAD DATA
# ==================================================

file_name <- list.files(pattern = "^oasis.*cross-sectional.*\\.xlsx$")[1]

if (is.na(file_name)) {
  file_name <- file.choose()
}

df <- read_excel(file_name)

required_columns <- c("ID", "M/F", "Age", "Educ", "SES",
                      "MMSE", "CDR", "eTIV", "nWBV")
missing_columns <- setdiff(required_columns, names(df))

if (length(missing_columns) > 0) {
  stop(paste("Missing columns in Excel file:",
             paste(missing_columns, collapse = ", ")))
}

# Categorical variables stored as text
df <- df |>
  mutate(
    Educ = ifelse(is.na(Educ), "Missing", as.character(Educ)),
    SES  = ifelse(is.na(SES),  "Missing", as.character(SES)),
    CDR  = ifelse(is.na(CDR),  "Missing", as.character(CDR))
  )

variables <- c(
  "Age" = "Age",
  "Brain volume (nWBV)" = "nWBV",
  "Cognitive score (MMSE)" = "MMSE",
  "Intracranial volume (eTIV)" = "eTIV"
)

group_variables <- c(
  "Sex" = "M/F",
  "Education level" = "Educ",
  "Socioeconomic status" = "SES",
  "Clinical Dementia Rating" = "CDR"
)

group_notes <- list(
  "M/F" = "Sex: F = female, M = male.",
  Educ  = "Education level is coded 1 (lowest) to 5 (highest). It is treated as separate categories here, not as a continuous scale.",
  SES   = "Socioeconomic status is coded 1 (highest) to 5 (lowest). It is treated as separate categories here, not as a continuous scale.",
  CDR   = "Clinical Dementia Rating: 0 = no dementia, 0.5 = very mild, 1 = mild, 2 = moderate. Participants without a rating appear as 'Not recorded'."
)

# ==================================================
# 2. HELPER FUNCTIONS
# ==================================================

r_strength <- function(r) {
  a <- abs(r)
  s <- if (a < 0.1) "negligible" else if (a < 0.3) "weak" else
    if (a < 0.5) "moderate" else if (a < 0.7) "fairly strong" else "strong"
  if (a < 0.1) s else paste(s, if (r < 0) "negative" else "positive")
}

format_p <- function(p) if (p < 0.001) "< .001" else sprintf("= %.3f", p)

safe_cor <- function(x, y) {
  if (length(x) < 4 || sd(x) == 0 || sd(y) == 0) return(NA_real_)
  cor(x, y)
}

# ==================================================
# 3. USER INTERFACE
# ==================================================

ui <- page_sidebar(
  title = tagList(
    span("OASIS", class = "brand"),
    span(" / Explorer", class = "brand-light")
  ),
  theme = bs_theme(
    version = 5, bootswatch = "minty",
    primary = "#477C73", secondary = "#788B84",
    base_font = "Arial", heading_font = "Arial"
  ),
  
  sidebar = sidebar(
    width = 290,
    title = "Explore the data",
    p("Choose which participants to display. Statistics always use the participants currently shown.",
      class = "sidebar-description"),
    
    h6("PARTICIPANT FILTERS", class = "section-label"),
    selectInput("sex", "Sex",
                choices = c("All participants" = "All", "Female" = "F", "Male" = "M")),
    selectInput("educ", "Education level",
                choices = c("All levels" = "All", "Level 1" = "1", "Level 2" = "2",
                            "Level 3" = "3", "Level 4" = "4", "Level 5" = "5",
                            "Not recorded" = "Missing")),
    selectInput("ses", "Socioeconomic status",
                choices = c("All groups" = "All", "Level 1" = "1", "Level 2" = "2",
                            "Level 3" = "3", "Level 4" = "4", "Level 5" = "5",
                            "Not recorded" = "Missing")),
    
    hr(),
    h6("RELATIONSHIP", class = "section-label"),
    selectInput("xvar", "Horizontal axis", choices = variables, selected = "Age"),
    selectInput("yvar", "Vertical axis", choices = variables, selected = "nWBV"),
    checkboxInput("trend", "Show overall regression line", value = TRUE),
    
    hr(),
    h6("GROUP COMPARISON", class = "section-label"),
    selectInput(
      "colour", "Colour points by",
      choices = group_variables,
      selected = "CDR"
    ),
    checkboxInput("group_lines", "Fit a separate line for each group", value = FALSE),
    
    hr(),
    actionButton("reset", "Reset all filters", class = "btn-outline-secondary w-100"),
    div(class = "sidebar-note",
        "Tip: Hover over a point to see participant details. ",
        "Zoom in or download the plot with the toolbar.")
  ),
  
  div(
    class = "dashboard",
    
    div(
      class = "welcome",
      h2("How are two measures related?"),
      p("Pick two variables, filter participants, and see the strength of the ",
        "relationship. Then colour by a group to check whether the pattern looks similar across groups.")
    ),
    
    layout_column_wrap(
      width = 1/3,
      card(class = "metric-card", card_body(
        div(class = "metric-label", "PARTICIPANTS"),
        div(class = "metric-number", textOutput("n_selected", inline = TRUE)),
        div(class = "metric-caption", "Matching your filters"))),
      card(class = "metric-card", card_body(
        div(class = "metric-label", "POINTS PLOTTED"),
        div(class = "metric-number", textOutput("n_plotted", inline = TRUE)),
        div(class = "metric-caption", "With complete axis data"))),
      card(class = "metric-card", card_body(
        div(class = "metric-label", "MEDIAN AGE"),
        div(class = "metric-number", textOutput("median_age", inline = TRUE)),
        div(class = "metric-caption", "Years, among selected participants")))
    ),
    
    card(
      class = "plot-card",
      card_header(div(
        class = "plot-heading",
        div(h5("Participant explorer"),
            p("Each dot is one participant. The line is a least-squares regression fit.",
              class = "plot-subtitle")),
        uiOutput("group_badge")
      )),
      plotlyOutput("scatter", height = "480px"),
      card_footer(
        uiOutput("stats_text"),
        div(class = "chart-footnote",
            "Observational data: associations do not necessarily indicate causal relationships.")
      )
    ),
    
    uiOutput("group_card"),
    
    card(class = "insight-card", card_body(
      h5("About this view"),
      uiOutput("description")
    ))
  ),
  
  tags$head(tags$style(HTML("
    body { background-color:#F5F7F5; color:#263B36; }
    .navbar { background:#FFFFFF !important; border-bottom:1px solid #E4EAE6; padding:10px 20px; }
    .brand { color:#477C73; font-weight:800; letter-spacing:-0.5px; }
    .brand-light { color:#71827B; font-weight:400; }
    .dashboard { padding:18px 10px 35px; }
    .welcome { padding:12px 5px 22px; }
    .welcome h2 { font-size:29px; font-weight:700; letter-spacing:-0.8px; margin-bottom:8px; }
    .welcome p { color:#71827B; font-size:15px; max-width:650px; }
    .card { border:1px solid #E3EAE5; border-radius:15px; box-shadow:0 3px 12px rgba(35,65,52,0.035);
            margin-bottom:16px; overflow:hidden; }
    .metric-card { background:#FFFFFF; }
    .metric-label, .section-label { color:#788B84; font-size:10px; font-weight:800; letter-spacing:1.2px; }
    .metric-number { color:#315F55; font-size:32px; font-weight:750; letter-spacing:-1px; padding-top:6px; }
    .metric-caption { color:#85928C; font-size:12px; padding-top:3px; }
    .sidebar-description { color:#71827B; font-size:13px; }
    .sidebar .form-label { font-size:13px; font-weight:600; }
    .sidebar .form-select { border-radius:9px; border-color:#DCE5DF; font-size:13px; }
    .sidebar .btn { border-radius:9px; }
    .sidebar-note { margin-top:20px; padding:12px; background:#EDF4F0; border-radius:10px;
                    color:#58736A; font-size:12px; line-height:1.6; }
    .plot-card .card-header { background:#FFFFFF; padding:20px 22px 12px; border-bottom:1px solid #EEF1EE; }
    .plot-heading { display:flex; align-items:center; justify-content:space-between; gap:10px; }
    .plot-heading h5 { font-weight:700; margin-bottom:5px; }
    .plot-subtitle { color:#829088; font-size:12px; margin-bottom:0; }
    .chart-footnote { font-size:11px; color:#788B84; margin-top:8px; }
    .stats-line { display:flex; flex-wrap:wrap; gap:6px 18px; font-size:14px; color:#315F55; margin-bottom:6px; }
    .stats-note { font-size:12px; color:#65766E; margin-bottom:0; }
    .insight-card h5, .group-card h5 { font-size:15px; font-weight:700; }
    .insight-card p, .group-card p { font-size:13px; line-height:1.7; color:#65766E; margin-bottom:8px; }
    .group-card table { font-size:13px; margin-bottom:10px; }
    .caveat { background:#FFF8E6; border-radius:10px; padding:10px 12px; color:#7A6420 !important; }
    .group-pill { background:#EDF4F0; color:#477C73; border-radius:20px; padding:6px 10px;
                  font-size:11px; font-weight:700; white-space:nowrap; }
    @media (max-width:700px) {
      .welcome h2 { font-size:24px; } .dashboard { padding:8px 0; } .metric-number { font-size:25px; }
    }
  ")))
)

# ==================================================
# 4. SERVER
# ==================================================

server <- function(input, output, session) {
  
  observeEvent(input$reset, {
    updateSelectInput(session, "sex", selected = "All")
    updateSelectInput(session, "educ", selected = "All")
    updateSelectInput(session, "ses", selected = "All")
    updateSelectInput(session, "xvar", selected = "Age")
    updateSelectInput(session, "yvar", selected = "nWBV")
    updateSelectInput(session, "colour", selected = "CDR")
    updateCheckboxInput(session, "trend", value = TRUE)
    updateCheckboxInput(session, "group_lines", value = FALSE)
  })
  
  var_label <- function(v) names(variables)[match(v, variables)]
  
  filtered_data <- reactive({
    dat <- df
    if (input$sex != "All")  dat <- dat |> filter(`M/F` == input$sex)
    if (input$educ != "All") dat <- dat |> filter(Educ == input$educ)
    if (input$ses != "All")  dat <- dat |> filter(SES == input$ses)
    dat
  })
  
  # Displayed participants = complete on both axes
  plot_data <- reactive({
    dat <- filtered_data() |>
      filter(!is.na(.data[[input$xvar]]), !is.na(.data[[input$yvar]])) |>
      mutate(Hover = paste0(
        "<b>Participant: ", ID, "</b>",
        "<br>Age: ", Age, "<br>Sex: ", `M/F`,
        "<br>Education: ", Educ, "<br>SES: ", SES, "<br>CDR: ", CDR,
        "<br>MMSE: ", MMSE, "<br>Brain volume (nWBV): ", nWBV))
    
    g <- as.character(dat[[input$colour]])
    g[is.na(g) | g == "Missing"] <- "Not recorded"
    dat$Group <- g
    dat
  })
  
  # Correlation and regression from the displayed participants
  stats <- reactive({
    dat <- plot_data()
    if (nrow(dat) < 4) return(NULL)
    x <- dat[[input$xvar]]; y <- dat[[input$yvar]]
    if (sd(x) == 0 || sd(y) == 0) return(NULL)
    ct <- cor.test(x, y)
    co <- coef(lm(y ~ x))
    list(n = length(x), r = unname(ct$estimate), ci = ct$conf.int,
         p = ct$p.value, intercept = unname(co[1]), slope = unname(co[2]))
  })
  
  # ---- Summary cards ----
  output$n_selected <- renderText(nrow(filtered_data()))
  output$n_plotted  <- renderText(nrow(plot_data()))
  
  output$median_age <- renderText({
    ages <- filtered_data()$Age
    if (length(ages) == 0 || all(is.na(ages))) return("N/A")
    round(median(ages, na.rm = TRUE), 1)
  })
  
  output$group_badge <- renderUI({
    nm <- names(group_variables)[match(input$colour, group_variables)]
    tags$span(class = "group-pill", paste("COLOURED BY", toupper(nm)))
  })
  
  # ---- Statistics under the plot ----
  output$stats_text <- renderUI({
    s <- stats()
    if (is.null(s)) {
      return(p(class = "stats-note",
               "Not enough complete observations (at least 4, with variation in both variables) to compute a correlation."))
    }
    xl <- var_label(input$xvar); yl <- var_label(input$yvar)
    tagList(
      div(class = "stats-line",
          span(HTML(sprintf("<b>r</b> = %.2f", s$r))),
          span(sprintf("95%% CI [%.2f, %.2f]", s$ci[1], s$ci[2])),
          span(sprintf("n = %d", s$n)),
          span(paste("p", format_p(s$p)))),
      div(class = "stats-line",
          span(sprintf("Fitted line: %s = %.3f %s %.3f * %s",
                       yl, s$intercept, ifelse(s$slope < 0, "-", "+"), abs(s$slope), xl))),
      p(class = "stats-note",
        sprintf("This is a %s linear association among the %d participants currently displayed.",
                r_strength(s$r), s$n))
    )
  })
  
  # ---- Scatter plot ----
  output$scatter <- renderPlotly({
    dat <- plot_data()
    validate(need(nrow(dat) > 0, "No participants match these filters and axis selections."))
    
    p <- ggplot(dat, aes(x = .data[[input$xvar]], y = .data[[input$yvar]],
                         colour = Group, text = Hover)) +
      geom_point(alpha = 0.78, size = 2.8)
    
    p <- p + scale_colour_brewer(palette = "Set2", na.value = "#A7B2AC")
    
    # Separate line per group
    if (input$group_lines) {
      big <- names(which(table(dat$Group) >= 4))
      dat_g <- dat |> filter(Group %in% big)
      if (nrow(dat_g) > 0) {
        p <- p + geom_smooth(
          data = dat_g,
          aes(x = .data[[input$xvar]], y = .data[[input$yvar]],
              colour = Group, group = Group),
          inherit.aes = FALSE, method = "lm", formula = y ~ x,
          se = FALSE, linewidth = 0.9
        )
      }
    }
    
    # Overall regression line
    if (input$trend && !is.null(stats())) {
      p <- p + geom_smooth(
        aes(x = .data[[input$xvar]], y = .data[[input$yvar]], group = 1),
        inherit.aes = FALSE, data = dat, method = "lm", formula = y ~ x,
        se = FALSE, colour = "#344E46", linewidth = 1.1, linetype = "dashed"
      )
    }
    
    p <- p +
      labs(x = var_label(input$xvar), y = var_label(input$yvar),
           colour = names(group_variables)[match(input$colour, group_variables)]) +
      theme_minimal(base_size = 12) +
      theme(
        plot.background = element_rect(fill = "#FFFFFF", colour = NA),
        panel.background = element_rect(fill = "#FFFFFF", colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#EDF1EE", linewidth = 0.4),
        axis.title = element_text(colour = "#566B62", face = "bold", size = 11),
        axis.text = element_text(colour = "#78877F"),
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 10),
        plot.margin = margin(12, 15, 8, 8)
      )
    
    ggplotly(p, tooltip = "text") |>
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("lasso2d", "select2d"),
             toImageButtonOptions = list(format = "png", filename = "oasis-scatter-plot")) |>
      layout(hoverlabel = list(bgcolor = "#FFFFFF", bordercolor = "#DCE5DF",
                               font = list(color = "#263B36")),
             legend = list(orientation = "h", x = 0, y = -0.2))
  })
  
  # ---- Group comparison ----
  output$group_table <- renderTable({
    dat <- plot_data()
    x <- input$xvar; y <- input$yvar
    out <- lapply(sort(unique(dat$Group)), function(g) {
      d <- dat[dat$Group == g, ]
      r <- safe_cor(d[[x]], d[[y]])
      data.frame(Group = g, n = nrow(d),
                 `Pearson r` = if (is.na(r)) "too few points" else sprintf("%.2f", r),
                 check.names = FALSE, stringsAsFactors = FALSE)
    })
    bind_rows(out)
  }, striped = TRUE, spacing = "s", align = "lrr")
  
  output$group_card <- renderUI({
    nm <- names(group_variables)[match(input$colour, group_variables)]
    note <- group_notes[[input$colour]]
    
    card(class = "group-card", card_body(
      h5(paste("Does the pattern differ by", tolower(nm), "?")),
      p(note),
      p(paste0("Correlation between ", tolower(var_label(input$xvar)), " and ",
               tolower(var_label(input$yvar)), " within each group (displayed participants only):")),
      tableOutput("group_table"),
      p(class = "caveat",
        "Caution: groups that look different, with different slopes or different r values, ",
        "do not automatically differ in a statistically significant way. ",
        "Small groups give unstable estimates (see the n column), and a formal test, ",
        "such as a group-by-variable interaction in a regression, would be needed. ",
        "Groups may also differ in other ways, such as age.")
    ))
  })
  
  output$description <- renderUI({
    x <- var_label(input$xvar); y <- var_label(input$yvar)
    nm <- names(group_variables)[match(input$colour, group_variables)]
    p(paste0(
      "This plot compares ", tolower(y), " with ", tolower(x), ". Each dot is one participant, ",
      "coloured by ", tolower(nm), ". The demographic filters decide who is included, and Pearson's r, ",
      "n and the regression line are always calculated from the participants currently displayed. ",
      "Participants missing either axis measurement are excluded."
    ))
  })
}

# ==================================================
# 5. RUN APP
# ==================================================

shinyApp(ui = ui, server = server)