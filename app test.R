install.packages(c("shiny", "bslib", "readxl","dplyr", "ggplot2", "plotly"))

library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)

# ==================================================
# 1. LOAD DATA
# ==================================================

file_name <- "oasis-cross-sectional-5708aa0a98d82080.xlsx"

if (!file.exists(file_name)) {
  file_name <- file.choose()
}

df <- read_excel(file_name)

# Check that the required columns exist
required_columns <- c(
  "ID", "M/F", "Age", "Educ", "SES",
  "MMSE", "CDR", "eTIV", "nWBV"
)

missing_columns <- setdiff(required_columns, names(df))

if (length(missing_columns) > 0) {
  stop(
    paste(
      "Missing columns in Excel file:",
      paste(missing_columns, collapse = ", ")
    )
  )
}

# Convert categorical variables to character
df <- df |>
  mutate(
    Educ = ifelse(is.na(Educ), "Missing",
                  as.character(Educ)),
    SES = ifelse(is.na(SES), "Missing",
                 as.character(SES)),
    CDR = ifelse(is.na(CDR), "Missing",
                 as.character(CDR))
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

# ==================================================
# 2. USER INTERFACE
# ==================================================

ui <- page_sidebar(
  title = tagList(
    span("OASIS", class = "brand"),
    span(" / Explorer", class = "brand-light")
  ),

  theme = bs_theme(
    version = 5,
    bootswatch = "minty",
    primary = "#477C73",
    secondary = "#788B84",
    base_font = "Arial",
    heading_font = "Arial"
  ),

  sidebar = sidebar(
    width = 290,
    title = "Explore the data",

    p(
      "Choose which participants to display.",
      class = "sidebar-description"
    ),

    h6("PARTICIPANT FILTERS", class = "section-label"),

    selectInput(
      "sex", "Sex",
      choices = c(
        "All participants" = "All",
        "Female" = "F",
        "Male" = "M"
      ),
      selected = "All"
    ),

    selectInput(
      "educ", "Education level",
      choices = c(
        "All levels" = "All",
        "Level 1" = "1",
        "Level 2" = "2",
        "Level 3" = "3",
        "Level 4" = "4",
        "Level 5" = "5",
        "Not recorded" = "Missing"
      ),
      selected = "All"
    ),

    selectInput(
      "ses", "Socioeconomic status",
      choices = c(
        "All groups" = "All",
        "Level 1" = "1",
        "Level 2" = "2",
        "Level 3" = "3",
        "Level 4" = "4",
        "Level 5" = "5",
        "Not recorded" = "Missing"
      ),
      selected = "All"
    ),

    hr(),

    h6("PLOT SETTINGS", class = "section-label"),

    selectInput(
      "xvar", "Horizontal axis",
      choices = variables,
      selected = "Age"
    ),

    selectInput(
      "yvar", "Vertical axis",
      choices = variables,
      selected = "nWBV"
    ),

    selectInput(
      "colour", "Colour points by",
      choices = group_variables,
      selected = "Educ"
    ),

    checkboxInput(
      "trend", "Show trend line",
      value = FALSE
    ),

    hr(),

    actionButton(
      "reset",
      "Reset all filters",
      class = "btn-outline-secondary w-100"
    ),

    div(
      class = "sidebar-note",
      "Tip: Hover over a point to see participant details. ",
      "You can zoom in or download the plot."
    )
  ),

  # ==================================================
  # 3. MAIN DASHBOARD
  # ==================================================

  div(
    class = "dashboard",

    div(
      class = "welcome",
      h2("Discover the data"),
      p(
        "Explore relationships between age, brain volume ",
        "and cognitive performance across participant groups."
      )
    ),

    layout_column_wrap(
      width = 1/3,

      card(
        class = "metric-card",
        card_body(
          div(class = "metric-label", "PARTICIPANTS"),
          div(
            class = "metric-number",
            textOutput("n_selected", inline = TRUE)
          ),
          div(class = "metric-caption", "Matching your filters")
        )
      ),

      card(
        class = "metric-card",
        card_body(
          div(class = "metric-label", "POINTS PLOTTED"),
          div(
            class = "metric-number",
            textOutput("n_plotted", inline = TRUE)
          ),
          div(class = "metric-caption", "With complete axis data")
        )
      ),

      card(
        class = "metric-card",
        card_body(
          div(class = "metric-label", "MEDIAN AGE"),
          div(
            class = "metric-number",
            textOutput("median_age", inline = TRUE)
          ),
          div(
            class = "metric-caption",
            "Years, among selected participants"
          )
        )
      )
    ),

    card(
      class = "plot-card",

      card_header(
        div(
          class = "plot-heading",
          div(
            h5("Participant explorer"),
            p(
              "Each dot represents one participant.",
              class = "plot-subtitle"
            )
          ),
          uiOutput("group_badge")
        )
      ),

      plotlyOutput("scatter", height = "480px"),

      card_footer(
        div(
          class = "chart-footnote",
          "Observational data: associations do not necessarily ",
          "indicate causal relationships."
        )
      )
    ),

    card(
      class = "insight-card",
      card_body(
        h5("About this view"),
        uiOutput("description")
      )
    )
  ),

  # ==================================================
  # 4. CUSTOM STYLING
  # ==================================================

  tags$head(
    tags$style(HTML("
      body {
        background-color: #F5F7F5;
        color: #263B36;
      }

      .navbar {
        background: #FFFFFF !important;
        border-bottom: 1px solid #E4EAE6;
        padding: 10px 20px;
      }

      .brand {
        color: #477C73;
        font-weight: 800;
        letter-spacing: -0.5px;
      }

      .brand-light {
        color: #71827B;
        font-weight: 400;
      }

      .dashboard {
        padding: 18px 10px 35px;
      }

      .welcome {
        padding: 12px 5px 22px;
      }

      .welcome h2 {
        font-size: 29px;
        font-weight: 700;
        letter-spacing: -0.8px;
        margin-bottom: 8px;
      }

      .welcome p {
        color: #71827B;
        font-size: 15px;
        max-width: 650px;
      }

      .card {
        border: 1px solid #E3EAE5;
        border-radius: 15px;
        box-shadow: 0 3px 12px rgba(35,65,52,0.035);
        margin-bottom: 16px;
        overflow: hidden;
      }

      .metric-card {
        background: #FFFFFF;
      }

      .metric-label,
      .section-label {
        color: #788B84;
        font-size: 10px;
        font-weight: 800;
        letter-spacing: 1.2px;
      }

      .metric-number {
        color: #315F55;
        font-size: 32px;
        font-weight: 750;
        letter-spacing: -1px;
        padding-top: 6px;
      }

      .metric-caption {
        color: #85928C;
        font-size: 12px;
        padding-top: 3px;
      }

      .sidebar-description {
        color: #71827B;
        font-size: 13px;
      }

      .sidebar .form-label {
        font-size: 13px;
        font-weight: 600;
      }

      .sidebar .form-select {
        border-radius: 9px;
        border-color: #DCE5DF;
        font-size: 13px;
      }

      .sidebar .btn {
        border-radius: 9px;
      }

      .sidebar-note {
        margin-top: 20px;
        padding: 12px;
        background: #EDF4F0;
        border-radius: 10px;
        color: #58736A;
        font-size: 12px;
        line-height: 1.6;
      }

      .plot-card .card-header {
        background: #FFFFFF;
        padding: 20px 22px 12px;
        border-bottom: 1px solid #EEF1EE;
      }

      .plot-heading {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 10px;
      }

      .plot-heading h5 {
        font-weight: 700;
        margin-bottom: 5px;
      }

      .plot-subtitle {
        color: #829088;
        font-size: 12px;
        margin-bottom: 0;
      }

      .chart-footnote {
        font-size: 11px;
        color: #788B84;
      }

      .insight-card h5 {
        font-size: 15px;
        font-weight: 700;
      }

      .insight-card p {
        font-size: 13px;
        line-height: 1.7;
        color: #65766E;
        margin-bottom: 0;
      }

      .group-pill {
        background: #EDF4F0;
        color: #477C73;
        border-radius: 20px;
        padding: 6px 10px;
        font-size: 11px;
        font-weight: 700;
        white-space: nowrap;
      }

      @media (max-width: 700px) {
        .welcome h2 {
          font-size: 24px;
        }

        .dashboard {
          padding: 8px 0;
        }

        .metric-number {
          font-size: 25px;
        }
      }
    "))
  )
)

# ==================================================
# 5. SERVER
# ==================================================

server <- function(input, output, session) {

  # Reset filters
  observeEvent(input$reset, {
    updateSelectInput(session, "sex", selected = "All")
    updateSelectInput(session, "educ", selected = "All")
    updateSelectInput(session, "ses", selected = "All")
    updateSelectInput(session, "xvar", selected = "Age")
    updateSelectInput(session, "yvar", selected = "nWBV")
    updateSelectInput(session, "colour", selected = "Educ")
    updateCheckboxInput(session, "trend", value = FALSE)
  })

  # Filter participants
  filtered_data <- reactive({
    dat <- df

    if (input$sex != "All") {
      dat <- dat |> filter(`M/F` == input$sex)
    }

    if (input$educ != "All") {
      dat <- dat |> filter(Educ == input$educ)
    }

    if (input$ses != "All") {
      dat <- dat |> filter(SES == input$ses)
    }

    dat
  })

  # Prepare plot data
  plot_data <- reactive({
    dat <- filtered_data()

    dat <- dat |>
      filter(
        !is.na(.data[[input$xvar]]),
        !is.na(.data[[input$yvar]])
      ) |>
      mutate(
        Group = as.character(.data[[input$colour]]),
        Group = ifelse(
          is.na(Group) | Group == "Missing",
          "Not recorded",
          Group
        ),
        Hover = paste0(
          "<b>Participant: ", ID, "</b>",
          "<br>Age: ", Age,
          "<br>Sex: ", `M/F`,
          "<br>Education: ", Educ,
          "<br>SES: ", SES,
          "<br>MMSE: ", MMSE,
          "<br>Brain volume (nWBV): ", nWBV
        )
      )

    dat
  })

  # Summary cards
  output$n_selected <- renderText({
    nrow(filtered_data())
  })

  output$n_plotted <- renderText({
    nrow(plot_data())
  })

  output$median_age <- renderText({
    ages <- filtered_data()$Age

    if (length(ages) == 0 || all(is.na(ages))) {
      return("N/A")
    }

    round(median(ages, na.rm = TRUE), 1)
  })

  # Colour badge
  output$group_badge <- renderUI({
    group_name <- names(group_variables)[
      match(input$colour, group_variables)
    ]

    tags$span(
      class = "group-pill",
      paste("COLOURED BY", toupper(group_name))
    )
  })

  # Interactive scatter plot
  output$scatter <- renderPlotly({
    dat <- plot_data()

    validate(
      need(
        nrow(dat) > 0,
        "No participants match these filters and axis selections."
      )
    )

    p <- ggplot(
      dat,
      aes(
        x = .data[[input$xvar]],
        y = .data[[input$yvar]],
        colour = Group,
        text = Hover
      )
    ) +
      geom_point(alpha = 0.78, size = 2.8) +
      scale_colour_brewer(
        palette = "Set2",
        na.value = "#A7B2AC"
      ) +
      labs(
        x = names(variables)[match(input$xvar, variables)],
        y = names(variables)[match(input$yvar, variables)],
        colour = "Group"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.background = element_rect(
          fill = "#FFFFFF", colour = NA
        ),
        panel.background = element_rect(
          fill = "#FFFFFF", colour = NA
        ),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(
          colour = "#EDF1EE", linewidth = 0.4
        ),
        axis.title = element_text(
          colour = "#566B62",
          face = "bold",
          size = 11
        ),
        axis.text = element_text(colour = "#78877F"),
        legend.position = "bottom",
        legend.title = element_text(
          face = "bold", size = 10
        ),
        legend.text = element_text(size = 10),
        plot.margin = margin(12, 15, 8, 8)
      )

    if (input$trend && nrow(dat) >= 3) {
      p <- p +
        geom_smooth(
          aes(group = 1),
          method = "lm",
          formula = y ~ x,
          se = FALSE,
          colour = "#344E46",
          linewidth = 1,
          linetype = "dashed"
        )
    }

    ggplotly(p, tooltip = "text") |>
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c("lasso2d", "select2d"),
        toImageButtonOptions = list(
          format = "png",
          filename = "oasis-scatter-plot"
        )
      ) |>
      layout(
        hoverlabel = list(
          bgcolor = "#FFFFFF",
          bordercolor = "#DCE5DF",
          font = list(color = "#263B36")
        ),
        legend = list(
          orientation = "h",
          x = 0,
          y = -0.2
        )
      )
  })

  # Explanation below plot
  output$description <- renderUI({
    x <- names(variables)[match(input$xvar, variables)]
    y <- names(variables)[match(input$yvar, variables)]

    group <- names(group_variables)[
      match(input$colour, group_variables)
    ]

    p(
      paste0(
        "This plot compares ", tolower(y), " with ",
        tolower(x), ". Each dot represents one participant, ",
        "and its colour indicates ", tolower(group), ". ",
        "The demographic filters determine which participants ",
        "are included. Participants missing either selected ",
        "axis measurement are excluded from the plot."
      )
    )
  })
}

# ==================================================
# 6. RUN APP
# ==================================================

shinyApp(ui = ui, server = server)
