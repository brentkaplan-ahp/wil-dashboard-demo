# ----- Libraries -----------------------------------------------
library(shiny)
library(bslib)
library(bsicons)
library(tidyverse)
library(tigris)
library(sf)
library(leaflet)
library(plotly)
library(DT)
library(shiny.emptystate)
library(shinyWidgets)

# ----- Data Loading & Preparation -----------------------------
load("dashboard-data-fake.RData")

addResourcePath(prefix = "static", directoryPath = "static")

# ----- UI ------------------------------------------------------
# my_theme <- bs_theme() |>
#   bs_add_rules(
#     list(
#       sass::sass_file("custom.scss")
#     )
#   )

my_theme <- bs_theme(
  version = 5,
  "navbar-bg" = "#102f47",
  bg = "white",
  fg = "black",
  secondary = "#102f47", # ahp_darker_blue
  info = "#7ea6bf", # ahp_light_blue
  warning = "#f5bd47", # ahp_yellow
  danger = "#ea5a3b" # ahp_tangerine
) |>
  bs_add_variables(
    ahp_dark_blue = "#214a77",
    ahp_darker_blue = "#102f47",
    ahp_tangerine = "#ea5a3b",
    ahp_yellow = "#f5bd47",
    ahp_light_blue = "#7ea6bf",
    ahp_green = "#00aba6"
  )


# ----- CARDS ---------------------------------------------------

valueboxes <- list(
  value_box(
    title = "Total Number of Respondents",
    value = uiOutput("n_respondents"),
    showcase = bs_icon("people-fill"),
    showcase_layout = "top right",
    theme = "bg-secondary",
    class = "rounded-3 border noExpandable",
    full_screen = FALSE
  ),
  value_box(
    title = "Average Respondent Age",
    value = uiOutput("avg_age"),
    theme = "bg-gradient-blue-yellow",
    showcase = plotlyOutput("age_plot"),
    showcase_layout = "top right",
    full_screen = FALSE,
    fill = TRUE,
    height = NULL,
    class = "rounded-3 border noExpandable",
  ),
  value_box(
    title = "Average Respondent's Income",
    value = uiOutput("avg_income"),
    showcase_layout = "top right",
    showcase = bs_icon("cash"),
    theme = "bg-warning",
    class = "rounded-3 border noExpandable",
    full_screen = FALSE
  )
)


ui <- tagList(
  use_empty_state(),
  page_navbar(
    theme = my_theme,
    fillable = FALSE,
    footer = includeHTML("footer.html"),
    title = tagList(
      tags$img(
        src = "static/AHP_Logo_Secondary.png",
        height = "50px",
        style = "margin-right:10px;"
      ),
      "Workforce Innovation Lab Data Dashboard"
    ),
    sidebar = sidebar(
      title = "Sidebar",
      open = FALSE,
      shinyWidgets::virtualSelectInput(
        "state",
        "State",
        choices = unique(states_sf$NAME),
        selected = c("West Virginia", "Ohio", "Pennsylvania"),
        multiple = TRUE
      )
    ),
    nav_spacer(),
    nav_panel(
      "Outcomes",
      div(
        id = "outcomes-page",
        layout_columns(
          row_heights = c(1),
          valueboxes[[1]],
          valueboxes[[2]],
          valueboxes[[3]]
        ),
        navset_card_tab(
          title = "How likely are respondents to quit their job?",
          nav_panel(
            full_screen = TRUE,
            card_header("Map"),
            card_body(leafletOutput("intent_to_quit_map", height = "600px")),
            card_footer(
              "Percentage of respondents within each state who indicated they were likely or
very likely to quit their job within the next month. No one from
Maryland indicated they were likely or very likely to quit their job
within the next month."
            )
          ),
          nav_panel(
            full_screen = TRUE,
            card_header("Figure"),
            card_body(plotlyOutput("intent_to_quit_plot", height = "600px")),
            card_footer(
              "Percentage of respondents within each state who indicated they were likely or
very likely to quit their job within the next month. No one from
Maryland indicated they were likely or very likely to quit their job
within the next month."
            )
          ),
          nav_panel(
            full_screen = TRUE,
            card_header("Table"),
            card_body(DT::DTOutput("intent_to_quit_table")),
            card_footer(
              "Table showing the percentage of respondents within each state who indicated they were likely or
very likely to quit their job within the next month. No one from
Maryland indicated they were likely or very likely to quit their job
within the next month."
            )
          )
        ),
        navset_card_tab(
          title = "How burned out are respondents?",
          nav_panel(
            full_screen = TRUE,
            card_header("Figure"),
            card_body(plotlyOutput("burnout_plot")),
            card_footer(
              "Plot showing the percentage of respondents within each state who indicated they were burned out."
            )
          ),
          nav_panel(
            full_screen = TRUE,
            card_header("Table"),
            card_body(DT::DTOutput("burnout_table")),
            card_footer(
              "Table showing the percentage of respondents within each state who indicated they were burned out."
            )
          )
        )
      )
    ),
    nav_panel(
      "Factors",
      div(
        id = "factors-page",
        navset_card_tab(
          title = "Perceptions of Psychological Safety",
          nav_panel(
            full_screen = TRUE,
            card_header("Figure"),
            card_body(plotlyOutput("psych_safety_plot"))
          ),
          nav_panel(
            full_screen = TRUE,
            card_header("Table"),
            card_body(DT::DTOutput("psych_safety_table"))
          )
        ),
        navset_card_tab(
          title = "Experience with Workplace Harassment",
          nav_panel(
            full_screen = TRUE,
            card_header("Figure"),
            card_body(plotlyOutput("harassment_plot")),
            card_footer(
              "Plot showing the percentage of respondents within each state who indicated they have or have not experienced harassment."
            )
          ),
          nav_panel(
            full_screen = TRUE,
            card_header("Table"),
            card_body(DT::DTOutput("harassment_table")),
            card_footer(
              "Table showing the percentage of respondents within each state who indicated they have or have not experienced harassment."
            )
          )
        ),
        card(
          card_header("Onboarding Experience"),
          card_body(
            layout_column_wrap(
              plotlyOutput("onboarding_plot"),
              DT::DTOutput("onboarding_table")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # bs_themer()
  selected_state <- reactive({
    states_sf |>
      filter(NAME %in% input$state)
  })

  dat_r <- reactive({
    dat |>
      filter(state %in% selected_state()$NAME)
  })

  empty_ui <- empty_state_component(
    content = icon("map-marker-alt", class = "fa-3x"),
    title = "No State Selected",
    subtitle = "Please select at least one state to view the data."
  )

  esm_outcomes <- EmptyStateManager$new(
    id = "outcomes-page",
    html_content = empty_ui
  )

  esm_factors <- EmptyStateManager$new(
    id = "factors-page",
    html_content = empty_ui
  )

  observe({
    if (
      is.null(input$state) ||
        length(input$state) == 0 ||
        nrow(dat_r()) == 0
    ) {
      esm_outcomes$show()
      esm_factors$show()
    } else {
      esm_outcomes$hide()
      esm_factors$hide()
    }
  })

  output$n_respondents <- renderUI({
    req(nrow(dat_r()) > 0)
    nresp <- dat_r() |>
      nrow()
    htmltools::p(
      nresp,
      style = "font-size: 2rem;"
    )
  })

  output$avg_age <- renderUI({
    avgage <- dat_r() |>
      pull(age) |>
      mean() |>
      round(0)
    htmltools::p(
      paste0(avgage, " years old"),
      style = "font-size: 2rem;"
    )
  })

  output$age_plot <- renderPlotly({
    dat_r() |>
      plotly_time_series(
        x = ~age
      )
  })

  output$avg_income <- renderUI({
    avgincome <- dat_r() |>
      pull(income) |>
      mean() |>
      round(0)
    htmltools::p(
      scales::label_currency()(avgincome),
      style = "font-size: 2rem;"
    )
  })

  output$intent_to_quit_map <- renderLeaflet({
    map_data <- map_data |>
      filter(NAME %in% selected_state()$NAME) |>
      mutate(pct = pct / 100)

    pal <- colorNumeric(palette = "Blues", domain = map_data$pct)

    leaflet(map_data) |>
      addProviderTiles("CartoDB.Positron") |>
      addPolygons(
        fillColor = ~ pal(pct),
        color = "white",
        weight = 1,
        fillOpacity = 0.8,
        popup = ~ paste0(
          NAME,
          "<br>",
          "Likely / Very Likely: ",
          scales::percent(pct, accuracy = 0.1)
        )
      ) |>
      addLegend(
        pal = pal,
        values = ~pct,
        title = "Likely / Very Likely",
        position = "bottomright",
        labFormat = labelFormat(suffix = "%", transform = function(x) x * 100)
      )
  })

  output$intent_to_quit_plot <- renderPlotly({
    plt <- intent_to_quit |>
      filter(state %in% selected_state()$NAME) |>
      ggplot(aes(
        x = state,
        y = pct,
        fill = formal_intentto_quit,
        text = paste0(
          "State: ",
          state,
          "<br>",
          "Intent: ",
          formal_intentto_quit,
          "<br>",
          "Percent: ",
          scales::percent(pct, accuracy = 0.1)
        )
      )) +
      geom_bar(stat = "identity", position = position_dodge()) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(
        title = "State-Level Comparison of Intent to Quit",
        x = "State",
        y = "Percentage",
        fill = "Intent to Quit Category"
      ) +
      theme_minimal()

    plotly::ggplotly(plt, tooltip = "text")
  })

  output$intent_to_quit_table <- renderDT({
    intent_to_quit |>
      filter(state %in% selected_state()$NAME) |>
      mutate(pct = scales::percent_format(accuracy = 0.1)(pct)) |>
      rename(
        "State" = "state",
        "Intent to Quit" = "formal_intentto_quit",
        "Percent" = "pct"
      ) |>
      DT::datatable(
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          pageLength = 10
        ),
        rownames = FALSE,
        class = 'stripe hover row-border'
      )
  })

  # Burnout

  # Shared reactive data prep
  burnout_data <- reactive({
    burnout_levels <- c(
      "Never",
      "A few times a year or less",
      "Once a month or less",
      "A few times a month",
      "Once a week",
      "A few times a week",
      "Daily"
    )

    single_item |>
      filter(state %in% selected_state()$NAME) |>
      mutate(
        single_item_burnout = factor(
          single_item_burnout,
          levels = burnout_levels
        )
      ) |>
      arrange(state, single_item_burnout)
  })

  # Define palette once
  burnout_colors <- c(
    "Never" = "#214a77",
    "A few times a year or less" = "#7ea6bf",
    "Once a month or less" = "#00aba6",
    "A few times a month" = "#f5bd47",
    "Once a week" = "#ea5a3b",
    "A few times a week" = "#7ea6bf",
    "Daily" = "#214a77"
  )

  output$burnout_plot <- renderPlotly({
    # Create plot
    plot_ly(
      data = burnout_data(),
      x = ~pct,
      y = ~state,
      color = ~single_item_burnout,
      colors = burnout_colors,
      type = "bar",
      orientation = "h",
      text = ~ paste0(
        single_item_burnout,
        ": ",
        scales::percent(pct, accuracy = 0.1)
      ),
      hoverinfo = "text",
      textposition = "none"
    ) |>
      layout(
        barmode = "stack",
        title = list(text = "Burnout Frequency by State", x = 0),
        xaxis = list(title = "Percentage", tickformat = ".0%"),
        yaxis = list(title = "", categoryorder = "total ascending"),
        legend = list(title = list(text = "Burnout Level")),
        margin = list(l = 50, r = 10, t = 50, b = 50)
      )
  })

  output$burnout_table <- renderDT({
    burnout_data() |>
      rename(
        "State" = state,
        "Burnout Frequency" = single_item_burnout,
        "Percentage" = pct,
        "Count" = n
      ) |>
      mutate(
        "Percentage" = scales::percent(Percentage, accuracy = 0.1)
      ) |>
      DT::datatable(
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          pageLength = 10
        ),
        rownames = FALSE,
        class = 'stripe hover row-border'
      )
  })

  # Psychological Safety
  safety_data <- reactive({
    safety$response <- factor(
      safety$response,
      levels = c(
        "Strongly disagree",
        "Disagree",
        "Neither Agree nor Disagree",
        "Agree",
        "Strongly agree"
      )
    )

    safety |>
      filter(state %in% selected_state()$NAME)
  })

  output$psych_safety_plot <- renderPlotly({
    plt <- ggplot(
      safety_data(),
      aes(
        x = pct,
        y = state,
        fill = response,
        text = paste(
          "Question: ",
          question,
          "<br>",
          "State: ",
          state,
          "<br>",
          "Response: ",
          response,
          "<br>",
          "Percent: ",
          scales::percent(pct, accuracy = 0.1)
        )
      )
    ) +
      geom_col(position = position_stack(reverse = TRUE), color = "white") +
      facet_wrap(~question, ncol = 1, scales = "free_y") +
      scale_fill_brewer(palette = "RdYlGn", direction = 1) +
      scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(
        title = "Comparison of Responses by State for Each Question",
        x = "Percentage",
        y = "State",
        fill = "Response Level"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "bottom",
        axis.text.y = element_text(size = 10),
        strip.background = element_rect(fill = "gray90", color = NA),
        strip.text = element_text(face = "bold")
      )

    # Render with custom tooltip
    ggplotly(plt, tooltip = "text")
  })

  output$psych_safety_table <- renderDataTable({
    safety_data() |>
      mutate(pct = scales::percent_format(accuracy = 0.1)(pct)) |>
      rename(
        "State" = "state",
        "Statement" = "question",
        "Response" = "response",
        "Percent" = "pct",
        "Count" = "n"
      ) |>
      DT::datatable(
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          pageLength = 10
        ),
        rownames = FALSE,
        class = 'stripe hover row-border'
      )
  })

  # Workplace Harassment
  harassment_data <- reactive({
    harassment |>
      filter(state %in% selected_state()$NAME)
  })

  output$harassment_plot <- renderPlotly({
    plt <- harassment_data() |>
      ggplot(aes(
        x = pct,
        y = state,
        fill = response,
        text = paste(
          "Person: ",
          question,
          "<br>",
          "State: ",
          state,
          "<br>",
          "Response: ",
          response,
          "<br>",
          "Percent: ",
          scales::percent(pct, accuracy = 0.1)
        )
      )) +
      geom_col(orientation = "y") +
      facet_wrap(~question, ncol = 1, scales = "free_y") +
      scale_fill_manual(values = c("Yes" = "#ea5a3b", "No" = "#00aba6")) +
      scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(
        title = "Did you experience harassment from the following people?",
        x = "Percentage",
        y = "State",
        fill = "Response Level"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "bottom",
        axis.text.y = element_text(size = 10),
        strip.background = element_rect(fill = "gray90", color = NA),
        strip.text = element_text(face = "bold")
      ) +
      guides(fill = guide_legend(reverse = TRUE))

    # Render with custom tooltip
    ggplotly(plt, tooltip = "text")
  })

  output$harassment_table <- renderDT({
    harassment_data() |>
      mutate(pct = scales::percent_format(accuracy = 0.1)(pct)) |>
      rename(
        "State" = "state",
        "From Whom" = "question",
        "Response" = "response",
        "Percent" = "pct",
        "Count" = "n"
      ) |>
      DT::datatable(
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          pageLength = 10
        ),
        rownames = FALSE,
        class = 'stripe hover row-border'
      )
  })

  # onboarding data
  onboarding_data <- reactive({
    onboarding |>
      filter(state %in% selected_state()$NAME) |>
      mutate(
        response = factor(
          response,
          levels = c(
            "Very dissatisfied",
            "Dissatisfied",
            "Neutral",
            "Satisfied",
            "Very satisfied"
          )
        )
      )
  })

  output$onboarding_plot <- renderPlotly({
    plt <- onboarding_data() |>
      mutate(response = fct_rev(response)) |>
      ggplot(aes(
        x = response,
        y = pct,
        fill = response,
        text = paste(
          "State: ",
          state,
          "<br>",
          "Response: ",
          response,
          "<br>",
          "Percent: ",
          scales::percent(pct, accuracy = 0.1)
        )
      )) +
      geom_col() +
      scale_fill_viridis_d(option = "plasma", direction = -1) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(
        title = "Onboarding Experience",
        x = "Response",
        y = "Percentage",
        fill = "Response"
      ) +
      theme_minimal(base_size = 12) +
      facet_wrap(~state, ncol = 2) +
      theme(
        legend.position = "none",
        axis.text.y = element_text(size = 10),
        strip.background = element_rect(fill = "gray90", color = NA),
        strip.text = element_text(face = "bold")
      )

  ggplotly(plt, tooltip = "text")
  })

  output$onboarding_table <- renderDataTable({
    onboarding_data() |>
      rename(
        "State" = "state",
        "Response" = "response",
        "Percent" = "pct",
        "Count" = "n"
      ) |>
      mutate(Percent = scales::percent(Percent, accuracy = 1)) |>
      DT::datatable(
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          pageLength = 10
        ),
        rownames = FALSE,
        class = 'stripe hover row-border'
      )
  })

  plotly_time_series <- function(d, x) {
    info <- getCurrentOutputInfo()
    large <- isTRUE(info$height() > 200)

    plotly::plot_ly(
      dat,
      x = ~age,
      color = I(info$fg()),
      type = "histogram"
    ) %>%
      layout(
        hovermode = "x",
        margin = list(t = 0, r = 0, l = 0, b = 0),
        font = list(color = info$fg()),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent",
        xaxis = list(
          title = "Age",
          visible = "large",
          showgrid = FALSE
        ),
        yaxis = list(
          title = "",
          visible = "large",
          showgrid = FALSE
        )
      ) %>%
      config(displayModeBar = FALSE)
  }
}

# ----- Run App --------------------------------------------------
shinyApp(ui, server)
