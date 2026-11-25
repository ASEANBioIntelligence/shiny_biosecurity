library(shiny)
library(bslib)
library(tidyverse)
library(DT)

source(file.path("R", "biosecurity.R"), local = TRUE)

dashboard_table <- function(data) {
  data <- data |> mutate(across(where(~ inherits(.x, "Date")), as.character))
  DT::datatable(data, rownames = FALSE, filter = "top", selection = "none", escape = TRUE,
    options = list(pageLength = 25,
      lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "All")),
      scrollX = TRUE, scrollY = "55vh", scrollCollapse = TRUE, order = list()))
}

ui <- fluidPage(
  theme = bs_theme(version = 5, primary = "#16745D", base_font = "system-ui"),
  tags$head(tags$style(HTML("
    body { background:#f5f7f6; color:#202725; }
    .container-fluid { max-width:1600px; padding:24px; }
    h2 { font-size:26px; } .well { background:white; border-radius:6px; }
    .metrics { display:flex; gap:32px; flex-wrap:wrap; margin:20px 0; }
    .metric strong { display:block; font-size:26px; }
    .metric span,.status { color:#56615b; font-size:13px; }
    .status { margin:12px 0; overflow-wrap:anywhere; }
    .tab-content { padding-top:16px; min-width:0; }
    .download-link { margin-top:12px; }
    @media(max-width:767px) { .container-fluid { padding:12px; } }
  "))),
  titlePanel("BlueDot Biosecurity"),
  sidebarLayout(
    sidebarPanel(width = 3,
      dateInput("start_date", "Start date", value = Sys.Date() - 1, max = Sys.Date()),
      dateInput("end_date", "End date", value = Sys.Date() - 1, max = Sys.Date()),
      helpText("Select up to five consecutive days, including both dates."),
      selectInput("country_scope", "Countries",
        choices = c("ASEAN" = "asean", "All countries" = "all", "Selected countries" = "selected"), selected = "asean"),
      conditionalPanel("input.country_scope == 'selected'",
        selectizeInput("country_ids", "Select countries", choices = bluedot_id_choices("country"),
          multiple = TRUE, options = list(placeholder = "Search country names or IDs"))),
      conditionalPanel("input.country_scope != 'all'",
        checkboxInput("only_selected_countries", "Exclude stories also tagged to other countries", TRUE)),
      helpText("Country filters check returned location tags. Clear the checkbox to include stories that also mention other countries."),
      selectInput("disease_classification", "Disease classification",
        choices = c("Human and animal diseases" = "all", "Human diseases" = "human", "Animal diseases" = "animal"),
        selected = "all"),
      selectizeInput("disease_ids", "Diseases (any match)", choices = bluedot_id_choices("disease"),
        multiple = TRUE, options = list(placeholder = "All diseases")),
      helpText("Classification and disease selections filter returned records using the new disease catalog. Diseases listed in both classifications can match either filter."),
      selectizeInput("tags", "Tags (any match)", choices = character(), multiple = TRUE,
        options = list(placeholder = "All tags", create = TRUE)),
      actionButton("refresh_tags", "Refresh tags", icon = icon("arrows-rotate")),
      div(class = "status", textOutput("tag_status")),
      actionButton("fetch", "Fetch articles", icon = icon("download"), class = "btn-primary"),
      hr(),
      downloadButton("download", "Download CSV")
    ),
    mainPanel(width = 9,
      div(class = "status", textOutput("status")),
      uiOutput("metrics"),
      tabsetPanel(id = "view",
        tabPanel("Daily articles", value = "daily", DT::DTOutput("articles")),
        tabPanel("Clusters", value = "clusters", DT::DTOutput("clusters")),
        tabPanel("Cluster sources", value = "sources",
          tags$p("Source articles follow the matching clusters. cluster_matched_countries describes the parent cluster; individual source articles may cover other countries."),
          DT::DTOutput("sources")),
        tabPanel("Fetch status", value = "status", DT::DTOutput("fetch_status"))
      )
    )
  )
)

server <- function(input, output, session) {
  active_country_ids <- reactive({
    if (identical(input$country_scope, "all")) return(NULL)
    if (identical(input$country_scope, "selected")) return(as.character(input$country_ids))
    bluedot_asean_ids
  })
  fetched_country_ids <- reactiveVal(NULL)
  disease_classification <- reactive({
    if (is.null(input$disease_classification)) "all" else input$disease_classification
  })
  observeEvent(disease_classification(), {
    choices <- bluedot_id_choices("disease", disease_classification())
    selected <- intersect(isolate(input$disease_ids), unname(choices))
    updateSelectizeInput(session, "disease_ids", choices = choices, selected = selected)
  })
  results <- reactiveVal(NULL)
  status <- reactiveVal("Choose countries and dates, then fetch | Maximum five consecutive days")
  tag_status <- reactiveVal("")
  catalog <- reactiveVal(character())
  safe_message <- function(e) {
    message <- conditionMessage(e)
    if (!grepl("^(HTTP |Choose |Configure |BlueDot )", message)) {
      message <- "Request failed. Check network access and the server's BlueDot subscription."
    }
    message
  }
  update_tags <- function(values) {
    values <- values[!is.na(values) & nzchar(values)]
    catalog(sort(unique(c(catalog(), values))))
    updateSelectizeInput(session, "tags", choices = sort(unique(c(catalog(), input$tags))),
                         selected = input$tags)
  }
  observeEvent(input$refresh_tags, {
    tryCatch({
      update_tags(fetch_tag_catalog(biosecurity_key()))
      tag_status(sprintf("%s tags available", length(catalog())))
    }, error = function(e) tag_status(safe_message(e)))
  }, ignoreNULL = FALSE)

  observeEvent(input$fetch, {
    tryCatch({
      dates <- validate_dates(input$start_date, input$end_date)
      if (any(dates > Sys.Date())) stop("Choose dates no later than today.", call. = FALSE)
      days <- format(seq(dates[1], dates[2], by = "day"), "%Y-%m-%d")
      all_countries <- identical(input$country_scope, "all")
      location_ids <- if (all_countries) NULL else ams_location_ids
      country_scope <- if (all_countries) "All countries" else "ASEAN"
      if (identical(input$country_scope, "selected")) {
        if (!length(input$country_ids) || any(!input$country_ids %in% bluedot_countries$country_id)) {
          stop("Choose at least one country from the catalog.", call. = FALSE)
        }
        location_ids <- input$country_ids
        country_scope <- paste(bluedot_countries$country_name[
          match(location_ids, bluedot_countries$country_id)], collapse = ", ")
      }
      key <- biosecurity_key()
      results(NULL)
      value <- withProgress(message = "Fetching BlueDot biosecurity", value = 0, {
        daily <- fetch_feed("/articles/biosecurity/", days, key, location_ids = location_ids)
        incProgress(0.45)
        clustered <- fetch_feed("/clusters_biosec/", days, key, location_ids = location_ids)
        incProgress(0.45)
        list(articles = tidy_articles(daily), clusters = tidy_clusters(clustered),
          sources = tidy_cluster_articles(clustered),
          status = bind_rows(feed_status(daily, "Daily articles"),
                             feed_status(clustered, "Clustered biosecurity")))
      })
      results(value)
      fetched_country_ids(if (is.null(location_ids)) NULL else as.character(location_ids))
      values <- unlist(strsplit(c(value$articles$tags, value$clusters$tags), "; ", fixed = TRUE))
      update_tags(values)
      failed <- sum(value$status$status == "failed")
      partial <- sum(value$status$status == "possibly truncated")
      status(sprintf("%s to %s | Fetched scope: %s | %s failed requests | %s possibly truncated | Retrieved %s",
        days[1], tail(days, 1), country_scope, failed, partial, format(Sys.time(), "%Y-%m-%d %H:%M %Z")))
      if (failed > 0) showNotification("Some requests failed. See Fetch status.", type = "warning")
    }, error = function(e) {
      status(safe_message(e))
      showNotification(safe_message(e), type = "error")
    })
  })

  articles <- reactive({
    req(results())
    data <- matching_biosecurity_countries(results()$articles, active_country_ids(),
                                           only_selected = isTRUE(input$only_selected_countries))
    matching_bluedot_diseases(matching_tags(data, input$tags), input$disease_ids,
                              classification = disease_classification())
  })
  clusters <- reactive({
    req(results())
    data <- matching_biosecurity_countries(results()$clusters, active_country_ids(),
                                           only_selected = isTRUE(input$only_selected_countries))
    matching_bluedot_diseases(matching_tags(data, input$tags), input$disease_ids,
                              classification = disease_classification())
  })
  sources <- reactive({
    req(results())
    results()$sources |> inner_join(clusters() |> select(cluster_id, fetched_date,
      cluster_matched_countries = matched_countries) |> distinct(cluster_id, fetched_date, .keep_all = TRUE),
      by = c("cluster_id", "fetched_date"))
  })
  output$status <- renderText({
    if (is.null(results())) return(status())
    countries <- active_country_ids()
    scope <- if (is.null(countries)) "All countries" else if (identical(input$country_scope, "selected")) {
      if (!length(countries)) "Choose at least one country" else paste(
        bluedot_countries$country_name[match(countries, bluedot_countries$country_id)], collapse = ", ")
    } else "ASEAN"
    mode <- if (is.null(countries)) "no country restriction" else if (isTRUE(input$only_selected_countries)) {
      "selected countries only"
    } else "any selected country"
    notice <- if (!setequal(countries, fetched_country_ids())) {
      "Country selection changed; fetch again for complete results in this scope."
    } else ""
    paste(c(status(), paste0("Showing: ", scope, " (", mode, ")"), notice[nzchar(notice)]), collapse = " | ")
  })
  output$tag_status <- renderText(tag_status())
  output$metrics <- renderUI({
    req(results())
    metric <- function(value, label) div(class = "metric", strong(value), span(label))
    count <- function(data, feed) {
      failed <- results()$status |> filter(.data$feed == .env$feed)
      if (all(failed$status == "failed")) "Unavailable" else nrow(data)
    }
    div(class = "metrics",
      metric(count(articles(), "Daily articles"), "Daily articles"),
      metric(count(clusters(), "Clustered biosecurity"), "Cluster snapshots"),
      metric(count(sources(), "Clustered biosecurity"), "Cluster-source links"))
  })
  output$articles <- DT::renderDT({
    dashboard_table(articles() |> select(published, headline, tags, diseases, disease_classification, matched_countries, locations,
      source, summary, url, everything()))
  }, server = FALSE)
  output$clusters <- DT::renderDT({
    dashboard_table(clusters() |> select(cluster_date, title, tags, diseases, disease_classification, matched_countries, countries,
      summary, cluster_size, everything()))
  }, server = FALSE)
  output$sources <- DT::renderDT({ dashboard_table(sources()) }, server = FALSE)
  output$fetch_status <- DT::renderDT({ req(results()); dashboard_table(results()$status) }, server = FALSE)
  export_data <- reactive({
    req(results())
    switch(input$view, daily = articles(), clusters = clusters(), sources = sources(),
           status = results()$status, articles())
  })
  output$download <- downloadHandler(
    filename = function() paste0(format(Sys.Date(), "%Y%m%d"), "_biosecurity_", input$view, ".csv"),
    content = function(file) write.csv(export_data(), file, row.names = FALSE, na = "")
  )
}

shinyApp(ui, server)
