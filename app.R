# app.R for Shiny app using Kolada API
library(shiny)
library(httr2)
library(dplyr)
library(tibble)
library(rlang)
library(tidyr)

# Internal functions from kolada-client.R and kolada-public.R
.kld_base_url <- "https://api.kolada.se/v3"

.kld_as_df <- function(x) {
  candidates <- list(
    x[["results"]],
    if (!is.null(x[["data"]])) x[["data"]][["results"]] else NULL,
    x[["items"]],
    x[["kpis"]],
    x[["municipalities"]],
    x[["values"]]
  )

  pick_rows <- function(cand) {
    if (is.null(cand)) return(NULL)
    if (is.data.frame(cand)) return(tibble::as_tibble(cand))
    if (is.list(cand) && length(cand) > 0L &&
        all(vapply(cand, function(e) is.list(e) || is.data.frame(e), logical(1)))) {
      df <- tryCatch(dplyr::bind_rows(cand), error = function(e) NULL)
      if (!is.null(df)) return(tibble::as_tibble(df))
    }
    if (is.atomic(cand)) return(tibble::tibble(value = cand))
    NULL
  }

  for (cand in candidates) {
    df <- pick_rows(cand)
    if (!is.null(df)) return(df)
  }
  if (!is.null(x[["results"]]) && length(x[["results"]]) == 0L) {
    return(tibble::tibble())
  }
  tibble::tibble()
}

.kld_get <- function(path, query = list(), base_url = "https://api.kolada.se/v3",
                     page_max = Inf, pause = 0.2) {
  url <- paste0(base_url, path)
  req <- httr2::request(url) |>
    httr2::req_user_agent("LabAPI[](https://github.com/asetzhumatai/LabAPI)") |>
    httr2::req_error(is_error = ~ FALSE) |>
    httr2::req_timeout(15) |>
    httr2::req_retry(max_tries = 3)
  if (length(query)) req <- httr2::req_url_query(req, !!!query)
  resp <- req |> httr2::req_perform()
  if (httr2::resp_status(resp) >= 400) {
    rlang::abort(
      paste("Kolada request failed [", httr2::resp_status(resp), "]: ", path),
      class = "kolada_http_error"
    )
  }
  payload <- httr2::resp_body_json(resp)
  .kld_as_df(payload)
}

kolada_available <- function() {
  tryCatch({
    resp <- httr2::request("https://api.kolada.se/v3/kpi") |>
      httr2::req_url_query(limit = 1) |>
      httr2::req_user_agent("LabAPI[](https://github.com/asetzhumatai/LabAPI)") |>
      httr2::req_timeout(5) |>
      httr2::req_retry(max_tries = 2) |>
      httr2::req_perform()
    status <- httr2::resp_status(resp)
    status >= 200 && status < 300
  }, error = function(e) FALSE)
}

kld_kpis <- function(title = NULL, per_page = 100L, page = 1L) {
  q <- list()
  if (!is.null(title)) q$title <- title
  q$per_page <- per_page
  q$page <- page
  .kld_get("/kpi", query = q, base_url = "https://api.kolada.se/v3")
}

kld_municipalities <- function(title = NULL, region_type = "municipality",
                               per_page = 290L, page = 1L) {
  q <- list()
  if (!is.null(title)) q$title <- title
  if (!is.null(region_type)) q$region_type <- region_type
  q$per_page <- per_page
  q$page <- page
  .kld_get("/municipality", query = q, base_url = "https://api.kolada.se/v3")
}

kld_values <- function(kpi, municipality, year) {
  stopifnot(!is.null(kpi), !is.null(municipality), !is.null(year))
  combos <- expand.grid(year = year, municipality = municipality, stringsAsFactors = FALSE)
  dfs <- apply(combos, 1, function(row) {
    path <- paste0("/data/kpi/", kpi, "/municipality/", row[["municipality"]], "/year/", row[["year"]])
    .kld_get(path, base_url = "https://api.kolada.se/v2")
  })
  dplyr::bind_rows(dfs)
}

# Shiny UI
ui <- fluidPage(
  titlePanel("Kolada API Interactive Analyzer"),
  sidebarLayout(
    sidebarPanel(
      textInput("kpi_search", "Search KPI by title:", ""),
      selectInput("kpi_id", "Select KPI:", choices = c("None" = "")),
      selectInput("muni_id", "Select Municipality(ies):", choices = NULL, multiple = TRUE),
      sliderInput("years", "Year Range:",
                  min = 2000, max = 2024, value = c(2015, 2024), step = 1),
      actionButton("fetch", "Fetch Data"),
      textOutput("kpi_status")
    ),
    mainPanel(
      tableOutput("data_table")
    )
  )
)

# Shiny Server
server <- function(input, output, session) {
  # Check API availability
  api_available <- reactive({
    kolada_available()
  })

  # Fetch all municipalities (once)
  munis <- reactive({
    if (api_available()) {
      kld_municipalities(region_type = "municipality", per_page = 300L)
    } else {
      tibble(id = character(), title = character())
    }
  })

  # Update municipality selectInput
  observe({
    m <- munis()
    if (nrow(m) > 0) {
      updateSelectInput(session, "muni_id",
                        choices = setNames(m$id, m$title))
    } else {
      updateSelectInput(session, "muni_id", choices = c("None" = ""))
    }
  })

  # Reactive for KPIs based on search
  kpis <- reactive({
    if (!api_available()) {
      return(tibble(id = character(), title = character()))
    }
    if (nchar(trimws(input$kpi_search)) >= 2) {  # Reduced to 2 characters
      tryCatch({
        kld_kpis(title = input$kpi_search, per_page = 200L)
      }, error = function(e) {
        tibble(id = character(), title = character())
      })
    } else {
      kld_kpis(per_page = 50L)  # Fetch a small set of KPIs when search is empty
    }
  })

  # Update KPI selectInput and status message
  output$kpi_status <- renderText({
    k <- kpis()
    if (!api_available()) {
      "Kolada API is unavailable"
    } else if (nrow(k) == 0) {
      "No KPIs found for the search term"
    } else {
      paste("Found", nrow(k), "KPIs")
    }
  })

  observe({
    k <- kpis()
    if (nrow(k) > 0) {
      updateSelectInput(session, "kpi_id",
                        choices = c("Select a KPI" = "", setNames(k$id, k$title)))
    } else {
      updateSelectInput(session, "kpi_id", choices = c("None" = ""))
    }
  })

  # Fetch data on button press
  data <- eventReactive(input$fetch, {
    req(input$kpi_id, input$muni_id, api_available())
    years <- seq(input$years[1], input$years[2])
    raw_data <- tryCatch({
      kld_values(input$kpi_id, input$muni_id, years)
    }, error = function(e) {
      showNotification("Failed to fetch data from Kolada API", type = "error")
      tibble()
    })
    if ("values" %in% names(raw_data)) {
      raw_data <- raw_data %>% tidyr::unnest(values, keep_empty = TRUE)
    }
    raw_data
  })

  # Render table
  output$data_table <- renderTable({
    data()
  })
}

# Run the app
shinyApp(ui = ui, server = server)
