local({
  source_paths <- lapply(sys.frames(), function(frame) {
    path <- frame$ofile
    if (is.null(path)) path <- frame$file
    if (is.character(path) && length(path) == 1L && !is.na(path)) path else character()
  })
  source_dirs <- dirname(rev(unlist(source_paths, use.names = FALSE)))
  candidates <- unique(c(file.path(source_dirs, "bluedot_lookups.R"),
                         file.path("R", "bluedot_lookups.R"), "bluedot_lookups.R"))
  helper_paths <- candidates[file.exists(candidates)]
  if (!length(helper_paths)) {
    stop("Cannot locate bluedot_lookups.R. Include both BlueDot lookup files in the app's R folder.", call. = FALSE)
  }
  source(helper_paths[1], local = parent.env(environment()))
})

base_url <- "https://developer.bluedot.global/daas"

biosecurity_key <- function() {
  key_names <- c("BLUEDOT_BIOSECURITY_API_KEY", "BLUEDOT_API_KEY")
  existing <- Sys.getenv(key_names, unset = "")
  for (path in c(path.expand("~/.Renviron"), "../.Renviron", ".Renviron", "secrets.Renviron")) {
    if (file.exists(path)) readRenviron(path)
  }
  if (any(nzchar(existing))) do.call(Sys.setenv, as.list(existing[nzchar(existing)]))
  keys <- Sys.getenv(key_names, unset = "")
  if (!any(nzchar(keys))) stop("Configure a BlueDot biosecurity key on the server.", call. = FALSE)
  unname(keys[which(nzchar(keys))[1]])
}

fetch_tag_catalog <- function(api_key) {
  response <- httr::GET(paste0(base_url, "/lookup/articles/tags/"),
    query = list(`api-version` = "v1"),
    httr::add_headers("Ocp-Apim-Subscription-Key" = api_key), httr::timeout(30))
  status <- httr::status_code(response)
  if (status >= 400) stop(sprintf("HTTP %s loading tags. Check the server's biosecurity subscription.", status), call. = FALSE)
  payload <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
  values <- map_chr(payload$data %||% list(), function(group) collapse_names(group$tags))
  sort(unique(unlist(strsplit(values, "; ", fixed = TRUE))))
}

matching_tags <- function(data, tags) {
  if (!length(tags)) return(data)
  selected <- tolower(trimws(tags))
  keep <- map_lgl(data$tags, function(value) {
    if (is.na(value) || !nzchar(value)) return(FALSE)
    any(tolower(trimws(strsplit(value, "; ", fixed = TRUE)[[1]])) %in% selected)
  })
  data[keep, , drop = FALSE]
}

ams_location_ids <- as.numeric(bluedot_asean_ids)

country_ids_for_locations <- function(locations) {
  if (!length(locations)) return(character())
  if (is.data.frame(locations)) locations <- split(locations, seq_len(nrow(locations)))
  if (is.list(locations) && any(c("id", "locationId", "countryId", "name") %in% names(locations))) {
    locations <- list(locations)
  }
  ids <- unlist(lapply(locations, function(location) {
    if (is.list(location)) {
      country_id <- location$countryId %||% location$country_id
      location_id <- location$locationId %||% location$id
      location_name <- location$countryName %||% location$name %||% location$locationName
      if (length(country_id)) return(as.character(country_id))
      feature <- location$featureCode %||% location$feature_code
      type <- location$locationType %||% location$location_type
      if (length(location_id) && (any(feature %in% c("PCL", "PCLI", "PCLD", "PCLF", "PCLS")) || any(type %in% 6))) {
        return(as.character(location_id))
      }
    } else {
      location_id <- as.character(location)
      location_name <- as.character(location)
    }
    mapped <- bluedot_locations$country_id[
      match(as.character(location_id), bluedot_locations$location_id)]
    mapped <- mapped[!is.na(mapped)]
    if (length(mapped)) return(mapped)
    aliases <- c("brunei darussalam" = "Brunei", "viet nam" = "Vietnam", "lao pdr" = "Laos",
                 "lao people's democratic republic" = "Laos", "timor-leste" = "Timor Leste")
    name <- tolower(trimws(as.character(location_name)))
    aliased <- name %in% names(aliases)
    name[aliased] <- tolower(unname(aliases[name[aliased]]))
    bluedot_countries$country_id[match(name, tolower(bluedot_countries$country_name))]
  }), use.names = FALSE)
  unique(ids[!is.na(ids) & nzchar(ids)])
}

matching_biosecurity_countries <- function(data, country_ids, only_selected = FALSE) {
  selected <- as.character(country_ids)
  row_ids <- strsplit(data$country_ids, "; ", fixed = TRUE)
  row_ids <- lapply(row_ids, function(ids) ids[!is.na(ids) & nzchar(ids)])
  keep <- vapply(row_ids, function(ids) {
    if (is.null(country_ids)) return(TRUE)
    length(ids) > 0L && any(ids %in% selected) && (!only_selected || all(ids %in% selected))
  }, logical(1))
  data$matched_countries <- vapply(row_ids, function(ids) {
    if (!is.null(country_ids)) ids <- intersect(ids, selected)
    names <- bluedot_countries$country_name[match(ids, bluedot_countries$country_id)]
    paste(unique(names[!is.na(names)]), collapse = "; ")
  }, character(1))
  data[keep, , drop = FALSE]
}

validate_dates <- function(start, end) {
  dates <- as.Date(c(start, end))
  if (length(dates) != 2L || anyNA(dates) || dates[1] > dates[2] ||
      as.integer(dates[2] - dates[1]) > 4L) {
    stop("Choose one to five consecutive days (inclusive).", call. = FALSE)
  }
  dates
}

bluedot_get <- function(path, day, api_key, location_ids = ams_location_ids) {
  if (!startsWith(path, "/") || startsWith(path, "//")) {
    stop("Use an API path beginning with one slash.", call. = FALSE)
  }
  query <- list(
    startDate = day, endDate = day,
    affectedCategoryIds = "1,2", includeBody = "FALSE",
    includeDuplicates = "FALSE", format = "json", `api-version` = "v1"
  )
  if (length(location_ids)) query$locationIds <- paste(location_ids, collapse = ",")
  response <- httr::RETRY(
    "GET", paste0(base_url, path), query = query,
    httr::add_headers(
      "Ocp-Apim-Subscription-Key" = api_key, "Cache-Control" = "no-cache"
    ),
    httr::timeout(90), times = 3, pause_min = 1, pause_cap = 5,
    terminate_on = c(400, 401, 403, 404), quiet = TRUE
  )
  status <- httr::status_code(response)
  if (status >= 400) {
    detail <- switch(as.character(status),
      `401` = "Check the configured key and its subscription.",
      `403` = "Check access to this endpoint.",
      `404` = "Check the API path against your BlueDot portal.",
      "Check the query parameters and service availability.")
    stop(sprintf("HTTP %s for %s. %s", status, path, detail), call. = FALSE)
  }
  payload <- jsonlite::fromJSON(
    httr::content(response, as = "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )
  if (!is.list(payload) || !"data" %in% names(payload)) {
    stop("BlueDot returned an unexpected response envelope.", call. = FALSE)
  }
  records <- payload$data %||% list()
  if (!is.list(records) || (length(records) &&
      (!is.null(names(records)) || !all(map_lgl(records, is.list))))) {
    stop("Expected data to contain an array of records.", call. = FALSE)
  }
  payload$data <- records
  payload
}

fetch_feed <- function(path, days, api_key, location_ids = ams_location_ids) {
  set_names(map(days, function(day) {
    tryCatch({
      payload <- bluedot_get(path, day, api_key, location_ids = location_ids)
      metadata <- payload$metadata %||% list()
      token <- metadata$continuationToken %||% payload$continuationToken
      partial <- length(payload$data) >= 1000L ||
        (length(token) > 0L && any(nzchar(as.character(token))))
      list(data = payload$data, metadata = metadata,
           status = if (partial) "possibly truncated" else "ok",
           detail = if (partial) "Record cap or continuation token returned; completeness is not confirmed." else "")
    }, error = function(e) {
      detail <- conditionMessage(e)
      if (!grepl("^(HTTP |BlueDot |Expected |Use an)", detail)) {
        detail <- "Request or JSON parsing failed. Check network access and API availability."
      }
      list(data = list(), metadata = list(), status = "failed", detail = detail)
    })
  }), days)
}

feed_status <- function(feed, name) {
  imap_dfr(feed, function(result, day) {
    tibble(feed = name, date = as.Date(day), records = length(result$data),
           status = result$status, detail = result$detail)
  })
}

scalar_text <- function(record, field) {
  value <- record[[field]]
  if (is.null(value) || !length(value)) return(NA_character_)
  as.character(value[[1]])
}

collapse_names <- function(items) {
  if (is.null(items) || !length(items)) return("")
  if (!is.list(items)) return(paste(unique(as.character(items)), collapse = "; "))
  values <- map_chr(items, function(item) {
    if (is.list(item)) {
      value <- item$name %||% item$tagName %||% item$locationName
      if (is.null(value)) return(as.character(jsonlite::toJSON(item, auto_unbox = TRUE)))
      return(as.character(value[[1]]))
    }
    as.character(item)
  })
  paste(unique(values[!is.na(values) & nzchar(values)]), collapse = "; ")
}

empty_articles <- function() {
  tibble(article_id = character(), published = as.Date(character()),
    fetched_date = as.Date(character()), source = character(), language = character(),
    headline = character(), summary = character(), diseases = character(),
    locations = character(), country_ids = character(), categories = character(), tags = character(),
    law_enforcement = character(), url = character())
}

tidy_articles <- function(feed) {
  rows <- imap(feed, function(result, day) {
    map(result$data, function(record) {
      translated <- na_if(scalar_text(record, "articleHeadlineTranslated"), "")
      tibble(
        article_id = scalar_text(record, "articleId"),
        published = as.Date(substr(scalar_text(record, "publishedTimestamp"), 1, 10)),
        fetched_date = as.Date(day), source = scalar_text(record, "sourceName"),
        language = scalar_text(record, "originalLanguageIsoCode"),
        headline = coalesce(translated, scalar_text(record, "articleHeadline")),
        summary = scalar_text(record, "articleSummary"),
        diseases = collapse_names(record$diseases),
        locations = collapse_names(record$locations),
        country_ids = paste(country_ids_for_locations(record$locations), collapse = "; "),
        categories = collapse_names(record$affectedCategories),
        tags = collapse_names(record$tags),
        law_enforcement = scalar_text(record, "lawEnforcementInvolvement"),
        url = scalar_text(record, "sourceUrl")
      )
    }) |> bind_rows()
  }) |> bind_rows()
  rows <- bind_rows(empty_articles(), rows)
  # Missing IDs must not collapse unrelated records into a single row.
  identified <- rows |> filter(!is.na(article_id), article_id != "") |>
    distinct(article_id, .keep_all = TRUE)
  unidentified <- rows |> filter(is.na(article_id) | article_id == "") |> distinct()
  bind_rows(identified, unidentified) |> arrange(desc(published), headline) |>
    classify_bluedot_diseases()
}

tidy_clusters <- function(feed) {
  empty <- tibble(cluster_id = character(), cluster_date = as.Date(character()),
    fetched_date = as.Date(character()), cluster_size = integer(), title = character(),
    summary = character(), diseases = character(), countries = character(), country_ids = character(),
    sublocations = character(), categories = character(), tags = character(),
    source_articles_returned = integer())
  rows <- imap_dfr(feed, function(result, day) {
    map_dfr(result$data, function(record) {
      tibble(
        cluster_id = scalar_text(record, "clusterId"),
        cluster_date = as.Date(substr(scalar_text(record, "clusterDate"), 1, 10)),
        fetched_date = as.Date(day),
        cluster_size = as.integer(scalar_text(record, "clusterSize")),
        title = scalar_text(record, "clusterTitle"),
        summary = scalar_text(record, "clusterSummary"),
        diseases = collapse_names(record$diseases),
        countries = collapse_names(record$countryLocations),
        country_ids = paste(country_ids_for_locations(c(record$countryLocations, record$subLocations)), collapse = "; "),
        sublocations = collapse_names(record$subLocations),
        categories = collapse_names(record$affectedCategories),
        tags = collapse_names(record$tags),
        source_articles_returned = length(record$sourceArticles)
      )
    })
  })
  # Keep each day's cluster snapshot; a cluster may recur on both days.
  bind_rows(empty, rows) |> arrange(desc(fetched_date), title) |>
    classify_bluedot_diseases()
}

tidy_cluster_articles <- function(feed) {
  empty <- tibble(cluster_id = character(), fetched_date = as.Date(character()),
    article_id = character(), published = as.Date(character()),
    headline = character(), summary = character(), url = character())
  rows <- imap_dfr(feed, function(result, day) {
    map_dfr(result$data, function(cluster) {
      map_dfr(cluster$sourceArticles %||% list(), function(article) {
        tibble(cluster_id = scalar_text(cluster, "clusterId"), fetched_date = as.Date(day),
          article_id = scalar_text(article, "id"),
          published = as.Date(substr(scalar_text(article, "publishedDate"), 1, 10)),
          headline = scalar_text(article, "title"), summary = scalar_text(article, "summary"),
          url = scalar_text(article, "url"))
      })
    })
  })
  bind_rows(empty, rows) |> distinct() |> arrange(desc(fetched_date), cluster_id, desc(published))
}

browse_table <- function(data) {
  gt(data) |> sub_missing(missing_text = "") |>
    cols_align("left", everything()) |>
    tab_options(table.font.size = px(12), data_row.padding = px(6),
      container.height = px(550), container.overflow.x = TRUE,
      container.overflow.y = TRUE)
}

explode <- function(data, column) {
  data |> separate_longer_delim({{ column }}, "; ") |>
    filter(!is.na({{ column }}), {{ column }} != "")
}

