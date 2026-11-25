# Base-R lookups; source this file from reports or use its bundled app copy.
local({
  # source() sets ofile; sys.source() and Shiny's sourceUTF8() use file.
  source_paths <- lapply(sys.frames(), function(frame) {
    path <- frame$ofile
    if (is.null(path)) path <- frame$file
    if (is.character(path) && length(path) == 1L && !is.na(path)) path else character()
  })
  source_dirs <- dirname(rev(unlist(source_paths, use.names = FALSE)))
  candidates <- unique(c(file.path(source_dirs, "bluedot_lookup_data.R"),
                         file.path("R", "bluedot_lookup_data.R"), "bluedot_lookup_data.R"))
  data_paths <- candidates[file.exists(candidates)]
  if (!length(data_paths)) {
    stop("Cannot locate bluedot_lookup_data.R. Keep it beside bluedot_lookups.R in the R folder.", call. = FALSE)
  }
  sys.source(data_paths[1], envir = parent.env(environment()))
})

bluedot_countries <- bluedot_locations[
  bluedot_locations$location_id == bluedot_locations$country_id,
  c("country_id", "country_name"), drop = FALSE
]
rownames(bluedot_countries) <- NULL

bluedot_global_id_conflicts <- bluedot_global_countries[
  duplicated(bluedot_global_countries$country_id) |
    duplicated(bluedot_global_countries$country_id, fromLast = TRUE), , drop = FALSE
]

# Match names as recorded in LocationsIds, retaining the existing 11-country scope.
bluedot_asean_names <- c("Singapore", "Thailand", "Philippines", "Brunei", "Malaysia",
                         "Indonesia", "Cambodia", "Myanmar", "Laos", "Vietnam", "Timor Leste")
bluedot_asean_ids <- bluedot_countries$country_id[
  match(bluedot_asean_names, bluedot_countries$country_name)
]
if (anyNA(bluedot_asean_ids)) stop("ASEAN countries are missing from the location lookup.", call. = FALSE)

bluedot_disease_catalog <- function(classification = c("all", "human", "animal")) {
  classification <- match.arg(classification)
  if (classification == "all") return(bluedot_diseases)
  bluedot_diseases[tolower(bluedot_diseases$disease_classification) == classification, , drop = FALSE]
}

bluedot_id_choices <- function(type = c("country", "location", "disease", "global_country"),
                               disease_classification = c("all", "human", "animal")) {
  type <- match.arg(type)
  disease_classification <- match.arg(disease_classification)
  data <- switch(type, country = bluedot_countries, location = bluedot_locations,
                 disease = bluedot_disease_catalog(disease_classification), global_country = bluedot_global_countries)
  # A disease ID may have separate human and animal records in the export.
  data <- unique(data[1:2])
  id <- data[[1]]
  label <- paste0(data[[2]], " (", id, ")")
  if (type == "disease") {
    catalog <- bluedot_disease_catalog(disease_classification)
    classifications <- vapply(id, function(value) {
      paste(sort(unique(catalog$disease_classification[catalog$disease_id == value])), collapse = " and ")
    }, character(1))
    label <- paste0(label, " [", classifications, "]")
  }
  choices <- stats::setNames(id, label)
  choices[order(tolower(label))]
}

search_bluedot_locations <- function(query, level = c("all", "country", "subnational")) {
  level <- match.arg(level)
  queries <- unique(trimws(unlist(strsplit(query, ",", fixed = TRUE))))
  queries <- queries[!is.na(queries) & nzchar(queries)]
  if (!length(queries)) stop("Enter at least one location name or ID.", call. = FALSE)
  data <- bluedot_locations
  keep <- rep(FALSE, nrow(data))
  for (value in queries) {
    keep <- keep | grepl(tolower(value), tolower(data$location_name), fixed = TRUE) |
      grepl(tolower(value), tolower(data$country_name), fixed = TRUE) | data$location_id == value
  }
  country <- data$location_id == data$country_id
  if (level == "country") keep <- keep & country
  if (level == "subnational") keep <- keep & !country
  data[keep, , drop = FALSE]
}

bluedot_ids <- function(names, type = c("country", "location", "disease", "global_country")) {
  type <- match.arg(type)
  data <- switch(type, country = bluedot_countries, location = bluedot_locations,
                 disease = bluedot_diseases, global_country = bluedot_global_countries)
  indices <- match(tolower(trimws(names)), tolower(data[[2]]))
  if (anyNA(indices)) stop("Unknown ", type, " name(s): ", paste(names[is.na(indices)], collapse = ", "), call. = FALSE)
  unname(data[[1]][indices])
}

# Filter returned disease names using catalog IDs; no undocumented API parameter.
matching_bluedot_diseases <- function(data, disease_ids = character(),
                                     classification = c("all", "human", "animal")) {
  classification <- match.arg(classification)
  if (!length(disease_ids) && classification == "all") return(data)
  if (any(!as.character(disease_ids) %in% bluedot_diseases$disease_id)) {
    stop("Choose disease IDs from the workbook catalog.", call. = FALSE)
  }
  catalog <- bluedot_disease_catalog(classification)
  if (length(disease_ids)) catalog <- catalog[catalog$disease_id %in% disease_ids, , drop = FALSE]
  selected <- tolower(catalog$disease_name)
  keep <- vapply(data$diseases, function(value) {
    if (is.na(value) || !nzchar(value)) return(FALSE)
    any(tolower(trimws(strsplit(value, "; ", fixed = TRUE)[[1]])) %in% selected)
  }, logical(1))
  data[keep, , drop = FALSE]
}

# Catalog classification describes the disease, not the affected host of an article.
classify_bluedot_diseases <- function(data) {
  data$disease_classification <- vapply(data$diseases, function(value) {
    if (is.na(value) || !nzchar(value)) return("Unclassified")
    disease_names <- tolower(trimws(strsplit(value, "; ", fixed = TRUE)[[1]]))
    labels <- bluedot_diseases$disease_classification[
      tolower(bluedot_diseases$disease_name) %in% disease_names]
    if (!length(labels)) return("Unclassified")
    paste(intersect(c("Human", "Animal"), labels), collapse = "; ")
  }, character(1))
  data
}
