# libraries --------------------------------------------------------------------
library(httr2)
library(jsonlite)
library(tidyverse)

# data -------------------------------------------------------------------------
# Fetch all station data from PickAPump API (county by county for Ireland)
counties_ie <- c(
  "Carlow", "Cavan", "Clare", "Cork", "Donegal", "Dublin",
  "Galway", "Kerry", "Kildare", "Kilkenny", "Laois", "Leitrim",
  "Limerick", "Longford", "Louth", "Mayo", "Meath", "Monaghan",
  "Offaly", "Roscommon", "Sligo", "Tipperary", "Waterford",
  "Westmeath", "Wexford", "Wicklow"
)

fetch_county <- function(county, limit = 100) {
  all_results <- list()
  offset <- 0
  
  repeat {
    resp <- request(glue::glue("https://api.pickapump.com/v1/stations/county/{county}")) |>
      req_url_query(offset = offset, limit = limit) |>
      req_headers(
        `User-Agent` = "pumpsie_bot/1.0 (fuel price tracker; https://github.com/damien-dupre/pumpsie_bot)",
        `Accept` = "application/json",
        `Content-Type` = "application/json",
        `Origin` = "https://pickapump.com",
        `Referer` = "https://pickapump.com/"
      ) |>
      req_retry(max_tries = 3, backoff = ~ 2) |>
      req_perform()
    
    body <- resp |> resp_body_json()
    results <- body$results
    
    if (length(results) == 0) break
    
    all_results <- c(all_results, results)
    page_size <- body$page_size %||% limit
    
    if (length(results) < page_size) break
    
    offset <- offset + page_size
    Sys.sleep(0.5) # be polite
  }
  
  cat(glue::glue("  {county}: {length(all_results)} stations\n"))
  all_results
}

all_stations <- map(counties_ie, \(county) {
  tryCatch(
    fetch_county(county),
    error = function(e) {
      warning(glue::glue("Failed to fetch {county}: {e$message}"))
      list()
    }
  )
}) |> list_flatten()

# Parse into a flat tibble (long format: one row per station per fuel type)
parse_station <- function(station) {
  base <- tibble(
    name      = station$stationName %||% NA_character_,
    brand     = station$brand %||% NA_character_,
    address   = station$address %||% NA_character_,
    town      = station$town %||% NA_character_,
    county    = station$county %||% NA_character_,
    latitude  = station$coords$lat %||% NA_real_,
    longitude = station$coords$lng %||% NA_real_
  )
  
  rows <- list()
  
  # Extract the price date (try prices$date_updated, then prices$date_added, then station$date_updated)
  price_date <- tryCatch(
    as.Date(station$prices$date_updated %||% station$prices$date_added %||% station$date_updated),
    error = function(e) NA_Date_
  )
  if (length(price_date) == 0) price_date <- NA_Date_
  
  if (!is.null(station$prices$petrol)) {
    rows[[length(rows) + 1]] <- base |> mutate(
      fuel        = "Petrol",
      price       = as.numeric(station$prices$petrol),
      dateupdated = price_date
    )
  }
  
  if (!is.null(station$prices$diesel)) {
    rows[[length(rows) + 1]] <- base |> mutate(
      fuel        = "Diesel",
      price       = as.numeric(station$prices$diesel),
      dateupdated = price_date
    )
  }
  
  if (length(rows) == 0) return(NULL)
  bind_rows(rows)
}

df <- map(all_stations, parse_station) |>
  bind_rows() |>
  # Standardise county names
  mutate(
    county = county |>
      str_remove("(?i)^county\\s+|^co\\.?\\s*") |>
      str_to_title() |>
      str_trim()
  )

# Save today's snapshot
dir.create("data", showWarnings = FALSE)
write_csv(df, glue::glue("data/pickapump_{Sys.Date()}.csv"))
