library(tabulizer)
library(tidyverse)
tab <- 
  extract_tables("https://www.theaa.ie/~/media/Historical-fuel-prices/FuelpricesHistory_Mar22")

add_row_5 <- function(df) {
  df |> 
    add_row(
      V1 = "Date", 
      V2 = "Premium Leaded",
      V3 = "Super Unleaded",
      V4 = "Reg Unleaded",
      V5 = "Diesel",
      .before = 1
    )
}

add_row_3 <- function(df) {
  df |> 
    add_row(
      V1 = "Date", 
      V2 = "Reg Unleaded",
      V3 = "Diesel",
      .before = 1
    )
}

custom_transform <- function(df) {
  df <- as.data.frame(df)
  
  if (ncol(df) == 5) {
    df <- add_row_5(df)
  }
  if (ncol(df) == 3) {
    df <- add_row_3(df)
  }
  df
}

test_purrr <- map(tab, custom_transform)

raw <- enframe(unlist(test_purrr))

test <- raw |> 
  rowid_to_column("id") |> 
  select(-name) |> 
  filter(value != "" & id <1353) |> 
  mutate(
    value_num = parse_number(value),
    fuel_type = case_when(
      value == "Premium" ~ "Premium Leaded",
      value == "Leaded" ~ "Premium Leaded",
      value == "Premium Leaded" ~ "Premium Leaded",
      value == "Super Unleaded" ~ "Super Unleaded",
      value == "Reg Unleaded" ~ "",
      value == "Reg" ~ "Reg Unleaded",
      value == "Unleaded" ~ "Reg Unleaded",
      value == "Diesel" ~ "Diesel",
      TRUE ~ NA_character_
    )
  ) |> 
  fill(fuel_type, .direction = "down")
  
test2 <- test |> 
  filter(between(value_num, 50, 200)) |> 
  filter(fuel_type %in% c("Reg Unleaded", "Diesel"))

test3 <- test2 |> 
  select(-value,) |> 
  pivot_wider(names_from = fuel_type, values_from = value_num)

Diesel_test <- test3 |> 
  select(Diesel) |> 
  drop_na() |> 
  mutate(date = seq(from = as.Date("2022-03-01"), length = 184, by = "-1 month"))
