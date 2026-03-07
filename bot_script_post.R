# libraries --------------------------------------------------------------------
library(bskyr)
library(glue)
library(here)
library(scales)
library(tidyverse)

# data -------------------------------------------------------------------------
data_latest <- read_csv(here(glue("data/pickapump_{Sys.Date()}.csv"))) |> 
  filter(county %in% c("Dublin", "Cork"))

df_2_days <- data_latest |> 
  filter(as.Date(dateupdated) == Sys.Date() - 2) |> 
  filter(abs(price) < mean(price, na.rm = TRUE) + sd(price, na.rm = TRUE))

post_text <- data_latest |> 
  filter(as.Date(dateupdated) == Sys.Date() - 1) |> 
  group_by(county, fuel) |> 
  filter(abs(price) < mean(price, na.rm = TRUE) + sd(price, na.rm = TRUE)) |> 
  summarise(
    n_report = n(),
    m_price = mean(price, na.rm = TRUE) |> round(1)
  ) |> 
  mutate(
    perc_change = ((m_price - mean(df_2_days$price, na.rm = TRUE))/mean(df_2_days$price, na.rm = TRUE)) |> percent(0.1),
    text = glue("{n_report} {fuel} prices (avg:{m_price}, change: {perc_change})")
  ) |> 
  select(county, fuel, text) |> 
  group_by(county) |>
  pivot_wider(names_from = fuel, values_from = text) |> 
  glue_data("In Co. {county}, {Petrol} and {Diesel} have been reported yesterday") |> 
  paste(collapse = "\n") |> 
  paste("Ireland", sep = "\n")

# post to bluesky --------------------------------------------------------------
bs_post(
  text = post_text,
  user = Sys.getenv("BLUESKY_APP_USER"),
  pass = Sys.getenv("BLUESKY_APP_PASS")
)
