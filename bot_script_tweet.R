# libraries --------------------------------------------------------------------
library(glue)
library(here)
library(rtweet)
library(scales)
library(tidyverse)

# options in -------------------------------------------------------------------
options(encoding = "windows-1252")

# functions --------------------------------------------------------------------
api_call <- function(url) {
  df_2_days <- resp |> 
    filter(as.Date(dateupdated) == Sys.Date() - 2) |> 
    filter(abs(price) < mean(price, na.rm = TRUE) + sd(price, na.rm = TRUE))
  
  df <- resp |> 
    filter(as.Date(dateupdated) == Sys.Date() - 1) |> 
    group_by(fuel) |> 
    filter(abs(price) < mean(price, na.rm = TRUE) + sd(price, na.rm = TRUE)) |> 
    summarise(
      n_report = n(),
      m_price = mean(price, na.rm = TRUE) |> round(1)
    ) |> 
    mutate(
      perc_change = ((m_price - mean(df_2_days$price, na.rm = TRUE))/mean(df_2_days$price, na.rm = TRUE)) |> percent(0.1),
      text = glue("{n_report} {fuel} prices (avg:{m_price}, change: {perc_change})")
    )
}

# twitter ----------------------------------------------------------------------
# bot_token <- 
# rtweet::create_token(
#   app = "pumpsie_bot",
#   consumer_key = config::get("pumpsie_bot")$api_key,
#   consumer_secret = config::get("pumpsie_bot")$api_key_secret,
#   access_token = config::get("pumpsie_bot")$access_token,
#   access_secret = config::get("pumpsie_bot")$access_token_secret
# )

bot_token <- 
  create_token(
    app = "pumpsie_bot",
    consumer_key = Sys.getenv("TWITTER_CONSUMER_API_KEY"),
    consumer_secret = Sys.getenv("TWITTER_CONSUMER_API_SECRET"),
    access_token = Sys.getenv("TWITTER_ACCESS_TOKEN"),
    access_secret = Sys.getenv("TWITTER_ACCESS_TOKEN_SECRET"),
    set_renv = FALSE
  )

data_latest <- read_csv(here(glue("data/pumpsie_{Sys.Date()}.csv"))) |> 
  filter(county %in% c("Dublin", "Cork"))

df_2_days <- data_latest |> 
  filter(as.Date(dateupdated) == Sys.Date() - 2) |> 
  filter(abs(price) < mean(price, na.rm = TRUE) + sd(price, na.rm = TRUE))

df <- data_latest |> 
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
  glue_data("In Co. #{county}, {Petrol} and {Diesel} have been reported yesterday") |> 
  paste(collapse = "\n") |> 
  paste(" #Ireland", sep = "\n") |> 
  post_tweet(token = bot_token, media_alt_text = "")
