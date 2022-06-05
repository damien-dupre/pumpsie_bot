# libraries --------------------------------------------------------------------
library(glue)
library(here)
library(rtweet)
library(scales)
library(tidyverse)

# options in -------------------------------------------------------------------
options(encoding = "windows-1252")

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

connaught <- c("Galway", "Mayo", "Sligo", "Leitrim", "Roscommon")
leinster <- c("Carlow", "Dublin", "Kildare", "Kilkenny", "Offaly", "Longford", "Louth", "Meath", "Laois", "Westmeath", "Wexford", "Wicklow")
munster <- c("Clare", "Cork", "Kerry", "Limerick", "Tipperary", "Waterford")
ulster_ie <- c("Cavan", "Donegal", "Monaghan")

# data -------------------------------------------------------------------------
df <- 
  read_csv(here(glue("data/pumpsie_{Sys.Date()}.csv"))) |> 
  filter(dateupdated > (Sys.Date() - 3) & dateupdated < Sys.Date()) |> 
  distinct(ID, dateupdated, fuel, .keep_all = TRUE) |> 
  mutate(
    County = County |> 
      tolower() |> 
      str_remove("county |co |co\\. |co\\.") |> 
      str_extract("\\w+") |> 
      str_to_title()
  ) |> 
  filter(County %in% c(connaught, leinster, munster, ulster_ie)) |> 
  group_by(fuel) |> 
  mutate(
    c1 = quantile(price, prob = 0.01),
    c99 = quantile(price, prob = 0.99)
  ) |> 
  rowwise() |> 
  filter(between(price, c1, c99)) |> 
  ungroup()

test <- df |> 
  mutate(dateupdated = if_else(dateupdated == Sys.Date() - 1, "yesterday", "daybefore")) |> 
  group_by(County, fuel, dateupdated) |> 
  summarise(price_avg = mean(price, na.rm = TRUE)) |> 
  pivot_wider(names_from = dateupdated, values_from = price_avg) |> 
  drop_na(yesterday) |> 
  mutate(perc_change = ((yesterday - daybefore)/daybefore)) |> 
  left_join(df |> filter(dateupdated == Sys.Date() - 1) |> count(County, fuel)) |> 
  left_join(df |> filter(dateupdated == Sys.Date() - 1) |> group_by(County, fuel) |> slice_min(price) |> select(County, fuel, price_min = price, name, Zone)) 
library(gt)
