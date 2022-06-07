# libraries --------------------------------------------------------------------
library(fontawesome)
library(glue)
library(gt)
library(here)
library(rtweet)
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
  ungroup() |> 
  mutate(dateupdated = if_else(dateupdated == Sys.Date() - 1, "yesterday", "daybefore")) |> 
  group_by(County, fuel, dateupdated) |> 
  summarise(price_avg = mean(price, na.rm = TRUE)) |> 
  pivot_wider(names_from = dateupdated, values_from = price_avg) |> 
  drop_na(yesterday) |> 
  mutate(perc_change = ((yesterday - daybefore)/daybefore)) |> 
  left_join(df |> filter(dateupdated == Sys.Date() - 1) |> count(County, fuel)) |> 
  left_join(df |> filter(dateupdated == Sys.Date() - 1) |> group_by(County, fuel) |> slice_min(price, with_ties = FALSE) |> select(County, fuel, price_min = price, name, Zone)) |> 
  ungroup()

# visualisation ----------------------------------------------------------------
df |>
  arrange(fuel) |>
  mutate(
    `Avg Price` = yesterday/100,
    `Min Price` = price_min/100,
  ) |>
  select(Fuel = fuel, County, N = n, `Avg Price`, Arrow = perc_change, Change = perc_change, Location = name, Zone, `Min Price`) |>
  gt(groupname_col = "Fuel") |>
  sub_missing() |>
  opt_row_striping() |> 
  cols_label(Arrow = "", Change = "") |>
  tab_header(
    title = md("**Fuel prices reported on *pumps.ie* yesterday by Irish county**"),
    subtitle = glue("updated: {Sys.Date()}")
  ) |>
  tab_spanner(
    label = "Lowest Price Reported",
    columns = c(`Min Price`, Location, Zone)
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = everything())
  ) |>
  tab_style(
    style = cell_text(size = px(12)),
    locations = cells_body(columns = `Min Price`)
  ) |> 
  fmt_currency(columns = c(`Avg Price`, `Min Price`), currency = "EUR") |>
  fmt_percent(columns = Change, decimals = 1) |> 
  cols_merge(
    columns = c(Location, Zone),
    pattern = "<div><span style='font-weight:bold;font-variant:small-caps;font-size:12px'>{1}</div>
        <div><span style ='font-weight:bold;color:grey;font-size:10px'>{2}</span></div>"
  ) |> 
  text_transform(
    locations = cells_body(columns = County),
    fn = function(x){glue("<div><span style='font-weight:bold;font-variant:small-caps;font-size:14px'>{x}</div>")}
  ) |>
  text_transform(
    locations = cells_body(columns = Arrow),
    fn = function(x){
      case_when(
      x < 0 ~ as.character(fa("arrow-down", fill = "red")),
      x > 0 ~ as.character(fa("arrow-up", fill = "green")),
      x == 0 ~ as.character(fa("equals", fill = "grey"))
    )}
  ) |>
  text_transform(
    locations = cells_body(columns = Arrow, rows = is.na(Arrow)),
    fn = function(x){"—"}
  ) |> 
  tab_footnote(
    footnote = md("Missing counties among the 26 (IE) have no fuel price reported on *pumps.ie* yesterday.")
  ) |> 
  tab_footnote(
    footnote = "No data available the day before to compare with.",
    locations = cells_body(columns = Arrow, rows = is.na(Arrow))
  ) |> 
  tab_source_note(
    source_note = "Source: pumps.ie/@damien_dupre"
  ) |> 
  gtsave("pumps.png")

# tweet ------------------------------------------------------------------------
post_tweet(glue("Average fuel prices reported on pumps.ie yesterday. #Ireland #Petrol #Diesel #FuelPrice"), media = "pumps.png", token = bot_token)

# options out ------------------------------------------------------------------
unlink("pumps.png")