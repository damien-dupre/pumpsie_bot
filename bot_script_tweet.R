# libraries --------------------------------------------------------------------
library(glue)
library(httr2)
library(rtweet)
library(scales)
library(tidyverse)
library(xml2)

# functions --------------------------------------------------------------------
api_call <- function(url) {
  requ <- request(url) |> 
    req_perform()
  
  resp <- resp_body_xml(requ, encoding = "windows-1252") |> 
    xml_find_all("//station") |> 
    xml_attrs() %>%
    map_df(~as.list(.)) |> 
    mutate(price = as.numeric(price))
  
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

tribble(
  ~county, ~fuel, ~url,
  "Dublin", "petrol", "https://pumps.ie/api/getStationsByPriceAPI.php?county=Dublin&minLat=53.23624942625596&maxLat=53.43919600129776&minLng=-6.4290618896484375&maxLng=-6.1241912841796875&fuel=petrol&noCache=2.0784268063901865",
  "Dublin", "diesel", "https://pumps.ie/api/getStationsByPriceAPI.php?county=Dublin&minLat=53.23624942625596&maxLat=53.43919600129776&minLng=-6.4290618896484375&maxLng=-6.1241912841796875&fuel=diesel&noCache=2.0784268063901865",
  "Cork", "petrol", "https://pumps.ie/api/getStationsByPriceAPI.php?county=Cork&minLat=51.55402061770002&maxLat=52.391525337232224&minLng=-9.349365234375&maxLng=-8.1298828125&fuel=petrol&noCache=1.68899746901723",
  "Cork", "diesel", "https://pumps.ie/api/getStationsByPriceAPI.php?county=Cork&minLat=51.55402061770002&maxLat=52.391525337232224&minLng=-9.349365234375&maxLng=-8.1298828125&fuel=diesel&noCache=1.68899746901723"
) |> 
  group_by(county, fuel) |> 
  summarise(api_call(url)) |> 
  select(county, fuel, text) |> 
  group_by(county) |>
  pivot_wider(names_from = fuel, values_from = text) |> 
  glue_data("In Co. #{county}, {Petrol} and {Diesel} have been reported yesterday") |> 
  paste(collapse = "\n") |> 
  paste(" #Ireland", sep = "\n") |> 
  post_tweet(token = bot_token)

