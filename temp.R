# libraries --------------------------------------------------------------------
library(glue)
library(httr2)
library(scales)
library(rtweet)
library(tidyverse)
library(xml2)

api_call <- function(url) {
  requ <- request(url) |> 
    req_perform()
  
  resp <- resp_body_xml(requ) |> 
    xml_find_all("//station") |> 
    xml_attrs() %>%
    map_df(~as.list(.)) |> 
    mutate(
      price = as.numeric(price),
      dateupdated = as.Date(dateupdated)
    )
}

df <- 
  tribble(
    ~county, ~fuel, ~url,
    "Dublin", "petrol", "https://pumps.ie/api/getStationsByPriceAPI.php?county=Dublin&minLat=53.23624942625596&maxLat=53.43919600129776&minLng=-6.4290618896484375&maxLng=-6.1241912841796875&fuel=petrol&noCache=2.0784268063901865",
    "Dublin", "diesel", "https://pumps.ie/api/getStationsByPriceAPI.php?county=Dublin&minLat=53.23624942625596&maxLat=53.43919600129776&minLng=-6.4290618896484375&maxLng=-6.1241912841796875&fuel=diesel&noCache=2.0784268063901865"
  ) |> 
  group_by(county, fuel) |> 
  summarise(api_call(url))

lowest <- df |> 
  filter(price < 500 & dateupdated > (Sys.Date()-10)) |> 
  slice_min(price)

library(ggrepel)

df |> 
  filter(price < 500 & dateupdated > (Sys.Date()-10)) |> 
  ggplot(aes(dateupdated, price, color = fuel)) +
  geom_jitter(alpha = 0.5) +
  geom_smooth(method = "loess", formula = "y ~ x")

df |> 
  filter(price < 500 & dateupdated > (Sys.Date()-10)) |> 
  ggplot(aes(dateupdated, price, color = fuel)) +
  geom_jitter(alpha = 0.5) +
  geom_smooth(method = "loess", formula = "y ~ x") +
  geom_label_repel(data = lowest, aes(label = name)) +
  labs(
    title = "Reported price of Petrol/Diesel in Co. Dublin in the last 10 days",
    caption = "Source: pumps.ie/@damien-dupre",
    x = "",
    y = "Price (0.01€)"
  ) +
  theme_bw()

