# libraries --------------------------------------------------------------------
library(geomtextpath)
library(ggrepel)
library(ggtext)
library(glue)
library(httr2)
library(rtweet)
library(scales)
library(showtext)
library(tidyverse)
library(xml2)

# options in -------------------------------------------------------------------
font_add_google("Roboto Condensed", "Roboto Condensed")
showtext_auto()

# functions --------------------------------------------------------------------
api_call <- function(url) {
  requ <- request(url) |> 
    req_perform()
  
  resp <- resp_body_xml(requ, encoding = "windows-1252") |> 
    xml_find_all("//station") |> 
    xml_attrs() %>%
    map_df(~as.list(.)) |> 
    mutate(
      price = as.numeric(price),
      dateupdated = as.Date(dateupdated)
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

# data -------------------------------------------------------------------------
pumps_county_df <- 
  tribble(~province, ~county, ~minLat, ~maxLat, ~minLng, ~maxLng,
    "Leinster", "Carlow", 52.47818014456486, 52.89026331616186, -7.1246337890625, -6.514892578125,
    "Ulster (IE)", "Cavan", 53.599839905016815, 54.39894871869461, -7.97332763671875, -6.75384521484375,
    "Munster", "Clare", 52.440106507645744, 53.26110582598391, -9.59381103515625, -8.39630126953125,
    "Munster", "Cork", 51.55402061770002, 52.391525337232224, -9.349365234375, -8.1298828125,
    "Ulster (IE)", "Donegal", 54.50722330203749, 55.28898256534061, -8.460302734374995, -7.339697265624996,
    "Leinster", "Dublin", 53.23624942625596, 53.43919600129776, -6.4290618896484375, -6.1241912841796875,
    "Connaught", "Galway", 52.8765898173371, 53.689388209233215, -9.65423583984375, -8.43475341796875,
    "Munster", "Kerry", 51.64444187569695, 52.480271148513935, -10.3216552734375, -9.1021728515625,
    "Leinster", "Kildare", 52.901033338964616, 53.30913427097515, -7.1905517578125, -6.580810546875,
    "Leinster", "Kilkenny", 52.37937168126617, 52.7923822043612, -7.554870605468744, -6.945129394531244,
    "Leinster", "Laois", 52.824757152097966, 53.23357804488607, -7.606658935546875, -6.996917724609375,
    "Connaught", "Leitrim", 53.92577258712727, 54.32413251396739, -8.337249755859375, -7.779693603515625,
    "Munster", "Limerick", 51.968807666562874, 52.79861002821915, -9.537506103515625, -8.348236083984375,
    "Leinster", "Longford", 53.5407154335218, 53.94275054924692, -8.070144653320312, -7.4810028076171875,
    "Leinster", "Louth", 53.66701895753172, 54.06785053038563, -6.844482421875, -6.2347412109375,
    "Connaught", "Mayo", 53.45453140531, 54.256400599379525, -9.89593505859375, -8.67645263671875,
    "Leinster", "Meath", 53.45575799847186, 53.858601588271675, -7.0017242431640625, -6.3919830322265625,
    "Ulster (IE)", "Monaghan", 53.86427078128475, 54.26321890822346, -7.2777557373046875, -6.6803741455078125,
    "Leinster", "Offaly", 52.72298552457069, 53.53867518408384, -8.2012939453125, -6.9818115234375,
    "Connaught", "Roscommon", 53.23809874647396, 54.04407014753032, -8.800048828125, -7.58056640625,
    "Connaught", "Sligo", 54.01140032647095, 54.408940560718236, -9.08294677734375, -8.47320556640625,
    "Munster", "Tipperary", 52.17477387796152, 53.000735829422524, -8.45123291015625, -7.25372314453125,
    "Munster", "Waterford", 51.806916535158145, 52.63973017532397, -8.118896484375, -6.8994140625,
    "Leinster", "Westmeath", 53.12122943574266, 53.929411303402084, -8.05023193359375, -6.83074951171875,
    "Leinster", "Wexford", 52.292522517043615, 52.70634714950863, -6.87469482421875, -6.26495361328125,
    "Leinster", "Wicklow", 52.78324644967133, 53.19245885638094, -6.646728515625, -6.0369873046875
  ) |> 
  mutate(day = case_when(
    province == "Connaught" ~ "Tuesday",
    province == "Leinster" ~ "Wednesday",
    province == "Munster" ~ "Thursday",
    province == "Ulster (IE)" ~ "Friday",
  ))

df <- pumps_county_df |> 
  filter(day == format(Sys.time(), "%A")) |> 
  expand_grid(fuel = c("Diesel", "Petrol")) |> 
  mutate(url = glue("https://pumps.ie/api/getStationsByPriceAPI.php?county={county}&minLat={minLat}&maxLat={maxLat}&minLng={minLng}&maxLng={maxLng}&fuel={fuel}")) |> 
  group_by(province, county, fuel) |> 
  summarise(api_call(url)) |> 
  filter(dateupdated > (Sys.Date() - 14) & dateupdated < Sys.Date()) |>
  filter(price < 500 & price > 130) |> 
  distinct(ID, dateupdated, fuel, .keep_all = TRUE)

province_title <- unique(df$province)[1]

lowest <- df |> 
  group_by(fuel) |> 
  slice_max(dateupdated, with_ties = TRUE) |> 
  slice_min(price, with_ties = FALSE)

wti_last_month <- "https://www.marketwatch.com/investing/future/cl.1/downloaddatapartial?csvdownload=true" |> 
  read_csv(col_types = list(col_date(format = "%m/%d/%Y"))) |> 
  mutate(type = "WTI")

brent_last_month <- "https://www.marketwatch.com/investing/future/brn00/downloaddatapartial?csvdownload=true&countrycode=uk" |> 
  read_csv(col_types = list(col_date(format = "%m/%d/%Y"))) |> 
  mutate(type = "Brent")

oil_df <- bind_rows(wti_last_month, brent_last_month) |> 
  filter(Date > (Sys.Date() - 14))

# visualisation ----------------------------------------------------------------
df |> 
  ggplot() +
  geom_textline(data = oil_df, aes(Date, ((Close + 20) / 70), linetype = type, label = type), color = "gray50", family = "serif", hjust = 0.08, size = 5) +
  geom_point(aes(dateupdated, price / 100, color = fuel), alpha = 0.3, size = 1) +
  geom_labelsmooth(aes(dateupdated, price / 100, color = fuel, label = fuel), method = "loess", formula = "y ~ x", text_smoothing = 30, fill = "#F6F6FF", size = 5, linewidth = 1, boxlinewidth = 0.3, hjust = 0.8) +
  geom_text_repel(data = oil_df |> group_by(type) |> slice_max(Date), aes(Date, ((Close + 20) / 70), label = dollar(Close, accuracy = 1)), hjust = -1, direction = "y", size = 4, force = 0.5, family = "serif", color = "gray30") +
  geom_text_repel(data = oil_df |> group_by(type) |> slice_min(Date), aes(Date, ((Close + 20) / 70), label = dollar(Close, accuracy = 1)), hjust = 1, direction = "y", size = 4, force = 0.5, family = "serif", color = "gray30") +
  geom_text_repel(data = lowest, aes(dateupdated, price / 100, label = paste(addr2, dollar(price / 100, prefix = "€")), color = fuel), hjust = -1, direction = "y", fontface = "bold", size = 6) +
  scale_y_continuous(labels = dollar_format(prefix = "€")) + 
  scale_linetype_manual(values = c("twodash", "dotted")) +
  scale_color_manual(values = c("#D0BE24", "#549B8C")) +
  expand_limits(x = Sys.Date()) +
  labs(
    title = glue("Reported prices of <span style='color:#D0BE24;'>Diesel</span> and <span style='color:#549B8C;'>Petrol</span> in {province_title} in the last 14 days"),
    subtitle = "Station names correspond to the lowest prices recently reported, Brent and WTI stock prices for information",
    caption = "Source: pumps.ie/@damien_dupre",
    x = "",
    y = "Fuel Price"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_markdown(lineheight = 1),
    text = element_text(size = 14)
  )

ggsave("pumps.png", width = 8, height = 4)

# tweet ------------------------------------------------------------------------
post_tweet(glue("Fluctuation of fuel prices reported on pumps.ie in #{province_title} for the last 2 weeks. #Ireland #Petrol #Diesel #FuelPrice"), media = "pumps.png", token = bot_token)

# options out ------------------------------------------------------------------
unlink("pumps.png")
showtext_auto(FALSE)
