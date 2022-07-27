# libraries --------------------------------------------------------------------
library(geomtextpath)
library(ggrepel)
library(ggtext)
library(glue)
library(here)
library(rtweet)
library(scales)
library(showtext)
library(tidyverse)

# options in -------------------------------------------------------------------
options(encoding = "windows-1252")
font_add_google("Roboto Condensed", "Roboto Condensed")
showtext_auto()

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
  mutate(County = str_remove(County, "County |Co |Co. |Co.")) |> 
  mutate(county_check = case_when(
    format(Sys.time(), "%A") == "Tuesday" & County %in% connaught ~ TRUE,
    format(Sys.time(), "%A") == "Wednesday" & County %in% leinster ~ TRUE,
    format(Sys.time(), "%A") == "Thursday" & County %in% munster ~ TRUE,
    format(Sys.time(), "%A") == "Friday" & County %in% ulster_ie ~ TRUE,
    TRUE ~ NA
  )
  ) |> 
  filter(county_check == TRUE) |> 
  filter(dateupdated > (Sys.Date() - 14) & dateupdated < Sys.Date()) |>
  group_by(fuel) |> 
  mutate(
    c1 = quantile(price, prob = 0.01),
    c99 = quantile(price, prob = 0.99)
  ) |> 
  rowwise() |> 
  filter(between(price, c1, c99)) |> 
  ungroup() |> 
  distinct(ID, dateupdated, fuel, .keep_all = TRUE)

province_title <- 
  case_when(
    format(Sys.time(), "%A") == "Tuesday" ~ "Connaught",
    format(Sys.time(), "%A") == "Wednesday" ~ "Leinster",
    format(Sys.time(), "%A") == "Thursday" ~ "Munster",
    format(Sys.time(), "%A") == "Friday" ~ "Ulster (IE)"
  )

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
  geom_textline(data = oil_df, aes(Date, ((Close + 20) / 70), linetype = type, label = type), color = "gray50", family = "serif", hjust = 0.08, size = 3) +
  geom_point(aes(dateupdated, price / 100, color = fuel), alpha = 0.3, size = 1) +
  geom_labelsmooth(aes(dateupdated, price / 100, color = fuel, label = fuel), method = "loess", formula = "y ~ x", text_smoothing = 30, fill = "#F6F6FF", size = 3, linewidth = 1, boxlinewidth = 0.3, hjust = 0.8) +
  geom_text_repel(data = oil_df |> group_by(type) |> slice_max(Date), aes(Date, ((Close + 20) / 70), label = dollar(Close, accuracy = 1)), hjust = -1, direction = "y", size = 2, force = 0.5, family = "serif", color = "gray30") +
  geom_text_repel(data = oil_df |> group_by(type) |> slice_min(Date), aes(Date, ((Close + 20) / 70), label = dollar(Close, accuracy = 1)), hjust = 1, direction = "y", size = 2, force = 0.5, family = "serif", color = "gray30") +
  geom_text_repel(data = lowest, aes(dateupdated, price / 100, label = paste(addr2, dollar(price / 100, prefix = "€")), color = fuel), hjust = -1, direction = "y", fontface = "bold", size = 2) +
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
    text = element_text(size = 8)
  )

ggsave("pumps.png", width = 8, height = 4)

# tweet ------------------------------------------------------------------------
post_tweet(glue("Fluctuation of fuel prices reported on pumps.ie in #{province_title} for the last 2 weeks. #Ireland #Petrol #Diesel #FuelPrice"), media = "pumps.png", token = bot_token, media_alt_text = "")

# options out ------------------------------------------------------------------
unlink("pumps.png")
showtext_auto(FALSE)
