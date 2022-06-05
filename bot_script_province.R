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
df <- read_csv(here(glue("data/pumpsie_{Sys.Date()}.csv"))) |> 
  mutate(County = str_remove(County, "County |Co |Co\\. |Co\\.")) |> 
  mutate(province = case_when(
    County %in% connaught ~ "connaught",
    County %in% leinster ~ "leinster",
    County %in% munster ~ "munster",
    County %in% ulster_ie ~ "ulster_ie",
    TRUE ~ NA_character_
  )) |> 
  distinct(ID, dateupdated, fuel, .keep_all = TRUE) |> 
  mutate(month_year = format(dateupdated, "%m-%Y")) |> 
  group_by(fuel, month_year) |> 
  mutate(
    c1 = quantile(price, prob = 0.01),
    c99 = quantile(price, prob = 0.99)
  ) |> 
  rowwise() |> 
  filter(
    between(price, c1, c99)
    ) |> 
  ungroup()

df |> 
  filter(!is.na(province), dateupdated > (Sys.Date() - 30)) |> 
  ggplot(aes(dateupdated, price/100, color = province)) +
  geom_jitter(alpha = 0.3, size = 1) +
  geom_smooth(se = FALSE) +
  facet_wrap(~ fuel) +
  scale_y_continuous(labels = dollar_format(prefix = "€")) + 
  scale_color_manual(
    values = c("connaught" = "chartreuse4", "leinster" = "blue4", "munster" = "yellow4", "ulster_ie" = "red4"),
    labels = c("Connaught", "Leinster", "Munster", "Ulster (IE)")
  ) +
  labs(
    title = "Reported prices of <span style='color:#D0BE24;'>Diesel</span> and <span style='color:#549B8C;'>Petrol</span> by Irish province in the last 30 days",
    subtitle = "Note: a lack of data points can lead to unrealistic changes in the non-linear approximation",
    caption = "Source: pumps.ie/@damien_dupre",
    x = "",
    y = "Fuel Price",
    color = "Irish Province"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    plot.title = element_markdown(lineheight = 1),
    text = element_text(size = 8)
  )

ggsave("pumps.png", width = 8, height = 4)

# tweet ------------------------------------------------------------------------
post_tweet("Fluctuation of fuel prices reported on pumps.ie in #Ireland for the last 30 days. #Petrol #Diesel #FuelPrice", media = "pumps.png", token = bot_token)

# options out ------------------------------------------------------------------
unlink("pumps.png")
showtext_auto(FALSE)
