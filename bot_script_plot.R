# libraries --------------------------------------------------------------------
library(bskyr)
library(geomtextpath)
library(ggrepel)
library(ggtext)
library(glue)
library(here)
library(scales)
library(showtext)
library(tidyverse)
library(tidyquant)

# options in -------------------------------------------------------------------
options(encoding = "windows-1252")
font_add_google("Roboto Condensed", "Roboto Condensed")
showtext_auto()

connaught <- c("Galway", "Mayo", "Sligo", "Leitrim", "Roscommon")
leinster <- c("Carlow", "Dublin", "Kildare", "Kilkenny", "Offaly", "Longford", "Louth", "Meath", "Laois", "Westmeath", "Wexford", "Wicklow")
munster <- c("Clare", "Cork", "Kerry", "Limerick", "Tipperary", "Waterford")
ulster_ie <- c("Cavan", "Donegal", "Monaghan")

# data -------------------------------------------------------------------------
df <- 
  read_csv(here(glue("data/pickapump_{Sys.Date()}.csv"))) |> 
  mutate(county = str_remove(county, "county |Co |Co. |Co.")) |> 
  mutate(county_check = case_when(
    format(Sys.time(), "%A") == "Tuesday" & county %in% connaught ~ TRUE,
    format(Sys.time(), "%A") == "Wednesday" & county %in% leinster ~ TRUE,
    format(Sys.time(), "%A") == "Thursday" & county %in% munster ~ TRUE,
    format(Sys.time(), "%A") == "Friday" & county %in% ulster_ie ~ TRUE,
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
  distinct(name, dateupdated, fuel, .keep_all = TRUE)

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

wti_last_month <- tq_get("CL=F", from = Sys.Date() - 14) |> 
  select(Date = date, Close = close) |> 
  mutate(type = "WTI")

brent_last_month <- tq_get("BZ=F", from = Sys.Date() - 14) |> 
  select(Date = date, Close = close) |> 
  mutate(type = "Brent")

oil_df <- bind_rows(wti_last_month, brent_last_month)

# visualisation ----------------------------------------------------------------
df |> 
  ggplot() +
  geom_textline(data = oil_df, aes(Date, ((Close + 20) / 70), linetype = type, label = type), color = "gray50", family = "serif", hjust = 0.08, size = 3) +
  geom_point(aes(dateupdated, price / 100, color = fuel), alpha = 0.3, size = 1) +
  geom_labelsmooth(aes(dateupdated, price / 100, color = fuel, label = fuel), method = "loess", formula = "y ~ x", text_smoothing = 30, fill = "#F6F6FF", size = 3, linewidth = 1, boxlinewidth = 0.3, hjust = 0.8) +
  geom_text_repel(data = oil_df |> group_by(type) |> slice_max(Date), aes(Date, ((Close + 20) / 70), label = dollar(Close, accuracy = 1)), hjust = -1, direction = "y", size = 2, force = 0.5, family = "serif", color = "gray30") +
  geom_text_repel(data = oil_df |> group_by(type) |> slice_min(Date), aes(Date, ((Close + 20) / 70), label = dollar(Close, accuracy = 1)), hjust = 1, direction = "y", size = 2, force = 0.5, family = "serif", color = "gray30") +
  geom_text_repel(data = lowest, aes(dateupdated, price / 100, label = paste(name, town, dollar(price / 100, prefix = "\u20ac")), color = fuel), hjust = -1, direction = "y", fontface = "bold", size = 2) +
  scale_y_continuous(labels = dollar_format(prefix = "\u20ac")) + 
  scale_linetype_manual(values = c("twodash", "dotted")) +
  scale_color_manual(values = c("#D0BE24", "#549B8C")) +
  expand_limits(x = Sys.Date()) +
  labs(
    title = glue("Reported prices of <span style='color:#D0BE24;'>Diesel</span> and <span style='color:#549B8C;'>Petrol</span> in {province_title} in the last 14 days"),
    subtitle = "Station names correspond to the lowest prices recently reported, Brent and WTI stock prices for information",
    caption = "Source: pickapump.com",
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

# post to bluesky --------------------------------------------------------------
bs_post(
  text = glue("Fluctuation of fuel prices reported on pickapump.com in {province_title} for the last 2 weeks. #Ireland #Petrol #Diesel #FuelPrice"),
  images = "pumps.png",
  images_alt = glue("Chart showing diesel and petrol price fluctuations in {province_title} over the last 14 days"),
  user = Sys.getenv("BLUESKY_APP_USER"),
  pass = Sys.getenv("BLUESKY_APP_PASS")
)
# options out ------------------------------------------------------------------
unlink("pumps.png")
showtext_auto(FALSE)
