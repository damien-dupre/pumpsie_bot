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

# options in -------------------------------------------------------------------
font_add_google("Roboto Condensed", "Roboto Condensed")
showtext_auto()

connaught <- c("Galway", "Mayo", "Sligo", "Leitrim", "Roscommon")
leinster <- c("Carlow", "Dublin", "Kildare", "Kilkenny", "Offaly", "Longford", "Louth", "Meath", "Laois", "Westmeath", "Wexford", "Wicklow")
munster <- c("Clare", "Cork", "Kerry", "Limerick", "Tipperary", "Waterford")
ulster_ie <- c("Cavan", "Donegal", "Monaghan")

# data -------------------------------------------------------------------------
df <- read_csv(here(glue("data/pickapump_{Sys.Date()}.csv"))) |> 
  mutate(county = str_remove(county, "county |Co |Co\\. |Co\\.")) |> 
  mutate(province = case_when(
    county %in% connaught ~ "connaught",
    county %in% leinster ~ "leinster",
    county %in% munster ~ "munster",
    county %in% ulster_ie ~ "ulster_ie",
    TRUE ~ NA_character_
  )) |> 
  distinct(name, dateupdated, fuel, .keep_all = TRUE) |> 
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
  scale_y_continuous(labels = dollar_format(prefix = "\u20ac")) + 
  scale_color_manual(
    values = c("connaught" = "chartreuse4", "leinster" = "blue4", "munster" = "yellow4", "ulster_ie" = "red4"),
    labels = c("Connaught", "Leinster", "Munster", "Ulster (IE)")
  ) +
  labs(
    title = "Reported prices of <span style='color:#D0BE24;'>Diesel</span> and <span style='color:#549B8C;'>Petrol</span> by Irish province in the last 30 days",
    subtitle = "Note: a lack of data points can lead to unrealistic changes in the non-linear approximation",
    caption = "Source: pickapump.com",
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

# post to bluesky --------------------------------------------------------------
bs_post(
  text = "Fluctuation of fuel prices reported on pickapump.com in Ireland for the last 30 days. #Ireland #Petrol #Diesel #FuelPrice",
  images = "pumps.png",
  images_alt = "Chart showing diesel and petrol price fluctuations by Irish province over the last 30 days",
  user = Sys.getenv("BLUESKY_APP_USER"),
  pass = Sys.getenv("BLUESKY_APP_PASS")
)

# options out ------------------------------------------------------------------
unlink("pumps.png")
showtext_auto(FALSE)
