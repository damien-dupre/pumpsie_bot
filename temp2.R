df <- pumps_county_df |> 
  filter(day == format(Sys.time(), "%A")) |> 
  expand_grid(fuel = c("Diesel", "Petrol")) |> 
  mutate(url = glue("https://pumps.ie/api/getStationsByPriceAPI.php?county={county}&minLat={minLat}&maxLat={maxLat}&minLng={minLng}&maxLng={maxLng}&fuel={fuel}")) |> 
  group_by(province, county, fuel) |> 
  summarise(api_call(url)) |> 
  filter(dateupdated > (Sys.Date() - 14) & dateupdated < Sys.Date()) |>
  filter(price < 500 & price > 130) |> 
  distinct(ID, dateupdated, fuel, .keep_all = TRUE)