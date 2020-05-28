library(data.table)
library(sf)
library(tidyverse)

OD_adm <- fread(here::here("zzz_temporary", "data", "P1467.csv"))

origins <- sf::read_sf(here::here("zzz_temporary", "data", "rej_ursynow.shp"))

test <- OD_adm[str_sub(Name, 1, 6) %in% origins$REJ &
                 str_sub(Name, nchar(Name)-5, nchar(Name)) %in% origins$REJ]

test2 <- fread(here::here("zzz_temporary", "data", "P1468.csv")) %>%
  .[str_sub(Name, 1, 6) %in% origins$REJ &
      str_sub(Name, nchar(Name)-5, nchar(Name)) %in% origins$REJ]

test3 <- test %>% rbind(test2)
# 1465138

fwrite(test3, here::here("zzz_temporary", "data", "od_example.csv") )

od <- fread(here::here("zzz_temporary", "data", "od_example.csv"))

od[, travel_time := round(Total_Time, 1)][, Total_Time := NULL]

fwrite(od, here::here("zzz_temporary", "data", "od_example.csv") )

od <- fread(here::here("zzz_temporary", "data", "od_example.csv"))

od[, c("or", "dest") := list(str_sub(Name, 1, 6), str_sub(Name, nchar(Name)-5, nchar(Name)))] %>%
  .[, .(or, dest, travel_time)]

fwrite(od, here::here("zzz_temporary", "data", "od_example2.csv") )
