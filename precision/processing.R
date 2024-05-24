library(tidyverse)
library(readxl)


get_house <- function(i) {
  vals <- read_excel(
    "For Review_Ret_Justa_July KPT Complete_KPT_4day (1).xlsx",
    range = "I15:L15",
    sheet = paste0("HH", i, " Data"),
    col_names = FALSE
  ) %>%
    as.matrix() %>%
    t() %>%
    .[, 1]
  vals <- vals[!is.na(vals)]
  data.frame(
    house = rep(i, length(vals)),
    wood = vals
  )
}


all_meas <-
  map_dfr(factor(1:16), get_house) %>%
  group_by(house) %>%
  mutate(mean_percap = mean(wood)) %>%
  mutate(deviation = wood - mean_percap)

write_csv(all_meas, file = "all_meas.csv")
