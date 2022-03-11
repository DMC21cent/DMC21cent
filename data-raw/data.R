## code to prepare `adsl` dataset goes here
library(haven)
library(here)
library(dplyr)
library(readr)
library(glue)

## Source: https://github.com/phuse-org/phuse-scripts/raw/master/data/adam/cdisc-split/***.xpt

download_if_not_exist <- function(domain, path = "cdisc-split"){
  if (!file.exists(here::here(glue::glue("data-raw/{domain}.xpt")))) {
    download.file(glue::glue("https://github.com/phuse-org/phuse-scripts/raw/master/data/adam/{path}/{domain}.xpt"),
                  here::here(glue::glue("data-raw/{domain}.xpt")))
  } else {
    return(invisible(NULL))
  }
}


## adsl ----------------------------------------------------
download_if_not_exist("adsl")

adsl <- read_xpt(
  here::here("data-raw", "adsl.xpt"),
  col_select = NULL,
  skip = 0,
  n_max = Inf,
  .name_repair = "unique"
) %>%
  filter (ARM %in% c("Placebo", "Xanomeline High Dose"))

write_csv(adsl, here("data-raw/adsl.csv"))
save(adsl, file = here("data/adsl.rda"), compress = "bzip2")

## adae ----------------------------------------------------
download_if_not_exist("adae")

adae <- read_xpt(
  here::here("data-raw", "adae.xpt"),
  col_select = NULL,
  skip = 0,
  n_max = Inf,
  .name_repair = "unique"
) %>%
  filter (TRTA %in% c("Placebo", "Xanomeline High Dose"))


write_csv(adae, here("data-raw/adae.csv"))
save(adae, file = here("data/adae.rda"), compress = "bzip2")

## adlbc ----------------------------------------------------
download_if_not_exist("adlbc")

adlbc <- read_xpt(
  here::here("data-raw", "adlbc.xpt"),
  col_select = NULL,
  skip = 0,
  n_max = Inf,
  .name_repair = "unique"
) %>% filter (TRTP %in% c("Placebo", "Xanomeline High Dose"))

save(adlbc, file = here("data/adlbc.rda"), compress = "bzip2")

# adlbh ----------------------------------------------------
download_if_not_exist("adlbh")

adlbh <- read_xpt(
  here::here("data-raw", "adlbh.xpt"),
  col_select = NULL,
  skip = 0,
  n_max = Inf,
  .name_repair = "unique"
) %>% filter (TRTP %in% c("Placebo", "Xanomeline High Dose"))

write_csv(adlbh, here("data-raw/adlbh.csv"))
save(adlbh, file = here("data/adlbh.rda"), compress = "bzip2")

# adlbhy ----------------------------------------------------
download_if_not_exist("adlbhy")

adlbhy <- read_xpt(
  here::here("data-raw", "adlbhy.xpt"),
  col_select = NULL,
  skip = 0,
  n_max = Inf,
  .name_repair = "unique"
) %>% filter (TRTP %in% c("Placebo", "Xanomeline High Dose"))

write_csv(adlbhy, here("data-raw/adlbhy.csv"))
save(adlbhy, file = here("data/adlbhy.rda"), compress = "bzip2")




## ADTTE ----------------------------------------------------
download_if_not_exist("adtte")

adtte <- read_xpt(
  here::here("data-raw", "adtte.xpt"),
  col_select = NULL,
  skip = 0,
  n_max = Inf,
  .name_repair = "unique"
) %>%
  filter (TRTA %in% c("Placebo", "Xanomeline High Dose"))

write_csv(adtte, here("data-raw/adtte.csv"))
save(adtte, file = here("data/adtte.rda"), compress = "bzip2")


## ADVS ----------------------------------------------------
download_if_not_exist("advs")

advs <- read_xpt(
  here::here("data-raw", "advs.xpt"),
  col_select = NULL,
  skip = 0,
  n_max = Inf,
  .name_repair = "unique"
) %>%
  filter (TRTA %in% c("Placebo", "Xanomeline High Dose"))

write_csv(advs, here("data-raw/advs.csv"))
save(advs, file = here("data/advs.rda"), compress = "bzip2")


## ADEX ----------------------------------------------------
ex <- haven::read_xpt("data-raw/ex.xpt")

write_csv(ex, here("data-raw/ex.csv"))
save(ex, file = here("data/ex.rda"), compress = "bzip2")



## ADCM ----------------------------------------------------
download_if_not_exist("adcm", path = "cdisc")

adcm <- read_xpt(
  here::here("data-raw", "adcm.xpt"),
  col_select = NULL,
  skip = 0,
  n_max = Inf,
  .name_repair = "unique"
) %>%
  filter (TRTA %in% c("Placebo", "Xanomeline High Dose"))

write_csv(adcm, here("data-raw/adcm.csv"))
save(adcm, file = here("data/adcm.rda"), compress = "bzip2")

