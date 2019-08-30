#!/usr/bin/env Rscript

# brd_elex_dt.R


# USEFUL LIBRARIES ----
library(data.table)
library(dplyr)
library(readr)

# USEFUL VARS ----
# `%>%` <- magrittr::`%>%`


# MICHIGAN BOARD OF ELECTIONS DATA ----

# _ LOAD DATA ----

# # _ _ Define fixed-widths and column names ----
# fixed_widths <- c(35, 20, 20, 3, 4, 1, 8, 1, 7, 4, 2, 30, 6, 2, 13, 35, 2, 5,
#                   50, 50, 50, 50, 50, 13, 2, 5, 6, 5, 5, 5, 5, 5, 5, 6, 6, 1,
#                   2, 1)
# header <- c("last_name", "first_name", "middle_name", "name_suffix",
#             "birth_year", "gender",
#             "regis_date",
#             "house_num_char", "res_street_num", "house_suffix",
#             "predirection", "street_name", "street_type",
#             "suffix_direction", "res_ext",
#             "city", "state", "zip",
#             "mail_add_1", "mail_add_2", "mail_add_3",
#             "mail_add_4", "mail_add_5",
#             "voter_id", "county_code", "juris", "ward_precinct", "school_code",
#             "state_house", "state_senate", "us_congress", "county_commis",
#             "village_code", "village_precinct", "school_precinct",
#             "perm_absent_ind", "status_type", "uocava_status")
# 
# # _ _ Get data from each .lst file ----
# brd_elex_data_path <-
#   "~/Box Sync/Documents/I-CONECT/Recruitment/Board of Elections data - Michigan/"
# 
# cnty_liv <-
#   read_fwf(paste0(brd_elex_data_path, "raw_data/Livingston.lst"),
#                   col_positions = fwf_widths(fixed_widths,
#                                                     col_names = header),
#                   progress = TRUE,
#                   col_types =
#                     cols(.default = col_character()))
# cnty_mac <-
#   read_fwf(paste0(brd_elex_data_path, "raw_data/Macomb.lst"),
#                   col_positions = fwf_widths(fixed_widths,
#                                                     col_names = header),
#                   progress = TRUE,
#                   col_types =
#                     cols(.default = col_character()))
# cnty_mon <-
#   read_fwf(paste0(brd_elex_data_path, "raw_data/Monroe.lst"),
#                   col_positions = fwf_widths(fixed_widths,
#                                                     col_names = header),
#                   progress = TRUE,
#                   col_types =
#                     cols(.default = col_character()))
# cnty_oak <-
#   read_fwf(paste0(brd_elex_data_path, "raw_data/Oakland.lst"),
#                   col_positions = fwf_widths(fixed_widths,
#                                                     col_names = header),
#                   progress = TRUE,
#                   col_types =
#                     cols(.default = col_character()))
# cnty_stc <-
#   read_fwf(paste0(brd_elex_data_path, "raw_data/StClair.lst"),
#                   col_positions = fwf_widths(fixed_widths,
#                                                     col_names = header),
#                   progress = TRUE,
#                   col_types =
#                     cols(.default = col_character()))
# cnty_wsh <-
#   read_fwf(paste0(brd_elex_data_path, "raw_data/Washtenaw.lst"),
#                   col_positions = fwf_widths(fixed_widths,
#                                                     col_names = header),
#                   progress = TRUE,
#                   col_types =
#                     cols(.default = col_character()))
# cnty_way <-
#   read_fwf(paste0(brd_elex_data_path, "raw_data/Wayne.lst"),
#                   col_positions = fwf_widths(fixed_widths,
#                                                     col_names = header),
#                   progress = TRUE,
#                   col_types =
#                     cols(.default = col_character()))
# 
# # How many rows?
# county_names <- list("cnty_liv", "cnty_mac", "cnty_mon", "cnty_oak",
#                      "cnty_stc", "cnty_wsh", "cnty_way")
# sum(purrr::map_int(county_names, function(x) nrow(get(x))))
# 
# # How much memory space?
# purrr::walk(county_names, function(x) {
#   print(object.size(get(x)), units = "auto")
# })
# 
# # County codes?
# purrr::walk(county_names, function(x) {
#   print(
#     paste(x,
#           as.numeric(unique(get(x)[, "county_code"]))))
# })
# 
# # _ _ Bind counties together ----
# counties <-
#   bind_rows(cnty_liv, cnty_mac, cnty_mon, cnty_oak,
#             cnty_stc, cnty_way, cnty_wsh)
# # _ _ Delete county-only data sets
# rm(cnty_liv); rm(cnty_mac); rm(cnty_mon); rm(cnty_oak)
# rm(cnty_stc); rm(cnty_way); rm(cnty_wsh) # ... purrr::walk didn't work ????
# 
# # _ _ Convert tibble to data.table ----
# class(counties)
# setDT(counties); class(counties)
# saveRDS(counties, 'counties.Rds')


# <<< ____ - - - - - - - - - - - ____ >>> ----
# <<< ____ START HERE ____ >>> ----
# <<< ____ - - - - - - - - - - - ____ >>> ----


# _ LOAD PREPROCESSED DATA ----
counties <- readRDS('counties.Rds')

# _ PREVIEW DATA ----

head(counties)
str(counties)
# counties %>% 
#   dplyr::summarise(N = n())
counties[, .N]
counties[, .N, keyby = birth_year]
counties[county_code == 81L &  ## 81 = Washtenaw
           birth_year <= lubridate::year(lubridate::today()) - 70L & 
           birth_year >= lubridate::year(lubridate::today()) - 100L, 
         .N, 
         keyby = birth_year]
counties[, .N, keyby = gender]
counties[, .N, keyby = county_code] # see purrr::walk above for county codes
# counties[, .N, keyby = street_type]

indices(counties)
setindex(counties, NULL) 
indices(counties)

# _ MUTATES ----

# _ _ Mutate `birth_year` as integer ----
# counties <- counties %>% dplyr::mutate(birth_year = as.integer(birth_year))
counties[, birth_year := as.integer(birth_year)]
str(counties)

# _ _ Mutate `regis_date` field as Date (registration date) ----
# counties <- counties %>% 
#   dplyr::mutate(regis_date = lubridate::mdy(regis_date))
counties[, regis_date := lubridate::mdy(regis_date)]
str(counties)

# _ _ Mutate `county_code` field as integer ----
sort(unique(counties[, county_code]))
counties[, county_code := as.integer(county_code)]
str(counties)

# _ _ Mutate `zip` field as integer ----
# This is okay because no Michigan ZIPs start with leading zero.
sort(unique(counties[, zip]))
counties[, zip := as.integer(zip)]
str(counties)

counties_wsht_slct <- 
  counties[county_code == 81L &  ## 81 = Washtenaw
             birth_year <= lubridate::year(lubridate::today()) - 70L & 
             birth_year >= lubridate::year(lubridate::today()) - 100L, 
           .(
             last_name
             , first_name
             , birth_year
             , gender
             , house_num_char
             , res_street_num
             , house_suffix
             , predirection
             , street_name
             , street_type
             , suffix_direction
             , res_ext
             , city
             , state
             , zip
             )]

fwrite(counties_wsht_slct, 
       paste0("MichVoterRegis_Washtenaw_SelectFields_", Sys.Date(), ".csv"),
       na = "")


