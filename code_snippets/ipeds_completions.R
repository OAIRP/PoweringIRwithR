# load libraries
library("tidyverse")
library("readr")
library("curl")

# download and process data
IPEDS_url <- "https://nces.ed.gov/ipeds/datacenter/data/"
IPEDS_year <- 2022

curl_download(
    paste0(IPEDS_url, "HD", IPEDS_year, ".zip"),
    destfile = paste0("HD", IPEDS_year, ".zip")
    )

curl_download(
    paste0(IPEDS_url, "C", IPEDS_year, "_A", ".zip"),
    destfile = paste0("C", IPEDS_year, "_A", ".zip")
    )

# read in IPEDS IC Header data
institutions <- read_csv(
    paste0("HD", IPEDS_year, ".zip"),
    )  %>%

# select variables of interest
    select(UNITID, INSTNM, OPEID, STABBR, SECTOR, HLOFFER) %>%

# create new variables
    mutate(
        OPEID_branch = substr(OPEID, 7, 8),
        )

# read in IPEDS Completions data
completions <- read_csv(
    paste0("C", IPEDS_year, "_A.zip"),
    ) %>%

# select variables of interest
    select(UNITID, CIPCODE, MAJORNUM, AWLEVEL, CTOTALT) %>%

# filter first majors only
    filter(MAJORNUM == 1)

# combine datasets
combined <- full_join(institutions, completions, by = "UNITID", keep = TRUE) %>%

# filter to Ohio institutions
    filter(STABBR == "OH")
