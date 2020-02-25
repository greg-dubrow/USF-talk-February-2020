## tuition/fees from IPEDS

library(tidyverse)
library(tidylog)
library(ipeds)
library(educationdata)
library(janitor)
library(readr)
library(patchwork)
library(scales)

# unitid for jesuit schools
jesids <- c("164924", "181002", "186432", "122931", "169716", "159656", "127918", "192323", "163046",
						"122612", "236595", "239105", "203368", "179159", "215770", "215929", "131496", "166124", "102234",
						"117946", "206622", "102234", "166124", "117946", "206622", "235316", "129242")

# use 2018 instchar to filter out enr 1618 by level and sector
# mac home
# instchar_18 <- as_tibble(readr::read_csv("~/Data/ipeds/instchar_2018.csv")) %>%
# pc work
instchar_18 <- as_tibble(readr::read_csv("C:/Data/ipeds/instchar_2018.csv")) %>%
	filter(ICLEVEL %in% c(1, 2)) %>%
	filter(SECTOR > 0 & SECTOR <=6) %>%
	mutate(UNITID = as.character(UNITID)) %>%
	mutate(jescoll = ifelse(UNITID %in% jesids, 1, 0)) %>%
	mutate(CARNEGIE = as.character(CARNEGIE)) %>%
	select(UNITID, INSTNM, SECTOR, ICLEVEL, CARNEGIE, jescoll)
