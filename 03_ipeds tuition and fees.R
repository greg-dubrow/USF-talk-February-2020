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
instchar_18 <- as_tibble(readr::read_csv("~/Data/ipeds/instchar_2018.csv")) %>%
# pc work
# instchar_18 <- as_tibble(readr::read_csv("C:/Data/ipeds/instchar_2018.csv")) %>%
	filter(ICLEVEL %in% c(1, 2)) %>%
	filter(SECTOR > 0 & SECTOR <=6) %>%
	mutate(UNITID = as.character(UNITID)) %>%
	mutate(jescoll = ifelse(UNITID %in% jesids, 1, 0)) %>%
	mutate(CARNEGIE = as.character(CARNEGIE)) %>%
	select(UNITID, INSTNM, SECTOR, ICLEVEL, CARNEGIE, jescoll)

glimpse(instchar_18)

# tuition from ipeds
# mac home
tuit_18 <- as_tibble(readr::read_csv("~/Data/ipeds/tuition_2018.csv")) %>%
# work desktop
# tuit_18 <- as_tibble(readr::read_csv("C:/Data/ipeds/tuition_2018.csv")) %>%
	mutate(UNITID = as.character(UNITID)) %>%
	mutate(year = "Fall 2018") %>%
	select(UNITID, year, tuition_fee_ug = CHG2AY3) %>%
	mutate(tuition_fee_ug = as.double(tuition_fee_ug))
glimpse(tuit_18)

tuit_17 <- as_tibble(readr::read_csv("~/Data/ipeds/tuition_2018.csv")) %>%
	# work desktop
	# tuit_18 <- as_tibble(readr::read_csv("C:/Data/ipeds/tuition_2018.csv")) %>%
	mutate(UNITID = as.character(UNITID)) %>%
	mutate(year = "Fall 2017") %>%
	select(UNITID, year, tuition_fee_ug = CHG2AY2) %>%
	mutate(tuition_fee_ug = as.double(tuition_fee_ug))

glimpse(tuit_17)

tuit_16 <- as_tibble(readr::read_csv("~/Data/ipeds/tuition_2018.csv")) %>%
	# work desktop
	# tuit_18 <- as_tibble(readr::read_csv("C:/Data/ipeds/tuition_2018.csv")) %>%
	mutate(UNITID = as.character(UNITID)) %>%
	mutate(year = "Fall 2016") %>%
	select(UNITID, year, tuition_fee_ug = CHG2AY1) %>%
	mutate(tuition_fee_ug = as.double(tuition_fee_ug))
glimpse(tuit_16)

tuit1618 <- fallenroll1618 <- do.call("rbind", list(tuit_18, tuit_17, tuit_16)) %>%
	right_join(instchar_18) %>%
	#filter(!is.na(tot_enr)) %>%
	select(UNITID, year, tuition_fee_ug) %>%
	arrange(year, UNITID)

glimpse(tuit1618)

# tuition data from delta
#mac home
delta0015all <- (haven::read_sas("~/Data/ipeds/delta_public_release_00_15.sas7bdat", NULL))
# pc work
# delta0015all <- (haven::read_sas("C:/Data/ipeds/delta_public_release_00_15.sas7bdat", NULL))
glimpse(delta0015all)

delta0015_tuitaid <- delta0015all %>%
	filter(iclevel %in% c(1, 2)) %>%
	filter(sector_revised != 0) %>%
	filter(sector_revised != 6) %>%
	mutate(UNITID = as.character(unitid)) %>%
	mutate(year = paste("Fall", academicyear, sep = " ")) %>%
	select(UNITID, year, tuition_fee_ug = tuitionfee02_tf)

glimpse(delta0015_tuitaid)

delta0015_tuitaid %>%
	count(year)

#mac home
delta8799all <- (haven::read_sas("~/Data/ipeds/delta_public_release_87_99.sas7bdat", NULL))
# pc work
#delta8799all <- (haven::read_sas("C:/Data/ipeds/delta_public_release_87_99.sas7bdat", NULL))
glimpse(delta8799all)

delta8799_tuitaid <- as_tibble(delta8799all) %>%
	filter(iclevel %in% c(1, 2)) %>%
	filter(sector_revised != 0) %>%
	filter(sector_revised != 6) %>%
	mutate(UNITID = as.character(unitid)) %>%
	mutate(year = paste("Fall", academicyear, sep = " ")) %>%
	select(UNITID, year, tuition_fee_ug = tuitionfee02_tf)
glimpse(delta8799_tuitaid)

## rowbind enr files
tuit_8718 <- do.call("rbind", list(tuit1618, delta0015_tuitaid, delta8799_tuitaid)) %>%
	#	filter(!is.na(total_enr_ug)) %>%
	arrange(year, UNITID)
glimpse(tuit_8718)

## load inst characteristics from delta
delta_ic8799 <-  as_tibble(delta8799all) %>%
	filter(iclevel %in% c(1, 2)) %>%
	filter(sector_revised >=1 & sector_revised <=6) %>%
	mutate(UNITID = as.character(unitid)) %>%
	mutate(lac8799 = ifelse(carnegie2000 == 31, 1, 0)) %>%
	select(UNITID, academicyear, instnm8799 = instname, instsec8799 = sector_revised, lac8799) %>%
	arrange(UNITID, academicyear) %>%
	distinct(UNITID, .keep_all = T) %>%
	select(-academicyear)

glimpse(delta_ic8799)

delta_ic0015 <-  as_tibble(delta0015all) %>%
	filter(iclevel %in% c(1, 2)) %>%
	filter(sector_revised >=1 & sector_revised <=6) %>%
	mutate(UNITID = as.character(unitid)) %>%
	mutate(lac0015 = ifelse(carnegie2000 == 31, 1, 0)) %>%
	select(UNITID, academicyear, instnm0015 = instname, instsec0015 = sector_revised, lac0015) %>%
	arrange(UNITID, academicyear) %>%
	distinct(UNITID, .keep_all = T) %>%
	select(-academicyear)

glimpse(delta_ic0015)

delta_ic0015 %>%
	count(instsec0015)

instchar <- merge(delta_ic8799, delta_ic0015, all = T) %>%
	mutate(inst_name = ifelse(!is.na(instnm0015), instnm0015, instnm8799)) %>%
	mutate(inst_name = str_to_title(inst_name)) %>%
	mutate(lac = ifelse(!is.na(lac0015), lac0015, lac8799)) %>%
	mutate(inst_sec = ifelse(!is.na(instsec0015), instsec0015, instsec8799)) %>%
	mutate(jescoll = ifelse(UNITID %in% jesids, 1, 0)) %>%
	arrange(UNITID) %>%
	distinct(UNITID, .keep_all = T) %>%
	select(UNITID, inst_name, inst_sec, lac, jescoll)

glimpse(instchar)

ipeds_tuition_8718 <- merge(tuit_8718, instchar) %>%
	select(UNITID, inst_name, inst_sec, lac, jescoll, year, tuition_fee_ug) %>%
	mutate(inst_sec_desc = case_when(inst_sec == 1 ~ "Public 4-year or above",
																	 inst_sec == 2 ~ "Private nonprofit 4-year or above",
																	 inst_sec == 3 ~ "Private for-profit 4-year or above",
																	 inst_sec == 4 ~ "Public 2-Year",
																	 inst_sec == 5 ~ "Private nonprofit 2-year",
																	 inst_sec == 6 ~ "Private for-profit 2-year"))  %>%
	arrange(UNITID, year) %>%
	group_by(UNITID) %>%
	mutate(tuit_change = (tuition_fee_ug - lag(tuition_fee_ug))) %>%
	mutate(tuit_pct_change = (tuition_fee_ug/lag(tuition_fee_ug) - 1) * 100) %>%
	ungroup() %>%
	select(UNITID, inst_name, inst_sec, inst_sec_desc, lac, jescoll,
				 year, tuition_fee_ug, tuit_change, tuit_pct_change)
glimpse(ipeds_tuition_8718)

ipeds_fallenroll_8718 %>%
	count(year)

saveRDS(ipeds_tuition_8718, file = "data/ipeds_tuition_8718.rds")
ipeds_tuition_8718 <- readRDS(file = "data/ipeds_tuition_8718.rds")

ipeds_tuition_8718 %>%
	filter(jescoll == 1, UNITID != "122612", year >= "Fall 1989") %>%
	group_by(year) %>%
	summarise(tuit_mean = mean(tuition_fee_ug)) %>%
	view()


