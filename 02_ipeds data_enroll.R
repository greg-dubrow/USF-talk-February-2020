## ipeds data for trend enrollment & other data

library(tidyverse)
library(tidylog)
library(ipeds)
library(educationdata)
library(janitor)
library(readr)

# unitid for jesuit schools
jesids <- c("164924", "181002", "186432", "122931", "169716", "159656", "127918", "192323", "163046",
	"236595", "239105", "203368", "179159", "215770", "215929", "131496", "166124", "102234",
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

glimpse(instchar_18)
instchar_18 %>%
	count(ICLEVEL)

## load fall enrolls
# 2018
# mac home
#fallenroll_2018 <- as_tibble(readr::read_csv("~/Data/ipeds/fallenroll_2018.csv")) %>%
# pc work
fallenroll_2018 <- as_tibble(readr::read_csv("C:/Data/ipeds/fallenroll_2018.csv")) %>%
	filter(EFALEVEL == 2) %>%
	mutate(UNITID = as.character(UNITID)) %>%
	mutate(year = "Fall 2018") %>%
	select(UNITID, year, total_enr_ug = EFTOTLT)

glimpse(fallenroll_2018)

fallenroll_2018 %>%
	count(jescoll)

# mac home
#fallenroll_2017 <- as_tibble(readr::read_csv("~/Data/ipeds/fallenroll_2017.csv")) %>%
# pc work
fallenroll_2017 <- as_tibble(readr::read_csv("C:/Data/ipeds/fallenroll_2017.csv")) %>%
	filter(EFALEVEL == 2) %>%
	mutate(UNITID = as.character(UNITID)) %>%
	mutate(year = "Fall 2017") %>%
	select(UNITID, year, total_enr_ug = EFTOTLT)

glimpse(fallenroll_2017)

#fallenroll_2016 <- as_tibble(readr::read_csv("~/Data/ipeds/fallenroll_2016.csv")) %>%
# pc work
fallenroll_2016 <- as_tibble(readr::read_csv("C:/Data/ipeds/fallenroll_2016.csv")) %>%
	filter(EFALEVEL == 2) %>%
	mutate(UNITID = as.character(UNITID)) %>%
	mutate(year = "Fall 2016") %>%
	select(UNITID, year, total_enr_ug = EFTOTLT)

glimpse(fallenroll_2016)

# rbind enrolls, right join on instchar filter, remove records with no enrolls
fallenroll1618 <- do.call("rbind", list(fallenroll_2018, fallenroll_2017, fallenroll_2016)) %>%
	right_join(instchar_18) %>%
	filter(!is.na(total_enr_ug)) %>%
	select(UNITID, year, total_enr_ug) %>%
	arrange(year, UNITID)

glimpse(fallenroll1618)

fallenroll1618 %>%
	count(SECTOR)

#mac home
#delta0015all <- (haven::read_sas("~/Data/ipeds/delta_public_release_00_15.sas7bdat", NULL))
# pc work
delta0015all <- (haven::read_sas("C:/Data/ipeds/delta_public_release_00_15.sas7bdat", NULL))
glimpse(delta0015all)

delta0015all %>%
	filter(iclevel %in% c(1, 2)) %>%
	count(sector_revised)
	filter(sector_revised == 0) %>%
	count(unitid)


delta0015 <- as_tibble(delta0015all) %>%
	filter(iclevel %in% c(1, 2)) %>%
	filter(sector_revised != 0) %>%
	filter(sector_revised != 6) %>%
	mutate(UNITID = as.character(unitid)) %>%
	mutate(year = paste("Fall", academicyear, sep = " ")) %>%
	select(UNITID, year, total_enr_ug = total_undergraduates)

glimpse(delta0015)

delta0015 %>%
	count(year)

#mac home
#delta8799all <- (haven::read_sas("~/Data/ipeds/delta_public_release_87_99.sas7bdat", NULL))
# pc work
delta8799all <- (haven::read_sas("C:/Data/ipeds/delta_public_release_87_99.sas7bdat", NULL))
glimpse(delta8799all)

delta8799all %>%
	count(carnegie2000)

delta8799 <- as_tibble(delta8799all) %>%
	filter(iclevel %in% c(1, 2)) %>%
	filter(sector_revised != 0) %>%
	filter(sector_revised != 6) %>%
	mutate(UNITID = as.character(unitid)) %>%
	mutate(year = paste("Fall", academicyear, sep = " ")) %>%
	select(UNITID, year, total_enr_ug = total_undergraduates)

glimpse(delta8799)

## rowbind enr files
fallenrolla <- do.call("rbind", list(fallenroll1618, delta0015, delta8799)) %>%
	filter(!is.na(total_enr_ug)) %>%
	arrange(year, UNITID)

glimpse(fallenrolla)

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

instchar %>%
	count(inst_sec)

## left join enr with instchar

ipeds_fallenroll_8718 <- merge(fallenrolla, instchar) %>%
	select(UNITID, inst_name, inst_sec, lac, jescoll, year, total_enr_ug) %>%
	mutate(inst_sec_desc = case_when(inst_sec == 1 ~ "Public 4-year or above",
																	 inst_sec == 2 ~ "Private nonprofit 4-year or above",
																	 inst_sec == 3 ~ "Private for-profit 4-year or above",
																	 inst_sec == 4 ~ "Public 2-Year",
																	 inst_sec == 5 ~ "Private nonprofit 2-year",
																	 inst_sec == 6 ~ "Private for-profit 2-year"))  %>%
	arrange(UNITID, year) %>%
	group_by(UNITID) %>%
	mutate(enr_change = (total_enr_ug - lag(total_enr_ug))) %>%
	mutate(enr_pct_change = (total_enr_ug/lag(total_enr_ug) - 1) * 100) %>%
	ungroup() %>%
	select(UNITID, inst_name, inst_sec, inst_sec_desc, lac, jescoll,
				 year, total_enr_ug, enr_change, enr_pct_change)
glimpse(ipeds_fallenroll_8718)

ipeds_fallenroll_8718 %>%
	count(year)

saveRDS(ipeds_fallenroll_8718, file = "data/ipeds_fallenroll_8718.rds")

fallenroll %>%
	count(inst_sec)




## jesuit schools
# 	122612	University of San FranciscoInfo	San Francisco	CA
# 164924	Boston CollegeInfo	Chestnut Hill	MA
# 181002	Creighton University	Omaha	NE
# 186432	Saint Peter's University	Jersey City	NJ
# 	122931	Santa Clara University	Santa Clara	CA
# 	169716	University of Detroit Mercy	Detroit	MI
# 	159656	Loyola University New Orleans	New Orleans	LA
# 	127918	Regis University	Denver	CO
# 	192323	Le Moyne College	Syracuse	NY
# 	163046	Loyola University Maryland	Baltimore	MD
# 	236595	Seattle University	Seattle	WA
# 	239105	Marquette University	Milwaukee	WI
# 	203368	John Carroll University	University Heights	OH
# 	179159	Saint Louis University	Saint Louis	MO
# 	215770	Saint Joseph's University	Philadelphia	PA
# 215929	University of Scranton	Scranton	PA
# 131496	Georgetown University	Washington	DC
# 166124	College of the Holy Cross	Worcester	MA
# 102234	Spring Hill College	Mobile	AL
# 117946	Loyola Marymount University	Los Angeles	CA
# 206622	Xavier University	Cincinnati	OH
# 102234	Spring Hill College	Mobile	AL
# 166124	College of the Holy Cross	Worcester	MA
# 117946	Loyola Marymount University	Los Angeles	CA
# 206622	Xavier University	Cincinnati	OH
# 235316	Gonzaga University	Spokane	WA
# 129242	Fairfield University	Fairfield	CT


## old code
# ipeds package
# ipeds::download_ipeds(year = 2016, dir = "/Users/gregdubrow/Data/USF-talk-February-2020/data/")
# ipeds2017 <- ipeds::load_ipeds(year = 2017)
# names(ipeds2017)

# education data package
# ipeds17_racesex <- get_education_data(level = 'college-university',
# 																			source = 'ipeds',
# 																			topic = 'fall-enrollment',
# 																			by = list('race', 'sex'),
# 																			filters = list(year = 2017),
# 																			#												 							 grade = 9:12,
# 																			#												 							 ncessch = '340606000122'),
# 																			add_labels = TRUE)
#
# glimpse(ipeds17)
# saveRDS(ipeds17, "data/ipeds17_agesex.rds")
# ipeds17_agesex <- readRDS("data/ipeds17_agesex.rds")
#
# years <- c(2016, 2015)
# ipeds16 <- get_education_data(level = 'college-university',
# 															source = 'ipeds',
# 															topic = 'fall-enrollment',
# 															by = list("race", "sex"),
# 															filters = list(year = 2016),
# 															add_labels = TRUE,
# 															csv = TRUE)
#
instchar_18 <- as_tibble(readr::read_csv("~/Data/ipeds/instchar_2018.csv")) %>%
	mutate(UNITID = as.character(UNITID)) %>%
	mutate(CARNEGIE = as.character(CARNEGIE))

glimpse(instchar_18)
