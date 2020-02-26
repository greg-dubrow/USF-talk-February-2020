## ipeds data for trend enrollment & other data

library(tidyverse)
library(tidylog)
#library(ipeds)
#library(educationdata)
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

glimpse(instchar_18)
instchar_18 %>%
	count(ICLEVEL)

## load fall enrolls
# 2018
# mac home
#fallenroll_2018 <- as_tibble(readr::read_csv("~/Data/ipeds/fallenroll_2018.csv")) %>%
# pc work
fallenroll_2018 <- as_tibble(readr::read_csv("C:/Data/ipeds/fallenroll_2018.csv")) %>%
	filter(EFALEVEL == 2 | EFALEVEL == 12) %>%
	mutate(UNITID = as.character(UNITID)) %>%
	mutate(level = case_when(EFALEVEL == 2 ~ "Undergraduate",
													 EFALEVEL == 12 ~ "Graduate")) %>%
	mutate(year = "Fall 2018") %>%
	mutate(tot_enr = EFTOTLT) %>%
	group_by(UNITID) %>%
	mutate(enrall = sum(tot_enr)) %>%
	mutate(pct_enr = tot_enr / enrall) %>%
	ungroup() %>%
	select(UNITID, year, level, tot_enr, enrall, pct_enr)

glimpse(fallenroll_2018)

fallenroll_2018 %>%
	count(jescoll)

# mac home
#fallenroll_2017 <- as_tibble(readr::read_csv("~/Data/ipeds/fallenroll_2017.csv")) %>%
# pc work
fallenroll_2017 <- as_tibble(readr::read_csv("C:/Data/ipeds/fallenroll_2017.csv")) %>%
	filter(EFALEVEL == 2 | EFALEVEL == 12) %>%
	mutate(UNITID = as.character(UNITID)) %>%
	mutate(level = case_when(EFALEVEL == 2 ~ "Undergraduate",
													 EFALEVEL == 12 ~ "Graduate")) %>%
	mutate(year = "Fall 2017") %>%
	mutate(tot_enr = EFTOTLT) %>%
	group_by(UNITID) %>%
	mutate(enrall = sum(tot_enr)) %>%
	mutate(pct_enr = tot_enr / enrall) %>%
	ungroup() %>%
	select(UNITID, year, level, tot_enr, enrall, pct_enr)

glimpse(fallenroll_2017)

#fallenroll_2016 <- as_tibble(readr::read_csv("~/Data/ipeds/fallenroll_2016.csv")) %>%
# pc work
fallenroll_2016 <- as_tibble(readr::read_csv("C:/Data/ipeds/fallenroll_2016.csv")) %>%
	filter(EFALEVEL == 2 | EFALEVEL == 12) %>%
	mutate(UNITID = as.character(UNITID)) %>%
	mutate(level = case_when(EFALEVEL == 2 ~ "Undergraduate",
													 EFALEVEL == 12 ~ "Graduate")) %>%
	mutate(year = "Fall 2016") %>%
	mutate(tot_enr = EFTOTLT) %>%
	group_by(UNITID) %>%
	mutate(enrall = sum(tot_enr)) %>%
	mutate(pct_enr = tot_enr / enrall) %>%
	ungroup() %>%
	select(UNITID, year, level, tot_enr, enrall, pct_enr)

glimpse(fallenroll_2016)

#fallenroll_2015 <- as_tibble(readr::read_csv("~/Data/ipeds/fallenroll_2015.csv")) %>%
# pc work
fallenroll_2015 <- as_tibble(readr::read_csv("C:/Data/ipeds/fallenroll_2015.csv")) %>%
	filter(EFALEVEL == 2 | EFALEVEL == 12) %>%
	mutate(UNITID = as.character(UNITID)) %>%
	mutate(level = case_when(EFALEVEL == 2 ~ "Undergraduate",
													 EFALEVEL == 12 ~ "Graduate")) %>%
	mutate(year = "Fall 2015") %>%
	mutate(tot_enr = EFTOTLT) %>%
	group_by(UNITID) %>%
	mutate(enrall = sum(tot_enr)) %>%
	mutate(pct_enr = tot_enr / enrall) %>%
	ungroup() %>%
	select(UNITID, year, level, tot_enr, enrall, pct_enr)

glimpse(fallenroll_2015)


# rbind enrolls, right join on instchar filter, remove records with no enrolls
fallenroll1518 <- do.call("rbind", list(fallenroll_2018, fallenroll_2017,
																				fallenroll_2016, fallenroll_2015)) %>%
	right_join(instchar_18) %>%
	#filter(!is.na(tot_enr)) %>%
	select(UNITID, year, level, tot_enr, enrall, pct_enr) %>%
	arrange(year, UNITID)

glimpse(fallenroll1518)


## note - academic year in delt files is named to trailing year, so 2014-15 = academic year 2015
# fall enroll data adjusted to trailing year, so data from Fall 1999 to fall 2014
#mac home
#delta0015all <- (haven::read_sas("~/Data/ipeds/delta_public_release_00_15.sas7bdat", NULL))
# pc work
delta0015all <- (haven::read_sas("C:/Data/ipeds/delta_public_release_00_15.sas7bdat", NULL))
glimpse(delta0015all)

delta0015all %>%
	filter(unitid == 122597) %>%
	select(total_undergraduates, total_postbacc, total_enrollment)


delta0015ug <- as_tibble(delta0015all) %>%
	filter(iclevel %in% c(1, 2)) %>%
	filter(sector_revised != 0) %>%
	filter(sector_revised != 6) %>%
	mutate(UNITID = as.character(unitid)) %>%
	mutate(year_n = as.double(academicyear) - 1) %>%
	mutate(year = paste("Fall", year_n, sep = " ")) %>%
	mutate(level = "Undergraduate") %>%
	mutate(tot_enr = total_undergraduates) %>%
	select(UNITID, year, level, tot_enr)

glimpse(delta0015ug)

delta0015ug %>%
	count(year)

delta0015gr <- as_tibble(delta0015all) %>%
	filter(iclevel %in% c(1, 2)) %>%
	filter(sector_revised != 0) %>%
	filter(sector_revised != 6) %>%
	mutate(UNITID = as.character(unitid)) %>%
	mutate(year_n = as.double(academicyear) - 1) %>%
	mutate(year = paste("Fall", year_n, sep = " ")) %>%
	mutate(level = "Graduate") %>%
	mutate(tot_enr = total_postbacc) %>%
	select(UNITID, year, level, tot_enr)

glimpse(delta0015gr)

delta0015 <- gdata::interleave(delta0015ug, delta0015gr) %>%
	mutate(tot_enr = ifelse(is.na(tot_enr), 0, tot_enr)) %>%
	group_by(UNITID, year) %>%
	mutate(enrall = sum(tot_enr)) %>%
	mutate(pct_enr = tot_enr / enrall) %>%
	ungroup() %>%
	arrange(UNITID, year) %>%
	filter(enrall >0) %>%
	select(UNITID, year, level, tot_enr, enrall, pct_enr)

glimpse(delta0015)

delta0015 %>%
	count(year)

## note - academic year in delt files is named to trailing year, so 1998-99 = academic year 1999
# fall enroll data adjusted to trailing year, so data from Fall 1986 to fall 1998
#mac home
#delta8799all <- (haven::read_sas("~/Data/ipeds/delta_public_release_87_99.sas7bdat", NULL))
# pc work
delta8799all <- (haven::read_sas("C:/Data/ipeds/delta_public_release_87_99.sas7bdat", NULL))
glimpse(delta8799all)

delta8799all %>%
	count(carnegie2000)

delta8799ug <- as_tibble(delta8799all) %>%
	filter(iclevel %in% c(1, 2)) %>%
	filter(sector_revised != 0) %>%
	filter(sector_revised != 6) %>%
	mutate(UNITID = as.character(unitid)) %>%
	mutate(year_n = as.double(academicyear) - 1) %>%
	mutate(year = paste("Fall", year_n, sep = " ")) %>%
	mutate(level = "Undergraduate") %>%
	mutate(tot_enr = total_undergraduates) %>%
	select(UNITID, year, level, tot_enr)

glimpse(delta8799ug)

delta8799gr <- as_tibble(delta8799all) %>%
	filter(iclevel %in% c(1, 2)) %>%
	filter(sector_revised != 0) %>%
	filter(sector_revised != 6) %>%
	mutate(UNITID = as.character(unitid)) %>%
	mutate(year_n = as.double(academicyear) - 1) %>%
	mutate(year = paste("Fall", year_n, sep = " ")) %>%
	mutate(level = "Graduate") %>%
	mutate(tot_enr = total_postbacc) %>%
	select(UNITID, year, level, tot_enr)


delta8799 <- gdata::interleave(delta8799ug, delta8799gr) %>%
	mutate(tot_enr = ifelse(is.na(tot_enr), 0, tot_enr)) %>%
	group_by(UNITID, year) %>%
	mutate(enrall = sum(tot_enr)) %>%
	mutate(pct_enr = tot_enr / enrall) %>%
	ungroup() %>%
	filter(enrall >0) %>%
	select(UNITID, year, level, tot_enr, enrall, pct_enr)
glimpse(delta8799)

## rowbind enr files
fallenrolla <- do.call("rbind", list(fallenroll1518, delta0015, delta8799)) %>%
#	filter(!is.na(total_enr_ug)) %>%
	filter(year != "Fall 1986") %>%
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
	select(UNITID, inst_name, inst_sec, lac, jescoll, year, level, tot_enr, enrall, pct_enr) %>%
	mutate(inst_sec_desc = case_when(inst_sec == 1 ~ "Public 4-year or above",
																	 inst_sec == 2 ~ "Private nonprofit 4-year or above",
																	 inst_sec == 3 ~ "Private for-profit 4-year or above",
																	 inst_sec == 4 ~ "Public 2-Year",
																	 inst_sec == 5 ~ "Private nonprofit 2-year",
																	 inst_sec == 6 ~ "Private for-profit 2-year"))  %>%
	arrange(UNITID, year, level) %>%
	group_by(UNITID, level) %>%
	mutate(enr_change = (tot_enr - lag(tot_enr))) %>%
	mutate(enr_pct_change = (tot_enr/lag(tot_enr) - 1) * 100) %>%
	ungroup() %>%
	select(UNITID, inst_name, inst_sec, inst_sec_desc, lac, jescoll,
				 year, level, tot_enr, enrall, pct_enr, enr_change, enr_pct_change)
glimpse(ipeds_fallenroll_8718)

ipeds_fallenroll_8718 %>%
	count(year) %>%
	view()

saveRDS(ipeds_fallenroll_8718, file = "data/ipeds_fallenroll_8718.rds")
ipeds_fallenroll_8718 <- readRDS(file = "data/ipeds_fallenroll_8718.rds")

## charts
# USF UG & grad enroll over time

ipeds_fallenroll_8718 %>%
	filter(UNITID == "122612", level == "Graduate") %>%
	summary(tot_enr)

plot_usfenr_ug <-
	ipeds_fallenroll_8718 %>%
	filter(UNITID == "122612", level == "Undergraduate") %>%
	select(year, level, tot_enr) %>%
	ggplot(aes(year, tot_enr)) +
	geom_bar(stat = "identity", fill = "#00543C") +
	geom_text(aes(label = scales::comma(tot_enr)), color = "#919194", vjust = -.75, size = 5) +
	labs(x = "", y = "") +
	scale_y_continuous(label = scales::comma, limits = c(0, 7000),
										 breaks = c(0, 1750, 3500, 5250, 7000)) +
	theme_minimal() +
	theme(text = element_text(family = "Calibri"),
				panel.grid.major = element_blank(),	panel.grid.minor = element_blank(),
				axis.text.y = element_text(size = 10))

plot_usfenr_gr <-
	ipeds_fallenroll_8718 %>%
	filter(UNITID == "122612", level == "Graduate") %>%
	select(year, level, tot_enr) %>%
	ggplot(aes(year, tot_enr)) +
	geom_bar(stat = "identity", fill = "#FDBB30") +
	geom_text(aes(label = scales::comma(tot_enr, accuracy = 1)),
						color = "#919194", vjust = -1.15, size = 5) +
	labs(x = "", y = "") +
	scale_y_continuous(label = scales::comma, limits = c(0, 4500),
										 breaks = c(0, 1500, 3000, 4500)) +
	theme_minimal() +
	theme(text = element_text(family = "Calibri"),
		panel.grid.major = element_blank(),	panel.grid.minor = element_blank(),
				axis.text.y = element_text(size = 10))

plot_usfenr_all <- plot_usfenr_ug + plot_usfenr_gr +
	plot_layout(ncol = 1)

ggsave("figs/plot_usf_enr87to18.png", plot_usfenr_all, device = "png", dpi = 160,
				 width = 18.89, height = 10, units = "in")

ipeds_fallenroll_8718 %>%
	filter(UNITID == "122612", level == "Undergraduate") %>%
	select(pct_enr) %>%
	summary()

ipeds_fallenroll_8718 %>%
	filter(UNITID == "122612", level == "Undergraduate") %>%
	select(year, pct_enr) %>%
	ggplot(aes(year, pct_enr, group = 1)) +
	geom_line(color = "#00543C", size = 1.5) +
	# geom_text(aes(label = scales::comma(tot_enr, accuracy = 1)),
	# 					color = "#919194", vjust = -1.15, size = 5) +
	labs(x = "", y = "") +
	annotate("text", x = 10, y = .9, label = "Ratio of undergrad:grad students fairly steady",
					 size = 5, fontface = "italic", color = "#919194") +
	annotate("text", x = 6.4, y = .8, label = "ranged from 56% to 64%",
					 size = 5, fontface = "italic", color = "#919194") +
	scale_y_continuous(label = scales::percent, limits = c(0, 1),
										 breaks = c(0, .2, .4, .6, .8, 1)) +
	theme_minimal() +
	theme(text = element_text(family = "Calibri"),
				panel.grid.major = element_blank(),	panel.grid.minor = element_blank(),
				axis.text.y = element_text(size = 10))

ggsave("figs/plot_usf_pctug87to18.png", device = "png", dpi = 160,
			 width = 18.89, height = 10, units = "in")

#adjusted enrollment trends for jesuit colleges
enrollindex_jes <-
ipeds_fallenroll_8718 %>%
	filter(jescoll == 1, level == "Undergraduate") %>%
  mutate(enr_pct_change2 = enr_pct_change / 100) %>%
	mutate(enr_pct_change2 = ifelse(year == "Fall 1987", 1, enr_pct_change2)) %>%
	arrange(UNITID, year) %>%
	group_by(UNITID) %>%
	mutate(index_enr_inst = 1) %>%
	mutate(index_enr_inst = ifelse(year >= "Fall 1988", cumsum(enr_pct_change2),
																 index_enr_inst)) %>%
	# mutate(index_enr_inst = ifelse(year >= "Fall 1989", lag(index_enr_inst) + enr_pct_change2,
	# 															 index_enr_inst)) %>%
	ungroup() %>%
	## fix loyoal NO b/c of enroll drop after katrina
	mutate(index_enr_inst = ifelse((UNITID == "159656" & year == "Fall 2006"), 0.833399497, index_enr_inst)) %>%
	# mutate(index_enr_inst = ifelse((UNITID == "159656" & is.infinite(index_enr_inst)),
	# 															 lag(index_enr_inst) + enr_pct_change2, index_enr_inst)) %>%
	mutate(index_enr_inst = ifelse((UNITID == "159656" & year >= "Fall 2007"),
																 lag(index_enr_inst) + enr_pct_change2, index_enr_inst)) %>%
	mutate(index_enr_inst = ifelse((UNITID == "159656" & year >= "Fall 2008"),
																 lag(index_enr_inst) + enr_pct_change2, index_enr_inst)) %>%
	mutate(index_enr_inst = ifelse((UNITID == "159656" & year >= "Fall 2009"),
																 lag(index_enr_inst) + enr_pct_change2, index_enr_inst)) %>%
	mutate(index_enr_inst = ifelse((UNITID == "159656" & year >= "Fall 2010"),
																 lag(index_enr_inst) + enr_pct_change2, index_enr_inst)) %>%
	mutate(index_enr_inst = ifelse((UNITID == "159656" & year >= "Fall 2011"),
																 lag(index_enr_inst) + enr_pct_change2, index_enr_inst)) %>%
	mutate(index_enr_inst = ifelse((UNITID == "159656" & year >= "Fall 2012"),
																 lag(index_enr_inst) + enr_pct_change2, index_enr_inst)) %>%
	mutate(index_enr_inst = ifelse((UNITID == "159656" & year >= "Fall 2013"),
																 lag(index_enr_inst) + enr_pct_change2, index_enr_inst)) %>%
	mutate(index_enr_inst = ifelse((UNITID == "159656" & year >= "Fall 2014"),
																 lag(index_enr_inst) + enr_pct_change2, index_enr_inst)) %>%
	mutate(index_enr_inst = ifelse((UNITID == "159656" & year >= "Fall 2015"),
																 lag(index_enr_inst) + enr_pct_change2, index_enr_inst)) %>%
	mutate(index_enr_inst = ifelse((UNITID == "159656" & year >= "Fall 2016"),
																 lag(index_enr_inst) + enr_pct_change2, index_enr_inst)) %>%
	mutate(index_enr_inst = ifelse((UNITID == "159656" & year >= "Fall 2017"),
																 lag(index_enr_inst) + enr_pct_change2, index_enr_inst)) %>%
	mutate(index_enr_inst = ifelse((UNITID == "159656" & year >= "Fall 2018"),
																 lag(index_enr_inst) + enr_pct_change2, index_enr_inst)) %>%
	select(UNITID, inst_name, year, tot_enr, enr_change, enr_pct_change, enr_pct_change2, index_enr_inst)

enrollindex_jes %>%
	filter(year == "Fall 2014") %>%
	slice(which.max(index_enr_inst))
enrollindex_jes %>%
	filter(year == "Fall 2014") %>%
	filter(index_enr_inst > 1.5)

enrollindex_jes %>%
	filter(year == "Fall 2016") %>%
	slice(which.min(index_enr_inst))

enrollindex_jes %>%
	filter(UNITID == "159656" & year == "Fall 2011") %>%
	count(index_enr_inst)


ggplot(enrollindex_jes, aes(year, index_enr_inst, group = UNITID)) +
	geom_line(data = subset(enrollindex_jes, UNITID != "122612"), color = "grey") +
	geom_line(data = subset(enrollindex_jes, UNITID == "122612"), color = "#00543C", size = 1) +
	scale_y_continuous(limits = c(-.5, 2),
	 									 breaks = c(-.5, 0, .5, 1, 1.5, 2)) +
	labs(x = "", y = "") +
	theme_minimal() +
	theme(text = element_text(family = "Calibri"),
				panel.grid.major = element_blank(),	panel.grid.minor = element_blank(),
				axis.text.y = element_text(size = 14))

ggsave("figs/plot_jescollugenrindex.png", device = "png", dpi = 160,
			 width = 18.89, height = 10, units = "in")


# enrollindex_lac <-
# 	ipeds_fallenroll_8718 %>%
# 	mutate(lac = ifelse(UNITID == "122612", 1, lac)) %>%
# 	filter(lac == 1, level == "Undergraduate") %>%
# 	mutate(enr_pct_change2 = enr_pct_change / 100) %>%
# 	mutate(enr_pct_change2 = ifelse(year == "Fall 1987", 1, enr_pct_change2)) %>%
# 	arrange(UNITID, year) %>%
# 	group_by(UNITID) %>%
# 	mutate(index_enr_inst = 1) %>%
# 	mutate(index_enr_inst = ifelse(year >= "Fall 1988", cumsum(enr_pct_change2),
# 																 index_enr_inst)) %>%
# 	# mutate(index_enr_inst = ifelse(year >= "Fall 1989", lag(index_enr_inst) + enr_pct_change2,
# 	# 															 index_enr_inst)) %>%
# 	ungroup() %>%
# 	select(UNITID, inst_name, year, tot_enr, enr_change, enr_pct_change, enr_pct_change2, index_enr_inst)


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
