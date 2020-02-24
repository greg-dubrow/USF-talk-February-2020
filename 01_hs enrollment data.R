library(tidyverse)
library(tidylog)
library(rCAEDDATA)
library(rsfsu)
library(readr)
library(janitor)

# data files from https://www.cde.ca.gov/ds/sd/sd/

## from rCAEDDATA package (dl from github
 # only goes to 1516 - need 1617 and 1718 by school, eth, gender grads, uc grads

cahsgrad93to16 <- readRDS(file = "Data/cahsgrad93to16.rds") %>%
	arrange(CDS_CODE, YEAR, ETHNIC, GENDER) %>%
	mutate(ETHNIC = as.character(ETHNIC)) %>%
	arrange(CDS_CODE, YEAR, ETHNIC, GENDER)

glimpse(cahsgrad93to16)

# gradall1617.txt has same structure as gradutes
# graduates1718.xlsx is different, needs work to get to same structure
cahsgrad17 <- read_delim("data/gradall_1617.txt",
													 "\t", escape_double = FALSE, trim_ws = TRUE) %>%
	mutate(YEAR = factor(YEAR)) %>%
	mutate(ETHNIC = as.character(ETHNIC)) %>%
	mutate(GRADS = as.integer(GRADS)) %>%
	mutate(UC_GRADS = as.integer(UC_GRADS)) %>%
	arrange(CDS_CODE, YEAR, ETHNIC, GENDER)

glimpse(cahsgrad17)

cahsgrad93to17 <- rbind(cahsgrad93to16, cahsgrad17) %>%
	arrange(CDS_CODE, YEAR, ETHNIC, GENDER)
glimpse(cahsgrad93to17)

cahsgrad93to17 %>%
	count(YEAR) %>%
	view()

cahsgrad93to17_tot <- cahsgrad93to17 %>%
	group_by(YEAR) %>%
	summarise(total_grads = sum(GRADS),
						uccsu = sum(UC_GRADS),
						notuccsu = total_grads - uccsu) %>%
	ungroup()
glimpse(cahsgrad93to17_tot)

# file structure https://www.cde.ca.gov/ds/sd/sd/fsacgr.asp
# return a df with year, total grads and uc elig yes/no
# work desktop
cahsgrad18 <- read.delim("C:/Data/research projects/USF-talk-February-2020/data/cahsgrad18.txt",
												 stringsAsFactors=FALSE)
#mac laptop
cahsgrad18 <- read.delim("data/cahsgrad18.txt", stringsAsFactors=FALSE) %>%
	clean_names() %>%
	filter(reporting_category == "TA") %>%
	filter(aggregate_level == "S") %>%
	filter(dass == "All") %>%
	filter(charter_school == "All") %>%
	# filter(school_name != "Nonpublic, Nonsectarian Schools") %>%
	# filter(school_name != "District Office") %>%
	mutate(YEAR = "1718") %>%
	mutate(YEAR = factor(YEAR)) %>%
	mutate_at(vars(ends_with("_code")), as.character) %>%
	mutate(county_code = ifelse(nchar(county_code) == 1, str_pad(county_code, 2, "left", "0"), county_code)) %>%
	mutate(CDS_CODE = paste(county_code, district_code, school_code, sep = "")) %>%
	mutate(GRADS = as.integer(ifelse(regular_hs_diploma_graduates_count == "*",
																	 0, regular_hs_diploma_graduates_count))) %>%
	mutate(UC_GRADS = as.integer(ifelse(met_uc_csu_grad_req_s_count == "*",
																		 0, met_uc_csu_grad_req_s_count))) %>%
	select(CDS_CODE, GRADS, UC_GRADS, YEAR) %>%
	group_by(YEAR) %>%
	summarise(total_grads = sum(GRADS),
						uccsu = sum(UC_GRADS),
						notuccsu = total_grads - uccsu)

glimpse(cahsgrad18)

cahsgrad93to18_tot <- rbind(cahsgrad93to17_tot, cahsgrad18) %>%
	arrange(YEAR, total_grads, uccsu, notuccsu) %>%
	# amend 201718 from
	# https://dq.cde.ca.gov/dataquest/dqcensus/CohRateLevels.aspx?cds=00&agglevel=state&year=2017-18&initrow=&ro=y
	mutate(total_grads = ifelse(YEAR == "1718", 418205, total_grads)) %>%
	mutate(uccsu = ifelse(YEAR == "1718", 208769, uccsu)) %>%
	mutate(notuccsu = ifelse(YEAR == "1718", total_grads - uccsu, notuccsu)) %>%
	arrange(YEAR) %>%
	mutate(year_ch = as.character(YEAR)) %>%
	mutate(type = "Actual")

glimpse(cahsgrad93to18_tot)

# projected grads from dept finance - 2018-19 onward is projected delete 201718
grproj_to2028 <- readxl::read_excel("data/capublic_k12_enrollproj_to2028.xlsx",
																						 sheet = "hsgrads-tr") %>%
	mutate(year_ch = str_sub(year, 3, 7)) %>%
	mutate(year_ch = str_replace(year_ch, "-", "")) %>%
	filter(year_ch != "1718") %>%
	mutate(YEAR = factor(year_ch)) %>%
	mutate(uccsu = as.integer(NA)) %>%
	mutate(notuccsu = as.integer(NA)) %>%
	mutate(notuccsu = as.integer(NA)) %>%
	mutate(type = "Projected") %>%
	select(YEAR, total_grads = total, uccsu, notuccsu, year_ch, type) %>%
	# amend 2018-19 with actual results from
	# https://dq.cde.ca.gov/dataquest/dqcensus/CohRateLevels.aspx?cds=00&agglevel=state&year=2018-19
	mutate(total_grads = ifelse(YEAR == "1819", 417496, total_grads)) %>%
	mutate(uccsu = ifelse(year_ch == "1819", 210980, uccsu)) %>%
	mutate(notuccsu = ifelse(year_ch == "1819", total_grads - uccsu, notuccsu)) %>%
	mutate(type = ifelse(year_ch == "1819", "Actual", type))

glimpse(grproj_to2028)

# merge actual and projected, impute vals for projected
cahsgrads_1993_2028 <- rbind(cahsgrad93to18_tot, grproj_to2028) %>%
	mutate(pctucgrads = uccsu / total_grads) %>%
	arrange(YEAR) %>%
	# add projected uccsu grads based on constant 2017-18 to 2018-19 increase 0.0061437
	mutate(pctucgrads = ifelse(year_ch >= "1920", lag(pctucgrads) + 0.0061437, pctucgrads)) %>%
	mutate(pctucgrads = ifelse(year_ch == "2021", lag(pctucgrads) + 0.0061437, pctucgrads)) %>%
	mutate(pctucgrads = ifelse(year_ch == "2122", lag(pctucgrads) + 0.0061437, pctucgrads)) %>%
	mutate(pctucgrads = ifelse(year_ch == "2223", lag(pctucgrads) + 0.0061437, pctucgrads)) %>%
	mutate(pctucgrads = ifelse(year_ch == "2324", lag(pctucgrads) + 0.0061437, pctucgrads)) %>%
	mutate(pctucgrads = ifelse(year_ch == "2425", lag(pctucgrads) + 0.0061437, pctucgrads)) %>%
	mutate(pctucgrads = ifelse(year_ch == "2526", lag(pctucgrads) + 0.0061437, pctucgrads)) %>%
	mutate(pctucgrads = ifelse(year_ch == "2627", lag(pctucgrads) + 0.0061437, pctucgrads)) %>%
	mutate(pctucgrads = ifelse(year_ch == "2728", lag(pctucgrads) + 0.0061437, pctucgrads)) %>%
	mutate(pctucgrads = ifelse(year_ch == "2829", lag(pctucgrads) + 0.0061437, pctucgrads)) %>%
	mutate(uccsu = ifelse(type == "Projected", round(pctucgrads * total_grads, 0), uccsu)) %>%
	mutate(notuccsu = ifelse(type == "Projected", round(total_grads -uccsu, 0), notuccsu)) %>%
	#mutate(uccsu = round(uccsu, 0)) %>%
	mutate(gr_tot_change = (total_grads - lag(total_grads))) %>%
	mutate(gr_tot_pct_change = (total_grads/lag(total_grads)- 1)) %>%
	mutate(gr_uc_change = (uccsu - lag(uccsu))) %>%
	mutate(gr_uc_pct_change = (uccsu/lag(uccsu) - 1)) %>%
	mutate(gr_notuc_change = (notuccsu - lag(notuccsu))) %>%
	mutate(gr_notuc_pct_change = (notuccsu/lag(notuccsu) - 1)) %>%
	select(YEAR, total_grads, uccsu, notuccsu, type, pctucgrads, type, everything())

glimpse(cahsgrads_1993_2028)
saveRDS(cahsgrads_1993_2028, file = "data/cahsgrads_1993_2028.rds")

## charts

cahsgrads_1993_2028 %>%
	select(YEAR, uccsu, notuccsu) %>%
	pivot_longer(-YEAR, names_to = "ucelig", values_to = "n") %>%
	ggplot(aes(YEAR, n, fill = rev(ucelig))) +
	geom_bar(stat = "identity", color = "black") +
	#geom_vline(xintercept = "1920") +
	geom_vline(xintercept = 27.5) +
	scale_y_continuous(labels = scales::comma, limits = c(0, 550000)) +
	# scale_fill_discrete(labels = c("UC CSU Eligible", "Not UC/CSU Elig"),
	# 										(values = c("yellow", "lightblue"))) +
	scale_fill_manual(values = c("yellow", "lightblue"),
										labels = c("UC CSU Eligible", "Not UC/CSU Elig")) +
	labs(x = "Year", y = "Graduates",
			 fill = "UC/CSU Eligible?") +
	annotate("text", x = "1920", y = 500000, label = "Projected", hjust = -.25) +
	theme_minimal() +
	theme(legend.position = c(.1, .85))














# total by uc/csu elig & not
cahsgrad93to17 %>%
	group_by(YEAR) %>%
	summarise(total_grads = sum(GRADS),
						Yes = sum(UC_GRADS),
						No = total_grads - Yes) %>%
	select(-total_grads) %>%
	gather(Eligibility, Graduates, -YEAR) %>%
	ggplot(aes(YEAR, Graduates, fill = Eligibility)) +
	geom_bar(stat = "identity", color = "black") +
	labs(x = "Year",
			 y = "Graduates",
			 title = "California High School Graduates, 1992-2016",
			 fill = "UC Eligible?") +
	scale_y_continuous(labels = scales::comma) +
	scale_fill_manual(values = c("yellow", "lightblue")) +
	theme_minimal()
