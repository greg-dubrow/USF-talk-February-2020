library(tidyverse)
library(tidylog)
library(rCAEDDATA)
library(ipeds)
library(rsfsu)
library(readr)
library(janitor)

# data files from https://www.cde.ca.gov/ds/sd/sd/

## from rCAEDDATA package (dl from github
 # only goes to 1516 - need 1617 and 1718 by school, eth, gender grads, uc grads

cahsgrad93to16 <- readRDS(file = "Data/cahsgrad93to16.rds") %>%
	arrange(CDS_CODE, YEAR, ETHNIC, GENDER) %>%
	mutate(ETHNIC = as.character(ETHNIC))

glimpse(cahsgrad93to16)

cahsgrad93to16 <- cahsgrad93to16 %>%
	arrange(CDS_CODE, YEAR, ETHNIC, GENDER)

cahsgrad93to16  %>%
	count(YEAR) %>%
	view()


# gradall1617.txt has same structure as gradutes
# graduates1718.xlsx is different, needs work to get to same structure
cahsgrad17 <- read_delim("data/gradall_1617.txt",
													 "\t", escape_double = FALSE, trim_ws = TRUE) %>%
	mutate(YEAR = factor(YEAR)) %>%
	mutate(ETHNIC = as.character(ETHNIC)) %>%
	mutate(GRADS = as.integer(GRADS)) %>%
	mutate(UC_GRADS = as.integer(UC_GRADS))

glimpse(cahsgrad17)

cahsgrad17 %>%
	count(YEAR)

cahsgrad93to17 <- rbind(cahsgrad93to16, cahsgrad17) %>%
	arrange(CDS_CODE, YEAR, ETHNIC, GENDER)
glimpse(cahsgrad93to17)

cahsgrad93to17 %>%
	count(YEAR) %>%
	view()

cahsgrad93to17_tot <- cahsgrad93to17 %>%
	group_by(YEAR) %>%
	summarise(total_grads = sum(GRADS),
						Yes = sum(UC_GRADS),
						No = total_grads - Yes) %>%
	ungroup()
glimpse(cahsgrad93to17_tot)

# file structure https://www.cde.ca.gov/ds/sd/sd/fsacgr.asp
# return a df with year, total grads and uc elig yes/no
cahsgrad18 <- read.delim("C:/Data/research projects/USF-talk-February-2020/data/cahsgrad18.txt",
												 stringsAsFactors=FALSE) %>%
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
							Yes = sum(UC_GRADS),
							No = total_grads - Yes)

glimpse(cahsgrad18)

cahsgrad93to18_tot <- rbind(cahsgrad93to17_tot, cahsgrad18) %>%
	arrange(YEAR, total_grads, Yes, No) %>%
	mutate(total_grads = ifelse(YEAR == "1718", 418205, total_grads)) %>%
	mutate(pctucgrads = Yes / total_grads)

glimpse(cahsgrad93to18_tot)





## charts
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

ucgrads <- get_school_grad_data('2016-17', 'UCGradEth')
