## ipeds data for trend enrollment & other data

library(tidyverse)
library(tidylog)
library(ipeds)
library(educationdata)
library(janitor)
library(readr)

# ipeds package
ipeds::download_ipeds(year = 2016, dir = "/Users/gregdubrow/Data/USF-talk-February-2020/data/")
ipeds2017 <- ipeds::load_ipeds(year = 2017)
names(ipeds2017)

# education data package
ipeds17_racesex <- get_education_data(level = 'college-university',
												 source = 'ipeds',
												 topic = 'fall-enrollment',
												 by = list('race', 'sex'),
												 filters = list(year = 2017),
#												 							 grade = 9:12,
#												 							 ncessch = '340606000122'),
												 add_labels = TRUE)

glimpse(ipeds17)
saveRDS(ipeds17, "data/ipeds17_agesex.rds")
ipeds17_agesex <- readRDS("data/ipeds17_agesex.rds")

years <- c(2016, 2015)
ipeds16 <- get_education_data(level = 'college-university',
															source = 'ipeds',
															topic = 'fall-enrollment',
															by = list("race", "sex"),
															filters = list(year = 2016),
															add_labels = TRUE,
															csv = TRUE)

c("164924", "181002", "186432", "122931", "169716", "159656", "127918", "192323", "163046",
	"236595", "239105", "203368", "179159", "215770", "215929", "131496", "166124", "102234",
	"117946", "206622", "102234", "166124", "117946", "206622", "235316", "129242")



164924	Boston CollegeInfo	Chestnut Hill	MA
181002	Creighton University	Omaha	NE
186432	Saint Peter's University	Jersey City	NJ
	122931	Santa Clara University	Santa Clara	CA
	169716	University of Detroit Mercy	Detroit	MI
	159656	Loyola University New Orleans	New Orleans	LA
	127918	Regis University	Denver	CO
	192323	Le Moyne College	Syracuse	NY
	163046	Loyola University Maryland	Baltimore	MD
	236595	Seattle University	Seattle	WA
	239105	Marquette University	Milwaukee	WI
	203368	John Carroll University	University Heights	OH
	179159	Saint Louis University	Saint Louis	MO
	215770	Saint Joseph's University	Philadelphia	PA
215929	University of Scranton	Scranton	PA
131496	Georgetown University	Washington	DC
166124	College of the Holy Cross	Worcester	MA
102234	Spring Hill College	Mobile	AL
117946	Loyola Marymount University	Los Angeles	CA
206622	Xavier University	Cincinnati	OH
102234	Spring Hill College	Mobile	AL
166124	College of the Holy Cross	Worcester	MA
117946	Loyola Marymount University	Los Angeles	CA
206622	Xavier University	Cincinnati	OH
235316	Gonzaga University	Spokane	WA
129242	Fairfield University	Fairfield	CT
