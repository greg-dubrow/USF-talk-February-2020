library(tidyverse)
library(tidylog)
library(rCAEDDATA)
library(ipeds)
library(rsfsu)
library(readr)



## only goes to 1516 - need 1617 and 1718 by school, eth, gender grads, uc grads

# on home mac
load("~/Data/USF-talk-February-2020/data/cahsgrad93to16.rda")
# on work desktop
load("C:/Data/research projects/USF-talk-February-2020/data/cahsgrad93to16.rda")
glimpse(graduates)

graduates <- graduates %>%
	arrange(CDS_CODE, YEAR, ETHNIC, GENDER)

graduates %>%
	count(YEAR) %>%
	view()


# gradall1617.txt has same structure as gradutes
# graduates1718.xlsx is different, needs work to get to same structure
gradall_1617 <- read_delim("data/gradall_1617.txt",
													 "\t", escape_double = FALSE, trim_ws = TRUE)
View(gradall_1617)
glimpse(gradall_1617)











graduates %>%
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
