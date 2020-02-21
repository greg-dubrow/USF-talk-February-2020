library(tidyverse)
library(tidylog)
library(rCAEDDATA)
library(ipeds)
library()

load("~/Data/USF-talk-February-2020/data/graduates.rda")
glimpse(graduates)

graduates <- graduates %>%
	arrange(CDS_CODE, YEAR, ETHNIC, GENDER)

graduates %>%
	count(YEAR) %>%
	view()

## only goes to 1516 - need 1617 and 1718 by school, eth, gender grads, uc grads
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
