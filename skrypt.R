library(readr)
library(tidyverse)
library(lubridate)

JSW_intraday <- read_delim("JSW_intraday.csv", 
                           ";", escape_double = FALSE, col_types = cols(`<DATE>` = col_character(), 
                                                                        `<TIME>` = col_character()), trim_ws = TRUE)
str(JSW_intraday)
#View(JSW_intraday)

JSW_intraday$`<TIME>` <- gsub("^[9]", "09", JSW_intraday$`<TIME>`)

JSW_intraday$`<DATETIME>` <- paste(JSW_intraday$`<DATE>`, JSW_intraday$`<TIME>`)
JSW_intraday$`<DATETIME>` <- ymd_hms(JSW_intraday$`<DATETIME>`)

JSW_intraday <- JSW_intraday %>%
  select(-c(`<TIME>`, `<DATE>`))

JSW_intraday <- JSW_intraday %>%
  mutate(errors = ifelse((`<DATETIME>`-lag(`<DATETIME>`)) != 5 & (`<DATETIME>`-lag(`<DATETIME>`) < 55),1,0))

daty <- seq(ymd_hms("2020-08-27 09:00:00"), ymd_hms("2020-10-16 16:45:00"), by='5 min')
daty <- daty[-which(hour(daty) > 16 | hour(daty) < 9)]
daty <- daty[-which(hour(daty)==16 & minute(daty) > 45)]
daty <- daty[-which(weekdays(daty) == 'sobota' | weekdays(daty) == 'niedziela')]
daty <- as.data.frame(daty)
colnames(daty) <- "<DATETIME>"

JSW_intraday <- right_join(JSW_intraday, daty)
JSW_intraday <- JSW_intraday[order(JSW_intraday$`<DATETIME>`),]

JSW_intraday <- JSW_intraday %>%
  select(`<DATETIME>`, `<OPEN>`, `<CLOSE>`, `<VOL>`) %>%
  fill(c(`<OPEN>`, `<CLOSE>`, `<VOL>`), .direction = 'down') %>%
  mutate(stopy = log(`<CLOSE>`/`<OPEN>`))

head(JSW_intraday)


