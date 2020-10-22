library(readr)
library(tidyverse)
library(lubridate)
library(tsibble)

JSW_intraday <- read_delim("JSW_intraday.csv", 
                           ";", escape_double = FALSE, col_types = cols(`<DATE>` = col_character(), 
                                                                        `<TIME>` = col_character()), trim_ws = TRUE)
str(JSW_intraday)
#View(JSW_intraday)

JSW_intraday$`<TIME>` <- gsub("^[9]", "09", JSW_intraday$`<TIME>`)

JSW_intraday$DATETIME <- paste(JSW_intraday$`<DATE>`, JSW_intraday$`<TIME>`)
JSW_intraday$DATETIME <- ymd_hms(JSW_intraday$DATETIME)

JSW_intraday <- JSW_intraday %>%
  select(-c(`<TIME>`, `<DATE>`))

JSW_intraday <- JSW_intraday %>%
  mutate(errors = ifelse((DATETIME-lag(DATETIME)) != 5 & (DATETIME-lag(DATETIME) < 55),1,0))

daty <- seq(ymd_hms("2020-08-27 09:00:00"), ymd_hms("2020-10-16 16:45:00"), by='5 min')
daty <- daty[-which(hour(daty) > 16 | hour(daty) < 9)]
daty <- daty[-which(hour(daty)==16 & minute(daty) > 45)]
daty <- daty[-which(weekdays(daty) == 'sobota' | weekdays(daty) == 'niedziela')]
daty <- as.data.frame(daty)
colnames(daty) <- "DATETIME"

JSW_intraday <- right_join(JSW_intraday, daty)
JSW_intraday <- JSW_intraday[order(JSW_intraday$DATETIME),]

JSW_intraday <- JSW_intraday %>%
  select(DATETIME, `<OPEN>`, `<CLOSE>`, `<VOL>`) %>%
  fill(c(`<OPEN>`, `<CLOSE>`, `<VOL>`), .direction = 'down') %>%
  mutate(stopy = log(`<CLOSE>`/`<OPEN>`), cena = `<CLOSE>`*`<VOL>`, vol_ln = log(`<VOL>`), cena_ln = log(cena))

head(JSW_intraday)

JSW_intraday %>% 
  group_by(hour(DATETIME), minute(DATETIME)) %>% 
  select(cena) %>% 
  summarise(mean = mean(cena))

#TO RACZEJ ZLE WYKRESY
JSW_intraday %>%
  ggplot(aes(x=DATETIME, y=stopy)) + geom_point()

JSW_intraday %>%
  ggplot(aes(x=DATETIME, y=vol_ln)) + geom_point()

JSW_intraday %>%
  ggplot(aes(x=DATETIME, y=cena_ln)) + geom_point()

######===============STREFA_TESTOWA===============#####
#TO CHYBA NAJLADNIEJSZA OPCJA
JSW_intraday %>% 
  group_by(hour(DATETIME), minute(DATETIME)) %>% 
  select(stopy) %>% 
  summarise(mean = mean(stopy)) %>%
  mutate(czas = paste(`hour(DATETIME)`, ':', `minute(DATETIME)`)) %>%
  mutate(czas = hm(czas)) %>%
  select(czas, mean) %>%
  ggplot(aes(x=czas, y=mean)) +
  geom_point() +
  scale_x_time()

JSW_intraday %>% 
  group_by(hour(DATETIME), minute(DATETIME)) %>% 
  select(cena) %>% 
  summarise(mean = mean(cena)) %>%
  mutate(czas = paste(`hour(DATETIME)`, ':', `minute(DATETIME)`)) %>%
  mutate(czas = hm(czas)) %>%
  select(czas, mean) %>%
  ggplot(aes(x=czas, y=mean)) +
  geom_point() +
  scale_x_time()

JSW_intraday %>% 
  group_by(hour(DATETIME), minute(DATETIME)) %>% 
  select(vol_ln) %>% 
  summarise(mean = mean(vol_ln)) %>%
  mutate(czas = paste(`hour(DATETIME)`, ':', `minute(DATETIME)`)) %>%
  mutate(czas = hm(czas)) %>%
  select(czas, mean) %>%
  ggplot(aes(x=czas, y=mean)) +
  geom_point() +
  scale_x_time()

JSW_intraday %>% 
  group_by(hour(DATETIME), minute(DATETIME)) %>% 
  select(cena_ln) %>% 
  summarise(mean = mean(cena_ln)) %>%
  mutate(czas = paste(`hour(DATETIME)`, ':', `minute(DATETIME)`)) %>%
  mutate(czas = hm(czas)) %>%
  select(czas, mean) %>%
  ggplot(aes(x=czas, y=mean)) +
  geom_point() +
  scale_x_time()

#PONIZEJ BRZYDKIE
srednie <- c()
for(i in seq(9, 16, by = 1)){
  for(j in seq(0, 55, by = 5)){
    srednie <- c(srednie, mean(JSW_intraday$stopy[which(minute(JSW_intraday$`<DATETIME>`) == j & hour(JSW_intraday$`<DATETIME>`) == i)]))
  }
}

test <- data.frame(rep(9:16, each=12), rep(seq(0, 55, by = 5), 8))
test <- test[-c(95,96),c(1,2)]
test <- data.frame(test, srednie[-c(95,96)])
colnames(test) <- c('czas', 'm', 'srednie')

test$czas <- paste(test$czas, ':', test$m)
test <- test %>% select(-m) %>% mutate(czas = hm(czas))

test %>% ggplot(aes(x=czas, y=srednie)) + geom_line() + scale_x_time()
