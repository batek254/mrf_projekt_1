library(readr)
library(tidyverse)
library(lubridate)
library(tsibble)
library(tseries)

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

#JSW_intraday <- JSW_intraday %>%
#  mutate(errors = ifelse((DATETIME-lag(DATETIME)) != 5 & (DATETIME-lag(DATETIME) < 55),1,0))

daty <- seq(ymd_hms("2020-09-04 09:00:00"), ymd_hms("2020-10-16 16:45:00"), by='5 min')
daty <- daty[-which(hour(daty) > 16 | hour(daty) < 9)]
daty <- daty[-which(hour(daty) == 16 & minute(daty) > 45)]
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

#JSW_intraday %>% 
#  group_by(hour(DATETIME), minute(DATETIME)) %>% 
#  select(cena) %>% 
#  summarise(mean = mean(cena))

#TO RACZEJ ZLE WYKRESY
#JSW_intraday %>%
#  ggplot(aes(x=DATETIME, y=stopy)) + geom_point()
#
#JSW_intraday %>%
#  ggplot(aes(x=DATETIME, y=vol_ln)) + geom_point()
#
#JSW_intraday %>%
#  ggplot(aes(x=DATETIME, y=cena_ln)) + geom_point()

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
#srednie <- c()
#for(i in seq(9, 16, by = 1)){
#  for(j in seq(0, 55, by = 5)){
#    srednie <- c(srednie, mean(JSW_intraday$stopy[which(minute(JSW_intraday$`<DATETIME>`) == j & hour(JSW_intraday$`<DATETIME>`) == i)]))
#  }
#}
#
#test <- data.frame(rep(9:16, each=12), rep(seq(0, 55, by = 5), 8))
#test <- test[-c(95,96),c(1,2)]
#test <- data.frame(test, srednie[-c(95,96)])
#colnames(test) <- c('czas', 'm', 'srednie')
#
#test$czas <- paste(test$czas, ':', test$m)
#test <- test %>% select(-m) %>% mutate(czas = hm(czas))
#
#test %>% ggplot(aes(x=czas, y=srednie)) + geom_line() + scale_x_time()

#ZAD2
JSW_intraday %>% 
  group_by(hour(DATETIME), minute(DATETIME)) %>% 
  select(stopy) %>%
  mutate(stopy = abs(stopy)) %>%
  summarise(mean = mean(stopy)) %>%
  mutate(czas = paste(`hour(DATETIME)`, ':', `minute(DATETIME)`)) %>%
  mutate(czas = hm(czas)) %>%
  select(czas, mean) %>%
  ggplot(aes(x=czas, y=mean)) +
  geom_line() +
  scale_x_time()

JSW_intraday %>%
  mutate(stopy = abs(stopy)) %>%
  select(stopy) %>%
  acf(lag.max = 500)

JSW_intraday %>%
  mutate(stopy = abs(stopy)) %>%
  select(stopy) %>%
  Box.test(lag =500, type=c("Ljung-Box"))

#ZAD3
JSW_intraday %>% 
  group_by(hour(DATETIME), minute(DATETIME)) %>%
  mutate(stopy = abs(stopy), new = stopy/mean(stopy)) %>%
  ungroup() %>%
  select(new) %>%
  acf(lag.max = 500)

JSW_intraday %>% 
  group_by(hour(DATETIME), minute(DATETIME)) %>%
  mutate(stopy = abs(stopy), new = stopy/mean(stopy)) %>%
  ungroup() %>%
  select(new) %>%
  Box.test(lag =500, type=c("Ljung-Box"))

#ZAD4
JSW_intraday %>%
  select(vol_ln) %>%
  acf(lag.max = 500)

JSW_intraday %>%
  select(vol_ln) %>%
  Box.test(lag =500, type=c("Ljung-Box"))

#=======================================================================================================================
#ZAD5
CIE_intraday <- read_delim("cie.txt", 
                           ",", escape_double = FALSE, col_types = cols(`<DATE>` = col_character(), 
                                                                        `<TIME>` = col_character()), trim_ws = TRUE)


str(CIE_intraday)
#View(CIE_intraday)

CIE_intraday$DATETIME <- paste(CIE_intraday$`<DATE>`, CIE_intraday$`<TIME>`)
CIE_intraday$DATETIME <- ymd_hms(CIE_intraday$DATETIME)

CIE_intraday <- CIE_intraday %>%
  select(-c(`<TIME>`, `<DATE>`))

#CIE_intraday <- CIE_intraday %>%
#  mutate(errors = ifelse((DATETIME-lag(DATETIME)) != 5 & (DATETIME-lag(DATETIME) < 55),1,0))

daty <- seq(ymd_hms("2020-09-04 09:00:00"), ymd_hms("2020-10-16 16:45:00"), by='5 min')
daty <- daty[-which(hour(daty) > 16 | hour(daty) < 9)]
daty <- daty[-which(hour(daty) == 16 & minute(daty) > 45)]
daty <- daty[-which(weekdays(daty) == 'sobota' | weekdays(daty) == 'niedziela')]
daty <- as.data.frame(daty)
colnames(daty) <- "DATETIME"

CIE_intraday <- right_join(CIE_intraday, daty)
CIE_intraday <- CIE_intraday[order(CIE_intraday$DATETIME),]

CIE_intraday <- CIE_intraday %>%
  select(DATETIME, `<OPEN>`, `<CLOSE>`, `<VOL>`) %>%
  fill(c(`<OPEN>`, `<CLOSE>`, `<VOL>`), .direction = 'down') %>%
  mutate(stopy = log(`<CLOSE>`/`<OPEN>`), cena = `<CLOSE>`*`<VOL>`, vol_ln = log(`<VOL>`), cena_ln = log(cena))

head(CIE_intraday)

#CIE_intraday %>% 
#  group_by(hour(DATETIME), minute(DATETIME)) %>% 
#  select(cena) %>% 
#  summarise(mean = mean(cena))

#TO RACZEJ ZLE WYKRESY
#CIE_intraday %>%
#  ggplot(aes(x=DATETIME, y=stopy)) + geom_point()
#
#CIE_intraday %>%
#  ggplot(aes(x=DATETIME, y=vol_ln)) + geom_point()
#
#CIE_intraday %>%
#  ggplot(aes(x=DATETIME, y=cena_ln)) + geom_point()

######===============STREFA_TESTOWA===============#####
#TO CHYBA NAJLADNIEJSZA OPCJA
CIE_intraday %>% 
  group_by(hour(DATETIME), minute(DATETIME)) %>% 
  select(stopy) %>% 
  summarise(mean = mean(stopy)) %>%
  mutate(czas = paste(`hour(DATETIME)`, ':', `minute(DATETIME)`)) %>%
  mutate(czas = hm(czas)) %>%
  select(czas, mean) %>%
  ggplot(aes(x=czas, y=mean)) +
  geom_point() +
  scale_x_time()

CIE_intraday %>% 
  group_by(hour(DATETIME), minute(DATETIME)) %>% 
  select(cena) %>% 
  summarise(mean = mean(cena)) %>%
  mutate(czas = paste(`hour(DATETIME)`, ':', `minute(DATETIME)`)) %>%
  mutate(czas = hm(czas)) %>%
  select(czas, mean) %>%
  ggplot(aes(x=czas, y=mean)) +
  geom_point() +
  scale_x_time()

CIE_intraday %>% 
  group_by(hour(DATETIME), minute(DATETIME)) %>% 
  select(vol_ln) %>% 
  summarise(mean = mean(vol_ln)) %>%
  mutate(czas = paste(`hour(DATETIME)`, ':', `minute(DATETIME)`)) %>%
  mutate(czas = hm(czas)) %>%
  select(czas, mean) %>%
  ggplot(aes(x=czas, y=mean)) +
  geom_point() +
  scale_x_time()

CIE_intraday %>% 
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
#srednie <- c()
#for(i in seq(9, 16, by = 1)){
#  for(j in seq(0, 55, by = 5)){
#    srednie <- c(srednie, mean(CIE_intraday$stopy[which(minute(CIE_intraday$`<DATETIME>`) == j & hour(CIE_intraday$`<DATETIME>`) == i)]))
#  }
#}
#
#test <- data.frame(rep(9:16, each=12), rep(seq(0, 55, by = 5), 8))
#test <- test[-c(95,96),c(1,2)]
#test <- data.frame(test, srednie[-c(95,96)])
#colnames(test) <- c('czas', 'm', 'srednie')
#
#test$czas <- paste(test$czas, ':', test$m)
#test <- test %>% select(-m) %>% mutate(czas = hm(czas))
#
#test %>% ggplot(aes(x=czas, y=srednie)) + geom_line() + scale_x_time()

#ZAD2
CIE_intraday %>% 
  group_by(hour(DATETIME), minute(DATETIME)) %>% 
  select(stopy) %>%
  mutate(stopy = abs(stopy)) %>%
  summarise(mean = mean(stopy)) %>%
  mutate(czas = paste(`hour(DATETIME)`, ':', `minute(DATETIME)`)) %>%
  mutate(czas = hm(czas)) %>%
  select(czas, mean) %>%
  ggplot(aes(x=czas, y=mean)) +
  geom_line() +
  scale_x_time()

CIE_intraday %>%
  mutate(stopy = abs(stopy)) %>%
  select(stopy) %>%
  acf(lag.max = 500)

CIE_intraday %>%
  mutate(stopy = abs(stopy)) %>%
  select(stopy) %>%
  Box.test(lag =500, type=c("Ljung-Box"))

#ZAD3
CIE_intraday %>% 
  group_by(hour(DATETIME), minute(DATETIME)) %>%
  mutate(stopy = abs(stopy), new = stopy/mean(stopy)) %>%
  ungroup() %>%
  select(new) %>%
  acf(lag.max = 500)

CIE_intraday %>% 
  group_by(hour(DATETIME), minute(DATETIME)) %>%
  mutate(stopy = abs(stopy), new = stopy/mean(stopy)) %>%
  ungroup() %>%
  select(new) %>%
  Box.test(lag =500, type=c("Ljung-Box"))

#ZAD4
CIE_intraday %>%
  select(vol_ln) %>%
  acf(lag.max = 500)

CIE_intraday %>%
  select(vol_ln) %>%
  Box.test(lag =500, type=c("Ljung-Box"))


#ZAD6
#CIE10
daty <- seq(ymd_hms("2020-09-04 09:00:00"), ymd_hms("2020-10-16 16:45:00"), by='10 min')
daty <- daty[-which(hour(daty) > 16 | hour(daty) < 9)]
daty <- daty[-which(hour(daty) == 16 & minute(daty) > 45)]
daty <- daty[-which(weekdays(daty) == 'sobota' | weekdays(daty) == 'niedziela')]
daty <- as.data.frame(daty)
colnames(daty) <- "DATETIME"

CIE_intraday_10 <- right_join(CIE_intraday, daty)
CIE_intraday_10 <- CIE_intraday_10[order(CIE_intraday_10$DATETIME),]

CIE_intraday_10 <- CIE_intraday_10 %>%
  select(DATETIME, `<OPEN>`, `<CLOSE>`, `<VOL>`) %>%
  fill(c(`<OPEN>`, `<CLOSE>`, `<VOL>`), .direction = 'down') %>%
  mutate(stopy = log(`<CLOSE>`/`<OPEN>`), cena = `<CLOSE>`*`<VOL>`, vol_ln = log(`<VOL>`), cena_ln = log(cena))

head(CIE_intraday_10)

#=======================TEST==============================
CIE_intraday %>%
  filter(minute(DATETIME) %in% c(0,10,20,30,40,50)) %>%
  mutate(stopy = log(`<CLOSE>`/`<OPEN>`))
#=======================KONIEC============================

JSW_intraday_10 <- right_join(JSW_intraday, daty)
JSW_intraday_10 <- JSW_intraday_10[order(JSW_intraday_10$DATETIME),]

JSW_intraday_10 <- JSW_intraday_10 %>%
  select(DATETIME, `<OPEN>`, `<CLOSE>`, `<VOL>`) %>%
  fill(c(`<OPEN>`, `<CLOSE>`, `<VOL>`), .direction = 'down') %>%
  mutate(stopy = log(`<CLOSE>`/`<OPEN>`), cena = `<CLOSE>`*`<VOL>`, vol_ln = log(`<VOL>`), cena_ln = log(cena))

head(JSW_intraday_10)

#CIE15
daty <- seq(ymd_hms("2020-09-04 09:00:00"), ymd_hms("2020-10-16 16:45:00"), by='15 min')
daty <- daty[-which(hour(daty) > 16 | hour(daty) < 9)]
daty <- daty[-which(weekdays(daty) == 'sobota' | weekdays(daty) == 'niedziela')]
daty <- as.data.frame(daty)
colnames(daty) <- "DATETIME"

CIE_intraday_15 <- right_join(CIE_intraday, daty)
CIE_intraday_15 <- CIE_intraday_15[order(CIE_intraday_15$DATETIME),]

CIE_intraday_15 <- CIE_intraday_15 %>%
  select(DATETIME, `<OPEN>`, `<CLOSE>`, `<VOL>`) %>%
  fill(c(`<OPEN>`, `<CLOSE>`, `<VOL>`), .direction = 'down') %>%
  mutate(stopy = log(`<CLOSE>`/`<OPEN>`), cena = `<CLOSE>`*`<VOL>`, vol_ln = log(`<VOL>`), cena_ln = log(cena))

head(CIE_intraday_15)

JSW_intraday_15 <- right_join(JSW_intraday, daty)
JSW_intraday_15 <- JSW_intraday_15[order(JSW_intraday_15$DATETIME),]

JSW_intraday_15 <- JSW_intraday_15 %>%
  select(DATETIME, `<OPEN>`, `<CLOSE>`, `<VOL>`) %>%
  fill(c(`<OPEN>`, `<CLOSE>`, `<VOL>`), .direction = 'down') %>%
  mutate(stopy = log(`<CLOSE>`/`<OPEN>`), cena = `<CLOSE>`*`<VOL>`, vol_ln = log(`<VOL>`), cena_ln = log(cena))

head(JSW_intraday_15)

#CIE20
daty <- seq(ymd_hms("2020-09-04 09:00:00"), ymd_hms("2020-10-16 16:45:00"), by='20 min')
daty <- daty[-which(hour(daty) > 16 | hour(daty) < 9)]
daty <- daty[-which(weekdays(daty) == 'sobota' | weekdays(daty) == 'niedziela')]
daty <- as.data.frame(daty)
colnames(daty) <- "DATETIME"

CIE_intraday_20 <- right_join(CIE_intraday, daty)
CIE_intraday_20 <- CIE_intraday_20[order(CIE_intraday_20$DATETIME),]

CIE_intraday_20 <- CIE_intraday_20 %>%
  select(DATETIME, `<OPEN>`, `<CLOSE>`, `<VOL>`) %>%
  fill(c(`<OPEN>`, `<CLOSE>`, `<VOL>`), .direction = 'down') %>%
  mutate(stopy = log(`<CLOSE>`/`<OPEN>`), cena = `<CLOSE>`*`<VOL>`, vol_ln = log(`<VOL>`), cena_ln = log(cena))

head(CIE_intraday_20)

JSW_intraday_20 <- right_join(JSW_intraday, daty)
JSW_intraday_20 <- JSW_intraday_20[order(JSW_intraday_20$DATETIME),]

JSW_intraday_20 <- JSW_intraday_20 %>%
  select(DATETIME, `<OPEN>`, `<CLOSE>`, `<VOL>`) %>%
  fill(c(`<OPEN>`, `<CLOSE>`, `<VOL>`), .direction = 'down') %>%
  mutate(stopy = log(`<CLOSE>`/`<OPEN>`), cena = `<CLOSE>`*`<VOL>`, vol_ln = log(`<VOL>`), cena_ln = log(cena))

head(JSW_intraday_20)

#CIE30
daty <- seq(ymd_hms("2020-09-04 09:00:00"), ymd_hms("2020-10-16 16:45:00"), by='30 min')
daty <- daty[-which(hour(daty) > 16 | hour(daty) < 9)]
daty <- daty[-which(weekdays(daty) == 'sobota' | weekdays(daty) == 'niedziela')]
daty <- as.data.frame(daty)
colnames(daty) <- "DATETIME"

CIE_intraday_30 <- right_join(CIE_intraday, daty)
CIE_intraday_30 <- CIE_intraday_30[order(CIE_intraday_30$DATETIME),]

CIE_intraday_30 <- CIE_intraday_30 %>%
  select(DATETIME, `<OPEN>`, `<CLOSE>`, `<VOL>`) %>%
  fill(c(`<OPEN>`, `<CLOSE>`, `<VOL>`), .direction = 'down') %>%
  mutate(stopy = log(`<CLOSE>`/`<OPEN>`), cena = `<CLOSE>`*`<VOL>`, vol_ln = log(`<VOL>`), cena_ln = log(cena))

head(CIE_intraday_30)

JSW_intraday_30 <- right_join(JSW_intraday, daty)
JSW_intraday_30 <- JSW_intraday_30[order(JSW_intraday_30$DATETIME),]

JSW_intraday_30 <- JSW_intraday_30 %>%
  select(DATETIME, `<OPEN>`, `<CLOSE>`, `<VOL>`) %>%
  fill(c(`<OPEN>`, `<CLOSE>`, `<VOL>`), .direction = 'down') %>%
  mutate(stopy = log(`<CLOSE>`/`<OPEN>`), cena = `<CLOSE>`*`<VOL>`, vol_ln = log(`<VOL>`), cena_ln = log(cena))

head(JSW_intraday_30)

#KORELACJE
cor(CIE_intraday$stopy, JSW_intraday$stopy)
cor(CIE_intraday_10$stopy, JSW_intraday_10$stopy)
cor(CIE_intraday_15$stopy, JSW_intraday_15$stopy)
cor(CIE_intraday_20$stopy, JSW_intraday_20$stopy)
cor(CIE_intraday_30$stopy, JSW_intraday_30$stopy)

