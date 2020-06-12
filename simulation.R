library(tidyverse)
library(lubridate)
library(caret)
library(plotly)

#read in data
raw_data <- read_csv2("Covid.csv")

#transform from wide to long format
cases <- raw_data %>% 
  pivot_longer(-Region, names_to="Date", values_to="Cases")

#parse date strings to date objects
cases$Date <- dmy(cases$Date)

#make regions factors
cases$Region <- as.factor(cases$Region)

#add column with cumulative cases
cases <- cases %>% 
  group_by(Region) %>% 
  mutate(SumCases = cumsum(Cases))

#add column with days since 100 cases
cases <- cases %>% 
  group_by(Region) %>% 
  mutate(DaysSince100 = if_else(SumCases>=100, 1, 0)) %>% 
  mutate(DaysSince100 = cumsum(DaysSince100))

#add column with squared days
cases <- cases %>% 
  mutate(DaysSince100.2 = DaysSince100^2) %>% 
  mutate(DaysSince100.3 = DaysSince100^3)



#add social distancing measures
#initialize column
cases$SocialDistancing <- rep(0, nrow(cases))

#Baden-Württemberg (mittel: 23.03.-10.05., leicht: 11.05.-)
cases <- cases %>% 
  mutate(SocialDistancing = if_else((Region=="Baden-Württemberg") & (Date>=dmy("23.03.2020")) & (Date<=dmy("10.05.2020")), 
                                   2, 
                                   if_else((Region=="Baden-Württemberg") & (Date>=dmy("11.05.2020")),
                                           1,
                                           SocialDistancing)))


#Bayern (stark: 21.03.-05.05., mittel: 06.05.-07.05., leicht: 08.05.-)
cases <- cases %>% 
  mutate(SocialDistancing = if_else((Region=="Bayern") & (Date>=dmy("21.03.2020")) & (Date<=dmy("05.05.2020")), 
                                    3, 
                                    if_else((Region=="Bayern") & (Date>=dmy("06.05.2020")) & (Date<=dmy("07.05.2020")),
                                            2,
                                            if_else((Region=="Bayern") & (Date>=dmy("08.05.2020")),
                                                    1,
                                                    SocialDistancing))))


#Berlin (mittel: 23.03.-29.05., leicht: 30.05.-)
cases <- cases %>% 
  mutate(SocialDistancing = if_else((Region=="Berlin") & (Date>=dmy("23.03.2020")) & (Date<=dmy("29.05.2020")), 
                                    2, 
                                    if_else((Region=="Berlin") & (Date>=dmy("30.05.2020")),
                                            1,
                                            SocialDistancing)))


#Brandenburg (mittel: 23.03.-08.05, leicht: 09.05.-)
cases <- cases %>% 
  mutate(SocialDistancing = if_else((Region=="Brandenburg") & (Date>=dmy("23.03.2020")) & (Date<=dmy("08.05.2020")), 
                                    2, 
                                    if_else((Region=="Brandenburg") & (Date>=dmy("09.05.2020")),
                                            1,
                                            SocialDistancing)))


#Bremen (mittel: 23.03.-06.05. , leicht: 07.05.-)
cases <- cases %>% 
  mutate(SocialDistancing = if_else((Region=="Bremen") & (Date>=dmy("23.03.2020")) & (Date<=dmy("06.05.2020")), 
                                    2, 
                                    if_else((Region=="Bremen") & (Date>=dmy("07.05.2020")),
                                            1,
                                            SocialDistancing)))


#Hamburg (mittel: 22.03.-12.05., leicht:13.05.-)
cases <- cases %>% 
  mutate(SocialDistancing = if_else((Region=="Hamburg") & (Date>=dmy("22.03.2020")) & (Date<=dmy("12.05.2020")), 
                                    2, 
                                    if_else((Region=="Hamburg") & (Date>=dmy("13.05.2020")),
                                            1,
                                            SocialDistancing)))


#Hessen (mittel: 22.03.-06.05., leicht:07.05.-)
cases <- cases %>% 
  mutate(SocialDistancing = if_else((Region=="Hessen") & (Date>=dmy("22.03.2020")) & (Date<=dmy("06.05.2020")), 
                                    2, 
                                    if_else((Region=="Hessen") & (Date>=dmy("07.05.2020")),
                                            1,
                                            SocialDistancing)))


#Mecklenburg-Vorpommern (mittel: 23.03.-10.05., leicht:11.05.-)
cases <- cases %>% 
  mutate(SocialDistancing = if_else((Region=="Mecklenburg-Vorpommern") & (Date>=dmy("23.03.2020")) & (Date<=dmy("10.05.2020")), 
                                    2, 
                                    if_else((Region=="Mecklenburg-Vorpommern") & (Date>=dmy("11.05.2020")),
                                            1,
                                            SocialDistancing)))


#Niedersachsen (mittel: 23.03.-10.05., leicht:11.05.-)
cases <- cases %>% 
  mutate(SocialDistancing = if_else((Region=="Niedersachsen") & (Date>=dmy("23.03.2020")) & (Date<=dmy("10.05.2020")), 
                                    2, 
                                    if_else((Region=="Niedersachsen") & (Date>=dmy("11.05.2020")),
                                            1,
                                            SocialDistancing)))


#Nordrhein-Westfalen (mittel: 23.03.-10.05., leicht:11.05.-)
cases <- cases %>% 
  mutate(SocialDistancing = if_else((Region=="Nordrhein-Westfalen") & (Date>=dmy("23.03.2020")) & (Date<=dmy("10.05.2020")), 
                                    2, 
                                    if_else((Region=="Nordrhein-Westfalen") & (Date>=dmy("11.05.2020")),
                                            1,
                                            SocialDistancing)))


#Rheinland-Pfalz (mittel: 23.03.-12.05., leicht:13.05.-)
cases <- cases %>% 
  mutate(SocialDistancing = if_else((Region=="Rheinland-Pfalz") & (Date>=dmy("23.03.2020")) & (Date<=dmy("12.05.2020")), 
                                    2, 
                                    if_else((Region=="Rheinland-Pfalz") & (Date>=dmy("13.05.2020")),
                                            1,
                                            SocialDistancing)))


#Saarland (stark: 21.03.-03.05., mittel: 04.05.-31.05., leicht: 01.06.-)
cases <- cases %>% 
  mutate(SocialDistancing = if_else((Region=="Saarland") & (Date>=dmy("21.03.2020")) & (Date<=dmy("03.05.2020")), 
                                    3, 
                                    if_else((Region=="Saarland") & (Date>=dmy("04.05.2020")) & (Date<=dmy("31.05.2020")),
                                            2,
                                            if_else((Region=="Saarland") & (Date>=dmy("01.06.2020")),
                                                    1,
                                                    SocialDistancing))))


#Sachsen (stark: 21.03.-20.04., mittel: 21.04.-14.05., leicht:15.05.-06.06)
cases <- cases %>% 
  mutate(SocialDistancing = if_else((Region=="Sachsen") & (Date>=dmy("21.03.2020")) & (Date<=dmy("20.04.2020")), 
                                    3, 
                                    if_else((Region=="Sachsen") & (Date>=dmy("21.04.2020")) & (Date<=dmy("14.05.2020")),
                                            2,
                                            if_else((Region=="Sachsen") & (Date>=dmy("15.05.2020"))& (Date<=dmy("06.06.2020")),
                                                    1,
                                                    SocialDistancing))))


#Sachsen-Anhalt (mittel: 23.03.-03.05., leicht: 04.05.-28.05.)
cases <- cases %>% 
  mutate(SocialDistancing = if_else((Region=="Sachsen-Anhalt") & (Date>=dmy("23.03.2020")) & (Date<=dmy("03.05.2020")), 
                                    2, 
                                    if_else((Region=="Sachsen-Anhalt") & (Date>=dmy("04.05.2020")) & (Date<=dmy("28.05.2020")),
                                            1,
                                            SocialDistancing)))


#Schleswig-Holstein (mittel: 23.03.-09.05, leicht:10.05.-)
cases <- cases %>% 
  mutate(SocialDistancing = if_else((Region=="Schleswig-Holstein") & (Date>=dmy("23.03.2020")) & (Date<=dmy("09.05.2020")), 
                                    2, 
                                    if_else((Region=="Schleswig-Holstein") & (Date>=dmy("10.05.2020")),
                                            1,
                                            SocialDistancing)))


#Thüringen (mittel: 23.03.-12.05., leicht:13.05.)
cases <- cases %>% 
  mutate(SocialDistancing = if_else((Region=="Thüringen") & (Date>=dmy("23.03.2020")) & (Date<=dmy("12.05.2020")), 
                                    2, 
                                    if_else((Region=="Thüringen") & (Date>=dmy("13.05.2020")),
                                            1,
                                            SocialDistancing)))


#add SD dummy
cases <- cases %>% 
  mutate(DummySD = as.factor(if_else(SocialDistancing > 0, 1, 0)))

#add ongoing duration of any social distancing measures
cases <- cases %>% 
  group_by(Region) %>% 
  mutate(DurationSD = if_else(SocialDistancing>0, 1, 0)) %>% 
  mutate(DurationSD = cumsum(DurationSD)) %>% 
  mutate(DurationSD = if_else(SocialDistancing==0, 0, DurationSD))

#add ongoing duration of maximum social distancing measures
cases <- cases %>% 
  group_by(Region) %>% 
  mutate(DurationMaxSD = if_else(SocialDistancing==max(SocialDistancing), 1, 0)) %>% 
  mutate(DurationMaxSD = cumsum(DurationMaxSD)) %>% 
  mutate(DurationMaxSD = if_else(SocialDistancing==max(SocialDistancing), DurationMaxSD, 0))

#factorize sd column
cases$SocialDistancing <- as.factor(cases$SocialDistancing)

#add squared durations
cases <- cases %>% 
  mutate(DurationSD.2 = DurationSD^2) %>% 
  mutate(DurationMaxSD.2 = DurationMaxSD^2)

#make data with no sd for simulation
sim.cases <- cases %>% 
  mutate(SocialDistancing=as.factor(0)) %>% 
  mutate(DummySD=as.factor(0)) %>%
  mutate(DurationSD=0) %>% 
  mutate(DurationSD.2=0) %>% 
  mutate(DurationMaxSD=0) %>% 
  mutate(DurationMaxSD.2=0) 


#linear regression model
model <- train(Cases ~ Region + Date + DummySD + DaysSince100 + DaysSince100.2 + DaysSince100.3 + DurationSD + DurationSD.2 + DurationMaxSD + DurationMaxSD.2, data=cases, method="lm")

#make data.frame with actual and simulated cases
sim.data <- data.frame(Land=cases$Region,
                       Datum=cases$Date,
                       Tag.tatsächlich=cases$Cases,
                       Tag.simuliert=round(predict(model, sim.cases)))

#get cases for entire Germany and bind dataframes together
totals <- sim.data %>%
  group_by(Datum) %>% 
  summarise(Tag.tatsächlich=sum(Tag.tatsächlich),
            Tag.simuliert=sum(Tag.simuliert)) %>% 
  mutate(Land = "Deutschland")

sim.data <- bind_rows(sim.data, totals)

#accumulate cases
sim.data <- sim.data %>% 
  mutate(Tag.simuliert = if_else(Tag.simuliert>=0, Tag.simuliert, 0)) %>% 
  group_by(Land) %>% 
  mutate(Fälle.tatsächlich = cumsum(Tag.tatsächlich)) %>% 
  mutate(Fälle.simuliert = cumsum(Tag.simuliert))


#make static plot
plot <- ggplot(sim.data, aes(color=Land)) + 
  geom_line(aes(Datum, Fälle.tatsächlich), alpha=0.4) + 
  geom_line(aes(Datum, Fälle.simuliert)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::comma) + 
  ggtitle("Covid-19 Fälle in Deutschland\n(tatsächlich vs. ohne Lockdown)") +
  ylab("Fälle")

#make plot interactive
ggplotly(plot)

