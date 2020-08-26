####### Chap. 3

library(nycflights13)
library(tidyverse)

?flights
flights

filter(flights, month == 1, day ==1)

(dec25 <- filter(flights, month == 12, day == 25))

sqrt(2)^2 == 2
near(sqrt(2)^2,2)

filter(flights, month == 11 | month == 12)
filter(flights, month %in% c(11,12))

filter(flights, !(arr_delay > 120 | dep_delay > 120))
filter(flights, arr_delay <= 120, dep_delay <= 120)

df <- tibble(x = c(1,NA,3))
filter(df,x>1)
filter(df,is.na(x) | x> 1)

# Ex.
filter(flights, arr_delay >= 120)
filter(flights, dest %in% c("IAH","HOU"))
filter(flights, carrier %in% c("UA","DL","AA"))
filter(flights, month %in% c(7,8,9))
filter(flights, arr_delay > 120, dep_delay <= 0)
filter(flights, dep_time <= 600 | dep_time >= 2400)
filter(flights, between(dep_time, 0001,600))

?between
filter(flights,is.na(dep_time))

NA^0
NA | TRUE
F & NA

NA*0
##### is.na returns T for NA values and F for nonNA values

flights
arrange(flights,year,month,day)
arrange(flights,desc(arr_delay))

#arrange(flights,year) ?????
  
arrange(flights,desc(dep_delay))
arrange(flights,dep_delay)
arrange(flights,air_time)
arrange(flights,distance)
arrange(flights,desc(distance))


#### now for oclumns
select(flights,year,month,day)
select(flights,year:day)
select(flights,-(year:day))

rename(flights, airline = carrier)

select(flights,time_hour,carrier, everything())
select(flights,year, year)


?one_of
vars <- c("year","month","day","dep_delay","arr_delay")
select(flights,one_of(vars[3]))

select(flights, contains("Time", ignore.case = T))



flights_sml <- select(flights,
                      year:day,
                      ends_with("delay"),
                      distance,
                      air_time)
View(flights_sml)

mutate(flights_sml, gain = arr_delay - dep_delay,
       speed = distance / air_time * 60)
mutate(flights_sml,
       gain = arr_delay - dep_delay,
       hours = air_time / 60, 
       gain_per_hour = gain / hours)

transmute(flights, 
          gain = arr_delay - dep_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours)

transmute(flights, dep_time,
          hour = dep_time %/% 100,
          minute = dep_time %% 100)

transmute(flights, dep_time,
          hour = dep_time / 100,
          minute = dep_time / 100)
#flights$dep_time

x <- 1:10
lag(x)
x - lag(x)
lead(x)
cumsum(x)
cummean(x)

flights
NewFlights <- mutate(flights,
       dep_timeN = (dep_time %/% 100) *60 + (dep_time %%100),
       sched_dep_timeN = (sched_dep_time %/% 100) * 60 + (sched_dep_time %% 100),
       )

(NewFlights <- select(flights,
                     air_time, arr_time, dep_time))
mutate(NewFlights,
       arrTimeComp = arr_time - dep_time)

mutate(NewFlights,
       arr_timeN = (arr_time %/% 100)*60 + (arr_time%%100),
       dep_timeN = (dep_time %/% 100)*60 + (dep_time%%100),
       airTimeComp = arr_timeN - dep_timeN)


select(flights, contains = "dep")
depDat <- select(flights, dep_time, sched_dep_time, dep_delay)
mutate(depDat, 
       dep_timeCont = (dep_time %/% 100) * 60 + (dep_time%%100),
       sched_dep_timeCont = (sched_dep_time %/% 100)*60 + (sched_dep_time%%100),
       delayCalc = dep_time - sched_dep_time,
       delayCalcC = dep_timeCont - sched_dep_timeCont
       )

min_rank(flights$dep_delay)[1:10]

1:3 + 1:10


by_day <- group_by(flights, year, month, day)
by_day

summarize(by_day, delay = mean(dep_delay, na.rm = T))

by_dest <- group_by(flights, dest)
delay <- summarize(by_dest, 
                  count = n(),
                  dist = mean(distance, na.rm = T),
                  delay = mean(arr_delay, na.rm = T))
delay <- filter(delay, count > 20, dest != "HNL")
delay
ggplot(delay, aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) + 
  geom_smooth(se = F)


# more efficient method:
delays <- flights %>%
  group_by(dest) %>%
  summarize( count = n(),
             dist = mean(distance, na.rm = T),
             delay = mean(arr_delay, na.rm = T)) %>%
  filter(count > 20, dest != "HNL")

not_cancelled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>%
  group_by(year,month,day) %>%
  summarize(mean = mean(dep_delay))

delays <- not_cancelled %>%
  group_by(tailnum)%>%
  summarize(delay = mean(arr_delay))

ggplot(delays, aes(x = delay)) +
  geom_freqpoly(binwidth = 10)


delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarize(
    delay = mean(arr_delay, na.rm = T),
    n = n()
    
  )

ggplot(delays, aes(x=n, y = delay)) + 
  geom_point(alpha = 1/10)


delays %>%
  filter(n > 20) %>%
  ggplot(aes(x = n, y = delay)) +
  geom_point(alpha = 1/10)


batting <- as_tibble(Lahman::Batting)
batters <- batting %>%
  group_by(playerID) %>%
  summarize(
    ba = sum(H, na.rm = T) / sum(AB, na.rm = T),
    ab = sum(AB, na.rm = T)
  )

batters %>%
  filter(ab > 100) %>%
  ggplot(aes(x = ab, y = ba)) +
  geom_point() +
  geom_smooth(se=F)

not_cancelled %>%
  group_by(dest) %>%
  summarize(distance_sd = sd(distance)) %>%
  arrange(desc(distance_sd))

not_cancelled %>%
  group_by(year,month,day) %>%
  summarize(
    first = min(dep_time),
    last = max(dep_time))
  )

not_cancelled %>%
  group_by(dest) %>%
  summarize(carriers = n_distinct(carrier)) %>%
  arrange(desc(carriers))

not_cancelled %>%
  group_by(year,month,day) %>%
  summarize(n_early = sum(dep_time < 500))

not_cancelled %>%
  group_by(year,month,day) %>%
  summarize(big_delay = mean(dep_delay > 60))


daily <- group_by(flights, year,month,day)
(per_day <- summarize(daily, flights = n()))
(per_month <- summarize(per_day, flights = sum(flights)))
(per_year <- summarize(per_month, flights = sum(flights)))


daily %>%
  ungroup() %>%
  summarize(flights = n())


###### Exc:
not_cancelled %>%
  group_by(tailnum) %>%
  summarize(propDelay15 = mean(arr_delay <= 15 & arr_delay >= -15)) %>%
  filter(propDelay15 >= .5)

not_cancelled %>%
  group_by(tailnum) %>%
  summarize(BigDel = mean(arr_delay > 120)) %>%
  filter(BigDel == .01)

not_cancelled %>%
  count(dest)

not_cancelled %>%
  group_by(dest) %>%
  summarize(totFl = n())

not_cancelled %>%
  count(tailnum, wt = distance)

not_cancelled %>%
  group_by(tailnum) %>%
  summarize(DistPerPlane = sum(distance))


head(flights) 

cancelled_per_day <- 
  flights %>%
  mutate(Canc = (is.na(arr_delay) | is.na(dep_delay))) %>%
  group_by(year,month,day) %>%
  summarize(CancTot = sum(Canc), flights_num = n()
            
            )
ggplot(cancelled_per_day, aes(x=flights_num, y = CancTot)) +
         geom_point()

cancelled_per_day_01 <-
  flights %>%
  mutate(Canc = (is.na(arr_delay) | is.na(dep_delay))) %>%
  group_by(year,month,day) %>%
  mutate(
    CancTot = sum(Canc), 
    flights_num = n(),
    CancProp = mean(CancTot, na.rm = T),
    ArrDelAvg = mean(arr_delay, na.rm = T)
    )

ggplot(cancelled_per_day_01, aes(x = CancProp, y = ArrDelAvg)) +
  geom_point() + 
  geom_smooth()

ggplot(cancelled_per_day_01, aes(x = ArrDelAvg, y = CancProp)) +
  geom_point() + 
  geom_smooth()

flights %>%
  group_by(carrier) %>%
  summarize(AvgDel = mean(arr_delay, na.rm = T)) %>%
  arrange(desc(AvgDel))
  
flights %>%
  group_by(carrier, dest) %>%
  summarize(AvgDel = mean(arr_delay, na.rm = T)) %>%
  arrange(desc(AvgDel))

flights %>%
  group_by(year, month,day,tailnum) %>%
  filter(dep_delay < 60) %>%
  summarize(FlightsFirst = n()) %>%
  arrange(desc(FlightsFirst))
  

flights_sml %>%
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10)

popular_dests <- flights %>%
  group_by(dest) %>%
  filter(n() > 365)
popular_dests
View(popular_dests)

popular_dests %>%
  filter(arr_delay > 0) %>%
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>%
  select(year:day, dest, arr_delay, prop_delay)


##### Exc
not_cancelled %>%
  group_by(tailnum) %>%
  summarize(avgDel = mean(arr_delay), n = n()) %>%
  filter(n >= 20) %>%
  filter(min_rank(desc(avgDel)) ==1)

flights %>%
  filter(!is.na(tailnum)) %>%
  mutate(on_time = !is.na(arr_time) & (arr_delay <=0)) %>% # enusres not cancelled and not late
  group_by(tailnum) %>%
  summarize(on_time = mean(on_time), n = n()) %>%
  filter(min_rank(on_time) ==1)

quantile(count(flights, tailnum)$n)


flights %>%
  filter(!is.na(arr_delay | dep_delay)) %>%
  group_by(hour) %>%
  summarize(dep_delay = mean(dep_delay, na.rm = T), 
            arr_delay = mean(arr_delay,na.rm = T),
            tot_delay = dep_delay + arr_delay,
            n = n()
  ) %>%
  filter( n >= 20) %>%
  arrange(arr_delay)
  #filter(min_rank(arr_delay) == 1)

flights %>%
  filter(!is.na(arr_delay | dep_delay)) %>%
  group_by(dest)%>%
  summarize(TotDel = sum(arr_delay))%>%
  arrange(desc(TotDel))
  
flights %>%
  filter(!is.na(arr_delay | dep_delay)) %>%
  group_by(year,month,day) %>%
  summarize(TotDel = sum(arr_delay))

not_cancelled
by_flight_dest <- group_by(not_cancelled, year, month, day, tailnum, dest) 
(by_flight <- summarise(by_flight_dest, TotDel = sum(arr_delay)))  
(by_dest <- summarise(by_flight, TotDel = sum(TotDel)))


flight_del <- flights %>%
  filter(!is.na(arr_delay | dep_delay)) %>%
  group_by(dest) %>%
  mutate(TotDelTime = sum(arr_delay), 
         TotDelProp = arr_delay/TotDelTime)

flight_del %>%
  select(dest, flight, TotDelTime, TotDelProp)%>%
  arrange(desc(TotDelTime))

not_cancelled %>%
  arrange(origin, month, day, dep_time) %>%
  group_by(origin) %>%
  mutate(dep_delay_lag = lag(dep_delay)) %>%
  filter(!is.na(dep_delay), !is.na(dep_delay_lag))

fast_flights <- flights %>%
  group_by(dest) %>%
  mutate(TravelProp = air_time/distance) %>%
  filter(!is.na(TravelProp))

fast_flights %>%
  select(dest, air_time, distance, TravelProp, arr_delay) %>%
  arrange(TravelProp)

fast_flights %>%
  group_by(dest) %>%
  mutate(ShortDist = min(distance),
         
