#### Chapter 5 Exercises from book
library(tidyverse)

# categorical - use bar plot
ggplot(data = diamonds) +
  geom_bar(aes(x=cut))

diamonds %>%
  count(cut)

# continous
ggplot(data = diamonds) +
  geom_histogram(aes(x = carat), binwidth = .5)

diamonds %>%
  count(cut_width(carat, .5))

smaller <- diamonds %>%
  filter(carat < 3)

ggplot(data = smaller, aes(x = carat)) +
  geom_histogram(binwidth = .1)

# overlay multiple histograms
ggplot(data = smaller, aes(x = carat, color = cut)) +
  geom_freqpoly(binwidth = .1) # uses lines instead of bars

ggplot(data = smaller, aes(x = carat)) +
  geom_freqpoly(binwidth = .01)


ggplot(diamonds) +
  geom_histogram(aes(x=y), binwidth = .5) # y is width in mm

# to better see rare avalues use coord_cartesian:
ggplot(diamonds) +
  geom_histogram(aes(x=y), binwidth = .5) +
  coord_cartesian(ylim = c(0,50))


# now will remove those unusual values
unusual <- diamonds %>%
  filter (y < 3 | y > 20) %>%
  arrange(y)

unusual


ggplot(diamonds, aes(x=z)) +
  geom_histogram(binwidth = .5) #+
# coord_cartesian(ylim = c(0,50))

ggplot(diamonds, aes(x = price)) + # price in dollars
  geom_histogram(binwidth = 100)

diamonds %>%
  filter(carat == 1) %>%
  count()

head(diamonds)
ggplot(diamonds, aes(x = carat)) +
  geom_histogram(binwidth = .1) +
  coord_cartesian(xlim = c(0,.95)) #+ 
  #coord_cartesian(ylim = c(0,50))
  #xlim(0,1)

# dealing with missing values
diamonds2 <- diamonds %>%
  mutate(y = ifelse(y < 3 | y > 20, NA, y))

ggplot(data = diamonds2, aes(x = x, y = y)) +
  geom_point()

nycflights13::flights %>%
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>%
  ggplot(aes(x=sched_dep_time,y = ..density..)) +
  geom_freqpoly(aes(color = cancelled),
    binwidth = 1/4)

ggplot(diamonds2, aes(x=y) ) +
  geom_histogram()
ggplot(diamonds2, aes(x=y)) +
  geom_bar()

mean(diamonds2$y, na.rm = T)
sum(diamonds2$y, na.rm = T)

#####
ggplot(diamonds, aes(x=price)) +
  geom_freqpoly(aes(color = cut), binwidth = 500)

ggplot(diamonds) +
  geom_bar(aes(x=cut))

# use 'density' to standardize 'counts' - makes area under curve = 1
ggplot(diamonds, aes(x = price, y = ..density..)) +
  geom_freqpoly(aes(color = cut), binwidth = 500)

ggplot(diamonds, aes(x = cut, y = price)) +
  geom_boxplot()

## use reorder with boxplots
ggplot(mpg, aes(x = class, y = hwy)) +
  geom_boxplot()

ggplot(mpg) +
  geom_boxplot(aes(x = reorder(class, hwy, FUN = median),
                   y = hwy)) +
  coord_flip()

# quick look to see what relates to price
plot(diamonds)
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point()

ggplot(diamonds, aes(x= carat, y = price)) +
  geom_boxplot(aes(group = cut_width(carat, 0.1)))

diamonds %>%
  mutate(color = fct_rev(color)) %>%
  ggplot(aes(x = color, y = price)) +
  geom_boxplot()

ggplot(diamonds, aes(x = clarity, y = price)) +
  geom_boxplot()

ggplot(diamonds, aes(x = cut, y = carat)) +
  geom_boxplot()


### Ex #4 in Covariance
library(lvplot)
ggplot(diamonds, aes(x = cut, y = price)) +
  geom_lv(aes(fill = ..LV..))
?geom_lv
# for comparison:
ggplot(diamonds, aes(x = cut, y = price)) +
  geom_boxplot()

### Ex #5 in Covariance
ggplot(diamonds, aes(x = cut, y = price)) +
  geom_violin() +
  coord_flip()

ggplot(diamonds) +
  geom_histogram(aes(x=price), binwidth = 1000) +
  facet_wrap(~ cut, ncol =1, scales = "free_y")

ggplot(diamonds, aes(x = price, y = ..density..))  +
  geom_freqpoly(aes(color=cut),binwidth = 300)

### Ex #6 in Covariance
library(ggbeeswarm)
ls("package:ggbeeswarm")


#### Two Categorical Variables

# size of each circle represents the # of obs for each combo
ggplot(diamonds) +
  geom_count(aes(x=cut, y = color))

# or compute the count:
diamonds %>%
  count(color,cut)
# and visualize
diamonds %>%
  count(color,cut) %>%
  ggplot(aes(x=color, y = cut)) +
  geom_tile(aes(fill = n))

## 2 Cat Variables Exc#1
diamonds %>%
  count(color,cut) %>%
  group_by(color) %>%
  mutate(prop = n/ sum(n)) %>%
  ggplot(aes(x=cut, y = color)) +
  geom_tile(aes(fill = prop)) +
  scale_fill_viridis_c(limits = c(0,1))

## 2 Cat Variables Exc#2
head(nycflights13::flights)
?nycflights13::flights

nycflights13::flights %>%
  filter(!is.na(arr_time)) %>%
  group_by(dest,month) %>%
  mutate(avgDel = mean(arr_delay)) %>%
  ggplot(aes(x=factor(month),y = dest)) +
  geom_tile(aes(fill = avgDel))

# to make it a bit cleaner:
nycflights13::flights %>%
  filter(!is.na(arr_time)) %>%
  group_by(month,dest) %>%
  summarise(avgDel = mean(arr_delay, na.rm=T)) %>%
  group_by(dest) %>%
  filter(n() ==12) %>% # makes sure each dest has a value in each month (excludes missing values)
  ungroup() %>%
  mutate(dest = reorder(dest, avgDel)) %>% # sort destinations by avgDel
  ggplot(aes(x=factor(month),y = dest)) +
  geom_tile(aes(fill = avgDel)) +
    scale_fill_viridis_c() # use better color scheme

ccc <- nycflights13::flights %>%
  filter(!is.na(arr_time)) %>%
  group_by(month,dest) %>%
  summarise(avgDel = mean(arr_delay, na.rm=T)) %>%
  group_by(dest) %>%
  filter(n() ==12)



#### Two Continuous Variables
ggplot(diamonds) +
  geom_point(aes(x=carat, y = price))

ggplot(diamonds) +
  geom_point(aes(x=carat, y = price),
             alpha = 1/100)

# can also bin in 2 dimensions w/geom_bin2d or geom_hex

ggplot(smaller) +
  geom_bin2d(aes(x=carat, y = price))

ggplot(smaller) +
  geom_hex(aes(x = carat, y = price))

# varwidth = T: width of box is proportional to # of points in each box
ggplot(smaller, aes(x = carat, y = price)) +
  geom_boxplot(aes(group = cut_width(carat, 0.1)), orientation = "x", varwidth = T)

#or dispaly same number of points in each bin
ggplot(smaller, aes(x=carat,y = price)) +
  geom_boxplot(aes(group = cut_number(carat, 20)), orientation = "x")

### 2 Cont Variables Exc #1
ggplot(smaller, aes(x = price, y = ..density..)) +
  geom_freqpoly(aes(color = carat), binwidth = .1)

# cut_number: choose # of bins;
# cut_width: choose width of bins
ggplot(smaller, aes(x = price, color = cut_number(carat,10))) +
  geom_freqpoly(binwidth = 100)
# with smaller number of bins/larger width - less colors to see on plot

### 2 Cont Variables Exc #2
ggplot(smaller, aes(x = carat, color = cut_number(price, 10))) +
  geom_freqpoly(binwidth = .2 )

ggplot(smaller, aes(x = carat, y = price)) +
  geom_boxplot(aes(group = cut_number(price, 20)), orientation = "x")

### 2 Cont Variables Exc #4
str(smaller)
ggplot(smaller, aes(x = cut, y = price, color = cut_number(carat, 5))) +
  geom_boxplot() +
  coord_flip() +
  labs(x = "Price ($)", y = "Cut", color = "Carat")


ggplot(smaller, aes(x = carat, y = price, color = cut)) +
  geom_boxplot() +
  facet_wrap(~cut, nrow = 5)


### 2 Cont Variables Exc #5
ggplot(diamonds) +
  geom_point(aes(x = x, y = y)) +
  coord_cartesian(xlim = c(4, 11), ylim = c(4,11))

ggplot(diamonds) +
  geom_boxplot(aes(x=x, y =y))


####### Patterns & Models
ggplot(faithful) +
  geom_point(aes(x=eruptions, y = waiting))

library(modelr)
mod <- lm(log(price) ~ log(carat), data = diamonds)

diamonds2 <- diamonds %>%
  add_residuals(mod) %>%
  mutate(resid = exp(resid))

ggplot(diamonds2) +
  geom_point(aes(x=carat, y = resid))

ggplot(diamonds2) +
  geom_boxplot(aes(x = cut, y = resid))
