library(tidyverse)
mpg

ggplot(data = mpg) + 
  geom_point(mapping = aes(x=displ, y = hwy, color = displ < 5))

nrow(mpg)
ncol(mpg)

ggplot(data = mpg) +
  geom_point(aes(x=hwy, y = cyl))

ggplot(data = mpg) + 
  geom_point(aes(x=class, y = drv))

ggplot(data = mpg) +
  geom_point(aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

ggplot(data = mpg) +
  geom_point(aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)

ggplot(data = mpg) +
  geom_point(aes(x = displ, y = hwy)) + 
  facet_grid(. ~ class)

ggplot(data = mpg) +
  geom_point(aes(x=drv, y = cyl))

ggplot(data = mpg) +
  geom_point(aes(x=drv, y = cyl)) +
  facet_grid(drv~cyl)

ggplot(data = mpg) +
  geom_point(aes(x=displ,y = hwy)) +
  facet_grid(drv~ .)

ggplot(data = mpg) +
  geom_point(aes(x=displ,y = hwy)) +
  facet_grid(. ~ cyl)



######### GEOMS
ggplot(data = mpg) + 
  geom_point(aes(x=displ, y = hwy))

ggplot(data = mpg) +
  geom_smooth(aes(x=displ, y = hwy)) + 
  geom_point(aes(x=displ,y=hwy))

ggplot(data = mpg, aes(x = displ, y = hwy)) + 
  geom_smooth() + 
  geom_point()

ggplot(data = mpg, aes(x = displ, y = hwy)) + # global mappings
  geom_smooth() + 
  geom_point(aes(color=class)) # local mappings

ggplot(data = mpg, aes(x=displ, y = hwy)) +
  geom_point(aes(color = class)) + 
  geom_smooth(
    data = filter(mpg, class == "subcompact"), 
    se = F)

ggplot(data = mpg, aes(x=displ, y = hwy, color = drv)) +
  geom_point() +
  geom_smooth(se = T)


# 6
ggplot(data = mpg, aes(x=displ, y = hwy)) + 
  geom_point() +
  geom_smooth(se = F)

ggplot(data = mpg, aes(x=displ, y = hwy)) + 
  geom_point() +
  geom_smooth(aes(group=drv), se = F)

ggplot(data = mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point() + 
  geom_smooth(se=F)

ggplot(data = mpg, aes(x=displ,y=hwy)) + 
  geom_point(aes(color=drv)) + 
  geom_smooth(se=F)

ggplot(data = mpg, aes(x=displ, y = hwy)) +
  geom_point(aes(color=drv)) +
  geom_smooth(aes(linetype=drv), se = F)

ggplot(data = mpg, aes(x=displ,y=hwy,color = drv)) +
  geom_point()


############ Statistical Transformations
diamonds
ggplot(data = diamonds) +
  geom_bar(aes(x=cut))

ggplot(data = diamonds) +
  geom_bar(aes(x=cut, y = ..prop.., group = 10))

ggplot(data = diamonds) +
  geom_bar(aes(x=cut, y = ..prop..))

ggplot(data = diamonds) +
  stat_summary(aes(x=cut, y = depth),
               fun.ymin = min,
               fun.ymax = max,
               fun.y = median
               )


############### POsitions
ggplot(data = diamonds, aes(x=cut, fill = clarity)) +
  geom_bar(alpha = 1/5, position = "identity")

ggplot(data = diamonds, aes(x=cut, color = clarity)) +
  geom_bar(fill = NA, position = "identity")

ggplot(data = diamonds) +
  geom_bar(aes(x=cut, fill = clarity),
           position = "fill")

ggplot(data = diamonds) +
  geom_bar(aes(x=cut, fill = clarity),
           position = "dodge")

# to avoid overplotting (i.e. on a scatterplot)
ggplot(data = mpg) +
  geom_point(aes(x=displ,y=hwy),
             position = "jitter")

ggplot(data = mpg, aes(x=cty, y = hwy)) +
  geom_point()
ggplot(data = mpg, aes(x=cty, y = hwy)) +
  geom_point()+
  geom_jitter()

ggplot(data = mpg, aes(x=cty, y = hwy)) +
  geom_point()+
  geom_count()

mpg
ggplot(data = mpg, aes(x= trans, y = hwy)) +
  geom_boxplot()
?geom_boxplot

ggplot(data = mpg, aes(x= cty, y = hwy)) +
  geom_boxplot()

############ Coordinate Systems
ggplot(data = mpg, aes(x=class, y =hwy)) + 
  geom_boxplot()+
  coord_flip()


nz <- map_data("nz")
ggplot(nz, aes(long,lat,group=group)) +
  geom_polygon(fill = "white",color = "black") +
  coord_quickmap()


bar <- ggplot(diamonds) +
  geom_bar(aes(x=cut, fill = cut),
           show.legend = F,
           width = 1) +
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar
bar+coord_flip()
bar + coord_polar()

diamonds
stack1 <- ggplot(diamonds, aes(x = cut, fill = clarity)) +
  geom_bar()

stack1 + coord_polar()

?labs()

nz <- map_data("nz")
ggplot(nz, aes(long,lat,group=group)) +
  geom_polygon(fill = "white",color = "black") +
  coord_quickmap()

nz <- map_data("nz")
ggplot(nz, aes(long,lat,group=group)) +
  geom_polygon(fill = "white",color = "black") +
  coord_quickmap()

?coord_map
?coord_quickmap

ggplot(mpg, aes(x = cty, y=hwy)) +
  geom_point() + 
  geom_abline() +
  coord_fixed()

?coord_fixed
?geom_abline

