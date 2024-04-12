

#### MTE equations exploring (following Chapter 2 in Brown et al. Scaling textbook)
library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())



gr <- read_csv("data-raw/growth-rates.csv", col_names = c("temperature_k", "growth_rate")) %>% 
  mutate(temperature_c = temperature_k -273.15)

##plot data
k <- 8.62*(10^-5) ### boltzman's constant

### this is the same as Figure 2.5a
gr %>% 
  ggplot(aes(x = temperature_c, y = growth_rate)) + geom_point()


gr %>% 
  filter(growth_rate == max(growth_rate)) %>% View ### find the optimal temperature for growth, to use to subset the dataset to growth rates only in the rising portion of the curve

gr_rising <- gr %>% 
  filter(temperature_c < 30.41322) %>% 
  mutate(inverse_temp = 1/(k*temperature_k)) ### this is the wonky inverse temperature you see on MTE plots


### plot growth rates in the rising portion of the curve
gr_rising %>% 
  ggplot(aes(x = temperature_c, y = growth_rate)) + geom_point()



### this is the same as Figure 2.5b
gr_rising %>% 
  ggplot(aes(x = inverse_temp, y = log(growth_rate))) + geom_point() +geom_smooth(method = "lm")

ea_fit <- lm(log(growth_rate) ~ inverse_temp, data = gr_rising)


summary(ea_fit) ### the slope here (inverse_temp coefficient is the activation energy, E, slightly off from the -0.66 you see in the textbook, probably because of digitizing error and my cutoff for suboptimal temps is slightly different than theirs)


