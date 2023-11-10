req_pkgs <- c('tidyverse','ggplot2','ggthemes')
lapply(req_pkgs,library,character.only=T)

library(patchwork)

data(gapminder)
head(gapminder)

gapminder %>% 
  filter(country == 'Vietnam') -> viet

mean_viet <- viet %>% 
  summarize(avg = mean(lifeExp))

mean_viet

ggplot(viet)+
  aes(x = lifeExp) +
  geom_density(fill = 'yellow') +
  geom_vline(data = mean_viet, aes(xintercept = avg), linetype = 'dashed', linewidth = 1.0) +
  geom_text(data = viet, aes(x = 59, y = 0.015, label = 'Mean Life Expectancy '), hjust = 0, size = 4, color = 'black') +
  geom_text(data = viet, aes(x = 59, y = 0.014, label = 'in Vietnam = 57.5'), hjust = 0, size = 4, color = 'black') +
  theme_few() +
  ggtitle('Life Expectancy in Vietnam: 1952-2007') +
  theme(legend.position = 'none') -> p1

ggplot(viet) +
  aes(y = lifeExp) +  
  geom_boxplot(fill = 'moccasin') +
  geom_jitter(aes(x = 0), width = 0.1, size = 1.5, color = 'lightsalmon4') +  
  geom_text(data = viet, aes(x = 0.2, y = 58, label = 'Median Life Expectancy'), hjust = 0, size = 4, color = 'violetred4') +
  geom_text(data = viet, aes(x = 0.15, y = 58, label = 'in Vietnam = 57.3'), hjust = 0, size = 4, color = 'violetred4') +
  theme_bw() +
  coord_flip() +
  ggtitle('Summary Stats of Life Expectancy in Vietnam: 1952-2007') +
  theme(legend.position = 'none') -> p2

p1 + p2
