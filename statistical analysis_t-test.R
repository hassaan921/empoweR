req_pack <- c('gapminder','tidyverse','ggplot2','janitor','hrbrthemes','ggthemes')
lapply(req_pack, library, character.only = T)

gapminder %>% 
  clean_names() -> gapminder

# we can run a single-sample to test a known mean (e.g., mu = 50)
gapminder %>% 
  filter(country == 'Pakistan') -> pak

view(pak)

pak %>% 
  select(life_exp) %>% 
  t.test(mu = 50)

mean_le_pak <- pak %>% 
  summarize(mean_pk = mean(life_exp))

# we can create a visualization to showcase the stastically signigicant change
ggplot(data = pak) +
  aes(x = life_exp) +
  geom_density(fill = 'dark green', alpha = 0.7) +
  geom_vline(data = mean_le_pak, aes(xintercept = mean_pk, color = 'brown4'), linetype = 'dashed', linewidth = 1.0) +
  theme_ipsum() +
  labs(x = 'Life Expectancy', y = NULL,
       title = 'Life expectancy in Pakistan increased by 22 years over 55 years',
       subtitle = 'The data covers change between 1952 and 2007',
       caption = 'Source: Gapminder') +
  theme(legend.position = 'none')

# we can run a two-sided t-test (is there a stast. signi. difference?) to compare two unpaired countries
gapminder %>% 
  filter(country %in% c('India','China')) -> ind_chn
  
ind_chn %>% 
  t.test(life_exp ~ country, data = .,
         alternative = 'two.sided') # significant 

# in case, data points for the two countries aren't present, only keep instances when both are present in a certain year, for instance
  #group_by(year) %>%
  #filter(n_distinct(country) == 2) %>%
  #ungroup() -> ind_chn

# now, calculating mean life expectancy
mean_le_ind_chn <- ind_chn %>% 
  group_by(country) %>% 
  summarize(mean_lex = mean(life_exp))

# create density plots
ggplot(data = ind_chn) +
  aes(x = life_exp, group = country) +
  geom_density(aes(fill = country, alpha = 0.6)) +
  geom_vline(data = mean_le_ind_chn, aes(xintercept = mean_lex, linetype = country, color = country), linetype = 'dashed', linewidth = 1.0) +
  scale_linetype_manual(values = c(1, 2)) +
  geom_text(data = ind_chn %>% filter(country == 'India'), aes(x = 45, y = 0.04, label = 'Mean Life Expectancy in India = 53.2'), hjust = 0, size = 4, color = 'turquoise') +
  geom_text(data = ind_chn %>% filter(country == 'China'), aes(x = 54, y = 0.045, label = 'Mean Life Expectancy in China = 61.8'), hjust = 0, size = 4, color = 'tomato1') +
  theme_few() +
  labs(x = 'Life Span', y = NULL,
       title = "Life Expectancy for India and China: 1952-2007",
       subtitle = "Difference in mean life expectancy found to be statistically significant",
       caption = "Source: Gapminder") + 
  theme(legend.position = 'none') -> ind_chn_plot # i just removed the legend (alpha) and pushed the country names onto the density plots themselves

# the difference in mean life expectancy between 1952-2007 is not statistically significant

ggplot(data = pk_ban) +
  aes(x = life_exp, group = country) +
  geom_density(aes(fill = country, alpha = 0.6)) +
  geom_vline(data = avg_lex, aes(xintercept = mean_lex, linetype = country, color = country), linetype = 'dashed', linewidth = 1.0) +
  scale_linetype_manual(values = c(1, 2)) +
  theme_few() +
  labs(x = 'Life Span', y = NULL,
       title = "Life Expectancy for Pakistan and Bangladesh: 1952-2007",
       subtitle = "Difference in mean life expectancy found to be statistically insignificant",
       caption = "Source: Gapminder") +

print(pb_plot)

ggsave("C:\\Users\\HP\\Desktop\\ind_chn_plot.jpeg", plot = ind_chn_plot, device = "jpeg", width = 14, height = 10, units = "in")

# from the plots, it seems that the difference is not statistically significant. We check (one-sided, unpaired)
gapminder %>% 
  filter(country %in% c('Pakistan','Bangladesh')) %>% 
  t.test(life_exp ~ country, data = .,
         alternative = 'less',
         conf.level = 0.95) # by default, 95%

# we can also see if life expectancy on a certain continent has changed over the years (paired test: same sample at different times)
gapminder %>% 
  filter(year %in% c(1972,2007) | continent == 'Asia') %>% 
  mutate(year = factor(year, levels = c(2007, 1972))) %>%  
  t.test(life_exp ~ year, data = .,
         paired = T)

# filtering and creating the 'afr' dataset from the gapminder
gapminder %>%
  filter(year %in% c(1957, 2007) & continent == 'Africa') %>%
  mutate(year = factor(year, levels = c(2007, 1957))) -> afr

# Calculate mean lifeExp values for each year
mean_values <- afr %>%
  group_by(year) %>%
  summarize(mean_lifeExp = mean(lifeExp))

# Create the density plot with different fill and color
p <- ggplot(afr) +
  aes(x = lifeExp, group = year) +
  geom_density(aes(fill = year, color = year), alpha = 0.7) +
  scale_fill_manual(values = c("#69b3a2", "#FF5733")) +  # Set fill colors
  scale_color_manual(values = c("#e9ecef", "#FF5733")) +  # Set line colors
  theme_minimal()  # You can use 'theme_minimal' for simplicity

# Add dotted lines for mean values with increased width
p + geom_vline(data = mean_values, aes(xintercept = mean_lifeExp, linetype = year),
               color = "deepskyblue", linetype = "dashed", linewidth = 1.0)  # Increase the line width (adjust size as needed)
