# load the required packages
# install.packages() if a package is missing 

libs <- c('gapminder','tidyverse','ggplot2','ggthemes','hrbrthemes','janitor')

installed.libs <- libs %in% rownames(installed.libs)
if(any(installed.libs == F)){
  install.packages(libs[!installed.libs])
}

invisible(lapply(libs,library,character.only=T))


# examine the data set 
str(gapminder)

# convert year to factor for later use in ggplot
gapminder$year <- as.factor(gapminder$year)

# manipulate the data set for desired data subset from gapminder using dplyr package
selected_countries_asia <- gapminder %>% 
  clean_names() %>% 
  filter(continent == 'Asia') %>% 
  filter(year == c(1952,2007)) %>% 
  mutate(change_life_exp = diff(life_exp),
         .by = country) %>% 
  mutate(country = fct_reorder(country, change_life_exp))
         
# to create dumbbell plot for change in life expectancy in Asia: 1952-2007
selected_countries_asia %>% 
  ggplot(aes(x = life_exp, y = country)) +
  geom_line(
    color = '#333333',
    linewidth = 1
  ) +
  geom_point(
    aes(fill = year),
    shape = 21,  # try changing the value to look at different options
    size = 4,
    color = '#333333',
    stroke = 1
  ) +
  scale_fill_manual(values = c('firebrick1','springgreen4')) + # a plethora of colors are available on the internet
  labs(x = 'Change in average life expectancy',
       y = NULL,
       title = "Asian countries substantially improved their population's life expectancy between 1952-2007",
       subtitle = 'Source: Gapminder',
       fill = 'Year') +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text = element_text(face = 'bold', colour = 'black') # due to large number of labels, I use boldface for improved visibility
  ) -> dumbbell_plot

# lastly, save the plot in desired location, file type, size and units
ggsave("C:\\Users\\HP\\Desktop\\dumbbell_asia.jpeg", plot = dumbbell_plot, device = 'jpeg', width = 24, height = 12, units = c('in'))



# Alternatively, I replaced dumbbells with arrow heads 
# It makes it easier to decipher the direction of change in an instant 

# first, we add a column accounting for change in life expectancy arranged by country
# using if_else function makes sure countries with increased and decreased life expectancy are grouped, improving the ability to see the oddity 

selected_countries_africa <- gapminder %>% 
  clean_names() %>% 
  filter(continent == 'Africa') %>% 
  filter(year == c(1952,2007)) %>% 
  mutate(change_life_exp = diff(life_exp),
         order_arrow = if_else(change_life_exp < 0, -1, 1) * life_exp[1], 
         .by = country) %>% 
  mutate(country = fct_reorder(country, order_arrow))

# now, create a ggplot using the life expectancy data for Africa
selected_countries_africa %>% 
  ggplot(aes(x = life_exp, y = country)) +
  
  # in place of geom_line() and geom_point, I use geom_path with specifications for color, arrow, and correspomding line
  geom_path(
    aes(color = (change_life_exp < 0)),
    arrow = arrow(length = unit(0.3, 'cm'), type = 'closed'),
    linewidth = 1
  ) +
  scale_fill_manual(values = c('firebrick1','dodgerblue4')) +
  labs(x = 'Change in average life expectancy',
       y = element_blank(),
       title = "African states made substantial improvements in life expectancy between 1952-2007",
       subtitle = 'Zimbabwe and Swaziland, however, slipped on the key HDI indicator',
       caption = 'Source: Gapminder',
       fill = 'Year') +
  
  # abundant themes are available in ggplot2, with more in ggthemes and hrbrthemes packages. Try them out to find what suits your taste. Go wild!
  ggthemes::theme_few() +
  
  # due to color configurations, legend is no longer needed. To improve legibility, I tilt the y-axis tick labels by 25 degrees
  theme(legend.position = 'none',
        axis.text = element_text(face = 'bold', colour = 'black'),
        axis.text.y = element_text(angle = 25)) -> arrows

# lastly, save the plot in desired location, file type, size and units
ggsave("C:\\Users\\HP\\Desktop\\arrows_afr.jpeg", plot = arrows, device = 'jpeg', width = 24, height = 12, units = c('in'))
