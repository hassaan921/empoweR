###### load the required libraries ######

#install.packages('package_name) if necessary
req_pkgs <- c('ggplot2','tidyverse','janitor','rvest','ggdark','scales','ggparliament')
lapply(req_pkgs,library,character.only = T)

###### Scraping the data from your desired webpage ######

wiki_link <- 'https://en.wikipedia.org/wiki/2018_Pakistani_general_election'  
wiki_page <- rvest::read_html(wiki_link)

# selectorgadget extension (Google Chrome) may not come handy here
# instead, inspect the page for table and note the html_node for your table

# then simply run the first two lines
wiki_table <- wiki_page %>% 
  rvest::html_nodes('table') %>% 
  
  # from the list, select the number of your html_node and put in .[] 
  .[7] %>% 
  rvest::html_table() %>% .[[1]]

glimpse(wiki_table)
view(wiki_table)

###### Cleaning data ######

# Set column names based on the first row
colnames(wiki_table) <- wiki_table[1, ]

# Remove the first row
wiki_table <- wiki_table[-1, ]

# Reset row names
rownames(wiki_table) <- NULL

# remove commas in column showing number of votes
wiki_table$Votes <- gsub(',','',wiki_table$Votes)

# convert Votes to numeric
wiki_table$Votes <- as.numeric(wiki_table$Votes)

# simplifying to avoid messing with capitalization, special characters in column names
# in addition, renaming columns, removing unnecessary columns, and filtering for parties with 0 seats
wiki_table %>% janitor::clean_names() %>%
  dplyr::rename(political_party = party_2) %>%  
  dplyr::select(-party,-na) %>% 
  dplyr::filter(total > 0) -> wiki_table_clean

# lastly, removing NAs.
wiki_table_clean <- stats::na.omit(wiki_table_clean)

# voila!
tibble::view(wiki_table_clean) # a little tip: adding the package name beforehand creates a legible code

# we do not need total row here so let's slice it off
final_na_13 <- wiki_table_clean %>% 
  dplyr::slice(1:13) %>% 
  view()

# making a barplot showing votes polled for NA seats by party. Percentages are also shown
ggplot(top_10) +
  aes(x = reorder(political_party, votes), y = votes, fill = political_party) +
  geom_bar(stat = 'identity') +
  geom_text(
    aes(label = percent), 
    hjust = -0.15, 
    col = "white",
    size = 3
  ) +
  scale_fill_manual(values = c("#2c001e", 
                               "#47000a",
                               "#a50026",
                               "#d73027",
                               "#f46d43",
                               "#fdae61",
                               "#fee090",
                               "#ffffbf",
                               "#ffffff",
                               "#e0f3f8",
                               "#abd9e9",
                               "#74add1",
                               "#4575b4")) +
  scale_y_continuous(labels = scales::label_number()) + # scales::comma
  dark_theme_light() +
  theme(axis.text = element_text(color = 'white', face = 'bold')) +
  theme(legend.position = 'none') +
  labs(y = 'Votes',
       x = element_blank(),
       title = 'General Elections 2018: Final Vote Count for Top 10 Political Parties',
       subtitle = 'Top players bag 85.12 percent of all votes cast (percentage displayed by party)',
       caption = 'Source: Election Commission of Pakistan') +
  coord_flip() -> gep_18

print(gep_18)

# save the plot to desired location, in desired format, and size
ggsave("C:\\Users\\HP\\Desktop\\gpe_18.jpeg", plot = gep_18, device = jpeg, width = 12, height = 8, unit = c('in'))

# plotting semi circle parliament plot according to party position in NA (seats = 342)
library(ggparliament)

# sometimes, code for advanced democracies is available in R, however, not for
# countries like Pakistan. In this case, I used the instructions and code on 
# ??ggparliament (click on ggparliament::basic-parliament-plots_1) to adapt it 
# according to my scraped data

us_rep <- election_data %>%
  filter(country == "USA" &
           year == 2016 &
           house == "Representatives")
view(us_rep)

# replicating the code on instructions page 
final_na_13 %>% 
  dplyr::mutate(year = c(2016),
         country = c('Pakistan'),
         house = c('National Assembly'),
         colour = c("firebrick2", 
                    "palegreen4",
                    "black",
                    "lawngreen",
                    "orchid1",
                    "red3",
                    "springgreen4",
                    "limegreen",
                    "darkslategray",
                    "gold",
                    "blue",
                    "slategray",
                    "purple3")) %>% 
  select(year,country, house, political_party, total, colour) -> gg_par_pk

utils::str(final_na_13)
#convert total from character to numeric
gg_par_pk$total <- as.numeric(gg_par_pk$total)

# parliament_data() will give us coordinates (x,y) needed to plot
pk_na_semicircle <- parliament_data(election_data = gg_par_pk,
                                    type = 'semicircle',
                                    parl_rows = 10,
                                    party_seats = gg_par_pk$total)
head(pk_na_semicircle)

# create a ggplot
ggplot(pk_na_semicircle) +
  aes(x = x, y = y, color = political_party) +
  geom_parliament_seats(size = 5) +
  theme_ggparliament() +
  labs(colour = NULL,
       title = '2018 General Elections: Party Position in the National Assembly of Pakistan') +
  scale_color_manual(values = pk_na_semicircle$colour,
                     limits = pk_na_semicircle$political_party) +
  theme(legend.position = 'bottom') -> pak_na

print(pak_na)
# save the plot 
ggsave("C:\\Users\\HP\\Desktop\\pak_na.jpeg", plot = pak_na, device = jpeg, width = 12, height = 8, unit = c('in'))
