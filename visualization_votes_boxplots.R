pge18 <- read.csv("C:\\Users\\HP\\Desktop\\votes.csv",
                  stringsAsFactors = T)
pge18$constituency <- gsub('-','',pge18$constituency)
pge18$votes_cast <- gsub(',','',pge18$votes_cast)
pge18$votes_disq <- gsub(',','',pge18$votes_disq)

pge18$constituency <- as.factor(pge18$constituency)
pge18$votes_cast <- as.numeric(pge18$votes_cast)
pge18$votes_disq <- as.numeric(pge18$votes_disq)

str(pge18)

ggplot(data = pge18) +
  aes(x = votes_cast, y = votes_disq, color = party) +
  geom_point(size = 2.5, alpha = 0.6) +
  geom_smooth(method = lm, se = T, color = 'black') +
  theme_ipsum()

ggplot(data = pge18) +
  aes(x = votes_disq, y = party, fill = party) +
  geom_boxplot() +
  theme_ipsum() +
  theme(legend.position = 'none')

