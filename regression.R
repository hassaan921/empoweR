############ LINEAR REGRESSION ############

civ_death <- read.csv("C:\\Users\\HP\\Documents\\Rcourse\\module\\peacekeepers level.csv")
head(civ_death)
str(civ_death)
library(tidyverse)
library(janitor)
library(corrplot)
library(car)
library(ggplot2)
library(patchwork)
library(hrbrthemes)

civ_death <- civ_death %>% clean_names() %>% rename(police_deployment = police_deploymment)

hist(civ_death$civdeath_best)
shapiro.test(civ_death$civdeath_best) #normal distribution

hist(civ_death$troop_deployment)
shapiro.test(civ_death$troop_deployment) #normal distribution

hist(civ_death$police_deployment)
shapiro.test(civ_death$police_deployment) #normal (although slim)


cor(civ_death$troop_deployment, civ_death$civdeath_best) 
# ^ weak-negative to moderate correlation

# is correlation significant? (α = 0.05; always select significance in advance to prevent p-hacking)
# H0 = correlation is not significant (p = 0.079 > 0.05)
cor.test(civ_death$troop_deployment, civ_death$civdeath_best) # not significant


cor(civ_death$police_deployment, civ_death$civdeath_best) 
# ^ weak-positive correlation

# is correlation significant? (α = 0.05)
# H0 = correlation is not significant (p = 0.294 > 0.05)
cor.test(civ_death$police_deployment, civ_death$civdeath_best) # not significant

# plotting correlation
cor1 <- cor(civ_death)
cor1
corrplot(cor1, method = "ellipse")

# running regression analysis
civ_death %>% 
  select(-year) -> civ_death 

glimpse(civ_death)

# here, DV and IV are both numeric but discrete variables
# DV goes before tilda, IV after
fit_t <- lm(civdeath_best~troop_deployment, data = civ_death)

summary(fit_t)

# regression model
# civdeath_best = 2848.82 - 0.0265 (troop_deployment) 
# 2nd part is not significant thus we can drop it

fit_p <- lm(civdeath_best~police_deployment, data = civ_death)

summary(fit_p)
# civdeath_best = 2587 + 0.0091 (police_deployment)


# How do DV and IV relate  
fit2 <- lm(civdeath_best~troop_deployment+police_deployment, data = civ_death)

summary(fit2)

# civdeath_best = 2775 - 0.0596(troop_deployment) + 0.295(police_deployment)
# correlation coefficient = strong

# do IVs interact producing an effect in synergy
fit3 <- lm(civdeath_best~troop_deployment*police_deployment, data = civ_death)

summary(fit3)

# civdeath_best = 3047 - 0.112(troop_deployment) + 0.025 (troop_deployment * police_deployment)


## Conditions for regression
# There are four conditions and tests that use a single par(mfrow = c(2,2)) and plot(vector) to check
# Variables must be linear (fit vs. residual plot)
# homoscedasticity (scale-location)
# normality of distribution (qqplot)
# leverage of outliers (residuals vs. leverage plot)
# non-multicollinearity 
par(mfrow = c(2,2))
plot(fit1)

plot(fit_t)

plot(fit_p)

# plotting linear regression
ggplot(civ_death) +
  aes(troop_deployment,civdeath_best) +
  geom_point() +
  geom_smooth(method = lm, color = 'blue') +
  labs(title = 'Civilian deaths vis-a-vis UN troop deployment in Cote D Ivoire',
       subtitle = 'The relationship was obseeved to be moderately negatively correlated',
       x = 'number of troops',
       y = 'civilian deaths') +
  theme_ipsum() -> mil_plot

ggplot(civ_death) +
  aes(police_deployment,civdeath_best) +
  geom_point() +
  geom_smooth(method = lm, color = 'red') +
  labs(title = 'Civilian deaths vis-a-vis UN Police deployment in Cote D Ivoire',
       subtitle = 'The relationship was obseeved to be weakly positively correlated',
       caption = "Source: UNDPO and Davies, Pettersson & Öberg. Organized violence 1989-2022?",
       x = 'number of UNPOL',
       y = 'civilian deaths') +
  theme_ipsum()-> pol_plot

mil_plot / pol_plot -> comb_plot
ggsave("C:\\Users\\HP\\Desktop\\comb_plot.jpeg", device = 'jpeg', plot = comb_plot, height = 10, width = 18, unit = c('in'))
### Testing for Multicollinearity 

libs <- c("caret","mlbench")

installed_libs <- libs %in% rownames(
  installed.packages()
)

if(any(installed_libs == F)){
  install.packages(
    libs[!installed_libs]
  )
}

lapply(
  libs,
  library,
  character.only = T
)

data(BostonHousing)
?BostonHousing

##Predict the variation in medv house price- median house price

str(BostonHousing)
## medv, our target variable (Y) is a continuous variable

## Regression problem

#(1) Tackle multi-collinearity, i.e. presence of highly correlated
#predictors (X)

#Dropping non-numeric response variable (Y) for calculating Multicollinearity
BostonHousing %>% 
  select_if(is.numeric) -> mtcy_num

# remove the target variable
mtcy_num %>% 
  select(-medv) -> mtcy

str(mtcy)  

cor(mtcy) -> mtcy_cor
corrplot(mtcy_cor, method = "ellipse", title = 'Coorelation b/w Variables')

#we remove numerical variables with correlation>0.7
high_cor <- findCorrelation(mtcy_cor, cutoff = 0.7, verbose = T)
high_cor_col <- colnames(mtcy_cor)[high_cor]
                # function(data set)[perform these tasks] 

print(high_cor_col)
# ^ indus, tax, nox, and dis are highly correlated predictor variables

# Now we drop the above 4 variables and make a new data set
new_mc <- mtcy_num[,-which(colnames(mtcy_num) %in% high_cor_col)]
# ^ the other way here is to use select(-x) and name variables manually

str(new_mc)

names(new_mc)

cor_new_mc <- cor(new_mc)
cor_new_mc

corrplot(cor_new_mc, method = 'color')

# Variance Inflation Factor (VIF)
# Since findCorrelation() cannot cutoff negative coefficients, thus we use VIF from 
# car package.
# usually if vif>10, we assume multicollinearity. In larger models with lots of 
# predictor variables, and where it's expected and tolerated, larger values may be allowed.

fit <- lm(medv~., data=mtcy_num) 
summary(fit)

vif(fit)
