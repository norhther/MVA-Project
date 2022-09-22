library(tidyverse)
library(lubridate)

df <- read_csv("/Users/norhther/Desktop/openpowerlifting-2022-09-19-03cdaaf3.csv")

#Looking good
df %>%
  count(Event) %>%
  top_n(20) %>%
  mutate(Event = fct_reorder(Event, n))

df %>%
  count(Sex) %>%
  top_n(20) %>%
  mutate(Sex = fct_reorder(Sex, n))

#Mx? going to remove that, only 8

df <- df %>%
  filter(Sex != "Mx")

df %>%
  count(Equipment) %>%
  top_n(20) %>%
  mutate(Equipment = fct_reorder(Equipment, n))

#we could work something with "wraps" tho

#removing errors

df <- df %>%
 mutate(Age = ifelse(Age <= 4, NA, Age))


df %>%
  ggplot(aes(x = Age)) + geom_histogram()

df %>%
  ggplot(aes(x = Age)) + geom_boxplot()

df %>%
  filter(Age == max(df$Age, na.rm = T))

df %>%
  count(AgeClass)

sum(is.na(df$Age))

#we cant impute AgeClass using age in any observation
df %>%
  filter(is.na(AgeClass) & !is.na(Age))

#Probably too much correlated, better to remove
df %>%
  count(BirthYearClass) 

#This is a mess and is not giving any info
df %>%
  count(Division) 


df %>%
  ggplot(aes(x = BodyweightKg)) + geom_histogram()

df %>%
  ggplot(aes(x = BodyweightKg)) + geom_boxplot()

#plausible
df %>%
  filter(BodyweightKg < 40 && Age > 15)

df %>%
  select(BodyweightKg) %>%
  filter(BodyweightKg > 200) %>%
  arrange(desc(BodyweightKg))

#Weightclass same info

df %>%
  ggplot(aes(x = Squat1Kg)) + geom_boxplot()

#Lets check why negative
df %>%
  filter(Squat1Kg == min(df$Squat1Kg, na.rm = T))

#Seems that if the values are negative, we got a DQ!
#Same with the others.

df %>%
  filter(Squat1Kg < 0)


df %>%
  filter(TotalKg < 0)


df %>%
  count(Place) %>%
  arrange(desc(n))

df <- df %>%
  mutate(Place = fct_lump_n(Place, 11))

df %>%
  count(Place) %>%
  arrange(desc(n))


df %>%
  ggplot(aes(x = Dots)) + geom_boxplot()

df %>%
  ggplot(aes(x = Wilks)) + geom_boxplot()

df %>%
  ggplot(aes(x = Glossbrenner)) + geom_boxplot()

df %>%
  ggplot(aes(x = Goodlift)) + geom_boxplot()

df %>%
  count(Tested)

df %>%
  count(Country) %>%
  arrange(desc(n))


df <- df %>%
  mutate(Country = fct_lump_n(Country, 20))

#All na, remove
df %>%
  count(State) %>%
  arrange(desc(n))


# parentFederation has is only IPF, remove it
df %>%
  count(ParentFederation)

df <- df %>%
  select(-ParentFederation)

df %>%
  count(Federation) %>%
  top_n(20) %>%
  mutate(Federation = fct_reorder(Federation, n)) %>%
  ggplot(aes(x = Federation, y = n)) + geom_col() + coord_flip()
# we have 95 factors! lump them together to a max of 40 and Other


df <- df %>% 
  mutate(Federation = fct_lump_n(Federation, 40))


df %>%
  filter(is.na(Country) && !is.na(MeetCountry))

df %>%
  filter(!is.na(Country) && is.na(MeetCountry))

df <- df %>%
  mutate(MeetCountry = fct_lump_n(MeetCountry, 20))


# maybe remove meetstate, meettown and meetname

df %>%
  count(MeetName) %>%
  arrange(desc(n))

df <- df %>%
  filter(is.na(Squat4Kg), is.na(Bench4Kg), is.na(Deadlift4Kg))

# normally we only have 3 lifts
df <- df %>%
  select(Name, Sex, Event, Equipment, Age, BodyweightKg, 
         Squat1Kg, Squat2Kg, Squat3Kg,
         Bench1Kg, Bench2Kg, Bench3Kg,
         Deadlift1Kg, Deadlift2Kg, Deadlift3Kg,
         TotalKg, Place, Dots, Wilks, Glossbrenner,
         Goodlift, Tested, Country, Federation, Date)

df <- df %>%
  mutate(Sex = as_factor(Sex),
         Event = as_factor(Event),
         Equipment = as_factor(Equipment),
         Tested = as_factor(Tested),
         Date = as_date(Date)) 


##################################################
##################################################

library(ggcorrplot)

numeric_cols <- df %>%
  select_if(is.numeric)

round(cor(numeric_cols, use = "complete.obs"), 1) %>%
  ggcorrplot()

df <- df %>%
  select(-Glossbrenner, -Wilks, -Dots)

numeric_cols <- df %>%
  select_if(is.numeric)

#better now

#############
#### PCA ####
#############

library(tidymodels)

pca_rec <- recipe(~., data = numeric_cols) %>%
  step_normalize(all_predictors()) %>%
  step_naomit(all_predictors()) %>%
  step_pca(all_predictors()) 

pca_prep <- prep(pca_rec)

tidied_pca <- tidy(pca_prep, 3)

tidied_pca %>%
  filter(component %in% paste0("PC", 1:9)) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(value, terms, fill = terms)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~component, nrow = 1) +
  labs(y = NULL)

summary(pca_prep$steps[[3]]$res)

juice(pca_prep) %>%
  ggplot(aes(PC1, PC2)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(color = NULL)

pca_cumm <- as_data_frame(summary(pca_prep$steps[[3]]$res)$importance)[3, ] %>%
  gather()

pca_cumm %>%
  mutate(key = fct_inorder(key)) %>%
  ggplot(aes(x = key, y = value)) + geom_col(fill = "#91E5F8") 


##Plotting

df %>%
  ggplot(aes(x = TotalKg, fill = Sex)) + geom_boxplot() + 
  facet_wrap(~Sex) + scale_fill_viridis_d() 


df %>%
  filter(!is.na(Country)) %>%
  ggplot(aes(x = TotalKg, fill = Country)) + 
  geom_boxplot(show.legend = F) + 
  facet_wrap(~Country) +
  scale_fill_viridis_d() 


df %>%
  group_by(Date) %>%
  summarize(m = median(TotalKg)) %>%
  ggplot(aes(x = Date, y = m)) + geom_point() + geom_smooth()


df %>%
  mutate(Tested = as.numeric(Tested)) %>%
  mutate(Tested = replace_na(Tested, 0)) %>%
  group_by(Date) %>%
  summarize(`number of tested` = sum(Tested)) %>%
  ggplot(aes(x = Date, y = `number of tested`)) + geom_point(color = "#91E5F8", alpha = 0.8) + 
  geom_smooth(color = "#0000F9")


round(cor(numeric_cols, use = "complete.obs"), 1) %>%
  ggcorrplot(ggtheme = ggplot2::theme_gray,
             colors = c("#6D9EC1", "white", "#E46726"),
             lab = T, type = "lower")


df_aux <- df %>%
  filter_at(vars(colnames(df)[7:15]), all_vars(!is.na(.)))


sapply(df_aux, function(x) sum(is.na(x))/nrow(df_aux))

df_aux %>%
  count(Federation) %>%
  arrange(desc(n)) 

df_aux %>%
  count(Country) %>%
  arrange(desc(n)) 

df_aux %>%
  group_by(Country) %>%
  summarize(max(TotalKg, na.rm = T))



##World map over total df
library(viridis)
world <- map_data("world")

world %>%
  merge(df %>%
          group_by(Country) %>%
          summarize(n = n()) %>%
          mutate(country = Country), 
        by.x = "region", by.y = "country", all.x = T) %>%
  arrange(group, order) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = n)) + 
  geom_polygon(color = "white", size = 0.2) +
  scale_fill_viridis(option="magma", na.value = "gray90") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank())


world %>%
  merge(df %>%
          group_by(Country) %>%
          summarize(n = mean(TotalKg, na.rm = T)) %>%
          mutate(country = Country), 
        by.x = "region", by.y = "country", all.x = T) %>%
  arrange(group, order) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = n)) + 
  geom_polygon(color = "white", size = 0.2) +
  scale_fill_viridis(option="mako", na.value = "gray90") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank()) + 
  ggtitle("Mean KG lifted by country")


world %>%
  merge(df %>%
          group_by(Country) %>%
          summarize(n = mean(Age, na.rm = T)) %>%
          mutate(country = Country), 
        by.x = "region", by.y = "country", all.x = T) %>%
  arrange(group, order) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = n)) + 
  geom_polygon(color = "white", size = 0.2) +
  scale_fill_viridis(option="mako", na.value = "gray90") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank()) + 
  ggtitle("Mean Age by country")

 
# maybe number of tested divided by total 


colnames(df)
write_csv(df_aux, "/Users/norhther/Desktop/pseudo-preprocessed.csv")
library(tidyverse)

sapply(df %>% select_if(is.numeric), min, na.rm = T)
sapply(df %>% select_if(is.numeric), max, na.rm = T)
