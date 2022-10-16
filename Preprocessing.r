library(tidyverse)
library(lubridate)
library(ggpubr)


df <- read_csv("C:\\Users\\omarl\\OneDrive\\Escritorio\\MVA-Project\\openipf-2022-09-14-30eb84bf.csv")

df <- df %>%
  filter(Sex != "Mx")

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

boxplot_age <- df %>%
  ggplot(aes(x = Age)) + geom_boxplot() + 
  ggtitle("Boxplot of Age after preprocessing")

df <- df %>%
  mutate(Age = ifelse(Age <= 4, NA, Age))


df %>%
  ggplot(aes(x = Age)) + geom_histogram()



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

#Seems that if the values are negative, we got a bad lift!
#Same with the others.

df %>%
  filter(Squat1Kg < 0)


df %>%
  filter(TotalKg < 0)

place_counted <- df %>%
  count(Place) %>%
  top_n(30) %>%
  mutate(Place = fct_reorder(Place, -n)) %>%
  ggplot(aes(x = Place, y = n)) + geom_col() + 
  ggtitle("Sorted count of top 30 Place categorical values after preprocessing")

ggarrange(place_counted, boxplot_age)

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

#Remove if we are not going to do anything with country, this
#commented may deliver cool plots
#df <- df %>%
#  mutate(Country = fct_lump_n(Country, 20))

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

#Idem, leave meetcountry
#df <- df %>%
#  mutate(MeetCountry = fct_lump_n(MeetCountry, 20))


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
         Goodlift, Tested, Country, Federation, Date, MeetCountry, 
         MeetName)

df <- df %>%
  mutate(Sex = as_factor(Sex),
         Event = as_factor(Event),
         Equipment = as_factor(Equipment),
         Tested = as_factor(Tested),
         Date = as_date(Date),
         MeetCountry = as_factor(MeetCountry),
         MeetName = as_factor(MeetName))


##################################################
##################################################


library(ggcorrplot)
numeric_cols <- df %>%
  select_if(is.numeric)

round(cor(numeric_cols, use = "complete.obs"), 1) %>%
  ggcorrplot(ggtheme = ggplot2::theme_gray,
             colors = c("#6D9EC1", "white", "#E46726"),
             lab = T, type = "lower")
#After some viz, we can see that glossbrenner, wilks and dots are highly correlated
#because is a linear combination of the lifts.

df <- df %>%
  select(-Glossbrenner, -Wilks, -Dots)


#Remove variables of lifts with NAs, bc some records doesnt have them registered,
#only total kg or not even those...

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
  summarize(max = max(TotalKg, na.rm = T)) %>%
  arrange(desc(max))


colnames(df)

sapply(df %>% select_if(is.numeric), min, na.rm = T)
sapply(df %>% select_if(is.numeric), max, na.rm = T)


library(stringr)
paste0("Lifted", str_sub(colnames(df_aux[,7:15]), end=-3), "?")


df_aux <- df_aux %>%
  mutate(across(7:15, abs, .names = "WeightTried{.col}"),
         across(7:15, ~ +(.x >0), .names = "Lifted{.col}?"))


df_aux <- df_aux %>%
  mutate(Tested = as.character(Tested),
         Tested = replace_na(Tested, "No"),
         Tested = as_factor(Tested))


sapply(df_aux, function(x) sum(is.na(x))/nrow(df_aux))

library(tidymodels)
preprocess_recipe <- recipe(~., data = df_aux) %>%
  step_impute_knn(Age, Goodlift, BodyweightKg, TotalKg, Country, 
                  neighbors = floor(sqrt(ncol(df_aux))))



#apply a knn with k = sqrt(n_obs) -> good practical results
# Cite as 
#
#Devroye, L., Györfi, L., & Lugosi, G. (1996). A Probabilistic Theory of Pattern Recognition. Springer. https://doi.org/10.1007/978-1-4612-0711-5. See chapters 5, 6, 11, 26.
#
#Stone, C. J. (1977). Consistent nonparametric regression. The Annals of Statistics, 5(4) 595 - 645. https://doi.org/10.1214/aos/1176343886

#Chaudhuri, K., & Dasgupta, S. (2014). Rates of convergence for nearest neighbour classification. Advances in Neural Information Processing Systems 27, NIPS 2014. https://papers.nips.cc/paper/2014/hash/db957c626a8cd7a27231adfbf51e20eb-Abstract.html


preprocess_rec_prep <- prep(preprocess_recipe, df_aux)

df_aux_preprocessed <- juice(preprocess_rec_prep)

write_csv(df_aux_preprocessed, "C:\\Users\\omarl\\OneDrive\\Escritorio\\MVA-Project\\df_preprocessed.csv")

