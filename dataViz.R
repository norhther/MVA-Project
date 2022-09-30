library(tidyverse)
library(tidymodels)
library(lubridate)
library(ggcorrplot)
library(Factoshiny)

df <- read_csv("/Users/norhther/Documents/GitHub/MVA-Project/df_preprocessed.csv")

numeric_cols <- df %>%
  select_if(is.numeric)

cols_zero_var <- df %>% 
  select(colnames(numeric_cols)[1:13])


####Corr plot####

round(cor(cols_zero_var, use = "complete.obs"), 1) %>%
  ggcorrplot()

#############
#### PCA ####
#############

pca_rec <- recipe(~., data = cols_zero_var) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors()) 

pca_prep <- prep(pca_rec)

tidied_pca <- tidy(pca_prep, 2)

tidied_pca %>%
  filter(component %in% paste0("PC", 1:9)) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(value, terms, fill = terms)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~component, nrow = 1) +
  labs(y = NULL)

summary(pca_prep$steps[[2]]$res)

juice(pca_prep) %>%
  ggplot(aes(PC1, PC2)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(color = NULL)

pca_cumm <- as_data_frame(summary(pca_prep$steps[[]]$res)$importance)[3, ] %>%
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
  group_by(Tested) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = Tested, y = n)) + geom_col()

round(cor(numeric_cols, use = "complete.obs"), 1) %>%
  ggcorrplot(ggtheme = ggplot2::theme_gray,
             colors = c("#6D9EC1", "white", "#E46726"),
             lab = T, type = "lower")



##World map over total df
library(viridis)
world <- map_data("world")


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




#############
#### MCA ####
#############

df_mca <- df %>%
  mutate(across(ends_with("?"), ~factor(.x, labels = c("Not lifted", "Lifted")))) %>%
  select(Sex, Equipment, Tested, Federation, ends_with("?")) %>%
  mutate(across(1:5, ~factor(.x))) 
#%>% sample_n(1000)

#result <- Factoshiny(df_mca)

###...
library(ggrepel)
options(ggrepel.max.overlaps = Inf)

res.MCA <- MCA(df_mca,graph=FALSE)
plot.MCA(res.MCA, choix='var')
plot.MCA(res.MCA,invisible= 'ind',selectMod= 'cos2 0.2',label =c('var'), grah.type="ggplot") + 
  geom_text_repel() + ggtitle("Mca factor map with cos2 >= 0.2")

#Values 
summary(res.MCA)

#Description of the axis
#dimdesc(res.MCA)
