library(tidyverse)
library(tidymodels)
library(lubridate)
library(ggcorrplot)
library(Factoshiny)
library(FactoMineR)
library(factoextra)
library(arules)
library(RColorBrewer)
library(viridis)
options(ggrepel.max.overlaps = Inf)



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


#############
#### MCA ####
#############

df_mca <- df %>%
  mutate(across(ends_with("?"), ~factor(.x, labels = c("Not lifted", "Lifted")))) %>%
  select(Sex, Equipment, Tested, Federation, ends_with("?")) %>%
  mutate(across(1:5, ~factor(.x))) 
#%>% sample_n(1000)

#result <- Factoshiny(df_mca)

res.MCA <- MCA(df_mca,graph=FALSE)
plot.MCA(res.MCA, choix='var')
plot.MCA(res.MCA,invisible= 'ind',selectMod= 'cos2 0.2',label =c('var'), graph.type="ggplot") + 
  geom_text_repel() + ggtitle("Mca factor map with cos2 >= 0.2")

#Values 
summary(res.MCA)

#Description of the axis

#measures the degree of association between variable categories and a particular axis
fviz_mca_var(res.MCA, alpha.var = "cos2", select.var = list(cos2 = 0.15),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())

#visualize the inertia kept by dimensions

fviz_screeplot(res.MCA, addlabels = TRUE, ylim = c(0, 45))

#quality of representation  
fviz_cos2(res.MCA, choice = "var", axes = 1:2)


#############
#### MFA ####
#############

df_mfa <- df %>%
  mutate(across(ends_with("?"), ~factor(.x, labels = c("Not lifted", "Lifted")))) %>%
  select(Sex, Equipment, Tested, Federation, ends_with("?"), where(is.numeric)) %>%
  mutate(across(1:5, ~factor(.x))) 

#We have to decide groups

# Lifter associated health and condition
# Weight tried
# Quality of the lifts

df_mfa <- df_mfa %>%
  select(Age, BodyweightKg,
         contains("WeightTried"),
         ends_with("?"))

res.mfa <- MFA(df_mfa, 
               group = c(2, 9, 9), 
               type = c("s", "s", "n"),
               name.group = c("Lifter condition", "Weight tried", "Quality of the lift"),
               graph = FALSE)

head(get_eigenvalue(res.mfa))

fviz_screeplot(res.mfa)

group_mfa <- get_mfa_var(res.mfa, "group")
fviz_mfa_var(res.mfa, "group")
fviz_contrib(res.mfa, "group", axes = 1)
fviz_contrib(res.mfa, "group", axes = 2)

get_mfa_var(res.mfa, "quanti.var")

fviz_mfa_var(res.mfa, "quanti.var", palette = "jco", 
             col.var.sup = "violet", repel = TRUE,
             geom = c("point", "text"), legend = "bottom")

fviz_contrib(res.mfa, choice = "quanti.var", axes = 1, top = 20,
             palette = "jco")
fviz_contrib(res.mfa, choice = "quanti.var", axes = 2, top = 20,
             palette = "jco")

fviz_mfa_var(res.mfa, "quanti.var", col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             col.var.sup = "violet", repel = TRUE,
             geom = c("point", "text"))


fviz_mfa_var(res.mfa, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             col.var.sup = "violet", repel = TRUE, geom = c("point", "text"))

fviz_cos2(res.mfa, choice = "quanti.var", axes = 1)

#The graph of partial axes shows the relationship between the principal axes 
#of the MFA and the ones obtained from analyzing each group using either a PCA 
#(for groups of continuous variables) or a MCA (for qualitative variables).

fviz_mfa_axes(res.mfa, geom = c("point", "text"), graph.type="ggplot", repel = T)


######Association Rules######

itemFrequencyPlot(transactions(df_mca), topN = 20,
                  col = brewer.pal(8, 'BuPu'),
                  main = 'Relative Item Frequency Plot',
                  type = "relative",
                  ylab = "Item Frequency (Relative)")

#############
### Eclat ###
#############

#https://www.datarlabs.com/post/market-basket-optimisation-using-association-rule-mining

df_eclat <- transactions(df_mca)

rules_eclat = eclat(data = df_eclat, 
              parameter = list(support = 0.6, minlen = 2))


inspect(sort(rules_eclat, by = 'support')[1:10])

#Tested -> may imply better tournament so better lifters


#############
## Apriori ##
#############

df_apriori <- transactions(df_mca)

#colors just because I can


rules_apriori <- apriori(df_apriori, parameter = list(supp = 0.1, conf = 0.8))

inspect(rules_apriori[1:20])

#Quick check
df_mca %>% select(contains("1")) %>% group_by(`LiftedSquat1Kg?`) %>% summarize(n = n())
df_mca %>% select(contains("1")) %>% group_by(`LiftedBench1Kg?`) %>% summarize(n = n())
df_mca %>% select(contains("1")) %>% group_by(`LiftedDeadlift1Kg?`) %>% summarize(n = n())
df_mca %>% select(contains("2")) %>% group_by(`LiftedSquat2Kg?`) %>% summarize(n = n())
df_mca %>% select(contains("2")) %>% group_by(`LiftedBench2Kg?`) %>% summarize(n = n())
df_mca %>% select(contains("2")) %>% group_by(`LiftedDeadlift2Kg?`) %>% summarize(n = n())
df_mca %>% select(contains("3")) %>% group_by(`LiftedSquat3Kg?`) %>% summarize(n = n())
df_mca %>% select(contains("3")) %>% group_by(`LiftedBench3Kg?`) %>% summarize(n = n())
df_mca %>% select(contains("3")) %>% group_by(`LiftedDeadlift3Kg?`) %>% summarize(n = n())


######Association Rules ENDS######





###### Swag plots ######

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


