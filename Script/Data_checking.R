# First script to clean the volume data
# Created by BJ the 08/04/2026


# Libraries

library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)
library(plotly)


# Import and cleaning data ####

df <- read.csv2("Data-raw/Raw_data_brain_bagoti_BJ.csv",
                sep = ";", dec = ".", stringsAsFactors = TRUE) %>%
  as_tibble()

# change variables type

df$Ind_anon <- as.factor(df$Ind_anon)

levels(df$Brain_region)

# Remove Exterior
df <- df %>% 
  filter(Brain_region != "Exterior") %>% 
  droplevels()

levels(df$Brain_region)


# Corelation matrix ####

# Pivot to wide format 


wide_df <- df %>%
  select(Ind_anon, Indiv_ID, Group, Brain_region, Volume) %>%
  group_by(Ind_anon, Indiv_ID, Group) %>% 
  pivot_wider(names_from = Brain_region, values_from = Volume) %>% 
  ungroup()


wide_df_nonames <- wide_df %>% select(- c(Ind_anon, Indiv_ID, Group))

# Correlation matrix

ggpairs(wide_df_nonames,
        upper = list(continuous = wrap("cor", size = 4)), # Correlation numbers
        diag = list(continuous = wrap("densityDiag")),   # Distributions
        lower = list(continuous = wrap("points", alpha = 0.5, size = 0.5))) + # Scatterplots
  theme_bw() + 
  labs(title = "Brain Region Volume Relationships")

# Histogram faceted byu regions

ggplot(df, aes(x = log(Volume))) +
  geom_histogram() +
  facet_wrap(~Brain_region, scales = "free") +
  theme_minimal() +
  labs(title = "Distribution du volume par région cérébrale",
       x = "Volume",
       y = "Nombre d'individus")



