# First script to clean the volume data
# Created by BJ the 08/04/2026


# Load necessary libraries ####

library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally) # Nice correlation matrices plot
library(plotly) # For interactive plot with ind name, might remove
library(ggrepel) # For nice Ind labelling on graphs
library(ggpubr) # Automatic cor computation on graphs
library(purrr) # 
library(patchwork) # To have mutliple graphs



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


# Correlation matrices of volumes across brain regions ####

# Pivot to wide format 

wide_df <- df %>%
  select(Ind_anon, Indiv_ID, Group, Brain_region, Volume) %>%
  group_by(Ind_anon, Indiv_ID, Group) %>% 
  pivot_wider(names_from = Brain_region, values_from = Volume) %>% 
  ungroup()


wide_df_nonames <- wide_df %>% select(- c(Ind_anon, Indiv_ID, Group))




# Create the "plot_brain_correlations" function to correlate volume across regions #

plot_brain_correlations <- function(data, regions) {
  
  # 1. Generate unique pairs of regions (e.g., A vs B, A vs C)
  pairs <- combn(regions, 2, simplify = FALSE)
  
  # 2. Map through the pairs to create a list of plots
  plot_list <- map(pairs, function(p) {
    x_var <- p[1]
    y_var <- p[2]
    
    ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]], color = Ind_anon)) +
      geom_point() +
      # stat_cor with NPC coordinates: 0 is start, 1 is end of axis
      # size is reduced, and label is fixed at x=0.3, y=0.7
      stat_cor(
        method = "pearson", 
        color = "black", 
        label.x.npc = 0.05, 
        label.y.npc = 0.95, 
        size = 5
      ) + 
      # Global linear regression ignoring individual color grouping
      geom_smooth(
        method = "lm", 
        color = "black", 
        se = TRUE, 
        inherit.aes = FALSE, 
        aes(x = .data[[x_var]], y = .data[[y_var]])
      ) +
      # Individual labels
      geom_text_repel(aes(label = Ind_anon), size = 5) +
      # Theme settings
      theme_bw(base_size = 25) +
      theme(
        aspect.ratio = 1, 
        legend.position = "none",
        axis.text = element_text(size = 15) # Specific size for axis numbers
      )
  })
  
  # 3. Combine all plots into one window
  wrap_plots(plot_list)
}


# Grouping brain regions

colnames(wide_df)

smart_regions <- c("MB_lobe", "MB_lobe2", "Calyx", "RCB")
optic_regions <- c("Medula", "Lamina", "Lobula", "RCB")
navig_regions <- c("Central_complex_body", "PB", "Noduli", "RCB")


plot_brain_correlations(wide_df, smart_regions)
plot_brain_correlations(wide_df, optic_regions)
plot_brain_correlations(wide_df, navig_regions)



