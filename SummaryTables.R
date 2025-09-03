#Summary tables for Manuscript
library(ggplot2)
library(dplyr)


#Load trends tables

# species<-read.csv("Output/SalishSea_Species_TrendsSlope_iCAR.csv")
# diet<-read.csv("Output/SalishSea_Diet_TrendsSlope_iCAR.csv")
# migration<-read.csv("Output/SalishSea_Migration_TrendsSlope_iCAR.csv")
# family<-read.csv("Output/SalishSea_Family_TrendsSlope_iCAR.csv")

#Load trends tables

species<-read.csv("Output/Watershed_Species_TrendsSlope_iCAR.csv")
diet<-read.csv("Output/Watershed_Diet_TrendsSlope_iCAR.csv")
migration<-read.csv("Output/Watershed_Migration_TrendsSlope_iCAR.csv")
family<-read.csv("Output/Watershed_Family_TrendsSlope_iCAR.csv")

table <- bind_rows(species, diet, migration, family)
table <- table %>% select(species_id, species_code, area_code, trnd, lower_ci, upper_ci) %>% drop_na(area_code)

sp<-meta_species_taxonomy()
sp<-sp %>% select(species_id, english_name, scientific_name)
table<-table %>% left_join(sp, by=c("species_id"))
table <- table %>% mutate(english_name = ifelse(is.na(english_name), species_code, english_name))

table <- table %>%
  mutate(sig = ifelse(lower_ci > 0 | upper_ci < 0, "*", ""))

#Combine the trend and CI into one column
table <- table %>%
  mutate(trend_ci = paste0(round(trnd, 3), " (", round(lower_ci, 3), ", ", round(upper_ci, 3), ")", sig))

table_wide <- table %>%
  pivot_wider(
    id_cols = c(species_id, english_name, scientific_name),
    names_from = area_code,
    values_from = trend_ci,
    names_glue = "{area_code}_trend_ci"
  ) %>%
  select(-species_id)

table <- table %>%
  mutate(sig = ifelse(lower_ci > 0 | upper_ci < 0, "*", ""))

table <- table %>%
  mutate(
    sigsign = case_when(
      lower_ci > 0 & upper_ci > 0 ~ "+",
      lower_ci < 0 & upper_ci < 0 ~ "-",
      TRUE ~ ""
    )
  )

table<-table %>% mutate(area_code = ifelse(area_code=="Full Study Area", "BCR-5", area_code))

# Pivot wider for each variable separately
table_wide <- table %>%
  pivot_wider(
    id_cols = c(species_id, english_name, scientific_name),
    names_from = area_code,
    values_from = c(trnd, lower_ci, upper_ci, sig),
    names_glue = "{area_code}_{.value}"
  ) %>%
  select(-species_id)


###Create plots 

# Filter to regions of interest (adjust as needed)
region_levels <- c("British Columbia", "Washington", "BCR-5")
trend_tab <- table %>% filter(area_code %in% region_levels)
trend_plot <- list()

ci_bands <- trend_tab %>%
  select(english_name, area_code, lower_ci, upper_ci)

region_colors <- c(
  "British Columbia" = "#A6CEE3",
  "Washington" = "#B2DF8A",
  "BCR-5" = "#FB9A99"
)

  region_trend_plot <- ggplot(
    data = trend_tab, 
    mapping = aes(x = trnd, y = english_name, shape = area_code)
  ) +
    # Add colored CI bands
    geom_rect(
      data = ci_bands,
      aes(
        xmin = lower_ci, xmax = upper_ci,
        ymin = as.numeric(factor(english_name)) - 0.4,
        ymax = as.numeric(factor(english_name)) + 0.4,
        fill = area_code
      ),
      alpha = 0.16,
      inherit.aes = FALSE
    ) +
  geom_point(position = position_dodge(width = 1), size = 1.1) +
  geom_errorbar(
    mapping = aes(x = trnd, xmax = upper_ci, xmin = lower_ci), 
    position = position_dodge(width = 1), linewidth = 0.25
  ) +
  geom_col(
    data = trend_tab %>% group_by(english_name) %>% 
      summarize(Max = max(upper_ci, na.rm = TRUE)) %>% 
      mutate(Max = ifelse(Max < 0, NA, Max)), 
    mapping = aes(x = Max, y = english_name), alpha = 0.1, inherit.aes = FALSE
  ) +
  geom_col(
    data = trend_tab %>% group_by(english_name) %>% 
      summarize(Min = min(lower_ci, na.rm = TRUE)) %>% 
      mutate(Min = ifelse(Min > 0, NA, Min)), 
    mapping = aes(x = Min, y = english_name), alpha = 0.1, inherit.aes = FALSE
  ) +
  geom_text(
    aes(x = upper_ci, y = english_name, colour = area_code, label = ifelse(upper_ci < 0, "", sig)),
    position = position_dodge(width = 1), vjust = 0.79, hjust = -0.35, size = 2.75, inherit.aes = FALSE, show.legend = FALSE
  ) +
  geom_text(
    aes(x = lower_ci, y = english_name, colour = area_code, label = ifelse(lower_ci > 0, "", sig)),
    position = position_dodge(width = 1), vjust = 0.79, hjust = 1.2, size = 2.75, inherit.aes = FALSE, show.legend = FALSE
  ) +
  geom_vline(mapping = aes(xintercept = 0), linetype = "dashed", alpha = 0.5) +
  theme_classic() +
  guides(shape = guide_legend(override.aes = list(size=1.5))) +
  theme(
    legend.position = "top", legend.title = element_blank(),
    legend.text = element_text(size = 8), axis.title = element_text(size = 8),
    axis.text = element_text(size = 6), legend.margin = margin(t = 0, b = -0.25, unit = "cm"),
    plot.margin = margin(t = 0.25, r = 0.25, b = 0.25, l = 0.25, unit = "cm")
  ) +
  ylab("Species") +
  xlab("Mean Trend (% change per year)") +
  scale_x_continuous(limits = c(-20, 20)) +
  scale_y_discrete(expand = expansion(mult = c(0.03, 0))) +
  scale_shape_manual(values = c(18, 16, 17), breaks = region_levels) +
  scale_colour_manual(values = c("black", "black", "black"))

  plot(region_trend_plot)
