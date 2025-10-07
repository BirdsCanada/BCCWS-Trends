library(tidyverse)
library(naturecounts)
library(scales)
library(ggpubr)
library(rando)
library(qpdf)

if(!dir.exists("Output/Plots/SpTrends")) {
  
  dir.create("Output/Plots/SpTrends", recursive = T)
  
}

trend <- "slope"

plot.areas <- "all"

# Specify "all" or list species codes.
# Note: for diet and family guilds, spell out full group name (e.g. "Benthivore", "Alcidae")
# but for migration guilds use "Local", "SDM", or "LDM".
# For combined species, use species codes as in GuildList.csv (e.g. Canada-Cackling Goose = CAGO)

# species <- c("AMCO", "BAGO", "CAGO", "Benthivore", "SDM", "AMWI", "COLO", "COMU", "GRSC", "Omnivore", "LDM", "Local", "WEGR", "Alcidae")

species <- "all"

user.palette <- `if`(plot.areas == "all", c(hue_pal()(4), "#000"), hue_pal()(length(plot.areas)))

# Gather all species data

sp.tax<-meta_species_taxonomy()
sp.tax<-sp.tax %>% dplyr::select(species_id, sort_order, english_name) %>%
  mutate(english_name = case_when(species_id == 261 ~  "Canada-Cackling Goose",
                                  species_id == 695 ~  "Greater-Lesser Scaup",
                                  species_id == 41201 ~ "Eared-Horned Grebe",
                                  .default = english_name))

#read indices and trends for a specific site

indices<-NULL
indices<-read.csv("Output/BCCWS_AnnualIndices.csv")

all.trend <- NULL
if (trend == "endpoint") {
  all.trends <- read.csv(file.path(out.dir, paste0(name, "_TrendsEndPoint.csv"))) 
    } else if (trend == "slope") {
  all.trends <- read.csv("Output/BCCWS_TrendsSlope.csv")
    }

##Prepare trends for plotting
#Remove any row with all NA values across all columns
all.trends <- all.trends[rowSums(is.na(all.trends)) < ncol(all.trends), ]
all.trends<-all.trends %>% dplyr::select(area_code, species_code, species_id, years, trnd, lower_ci, upper_ci, index_type, percent_change) 

all.trends<-left_join(all.trends, sp.tax, by=c("species_id"="species_id")) %>%
  mutate(sig = ifelse(lower_ci > 0 | upper_ci < 0, "*", ""))


ed.trnd <- all.trends %>% 
  mutate(sp.trnd = paste(round( trnd, digits = 2),  " (", round( lower_ci, digits = 2), ", ", round( upper_ci, digits = 2), ")", sig, sep = "")) %>% dplyr::select(-trnd, -upper_ci, -lower_ci, -sig)
ed.trnd <- ed.trnd %>% dplyr::select(english_name, species_code, sp.trnd, area_code)

#Prepare Indices for plotting
indices <- indices[rowSums(is.na(indices)) < ncol(indices), ]
index <- indices %>%
  dplyr::select(index, species_code, upper_ci, lower_ci, LOESS_index, year, season, area_code) 

plot.dat <- NULL
plot.dat <- full_join(index, ed.trnd, by = c("area_code", "species_code"), relationship = "many-to-many")

# plot.dat <- plot.dat%>%
#   mutate(area_code = ifelse(area_code == "Full Study Area", "Basin Wide", area_code))

plot.dat <- mutate(plot.dat, english_name = case_when(is.na(english_name) & species_code == "LDM" ~ "Long Distance Migrant",
                                                      is.na(english_name) & species_code == "SDM" ~ "Short Distance Migrant",
                                                      is.na(english_name) & !(species_code %in% c("LDM", "SDM")) ~ species_code,
                                                      .default = english_name))

title <- NULL
title <- plot.dat %>%
  select(english_name, area_code, sp.trnd) %>%
  distinct() %>%  # Remove duplicates
  group_by(english_name) %>%  # Process each species separately
  mutate(
    # Combine area code with trend for each line
    area_trend = paste0(area_code, " ", sp.trnd),
    # Create full title: species name + all area trends (line breaks)
    full_title = paste(
      english_name, 
      paste(area_trend, collapse = "\n"), 
      sep = "\n"
    )
  ) %>% 
  select(english_name, full_title) %>% 
  distinct() 

plot.dat <- left_join(plot.dat, title, by="english_name")

plot.dat$species <- plot.dat$full_title

plot.dat <- mutate(plot.dat, species_code =  case_when(species_code == "Large Gull" ~ "CAGU",
                                                       species_code == "Eared-Horned Grebe" ~ "EAGR",
                                                       species_code == "Canada-Cackling Goose" ~ "CAGO",
                                                       species_code == "Greater-Lesser Scaup" ~ "GRSC",
                                                       .default = species_code),
                   english_name = case_when(english_name == "Western/Clark's Grebe" ~ "Western-Clark's Grebe",
                                            .default = english_name),
                   species = gsub(pattern = "Western/Clark's", replacement = "Western-Clark's", species))

# Specify an order for plots to be printed in

guild.list <- read_csv("Data/GuildList.csv") %>%
  mutate(species_code = case_when(species_code == "TUSW" ~ "TRUS",
                                  .default = species_code))

family<-meta_species_taxonomy() %>% select(species_id, group_id, family_name, family_english_name)

guild.list <-left_join(guild.list, family, by=c("species_id"))

sp.order <- c(sort(unique(guild.list$species_code[guild.list$species_code %in% plot.dat$species_code])), "Local", "SDM", "LDM", sort(unique(guild.list$Diet)), sort(unique(guild.list$family_name[guild.list$family_name %in% plot.dat$english_name])))

sp.order <- c(sp.order[1:14], sp.order[16:32], "CAGU", sp.order[33:length(sp.order)])

# Specify order of area codes in legend

plot.dat$area_code <- factor(plot.dat$area_code, levels = `if`(plot.areas == "all", c("Combined Outer Coast", "Greater Vancouver", "Juan de Fuca", "Strait of Georgia", "BC"), plot.areas))

# Specify linewidths (for BC trends, make it thicker)

linwid <- `if`(plot.areas == "all", c(rep(0.8, times = 4), 2), rep(0.8, times = length(plot.areas)))

if("BC" %in% plot.areas) {
  
  plot.areas[which(plot.areas == "BC")] <- 2
  
}

# Open lists and loop!

sp.dat <- list()
sp.plot <- list()

if(unique(species == "all")) {
  
  if(!dir.exists("Output/Plots/SpTrends/Combined")) {
    
    dir.create("Output/Plots/SpTrends/Combined", recursive = T)
    
  }
  
  nsheets <- ceiling(length(unique(plot.dat$species_code))/6)
  
  for(i in sp.order) {
    
    sp.dat[[i]] <- `if`(plot.areas == "all", plot.dat[plot.dat$species_code == i,], plot.dat[plot.dat$species_code == i & plot.dat$area_code %in% plot.areas,])
    
    if(plot.areas == "all" | "BC" %in% plot.areas) {
      
      sp.plot[[i]] <- ggplot(sp.dat[[i]], 
                             aes(x = as.numeric(year), y = index, colour = area_code)) +
        scale_color_manual(values = user.palette, name = "Region") + 
        scale_discrete_manual("linewidth", values = linwid, guide = "none") +
        geom_point(size = 2, alpha = 0.7, na.rm = TRUE) +
        geom_smooth(aes(linewidth = area_code), se = FALSE, na.rm = TRUE) +
        geom_linerange(data = sp.dat[[i]] %>% filter(area_code == "BC"), aes(ymin = lower_ci, ymax = upper_ci), color = "black", alpha = 0.5, linewidth = 0.4) +
        scale_y_continuous(trans = 'log', labels = number_format(accuracy = 0.01)) +
        ggtitle(unique(sp.dat[[i]]$species)) +
        xlab("Year") +
        ylab("Annual Index (log)") +
        theme_classic() +
        theme(
          plot.title = element_text(hjust = 0.5),
          strip.text = element_text(face = "bold", color = "black"),
          strip.background = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1, color = "black"),
          legend.position = "bottom",
          legend.direction = "horizontal"
        )
      
    } else {
      
      sp.plot[[i]] <- ggplot(sp.dat[[i]], 
                             aes(x = as.numeric(year), y = index, colour = area_code)) +
        scale_color_manual(values = user.palette, name = "Region") + 
        scale_discrete_manual("linewidth", values = linwid, guide = "none") +
        geom_point(size = 2, alpha = 0.7, na.rm = TRUE) +
        geom_smooth(aes(linewidth = area_code), se = FALSE, na.rm = TRUE) +
        geom_linerange(data = sp.dat[[i]] %>% filter(area_code == "BC"), aes(ymin = lower_ci, ymax = upper_ci), color = "black", alpha = 0.5, linewidth = 0.4) +
        scale_y_continuous(trans = 'log', labels = number_format(accuracy = 0.01)) +
        ggtitle(unique(sp.dat[[i]]$species)) +
        xlab("Year") +
        ylab("Annual Index (log)") +
        theme_classic() +
        theme(
          plot.title = element_text(hjust = 0.5),
          strip.text = element_text(face = "bold", color = "black"),
          strip.background = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1, color = "black"),
          legend.position = "bottom",
          legend.direction = "horizontal"
        )
      
    }
    

    ggsave(paste0("./Output/Plots/SpTrends/", unique(sp.dat[[i]]$english_name), ".png"), sp.plot[[i]],
           width = 9, height = 6, units = "in")
    
    if(which(sp.order == i) %in% c(seq(from = 6, to = (nsheets-1)*6, by = 6))) {
      
      pl.indices <- seq(from = which(sp.order == i) - 5, to = which(sp.order == i), by = 1)
      
      pl.combined <- ggarrange(sp.plot[[pl.indices[1]]], sp.plot[[pl.indices[2]]],
                               sp.plot[[pl.indices[3]]], sp.plot[[pl.indices[4]]],
                               sp.plot[[pl.indices[5]]], sp.plot[[pl.indices[6]]],
                               ncol = 2, nrow = 3)
      
      ggsave(paste0("./Output/Plots/SpTrends/Combined/Sheet", which(seq(from = 6, to = (nsheets-1)*6, by = 6) == which(sp.order == i)), ".pdf"), pl.combined,
             width = 18, height = 18, units = "in")
      
    } else if(which(sp.order == i) == length(sp.order)) {
      
      pl.indices <- seq(from = (nsheets-1)*6 + 1, to = length(sp.order), by = 1)
      
      pl.combined <- ggarrange(plotlist = sp.plot[min(pl.indices):max(pl.indices)], ncol = 2, nrow = 3)
      
      ggsave(paste0("./Output/Plots/SpTrends/Combined/Sheet", nsheets, ".pdf"), pl.combined,
             width = 18, height = 18, units = "in")
      
    }
    
  }
  
  
} else {
  
  if(!dir.exists("Output/Plots/SpTrends/Combined") & length(species) > 1) {
    
    dir.create("Output/Plots/SpTrends/Combined", recursive = T)
    
  }
  
  nsheets <- ceiling(length(species)/6)
  
  sp.order <- sp.order[sp.order %in% species]
  
  pl.per.sheet <- ifelse(length(sp.order) >= 5, 6, ifelse(length(sp.order) %in% c(3, 4), 4, ifelse(length(sp.order) == 2, 2, NA)))
  
  for(i in sp.order) {
    
    sp.dat[[i]] <- `if`(plot.areas == "all", plot.dat[plot.dat$species_code == i,], plot.dat[plot.dat$species_code == i & plot.dat$area_code %in% plot.areas,])
    
    sp.plot[[i]] <- ggplot(sp.dat[[i]], 
                           aes(x = as.numeric(year), y = index, colour = area_code)) +
      scale_color_manual(values = user.palette, name = "Region") + 
      scale_discrete_manual("linewidth", values = linwid, guide = "none") +
      geom_point(size = 2, alpha = 0.7, na.rm = TRUE) +
      geom_smooth(aes(linewidth = area_code), se = FALSE, na.rm = TRUE) +
      geom_linerange(data = sp.dat[[i]] %>% filter(area_code == "BC"), aes(ymin = lower_ci, ymax = upper_ci), color = "black", alpha = 0.5, linewidth = 0.4) +
      scale_y_continuous(trans = 'log', labels = number_format(accuracy = 0.01)) +
      ggtitle(unique(sp.dat[[i]]$species)) +
      xlab("Year") +
      ylab("Annual Index (log)") +
      theme_classic() +
      theme(
        plot.title = element_text(hjust = 0.5),
        strip.text = element_text(face = "bold", color = "black"),
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, color = "black"),
        legend.position = "bottom",
        legend.direction = "horizontal"
      )
    
    ggsave(paste0("./Output/Plots/SpTrends/", unique(sp.dat[[i]]$english_name), ".png"), sp.plot[[i]],
           width = 9, height = 6, units = "in")
    
    if(which(sp.order == i) == length(sp.order) & length(sp.order) < 6 & length(sp.order) > 1) {
      
      pl.indices <- seq(from = 1, to = which(sp.order == i), by = 1)
      
      pl.combined <- ggarrange(plotlist = sp.plot[min(pl.indices):max(pl.indices)], ncol = 2, nrow = ifelse(pl.per.sheet == 5, 3, ifelse(pl.per.sheet, 4, 2, 1)))
      
      ggsave(paste0("./Output/Plots/SpTrends/Combined/Sheet", which(seq(from = 6, to = (nsheets-1)*6, by = 6) == which(sp.order == i)), ".pdf"), pl.combined,
             width = 18, height = ifelse(pl.per.sheet == 5, 18, ifelse(pl.per.sheet, 4, 12, 6)), units = "in")
      
    } else if(which(sp.order == i) %in% c(seq(from = 6, to = ifelse(is_wholenumber(length(sp.order)/6), nsheets*6, ifelse(nsheets == 1, 6, (nsheets-1)*6)), by = 6))) {
      
      pl.indices <- seq(from = which(sp.order == i) - 5, to = which(sp.order == i), by = 1)
      
      pl.combined <- ggarrange(sp.plot[[pl.indices[1]]], sp.plot[[pl.indices[2]]],
                               sp.plot[[pl.indices[3]]], sp.plot[[pl.indices[4]]],
                               sp.plot[[pl.indices[5]]], sp.plot[[pl.indices[6]]],
                               ncol = 2, nrow = 3)
      
      ggsave(paste0("./Output/Plots/SpTrends/Combined/Sheet", which(seq(from = 6, to = ifelse(is_wholenumber(length(sp.order)/6), nsheets*6, ifelse(nsheets == 1, 6, (nsheets-1)*6)), by = 6) == which(sp.order == i)), ".pdf"), pl.combined,
             width = 18, height = 18, units = "in")
      
    } else if(which(sp.order == i) == length(sp.order) & length(sp.order) > 6 & !is_wholenumber(length(sp.order)/6)) {
      
      pl.indices <- seq(from = (nsheets-1)*6 + 1, to = length(sp.order), by = 1)
      
      pl.combined <- ggarrange(plotlist = sp.plot[min(pl.indices):max(pl.indices)], ncol = 2, nrow = 3)
      
      ggsave(paste0("./Output/Plots/SpTrends/Combined/Sheet", nsheets, ".pdf"), pl.combined,
             width = 18, height = 18, units = "in")
      
    }
    
  }
  
}

if(dir.exists("Output/Plots/SpTrends/Combined")) {
  
  sheets <- paste0("Output/Plots/SpTrends/Combined/Sheet", 1:nsheets, ".pdf")
  
  pdf_combine(input = sheets,
              output = "Output/Plots/SpTrends/Combined/Master.pdf")
  
}
