guild <- tolower(guild)
trend <- "slope"

sp.tax<-meta_species_taxonomy()
sp.tax<-sp.tax %>% dplyr::select(species_id, sort_order, english_name)

#read indices and trends for a specific site

indices<-NULL
indices<-read.csv(paste(out.dir, name, "_AnnualIndices_iCAR.csv", sep=""))

all.trend <- NULL
if (trend == "endpoint") {
  all.trends <- read.csv(file.path(out.dir, paste0(name, "_TrendsEndPoint_iCAR.csv"))) 
    } else if (trend == "slope") {
  all.trends <- read.csv(file.path(out.dir, paste0(name, "_TrendsSlope_iCAR.csv"))) 
    }

##Prepare trends for plotting
#Remove any row with all NA values across all columns
all.trends <- all.trends[rowSums(is.na(all.trends)) < ncol(all.trends), ]
all.trends<-all.trends %>% dplyr::select(area_code, species_code, species_id, years, trnd, lower_ci, upper_ci, index_type, percent_change) 

if(guild == "no"){
all.trends<-left_join(all.trends, sp.tax, by=c("species_id"="species_id"))
} 

if(guild == "yes"){
all.trends$english_name<-all.trends$species_code  
}

ed.trnd <- all.trends %>% 
  mutate(sp.trnd = paste(round( trnd, digits = 2),  " (", round( lower_ci, digits = 2), ", ", round( upper_ci, digits = 2), ")", sep = "")) %>% dplyr::select(-trnd, -upper_ci, -lower_ci)
ed.trnd <- ed.trnd %>% dplyr::select(english_name, species_code, sp.trnd, area_code)

#Prepare Indices for plotting
indices <- indices[rowSums(is.na(indices)) < ncol(indices), ]
index <- indices %>%
  dplyr::select(index, species_code, upper_ci, lower_ci, LOESS_index, year, season, area_code) 

plot.dat <- NULL
plot.dat <- full_join(index, ed.trnd, by = c("area_code", "species_code"), relationship = "many-to-many")

# plot.dat <- plot.dat%>%
#   mutate(area_code = ifelse(area_code == "Full Study Area", "Basin Wide", area_code))

plot.dat<-plot.dat %>% filter(area_code != "Full Study Area")


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
sp.list2 <- unique(plot.dat$full_title) 

# Calculate number of batches needed
num_batches <- ceiling(length(sp.list2)/6)

# Create PDF outside of loop
pdf(paste(plot.dir, name, "_IndexPlot_", model, ".pdf", sep=""),
    height = 10, width = 8, paper = "letter")

if(guild=="yes"){
  nplot<-4
}else{
nplot<-6
}

for(k in 1:num_batches) {
  start_idx <- (k-1)*nplot + 1
  end_idx <- min(k*nplot, length(sp.list2))
  current_sp_trnd <- sp.list2[start_idx:end_idx]
  
  # Pad with dummy titles if needed
  num_real <- length(current_sp_trnd)
  if (num_real < nplot) {
    dummy_titles <- paste0("dummy_", seq_len(nplot - num_real))
    facet_levels <- c(current_sp_trnd, dummy_titles)
  } else {
    facet_levels <- current_sp_trnd
  }
  
  # Subset real data
  dat_sub <- subset(plot.dat, species %in% current_sp_trnd)
  
  # Add dummy rows if needed
  if (num_real < nplot) {
    dummy <- dat_sub[rep(1, nplot - num_real), ] # copy structure
    dummy[] <- NA
    dummy$full_title <- dummy_titles
    dummy$species <- dummy_titles
    dat_sub <- rbind(dat_sub, dummy)
  }
  
  # Set full_title as a factor with all levels (real and dummy)
  dat_sub$full_title <- factor(dat_sub$full_title, levels = facet_levels)
  
  # Only plot if there is at least one real plot
  if (any(!grepl("^dummy_", dat_sub$full_title))) {
    current_plot <- ggplot(dat_sub, 
                           aes(x = as.numeric(year), y = index, colour = area_code)) +
      facet_wrap(~ full_title, ncol = 2, nrow = ceiling(nplot/2), scales = "free_y", as.table = TRUE) +
      geom_pointrange(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.7, na.rm = TRUE) +
      geom_line(aes(y = LOESS_index), linewidth = 0.8, na.rm = TRUE) +
      scale_color_grey(start = 0, end = 0.8, name = "Region") +
      scale_y_continuous(trans = 'log10') +
      xlab("Year") +
      ylab("Annual Index (log)") +
      theme_classic() +
      theme(
        strip.text = element_text(face = "bold", color = "black"),
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, color = "black"),
        legend.position = "bottom",
        legend.direction = "horizontal"
      )
    print(current_plot)
  }
}

while (!is.null(dev.list())) dev.off()
