
# library(tidyverse)
# library(GGally)

#  var_DF2.RDS is created by fig2.R and holds cumulative infections etc
df_import <- readRDS("var_df2.RDS") 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data preparation 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# There are multiple entries per hospital. Check whether the variables of interest 
# differ between these rows 

df_check <- df_import %>% 
  group_by(id) %>%
  summarize(
    defnosocomialmin = min(day15per100beds),
    defnosocomialmax = max(day15per100beds),
    probnosocomialmin = min(day8to14per100beds),
    probnosocomialmax = max(day8to14per100beds),
    hcwcasesmin = min(hcw_casesper100beds), # note - also available per 100 staff
    hcwcasesmax = max(hcw_casesper100beds),
    communitycasesmin = min(cumul_ca_casesper100beds),
    communitycasesmax = max(cumul_ca_casesper100beds),
    occupancymin = min(occupancy),
    occupancymax = max(occupancy),
    percentpre1965min = min(percent_pre1965),
    percentpre1965max = max(percent_pre1965),
    singleroomsmin = min(proportion_single_rooms),
    singleroomsmax = max(proportion_single_rooms),
    volumeperbedmin = min(volume_per_bed),
    volumeperbedmax = max(volume_per_bed), 
    hcwcasesmin_perstaff = min(hcw_casesper100staff),
    hcwcasesmax_perstaff = max(hcw_casesper100staff)
  ) %>%
  mutate(
    defnosocomialdiff = abs(defnosocomialmin - defnosocomialmax),
    probnosocomialdiff = abs(probnosocomialmin - probnosocomialmax),
    hcwcasesdiff = abs(hcwcasesmin - hcwcasesmax),
    communitycasesdiff = abs(communitycasesmin - communitycasesmax),
    occupancydiff = abs(occupancymin - occupancymax),
    percentpre1965diff = abs(percentpre1965min - percentpre1965max),
    singleroomsdiff = abs(singleroomsmin - singleroomsmax),
    volumeperbeddiff = abs(volumeperbedmin - volumeperbedmax),
    hcwcasesdiff_perstaff = abs(hcwcasesmin_perstaff - hcwcasesmax_perstaff)
  )
max(df_check$defnosocomialdiff, na.rm = TRUE)
# [1] 0
max(df_check$defnosocomialdiff, na.rm = TRUE)
# [1] 0
max(df_check$probnosocomialdiff, na.rm = TRUE)
# [1] 0
max(df_check$hcwcasesdiff, na.rm = TRUE)
# [1] 0
max(df_check$communitycasesdiff, na.rm = TRUE)
# [1] 0
max(df_check$occupancydiff, na.rm = TRUE)
# [1] 0
max(df_check$percentpre1965diff, na.rm = TRUE)
# [1] 0
max(df_check$singleroomsdiff, na.rm = TRUE)
# [1] 0
max(df_check$volumeperbeddiff, na.rm = TRUE)
# [1] 0
max(df_check$hcwcasesdiff_perstaff, na.rm = TRUE)
# [1] 0
# No variation in any variables of interest

df <- df_import %>% 
  group_by(id) %>%
  summarize(
    region = first(region),
    defnosocomial = min(day15per100beds),
    probnosocomial = min(day8to14per100beds),
    hcwcases = min(hcw_casesper100beds), # note - also available per 100 staff
    communitycases = min(cumul_ca_casesper100beds),
    occupancy = min(occupancy),
    percentpre1965 = min(percent_pre1965),
    singlerooms = min(proportion_single_rooms),
    volumeperbed = min(volume_per_bed),
    hcwcases_perstaff = min(hcw_casesper100staff)
  ) 

# Count missing values per row 
sum( is.na(df$defnosocomial) )
# [1] 0
sum( is.na(df$probnosocomial) )
# [1] 0
sum( is.na(df$hcwcases) )
# [1] 36
sum( is.na(df$communitycases) )
# [1] 0
sum( is.na(df$occupancy) )
# [1] 0
sum( is.na(df$percentpre1965) )
# [1] 0
sum( is.na(df$singlerooms) )
# [1] 0
sum( is.na(df$volumeperbed) )
# [1] 0
sum( is.na(df$hcwcases_perstaff) )
# [1] 36

# Only missing data are among healthcare worker cases 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Range of data values 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

quantile(df$defnosocomial)
#      0%      25%      50%      75%     100% 
# 0.00000 10.35345 17.54264 23.82777 45.41502 
quantile(df$defnosocomial, na.rm = TRUE)
#      0%      25%      50%      75%     100% 
# 0.00000 10.35345 17.54264 23.82777 45.41502 
quantile(df$probnosocomial, na.rm = TRUE)
#       0%      25%      50%      75%     100% 
# 0.00000 10.74624 19.43278 26.84214 44.51326 
quantile(df$hcwcases, na.rm = TRUE)
#       0%      25%      50%      75%     100% 
# 115.1216 213.3758 281.9616 340.1800 597.8403 
quantile(df$communitycases, na.rm = TRUE)
#       0%        25%        50%        75%       100% 
# 1.499837 159.986088 202.673613 254.710198 466.672256 
quantile(df$occupancy, na.rm = TRUE)
#       0%       25%       50%       75%      100% 
# 57.87779  77.49606  83.14246  88.01421 100.00000 
quantile(df$percentpre1965, na.rm = TRUE)
#       0%       25%       50%       75%      100% 
# 0.000000  3.545559 13.094471 32.762060 88.640000 
quantile(df$singlerooms, na.rm = TRUE)
#       0%        25%        50%        75%       100% 
# 8.416831  26.080969  33.665891  43.822417 100.000000 
quantile(df$volumeperbed, na.rm = TRUE)
#       0%       25%       50%       75%      100% 
# 107.7008  399.4547  491.1590  591.3001 1856.5202

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Make the plot 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Remove Trust IDs
df <- df %>% select(-id) %>%
  # Make NE+Yorkshire fit 
  mutate(region = if_else(region == "North East and Yorkshire", "N. East & Yorkshire", region))


# color-blind friendly palette 

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# currently not used 

# Custom density function 

customdensity <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    geom_density(..., alpha = .4, bw = "nrd") 
}

# First plot - definite, probable, HCW, community acquired

plot1 <- ggpairs(
  data = df,
  columns = 2:5,
  mapping = aes(fill = NA, color = region),
  diag = list(continuous = customdensity),
  columnLabels = c(
    "Definite hospital-associated infections", 
    "Probably hospital-associated infections", 
    "Healthcare worker cases", 
    "Community-acquired admissions"
  )
) 

for(i in 1:4) {
  for(j in 1:4){
    plot1[i,j] <- plot1[i,j] +
      scale_fill_brewer(palette = "Dark2") +
      scale_color_brewer(palette = "Dark2")
  }
}
plot1 <- plot1 + 
  theme(
    panel.border = element_blank(),
    panel.background = element_blank(),
    strip.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line = element_line()
  )

plot1

#ggsave("pairsplot1.tiff",  units = "cm", width = 25, height = 25, device='tiff', dpi=300)

# Second plot - add occupancy, pre1965, single rooms, volume per bed

# Need even smaller labels 
# Remove Trust IDs
df <- df %>% mutate(
  region = if_else(
    region == "N. East & Yorkshire", 
    "N.E. & Yorks", 
    if_else(region == "East of England", "E. England", region)
  )
)

plot2 <- ggpairs(
  data = df,
  columns = 2:9,
  mapping = aes(fill = NA, color = region),
  diag = list(continuous = customdensity),
  columnLabels = c(
    "Definite hospital-associated", # some labels also abridged compared to earlier
    "Probably hospital-associated", 
    "Healthcare worker cases", 
    "Community-acquired", 
    "Bed occupancy",
    "% pre-1965",
    "Single rooms",
    "Heated volume per bed"
  )
) 

for(i in 1:8) {
  for(j in 1:8){
    plot2[i,j] <- plot2[i,j] +
      scale_fill_brewer(palette = "Dark2") +
      scale_color_brewer(palette = "Dark2")
  }
}
plot2 <- plot2 + 
  theme(
    panel.border = element_blank(),
    panel.background = element_blank(),
    strip.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line = element_line()
  )

plot2

# ggsave("pairsplot2.tiff",  units = "cm", width = 35, height = 35, device='tiff', dpi = 300)

