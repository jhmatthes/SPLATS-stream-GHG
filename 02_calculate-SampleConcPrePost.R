# Use exported data from LI-7810 to calculate the
# CH4 and CO2 concentrations in stream gas samples

library(tidyverse)

# Running mean function for estimating start/stop
runmean <- function(x, n = 5){
  stats::filter(x, rep(1 / n, n), sides = 2)
}

files = list.files("data/raw-data-7810/",full.names = T)

# Required to estimate sample concentration from atm + sample mix
sample_vol = 1 # 1 mL sample volume
eff_vol = 30.47141189 #effective analyzer vol in mL

for(f in 1:length(files)){
  
  # 1. Load in the raw text file downloaded from LI-7810
  # only pull in the actual data and the main header (column names)
  #all_content = readLines("~/Desktop/SPLATS-streams/SPLATS-streams/raw-li7810-data/TG10-01861-2025-07-03T000000.data")
  all_content = readLines(paste0(files[f]))
  
  skip_second = all_content[-7]
  
  data = read.delim(textConnection(skip_second),
                    skip=5, na = "nan")
  
  # # 2. Use REMARKS column to find the unique sample IDs (each exetainer rep)
  # # siteYYYYmmdd[sample]-[rep]
  # # in R we use use data$col_name
  samples = unique(data$REMARK)    
  samples = samples[2:length(samples)]
  
  # check to remove tree flux samples (May 2025 onward)
  str_samples = c("B","A","Z","D","N")
  samples = samples[which(substr(samples,1,1) %in% str_samples)]
  
  # set up empty vectors for calculations
  CH4_post = CH4_base = CO2_base = CO2_post = CH4_base_sd = CH4_post_sd = 
    CO2_base_sd = CO2_post_sd = vector()
  site = sample = sample_date = rep = vector()
  
  # Loop through samples and find the conc jump when sample was run
  for(s in 1:length(samples)){
    
    # Index for start of sample (press Enter for Remark)
    # and end (end of Remark tag)
    samp_start = min(which(data$REMARK == samples[s]))
    samp_end = max(which(data$REMARK == samples[s]))
    sample_dat = data[(samp_start-20):(samp_end),]
    
    # Running mean - calculate mean for every 5 datapoints
    # diff of running mean on CH4 column in sample_dat
    CH4_diff = diff(runmean(sample_dat$CH4))
    CH4_diff = abs(CH4_diff[!is.na(CH4_diff)]) # abs = absolute value
    CH4_diffmax = which(CH4_diff==max(CH4_diff))
    
    # Take mean concentrations before & after mixing period
    # initial window limits
    min1 = 15; min2 = 5; max1 = 30; max2 = 40 
    
    CH4_base[s] = mean(sample_dat$CH4[(CH4_diffmax-min1):(CH4_diffmax-min2)], na.rm=T)
    CH4_base_sd[s] = sd(sample_dat$CH4[(CH4_diffmax-min1):(CH4_diffmax-min2)], na.rm=T)
    
    CH4_post[s] = mean(sample_dat$CH4[(CH4_diffmax+max1):(CH4_diffmax+max2)], na.rm=T)
    CH4_post_sd[s] = sd(sample_dat$CH4[(CH4_diffmax+max1):(CH4_diffmax+max2)], na.rm=T)
    
    CO2_base[s] = mean(sample_dat$CO2[(CH4_diffmax-min1):(CH4_diffmax-min2)], na.rm=T)
    CO2_base_sd[s] = sd(sample_dat$CO2[(CH4_diffmax-min1):(CH4_diffmax-min2)], na.rm=T)
    
    CO2_post[s] = mean(sample_dat$CO2[(CH4_diffmax+max1):(CH4_diffmax+max2)], na.rm=T)
    CO2_post_sd[s] = sd(sample_dat$CO2[(CH4_diffmax+max1):(CH4_diffmax+max2)], na.rm=T)
    
    if(is.na(CO2_post[s])|is.na(CH4_post[s])){ # if NA try a shorter window
      max1 = 15; max2 = 25 
      CH4_post[s] = mean(sample_dat$CH4[(CH4_diffmax+max1):(CH4_diffmax+max2)], na.rm=T)
      CH4_post_sd[s] = sd(sample_dat$CH4[(CH4_diffmax+max1):(CH4_diffmax+max2)], na.rm=T)
      CO2_post[s] = mean(sample_dat$CO2[(CH4_diffmax+max1):(CH4_diffmax+max2)], na.rm=T)
      CO2_post_sd[s] = sd(sample_dat$CO2[(CH4_diffmax+max1):(CH4_diffmax+max2)], na.rm=T)
    } else if(is.na(CO2_post[s])|is.na(CH4_post[s])){
      CH4_post[s] = NA
      CH4_post_sd[s] = NA
      CO2_post[s] = NA
      CO2_post_sd[s] = NA
    }
    
    # Export pdf of each sample period
    # if(!is.na(CO2_post[s])){
    #   pdf(paste0("plots/",samples[s],".pdf"))
    #   tmp = sample_dat[(CH4_diffmax-min1):(CH4_diffmax+max2),]
    #   plot(tmp$SECONDS, tmp$CH4, main=samples[s])
    #   tmp2 = sample_dat[(CH4_diffmax-min1):(CH4_diffmax-min2),]
    #   points(tmp2$SECONDS, tmp2$CH4,col="red")
    #   tmp3 = sample_dat[(CH4_diffmax+max1):(CH4_diffmax+max2),]
    #   points(tmp3$SECONDS, tmp3$CH4,col="red")
    #   dev.off()
    # }
  }
  
  # make data frame from base & post concentration mean & sd
  GHGconc_df = data.frame(sample = samples, CH4_base, CH4_base_sd,
                          CH4_post, CH4_post_sd, CO2_base, CO2_base_sd, 
                          CO2_post, CO2_post_sd)

  GHGconc_df$CH4_change = GHGconc_df$CH4_post-GHGconc_df$CH4_base
  GHGconc_df$CO2_change = GHGconc_df$CO2_post-GHGconc_df$CO2_base
  
  # (sample_vol * CH4_post + eff_vol * CH4_change) / sample_vol
  GHGconc_df$CH4_conc = (sample_vol*GHGconc_df$CH4_post + eff_vol*GHGconc_df$CH4_change)/sample_vol
  GHGconc_df$CO2_conc = (sample_vol*GHGconc_df$CO2_post + eff_vol*GHGconc_df$CO2_change)/sample_vol
  
  if(f==1){
    GHGconc = GHGconc_df
  } else {
    GHGconc = dplyr::bind_rows(GHGconc,GHGconc_df)
  }
}

# Save processed output of everything
#write.csv(GHGconc, "data/stream-depth-ghg-rawconc.csv",row.names=F)

# Separate depth profile & stream samples
GHG_depth = GHGconc[grep("D",GHGconc$sample),]
GHG_stream = GHGconc[-grep("D",GHGconc$sample),]
GHG_stream = GHG_stream[-grep("ACSA",GHG_stream$sample),] # remove JG samples

# Format stream sample ID into site, date, and sample
GHG_stream_full = GHG_stream %>%
  dplyr::mutate(stream_sample = dplyr::case_when(grepl("AT", sample) ~ "AT",
                            grepl("W1", sample) ~ "W1",grepl("W2", sample) ~ "W2",
                            grepl("W3", sample) ~ "W3"),
                site = substr(sample,1,2), # always first two chars
                measurement_rep = substr(sample,nchar(sample),nchar(sample))) # always last char

# Fix typos & remove tests in the datasheet
GHG_stream_full$site[GHG_stream_full$site=="AV"] = "AU"
GHG_stream_full = GHG_stream_full[-grep("20m",GHG_stream_full$sample),] 
GHG_stream_full = GHG_stream_full[-grep("40m",GHG_stream_full$sample),] 
GHG_stream_full = GHG_stream_full[-grep("-10-",GHG_stream_full$sample),] 
GHG_stream_full = GHG_stream_full[-grep("-20-",GHG_stream_full$sample),] 
GHG_stream_full = GHG_stream_full[-grep("-50-",GHG_stream_full$sample),] 
GHG_stream_full = GHG_stream_full[-grep("-5-",GHG_stream_full$sample),] 
GHG_stream_full$stream_sample[is.na(GHG_stream_full$stream_sample)] = "W1"
GHG_stream_full$measurement_rep[GHG_stream_full$measurement_rep=="M"] = "1"
GHG_stream_full$measurement_rep[GHG_stream_full$measurement_rep=="4"] = "1"
GHG_stream_full$measurement_rep[GHG_stream_full$measurement_rep=="5"] = "2"
GHG_stream_full$measurement_rep[GHG_stream_full$measurement_rep=="6"] = "3"

# Extract dates from sample ID as string of 8 numbers
all_dates = gregexpr('(?<!\\d)[0-9]{8}(?!\\d)',GHG_stream_full$sample,perl=T)
date_start = unlist(all_dates)
date_df = data.frame(sample=GHG_stream_full$sample,date_start)
dates = substr(date_df$sample,date_start,date_start+7)

# Fix date typos
dates[dates=="AL2024"] = "20241101"
dates[dates=="AU2024"] = "20241101"
dates[dates=="AS2024"] = "20241209"
dates[dates=="BG2504"] = "20250408"
dates[dates=="AS2025"] = "20250417"

# Bridge date into year-month-dayofmonth cols for data safety
GHG_stream_full$date = lubridate::ymd(dates)
GHG_stream_full$year = lubridate::year(GHG_stream_full$date)
GHG_stream_full$month = lubridate::month(GHG_stream_full$date)
GHG_stream_full$mday = lubridate::mday(GHG_stream_full$date)

# Export and save the data as a csv file
#write.csv(GHG_stream_full, "data/GHGstream_hdspconc_thru20250710.csv",row.names=F)

# Format depth sample ID into site, date, and sample
#GHG_depth_full = GHG_depth[-grep("20m",GHG_depth$sample),] 
GHG_depth = GHGconc[grep("D",GHGconc$sample),]
GHG_depth = GHG_depth[-grep("40mL",GHG_depth$sample),] 
GHG_depth_full = GHG_depth %>%
  dplyr::mutate(stream_sample = dplyr::case_when(grepl("AT", sample) ~ "AT",
                                                 grepl("D15", sample) ~ "D15",
                                                 grepl("D30", sample) ~ "D30",
                                                 grepl("D60", sample) ~ "D60"),
                
                site = substr(sample,1,2), # always first two chars
                measurement_rep = substr(sample,nchar(sample),nchar(sample)))#, # always last char
                #depth_rep = substr(sample,nchar(sample)-1,nchar(sample)-1))

GHG_depth_full$depth_rep = rep(NA,nrow(GHG_depth_full))
GHG_depth_full$depth_rep[grep("2024",GHG_depth_full$sample)] = substr(GHG_depth_full$sample[grep("2024",GHG_depth_full$sample)],
                                                                       nchar(GHG_depth_full$sample)[grep("2024",GHG_depth_full$sample)]-1,
                                                                       nchar(GHG_depth_full$sample)[grep("2024",GHG_depth_full$sample)]-1)

GHG_depth_full$depth_rep[grep("250",GHG_depth_full$sample)] = substr(GHG_depth_full$sample[grep("250",GHG_depth_full$sample)],
                                                                      nchar(GHG_depth_full$sample)[grep("250",GHG_depth_full$sample)]-2,
                                                                      nchar(GHG_depth_full$sample)[grep("250",GHG_depth_full$sample)]-2)

GHG_depth_full$depth_rep[GHG_depth_full$depth_rep=="T"] = "1"
GHG_depth_full$depth_rep[GHG_depth_full$depth_rep=="M"] = "1"

# 2024 called 24 and BGS
GHG_depth_full$depth_rep[is.na(GHG_depth_full$depth_rep)] = substr(GHG_depth_full$sample[is.na(GHG_depth_full$depth_rep)],
                                                                nchar(GHG_depth_full$sample)[is.na(GHG_depth_full$depth_rep)]-2,
                                                                nchar(GHG_depth_full$sample)[is.na(GHG_depth_full$depth_rep)]-2)

GHG_depth_full$depth_rep[GHG_depth_full$depth_rep=="Z"] = substr(GHG_depth_full$sample[GHG_depth_full$depth_rep=="Z"],
                                                              nchar(GHG_depth_full$sample)[GHG_depth_full$depth_rep=="Z"]-3,
                                                              nchar(GHG_depth_full$sample)[GHG_depth_full$depth_rep=="Z"]-3)

# Extract dates from sample ID as string of 8 numbers
dates = rep(NA,nrow(GHG_depth_full))
dates[grep("2024",GHG_depth_full$sample)] = substr(GHG_depth_full$sample[grep("2024",GHG_depth_full$sample)],
                                                  3,10)
dates[grep("250",GHG_depth_full$sample)] = paste0("20",substr(GHG_depth_full$sample[grep("250",GHG_depth_full$sample)],
                                                  3,8))

dates[is.na(dates)] = paste0("20",substr(GHG_depth_full$sample[is.na(dates)],4,9))

GHG_depth_full$date = lubridate::ymd(dates)
GHG_depth_full$date[is.na(GHG_depth_full$date)] = lubridate::ymd("2025-05-07")
GHG_depth_full$year = lubridate::year(GHG_depth_full$date)
GHG_depth_full$month = lubridate::month(GHG_depth_full$date)
GHG_depth_full$mday = lubridate::mday(GHG_depth_full$date)

# Add in the BG & AS surface water samples
GHG_swamp = dplyr::filter(GHG_stream_full,site %in% c("AS","BG"))

GHG_depthsurface = dplyr::bind_rows(GHG_depth_full,GHG_swamp)

# write out file
#write.csv(GHG_depthsurface, "data/GHGdepth_hdspconc_thru20250710.csv",row.names=F)
