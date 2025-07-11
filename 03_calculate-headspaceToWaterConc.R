# Convert headspace concentration output for multiple days from the LI-7810
# with water & air temp, and air pressure to calculate the concentration
# within the water sample in the field

# Change file path to SPLATS-streams-v2 folder
source("GHG_noCarbonateCorr.R")


### Apply corrections to calculate the concentration in the streamwater
### from the headspace concentration

# Read in headspace concentration file for streams
stream_only = readr::read_csv("data/GHGstream_hdspconc_thru20250710.csv")

# Get hydromet table from aggregate-streamTempDischarge.R
# Format hydro data for linking with concentrations
hydro_all = hydromet %>%
  dplyr::group_by(date) %>%
  dplyr::summarize(nb.wt=mean(nb.wt,na.rm=T), al.wt=mean(al.wt,na.rm=T),
            au.wt=mean(au.wt,na.rm=T),bvs.wt=mean(bvs.wt,na.rm=T),
            bgs.wt=mean(bgs.wt,na.rm=T),tair = mean(airt,na.rm=T),
            Pkpa = mean(bar*0.1,na.rm=T))

# Format table to get into GHG-noCarbonateCorr.R format
hydro = select(hydro_all,date,nb.wt,al.wt,au.wt,bvs.wt,bgs.wt) %>%
  pivot_longer(nb.wt:bgs.wt,names_to = "site",values_to="wtemp") %>%
  mutate(xsite = substr(site,1,2)) %>% select(-site) %>%
  mutate(vsite = case_when(xsite == "nb" ~ "NB", xsite=="al" ~ "AL",
                           xsite=="au" ~ "AU",xsite=="bv" ~ "AS",
                           xsite=="bg" ~ "BG")) %>%
  select(-xsite) %>% 
  rename(site=vsite)

met = select(hydro_all, date, tair,  Pkpa)

# Combine hydro-met data with headspace data
hydromet = left_join(hydro, met, by="date")
streamGHGphys = left_join(stream_only,hydromet,by=c("site","date")) %>%
  filter(site != "AO")

# loop over dates & sites
dates = unique(streamGHGphys$date)
sites = unique(streamGHGphys$site)

for(d in 1:length(dates)){
  
  # filter to each date
  ddat = dplyr::filter(streamGHGphys, date == dates[d])
  sites = unique(ddat$site)
  
  #if(length(unique(ddat$stream_samp))>1 & length(sites)>1){
    for(s in 1:length(sites)){
      sddat = dplyr::filter(ddat, site == sites[s])
      if(length(unique(sddat$stream_sample))>1){
        atm_mean = dplyr::filter(sddat, stream_sample == "AT")
        if(nrow(atm_mean)==0){ # if ATM missing use ATM on same day
          atm_mean = filter(ddat,stream_sample=="AT")
        }
        streams = dplyr::filter(sddat, stream_sample!= "AT") %>%
          dplyr::rename(datetime.EST = date) %>%
          dplyr::group_by(datetime.EST, site, stream_sample) %>%
          dplyr::summarize(HS.mCH4.after = mean(CH4_conc, na.rm=T)/1000,
                           HS.mCO2.after = mean(CO2_conc, na.rm=T),
                           HS.mN2O.after = 0,
                           Temp.insitu = mean(wtemp),
                           Temp.equil = mean(tair),
                           Bar.pressure = mean(Pkpa)) %>%
          dplyr::mutate(site = paste0(site,"-",stream_sample)) %>%
          dplyr::select(-stream_sample)
        
        streams$HS.mCH4.before = rep(mean(atm_mean$CH4_conc, na.rm=T), 
                                     nrow(streams))/1000
        streams$HS.mCO2.before = rep(mean(atm_mean$CO2_conc, na.rm=T), 
                                     nrow(streams))
        streams$HS.mN2O.before = rep(0, nrow(streams))
        
        streams$Volume.gas = rep(20	, nrow(streams)) # mL gas
        streams$Volume.water = rep(80, nrow(streams)) # mL water
        
        output <- GHG(streams)
        output$date = rep(unique(sddat$date), nrow(streams))
        
        if(s == 1 & d == 1){
          stream_GHGwater = output
        } else {
          stream_GHGwater = bind_rows(stream_GHGwater, output)
        }
      }
      
    }
  #}
}

# write.csv(stream_GHGwater,"data/streamGHG_water_thru2025-07-11.csv",
#           row.names=F,na="")

