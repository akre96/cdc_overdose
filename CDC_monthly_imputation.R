#Code for "Hidden in the Aggregate: The Deadliest Month on Record for Overdose in the United States"
#Written by: Joseph Friedman, MPH
#joseph.robert.friedman@gmail.com
#Last Updated: January 3rd, 2021

## Setup
# Before running change 'root' variable to location of repository on local machine
rm(list = ls())
pacman::p_load(data.table, tidyverse, ggplot2,ggrepel, grid, gridExtra,lubridate,cowplot,rgdal)
root <- "/Users/akre96/Code/cdc_overdose"

#load final monthly mortality files
#state-level
cdc_state <- fread(paste0(root,"/input/ground_truth_states_with_supressed.csv"))
#division-level
cdc_division <- fread(paste0(root,"/input/ground_truth_divisions_with_supressed.csv"))
#state-division mapping
st.rg.map <- fread(paste0(root,"/ref/state_region_division_map.csv"))

#merge
setnames(cdc_state,"State","State_Name")
cdc_state <- merge(cdc_state,st.rg.map,by="State_Name")

#sum states to divisions, to quantify total missings per division at state level
cdc_state[,Deaths:=as.numeric(Deaths)]
cdc_state[,div_deaths:=sum(Deaths,na.rm=T),by=.(Year,Month,Division)]

#merge on division totals
cdc_division[,Division:=str_replace(`Census Division`,"Division [1-9]: ","")]
setnames(cdc_division,"Deaths","Division_Deaths")
cdc <- merge(cdc_state,cdc_division[,c("Division","Month Code","Division_Deaths")],by=c("Month Code","Division"),all=T)

#add missings back proportional between missing states in division
cdc[,missing_deaths:=Division_Deaths-div_deaths]
cdc[is.na(Deaths),blank:=1]
cdc[,num_blanks:=sum(blank,na.rm=T),by=.(`Month Code`,Division)]
cdc[is.na(Deaths),Deaths:=missing_deaths/num_blanks]

#confirm no missings
nrow(cdc[is.na(Deaths)]) 

#save imputed ground truth for export to python
cdc <- cdc[,c("State_Name","Year","Month Code","Division","Region","Deaths")]
cdc <- cdc[order(State_Name,Year,`Month Code`)]
write.csv(cdc,paste0(root,"/output/ground_truth_states_imputation.csv"),row.names=F)









