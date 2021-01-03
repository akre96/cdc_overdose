#Code for "Hidden in the Aggregate: The Deadliest Month on Record for Overdose in the United States"
#Written by: Joseph Friedman, MPH
#joseph.robert.friedman@gmail.com
#Last Updated: January 3rd, 2021

## Setup
# Before running change 'root' variable to location of repository on local machine
rm(list = ls())
pacman::p_load(data.table, tidyverse, ggplot2,ggrepel, grid, gridExtra,lubridate,cowplot,rgdal)
root <- "/Users/akre96/Code/cdc_overdose"

# Load output from CDC_monthly_imputation.R
cdc <- fread(paste0(root,"/output/ground_truth_states_imputation.csv"))
#state-division mapping
st.rg.map <- fread(paste0(root,"/ref/state_region_division_map.csv"))

#-------------Load Output From Python -- Post Recovery Process--------------------#
cdc.rec <- fread(paste0(root,"/output/monthly_overdose_computed_m2.csv"))
cdc.rec[,date:=as.Date(timestamp,format="%m/%d/%Y")]
cdc.rec[,month:=month(date)]
cdc.rec[,year:=year(date)]
setnames(cdc.rec,"location","State_Name")
cdc.rec <- merge(cdc.rec,st.rg.map,by="State_Name")

#append past trends 2015-2019 onto 2020 from recovery algorithm
cdc[,month:=as.numeric(str_remove(`Month Code`,"201[1-9]/"))]
setnames(cdc,"Year","year")

#---------------Sanity Check------------------------
#implement recovery in R, compare to python
aggs <- fread(paste0(root,"/input/CDC_ts.csv"))
setnames(aggs,"location","State_Name")
aggs[,date:=as.Date(end_date,format="%m/%d/%Y")]

aggs[,mono:=month(date)+((year(date)-2010)*12)]
cdc[,mono:=month+(((year)-2010)*12)]

cdc2 <- cdc[mono!=120]
for (c.d in sort(unique(aggs[date>"2019-11-01",date]))){
  
  c.agg <- aggs[date==c.d]
  c.mono <- unique(c.agg$mono)
  c.fin <- cdc2[mono<c.mono & mono >(c.mono-12)]
  c.fin <- c.fin[, .(sum11=sum(Deaths)),by=.(State_Name)]
  c.agg <- merge(c.agg,c.fin,by="State_Name")
  c.agg[,Deaths:=predicted_val-sum11]
  
  c.agg <- c.agg[,c("State_Name","date","mono","Deaths")]
  cdc2 <- rbind(cdc2,c.agg,fill=T)
  
}

test <- merge(cdc2[!is.na(date),c("State_Name","date","Deaths")],cdc.rec[year==2020|(year==2019&month==12),c("State_Name","date","raw_predicted_val")],by=c("State_Name","date"))
View(test) #results match perfectly

#-------------------------------------------------
#finish merging
cdc.rec <- rbind(cdc.rec[year==2020],cdc,fill=T)
cdc.rec[is.na(raw_predicted_val),raw_predicted_val:=Deaths]

#merge on state-level pops for calculating rates
pop <- fread(paste0(root,"/input/state_pops.csv"))
pop[,State_Name:=str_remove(State_Name,".")]
pop <- melt.data.table(pop,id.vars = "State_Name",variable.name = "year",value.name = "pop")
pop[,pop:=as.numeric(str_remove_all(pop,","))]
pop[,year:=as.numeric(str_remove_all(year,"y"))]

cdc.rec <- merge(cdc.rec,pop,by=c("State_Name","year"),all.x=T)
cdc.rec.n <- cdc.rec[,.(deaths=sum(raw_predicted_val),pop=sum(pop)),by=.(month,year)]
cdc.rec.d <- cdc.rec[,.(deaths=sum(raw_predicted_val),pop=sum(pop)),by=.(month,year,Division)]

#add standard errors at state,division, national level
#state
errors <- fread(paste0(root,"/output/full_error.csv"))
setnames(errors,"location","State_Name")
errors <- merge(errors,st.rg.map,by="State_Name")
se.s <- errors[,.(sd_pe=sd(percent_error,na.rm=T)),by=.(month_out,State_Name)]
#division
errors.d <- errors[,.(truth=sum(truth),pred=sum(pred)),by=.(Division,pred_timestamp,month_out)]
errors.d[,error:=pred-truth]
errors.d[,percent_error:=(error/truth)*100]
se.d <- errors.d[,.(sd_pe=sd(percent_error,na.rm=T)),by=.(month_out,Division)]
#national
errors.n <- errors[,.(truth=sum(truth),pred=sum(pred)),by=.(pred_timestamp,month_out)]
errors.n[,error:=pred-truth]
errors.n[,percent_error:=(error/truth)*100]
se.n <- errors.n[,.(sd_pe=sd(percent_error,na.rm=T)),by=.(month_out)]

#line up months of predicting with data, starting December 2019 as month 1
cdc.rec[year==2020,month_out:=month+1]
cdc.rec <- merge(cdc.rec,se.s,by=c("State_Name","month_out"),all.x=T)
setnames(cdc.rec,"raw_predicted_val","deaths")

#calculate upper and lower bounds as 1.96 * standard error
cdc.rec[,deaths_upper:=deaths+(deaths*(sd_pe/100)*1.96)]
cdc.rec[,deaths_lower:=deaths-(deaths*(sd_pe/100)*1.96)]

#division
cdc.rec.d[year==2020,month_out:=month+1]
cdc.rec.d <- merge(cdc.rec.d,se.d,by=c("Division","month_out"),all.x=T)
cdc.rec.d[,deaths_upper:=deaths+(deaths*(sd_pe/100)*1.96)]
cdc.rec.d[,deaths_lower:=deaths-(deaths*(sd_pe/100)*1.96)]

#national
cdc.rec.n[year==2020,month_out:=month+1]
cdc.rec.n <- merge(cdc.rec.n,se.n,by=c("month_out"),all.x=T)
cdc.rec.n[,deaths_upper:=deaths+(deaths*(sd_pe/100)*1.96)]
cdc.rec.n[,deaths_lower:=deaths-(deaths*(sd_pe/100)*1.96)]


#Calculate state level percent increases
cdc.rec.d.w <- dcast.data.table(cdc.rec.d[month==5&year%in%c(2019,2020)],Division~year,value.var=c("deaths","pop"))
cdc.rec.d.w[,per_chg:=((deaths_2020-deaths_2019)/(deaths_2019))*100]



#Figure 1A -- plot national level counts

gg1a <- ggplot(cdc.rec.n,aes(y=deaths,ymin=deaths_lower,ymax=deaths_upper,x=month,color=factor(year),fill=factor(year))) +
  geom_segment(size=1.5,aes(y=deaths_lower,yend=deaths_upper,x=month,xend=month,color=factor(year)))+
  geom_line(size=2,alpha=.5)+
  geom_point(size=4,shape=21,color="black") + 
  theme_bw() +
  scale_y_continuous(limits=c(3000,9500),breaks=seq(0,9000,1000))+
  scale_x_continuous(breaks=seq(1,12,1),labels=month.abb)+
  scale_color_brewer(name="",type="div",palette = 9,direction = -1,guide = guide_legend(reverse = TRUE))+
  scale_fill_brewer(name="",type="div",palette = 9,direction=-1,guide = guide_legend(reverse = TRUE)) +
  labs(y="Overdose Deaths",x="Month",title="A) Monthly Overdose Deaths, National")+
  theme(legend.position = c(.87,.8),
        legend.background = element_blank())+
  theme(plot.title = element_text(size=12, face="bold"),
        axis.text=element_text(size=10,face="bold"),
        axis.title=element_text(size=10,face="bold")
  )

#Prep shapefile for Figure Part C and D
shp.st <- readOGR(paste0(root,"/ref/cb_2018_us_state_20m/cb_2018_us_state_20m.shp"),layer='cb_2018_us_state_20m')
shp <- readOGR(paste0(root,"/ref/cb_2018_us_division_20m/cb_2018_us_division_20m.shp"),layer='cb_2018_us_division_20m')
shp$level <- shp$NAME
shp2 <- data.table(fortify(shp,region='level'))
shp.st$level <- shp.st$NAME
shp.st2 <- data.table(fortify(shp.st,region='level'))
shp2[,Division:=id]
shp.st2[,Division:=id]

#define aesthetic
aesth <- theme_classic() + theme(
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
  strip.text.x = element_text(size =10,face="bold"),
  strip.text.y = element_text(size = 15),
  axis.text=element_text(size=15,face="bold"),
  axis.title.x = element_text(colour="grey20",size=15),
  axis.title.y = element_text(colour="grey20",size=15),
  legend.text=element_text(size=15),
  title=element_text(size=12),
  legend.title=element_text(size=15))

shp3 <- merge(shp2,cdc.rec.d.w,by='Division',allow.cartesian=T)

#Figure 1 Part C
gg1c <- ggplot(shp3[long>-130&long<0]) +
  geom_polygon(alpha=.9,aes(long,lat,group=group,fill=(deaths_2020/pop_2020)*1000000)) +
  geom_path(size=2,alpha=1,aes(long,lat,group=group),color="black") +
  #scale_fill_brewer(direction=-1,type="div",palette =7,name="Outcome\nPercentile") +
  #scale_fill_viridis_c(name="",guide=guide_colorbar(barwidth=20,barheight = 1),breaks=seq(0,60,10),labels=paste0(seq(0,60,10),"%"))+
  scale_fill_gradient2(name="",high="#d53e4f",low="#3288bd",mid="#ffffbf",midpoint=30,guide=guide_colorbar(barwidth=15,barheight = .5),limits=c(15,45),breaks=seq(15,45,5),labels=paste0(seq(15,45,5)))+
  geom_path(data=shp.st2[long>-130&long<0],aes(long,lat,group=group),color='white',alpha=.2) +
  coord_equal() +  aesth + labs(y="",x="",title="C) Overdose Deaths per Million, May 2020") +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  theme(legend.position="bottom",legend.direction = "horizontal",panel.border = element_rect(colour = "black", fill=NA)) +
  theme(strip.background =element_rect(fill="white")) +
  theme(plot.title = element_text(size=12, face="bold"))

#Figure 1 Part D
gg1d <- ggplot(shp3[long>-130&long<0]) +
  geom_polygon(alpha=.9,aes(long,lat,group=group,fill=per_chg)) +
  geom_path(size=2,alpha=1,aes(long,lat,group=group),color="black") +
  #scale_fill_brewer(direction=-1,type="div",palette =7,name="Outcome\nPercentile") +
  #scale_fill_viridis_c(name="",guide=guide_colorbar(barwidth=20,barheight = 1),breaks=seq(0,60,10),labels=paste0(seq(0,60,10),"%"))+
  scale_fill_gradient2(name="",high="#d53e4f",low="#3288bd",mid="#ffffbf",midpoint=50,guide=guide_colorbar(barwidth=15,barheight = .5),limits=c(20,100),breaks=seq(0,100,20),labels=paste0("+",seq(0,100,20),"%"))+
  geom_path(data=shp.st2[long>-130&long<0],aes(long,lat,group=group),color='white',alpha=.2) +
  coord_equal() +  aesth + labs(y="",x="",title="D) Percent Change, May 2019 to May 2020") +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  theme(legend.position="bottom",legend.direction = "horizontal",panel.border = element_rect(colour = "black", fill=NA)) +
  theme(strip.background =element_rect(fill="white")) +
  theme(plot.title = element_text(size=12, face="bold"))

#Part B - 2019 and 2020 in top 20 states
test <- cdc.rec[year==2020,.(deaths=sum(deaths),sd_pe=mean(sd_pe)),by=.(State_Name)]
test[,rnk:=frank(deaths)]
#c.st <- test[rnk>=21,State_Name]
c.st <- test[rnk>=21,State_Name]

#Calculate per capita deaths
cdc.rec[,deaths_pc:=deaths/pop*1000000]
cdc.rec[,deaths_pc_upper:=deaths_upper/pop*1000000]
cdc.rec[,deaths_pc_lower:=deaths_lower/pop*1000000]

#order accordingly 
cdc.rec <- cdc.rec[order(deaths_pc)]

#define order in graph
cdc.rec[,State_Name:=factor(State_Name,levels=unique(cdc.rec[year==2020&month==5,State_Name]))]

#Figure 1 Part B -- State Level per capita trends in May
gg1b <- ggplot(cdc.rec[State_Name%in%c.st&month==5&year>2013],
               aes(y=State_Name,x=deaths_pc,fill=factor(year)))+
  geom_line(aes(group=State_Name),color="grey10",size=2,alpha=.2) +
  geom_segment(size=1.5,aes(x=deaths_pc_lower,xend=deaths_pc_upper,y=State_Name,yend=State_Name,color=factor(year)))+
  geom_point(shape=21,size=4)+
  theme_bw()+
  scale_x_continuous(limits=c(0,105),breaks=seq(0,100,25)) +
  scale_fill_brewer(name="",type="div",palette = 9,direction=-1,guide = guide_legend(reverse = TRUE))+
  scale_color_brewer(name="",type="div",palette = 9,direction=-1,guide = guide_legend(reverse = TRUE))+
  labs(y="State",x="Deaths per Million",title="B) Deaths per Million in May, Top States")+
  theme(legend.position = c(.85,.3),
        legend.background = element_blank(),
        plot.title = element_text(size=12, face="bold"),
        axis.title=element_text(size=10,face="bold"),
        axis.text=element_text(size=10,face="bold")
  )

#Combine Graph parts and Save
pdf(paste0(root,"/visuals/CDC_Recovery_figure_",Sys.Date(),".pdf"),height=7,width=14)

lay = rbind(c(1,1,2,2,3,3),
            c(1,1,2,2,4,4))

grid.arrange(gg1a,gg1b,gg1c,gg1d,layout_matrix=lay)
dev.off()


#Number Plug for manuscript
View(cdc.rec[State_Name%in%c.st&month==5&year==2020,c("State_Name","deaths_pc","deaths_pc_lower","deaths_pc_upper")])

(((cdc.rec.n[year==2020&month==5,deaths] /  cdc.rec.n[year==2019&month==5,deaths]))-1)*100

(((cdc.rec.n[year==2020&month==5,deaths_upper] /  cdc.rec.n[year==2019&month==5,deaths]))-1)*100
(((cdc.rec.n[year==2020&month==5,deaths_lower] /  cdc.rec.n[year==2019&month==5,deaths]))-1)*100

cdc.rec.n[year==2020&month==5,deaths]
cdc.rec.n[year==2020&month==5,deaths_upper]
cdc.rec.n[year==2020&month==5,deaths_lower]


#supplemental figure 1
temp1 <- copy(cdc.rec)
temp1[deaths<10,deaths:=10]
temp1[deaths_lower<10,deaths_lower:=10]
ggs1 <- ggplot(temp1[State_Name%in%c.st&year>2013],aes(y=deaths,ymin=deaths_lower,ymax=deaths_upper,x=month,color=factor(year),fill=factor(year))) +
  geom_segment(size=1.5,aes(y=deaths_lower,yend=deaths_upper,x=month,xend=month,color=factor(year)))+
  geom_line(size=1,alpha=.5)+ facet_wrap(~State_Name,scales="free") +
  geom_point(size=2,shape=21,color="black") + 
  theme_bw() +
  #scale_y_continuous(limits=c(3000,9500),breaks=seq(0,9000,1000))+
  scale_x_continuous(breaks=seq(1,12,1),labels=month.abb)+
  scale_color_brewer(name="",type="div",palette = 9,direction = -1,guide = guide_legend(reverse = TRUE))+
  scale_fill_brewer(name="",type="div",palette = 9,direction=-1,guide = guide_legend(reverse = TRUE)) +
  labs(y="Overdose Deaths",x="Month",title="Monthly Overdose Deaths, States With More Stable Timeseries")+
  theme(legend.position = "top", strip.background  = element_blank(),
        legend.background = element_blank())+
  theme(plot.title = element_text(size=12, face="bold"),
        axis.text=element_text(size=7,face="bold"),
        axis.title=element_text(size=7,face="bold")
  )

pdf(paste0(root,"/visuals/CDC_Recovery_supp_figure_",Sys.Date(),".pdf"),height=12,width=16)
ggs1
dev.off()

#Supplemental Figure 2
temp2 <- copy(cdc.rec.d)
temp2[deaths<10,deaths:=10]
temp2[deaths_lower<10,deaths_lower:=10]
ggs2 <- ggplot(temp2[year>2013],aes(y=deaths,ymin=deaths_lower,ymax=deaths_upper,x=month,color=factor(year),fill=factor(year))) +
  geom_segment(size=1.5,aes(y=deaths_lower,yend=deaths_upper,x=month,xend=month,color=factor(year)))+
  geom_line(size=1,alpha=.5)+ facet_wrap(~Division,scales="free") +
  geom_point(size=2,shape=21,color="black") + 
  theme_bw() +
  #scale_y_continuous(limits=c(3000,9500),breaks=seq(0,9000,1000))+
  scale_x_continuous(breaks=seq(1,12,1),labels=month.abb)+
  scale_color_brewer(name="",type="div",palette = 9,direction = -1,guide = guide_legend(reverse = TRUE))+
  scale_fill_brewer(name="",type="div",palette = 9,direction=-1,guide = guide_legend(reverse = TRUE)) +
  labs(y="Overdose Deaths",x="Month",title="Monthly Overdose Deaths by Census Division")+
  theme(legend.position = "top", strip.background  = element_blank(),
        legend.background = element_blank())+
  theme(plot.title = element_text(size=12, face="bold"),
        axis.text=element_text(size=12,face="bold"),
        axis.title=element_text(size=12,face="bold")
  )

pdf(paste0(root,"/visuals/CDC_Recovery_supp_figure2_",Sys.Date(),".pdf"),height=12,width=16)
ggs2
dev.off()

#supplemental table 1
cdc.rec[,loc:=State_Name]
cdc.rec.d[,loc:=Division]
cdc.rec.n[,loc:="National"]

prnt <- rbind(cdc.rec[year==2020],cdc.rec.d[year==2020],cdc.rec.n[year==2020],fill=T)

prnt[,deaths:=paste0(round(deaths))]
prnt[,deaths_lower:=paste0(round(deaths_lower))]
prnt[,deaths_upper:=paste0(round(deaths_upper))]

prnt[deaths%in%seq(1,9),deaths:="<10"]
prnt[deaths_lower%in%seq(1,9),deaths_lower:="<10"]
prnt[deaths_upper%in%seq(1,9),deaths_upper:="<10"]

prnt[,prnt:=paste0(deaths," (",deaths_lower," - ",deaths_upper,")")]

prnt <- prnt[order(as.numeric(deaths))]
prnt[,tot:=sum(as.numeric(deaths,na.rm=T)),by=.(loc)]
prnt <- dcast.data.table(prnt,loc~month,value.var = "prnt")

names(prnt) <- c("Location","January 2020","February 2020","March 2020","April 2020","May 2020")

prnt<- prnt[seq(dim(prnt)[1],1),]

write.csv(prnt,paste0(root,"/visuals/CDC_Recovery_supp_table1_",Sys.Date(),".csv"))