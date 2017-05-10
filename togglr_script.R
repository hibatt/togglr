library(togglr)
library(lubridate)
library(purrr)
library(chron)


#set api token
options(toggl_api_token = "")

wk<-get_workspaces()
prj<-get_projects(wk[[1]]$id)

hrs_per_day<-9
month_start<-"5/8/17"
month_end<-"5/8/17"
#time split data
percentages<-rbind(
c("SEL Research"                              ,10),
c("Dev Database"                              ,10),

c("CORE General Analytics Support"            ,10),
c("CORE Growth Model"                         ,10),
c("CORE Human Capital Analytics"              ,10),
c("MAP New York"                              ,0 ),
c("Virginia Growth"                           ,10),
c("Linked Learning Alliance"                  ,0),
c("Delaware School and Teacher Growth"        ,10 ),

c("Business Development"                      ,10),
c("Operations Management"                     ,10),
c("Strategic Management"                      ,10),
c("Staff Management"    ,0),
c("Professional Planning and Review"          ,0),
c("EA Staff Meeting"                          ,0),

c("Internal Research"                         ,0 ),
c("General Admin"                             ,0 ))

colnames(percentages)<-c("project","percentage")

percentages<-as.data.frame(percentages)
percentages$duration<-chron(times=as.numeric(as.character(percentages$percentage))/100*hrs_per_day/24)
for(i in 1:nrow(percentages))
{
  if (i==1)
  {
    percentages$start_time[1]<-chron(times="9:00:00")
    et<-chron(times="9:00:00")+percentages$duration[1]
    percentages$end_time[1]<-et
  }
  else
  {
    percentages$start_time[i]<-percentages$end_time[i-1]
    et<-et+percentages$duration[i]
    percentages$end_time[i]<-et
  }
  
  percentages$project_id[i]<-prj[[grep(percentages$project[i],prj)]]$id  
}

#convert formats and drop 0 effort
percentages$start_time<-as.character(chron(times=percentages$start_time))
percentages$end_time<-as.character(chron(times=percentages$end_time))
percentages<-percentages[percentages$percentage!="0",]
##########################



#define time entry function
time_entry<-function(description,start_date,start_time,end_date,end_time,time_zone="America/Chicago",project_id,workspace_id)
{
  

time_start<-as.POSIXct(paste0(start_date,' ',start_time,collapse=''), format="%Y-%m-%d %H:%M", tz=time_zone)
time_end<-as.POSIXct(paste0(end_date,' ',end_time,collapse=''), format="%Y-%m-%d %H:%M", tz=time_zone)


#convert to gmt for toggl

time_start_g<-format(time_start,tz="GMT")
time_end_g<-format(time_end,tz="GMT")


toggl_create(description, start=time_start_g, stop=time_end_g,workspace_id=workspace_id,project_id=project_id)
}


date_range<-seq(as.POSIXct(month_start,format="%m/%d/%y"),as.POSIXct(month_end,format="%m/%d/%y"),by="days")


for(i in 1:length(date_range))
{
  if (weekdays(date_range[i])!="Sunday"&weekdays(date_range[i])!="Saturday")
  {
    for(j in 1:nrow(percentages))
    {
      time_entry(description='',start_date=as.character(date_range[i]),start_time=percentages$start_time[j],
                 end_date=as.character(date_range[i]),percentages$end_time[j],workspace_id=wk[[1]]$id, 
                 project_id=as.character(percentages$project_id[j]))
    }
  }

}
