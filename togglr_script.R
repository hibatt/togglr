library(togglr)
library(lubridate)
library(purrr)
library(chron)


#set api token
options(toggl_api_token = "ee543d211a7de3a8071f3745b4eaa272")



hrs_per_day<-9
month_start<-"5/8/17"
month_end<-"5/8/17"
time_zone<-"America/Chicago"
workspace_name="Education Analytics"



#time split data
percentages<-rbind(
c("SEL Research"                              , 10 ),
c("Dev Database"                              , 10 ),
    
c("CORE General Analytics Support"            , 0  ),
c("CORE Growth Model"                         , 0  ),
c("CORE Human Capital Analytics"              , 0  ),
c("MAP New York"                              , 0  ),
c("Virginia Growth"                           , 0  ),
c("Linked Learning Alliance"                  , 0  ),
c("Delaware School and Teacher Growth"        , 0  ),
    
c("Business Development"                      , 0  ),
c("Operations Management"                     , 0  ),
c("Strategic Management"                      , 0  ),
c("Staff Management"                          , 0  ),
c("Professional Planning and Review"          , 0  ),
c("EA Staff Meeting"                          , 0  ),
    
c("Internal Research"                         , 0  ),
c("General Admin"                             , 0  ))

######################
#pull data from toggl
######################
#pull the workspace metadata
wk<-get_workspaces()
#get workspace ID
wk_id<-wk[[grep("Education Analytics",wk)]]$id
#pull project list and metadata
prj<-get_projects(wk_id)

colnames(percentages)<-c("project","percentage")
percentages<-as.data.frame(percentages)

#calculate duration in minutes
percentages$duration<-chron(times=as.numeric(as.character(percentages$percentage))/100*hrs_per_day/24)

#fill in start times and end times 
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




date_range<-seq(as.POSIXct(month_start,format="%m/%d/%y"),as.POSIXct(month_end,format="%m/%d/%y"),by="days")


for(i in 1:length(date_range))
{
  if (weekdays(date_range[i])!="Sunday"&weekdays(date_range[i])!="Saturday")
  {
    for(j in 1:nrow(percentages))
    {
      time_entry(description='',
        start_date=as.character(date_range[i]),
        start_time=percentages$start_time[j],
        end_date=as.character(date_range[i]),
        end_time=percentages$end_time[j],
        workspace_id=wk_id, 
        project_id=as.character(percentages$project_id[j]),
        time_zone=time_zone
        )
    }
  }
}
