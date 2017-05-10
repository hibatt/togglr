library(togglr)
library(lubridate)
library(purrr)
library(chron)


#set api token
options(toggl_api_token = "")



hrs_per_day<-9
month_start<-"4/1/17"
month_end<-"4/30/17"
time_zone<-"America/Chicago"
workspace_name="Education Analytics"



#time split data -- currently unit free -- later on the script calculates ratios out of these
percentages<-rbind(
c("SEL Research"                              , 20  ),
c("Dev Database"                              , 0  ),
    
c("CORE General Analytics Support"            , 5  ),
c("CORE Growth Model"                         , 1  ),
c("CORE Human Capital Analytics"              , 7  ),
c("Hillsborough Contracted Research"          , 3  ),
c("Virginia Growth"                           , 6  ),
c("Linked Learning Alliance"                  , 11  ),
c("Hillsborough Growth"                       , 2  ),
c("IHE"                                       , 2  ),
    
c("Business Development"                      , 27  ),
c("Operations Management"                     , 50  ),
c("Strategic Management"                      , 5  ),
c("Staff Management"                          , 9  ),
c("Professional Planning and Review"          , 0  ),
c("EA Staff Meeting"                          , 4  ),
c("Board Management"                          , 0  ),
    
c("Internal Research"                         , 1  ),
c("General Admin"                             , 5  ))

######################
#pull data from toggl
######################
#pull the workspace metadata
wk<-get_workspaces()
#get workspace ID
wk_id<-wk[[grep("Education Analytics",wk)]]$id
#pull project list and metadata
prj<-get_projects(wk_id)
usr<-get_users(wk_id)

colnames(percentages)<-c("project","percentage")
percentages<-as.data.frame(percentages)

#normalize percentages
percentages$percentage<-as.numeric(as.character(percentages$percentage))
percentages$percentage<-percentages$percentage/sum(percentages$percentage)

#calculate duration in minutes
percentages$duration<-chron(times=percentages$percentage*hrs_per_day/24)

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
percentages

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
      Sys.sleep(1)
    }
  }
}
