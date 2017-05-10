
#' @title correct_date
#' @description  bidouille pour avoir iso 8601, pas propre mais la tisuit, osef
#' @param time as POSIXt
#' @export
correct_date <- function(time){
  paste0(gsub(" ","T",as.character(time)),"+00:00")
}

#' @title get_toggl_api_token
#' @description  return the toggle api token
#' @export
get_toggl_api_token <- function(){
  getOption("toggl_api_token")
}


#' @title time_entry
#' @description  define time entry function that deals with time zones
#' @export
time_entry<-function(description,start_date,start_time,end_date,end_time,time_zone="America/Chicago",project_id,workspace_id)
{
  
  
  time_start<-as.POSIXct(paste0(start_date,' ',start_time,collapse=''), format="%Y-%m-%d %H:%M", tz=time_zone)
  time_end<-as.POSIXct(paste0(end_date,' ',end_time,collapse=''), format="%Y-%m-%d %H:%M", tz=time_zone)
  
  
  #convert to gmt for toggl
  
  time_start_g<-format(time_start,tz="GMT")
  time_end_g<-format(time_end,tz="GMT")
  
  
  toggl_create(description, start=time_start_g, stop=time_end_g,workspace_id=workspace_id,project_id=project_id)
}
