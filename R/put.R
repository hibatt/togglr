#' @title toggl_start
#' @description  start a task
#' @param description the task you are doing
#' @param start start time in POSIXt 
#' @param api_token the toggl api token
#' @importFrom lubridate now
#' @importFrom httr POST authenticate content
#' @importFrom magrittr %>%
#' @importFrom jsonlite toJSON
#' @examples 
#' \dontrun{
#' options(toggl_api_token = "XXXXXXXX")# set your api token here
#' toggl_start()
#' }
#' @export
toggl_start <- function(
  description=get_context(),
  start=now(),
  api_token=get_toggl_api_token()){
  if (is.null(api_token)){
    stop("you have to set your api token using options(toggl_api_token = 'XXXXXXXX')")
    
  }
  
  POST("https://www.toggl.com/api/v8/time_entries/start",
       # verbose(),
       authenticate(api_token,"api_token"),
       encode="json",
       body=toJSON(
         list(time_entry = list(description = description,
                                created_with = "togglr",
                                duronly=FALSE)),
         auto_unbox = TRUE)
  ) %>% content() %>% .$data %>% .$id %>% invisible()
  
  if (requireNamespace("notifier", quietly = TRUE)){
    notifier::notify(
    title = paste(description," START")
    ,msg = c("at :",
             as.character(start)
             
    )
  )}
  
  
  
}



#' @title toggl_stop
#' @description  stop the active task
#' @param current list task id and start time
#' @param api_token the toggl api token
#' @importFrom httr PUT
#' @importFrom magrittr %>%
#' @importFrom lubridate ymd_hms
#' @importFrom prettyunits pretty_dt
#' @examples 
#' \dontrun{
#' options(toggl_api_token = "XXXXXXXX")# set your api token here
#' toggl_start()
#' toggl_stop()
#' }
#' @export
toggl_stop <- function(current=get_current(),
                       api_token=get_toggl_api_token()){
  if (is.null(api_token)){
    stop("you have to set your api token using options(toggl_api_token = 'XXXXXXXX')")
    
  }
  if (is.null(current$id)){
    
    stop("i can't find any task to stop...")
    
  }
  PUT(paste0("https://www.toggl.com/api/v8/time_entries/",current$id,"/stop"),
       # verbose(),
       authenticate(api_token,"api_token"),
       encode="json")
  
  if (requireNamespace("notifier", quietly = TRUE)){
    notifier::notify(
    title = paste(current$description," STOP")
    ,msg = c("duration :",
            pretty_dt(now() - ymd_hms(current$start))
            
    )
  )}
  
  
  
}





#' @title toggl_create
#' @description  create a
#' @param start time in POSIXt
#' @param stop time in POSIXt
#' @param duration in seconds
#' @param description the task you did
#' @param api_token the toggl api token
#' @importFrom lubridate now
#' @importFrom httr POST authenticate verbose
#' @importFrom jsonlite toJSON
#' @examples 
#' \dontrun{
#' options(toggl_api_token = "XXXXXXXX")# set your toggl api token here
#' toggl_create(duration=1200)
#' }
#' @export
toggl_create <- function(
  description=get_context(),
  start=now(),
  stop,
  duration,
  api_token=get_toggl_api_token(),workspace_id,project_id=NULL){
  if (is.null(api_token)){
    stop("you have to set your api token using options(toggl_api_token = 'XXXXXXXX')")
    
  }


  if (missing(duration) & missing(stop)){
    stop("You must give at least duration or stop time")
    }

  if (missing(duration)){
    duration <- round(as.numeric(difftime(stop,start,units = "secs")))
  }


  POST("https://www.toggl.com/api/v8/time_entries",
       verbose(),
       authenticate(api_token,"api_token"),
       encode="json",
       body=toJSON(list(time_entry = list(description = description,
                                   wid=workspace_id,
                                   pid=project_id,
                                   created_with = "togglr",
                                   duronly=FALSE,
                                   duration=duration,
                                   start = correct_date(start),
                                   at = correct_date(now())
       )
       ),auto_unbox = TRUE)
  )


}



