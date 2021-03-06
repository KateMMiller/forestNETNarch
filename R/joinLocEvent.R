#' @title joinLocEvent: merges Location and Event level data with options for filtering.
#'
#' @importFrom dplyr select filter arrange mutate summarise group_by between
#' @importFrom magrittr %>%
#' @importFrom stringr str_pad str_sub
#'
#' @description This function combines location and event data. Must run importData first.
#'
#' @param park Combine data from all parks or one park at a time. Acceptable options are:
#' \describe{
#' \item{"all"}{Includes all parks in the network}
#' \item{"ACAD"}{Acadia NP only}
#' \item{"MABI"}{Marsh-Billings-Rockefeller NHP only}
#' \item{"MIMA"}{Minute Man NHP only}
#' \item{"MORR"}{Morristown NHP only}
#' \item{"ROVA"}{Roosevelt-Vanderbilt NHS only}
#' \item{"SAGA"}{Saint-Gaudens NHS only}
#' \item{"SARA"}{Saratoga NHP only}
#' \item{"WEFA"}{Weir Farm NHS only}}
#' @param from Year to start analysis, ranging from 2006-2019
#' @param to Year to stop analysis, ranging from 2006-2019
#' @param QAQC Allows you to remove or include QAQC events.
#' \describe{
#' \item{FALSE}{Default. Only returns visits that are not QAQC visits}
#' \item{TRUE}{Returns all visits, including QAQC visits}}
#' @param rejected Allows you to remove (FALSE) or include (TRUE) rejected plots.
#' \describe{
#' \item{FALSE}{Default. Only returns plots that were not rejected.}
#' \item{TRUE}{returns all records}}
#' @param locType Allows you to only include plots that are part of the GRTS sample design or include all plots, such as deer exclosures
#' \describe{
#' \item{"VS"}{Only include plots that are part of the Vital Signs GRTS sample design}
#' \item{"all"}{Include all plots, such as deer exclosures and bonus plots}}
#' @param eventType Allows you to only include complete sampling events, or to include all sampling events
#' \describe{
#' \item{"complete"}{Only include sampling events for a plot that are complete.}
#' \item{"all}{Include all plots with an Event_ID, including plots that are missing all data associated with that event (eg ACAD-029.2010).}
#' }
#' @param panels Allows you to select individual panels from 1 to 4. Default is all 4 panels (1:4).
#' If more than one panel is selected, specify by c(1,3), for example.
#'
#' @return returns a dataframe with location and visit events
#'
#' @examples
#' importCSV('./forest_csvs')
#' # Select most recent survey of data from WEFA
#' WEFA_data <- joinLocEvent(park = 'WEFA', panels = c(2,4), from = 2015, to = 2018)
#'
#' # Select data from cycle 3
#' cycle3 <- joinLocEvent(from = 2014, to = 2017) # all parks is default
#'
#' # Select data from plots that had a QA/QC event in ACAD in 2018
#' ACAD_data<-joinLocEvent(park = 'ACAD', QAQC = T, from = 2018)
#' QAQC_plots<-ACAD_data$Plot_Name[which(ACAD_data$Event_QAQC==TRUE)]
#' ACAD_QAQC<-ACAD_data %>% filter(Plot_Name %in% QAQC_plots) %>% droplevels()
#'
#' @export
#'

#------------------------
# Joins tbl_Locations and tbl_Events tables and filters by park, year, and plot/visit type
#------------------------
joinLocEvent<-function(park="all", from=2006,to=2019, QAQC=FALSE, rejected=FALSE, panels=1:4,
                       locType='VS', eventType=c('complete','all'), output='short', ...){

  eventType<-match.arg(eventType)

  loc2<-loc %>% mutate(Unit_Code=as.factor(str_sub(Unit_ID,1,4)))
  loc2$Plot_Number<-str_pad(loc2$Plot_Number,width=3,side="left",pad=0) #Pad plot number so retains 3-digits
  loc2$Plot_Name<-paste(loc2$Unit_Code, loc2$Plot_Number, sep="-")

  loc3<- if (locType=='VS') {filter(loc2,Loc_Type=="VS") %>% droplevels()
  } else if (locType=='all') {(loc2)
  } else if (locType!='VS'|locType!='all') {stop("locType must either be 'VS' or 'all'")}

  loc4<- if (rejected==FALSE) {filter(loc3, Rejected==F)
  } else if (rejected==TRUE) {(loc3)
  } else {stop("rejected must be TRUE or FALSE")}

  loc5<- if (park=='all') {(loc4)
  } else if (park %in% levels(loc4$Unit_Code)){filter(loc4,Unit_Code==park)
  } else {stop("park must be one of the factor levels of Unit_Code")}

  park.ev<-merge(loc5,event,by="Location_ID",all.x=T)

  park.ev2<- if (QAQC==FALSE) {filter(park.ev, Event_QAQC==0)
  } else if (QAQC==TRUE) {(park.ev)
  } else {stop("QAQC must be TRUE or FALSE")}

  park.ev3<- if (eventType=='complete') {filter(park.ev2, !(Plot_Name=='ACAD-029' & Start_Date =='2010-07-07')) %>% droplevels()
                                  #Event_ID=='3BF64BE2-7089-42B6-B610-09B3511BF1B4')) %>% droplevels()
  } else {park.ev2}

  park.ev4<- park.ev3 %>% filter(Panel %in% panels) %>% droplevels()
  park.ev4$Year <- as.numeric(format(as.Date(park.ev4$Start_Date, format = "%Y-%m-%d"), "%Y"))

  park.ev5<- park.ev4 %>% mutate(cycle=ifelse(Year<=2009,1,
    ifelse(Year>=2010 & Year<=2013,2,
      ifelse(between(Year,2014,2017),3,ifelse(between(Year,2018,2021),4,NA))))) %>%
    filter(Year>=from & Year <=to) %>% droplevels()

  park.plots<- if (output=='short') {park.ev5 %>% select(Location_ID,Event_ID,Unit_Code,
    Plot_Name, Plot_Number, X_Coord, Y_Coord, Panel, Year, Event_QAQC, cycle)
  } else if (output=='verbose') {park.ev5 %>% select(Location_ID:Y_Coord,Coord_Units:Physiographic_Class,
    Plot_Name,Unit_Code:Start_Date,Event_QAQC, Year, cycle)}

  return(data.frame(park.plots))
} # end of function

