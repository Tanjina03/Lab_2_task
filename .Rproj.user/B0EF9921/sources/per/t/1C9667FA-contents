install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
install.packages("sf")
library(sf)
p.counties <- "./County_Boundaries.shp"
p.stations <- "./Non-Tidal_Water_Quality_Monitoring_Stations_in_the_Chesapeake_Bay.shp"
d.counties <- sf::read_sf(p.counties)
d.stations <- sf::read_sf(p.stations) 

#Task-1.1
d1.counties <- d.counties%>%group_by(STATEFP10)%>%mutate(stateLand=sum(ALAND10, AWATER10))
view(d1.counties)
d2.counties <- d1.counties%>%group_by(COUNTYFP10)%>%mutate(countyLandspercent=((ALAND10/stateLand)*100))
view(d2.counties)

#Task-1.2
d1.counties<-d1.counties %>% group_by(COUNTYFP10) %>% mutate(countyWLand =(AWATER10/stateLand))
d1.counties %>% group_by(STATEFP10) %>% dplyr::select(STATEFP10,COUNTYFP10,countyWLand)%>% dplyr::filter(countyWLand == max(countyWLand))

#Task-1.3
d.counties %>% as_tibble() %>% group_by(STATEFP10) %>% summarise(statecounty = n())

#Task- 1.4
d.stations %>% dplyr::slice_min(nchar(STATION_NA)) 

# Task-2.1
d.counties %>% ggplot(., aes(x=ALAND10,y=AWATER10,color = STATEFP10)) +
  geom_point()+
  theme_classic()+
  labs(title = "Land and Water Area of Each County",
       subtitle = "Statewise color variation",
       x= "Land Area",
       y= "water Area")

# Task-2.2
d.stations %>% ggplot(.,aes(x= Drainage_A)) +
  geom_histogram(fill = "dark red",bins=10)+
  theme_minimal()+
  labs(title = "Drainage Area of stations",
       x= "Drainage area")


# Task-2.3 
d.stations %>% sf:: st_is_valid()
d.counties %>% sf:: st_is_valid()
d.counties <- d.counties %>% sf:: st_make_valid()
county.station<- sf::st_intersection(d.counties,d.stations)
ggplot(county.station, aes(x=Drainage_A, fill=STATEFP10))+
  geom_histogram(bins=10)+
  theme_minimal()+
  labs(title= "Drainage Area of Stations",
       x= "Drainage area")

# Task-3
`my_function <- function(numbers){
  mean(numbers)
  median(numbers)
  min(numbers)
  max(numbers)
  sort(numbers, decreasing = FALSE)
  
  if(is.numeric(numbers)== FALSE){
    return("Error")
  }
  else{ 
    return(
      list( mean(numbers),
            median(numbers),
            min(numbers),
            max(numbers),
            sort(numbers, decreasing = FALSE)
      )
    )
  }
}
my_function(c(1,0,-1))
my_function(c(10, 100,1000))
my_function(c(.1, .001, 1e8))
my_function(c("a", "b","c"))
`

# Task-4.1
county.station %>% as_tibble()%>% group_by(STATEFP10)%>% summarise(statestations = n())

# Task-4.2
county.station %>% dplyr::filter(STATEFP10 == 36)%>% group_by(STATEFP10)%>% summarise(avgAcounty=mean(Shape_Area))
d.counties %>% dplyr::filter(STATEFP10 == 36)%>% group_by(STATEFP10)%>% summarise(avgAcounty=mean(Shape_Area)) 


#Task-4.3
avgDrainage<- county.station%>% group_by(STATEFP10)%>% summarise(avgAcounty=mean(Drainage_A))
avgDrainage%>%dplyr :: slice_max(avgDrainage)


