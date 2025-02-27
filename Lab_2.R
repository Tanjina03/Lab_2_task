library(tidyverse)
library(sf)
library(tmap)
bmps <- read_csv("./BMPreport2016_landbmps.csv")
view(bmps)

#Task 1.1
bmps %>% group_by(StateAbbreviation) %>% summarise(Totalcost= sum(Cost, na.rm= TRUE)) 

# Task 1.2
bmps %>% dplyr::filter(.,Unit=="Acres") %>% 
  ggplot(., aes(x=sqrt(Cost),y=sqrt(TotalAmountCredited))) +
  geom_point()+
  geom_smooth()


bmps %>% dplyr::filter(.,Unit=="Acres") %>% 
  ggplot(., aes(x=log(Cost+1),y=log(TotalAmountCredited+1))) +
  geom_point()+
  geom_smooth()

#Task 1.3
bmps%>% mutate(.,Trimmed=stringr::str_sub(BMPShortName, 1,9))%>% filter(.,Trimmed=="covercrop")%>% 
   ggplot(., aes(x=StateAbbreviation,y=TotalAmountCredited, fill=StateAbbreviation)) +
  geom_boxplot()

#Task 1.4
dams <- sf::read_sf("./Dam_or_Other_Blockage_Removed_2012_2017.shp") %>% 
  sf::st_make_valid()

dams %>% dplyr::filter(.,YEAR!=0) %>% 
  ggplot(., aes(x=YEAR,y=STATE)) +
  geom_point()

#Task-1.5
join_table<- left_join(dams, bmps, by=c("STATE"= "StateAbbreviation"))
View(join_table)

ggplot(join_table, aes(x=DamRemoval,y=STATE, fill=STATE)) +
  geom_boxplot()


counties <- sf::read_sf("./County_Boundaries (1).shp") %>% 
  sf::st_make_valid()
dams <- sf::read_sf("./Dam_or_Other_Blockage_Removed_2012_2017.shp") %>% 
  sf::st_make_valid()
streams <- sf::read_sf("./Streams_Opened_by_Dam_Removal_2012_2017.shp") %>% 
  sf::st_make_valid()



#Task-2.1

streams %>% dplyr::arrange(desc(LengthKM)) %>% head(5)

# Task-2.2
counties %>% sf::st_is_valid()
counties <- counties %>% sf::st_make_valid()

streams.counties <- sf::st_join(streams, counties)
streams.countie_sum <- streams.counties %>% group_by(COUNTYFP10) %>% summarise(TotalLength=sum(LengthKM))
streams.countie_sum %>% dplyr::arrange(desc(TotalLength)) %>% head(3)

# Task-2.3
bmps <- bmps %>% mutate(., FIPS.trimmed = stringr::str_sub(GeographyName, 1, 5))
counties.bmps <- left_join(counties, bmps, by = c("GEOID10" = "FIPS.trimmed"))
glimpse(counties.bmps)
tm_shape(counties.bmps) + tm_polygons(fill = "Cost")

