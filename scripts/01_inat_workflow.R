# Install rinat package
install.packages("rinat")

# Load the package
library(rinat)
library(ggplot2)
library(dplyr)
library(lubridate)
# Retrieve observations for the species (add meta=TRUE to find the total number of records available
observations <- get_inat_obs(taxon_name = "Heterodon", bounds = c(24.523096,-87.634938,31.000888,-80.031362),maxresults=400)
observations<-observations %>% dplyr::filter(captive_cultivated!="true") #remove captive specimens
observations<-observations %>% dplyr::filter(quality_grade=="research") #include only research grade specimens
observations<-observations %>% dplyr::filter(scientific_name!="Heterodon nasicus")#remove record of escaped pet

#visually inspect data locations
us_states <- map_data("state")
florida<-us_states %>% dplyr::filter(region=="florida")

ggplot()+
  geom_polygon(data = florida, aes(x = long, y = lat, group = group),fill="red",color="black", size = 0.1)+
  geom_point(data=observations, aes(x=longitude,y=latitude))+
  theme_classic()+
  theme(axis.title = element_blank(),       
        axis.text = element_blank(),        
        axis.ticks = element_blank(),       
        axis.line = element_blank())

#format date column
observations$observed_on<-as.Date(observations$observed_on,format="%Y-%m-%d")
observations$month<-month(observations$observed_on)

#histogram of dates
observations.summ<-observations %>% dplyr::group_by(month) %>% summarise(n=n())
ggplot(data=observations.summ, aes(x=month,y=n))+
  geom_bar(stat="identity",position="dodge")+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), 
                     labels = c('January', 'February', 'March', 'April', 'May','June', 'July', 'August', 'September', 
                                'October', 'November', 'December'))+
  theme_classic()+
  theme(axis.title = element_blank(),       
       # axis.text = element_blank(),        
        axis.ticks = element_blank(),       
        axis.line = element_blank())
