# Data set and Packages ----------------------------------------------------
install.packages("WDI")
install.packages("tidyverse")
install.packages("viridis")
install.packages("devtools")
devtools::install_github("ricardo-bion/ggradar",
                         dependencies = TRUE, force=TRUE)

library(WDI)
library(tidyverse)
library(viridis)
library(ggradar)
library(scales)

options(scipen = 999)

new_wdi_cache <- WDIcache()

data <- WDI(country="all",
         indicator = c("EG.ELC.FOSL.ZS",    #Electricity production from oil, gas and coal sources (% of total)
                       "EG.ELC.ACCS.ZS",    #Access to electricity (% of population)
                       "EN.CO2.ETOT.ZS",    #CO2 emissions from electricity and heat production, total (% of total fuel combustion)
                       "EN.ATM.CO2E.GF.ZS", #CO2 emissions from gaseous fuel consumption (% of total)
                       "EN.ATM.CO2E.LF.ZS", #CO2 emissions from liquid fuel consumption (% of total)
                       "EN.ATM.CO2E.SF.ZS", #CO2 emissions from solid fuel consumption (% of total)
                       "EG.ELC.COAL.ZS",    #Electricity production from coal sources (% of total)
                       "EG.ELC.HYRO.ZS",    #Electricity production from hydroelectric sources (% of total)
                       "EG.ELC.NGAS.ZS",    #Electricity production from natural gas sources (% of total)
                       "EG.ELC.NUCL.ZS",    #Electricity production from nuclear sources (% of total)
                       "EG.ELC.PETR.ZS",    #Electricity production from oil sources (% of total)
                       "EG.ELC.RNWX.ZS",    #Electricity production from renewable sources, excluding hydroelectric (% of total)
                       "EG.ELC.RNEW.ZS",    #Renewable electricity output (% of total electricity output)
                       "EG.IMP.CONS.ZS",    #Energy imports, net (% of energy use)
                       "EN.ATM.METH.EG.ZS", #Energy related methane emissions (% of total)
                       "EG.USE.PCAP.KG.OE", #Energy use (kg of oil equivalent per capital)
                       "EG.USE.COMM.FO.ZS", #Fossil fuel energy consumption (% of total)
                       "EG.GDP.PUSE.KO.PP", #GDP per unit of energy use (PPP $ per kg of oil equivalent)
                       "EN.ATM.NOXE.EG.ZS", #Nitrous oxide emissions in energy sector (% of total)
                       "EG.FEC.RNEW.ZS",    #Renewable energy consumption (% of total final energy consumption)
                       "EN.ATM.GHGT.KT.CE"  #Total greenhouse gas emissions (kt of CO2 equivalent)),
         ),
         start = 1990 , end = 2022,
         extra=TRUE,
         cache = new_wdi_cache)

# Exploratory data analysis -----------------------------------------------
summary(data)

#list top rows of data
head(data)

#check distribution
hist(data$EG.FEC.RNEW.ZS)
hist(data$EG.IMP.CONS.ZS)
hist(data$EG.GDP.PUSE.KO.PP)

boxplot(data$EN.ATM.CO2E.GF.ZS)
boxplot(data$EN.ATM.CO2E.LF.ZS)
boxplot(data$EN.ATM.CO2E.SF.ZS)

# Choropleth Map Chart ----------------------------------------------------------------
world_map <- map_data("world")
countryDataWDI <- data %>% mutate(country=recode(str_trim(country),
                                                 "United States"="USA",
                                                 "United Kingdom"="UK",
                                                 "Congo, Dem. Rep."="Democratic Republic of the Congo",
                                                 "Congo, Rep."="Republic of Congo",
                                                 "Kyrgyz Republic"="Kyrgyzstan",
                                                 "Egypt, Arab Rep."="Egypt",
                                                 "Russian Federation"="Russia",
                                                 "Iran, Islamic Rep."="Iran",
                                                 "Venezuela, RB"="Venezuela",
                                                 "Yemen, Rep."="Yemen",
                                                 "Turkiye"="Turkey",
                                                 "Czechia"="Czech Republic",
                                                 "Slovak Republic"="Slovakia",
                                                 "Cote d'Ivoire"="Ivory Coast",
                                                 "Lao PDR"="Laos",
                                                 "Korea, Dem. People's Rep."="North Korea",
                                                 "Korea, Rep."="South Korea"))
countryDataWDI <- left_join(world_map,countryDataWDI, by = c("region"="country"))
ggplot(filter(countryDataWDI, year=="2015"),
       aes(long,lat,group=group))+
  geom_polygon(aes(fill=EG.FEC.RNEW.ZS),colour="white")+
  scale_fill_viridis_c(option = "D", limits=c(0,100), breaks=c(0,25,50,75,100))+
  theme_void()+
  labs(fill="% of\nRenewable \nEnergy \n\n", 
       title="\nWorld Map Coloured by Renewable Energy Consumption in 2015",
       caption="Data source: World Development Indicators\n") +
  theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust=0.5),
        legend.title = element_text(color = "black", size = 14, face = "bold", hjust=0.5),
        legend.text = element_text(color = "black", size = 13, face = "italic"),
        legend.key.height = unit(6,"line"), legend.position = "right", legend.title.align = 0,
        plot.caption = element_text(color="black", size = 12, face="italic"))

# Scatter Chart ------------------------------------------------------------
ggplot(filter(data, (year>=2006 & year<=2015) & !is.na(EG.ELC.FOSL.ZS) & !is.na(EN.CO2.ETOT.ZS) 
              & !is.na(income) & region!="Aggregates" & EG.ELC.RNEW.ZS>0 & income!="Not classified"), 
       aes(x=EG.ELC.RNEW.ZS, y=EN.CO2.ETOT.ZS, color=income)) +
  geom_point(alpha = 0.9, size=2) +
  geom_smooth(se=FALSE)+
  labs(x = "\n Renewable electricity output (% of Total Electricity Output)", 
       y = "CO2 emissions from Electricity Production\n", 
       color = "Countries Income \n", size = "New Cases \n",
       title = "Comparing CO2 Emissions and Renewable Electricity Production from 2006 to 2015\n",
       caption="Data source: World Development Indicators\n") +
  theme_linedraw() +
 theme(plot.title = element_text(color = "black", size = 20, face = "bold", hjust=0.5),
       legend.title = element_text(color = "black", size = 15, face = "bold"),
       legend.text = element_text(color = "black", size = 14, face = "italic"),
       strip.text.x = element_text(color = "white", size = 14),
       axis.title.x.bottom = element_text(color = "black", size = 14, face = "bold"),
       axis.title.y.left = element_text(color = "black", size = 14, face = "bold"),
       axis.text = element_text(color = "black", size = 13, face="bold"),
       legend.position = "bottom",
       plot.caption = element_text(color="black", size = 12, face="italic")) + 
  scale_fill_viridis_d(option = "D", aesthetics = "color")

# Circular Bar Chart ------------------------------------------------------------

# Creating new dataset
data1 <- data.frame(
  country=data$country,
  year = data$year,
  Gaseous_Fuel = data$EN.ATM.CO2E.GF.ZS, #CO2 emissions from gaseous fuel consumption (% of total)
  Liquid_Fuel = data$EN.ATM.CO2E.LF.ZS,  #CO2 emissions from liquid fuel consumption (% of total)
  Solid_Fuel = data$EN.ATM.CO2E.SF.ZS   #CO2 emissions from solid fuel consumption (% of total)
)

#filtering data
data1 <- filter(data1, (country=="India" | country=="United States" | 
                          country=="United Kingdom" | country=="Australia") &
                  (year<=2015 & year>=2011))

# Transform data in a tidy format (long format)
data1 <- data1 %>% gather(key = "observation", value="value", -c(1,2)) 
data1$country[data1$country == "United Kingdom"] <- "UK"
data1$country[data1$country == "United States"] <- "USA"
data1$year <- as.factor(data1$year)
nlevels(data1$year)

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 1
nObsType <- nlevels(as.factor(data1$observation))
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data1$year)*nObsType, ncol(data1)) )
colnames(to_add) <- colnames(data1)
to_add$year <- rep(levels(data1$year), each=empty_bar*nObsType )
data1 <- rbind(data1, to_add)
data1 <- data1 %>% arrange(year, country)
data1$id <- rep( seq(1, nrow(data1)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data <- data1 %>% group_by(id, country) %>% summarize(tot=sum(value))
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- data1 %>% 
  group_by(year) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# Make the plot
p = ggplot(data1) +      
  
  # Add the stacked bar
  geom_bar(aes(x=as.factor(id), y=value, fill=observation), stat="identity", alpha=0.8) +
  scale_fill_viridis(option = "D",discrete=TRUE) +
  labs(fill = "CO2 Emissions From:", title = "Comparing CO2 Emissions from Different Fuel Sources from 2011 to 2015") +
  
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "black", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "black", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "black", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 150, xend = start, yend = 150), colour = "black", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 200, xend = start, yend = 200), colour = "black", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  annotate("text", x = rep(max(data1$id),5), y = c(0, 50, 100, 150, 200), label = c("0 %", "50 %", "100 %", "150 %", "200 %") , color="black", size=5 , angle=95, fontface="bold", hjust=1) +
  
  ylim(-150,max(label_data$tot, na.rm=T)) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() +
  
  # Add labels on top of each bar
  geom_text(data=label_data, aes(x=id, y=tot, label=country, hjust=hjust), color="black", fontface="bold",alpha=0.9, size=7,angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=1 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=year), colour = "black", alpha=0.9, size=7, fontface="bold", inherit.aes = FALSE) +
  theme(plot.title = element_text(color = "black", size = 30, face = "bold", hjust = 0.5),
        legend.title = element_text(color = "black", size = 20, face = "bold"),
        legend.text = element_text(color = "black", size = 19, face = "italic"),
        legend.position = "bottom")

ggsave(p, file="output.png", width=15.2, height=17)

# Radar Chart ------------------------------------------------------------------

x <- data.frame(country = data$country,
                year = data$year,
                Renewable = data$EG.ELC.RNWX.ZS,
                Coal = data$EG.ELC.COAL.ZS,
                Hydroelectric = data$EG.ELC.HYRO.ZS,
                Natural_Gas = data$EG.ELC.NGAS.ZS,
                Nuclear = data$EG.ELC.NUCL.ZS,
                Oil = data$EG.ELC.PETR.ZS)
x <- filter(x, (country=="India" | country=="United Kingdom" | country=="Australia" | country=="United States") &
              year==2014)

x<-x[x$country!="Aggregates",]
x<-na.omit(x)

x = subset(x, select = -c(year) )

x %>% mutate_if(is.numeric, rescale) %>%
  mutate(new_country=str_replace_all(country, " ", "_")) %>%
  group_by(country) %>%
  summarise_if(is.numeric, mean) %>%
  ggradar(axis.label.size = 4,
          legend.text.size = 10,
          legend.title = "Country",
          plot.title = "        Energy Production from Different Sources in 2014",
          legend.position = "bottom", values.radar = c("Min","Avg","Max"))

# Area Chart ----------------------------------------------------------------

xy<-data.frame(region = data$region,
               year = data$year,
               income=data$income,
               country=data$country,
               energy_imports = data$EG.IMP.CONS.ZS,
               acc_elec =  data$EG.ELC.ACCS.ZS)
xy <- filter(xy, region!="Aggregates" & region!="Not Classified" & year<2015 & year>2004 &
               (country=="India"|country=="Australia"|country=="United States"|country=="United Kingdom"))

xy <- na.omit(xy)

ggplot (xy, aes(x=year, y=energy_imports, fill=country))+
  geom_area(alpha=0.8 ) +
  theme(legend.position="right") +
  labs(title="Trends in Energy Imports of countries from 2005 to 2014", 
       x="Years", y="Energy imports, net (% of energy use)\n", fill="Country") +
  theme(axis.text = element_text(size = 5),
        axis.title = element_text(size = 5)) + 
  scale_fill_viridis_d( option="D", aesthetics = "fill", begin = 0, end = 1)+
  theme_minimal()+
  scale_x_continuous(breaks = c(2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014)) +
  theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust=0.5),
        legend.title = element_text(color = "black", size = 14, face = "bold"),
        legend.text = element_text(color = "black", size = 13, face = "italic"),
        strip.text.x = element_text(color = "white", size = 13),
        axis.title.x.bottom = element_text(color = "black", size = 13, face = "bold"),
        axis.title.y.left = element_text(color = "black", size = 13, face = "bold"),
        axis.text = element_text(color = "black", size = 12, face="bold"),
        legend.position = "bottom")
