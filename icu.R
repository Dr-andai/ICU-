#ICU Beds Data Analysis

#set wd
setwd('C:/Users/user/Documents/R/icu')

#load libraries
pacman::p_load(
  rio,        # importing data  
  mice,        # missing data imputation
  janitor,    # data cleaning and tables
  tidyverse,   # data management and visualization
  ggthemes, # setting theme
  systemfonts, #load fonts
  ragg
)


myColours = c("#0474ed", "#91f0fa", "#08a29e", "#a2b458", "#a6cabd", "#326164")
myColours1 = c("#0474ed")
myColours2 = c("#16c2d5", "#89dee2", "#10217d", "#527c88", "#d7baad", "#2e4450", 
               "#326164")
myColours3 = c("#314e3c", "#a7abc9", "#6c6c96", "#282063", "#4d3ec0","#acc6d8",
               "#273c54", "#d099a0", "#f0ac7c", "#d7baad", "#527c88", "#0474ed", 
               "#a2b458", "#f45c2c")

#import data
icu <- import('number-of-icu-beds-per-county-1.xlsx')
icu
#removed first row
icu <- icu[-c(1),]
icu

#change column names
icu <- icu %>%
  janitor::clean_names()%>%
  rename(County = number_of_hospital_icu_beds_per_county,
         Hospital = x2,
         Number = x3)
icu

#Visualized the Data set
ggplot(data = icu, aes(x = County, fill =County )) +
  geom_bar(position = "dodge")

#changed Number column to numeric
icu <- transform (icu, Number = as.numeric(Number))

summary(icu)
#County            Hospital             Number      
#Length:99          Length:99          Min.   : 0.000  
#Class :character   Class :character   1st Qu.: 0.000  
#Mode  :character   Mode  :character   Median : 4.000  
 #                                     Mean   : 5.286  
  #                                    3rd Qu.: 6.000  
  #                                    Max.   :55.000  
   #                                   NA's   :1 

sum(icu$Number, na.rm = TRUE) 
#518 ICU Beds

## Analysis of the most highest count, Nairobi
#select hospitals in Nairobi
Nairobi <- icu[icu$County == 'Nairobi',]
## create id column
id <- c(1:24)
Nairobi <- cbind(Nairobi, id)


#create circular plot for Nairobi
P <- ggplot (data = Nairobi, 
             aes(x=as.factor(id), y= Number))+
  geom_bar (stat= 'identity', fill = alpha ("blue", 0.8))+
  ylim (-100,120)+
  
  #custom the theme
  theme_minimal()+
  theme (
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-2,6), "cm")
  )+
  coord_polar(start = 0) +
  
  # create labels to the data
  Nairobi_label <- Nairobi

  number_of_bar <- nrow(Nairobi_label)
  angle <- 90-360 * (Nairobi_label$id-0.5)/number_of_bar

  Nairobi_label$hjust <- ifelse(angle < -90, 1, 0)
  Nairobi_label$angle <- ifelse(angle < -90, angle+180, angle)
  
  #add labels
  geom_text(data = Nairobi_label,
            aes(id, Number +10,
                label = Hospital,
                hjust = hjust),
            color = "black",
            fontface = "bold",
            alpha = 0.6,
            size = 3,
            angle = Nairobi_label$angle,
            inherit.aes = FALSE)
P


#Plot each region and number of hospitals per county
# Nairobi Hospitals
Nairobi %>%
  ggplot (aes(x = Number, y = Hospital, fill = County))+
  geom_bar(stat = "identity", fill = myColours1)+
  labs (title = "Number of ICU beds in Nairobi",
        y = "Hospital",
        x = "No. of Beds",
        fill = "County")+
  scale_x_continuous(breaks = seq(0,60,10))+
  theme_tufte()+
  theme (axis.title = element_text())

sum(Nairobi$Number, na.rm = TRUE)#258


# Coast Hospitals
Coast <- icu [icu$County %in% c("Mombasa", "Lamu", "Kilifi", "Tana River", 
                       "Taita Taveta","Kwale"),]
Coast %>%
  ggplot (aes(x = Number, y = Hospital, fill = County))+
  geom_bar(stat = "identity")+
  labs (title = "Number of ICU beds per Hospital in Coast",
        y = "Hospital",
        x = "No. of Beds",
        fill = "County")+
  theme_fivethirtyeight()+
  theme (axis.title = element_text())+
  scale_fill_manual(values= myColours2)+
  scale_x_continuous(breaks = seq(0,40,2))
sum(Coast$Number, na.rm = TRUE) #42
 

# North Eastern
N_Eastern <- icu [icu$County %in% c("Garissa", "Wajir", "Mandera"),]
N_Eastern %>%
  ggplot (aes(x = Number, y = Hospital, fill = County))+
  geom_bar(stat = "identity")+
  labs (title = "Number of ICU beds per Hospital in N_Eastern",
        y = "Hospital",
        x = "No. of Beds",
        fill = "County")+
  theme_fivethirtyeight()+
  theme (axis.title = element_text())+
  scale_fill_manual(values= myColours2)+
  scale_x_continuous(breaks = seq(0,40,2))
sum(N_Eastern$Number, na.rm = TRUE) #6


# Eastern
Eastern <- icu [icu$County %in% c("Marsabit", "Isiolo", "Meru", "Tharaka -Nithi",
                                  "Embu", "Kitui", "Machakos", "Makuenui"),]
Eastern %>%
  ggplot (aes(x = Number, y = Hospital, fill = County))+
  geom_bar(stat = "identity")+
  labs (title = "Number of ICU beds per Hospital in Eastern",
        y = "Hospital",
        x = "No. of Beds",
        fill = "County")+
  theme_fivethirtyeight()+
  theme (axis.title = element_text())+
  scale_fill_manual(values= myColours2)+
  scale_x_continuous(breaks = seq(0,10,2))
sum(Eastern$Number, na.rm = TRUE) #16

#Central
Central <- icu [icu$County %in% c("Nyandarua", "Nyeri","Kirinyaga", "Murang'a",
                                  "Kiambu"),]
Central %>%
  ggplot (aes(x = Number, y = Hospital, fill = County))+
  geom_bar(stat = "identity")+
  labs (title = "Number of ICU beds per Hospital in Central",
        y = "Hospital",
        x = "No. of Beds",
        fill = "County")+
  theme_fivethirtyeight()+
  theme (axis.title = element_text())+
  scale_fill_manual(values= myColours2)+
  scale_x_continuous(breaks = seq(0,10,2))
sum(Central$Number, na.rm = TRUE) #41


#Rift Valley
Rift_Valley <- icu [icu$County %in% c("Turkana", "West Pokot", "Samburu", "Trans-Nzoia",
                                      "Uasin Gishu", "Marakwet", "Nandi",
                                      "Baringo", "Laikipia", "Nakuru", "Narok",
                                      "Kajiado", "Kericho", "Bomet"),]
Rift_Valley %>%
  ggplot (aes(x = Number, y = Hospital, fill = County))+
  geom_bar(stat = "identity")+
  labs (title = "Number of ICU beds per Hospital in Rift Valley",
        y = "Hospital",
        x = "No. of Beds",
        fill = "County")+
  theme_fivethirtyeight()+
  theme (axis.title = element_text())+
  scale_fill_manual(values= myColours3)+
  scale_x_continuous(breaks = seq(0,50,2))
sum(Rift_Valley$Number, na.rm = TRUE) #85

#Western
Western <- icu [icu$County %in% c("Kakamega", "Vihiga", "Bungoma", "Busia"),]
Western %>%
  ggplot (aes(x = Number, y = Hospital, fill = County))+
  geom_bar(stat = "identity")+
  labs (title = "Number of ICU beds per Hospital in Western",
        y = "Hospital",
        x = "No. of Beds",
        fill = "County")+
  theme_fivethirtyeight()+
  theme (axis.title = element_text())+
  scale_fill_manual(values= myColours)+
  scale_x_continuous(breaks = seq(0,30,2))
sum(Western$Number, na.rm = TRUE) #32

#Nyanza
Nyanza <- icu [icu$County %in% c("Siaya", "Kisumu", "Homa Bay", "Migori",
                                 "Kisii", "Nyamira"),]
Nyanza %>%
  ggplot (aes(x = Number, y = Hospital, fill = County))+
  geom_bar(stat = "identity")+
  labs (title = "Number of ICU beds per Hospital in Nyanza",
        y = "Hospital",
        x = "No. of Beds",
        fill = "County")+
  theme_fivethirtyeight()+
  theme (axis.title = element_text())+
  scale_fill_manual(values= myColours2)+
  scale_x_continuous(breaks = seq(0,10,2))
sum(Nyanza$Number, na.rm = TRUE) #34
