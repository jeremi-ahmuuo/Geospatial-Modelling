


library(sf) #contains the st_read() for reading shapes
library(tmap) #For plotting
library(dplyr) #Data manipulation

tmap_mode('plot') #Setting the mode to plot, to produce static plots

data <- read.csv("E:/Jeremiah/data.csv") # Reading the kdhs dataset
shapes <- st_read("D:/My_programs/My_Projects/R/Data Visualisation/Geospatial visualisation/Kenya/counties/County.shp") # Loading the shapes

## Reading layer `County' from data source 
##   `D:\My_programs\My_Projects\R\Data Visualisation\Geospatial visualisation\Kenya\counties\County.shp' 
##   using driver `ESRI Shapefile'
## Simple feature collection with 47 features and 8 fields
## Geometry type: MULTIPOLYGON
## Dimension:     XY
## Bounding box:  xmin: 33.91182 ymin: -4.702271 xmax: 41.90626 ymax: 5.430648
## Geodetic CRS:  WGS 84


#Poorest
poorest_deaths <- data %>% 
  filter(v190=='poorest') %>% #Filtering only those who are poorest
  group_by(COUNTY) %>% #Grouping the data by counties
  
  summarise(deaths = sum(infant_mortality=='Dead'), #Finding the total number of deaths in each county
            total = n()) %>%  
  mutate(prop = round((deaths/total)*100,2)) %>% 
  mutate(label = case_when(prop>=5~COUNTY,TRUE~"")) #Computing proportions and creating labels

#Ploting

poorest_merged <- inner_join(shapes,poorest_deaths,join_by(COUNTY)) #Merging those who were poorest and dead with the shape files

p1 <- tm_shape(poorest_merged)+
  tm_polygons(col = 'prop',palette='Reds')+
  tm_text(text='label',size = .5,col='black')+
  tm_layout(
    inner.margin = c(0,0,0,0),
    title = 'Poorest',
    title.position = c(.4,.95),
    title.fontface = 'bold',
    legend.height = .3,
    legend.position = c('left','bottom')
  )
p1

#Poorer

poorer_deaths <- data %>% 
  filter(v190=='poorer') %>% 
  group_by(COUNTY) %>% 
  
  summarise(deaths = sum(infant_mortality=='Dead'),
            total = n()) %>% 
  mutate(prop = round((deaths/total)*100,2)) %>% 
  mutate(label = case_when(prop>=5~COUNTY,TRUE~""))


poorer_merged <- inner_join(shapes,poorer_deaths,join_by(COUNTY))

p2 <- tm_shape(poorer_merged)+
  tm_polygons(col = 'prop',palette='Reds')+
  tm_text(text='label',size = .5,col='black')+
  tm_layout(
    inner.margin = c(0,0,0,0),
    title = 'Poorer',
    title.position = c(.4,.95),
    title.fontface = 'bold',
    legend.height = .3,
    legend.position = c('left','bottom')
  )
p2

#Middle

middle_deaths <- data %>% 
  filter(v190=='middle') %>% 
  group_by(COUNTY) %>% 
  
  summarise(deaths = sum(infant_mortality=='Dead'),
            total = n()) %>% 
  mutate(prop = round((deaths/total)*100,2)) %>% 
  mutate(label = case_when(prop>=5~COUNTY,TRUE~""))


middle_merged <- inner_join(shapes,middle_deaths,join_by(COUNTY))

p3 <- tm_shape(middle_merged)+
  tm_polygons(col = 'prop',palette='Reds')+
  tm_text(text='label',size = .5,col='black')+
  tm_layout(
    inner.margin = c(0,0,0,0),
    title = 'Middle',
    title.position = c(.4,.95),
    title.fontface = 'bold',
    legend.height = .3,
    legend.position = c('left','bottom')
  )
p3

#Richer

richer_deaths <- data %>% 
  filter(v190=='richer') %>% 
  group_by(COUNTY) %>% 
  
  summarise(deaths = sum(infant_mortality=='Dead'),
            total = n()) %>% 
  mutate(prop = round((deaths/total)*100,2)) %>% 
  mutate(label = case_when(prop>=5~COUNTY,TRUE~""))


richer_merged <- inner_join(shapes,richer_deaths,join_by(COUNTY))

p4 <- tm_shape(richer_merged)+
  tm_polygons(col = 'prop',palette='Reds')+
  tm_text(text='label',size = .5,col='black')+
  tm_layout(
    inner.margin = c(0,0,0,0),
    title = 'Richer',
    title.position = c(.4,.95),
    title.fontface = 'bold',
    legend.height = .3,
    legend.position = c('left','bottom')
  )
p4

#Richest

richest_deaths <- data %>% 
  filter(v190=='richest') %>% 
  group_by(COUNTY) %>% 
  summarise(deaths = sum(infant_mortality=='Dead'),
            total = n()) %>% 
  mutate(prop = round((deaths/total)*100,2)) %>% 
  mutate(label = case_when(prop>=5~COUNTY,TRUE~""))


richest_merged <- inner_join(shapes,richest_deaths,join_by(COUNTY))

p5 <- tm_shape(richest_merged)+
  tm_polygons(col = 'prop',palette='Reds')+
  tm_text(text='label',size = .5,col='black')+
  tm_layout(
    inner.margin = c(0,0,0,0),
    title = 'Richest',
    title.position = c(.4,.95),
    title.fontface = 'bold',
    legend.height = .3,
    legend.position = c('left','bottom')
  )

p5

 #Age groups
 
 # 15_19

group_15_19_deaths <- data %>% 
  filter(v013=='15-19') %>% 
  group_by(COUNTY) %>% 
  
  summarise(deaths = sum(infant_mortality=='Dead'),
            total = n()) %>% 
  mutate(prop = round((deaths/total)*100,2)) %>% 
  mutate(label = case_when(prop>=5~COUNTY,TRUE~""))


group_15_19_deaths_merged <- inner_join(shapes,group_15_19_deaths,join_by(COUNTY))

g1 <- tm_shape(group_15_19_deaths_merged)+
  tm_polygons(col = 'prop',palette='Reds')+
  tm_text(text='label',size = .5,col='black')+
  tm_layout(
    inner.margin = c(0,0,0,0),
    title = '15-19',
    title.position = c(.4,.95),
    title.fontface = 'bold',
    legend.height = .3,
    legend.position = c('left','bottom')
  )
g1

# 20-24

group_20_24_deaths <- data %>% 
  filter(v013=='20-24') %>% 
  group_by(COUNTY) %>% 
  
  summarise(deaths = sum(infant_mortality=='Dead'),
            total = n()) %>% 
  mutate(prop = round((deaths/total)*100,2)) %>% 
  mutate(label = case_when(prop>=5~COUNTY,TRUE~""))


group_20_24_deaths_merged <- inner_join(shapes,group_20_24_deaths,join_by(COUNTY))

g2 <- tm_shape(group_20_24_deaths_merged)+
  tm_polygons(col = 'prop',palette='Reds')+
  tm_text(text='label',size = .5,col='black')+
  tm_layout(
    inner.margin = c(0,0,0,0),
    title = '20-24',
    title.position = c(.4,.95),
    title.fontface = 'bold',
    legend.height = .3,
    legend.position = c('left','bottom')
  )
g2

# 25_29

group_25_29_deaths <- data %>% 
  filter(v013=='25-29') %>% 
  group_by(COUNTY) %>%
  summarise(deaths = sum(infant_mortality=='Dead'),
            total = n()) %>% 
  mutate(prop = round((deaths/total)*100,2)) %>% 
  mutate(label = case_when(prop>=5~COUNTY,TRUE~""))


group_25_29_deaths_merged <- inner_join(shapes,group_25_29_deaths,join_by(COUNTY))

g3 <- tm_shape(group_25_29_deaths_merged)+
  tm_polygons(col = 'prop',palette='Reds')+
  tm_text(text='label',size = .5,col='black')+
  tm_layout(
    inner.margin = c(0,0,0,0),
    title = '25_29',
    title.position = c(.4,.95),
    title.fontface = 'bold',
    legend.height = .3,
    legend.position = c('left','bottom')
  )
g3

# 30-34

group_30_34_deaths <- data %>% 
  filter(v013=='30-34') %>% 
  group_by(COUNTY) %>% 
  
  summarise(deaths = sum(infant_mortality=='Dead'),
            total = n()) %>% 
  mutate(prop = round((deaths/total)*100,2)) %>% 
  mutate(label = case_when(prop>=5~COUNTY,TRUE~""))


group_30_34_deaths_merged <- inner_join(shapes,group_30_34_deaths,join_by(COUNTY))

g3 <- tm_shape(group_30_34_deaths_merged)+
  tm_polygons(col = 'prop',palette='Reds')+
  tm_text(text='label',size = .5,col='black')+
  tm_layout(
    inner.margin = c(0,0,0,0),
    title = '30-34',
    title.position = c(.4,.95),
    title.fontface = 'bold',
    legend.height = .3,
    legend.position = c('left','bottom')
  )
g3

# 35-39

group_35_39_deaths <- data %>% 
  filter(v013=='35-39') %>% 
  group_by(COUNTY) %>% 
  
  summarise(deaths = sum(infant_mortality=='Dead'),
            total = n()) %>% 
  mutate(prop = round((deaths/total)*100,2)) %>% 
  mutate(label = case_when(prop>=5~COUNTY,TRUE~""))


group_35_39_deaths_merged <- inner_join(shapes,group_35_39_deaths,join_by(COUNTY))

g4 <- tm_shape(group_35_39_deaths_merged)+
  tm_polygons(col = 'prop',palette='Reds')+
  tm_text(text='label',size = .5,col='black')+
  tm_layout(
    inner.margin = c(0,0,0,0),
    title = '35-39',
    title.position = c(.4,.95),
    title.fontface = 'bold',
    legend.height = .3,
    legend.position = c('left','bottom')
  )
g4

# 40-44

group_40_44_deaths <- data %>% 
  filter(v013=='40-44') %>% 
  group_by(COUNTY) %>% 
  
  summarise(deaths = sum(infant_mortality=='Dead'),
            total = n()) %>% 
  mutate(prop = round((deaths/total)*100,2)) %>% 
  mutate(label = case_when(prop>=5~COUNTY,TRUE~""))


group_40_44_deaths_merged <- inner_join(shapes,group_40_44_deaths,join_by(COUNTY))

g5 <- tm_shape(group_40_44_deaths_merged)+
  tm_polygons(col = 'prop',palette='Reds')+
  tm_text(text='label',size = .5,col='black')+
  tm_layout(
    inner.margin = c(0,0,0,0),
    title = '40-44',
    title.position = c(.4,.95),
    title.fontface = 'bold',
    legend.height = .3,
    legend.position = c('left','bottom')
  )
g5

# 45-49

group_45_49_deaths <- data %>% 
  filter(v013=='45-49') %>% 
  group_by(COUNTY) %>% 
  
  summarise(deaths = sum(infant_mortality=='Dead'),
            total = n()) %>% 
  mutate(prop = round((deaths/total)*100,2)) %>% 
  mutate(label = case_when(prop>=5~COUNTY,TRUE~""))


group_45_49_deaths_merged <- inner_join(shapes,group_45_49_deaths,join_by(COUNTY))

g6 <- tm_shape(group_45_49_deaths_merged)+
  tm_polygons(col = 'prop',palette='Reds')+
  tm_text(text='label',size = .5,col='black')+
  tm_layout(
    inner.margin = c(0,0,0,0),
    title = '45-49',
    title.position = c(.4,.95),
    title.fontface = 'bold',
    legend.height = .3,
    legend.position = c('left','bottom')
  )
g6

Geographical distributions

regions <- data %>% group_by(COUNTY) %>% summarise(deaths = sum(infant_mortality=='Dead'), total = n()) %>% mutate(prop = round((deaths/total)*100,2)) %>% mutate(label = case_when(prop>=4~COUNTY,TRUE~""))

regions_merged <- inner_join(shapes,regions,join_by(COUNTY))

region_plot <- tm_shape(regions_merged)+ tm_polygons(col = 'prop',palette='Reds')+ tm_text(text='label',size = .5,col='black')+ tm_layout( inner.margin = c(0,0,0,0), title = 'Counties', title.position = c(.4,.95), title.fontface = 'bold', legend.height = .3, legend.position = c('left','bottom'))
region_plot

