#Loading the cross sectional csv and the shapes

cross_sectional <- read.csv("crosssectional1.csv")
shapes <- st_read("counties\\County.shp")

## Reading layer `County' from data source 
##   `E:\Jeremiah\assignment\counties\County.shp' using driver `ESRI Shapefile'
## Simple feature collection with 47 features and 8 fields
## Geometry type: MULTIPOLYGON
## Dimension:     XY
## Bounding box:  xmin: 33.91182 ymin: -4.702271 xmax: 41.90626 ymax: 5.430648
## Geodetic CRS:  WGS 84

# Total observations for 2002,2003 and 2007

cross_sectional <- cross_sectional %>% 
  mutate(total_observations =  obs2002+obs2007+obs2009)

#Total expected for 2002,2003 and 2007

cross_sectional <- cross_sectional %>% 
  mutate(total_expected = round(Exp2002+Exp2007+Exp2009))

# Total observations per county

county_observations <- cross_sectional %>% 
  group_by(COUNTY) %>% 
  summarise(observations = sum(total_observations))

#Total expected per county

county_expected <- cross_sectional %>% 
  group_by(COUNTY) %>% 
  summarise(expected = sum(total_expected))

#Merging county observations with shapes

observed_shapes <- inner_join(shapes,county_observations,join_by(COUNTY))

#Merging county expectations with shapes

expected_shapes <- inner_join(shapes,county_expected,join_by(COUNTY))

#Map for the observations

observed_shapes %>%
  tm_shape()+
  tm_polygons(col = "observations",palette="Reds")+
  tm_layout(inner.margin = c(0,0,.04,0),
            legend.height = .24,
            title = "Total observations for the years 2002,2003 and 2007",
            title.position = c(0.02,.98),
            title.size = 1)

#Map for the expected

expected_shapes %>%
  tm_shape()+
  tm_polygons(col = "expected",palette='Reds')+
  tm_layout(inner.margin = c(0,0,.04,0),
            legend.height = .24,
            title = "Total expected for the years 2002,2003 and 2007",
            title.position = c(0.02,.98),
            title.size = 1)

#2002,2003 and 2007 mortality rates

smrs <- cross_sectional %>% 
  select(COUNTY,smr2002,smr2003,smr2007)
smr_shapes <- inner_join(shapes,smrs,join_by(COUNTY))

#SMR map for 2002

smr_shapes %>% 
  tm_shape()+
  tm_polygons(col = "smr2002",palette='Reds',title="smr 2002")+
  tm_layout(inner.margin = c(0,0,0,0),
            legend.height = .24,
            title = "SMR for the year 2002",
            title.position = c(0.3,.98),
            title.size = 1)

#SMR map for 2003

smr_shapes %>% 
  tm_shape()+
  tm_polygons(col = "smr2003",palette='Reds',title="smr 2003")+
  tm_layout(inner.margin = c(0,0,0,0),
            legend.height = .24,
            title = "SMR for the year 2003",
            title.position = c(0.3,.98),
            title.size = 1)

#SMR map for 2007

smr_shapes %>% 
  tm_shape()+
  tm_polygons(col = "smr2007",palette='Reds',title="smr 2007")+
  tm_layout(inner.margin = c(0,0,0,0),
            legend.height = .24,
            title = "SMR for the year 2007",
            title.position = c(0.3,.98),
            title.size = 1)

