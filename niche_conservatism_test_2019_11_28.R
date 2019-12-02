library(BIEN)
library(ape) #Package for working with phylogenies in R
library(maps) #Useful for making quick maps of occurrences
library(sp) #A package for spatial data
library(stringr)
library(rgdal)
library(sp)


 
test_data<-read.csv("test_data.csv", sep = ";", na.string = "") #species list
cont_map <- readOGR("continents/continent.shp")  # should be TRF map

test_data_1 <- test_data[!is.na(test_data$species) & !is.na(test_data$accepted_name_id),] # Exclude "Unplaced" taxon_status_description

target_species <- unique(test_data_1$accepted_name_id)

ptm <- proc.time() # Start the clock
species_res <- list()

for(i in length(target_species)){
  t <- test_data_1[test_data_1$accepted_name_id==target_species[i],]  # subset to one species
  u <- unique(paste(t$genus, t$species))  # get species binomial name
  occurrence_records <- data.frame()  # setup empty data frame
  for(j in u){  # question: should this not be just one entry? is there multiple species names assigned to one id, and if so why? 
    occurrence_records <- rbind(occurrence_records, (BIEN_occurrence_species(j, only.new.world = F))) #download data from BIEN and add to occurrence_records
  }
  # ewww the download is incredibly slow!

  ######## Biome analysis ########
  occurrence_records = occurrence_records[!is.na(occurrence_records[,"latitude"]) & !is.na(occurrence_records[,"longitude"]),] # excluding NA coordinates
  occurrence_records = occurrence_records[!duplicated(occurrence_records),] # excluding repeated occurrences
  # Q: Ignore species with one occurrence point (try both with and without this line)
  coord <- occurrence_records[,c("latitude", "longitude")] ## Q: Are species now completely mixed? A: no, the order in the dataframe is maintained
  coordinates(coord) <- ~  longitude + latitude  # convert dataframe to spatial points object
  proj4string(coord) <- proj4string(cont_map) # match projection attributes of both objects
  # Q: Ignore points close to biome boundaries (perhaps wait with this)
  cont_res <- over(coord, cont_map) # matches coordinates with shapefile polygons
  # Q: calculate proportion of occurrence points in and out of polygon boundaries --> simple table(cont_res)/nrow(cont_res) should do the trick
  # Q: assign species name to be TRF or not based on proportion consensus (several definitions, e.g. 90 % and 50 % or 90 % and 10 % intervals below)
  # Q: seperate for geographic region (World, Asia, Africa, Neo-tropics)
  
  # store the results for each species:
  species_res[i] <- cont_res
}
# plot(cont_map)
# plot(coord, size=10, add=TRUE, col=c("blue", "red")[as.factor(occurrence_records$scrubbed_species_binomial)])


proc.time() - ptm # Stop the clock

 

############### Script examples. Do not run ############

#download maps
#read maps

cont_map <- readOGR("C:/Users/Emil/Documents/Aarhus_University/Master's_project/Shapefiles/Corlett and Primack/Archive/corlett_TRF_ext.shp") # Retrieve shapefiles from Wolf
#shapefiles for each of the 3 maps should be read, plotted and analyzed
#plot(world, cont_map) - sp?rg asger

coord <- all_occurrence_records[,c("latitude", "longitude")]
coordinates(coord) <- ~ lon + lat  # convert dataframe to spatial points object

proj4string(coord) <- proj4string(cont_map) # match projection attributes of both objects

cont_res <- over(coord, cont_map) # matches coordinates with shapefile polygons


# check results
plot(cont_map)
plot(coord, size=10, add=TRUE, col=c("blue", "red")[as.factor(species)])

# world <- (directory(world_map.shp) #read hypothetical world map
# spTransform(world, crs(+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0)) #transformation af world projektion til match TRF.
# plot(world, cont_map) plot TRF-map over world map

### Niche assignment - Comming soon...

#Ratio of occurrence inside/outside biome to determine niche
#Division of tropical rainforest geographic regions (ex. Neotropics, Africa, Southeast-Asia)


############ Assignment of niche determined species on phylogenetic tree + analysis - Comming soon...