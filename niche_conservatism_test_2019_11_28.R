library(BIEN)
library(ape) #Package for working with phylogenies in R
library(maps) #Useful for making quick maps of occurrences
library(sp) #A package for spatial data
library(stringr)
library(rgdal)
library(sp)

test_data<-read.csv("test_data.csv", sep = ";", na.string = "") # species list
map_trf <- readOGR("corlett_shapefiles/corlett_TRF_ext.shp")  # TRF map

map_cont <- readOGR("continents/continent.shp")  # continent map for testing

# Exclude "Unplaced" taxon_status_description
test_data_1 <- test_data[!is.na(test_data$species) & !is.na(test_data$accepted_name_id),] 
target_species <- unique(test_data_1$accepted_name_id)

#ptm <- proc.time() # Start the clock

# set up matrix to fill in results
# for biomes later...
#species_res <- matrix(nrow=length(target_species), ncol=length(grep("FID", names(map_trf))), data=NA)

#species_res <- data.frame(species=target_species, freq_in=rep(NA,length(target_species)))
species_res <- c()
for(i in 1:length(target_species)){
  t <- test_data_1[test_data_1$accepted_name_id==target_species[i],]  # subset to one species
  u <- unique(paste(t$genus, t$species))  # get species binomial name
  occurrence_records <- data.frame()  # setup empty data frame
  for(j in u){
    occurrence_records <- rbind(occurrence_records, (BIEN_occurrence_species(j, only.new.world = F))) 
  }
  ######## Biome analysis ########
  occurrence_records = occurrence_records[!is.na(occurrence_records[,"latitude"]) & !is.na(occurrence_records[,"longitude"]),] # excluding NA coordinates
  occurrence_records = occurrence_records[!duplicated(occurrence_records),] # excluding repeated occurrences
  # Q: Ignore species with one occurrence point (try both with and without this line)
  coord <- occurrence_records[,c("latitude", "longitude")]
  coordinates(coord) <- ~  longitude + latitude  # convert dataframe to spatial points object
  proj4string(coord) <- proj4string(map_trf) # match projection attributes of both objects
  # Q: Ignore points close to biome boundaries (perhaps wait with this)
  
#  trf_res <- over(coord, map_trf) # matches coordinates with shapefile polygons
  # return the number of points in each polygon:
  trf_res <- sapply(over(map_trf, geometry(coord), returnList = TRUE), length) 
  
  # store the results for each species:
  species_res <- c(species_res, sum(trf_res)/length(coord))
#  species_res$freq_in[species_res$species==target_species[i]] <- prop_species_trf
}
# plot(map)
# plot(coord, size=10, add=TRUE, col=c("blue", "red")[as.factor(occurrence_records$scrubbed_species_binomial)])


proc.time() - ptm # Stop the clock

 

############### Script examples. Do not run ############

#download maps
#read maps

map_trf <- readOGR("C:/Users/Emil/Documents/Aarhus_University/Master's_project/Shapefiles/Corlett and Primack/Archive/corlett_TRF_ext.shp") # Retrieve shapefiles from Wolf
#shapefiles for each of the 3 maps should be read, plotted and analyzed
#plot(world, map_trf) - sp?rg asger

coord <- all_occurrence_records[,c("latitude", "longitude")]
coordinates(coord) <- ~ lon + lat  # convert dataframe to spatial points object

proj4string(coord) <- proj4string(map_trf) # match projection attributes of both objects

cont_res <- over(coord, map_trf) # matches coordinates with shapefile polygons


# check results
plot(map_trf)
plot(coord, size=10, add=TRUE, col=c("blue", "red")[as.factor(species)])

# world <- (directory(world_map.shp) #read hypothetical world map
# spTransform(world, crs(+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0)) #transformation af world projektion til match TRF.
# plot(world, map) plot TRF-map over world map

### Niche assignment - Comming soon...

#Ratio of occurrence inside/outside biome to determine niche
#Division of tropical rainforest geographic regions (ex. Neotropics, Africa, Southeast-Asia)


############ Assignment of niche determined species on phylogenetic tree + analysis - Comming soon...