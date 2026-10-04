# Downloading occs
library(rgbif)
setwd("C:/users/ivo/Documents/Projects/quercus-insignis")
species <- "Quercus insignis"
occs_gbif <- occ_search(scientificName = species, return = "data")
nrow(occs_gbif)
dir.create("./data_folder")
write.csv(occs_gbif, "data_folder/occs_gbif.csv", row.names = FALSE)

# GBIF Load, select & clean
library(dplyr)
setwd("C:/users/ivo/Documents/Projects/quercus-insignis")
qi_gbif <- read.csv("./occs/Qinsignis_GBIF_366.csv", header = TRUE)
qi_gbif <- select(qi_gbif, species, countryCode, decimalLatitude, decimalLongitude, basisOfRecord)
glimpse(qi_gbif)

## Duplicates
qi_gbif_c1 <- distinct(qi_gbif, decimalLatitude, decimalLongitude, .keep_all = TRUE)
nrow(qi_gbif)-nrow(qi_gbif_c1) #cat

## NA's
qi_gbif_c2 <- qi_gbif_c1 %>% filter(!is.na(decimalLatitude), !is.na(decimalLongitude))
nrow(qi_gbif_c1)-nrow(qi_gbif_c2)

## Lat/Long (0,0)
qi_gbif_c3 <- qi_gbif_c2 %>% filter(decimalLatitude != '0' & decimalLongitude != '0')
nrow(qi_gbif_c2)-nrow(qi_gbif_c3)

## <= 2 Decimal Places
decimalplaces <- function(x) {
  if (abs(x - round(x)) > .Machine$double.eps^0.5) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}
qi_gbif_c4 <- qi_gbif_c3[sapply(qi_gbif_c3$decimalLongitude, decimalplaces) > 2 &
                             sapply(qi_gbif_c3$decimalLatitude, decimalplaces) > 2, ]
nrow(qi_gbif_c3)-nrow(qi_gbif_c4)

# Out of the region of interest (Mexico & Central America)
qi_gbif_c4$countryCode # or unique(qi_gbif_c4$countryCode)
qi_gbif_c5 <- subset(qi_gbif_c4, countryCode != "FR" & countryCode != "ES")
nrow(qi_gbif_c4)-nrow(qi_gbif_c5)

# GBIF Data Visualization
library(ggplot2)
plot1 <- ggplot(data = qi_gbif_c5) +
  geom_bar(mapping = aes(x = countryCode), fill = "steelblue") +
  xlab("Countries") + 
  ylab("Number of Occs") +
  theme_bw() +
  scale_x_discrete(labels = c("Belice", "Costa Rica", "Guatemala", "Honduras", "México", "Nicaragua", "Panamá"))
plot1 # colors, title(?), names in spanish, order by count(?)
dir.create("./plots")
ggsave("plots/countryCode.png", plot1, width = 5, height = 5)
plot2 <- ggplot(data = qi_gbif_c5) +
  geom_bar(mapping = aes(x = basisOfRecord), fill = "black") +
  xlab("basisOfRecord") +
  ylab("Number of Occs")
plot2
ggsave("plots/basisOfRecord.png", plot2, width = 5, height = 5)
library(patchwork)
plot3 <- plot1 + plot2
plot3
ggsave("plots/countryCode_basisOfRecord.png", plot3, width = 5, height = 5)

# Mapping GBIF Data
library(sf)
vector <- read_sf("./vectors/ne_10m_admin_0_countries.shp")
region <- select(vector, SUBREGION, ADMIN) %>% 
  subset(SUBREGION =="Central America" & ADMIN != "Clipperton Island")
region$SUBREGION <- NULL

ggplot(data = region) +
  geom_sf(color = "black", fill = "green") +
  geom_point(data = qi_gbif_c5, aes(x = decimalLongitude, y = decimalLatitude), size = 4, shape = 23, fill = "red") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Mexico & Central America")

# Load Cacoma & Merge with GBIF
qi_cacoma <- read.csv("occs/cacoma.csv", header = TRUE)
qi_occs <- qi_gbif_c5[,c(1,3,4)]
names(qi_occs) <- c("Species_name", "Latitude", "Longitude")
qi_occs <- rbind(qi_occs, qi_cacoma)
qi_occs$ID <- seq.int(nrow(qi_occs))
qi_occs <- qi_occs[,c(4,1,3,2)]

# Visualizing E & Finding Outliers
## Bio1, Bio12, Elevation (meters) values
## Occs by state (Mexico)
## spThin -- occs final
## M
## 3D plot w/ Bio1, Bio12 & Elevation -- Ecological Space (E)
## View Geographical Space (G)
library(raster)
lst <- list.files(path = "./rasters/bios", pattern = ".tif") # remove "quercus insignis"
lst
bio1 <- raster("./rasters/bios/CHELSA_bio10_1.tif")
bio12 <- raster("./rasters/bios/CHELSA_bio10_12.tif")
nbios <- stack(bio1, bio12)
gmted1 <- raster("./rasters/elevation/10N090W_20101117_gmted_mea300.tif")
gmted2 <- raster("./rasters/elevation/10N120W_20101117_gmted_mea300.tif")
gmted3 <- raster("./rasters/elevation/10S090W_20101117_gmted_mea300.tif")
gmted4 <- raster("./rasters/elevation/30N120W_20101117_gmted_mea300.tif")
elev <- merge(gmted1, gmted2, gmted3, gmted4)
cropped = crop(elev, region)
masked = mask (cropped, region, updatevalue = 0) # ok


plot(bios_crop)
plot(elev_crop)
plot(region, add = TRUE) # error
writeRaster(bios, filename = "./quercus-insignis/bios.tif") # 900 MB

envspecies <- extract(vars,cbind(occs$Longitude, occs$Latitude))
envspecies <- as.data.frame(envspecies)
records <- cbind(occs,envspecies)

# Environmental Predictors -- Selection
# Crop/M
# Correlation Matrix
library(usdm)
vifstep(bios)
bios <- exclude(bios)
bios
# # #
clim2.5 = getData('worldclim', var = 'bio', res = 2.5, download = TRUE, path = 'C:/users/ivo/Documents/Projects/quercus-insignis')
clim5 = getData('worldclim', var = 'bio', res = 5, download = TRUE, path = 'C:/users/ivo/Documents/Projects/quercus-insignis')
clim10 = getData('worldclim', var = 'bio', res = 10, download = TRUE, path = 'C:/users/ivo/Documents/Projects/quercus-insignis')
# # #
wc10 <- list.files("./wc10", pattern = ".bil$", full.names = TRUE)
wc10
pred <- stack(wc10)
names(pred)
plot(pred[[1:4]])
pairs(pred[[1:4]], cex = 0.1)

# Dplyr Experiments
gbif_c3 %>% 
  filter(between(elevation, 800, 3000)) %>%
  filter(order == "Didelphimorphia") %>%
  filter(order %in% c("Didelphimorphia", "Diprotodontia")) %>% # select more than one animal
  filter(bodywt > 100, (sleep_total > 15 | order != "Carnivora")) # multiple conditions
msleep %>%
  select(conservation, sleep_total, name) %>% # re-ordering columns
  select(animal = name, sleep_total, extinction_threat = conservation) %>% # rename columns
  rename(animal = name, extinction_threat = conservation) %>% # rename columns pt2
  glimpse
msleep %>%
  select(name, sleep_total) %>%
  mutate(sleep_total_min = sleep_total * 60) # new column "sleep_total_min"
msleep %>%
  count(order, sort = TRUE) # counting the number of observations
msleep %>%
  group_by(vore) %>%
  summarise(n = n(), average = mean(sleep_total), maximum = max(sleep_total)) # summarizing data
msleep %>%
  group_by(vore) %>%
  summarise(avg_sleep = mean(sleep_total)) %>%
  arrange(desc(avg_sleep)) # arranging rows

#spThin
library(spThin)
plot(occs$Longitude, occs$Latitude)
?thin
thinned_occs <-
  thin(loc.data = occs,
       lat.col = "Latitude", long.col = "Longitude",
       spec.col = "SPEC",
       thin.par = 10, reps = 100,
       locs.thinned.list.return = TRUE,
       write.files = FALSE,
       write.log.file = FALSE)
view(thinned_occs[[1]])
points(thinned_occs[1]$Longitude, thinned_occs[1]$Latitude, col = "red", pch = 20)