# Workspace

setwd("./quercus-insignis")

# Load and install packages

install.packages("pacman")
pacman::p_load(dplyr, ggplot2, patchwork, sf, tmap, raster, spThin)

# Import GBIF data

qi_gbif <- read.csv("./occs/Qinsignis_GBIF_366.csv") %>%
  dplyr::select(species, decimalLatitude, decimalLongitude, countryCode, basisOfRecord) %>%
  distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE) %>%
  filter(!is.na(decimalLatitude), !is.na(decimalLongitude)) %>%
  filter(decimalLatitude != '0' & decimalLongitude != '0') %>%
  filter(countryCode != 'ES', countryCode != 'FR')

# Remove occurrencies with <= 2 decimal places on longitude and latitude
# Function taken from

decimalplaces <- function(x) {
  if (abs(x - round(x)) > .Machine$double.eps^0.5) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}

qi_gbif <- qi_gbif[sapply(qi_gbif$decimalLongitude, decimalplaces) > 2 & 
                 sapply(qi_gbif$decimalLatitude, decimalplaces) > 2, ]

# Import Cacoma data

qi_cacoma <- read.csv("./occs/cacoma.csv") %>%
  tibble::add_column(countryCode = 'MX', basisOfRecord = 'HUMAN_OBSERVATION') %>%
  rename(decimalLatitude = Latitude, decimalLongitude = Longitude, species = Species)

# Merge GBIF and Cacoma data

qi_final <- dplyr::union(qi_gbif, qi_cacoma)
qi_final <- tibble::add_column(qi_final, ID = seq(nrow(qi_final))) %>% 
  dplyr::select(ID, species, decimalLongitude, decimalLatitude, countryCode, basisOfRecord)

# Visualize number of occurrences by country

plot_1 <- qi_final %>% count(countryCode, sort = TRUE)
p1_normal_v2 <- ggplot(plot_1, aes(x = reorder(countryCode, -n), y = n)) + 
  geom_bar(stat = "identity", fill = "cornflowerblue", colour = "black") +
  geom_text(aes(label = n), vjust = -0.5, size = 9 / .pt) +
  labs(x = "", y = "Frecuencia") +
  scale_x_discrete(labels = c("México", "Costa Rica", "Nicaragua", "Belice", "Panamá", "Honduras", "Guatemala")) +
  scale_y_continuous(limits = c(0, 85)) +
  theme_classic() +
  theme(text = element_text(size = 11), plot.margin = margin(0, 0, 0, 0))
p1_normal_v2

# Visualize number of occurrences by basis of record

plot_2 <- qi_final %>% count(basisOfRecord, sort = TRUE)
p2_normal_v2 <- ggplot(plot_2, aes(x = reorder(basisOfRecord, -n), y = n)) + 
  geom_bar(stat = "identity", fill = "orange", colour = "black") +
  geom_text(aes(label = n), vjust = -0.5, size = 9 / .pt) +
  labs(x = "", y = "Frecuencia") +
  scale_x_discrete(labels = c("Ejemplar de\nHerbario", "Observación\nHumana", "Desconocida")) +
  scale_y_continuous(limits = c(0, 115)) +
  theme_classic() +
  theme(text = element_text(size = 11), plot.margin = margin(0, 0, 0, 0))
p2_normal_v2

# Final plot

p1_normal_v2 + inset_element(p2_normal_v2, 0.3, 0.3, 1, 1)
p1_normal_v2 + inset_element(p2_normal_v2, 0.3, 0.3, 1, 1) + plot_annotation(tag_levels = "a")

# Load administrative borders and select region of interest

borders <- st_read("./vectors/ne_10m_admin_0_countries.shp")
region <- dplyr::select(borders, SUBREGION, ADMIN) %>%
  filter(SUBREGION == "Central America" & ADMIN != "Clipperton Island")

# Define an equal-area projection
# Create simple feature of occurrences dataset
# Data projection

proj <- "+proj=aea +lon_0=-97.03125 +lat_1=10.9557561 +lat_2=28.8188472 +lat_0=19.8873017 +datum=WGS84 +units=m +no_defs"
borders_proj <- st_transform(borders, proj)
region_proj <- st_transform(region, proj)
occs <- st_as_sf(qi_final, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
occs_proj <- st_transform(occs, proj)

# https://projectionwizard.org/#
# +proj=aea +lon_0=-97.03125 +lat_1=10.9557561 +lat_2=28.8188472 +lat_0=19.8873017 +datum=WGS84 +units=m +no_defs
# 33º 17' 05'' N north
# 6º 29' 24 N south
# 76º 01' 31'' W east
# 118º 02' 14'' W west
# Albers equal-area conic
# Equal-area projection for regional maps with an east-west extent 

# Visualize occurrencies in geographical space 

map1 <- tm_shape(borders_proj, bbox = c(-1873200, -1460437, 2205365, 1509202)) +
  tm_fill(col = "gray95") +
  tm_borders(col = "dimgray", lwd = 2, alpha = 0.6) +
  tm_shape(occs_proj) +
  tm_dots(col = "red", size = 0.13) +
  tm_scale_bar(breaks = c(0, 250, 500),
               text.size = 1, 
               position = c("left", "bottom"))
map1

# Import environmental variables

bios <- list.files("./rasters", pattern = 'CHELSA(.*)tif$', full.names = TRUE)
bios

bios_stack <- stack(bios)

srad <- list.files("./rasters", pattern = 'wc', full.names = TRUE)
srad

srad_stack <- stack(srad)

elev1 <- raster("./rasters/10N090W_20101117_gmted_mea300.tif")
elev2 <- raster("./rasters/10N120W_20101117_gmted_mea300.tif")
elev3 <- raster("./rasters/10S090W_20101117_gmted_mea300.tif")
elev4 <- raster("./rasters/30N120W_20101117_gmted_mea300.tif")

elev <- merge(elev1, elev2, elev3, elev4)

# Extract values of bio1, bio12, and elevation

bio1_extract <- raster::extract(bios_stack[[1]], occs)
bio12_extract <- raster::extract(bios_stack[[4]], occs)
elev_extract <- raster::extract(elev, occs)

bio1_values <- cbind(occs, bio1_extract)
bio12_values <- cbind(occs, bio12_extract)
elev_values <- cbind(occs, elev_extract)

all_values <- cbind(occs, c(st_drop_geometry(bio1_values[,5]), st_drop_geometry(bio12_values[,5]), st_drop_geometry(elev_values[,5])))

# Filter environmental outliers based on elevation

occs_without_outliers <- all_values %>% filter(elev_extract >= 800 & elev_extract <= 3000)

# Crop and mask environmental data

aoi_crop_bios <- crop(bios_stack, region)
aoi_crop_srad <- crop(srad_stack, region)
aoi_crop_elev <- crop(elev, region)

aoi_stack <- stack(aoi_crop_bios, aoi_crop_srad, aoi_crop_elev)

aoi_mask <- mask(aoi_stack, region)

# Random sample points for bio1, bio12, and elevation

set.seed(0)
bg <- sampleRandom(aoi_mask, 10000, na.rm = TRUE)
dev.off()

bg_data <- data.frame(bg[, c(1, 4, 28)])
colnames(bg_data) <- c("bio1", "bio12", "elev")

# Visualize species in environmental space

install.packages("scatterplot3d")
library(scatterplot3d)

x <- occs_without_outliers$bio1_values_ext / 10
y <- occs_without_outliers$bio12_values_ext
z <- occs_without_outliers$elev_values_ext

scatterplot3d(x, y, z,
              xlab = "Bio1 (°C)",
              ylab = "Bio12 (mm)",
              zlab = "Elevación (metros)",
              pch = 16,
              color = "cornflowerblue")

# all_values 150: ID, species, countryCode, basisOfRecord, bio1_extract, bio12_extract, elev_extract, geometry
# occs_without_outliers 120: same as below
# bg: 7802 15 + 12 + 1 = 28 environmental layers
# bg_data: 7802 bio1, bio12, elevation

# bg_data to simple feature, get random points coordenates

bg <- sampleRandom(aoi_mask, 10000, xy = TRUE)
bg_data <- st_as_sf(bg_data, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

# union occs_without_outliers with bg_data "species named quercus insginis or background"

bd_data <- tibble::add_column(species = "background")
rename(decimalLatitude = Latitude, decimalLongitude = Longitude, species = Species)

# Occurences by Mexico state

mex <- read_sf("./vectors/mge2013v6_2.shp")
mex_occs <- qi_final %>%
  filter(countryCode == "MX") %>%
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude")) %>%
  st_set_crs(st_crs(mex))
spatial_joining <- st_join(mex_occs, mex[,2])