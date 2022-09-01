# Install package management tool
install.packages("pacman")

# Load and install packages if they are not installed
pacman::p_load(dplyr, ggplot2, patchwork, sf, tmap, terra, spThin)

# Import GBIF occurrences. Clean and transform dataset.
qi_gbif <- read.csv("./occs/qinsignis-gbif-366.csv") %>%
  dplyr::select(species, decimalLatitude, decimalLongitude, countryCode, basisOfRecord) %>%
  distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE) %>%
  filter(!is.na(decimalLatitude), !is.na(decimalLongitude)) %>%
  filter(decimalLatitude != '0' & decimalLongitude != '0') %>%
  filter(countryCode != 'ES', countryCode != 'FR')
View(qi_gbif)

# Import Cacoma occurrences. Clean and transform dataset.
qi_cacoma <- read.csv("./occs/cacoma.csv") %>%
  tibble::add_column(countryCode = 'MX', basisOfRecord = 'HUMAN_OBSERVATION') %>%
  rename(decimalLatitude = Latitude, decimalLongitude = Longitude, species = Species)
View(qi_cacoma)

# Decimal places
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

# Merge GBIF and Cacoma
qi_final <- dplyr::union(qi_gbif, qi_cacoma)
qi_final <- tibble::add_column(qi_final, ID = seq(nrow(qi_final))) %>% 
  dplyr::select(ID, species, decimalLatitude, decimalLongitude, countryCode, basisOfRecord)
View(qi_final)

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
