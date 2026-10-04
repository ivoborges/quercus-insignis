# Install package management tool
install.packages("pacman")

# Packages used in this project:
# dplyr - data manipulation
# ggplot2 - data visualization
# patchwork - composer of plots
# sf - spatial data analysis (vectors)
# tmap - thematic maps
# terra - spatial data analysis (rasters)
# spThin - spatial thinning of species occurrence records for use in ecological models
# scatterplot3d - data visualization (3D graphics)
# kuenm - development of ecological niche models using Maxent

# Load and install packages if they are not installed
pacman::p_load(dplyr, ggplot2, patchwork, sf, tmap, terra, spThin, scatterplot3d)
