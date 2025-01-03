
cashews <- rast("Spatial/cambodia_cashew_distribution.tif") 
cashews <- subst(cashews, 0, NA)
cashewpal <- colorBin("OrRd", values(cashews), bins=6, na.color="transparent")
surfwater <- rast("Spatial/jrc_global_surface_water_coarse.tif")
surfwater_pal <- colorNumeric("RdBu", domain=c(-100,100), na.color='transparent')
surfwater_leg <- colorNumeric("RdBu", reverse=T, domain=c(-100,100), na.color='transparent')
crop_areas <- rast("Spatial/lgrip30-khm-agg10.tif")
crop_areas <- subst(crop_areas, 2, 1)
crop_areas <- subst(crop_areas, 3, 2)
croparea_df <- data.frame(id=c(1,2), area=c("Irrigated", "Rainfed"))
levels(crop_areas) <- croparea_df 
croppal <- colorFactor(c( "darkgreen", "goldenrod"), domain=NULL, na.color="transparent")
luse_areas <- c("Developed",
                "Mangrove",
                "Other Plantation",
                "Water",
                "Shrub",
                "Rice",
                "Cropland",
                "Grassland",
                "Evergreen",
                "Deciduous",
                "Wetland",
                "Rubber",
                "Flooded Forest",
                "Semi-evergreen",
                "Village",
                "Other")
landuse_df <- data.frame(id=seq(1,16), luse_areas)
#luse_colors <- (c("hotpink","yellow","orange3","darkblue","lightgreen","lemonchiffon", 'sandybrown', 'bisque3', 'darkgreen', 'forestgreen', 'lightblue', 'springgreen1', 'darkolivegreen3', 'chartreuse3', 'purple', 'lightgray'))
luse_colors <- c('#FF69B4', '#FFFF00', '#CD8500', '#00008B', '#90EE90', '#FFFACD', '#F4A460', '#CDB79E', '#006400', '#228B22', '#ADD8E6', '#00FF7F', '#7ee6b2', '#66CD00', '#A020F0', '#D3D3D3')
luse_pal <- colorFactor(luse_colors, domain=NULL, na.color='transparent')
land_use <- rast("Spatial/Landcover2023_coarse.tif")
levels(land_use) <- landuse_df

#AT NOTE: This API key is tied to my account. Please sign up for your own if you are uploading a different version of the app.
#Adding our own mapserver is on my to do list.
source_string <- "https://tiles.stadiamaps.com/tiles/alidade_smooth/{z}/{x}/{y}.png?api_key=05f94eb8-5969-40fc-98e7-3b82d9d830e7"