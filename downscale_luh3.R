
library(dplyr)
library(ggplot2)
library(terra)
library(sf)
library(rnaturalearth)
library(viridis)
library(devtools)

# To use mrdowscale as a developer and not just usign the package
devtools::load_all()

#when prompted about main folder path, indicate downscaling_mainfolder

additional_vars<- read.csv("./downscaling_mainfolder/sources/WITCH/iamc_luh3report_additionalVars_aug06_2026_onlyHL.csv")
luh3_vars<-read.csv("./downscaling_mainfolder/sources/WITCH/iamc_luh3report_aug06_2026_onlyHL.csv")

# Add units to additional vars

units<-data.frame(
  variable= c(
    "Forestry Production|Roundwood",
    "Forestry Production|Roundwood|Wood Fuel",
    "Forestry Production|Roundwood|Industrial Roundwood",
    "Land Cover",
    "Land Cover|Cropland",
    "Land Cover|Cropland|Irrigated",
    "Fertilizer Use|Nitrogen",
    "Fertilizer|Nitrogen|Intensity"
  ),
  units= c(
    "million m3/yr",
    "million m3/yr",
    "million m3/yr",
    "million ha",
    "million ha",
    "million ha",
    "Tg N/yr",
    "t N//ha/yr"
  )
  )

additional_vars<- left_join(additional_vars, units, by= "variable")

all_vars<- rbind(luh3_vars,additional_vars)

# Write csv for mrdownscale
write.csv(all_vars, file=paste0("./downscaling_mainfolder/sources/WITCH/","ssp5_HL_luh3_4mrdownscale.csv"))

tgzPath <- retrieveData("ScenarioMIP", input = "witch",
                        harmonization = "fade", downscaling = "magpieClassic",
                        harmonizationPeriod = c(2020, 2050),
                        yearsSubset = seq(1995, 2100, 5),
                        scenario = "ssp5_HL_NL",
                        rev = format(Sys.time(), "%Y-%m-%d"), puc = FALSE)

tgzPath <- retrieveData("ScenarioMIP", input = "witch",
                        harmonization = "fade", downscaling = "magpieClassic",
                        harmonizationPeriod = c(2020, 2050),
                        yearsSubset = seq(1995, 2100, 5),
                        scenario = "ssp1_VLLO",
                        rev = format(Sys.time(), "%Y-%m-%d"), puc = FALSE)

# Visualize results

countries_layer<- ne_countries(scale="small", returnclass="sf")
countries_layer_df <- st_as_sf(countries_layer)

witch17_countries<-vect("./world_countries_witch17")
witch17_countries_df<- st_as_sf(witch17_countries)

# Read downscaled maps
downscaled_nc_path<- "./downscaling_mainfolder/output/rev2026.9.2_h12_c77716e8_scenariomip/multiple-states_input4MIPs_landState_ScenarioMIP_ssp5_HL_NL-2026.9.2_gn_1995-2100.nc"
downscaled_nc_path<- "./downscaling_mainfolder/output/rev2026.10.27_h12_5bac6153_scenariomip/multiple-states_input4MIPs_landState_ScenarioMIP_ssp1_VLLO-2026.10.27_gn_1995-2100.nc"
downscaled_raster<- rast(downscaled_nc_path)
print(downscaled_raster)
sources(downscaled_raster)
varnames(downscaled_raster)
names(downscaled_raster)

#-------------------------------------------------------------------------------------------------
# Get year from period number
# Periods go from 1 to 22, being 1 1995 and 22, 2100
getYear<-function(period){
  year<- 1990 + 5* period
  year

}

getPeriod<-function(year){
  period <-(year-1990)/5
  period
}

getLayer<-function(varName, year){
  layerName<- paste0(varName,"_", getPeriod(year))
  layer<- downscaled_raster[[layerName]]
  layer
}

graph_map_var_year<- function(varName=NULL,year=2100){

  if(is.null(var)){
    print("Choose a variable")
  }
  else{
    # Explore one layer, i.e., one state for a particular year

    layerName<- paste0(varName,"_", getPeriod(year))
    var<- downscaled_raster[[layerName]]

    print(var)

    var_df <- as.data.frame(var, xy=TRUE)

    title<- paste0(varName, "_", year)

    p<-ggplot() +
      geom_tile(data=var_df, aes(x = x, y = y, fill = .data[[layerName]])) +
      #scale_fill_viridis_c(direction=1, limits=c(0,1)) + # Use a color scale (e.g., viridis)
      scale_fill_gradientn(
        colours = c("white", viridis(256)),
        limits = c(0, 1)
      ) +
      geom_sf(data=countries_layer_df, colour = "blue", fill = "transparent")+
      labs(title = title, x = "Longitude", y = "Latitude", fill = "Value") +
      theme_minimal()

    ggsave(paste0("./downscaling_mainfolder/output/plots/ ", varName, " for year ", year, ".png"), p, width = 10, height = 6.5, dpi = 320)
    p
  }

}

graph_map_var_year("primf", 2100)
graph_map_var_year("primn", 2100)
graph_map_var_year("secdf", 2100)
graph_map_var_year("secdn", 2100)
graph_map_var_year("pastr", 2100)
graph_map_var_year("urban", 2100)
graph_map_var_year("pltns", 2100)

#vars is a vector
graph_map_adding_vars<- function(vars, year, theTitle="Ag_vars", theCaption=""){

  agLayer<- NULL
  for(v in vars){
    layerName<- paste0(v,"_", getPeriod(year))
    var<- downscaled_raster[[layerName]]

    if (is.null(agLayer)) {
      agLayer <- var
    } else {
      agLayer <- agLayer + var
    }

    names(agLayer) <- "value"  # give it one predictable name
    agLayer_df <- as.data.frame(agLayer, xy=TRUE)
  }

  title<- paste0(theTitle, "_", year)

  p<-ggplot() +
    geom_tile(data=agLayer_df, aes(x = x, y = y, fill = value)) +
    #scale_fill_viridis_c(direction=1, limits=c(0,1)) + # Use a color scale (e.g., viridis)
    scale_fill_gradientn(
      colours = c("white", viridis(256)),
      limits = c(0, 1)
    ) +
    geom_sf(data=countries_layer_df, colour = "blue", fill = "transparent")+
    labs(title = title, x = "Longitude", y = "Latitude", fill = "Value", caption = theCaption) +
    theme_minimal()

  ggsave(paste0("./downscaling_mainfolder/output/plots/ ", theTitle, " for year ", year, ".png"), p, width = 10, height = 6.5, dpi = 320)
  p
}

graph_map_adding_vars(c("c3ann", "c3nfx", "c3per", "c4ann", "c4per"), 2100, "cropland", theCaption = "c3ann + c3nfx + c3per + c4ann + c4per")
graph_map_adding_vars(c("primf", "secdf"), 2100, "forest", theCaption = "primf + secdf")
graph_map_adding_vars(c("pastr", "range"), 2100, "grazing", theCaption = "pastr + range")
#Not all pixels are adding upt to 1
graph_map_adding_vars(c("primf", "primn", "secdf", "secdn", "pastr", "range",
                        "urban", "c3ann", "c3per", "c4ann", "c4per", "c3nfx", "pltns"), 2100, "all", theCaption = "all variables")

# Additional vars provided, but not info for
graph_map_var_year("c3ann", 2100)
graph_map_var_year("c4per", 2100)
