### Global rain data: https://docs.ropensci.org/rnoaa/articles/rnoaa.html
### https://github.com/ropensci/rnoaa
### https://github.com/ropensci/GSODR

### pedotransfer: https://github.com/julienmoeys/soilwater


##########################
#LISA
#
# - data evapot bulanan bisa diestimasi dari suhu aja {Thornthwaite}
# - debit tampilin bulanan juga , buat cek konsistensi juga
#
# - bagi sub das berdasarkan ordo stream - otomatis
#
##########################


# Nash-Sutcliffe model efficiency (NSE)
# https://search.r-project.org/CRAN/refmans/ie2misc/html/vnse.html


#https://icons.getbootstrap.com/icons/tsunami/

options(rgl.useNULL = TRUE)

install_load <- function (package1, ...)  {
  # convert arguments to vector
  packages <- c(package1, ...)
  # start loop to determine if each package is installed
  instp <- rownames(installed.packages())
  for (package in packages) {
    if (!package %in% instp) {
      install.packages(package, repos = "http://cran.us.r-project.org", dependencies = T)
    }
  }
}

libs <- c(
  "shiny",
  "remotes",
  "devtools",
  "bslib",
  "bsicons",
  "htmltools",
  "reactable",
  "plotly",
  "leaflet",
  "stars",
  "FNN",
  "excelR",
  "RColorBrewer",
  "jsonlite",
  "reshape2",
  "leafem",
  "terra",
  "yaml",

  
  "zip",
  "paletteer",
  "httr",
  
  "lubridate",
  "DBI",
  
  "future",
  "promises",
  "ipc",
  
  "RSQLite",
  "lwgeom"
)

install_load(libs)

library("remotes")

if (!("flowdem" %in% rownames(installed.packages()))) {
  install_github("KennethTM/flowdem")
}

if (!("shinycssloaders" %in% rownames(installed.packages()))) {
  install_github("daattali/shinycssloaders")
}

# lib_df <- data.frame(Library = sort(c(libs, "flowdem", "shinycssloaders")))
# lib_df$References <- lapply(lib_df$Library, function(x) {
#   gsub("[\r\n]", "", format(citation(x), style = "text"))
# })
# md <- knitr::kable(lib_df, format = "markdown", row.names = F)
# write(md, "lib.md")

library("shiny")

library("devtools")
library("bslib")
library("bsicons")
library("htmltools")
library("plotly")
#table UI
library("reactable")
library("excelR")
#spatial
library("stars")
library("terra")
library("FNN")
library("leaflet")
library("leafem")
#file IO
library("yaml")
library("zip")
#color
library("paletteer")
library("RColorBrewer")
#network
library("jsonlite")
library("httr")
#utiliy
library("reshape2")
library("lubridate")
library("DBI")
library("flowdem")
library("shinycssloaders")
library("RSQLite")
library(lwgeom)
library(future)
library(promises)
library(ipc)

plan(multisession)

theme_color <- list(
  primary = "#034464",
  secondary = "#219ebc",
  dark = "#404040",
  success = "#06d6a0",
  # info = "#fb8500",
  info = "#219ebc",
  warning = "#ffb703",
  danger = "#9a130e",
  light1 = "#CAEDF6",
  light2 = "#ECF9FC"
)



seconds_in_day <- 86400
default_wd <- getwd()


map_label <- function(desc = "",
                      title = "",
                      footer = "") {
  title_div <- ""
  if (title != "")
    title_div <- paste("<div style='font-size:1.5em;'>",
                       title,
                       "</div><hr style='margin:2px auto;'>")
  footer_div <- "</div>"
  if (footer != "")
    footer_div <- paste("<hr style='margin:2px auto;'><strong><em>",
                        footer,
                        "</em></strong></div>")
  paste("<div style='font-size:1.2em;'>",
        title_div,
        "<div>",
        desc,
        "</div>",
        footer_div) |>
    lapply(htmltools::HTML)
}

show_spinner <- function(label) {
  conditionalPanel(
    condition = "output.is_spinner",
    div(
      style = "position:absolute;top:100px;left:calc(50% - 150px);
      text-align:center;background-color:white;border-radius:5px;padding:20px",
      p(tags$strong(label)),
      div(
        class = "spinner-border text-warning",
        style = "width:3rem;height:3rem;margin-top:10px;",
        role = "status",
        span(class = "visually-hidden", "loading..")
      )
    )
  )
}

tooltip_blue <- function(...) {
  tooltip(..., options = list(customClass = "custom-tooltip"))
}

### variable def ################

var_cols <- c("variable", "definition", "category", "unit", "min", "max", "default")
variable_df <- read.csv("docs/variable_df.csv")
# variable_df$no <- c(1:nrow(variable_df))
# variable_df <- variable_df[c("no", var_cols)]
# names(variable_df) <- tools::toTitleCase(names(variable_df))
# variable_df$Min <- lapply(variable_df$Min, function(x) format(x, digits = 2))
# variable_df$Max <- lapply(variable_df$Max, function(x) format(x, digits = 2))
# variable_df$Default <- lapply(variable_df$Default, function(x) format(x, digits = 2))
# variable_df[variable_df == "NA"] <- ""
# md <- knitr::kable(variable_df, format = "markdown", row.names = F)
# write(md, "variable.md")

### opentopography.org ########################

opentopo_dataset_df <- data.frame(
  var = c(
    "SRTMGL3",
    "SRTMGL1",
    "SRTMGL1_E",
    "AW3D30",
    "AW3D30_E",
    "SRTM15Plus",
    "NASADEM",
    "COP30",
    "COP90",
    "EU_DTM",
    "GEDI_L3",
    "GEBCOIceTopo",
    "GEBCOSubIceTopo"
  ),
  
  label = c(
    "SRTMGL3 (SRTM GL3 90m)",
    "SRTMGL1 (SRTM GL1 30m)",
    "SRTMGL1_E (SRTM GL1 Ellipsoidal 30m)",
    "AW3D30 (ALOS World 3D 30m)",
    "AW3D30_E (ALOS World 3D Ellipsoidal, 30m)",
    "SRTM15Plus (Global Bathymetry SRTM15+ V2.1 500m)",
    "NASADEM (NASADEM Global DEM)",
    "COP30 (Copernicus Global DSM 30m)",
    "COP90 (Copernicus Global DSM 90m)",
    "EU_DTM (DTM 30m)",
    "GEDI_L3 (DTM 1000m)",
    "GEBCOIceTopo (Global Bathymetry 500m)",
    "GEBCOSubIceTopo (Global Bathymetry 500m)"
  )
)

dem_fail <- "The DEM download was failed. You may do the following:
* Make sure the internet connection is stable
* Try another DEM sources
* Register to  <a href='https://portal.opentopography.org/' target='_blank'>opentopography.org</a> and get your own API key.
Make sure the key was correctly copied into 'API key' input"

dem_info <- "The DEM data is acquired from <a href='https://opentopography.org/'
target='_blank'><b>opentopography.org</b></a>.
          OpenTopography provides open and free access to DEM dataset.
          Please visit the website for further info and follow
          the instruction at <a href='https://opentopography.org/citations' target='_blank'>
          https://opentopography.org/citations</a> for the <b>citation</b> of using the dataset"

dem_api_info <- "The API key can be obtained when you register at opentopography.org,
                         or you may leave it empty to try using the default key"



desc <- list(
  landcover = "**Land cover** is one of the main factors in watershed dynamics.
  A **time series** of land cover maps follows the dynamic changes in soil properties.
  Please provide land cover maps on the observed periods.
  The land cover map's boundary box will be used as the area for the DEM query,
  The DEM is later delineated to generate the watershed boundary.",
  
  soil_hydraulic = "Soil hydraulic at a potential of 0 kPa is in a state of saturation.
  At saturation, all soil pores are filled with water, and water typically drains
  from large pores by gravity. At a potential of −33 kPa, or −1/3 bar, (−10 kPa for sand),
  soil is at field capacity. Typically, at field capacity, air is in the macropores,
  and water in the micropores. Field capacity is viewed as the optimal condition
  for plant growth and microbial activity. At a potential of −1500 kPa,
  the soil is at its permanent wilting point, at which plant roots cannot
  extract the water through osmotic diffusion.(https://en.wikipedia.org/wiki/Water_potential)",
  
  soil_db = "The soil data is acquired from **Harmonized World Soil Database version 2.0** (HWSD v2.0). 
  **HWSD** is a comprehensive global soil inventory that offers detailed insights into soil properties, 
  including their morphology, chemistry, and physical characteristics, 
  with a focus on a 1 km resolution. Please visit 
  <a href='https://www.fao.org/soils-portal/data-hub/soil-maps-and-databases/harmonized-world-soil-database-v20/en/' target='_blank'>**FAO SOILS PORTAL**</a>
  for more information on the database and its suggested citation",
  
  soil_list = "The soil list here is extracted from the global soil database, including the **summarized properties** required for the model. 
  Check the Global Soil Database tab to view the soil properties in detail."
)


hydro_id <- c("SMU_ID",
              "SHARE",
              "soil_type",
              "SOIL",
              "soil_depth",
              "TOPDEP",
              "BOTDEP")
hydro_prop <- c("SAND",
                "SILT",
                "CLAY",
                "BULK",
                "REF_BULK",
                "ORG_CARBON",
                "CEC_SOIL",
                "PH_WATER")

soil_water_types <- list(
  "Soil Saturation" = "soil_saturation",
  "Field Capacity" = "field_capacity",
  "Permanent Wilting Point" = "permanent_wilting_point"
)

soil_water_availability <- list(
  "Soil quick flow capacity (mm)" = "soil_quick_flow_capacity",
  "Plant available water (mm)" = "plant_available_water",
  "Inaccessible water (mm)" = "inaccessible_water"
)

default_par <- function(df) {
  val <- as.list(df$value)
  names(val) <- df$var
  return(val)
}

rain_par_df <- data.frame(
  var = c(
    "rain_intensity_mean",
    "rain_intensity_coef",
    "rain_randseed"
  ),
  label = c(
    "Mean of rain intensity",
    "Coeffienct variation of rain intensity",
    "Random seed"
  ),
  value = c(10000, 0.3, 200),
  min = c(1000, 0, NA),
  max = c(100000, 1, NA),
  step = c(1, 0.1, NA),
  stringsAsFactors = FALSE
)

river_par_df <- data.frame(
  var = c(
    "I_RoutVeloc_m_per_s",
    "I_Tortuosity",
    "I_RiverflowDispersalFactor",
    "I_SurfLossFrac"
  ),
  label = c(
    "Routing velocity (m sec<sup>-1</sup>)",
    "TortuoSity",
    "River flow dispersal factor",
    "Surface loss fraction"
  ),
  value = c(0.55, 0.6, 0.6, 0),
  min = rep(0, 4),
  max = c(100, rep(1, 3)),
  step = rep(0.1, 4),
  stringsAsFactors = FALSE
)

infiltration_par_df <- data.frame(
  var = c(
    "soil_max_infiltration",
    "soil_max_infiltration_subsoil",
    "soil_infiltration_red"
  ),
  label = c(
    "Maximum infiltration",
    "Maximum infiltration of sub soil",
    "soil_infiltration_red"
  ),
  value = c(200, 100, 3),
  min = c(30, 0, 3),
  max = c(1000, 1000, 3.5),
  step = c(1, 1, 0.1),
  stringsAsFactors = FALSE
)

groundwater_par_df <- data.frame(
  var = c(
    "groundwater_rel_init",
    "groundwater_max_dynamic_const",
    "groundwater_rel_frac_const"
  ),
  label = c(
    "Initial relative ground water",
    "Maximum dynamic ground water constant",
    "Ground water relative fraction constant"
  ),
  value = c(1, 880, 0.01),
  min = c(0, 0, 0),
  max = c(1, 2000, 1),
  step = c(0.1, 1, 0.1),
  stringsAsFactors = FALSE
)

interception_par_df <- data.frame(
  var = c(
    "interception_effect_on_transp",
    "interception_drip_rt",
    "interception_drip_duration_max"
  ),
  label = c(
    "Rain interception effect on transpiration",
    "Rain interception drip rt",
    "Maximum rain interception drip duration"
  ),
  value = c(0.1, 10, 0.5),
  min = c(0, 5, 0),
  max = c(1, 15, 5),
  step = c(0.1, 1, 0.1),
  stringsAsFactors = FALSE
)

waterplant_par_df <- data.frame(
  var = c("waterplant_const", "waterplant_soilsat_min_fc_const"),
  label = c("Available water constant", "Soil saturated min FC const"),
  value = c(800, 100),
  min = c(0, 0),
  max = c(1000, 500),
  step = c(1, 1),
  stringsAsFactors = FALSE
)

soilplant_par_df <- data.frame(
  var = c(
    "percolation_frac_multiplier",
    "soil_water_initial",
    "soil_qflow_frac"
  ),
  label = c(
    "Percolation fraction multiplier",
    "Initial of soil water ",
    "Soil quick flow"
  ),
  value = c(0.05, 2, 0.1),
  min = c(0, 0, 0),
  max = c(10, 2, 2),
  step = c(0.1, 0.1, 0.1),
  stringsAsFactors = FALSE
)

lake_par_df <- data.frame(
  var = c(
    "L_LakeBottomElev",
    "L_LakeElevPreHEPP",
    "L_LakeLevelFullHEPP",
    "L_LakeLevelHalfHEPP",
    "L_LakeLevelNoHEPP",
    "L_FloodTresh",
    "L_QmecsHEPP",
    "L_QmecsSanFlow",
    "L_LakeOverFlowFrac",
    "L_LakeOverFlPostHEPP",
    "L_LakeOverFlPow",
    "L_m3_per_kwh",
    "L_ResrDepth"
    
  ),
  label = c(
    "L_LakeBottomElev",
    "L_LakeElevPreHEPP",
    "L_LakeLevelFullHEPP",
    "L_LakeLevelHalfHEPP",
    "L_LakeLevelNoHEPP",
    "L_FloodTresh",
    "L_QmecsHEPP",
    "L_QmecsSanFlow",
    "L_LakeOverFlowFrac",
    "L_LakeOverFlPostHEPP",
    "L_LakeOverFlPow",
    "L_m3_per_kwh",
    "L_ResrDepth"
  ),
  value = c(
    160,
    362.3,
    362.3,
    361.8,
    359.5,
    363,
    47.1,
    3,
    0.1,
    362.6,
    4,
    1.584,
    10000
  )
  ,
  min = c(rep(0, 13)),
  max = c(rep(NA, 13)),
  step = c(rep(0.1, 13)),
  stringsAsFactors = FALSE
)
