**GenRiver** is a generic river model on river flow.

Run the program by executing this script in R or RStudio:

    shiny::runGitHub("genriver", "degi")

The **online version** is available at: [https://genriver.agroforestri.id/](https://genriver.agroforestri.id/).
> If the server is down due to memory and bandwidth limitations, run the app using RStudio by running the script above.

**User manual** is available at: [https://degi.github.io/genriver/](https://degi.github.io/genriver/) 

## Feature highlights

* The DEM map is provided by the app through [opentopography.org](https://opentopography.org/)<br/> 
  
    <img src="docs/images/dem_get.png" height="300"/>
    <img src="docs/images/dem_map.png" height="300"/><br/>

* The watershed area, stream path, and sub-catchments are generated automatically<br/> 
  
    <img src="docs/images/dem_ws.png" height="300"/>
    <img src="docs/images/ws2.png" height="300"/><br/>

* View the watershed area in 3D<br/>

    <img src="docs/images/ws_3d.png" height="300"/><br/>

* The soil data is acquired from [FAO - Harmonized World Soil Database version 2.0](https://www.fao.org/soils-portal/data-hub/soil-maps-and-databases/harmonized-world-soil-database-v20)<br/>

    <img src="docs/images/soil_global_db.png" height="300"/><br/>

* The app provides a soil mapping method with automatic procedural segmentation<br/>
  
    <img src="docs/images/soil_map_global.png" height="300"/><br/>

* Soil water is pre-calculated from soil properties data
 
    <img src="docs/images/soil_water.png" height="300"/><br/>

* Simulation output in various graphical charts
  
    <img src="docs/images/sim_ws_ind.png" height="300"/>
    <img src="docs/images/sim_cum.png" height="300"/>
    <img src="docs/images/sim_buf.png" height="300"/>
    <img src="docs/images/sim_avg.png" height="300"/><br/>
