---
layout: page
title: User Manual
permalink: /manual/
---
- [How to run the software](#how-to-run-the-software)
  - [Installation Instructions](#installation-instructions)
  - [Launching the App](#launching-the-app)
- [Home Screen](#home-screen)
- [Input](#input)
  - [Land Cover](#land-cover)
    - [Land Cover Map](#land-cover-map)
    - [Hydrological Properties](#hydrological-properties)
    - [Evapotranspiration](#evapotranspiration)
  - [Watershed](#watershed)
    - [Watershed Map](#watershed-map)
    - [3D View](#3d-view)
    - [Lake and DAM](#lake-and-dam)
    - [Ground Water and River Flow](#ground-water-and-river-flow)
  - [Soil](#soil)
    - [Physical and Chemical Properties](#physical-and-chemical-properties)
    - [Hydraulic Properties](#hydraulic-properties)
    - [Soil and Plant Water](#soil-and-plant-water)
    - [Soil Erosion and Sedimentation](#soil-erosion-and-sedimentation)
  - [Rainfall and Rivers](#rainfall-and-rivers)
    - [Rainfall](#rainfall)
    - [River](#river)
    - [Consistency Check](#consistency-check)
  - [Options](#options)
- [Simulation](#simulation)
  - [Water Balance](#water-balance)
  - [Watershed Indicator](#watershed-indicator)
  - [Buffering Indicator](#buffering-indicator)
- [FlowPer](#flowper)

---

## How to run the software

GenRiver3 software is web application and the **online version** is available at: **<https://genriver.agroforestri.id/>**

The software can also be launched as a standalone app using R and RStudio. The following is a step-by-step guide for running the software from the source code as a standalone app.

### Installation Instructions
1. Install R from [CRAN](https://cran.r-project.org/).
2. Install RStudio from [RStudio](https://www.rstudio.com/products/rstudio/download/).
3. Install the Shiny package in R:
    ```R
    install.packages("shiny")
    ```

### Launching the App
1. Open RStudio.
2. Load the Shiny library:
    ```R
    library(shiny)
    ```
3. Run the app directly from GitHub source code:
    ```R
    shiny::runGitHub("genriver", "degi")
    ```
    Another option is to download all the source codes from https://github.com/degi/genriver. Extract all files to a local folder and execute the script below 
    ```R
    runApp("path/to/your/app")
    ```

> If you have the **source code** on the local folder, you will need an internet connection to run it for the first time. An internet connection is required to update and initialize the R libraries. Once the updates are completed, you will be able to run the app **without an internet connection**.<br/>
    
> <img src="../docs/images/info-circle.svg" width="18" height="18"> **Note:**<br>
> You might still need an internet connection to download the DEM. However, you can go without it once you have it in your saved parameters. 

## Home Screen

<img src="../docs/images/home.png" width="400"/>

The main interface to work with the model is divided into **Input**, **Simulation**, and **Flopper** sections.

- **Input:** The input parameters required for running the model
- **Simulation:** Run the model simulation after the required parameters are completed
- **Flopper:** Analyzing the river and rain data on its persistence

## Input

### Land Cover

**Land cover** is one of the main factors in the watershed dynamics. A time series of land cover maps follows the dynamic changes in soil properties. You may provide land cover maps for the observed periods. The boundary box of the land cover map will be used as the area for the DEM query. The DEM is later delineated to generate the watershed boundary.

#### Land Cover Map

<img src="../docs/images/landcover.png" width="400"/>

Once the map is uploaded, it will be displayed on the page. The map IDs will be displayed on the right side as landcover IDs. A default landcover label is generated for all the IDs. You may edit the landcover label in place, or upload it from a predefined CSV file.    

#### Hydrological Properties

<img src="../docs/images/lc_prop.png" width="400"/>

- **BD/BDref** is the bulk density of a soil layer relative to the ‘reference bulk density’ that can be expected for soil of similar texture under natural forest conditions

#### Evapotranspiration

<img src="../docs/images/evapot.png" width="400"/>

<img src="../docs/images/evapot_month.png" width="400"/>

### Watershed

<img src="../docs/images/dem_map.png" width="400"/>

#### Watershed Map

<img src="../docs/images/ws2.png" width="400"/>

#### 3D View

<img src="../docs/images/ws_3d.png" width="400"/>

#### Lake and DAM

<img src="../docs/images/lake.png" width="400"/>

#### Ground Water and River Flow

<img src="../docs/images/ground_water.png" width="400"/>

### Soil

#### Physical and Chemical Properties

<img src="../docs/images/soil_global_db.png" width="400"/>

<img src="../docs/images/soil_list_global.png" width="400"/>

<img src="../docs/images/soil_map_global.png" width="400"/>

<img src="../docs/images/soil_seg_sett.png" width="400"/><img src="../docs/images/soil_seg_sett2.png" height="400"/><img src="../docs/images/soil_seg_sett3.png" height="400"/>

#### Hydraulic Properties

<img src="../docs/images/soil_water.png" width="400"/>

<img src="../docs/images/soil_water_lc.png" width="400"/>

####  Soil and Plant Water

<img src="../docs/images/soil_water_par.png" width="400"/>

#### Soil Erosion and Sedimentation

<img src="../docs/images/erosion_map.png" width="400"/>

<img src="../docs/images/erosion_par.png" width="400"/>

### Rainfall and Rivers

#### Rainfall

<img src="../docs/images/rainfall.png" width="400"/>

#### River

<img src="../docs/images/river.png" width="400"/>

#### Consistency Check

<img src="../docs/images/rain_river_check.png" width="400"/>

### Options

## Simulation

### Water Balance

<img src="../docs/images/sim_cum.png" width="400"/>

### Watershed Indicator

<img src="../docs/images/sim_avg.png" width="400"/>

### Buffering Indicator

<img src="../docs/images/sim_buf.png" width="400"/>

## FlowPer

<img src="../docs/images/flowper.png" width="400"/>

*To be updated...*
