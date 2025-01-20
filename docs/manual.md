---
layout: page
title: User Manual
permalink: /manual/
---
## How to run the software

GenRiver3 software is web application and the **online** version is available at: https://genriver.agroforestri.id/

The software can also be launched as a standalone app using R and RStudio. Following is the step-by-step to run the software from the source code as a standalone app.

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
    > If you have the **source code** on the local folder, you will need an internet connection to run it for the first time. An internet connection is required for updating and initializing the R libraries. Once the updates are completed, you will be able to run the app **without an internet connection**.<br/>
    > *To be noted*: An internet connection might still be required for downloading the DEM. You can go without the internet once you have it in your saved parameters. 

## User Interface Overview

### Home

<img src="../docs/images/home.png" width="400"/>

### Input

### Simulation


*To be updated...*
