### Global rain data: https://docs.ropensci.org/rnoaa/articles/rnoaa.html
### https://github.com/ropensci/rnoaa
### https://github.com/ropensci/GSODR

### pedotransfer: https://github.com/julienmoeys/soilwater


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

install_load(
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
  "shinyjqui",
  "rgl",
  
  "DBI",
  "rayshader"
)

if (!("flowdem" %in% rownames(installed.packages()))) {
  install_github("KennethTM/flowdem")
}

if (!("shinycssloaders" %in% rownames(installed.packages()))) {
  install_github("daattali/shinycssloaders")
}


library("shiny")
library("remotes")
library("devtools")
library("bslib")
library("bsicons")
library("htmltools")
library("reactable")
library("plotly")
library("leaflet")
library("stars")
library("excelR")
library("RColorBrewer")
library("jsonlite")
library("reshape2")
library("leafem")
library("terra")
library("yaml")
library("zip")
library("paletteer")
library("httr")
library("lubridate")
library("shinyjqui")
library("rgl")
library("DBI")
library("flowdem")
library("rayshader")
library("shinycssloaders")




default_wd <- getwd()

input_dialog <- function(title = "",
                         desc = "",
                         confirm_id,
                         confirm_label = "Add",
                         input_var = NULL,
                         input_label = NULL,
                         input_def = NULL,
                         input_pholder = NULL,
                         input_type = NULL) {
  inp <- NULL
  if (!is.null(input_var)) {
    blank <- rep("", length(input_var))
    if (is.null(input_label))
      input_label <- blank
    if (is.null(input_def))
      input_def <- blank
    if (is.null(input_pholder))
      input_pholder <- blank
    if (is.null(input_type))
      input_type <- blank
    inp <- mapply(function(v, l, d, p, t) {
      if (t == "numeric") {
        paste(numericInput(v, HTML(l), d, width = "100%"))
      } else {
        paste(textInput(v, HTML(l), d, width = "100%", p))
      }
    },
    input_var,
    input_label,
    input_def,
    input_pholder,
    input_type)
  }
  names(inp) <- NULL
  inp <- HTML(inp)
  showModal(modalDialog(
    title = title,
    HTML(desc),
    inp,
    footer = tagList(
      actionButton("cancel_button_dialog", "Cancel"),
      actionButton(confirm_id, confirm_label)
    )
  ))
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






# co2_unit <- function(prefix = "", suffix = "") {
#   tags$html(
#     paste0(prefix, "CO"),
#     tags$sub(2, .noWS = c("after", "before")),
#     paste0("e", suffix),
#     .noWS = c("after", "before")
#   )
# }
# 
# per_ha_unit <- function(prefix = "", suffix = "") {
#   tags$html(paste0(prefix, "ha"),
#             tags$sup(-1, .noWS = c("after", "before")),
#             suffix,
#             .noWS = c("after", "before"))
# }
