







wait_spinner <- function() {
  div(
    p(tags$strong("Please wait while rendering the map..")),
    style = "position:absolute; top:200px;left:calc(50% - 150px);text-align:center;",
    div(
      class = "spinner-border text-warning",
      style = "width:5rem;height:5rem;margin-top:50px;",
      role = "status",
      span(class = "visually-hidden", "loading..")
    )
  )
}


pop_inp_map <- function(..., inp_map_id) {
  popover(
    ...,
    title = div(icon("upload"), "Upload map file"),
    id = paste0(inp_map_id, "_pop"),
    fileInput(
      inp_map_id,
      NULL,
      accept = c("image/tiff"),
      placeholder = "Upload map"
    )
  )
}

dem_input_option <- conditionalPanel(
  condition = "!output.is_dem_map && !output.is_stream_map",
  h3("Parameter input options:", style = "margin:20px auto;"),
  div(
    style = "margin:20px auto;text-align:left;",
    icon("mountain-sun", style = "font-size:4em;margin:10px 40px;float:left"),
    div(
      h5("Upload Digital Elevation Model (DEM) map file"),
      style = "float:right",
      fileInput(
        "dem_map_inp",
        NULL,
        accept = c("image/tiff"),
        placeholder = "DEM map file",
        width = "400px"
      )
    )
  ),
  h3("OR", style = "margin:0px auto;"),
  div(
    style = "margin:20px auto;text-align:left;",
    icon("file-import", style = "font-size:4em;margin:10px 40px;float:left"),
    div(
      h5("Upload previously saved parameters"),
      style = "float:right",
      fileInput(
        "upload_parameter",
        NULL,
        accept = ".zip",
        placeholder = "Parameters file",
        width = "400px"
      )
    )
  )
)


# tooltip_blue <- function(...) {
#   tooltip(..., options = list(customClass = "custom-tooltip"))
# }

outlet_table <- card(
  class = c("floating_right", "transparent_bg"),
  height = "50%",
  card_header(
    span(icon("droplet"), "Subcatchment"),
    class = "d-flex justify-content-between",
    span(
      actionButton(
        "delete_outlet_btn",
        "",
        icon = icon("trash-can"),
        class = "toolbar_button"
      ) |> tooltip_blue("Remove the selected subcathments"),
      actionButton(
        "merge_outlet_btn",
        "",
        icon = icon("object-group"),
        class = "toolbar_button"
      ) |> tooltip_blue("Merge the selected subcathments and combine the outlets")
    )
  ),
  card_body(padding = 0, reactableOutput("watershed_list"))
)

ui <-
  page_navbar(
    id = "main_page",
    theme = bs_theme(
      primary = "#034464",
      secondary = "#219ebc",
      dark = "#404040",
      success = "#06d6a0",
      info = "#fb8500",
      warning = "#ffb703",
      #"#FFF3B0",
      danger = "#9a130e",
      font_scale = 0.9
      
    ),
    bg = "#CAEDF6",
    header =
      tags$head(
        tags$style(
          tags$link(rel = "shortcut icon", href = "favicon.ico"),
          HTML(
            ".bg_theme{background-color: #CAEDF6;}
            .bg_theme2{background-color: #EDF9FC;}

            .bg_warning{background-color: #ffb703;}
            .bg_light1{background-color: #CAEDF6}
            .bg_light2{background-color: #ECF9FC !important;}

/*
            .shiny-notification {
               height: 100px !important;
               width: 800px !important;
               position:absolute !important;
               top: calc(50% - 50px);
               left: calc(50% - 320px);
               right: 10px !important;
               font-size: 120%;
               text-align: left;
               background-color: #ffb703;
              box-shadow: 2px 2px 10px black !important;
            }

            .shiny-notification-close {display: none !important;}
*/
            .modal {z-index: 1150;}

            .soil_table th, td {
              padding: 5px;
              text-align: left;
              border-bottom: 1px solid #ddd;
            }

            .floating_right {
              position: absolute;
              top: 20px;
              left:-webkit-calc(100% - 300px);
              left:-moz-calc(100% - 300px);
              left:calc(100% - 300px);
            }

            .transparent_bg {
              background-color: rgba(255, 255, 255, 0.7);
            }

            .toolbar_button {
              width: 24px;
              height: 24px;
              padding: 0px;
              border-width: 0px;
            }

            .custom-tooltip {
              --bs-tooltip-bg: #023047;
            }

            .map_label {
              color: white;
              background-color: #023047E9 ;
              border-width: 1px;
              border-radius: 10px;
              box-shadow: 3px 3px 12px black;
            }

            .leaflet-popup-content-wrapper{
              background-color: rgba(255, 255, 255, 0.9);
              box-shadow: 4px 4px 20px black;
            }

            .leaflet-popup-tip {
              background: rgba(255, 255, 255, 0.9);
            }

            #input_panel {
              --bs-nav-link-color: #219ebc;
              --bs-nav-tabs-border-radius:10px;
              --bs-nav-tabs-link-active-color:#fb8500;

            }

            .card-header {
              background-color: #ECF9FC
            }

            .subpanel .card-header {
              background-color: white;
              border-width: 0px;
            }

            .inline label{ display: table-cell; text-align: left; vertical-align: middle; padding: 0px 10px}
            .inline .form-group{display: table-row;}

          "
          )
        ),
        tags$script(src = "jexcel.js"),
        tags$link(rel = "stylesheet", href = "jexcel.css", type = "text/css"),
        tags$script(src = "jsuites.js"),
        tags$link(rel = "stylesheet", href = "jsuites.css", type = "text/css"),
        tags$link(rel = "stylesheet", type = "text/css", href = "table.css"),
      ),
    window_title = "GenRiver3",
    title =
      
      span(
        tags$img(
          height = 22,
          src = "images/genriver_logo.svg",
          style = "margin-right:5px"
        ),
        "Gen",
        span(
          "River",
          style = "color:#fff;",
          .noWS = c('before', "after")
        ),
        "3",
        style = "color:#ffb703; background:#034464; padding:2px 20px 4px 20px; border-radius:15px; font-weight:bold"
      ),
    
    padding = 0,
    
    nav_panel(
      #### INPUT OPTIONS ####
      title = "Input",
      icon = icon("arrow-down"),
      
      navset_card_tab(
        title = "Input data and parameters:",
        id = "input_panel",
        nav_panel(
          title = "Land Cover",
          icon = icon("tree"),
          card_body(
            class = "subpanel",
            navset_card_underline(
              nav_panel(
                title = "Land Cover Map",
                icon = icon("layer-group"),
                p(
                  "Map files can be uploaded individually or in an archived file (.zip).
                The map files from",
                  tags$b("R-Fallow") ,
                  "output can be uploaded directly."
                ),
                layout_column_wrap(
                  card_body(
                    padding = 0,
                    fileInput(
                      "rfalow_lc_map_inp",
                      NULL,
                      accept = c(".tif", ".zip"),
                      multiple = T,
                      width = "100%"
                    ),
                    card_body(padding = 0, table_edit_ui(
                      "lc_df_table", tags$i("Land cover legend")
                    ))
                  ),
                  card_body(padding = 10, uiOutput("lc_map_out"))
                )
              ),
              nav_panel(
                title = "Hydrological Properties",
                icon = icon("droplet"),
                card_body(height = "50%", table_edit_ui("lc_props_table")),
                card_body(height = "50%", table_edit_ui(
                  "lc_evapot_table",
                  markdown("*Multiplier of Daily Potential Evapotranspiration*")
                ))
              ),
              nav_panel(
                title = "Evapotranspiration",
                icon = icon("cloud-sun"),
                navset_card_pill(
                  nav_panel(title = "Daily Data", layout_column_wrap(
                    style = css(grid_template_columns = "1fr 2fr"),
                    table_edit_ui(
                      "evapotran_df_table",
                      markdown("*Potential Evapotranspiration (mm day<sup>-1</sup>)*"),
                      is_paginated = T
                    ),
                    card(
                      plotlyOutput("evapotran_daily_plot"),
                      plotlyOutput("evapotran_monthly_plot"),
                      plotlyOutput("evapotran_yearly_plot"),
                      full_screen = T
                    )
                  )),
                  nav_panel(title = "Monthly Data", layout_column_wrap(
                    style = css(grid_template_columns = "1fr 2fr"),
                    table_edit_ui(
                      "evapot_monthly_df_table",
                      markdown("*Potential Evapotranspiration (mm month<sup>-1</sup>)*"),
                      is_paginated = T
                    ),
                    card(plotlyOutput("evapot_monthly_data_plot"), full_screen = T)
                  ))
                )
              )
            )
          )
          
        )
        ,
        nav_panel(
          title = "Watershed",
          icon = icon("water"),
          card_body(
            class = "subpanel",
            navset_card_underline(
              nav_panel(
                title = "Watershed Area",
                icon = icon("mountain"),
                card_body(
                  padding = 0,
                  leafletOutput("watershed_map_leaflet"),
                  conditionalPanel(condition = "output.is_stream_map", jqui_draggable(outlet_table))
                )
              ),
              nav_panel(
                title = "Routing Distance",
                icon = icon("route"),
                card_body(padding = 0, leafletOutput("routing_map_leaflet"))
              ),
              nav_panel(
                title = "3D View",
                icon = icon("cube"),
                card_body(padding = 0, rglwidgetOutput("plot3d", width = "calc(100% - 20px)"))
              )
            )
          )
        ),
        nav_panel(
          title = span(icon("icicles"), "Soil"),
          card_body(class = "subpanel", navset_card_underline(
            nav_panel(
              title = "Physical and Chemical Properties",
              icon = icon("flask"),
              card_body(padding = 0, leafletOutput("soil_map_leaflet"))
            ),
            nav_panel(
              title = "Hydraulic Properties",
              icon = icon("droplet"),
              navset_card_pill(
                nav_panel(
                  title = "Top Soil (10-20 cm)",
                  card_body(padding = 0, reactableOutput("soil_hydraulic_top_table"))
                ),
                nav_panel(
                  title = "Sub Soil (20-200 cm)",
                  card_body(padding = 0, reactableOutput("soil_hydraulic_sub_table"))
                ),
                nav_panel(
                  title = "Soil Water Content",
                  card_body(padding = 0, reactableOutput("soil_water_content_table"))
                )
              )
            )
          ))
        ),
        nav_panel(
          title = "Rain and Rivers",
          icon = icon("cloud"),
          card_body(
            class = "subpanel",
            navset_card_underline(
              nav_panel(
                title = "Rainfall Data",
                icon = icon("cloud-showers-heavy"),
                layout_column_wrap(
                  style = css(grid_template_columns = "1fr 2fr"),
                  table_edit_ui(
                    "rain_df_table",
                    markdown("*Daily rainfall data (mm day<sup>-1</sup>)*"),
                    is_paginated = T
                  ),
                  card(
                    plotlyOutput("rain_daily_plot"),
                    plotlyOutput("rain_monthly_plot"),
                    plotlyOutput("rain_yearly_plot"),
                    full_screen = T
                  )
                )
              ),
              nav_panel(
                title = "River Flow Data",
                icon = icon("bacon"),
                layout_column_wrap(
                  style = css(grid_template_columns = "1fr 2fr"),
                  table_edit_ui(
                    "river_df_table",
                    markdown("*River flow data (m<sup>3</sup>sec<sup>-1</sup>)*"),
                    is_paginated = T
                  ),
                  card(
                    plotlyOutput("river_daily_plot"),
                    uiOutput("river_cum_desc"),
                    plotlyOutput("river_cum_plot"),
                    full_screen = T
                  )
                )
              ),
              nav_panel(
                title = span("Consistency Check"),
                icon = icon("check"),
                uiOutput("consistency_ui")
              )
            )
          )
        ),
        nav_spacer(),
        nav_menu(
          title = NULL,
          icon = icon("ellipsis-vertical"),
          nav_item(
            style = "margin: 0 20px",
            fileInput(
              "upload_parameter",
              span(icon("upload"), "Upload input data"),
              accept = ".zip",
              # placeholder = "Parameters file",
              width = "300px"
            )
          ),
          nav_item(style = "border-top: 2px dashed lightgray; margin:10px 20px"),
          nav_item(span(
            icon("download"),
            downloadLink("download_params", "Download input data"),
            style = "margin:0 20px"
          ))
        )
      )
    ),
    
    ### OUTPUT #############################
    
    nav_panel(
      title = "Output data",
      icon = icon("arrow-up"),
      layout_column_wrap(fill = F, card(
        card_header("Output", class = "bg_theme"), p("Output")
      ))
    )
    
    
  )
