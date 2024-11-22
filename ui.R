
wave_div <- HTML(
  '<div><svg class="waves" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"
            viewBox="0 24 150 28" preserveAspectRatio="none" shape-rendering="auto">
          <defs>
          <path id="gentle-wave" d="M-160 44c30 0 58-18 88-18s 58 18 88 18 58-18 88-18 58 18 88 18 v44h-352z" />
          </defs>
          <g class="parallax">
          <use xlink:href="#gentle-wave" x="48" y="0" fill="rgba(255,255,255,0.7" />
          <use xlink:href="#gentle-wave" x="48" y="3" fill="rgba(255,255,255,0.5)" />
          <use xlink:href="#gentle-wave" x="48" y="5" fill="rgba(255,255,255,0.3)" />
          <use xlink:href="#gentle-wave" x="48" y="7" fill="#fff" />
          </g></svg></div>'
)


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
  class = c("transparent_bg"),
  card_header(
    span(icon("droplet"), "Sub-Catchment"),
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
  card_body(padding = 0, reactableOutput("watershed_list", height = "500px"))
)

lake_table <- card(
  class = c("transparent_bg"),
  card_header(span("Lake")),
  card_body(padding = 0, reactableOutput("lake_list") |> popover(
    id = "edit_lake_label",
    title = "Edit Label",
    textInput("lake_label", NULL),
    actionButton("edit_label_confirm", "Confirm")
  ))
)

dam_table <- card(
  class = c("transparent_bg"),
  card_header(span("DAM")),
  card_body(padding = 0, reactable_edit_ui("dam_table"))
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
    bg = "#034464",
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

            .modal {z-index: 1150;}

            .soil_table th, td {
              padding: 5px;
              text-align: left;
              border-bottom: 1px solid #ddd;
            }

            .transparent_bg {
              background-color: rgba(255, 255, 255, 0.7);
              box-shadow: 2px 2px 10px #404040;
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

            .subpanel .card {
              border-width: 0px;
            }

            .bordercard .card {
              border-width:1px;
            }
            
            .bordercard .card-header {
              border-width:1px;
              background-color: #ECF9FC;
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
        tags$link(rel = "stylesheet", type = "text/css", href = "yinyang.css"),
        tags$link(rel = "stylesheet", type = "text/css", href = "wave.css")
      ),
    window_title = "GenRiver3",
    title =
      span(
        tags$img(
          height = 22,
          src = "images/genriver_logo.svg",
          style = "margin-right:5px;" #filter: drop-shadow(1px 1px 3px black)
        ),
        "Gen",
        span(
          "River",
          style = "color:#fff;",
          .noWS = c('before', "after")
        ),
        "3",
        # background: linear-gradient(60deg, #CAEDF6 -20%, #219ebc 50%
        # style = "color:#ffb703; background-color: #CAEDF6;
        # padding:2px 20px 4px 20px; border-radius:15px; font-weight:bold;
        # text-shadow: 1px 1px 5px black;"
        style = "color:#ffb703; font-weight:bold; text-shadow: 1px 1px 4px black;"
      ),
    
    padding = 0,
    
    nav_panel(
      title = "",
      icon = icon("house"),

      div(
        class = "header",
        div(
          class = "inner-header flex",
          tags$img(
            height = 100,
            src = "images/genriver_logo.svg",
            style = "filter: drop-shadow(2px 2px 6px black);margin-right:10px;"
          ),
          tags$h1(
            "Gen",
            span(
              "River",
              style = "color:#fff;",
              .noWS = c('before', "after")
            ),
            "3",
            style = "color:#ffb703;;font-weight:bold;font-size:4em; text-shadow: 2px 2px 10px black;"
          )
        ),
        wave_div,
      ),
      div(class = "content flex", HTML("&copy; World Agroforestry (ICRAF) - 2024"))
    ),
    nav_panel(
      #### INPUT OPTIONS ####
      title = "Input",
      icon = icon("arrow-down"),
      
      navset_card_tab(
        title = div("Input data and parameters:", style = "color:#219ebc;"),
        id = "input_panel",
        nav_panel(
          title = "Land Cover",
          icon = icon("tree"),
          card_body(
            class = "subpanel",
            padding = 0,
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
                      #if shp, it should accept  = c('.shp','.dbf','.sbn','.sbx','.shx','.prj')
                      multiple = T,
                      width = "100%"
                    ),
                    card_body(padding = 0, table_edit_ui(
                      "lc_df_table", tags$i("Land cover legend")
                    ))
                  ),
                  card_body(padding = 10, class = "bordercard", uiOutput("lc_map_out"))
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
                icon = icon("cloud-sun-rain"),
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
        ),
        nav_panel(
          title = "Watershed",
          icon = icon("water"),
          card_body(
            class = "subpanel",
            padding = 0,
            navset_card_underline(
              nav_panel(
                title = "Watershed Area",
                icon = icon("mountain-sun"),
                card_body(
                  padding = 0,
                  leafletOutput("watershed_map_leaflet"),
                  conditionalPanel(
                    condition = "output.is_stream_map",
                    absolutePanel(
                      outlet_table,
                      draggable = T,
                      right = "20px",
                      top = "70px",
                      width = "430px"
                    )
                  )
                )
              ), 
              nav_panel(
                title = "Lake and DAM",
                icon = icon("fish"),
                card_body(padding = 0, navset_card_pill(
                  nav_panel(title = "Lake and DAM Location", card_body(
                    padding = 0, leafletOutput("lake_map_leaflet"),
                    conditionalPanel(
                      condition = "output.is_lake_df",
                      absolutePanel(
                        lake_table,
                        draggable = T,
                        right = "300px",
                        top = "70px",
                        width = "260px"
                      ) 
                    ),
                    conditionalPanel(
                      condition = "output.is_dam_df",
                      absolutePanel(
                        dam_table,
                        draggable = T,
                        right = "20px",
                        top = "70px",
                        width = "260px"
                      )
                    )
                  )),
                  nav_panel(title = "Lake Outflows", card_body(layout_columns(
                    !!!numeric_input_ui("lake_par_input", lake_par_df)
                  )))
                ))
              ), 
              nav_panel(
                title = "Ground water and river flow",
                icon = icon("house-flood-water"),
                h5("Ground water dynamic and time of river flow"),
                card(
                  table_edit_ui("ground_water_table")
                )
              ),
              # nav_panel(
              #   title = "Routing Distance",
              #   icon = icon("route"),
              #   card_body(
              #     padding = 0,
              #     leafletOutput("routing_map_leaflet"),
              #     absolutePanel(
              #       top = "70px",
              #       left = "80px",
              #       card(
              #         class = "transparent_bg",
              #       numericInput(
              #         "stream_velocity_input",
              #         markdown("Routing velocity (m sec<sup>-1</sup>)"),
              #         0.4, 0.01, NA, 0.1,  width = "180px"
              #       ))
              #     )
              #   )
              # ), 
              nav_panel(
                title = "3D View",
                icon = icon("cube"),
                conditionalPanel(
                  condition = "!output.is_show_3d",
                  actionButton("generate_3d_button", "Click here to generate 3D view")
                ),
                conditionalPanel(condition = "output.is_show_3d", card_body(
                  padding = 0, rglwidgetOutput("plot3d", width = "calc(100% - 20px)")
                ))
              )
            )
          )
        ),
        nav_panel(
          title = span(icon("icicles"), "Soil"),
          card_body(
            class = "subpanel",
            padding = 0,
            navset_card_underline(
              nav_panel(
                title = "Physical and Chemical Properties",
                icon = icon("flask"),
                card_body(
                  padding = 0,
                  navset_card_pill(
                    nav_panel(title = "Soil Type", card_body(
                      padding = 0, leafletOutput("soil_type_leaflet")
                    )),
                    nav_panel(title = "Soil Depth", card_body(
                      padding = 0,
                      leafletOutput("soil_depth_leaflet"),
                      absolutePanel(
                        top = "70px",
                        left = "80px",
                        card(
                          class = "transparent_bg",
                          numericInput("min_soil_depth_input", "Minimum soil depth (cm)", 20, width = "180px"),
                          numericInput("max_soil_depth_input", "Maximum soil depth (cm)", 200, width = "180px")
                        )
                      )
                    )),
                    nav_panel(title = "Global Soil Database", card_body(
                      padding = 0, leafletOutput("soil_map_leaflet")
                    )),
                    nav_panel(title = "Slope Map", card_body(
                      padding = 0, leafletOutput("slope_map_leaflet")
                    ))
                  )
                )
              ), 
              nav_panel(
                title = "Hydraulic Properties",
                icon = icon("house-flood-water-circle-arrow-right"),
                card_body(padding = 0,
                          actionButton("generate_soil_water_button","Generate soil hydraulic properties"),
                          navset_card_pill(
                            nav_panel(title = "Soil Water Map", card_body(
                              class = "bordercard", 
                              
                              navset_card_tab(
                                nav_panel(title = "Subcatchment Soil Water", uiOutput("soil_water_subcathment_ui")),
                                nav_panel(title = "Soil Water Map", 
                                          selectInput("soil_water_select", NULL, soil_water_availability),
                                          uiOutput("soil_water_content_ui"))
                                
                              )
                            )),
                            nav_panel(title = "Top Soil (10%)", reactableOutput("soil_hydraulic_top_table")),
                            nav_panel(title = "Sub Soil (90%)", reactableOutput("soil_hydraulic_sub_table")),
                            nav_panel(title = "Soil Water Availability", reactableOutput("soil_water_content_table"))
                            
                          ))
              ),
              nav_panel(
                title = "Soil and Plant Water",
                icon = icon("seedling"),
                layout_column_wrap(
                  style = css(grid_template_columns = "1fr 1fr 1fr"),
                  class = "bordercard",
                  fill = F,
                  fillable = F,
                  card(
                    card_header(markdown("Infiltration (mm day<sup>-1</sup>)")),
                    numeric_input_ui("infiltration_par_input", infiltration_par_df)
                  ),
                  card(
                    card_header("Ground Water (mm)"),
                    input_switch("groundwater_switch", "Use constant ground water"),
                    numeric_input_ui("groundwater_par_input", groundwater_par_df)
                  ),
                  card(
                    card_header("Water Available for Plant (mm)"),
                    input_switch("waterplant_switch", "Use constant water availability"),
                    numeric_input_ui("waterplant_par_input", waterplant_par_df)
                  ),
                  card(
                    card_header("Rainfall Interception (mm)"),
                    numeric_input_ui("interception_par_input", interception_par_df)
                  ),
                  card(
                    card_header("Soil Water"),
                    numeric_input_ui("soilplant_par_input", soilplant_par_df)
                  )
                )
              )
            )
          )
        ),
        
        
        
        nav_panel(
          title = "Rainfall and Rivers",
          icon = icon("cloud"),
          card_body(
            class = "subpanel",
            padding = 0,
            navset_card_underline(
              nav_panel(
                title = "Rainfall",
                icon = icon("cloud-showers-heavy"),
                card_body(padding = 0, navset_card_pill(
                  nav_panel(
                    title = "Rainfall Intensity",
                    icon = icon("cloud-bolt"),
                    numeric_input_ui("rain_par_input", rain_par_df)
                  ),
                  nav_panel(
                    title = "Rainfall Data",
                    icon = icon("table"),
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
                  )
                ))
              ),
              nav_panel(
                title = "River",
                icon = icon("bacon"),
                card_body(padding = 0, navset_card_pill(
                  nav_panel(
                    title = "River hydrology",
                    icon = icon("tornado"),
                    numeric_input_ui("river_par_input", river_par_df)
                  ),
                  nav_panel(
                    title = "River Flow Data",
                    icon = icon("table"),
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
                  )
                ))
              ),
              nav_panel(
                title = span("Consistency Check"),
                icon = icon("check"),
                uiOutput("consistency_ui")
              )
            )
            
          )
        ),
        nav_panel(
          title = "Calibration",
          icon = icon("location-crosshairs"),
          layout_column_wrap(
            width = 1,
            heights_equal = "row",
            h4("Variable Check"),
            p(
              "Calculated variables with constant rain and evapotranspiration in one iteration"
            ),
            layout_columns(
              numericInput("test_month_input", "Month:", value = 1),
              numericInput("test_year_input", "Year:", value = 1990),
              numericInput("test_rain_day_input", "I_DailyRain (mm):", value = 50),
              numericInput("test_evapotrans_input", "I_Daily_Evapotrans (mm):", value = 2),
              numericInput("test_iteration_input", "Iteration step #:", value = 2)
            ),
            navset_card_pill(
              nav_panel(title = "All Variables", layout_columns(
                div(
                  h5("General variables"),
                  reactableOutput("general_table_output")
                ), div(
                  h5("Cumulative values of land cover and subcatchment variables"),
                  reactableOutput("cumulative_table_output")
                )
              )),
              nav_panel(title = "Land Cover Variables", reactableOutput("subc_lc_table_output")),
              nav_panel(title = "Subcatchment Variables", reactableOutput("subc_table_output"))
            )
          )
        ),  
        
        nav_menu(
          title = NULL,
          icon = icon("ellipsis-vertical"),
          nav_item(
            style = "margin: 0 20px",
            fileInput(
              "upload_parameter",
              span(icon("upload"), "Upload input data"),
              accept = ".zip",
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
    
    nav_panel(title = "Simulation",
              icon = icon("arrow-up"),
              navset_card_tab(nav_panel(
                title = "Simulation",
                card_body(
                  class = "subpanel",
                  padding = 0,
                  layout_column_wrap(
                    width = 1,
                    heights_equal = "row",
                    gap = "0px",
                    card(
                      fill = F,
                      layout_columns(
                        numericInput("I_WarmUpTime", "I_WarmUpTime:", value = 730),
                        numericInput("ndays_input", "Simulation Time:", value = 50),
                        actionButton(
                          "sim_run_button",
                          "Run",
                          icon = icon("play"),
                          style = "height:100%"
                        )
                      )
                    ),
                    
                    navset_card_underline(
                      nav_panel(title = "Water Balance", card_body(
                        padding = 0, navset_card_pill(
                          nav_panel(title = "Cumulative Plot", card(
                            full_screen = T, plotlyOutput("water_balance_plot", height = "100%")
                          )),
                          nav_panel(title = "Daily Data", reactableOutput("water_balance_table_output"))
                        )
                      )),
                      nav_panel(title = "Watershed Indicator", reactableOutput("watershed_indicator_table_output")),
                      nav_panel(title = "HEPP", card_body(
                        padding = 0, navset_card_pill(
                          nav_panel(title = "Cumulative Plot", plotlyOutput("hepp_plot", height = "100%")),
                          nav_panel(title = "Daily Data", reactableOutput("hepp_table_output"))
                        )
                      ))
                    )
                    
                  )
                )
              )))
    
    
  )
