


#jekyll-theme-minimal

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


menu_button <- function(id,
                        label,
                        icon = NULL,
                        desc = NULL,
                        placement = "auto") {
  d <- actionButton(id, label, icon = icon, class = "menu_button")
  if (is.null(desc)) {
    return(d)
  } else {
    return(tooltip_blue(
      d,
      desc,
      placement = placement,
      id = paste0("tooltip_", id)
    ))
  }
}


outlet_table <- card(
  class = c("transparent_bg"),
  full_screen = TRUE,
  card_header(
    span(icon("droplet"), "Sub-Catchments", style = "padding-top:5px"),
    class = "d-flex justify-content-between",
    style = "border-bottom: 2px solid #F4F4F4;",
    span(
      menu_button(
        "delete_outlet_btn",
        "Delete",
        icon("trash-can"),
        "Remove the selected subcathments",
        "top"
      ),
      menu_button(
        "merge_outlet_btn",
        "Merge",
        icon("object-group"),
        "Merge the selected subcathments",
        "top"
      ),
      actionButton(
        "menu_outlet_btn",
        "",
        icon = icon("ellipsis-vertical"),
        class = "menu_button"
      ) |> popover(
        id = "subcatchment_popover",
        div("Subcatchments:"),
        div(menu_button(
          "reset_subcathment", "Reset", icon("arrows-rotate")
        )),
        div(
          menu_button(
            "regenerate_subcathment",
            "Re-generate...",
            icon("wand-magic-sparkles")
          )
        ),
        
        div("Download:", style = "margin-top:20px"),
        downloadButton("download_subcathment", "Subcathment Map", class = "menu_button"),
        downloadButton("download_stream", "Stream Map", class = "menu_button"),
        downloadButton("download_dem", "DEM Map", class = "menu_button")
      )
    )
  ),
  card_body(
    padding = 0,
    reactableOutput("watershed_list", height = "500px") |> popover(
      id = "watershed_pop",
      title = "Edit",
      textInput("watershed_pop_input", NULL),
      actionButton("watershed_pop_confirm", "Confirm"),
      options = list(trigger = "manual")
    )
  ),
  card_footer(
    style = "background-color: rgba(255, 255, 255, 0.5);",
    "Unclassified watershed area:" ,
    tags$b(textOutput("ws_unclassified_area", inline = T)),
    "ha",
    info(
      "Unclassified watershed area will be considered as a sub-catchment which covers the remaining area (ID: 999)"
    )
  )
)

lake_table <- card(
  class = c("transparent_bg"),
  card_header(span("Lake")),
  card_body(
    padding = 0,
    reactableOutput("lake_list") |> popover(
      id = "edit_lake_label",
      title = "Edit Label",
      textInput("lake_label", NULL),
      actionButton("edit_label_confirm", "Confirm")
    )
  )
)

dam_table <- card(
  class = c("transparent_bg"),
  card_header(span("DAM")),
  card_body(padding = 0, reactable_edit_ui("dam_table"))
)

textc <- function(text) {
  div(text, style = "padding:5px 0")
}

download_link <- function(id, filename = NULL) {
  if (is.null(filename))
    filename <- paste0(id, ".csv")
  div(style = "margin-left:auto; margin-right:0;", table_download_link(id, filename = filename))
}

inset_plot <- function(plot_id, top = "70px") {
  absolutePanel(
    top = top,
    right = "20px",
    width = "300px",
    card(
      class = "transparent_bg",
      full_screen = TRUE,
      card_body(
        padding = 10,
        height = "250px",
        div(tags$b(textOutput(
          paste0(plot_id, "_title")
        )), style = "text-align:right"),
        plotOutput(plot_id)
      )
    )
  )
}

soil_segment_setting_ui <- layout_sidebar(
  class = "p-0",
  sidebar = sidebar(
    padding = 0,
    gap = 0,
    width = "350px",
    class = "bordercard squarecard",
    title = h5("Segmentation Setting", style = "margin:15px 20px"),
    card_body(padding = 0, navset_card_tab(
      nav_panel(
        title = span("Segmentation"),
        icon = icon("draw-polygon"),
        "Parameter setting on soil depth and procedural map segmentation.
        The segmentation calculated based on slope map and elevation map factors",
        accordion(
          open = F,
          accordion_panel(
            "Soil depth",
            h5("Soil depth ranges"),
            numericInput("min_soil_depth_input", "Minimum (cm)", 20),
            numericInput("max_soil_depth_input", "Maximum (cm)", 200),
            numericInput("top_soil_prop_input", "Depth of top soil (%)", 10),
            tags$hr(),
            h5("Slope map factor"),
            numericInput("slope_coe_input", "Scaling coefficient", 1),
            numericInput("slope_exp_input", "Scaling exponent", 1),
            tags$hr(),
            h5("Elevation map factor"),
            numericInput("elevation_coe_input", "Scaling coefficient", 1),
            numericInput("elevation_exp_input", "Scaling exponent", 1),
          ),
          accordion_panel(
            "Map segmentations",
            numericInput("n_class_input", "Number of segmentation classes", 10, min = 0),
            tags$hr(),
            h5("Slope map factor"),
            numericInput("slope_coe_segment_input", "Scaling coefficient", 1),
            numericInput("slope_exp_segment_input", "Scaling exponent", 1),
            tags$hr(),
            h5("Elevation map factor"),
            numericInput("elevation_coe_segment_input", "Scaling coefficient", 1),
            numericInput("elevation_exp_segment_input", "Scaling exponent", 1),
            tags$hr(),
            h5("Soil depth factor"),
            numericInput("depth_coe_segment_input", "Scaling coefficient", 1),
            numericInput("depth_exp_segment_input", "Scaling exponent", 1),
            tags$hr(),
            h5("Edge smoothing filter"),
            p(
              "A modal filter which smoothing the edge and removing small patches of segments"
            ),
            numericInput("noise_filter_input", "Filter mask size (pixels)", 15, min = 1),
            numericInput("noise_rep_input", "Repetition", 3, min = 0)
          ),
          accordion_panel(
            "Slope map smoothing filter",
            numericInput("slope_filter_input", "Filter mask size (pixels)", 15, min = 1),
            plotOutput("slope_std_plot", height = 200)
          ),
          accordion_panel(
            "Elevation map smoothing filter",
            numericInput("elevation_filter_input", "Filter mask size (pixels)", 15, min = 1),
            plotOutput("elevation_std_plot", height = 200)
          )
        )
      ),
      nav_panel(
        title = "Soil Map Table",
        icon = icon("table"),
        card_body(
          padding = 10,
          "List of segments and the asscociated soil type ID",
          conditionalPanel(condition = "input.soil_type_select == 'soil_type_global'", reactableOutput("soil_mapped_table_global")),
          conditionalPanel(condition = "input.soil_type_select == 'soil_type_user'", table_edit_ui("soil_mapped_table_user"))
        )
      )
    ))
  ),
  card_body(
    padding = 0,
    leafletOutput("soil_type_leaflet"),
    inset_plot("slope_map_plot", top = "20px"),
    inset_plot("elevation_map_plot", top = "290px"),
    inset_plot("depth_map_plot", top = "560px")
  )
)

erosion_setting_ui <- card_body(
  class = "subpanel",
  padding = 0,
  navset_card_pill(
    nav_panel(title = "Riparian Zone", card_body(
      padding = 0,
      leafletOutput("riparian_leaflet"),
      absolutePanel(
        top = "70px",
        right = "20px",
        width = "250px",
        card(
          class = "transparent_bg",
          card_body(
            gap = 5,
            numericInput("riparian_dist_input", "Riparian distance (m)", 500, 0),
            span("Riparian zone area:", tags$b(textOutput(
              "riparian_area", inline = T
            )), "ha"),
            tags$br(),
            tags$b("Zone shape modifier"),
            numericInput("pre_Simple_input", "Pre-simplify (tolerance)", 100, 0),
            numericInput("post_Simple_input", "Post-simplify (tolerance)", 0, 0)
          )
        )
      )
    )),
    nav_panel(title = "Erosion Level", table_edit_ui("lc_erosion_table")),
    nav_panel(title = "Sedimentation Level", table_edit_ui("sedimentation_table")),
    nav_panel(title = "Conservation Scenario", table_edit_ui("conservation_table"))
  )
)



ui <-
  page_navbar(
    id = "main_page",
    theme = bs_theme(
      primary = theme_color$primary,
      secondary = theme_color$secondary,
      dark = theme_color$dark,
      success = theme_color$success,
      info = theme_color$info,
      warning = theme_color$warning,
      danger = theme_color$danger,
      font_scale = 0.9
      
    ),
    navbar_options = navbar_options(bg=theme_color$primary),
    # bg = theme_color$primary,
    header =
      tags$head(
        tags$style(
          tags$link(rel = "shortcut icon", href = "favicon.ico"),
          HTML(
            "
            .modal {z-index: 1150;}

            .soil_table th, td {
              padding: 5px;
              text-align: left;
              border-bottom: 1px solid #ddd;
            }

            .transparent_bg {
              background-color: rgba(255, 255, 255, 0.7);
              box-shadow: 2px 2px 8px #404040C9;
            }

            .gray_bg {
              background-color: #F8F9FA;
            }

            .menu_button {
              height: 26px;
              padding: 2px 10px;
              margin: 2px;
              border-width: 0px;
            }

            .custom-tooltip {
              --bs-tooltip-bg: #023047D9;
              --bs-tooltip-border-radius: 8px;
              --bs-tooltip-opacity: 1;
              --bs-tooltip-max-width: 300px;
            }

            .map_label {
              color: white;
              background-color: #023047E9 ;
              border-width: 1px;
              border-radius: 10px;
              box-shadow: 3px 3px 12px black;
            }

            .leaflet-popup-content-wrapper{
              background-color: rgba(255, 255, 255, 0.85);
              box-shadow: 4px 4px 20px black;
            }

            .leaflet-popup-tip {
              background: rgba(255, 255, 255, 0.85);
            }

            #input_panel, #sim_panel, #info_panel {
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

            .squarecard .card {
              border-radius:0px
            }

            .squarecard .card-header {
              border-radius:0px
            }

            .inline label{ display: table-cell; text-align: left; vertical-align: middle; padding: 0px 10px}
            .inline .form-group{display: table-row;}

            #I_WarmUpTime, #ndays_input {
              color:#219ebc;
              border-width:0;
              height:30px;
              padding-right:0;
            }

            .border_right {
              border-right: 1px solid rgba(0, 0, 0, 0.05);
            }
/*
            .leaflet-control-container { position:absolute; top:35px; width = 300P }
*/
            .highlight_label {
              color: black;
              background-color: white;
              font-size: 1.5em;
              padding: 5px 10px;
              margin:10px 5px;
              border-radius:5px;
            }

            .highlight_label_blue {
              color: white;
              background-color: #219ebc;
              padding: 5px 20px;
              margin:10px 10px 0 10px;
              border-radius:5px;
              font-size: 1em;
            }

            #soil_type_select {
              margin:10px;
            }

            .soil_group {
              color:white;
              background-color:#CAEDF6;
              margin: 0 5px;
              font-family:'Arial black';
            }


          "
          )
        ),
        tags$script(src = "jexcel.js"),
        tags$link(rel = "stylesheet", href = "jexcel.css", type = "text/css"),
        tags$script(src = "jsuites.js"),
        tags$link(rel = "stylesheet", href = "jsuites.css", type = "text/css"),
        tags$link(rel = "stylesheet", type = "text/css", href = "table.css"),
        tags$link(rel = "stylesheet", type = "text/css", href = "yinyang.css"),
        tags$link(rel = "stylesheet", type = "text/css", href = "wave.css"),
        
        tags$head(
          tags$script(src = "https://www.googletagmanager.com/gtag/js?id=G-KJN0VTGXHG")
        ),
        tags$head(tags$script(
          HTML(
            "window.dataLayer = window.dataLayer || [];
        function gtag(){dataLayer.push(arguments);}
        gtag('js', new Date());

        gtag('config', 'G-KJN0VTGXHG');
        "
          )
        ))
        
        
        
      ),
    window_title = "GenRiver3",
    title =
      span(
        tags$img(
          height = 22,
          src = "images/genriver_logo.svg",
          style = "margin-right:5px;"
        ),
        "Gen",
        span(
          "River",
          style = "color:#fff;",
          .noWS = c('before', "after")
        ),
        "3",
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
            style = "color:#ffb703;font-weight:bold;font-size:4em; text-shadow: 2px 2px 10px black;"
          )
        ),
        wave_div,
      ),
      div(class = "content flex", div(
        div(
          tags$b("GenRiver"),
          "is a generic river model on river flow",
          style = "margin-bottom:60px"
        ),
        HTML("&copy; World Agroforestry (ICRAF) - 2025")
      ))
    ),
    nav_panel(
      #### INPUT OPTIONS ####
      title = "Input",
      icon = icon("arrow-down"),
      
      navset_card_tab(
        title = div("Input data and parameters", style = "color:#219ebc;font-size:1.2em; padding:5px 0 0;font-family:'Arial black';"),
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
                markdown(desc$landcover),
                layout_column_wrap(
                  class = "bordercard",
                  
                  card_body(
                    padding = 0,
                    # actionButton(
                    #   "test_button",
                    #   "Test"
                    # ),
                    div(
                      actionButton(
                        "add_lc_button",
                        "Add Land Cover Map",
                        icon = icon("plus"),
                        width = "100%"
                      ) |>
                        popover(
                          id = "add_lc_pop",
                          fileInput(
                            "lc_map_inp",
                            "Add land cover map files",
                            accept = c(".tif", ".zip"),
                            multiple = T,
                            width = "100%"
                          )
                        )
                    ) |>
                      tooltip_blue(
                        "Map files can be uploaded individually or in an archived file (.zip)",
                        placement = "top"
                      ),
                    card_body(uiOutput("lc_map_out"))
                  ),
                  conditionalPanel(condition = "output.is_lc_df", card(
                    table_edit_ui("lc_df_table", h5("Land cover legend"), is_label = T)
                  ))
                )
              ),
              nav_panel(
                title = "Hydrological Properties",
                icon = icon("arrow-up-from-ground-water"),
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
        
        ### WATERSHED ###################
        
        nav_panel(
          title = "Watershed",
          icon = icon("water"),
          card_body(
            class = "subpanel",
            padding = 0,
            navset_card_underline(
              nav_panel(
                title = "Watershed Map",
                icon = icon("mountain-sun"),
                card_body(
                  padding = 0,
                  leafletOutput("watershed_map_leaflet"),
                  conditionalPanel(condition = "output.is_subcatchment", absolutePanel(
                    top = "70px",
                    left = "80px",
                    div(
                      div("Total area:", tags$b(textOutput("ws_area", inline = T)), "ha"),
                      div("Number of sub-catchment:", tags$b(textOutput("ws_n", inline = T))),
                      class = "transparent_bg",
                      style = "padding: 5px 10px; border-radius:5px;"
                    )
                  )),
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
                title = "3D View",
                icon = icon("cube"),
                card_body(padding = 5, plotlyOutput("ws3d_plot"))
              ),
              nav_panel(
                title = "Lake and DAM",
                icon = icon("fish"),
                card_body(
                  padding = 0,
                  navset_card_pill(
                    # nav_panel(
                    #   title = "Lake and DAM Location",
                    #   card_body(
                    #     padding = 0,
                    #     leafletOutput("lake_map_leaflet"),
                    #     conditionalPanel(
                    #       condition = "output.is_lake_df",
                    #       absolutePanel(
                    #         lake_table,
                    #         draggable = T,
                    #         right = "300px",
                    #         top = "70px",
                    #         width = "260px"
                    #       )
                    #     ),
                    #     conditionalPanel(
                    #       condition = "output.is_dam_df",
                    #       absolutePanel(
                    #         dam_table,
                    #         draggable = T,
                    #         right = "20px",
                    #         top = "70px",
                    #         width = "260px"
                    #       )
                    #     )
                    #   )
                    # ),
                    
                    nav_panel(title = "Lake Map", card_body(
                      padding = 0,
                      leafletOutput("lake_leaflet"),
                      absolutePanel(
                        top = "70px",
                        left = "80px",
                        div(
                          flowLayout(
                            cellArgs = list(style = "width:auto; margin:10px;"),
                            actionButton("add_lake_button", "Upload Lake Map", icon = icon("folder-open")) |>
                              popover(
                                id = "add_lake_pop",
                                fileInput(
                                  "lake_map_inp",
                                  'Upload lake map in shape format (".shp", ".dbf", ".shx", ".prj")',
                                  accept = c(".shp", ".dbf", ".shx", ".prj", ".zip"),
                                  multiple = T
                                )
                              ),
                            div(style = "margin-top:10px;",
                            input_switch("apply_lake", "Apply Lake Sub-catchment", width = "100%")),
                          ),
                          class = "transparent_bg",
                          style = "padding:0px; border-radius:5px;"
                        )
                      )
                    )),
                    nav_panel(title = "Lake Outflows", card_body(
                      flowLayout(
                        cellArgs = list(style = "width:auto; margin:0px;"),
                        !!!numeric_input_ui("lake_par_input", lake_par_df, width = "200px")
                      )
                    ))
                  )
                )
              ),
              nav_panel(
                title = "Ground water and river flow",
                icon = icon("house-flood-water"),
                h5("Ground water dynamic and time of river flow"),
                card(table_edit_ui("ground_water_table"))
              )
              
              
            )
          )
        ),
        
        ### SOIL #############################################
        
        nav_panel(
          title = "Soil",
          icon = icon("icicles"),
          card_body(
            class = "subpanel",
            padding = 0,
            navset_card_underline(
              nav_panel(
                title = "Physical and Chemical Properties",
                icon = icon("flask"),
                card_body(
                  padding = 0,
                  gap = 0,
                  div(
                    class = "highlight_label_blue",
                    radioButtons(
                      "soil_type_select",
                      NULL,
                      list(
                        "Use Global Soil Database" = "soil_type_global",
                        "Define the soil types manually" = "soil_type_user"
                      ),
                      inline = T
                    )
                  ),
                  navset_card_underline(
                    id = "soil_type_panel",
                    nav_panel(
                      title = "Soil Type List",
                      icon = icon("list"),
                      card_body(
                        class = "bordercard",
                        padding = 10,
                        conditionalPanel(condition = "input.soil_type_select == 'soil_type_user'", div(div(
                          actionButton("import_soil_button", "Import from Global Soil Database")
                        ), div(
                          table_edit_ui("soil_type_table", is_label = T, vspace = "30px")
                        ))),
                        conditionalPanel(condition = "input.soil_type_select == 'soil_type_global'", card_body(
                          padding = 0,
                          markdown(desc$soil_db),
                          navset_card_tab(
                            nav_panel(
                              title = "Soil Types",
                              icon = icon("mountain"),
                              markdown(desc$soil_list),
                              reactableOutput("soil_type_global_table")
                            ),
                            nav_panel(
                              title = "Global Soil Database",
                              icon = icon("database"),
                              
                              card_body(padding = 0, leafletOutput("soil_map_leaflet"))
                            ),
                            height = "100%"
                          )
                        ))
                      )
                    ),
                    nav_panel(
                      title = "Soil Map",
                      icon = icon("earth-asia"),
                      soil_segment_setting_ui
                    )
                  )
                )
              ),
              nav_panel(
                title = "Hydraulic Properties",
                icon = icon("house-flood-water-circle-arrow-right"),
                card_body(padding = 0, navset_card_pill(
                  nav_panel(
                    title = "Soil Water Map",
                    card_body(
                      padding = 10,
                      class = "bordercard",
                      flowLayout(
                        cellArgs = list(style = "width:auto; margin:0"),
                        style = " margin:0",
                        actionButton(
                          "generate_soil_water_button",
                          "Calculate Soil Water",
                          icon = icon("droplet")
                        ),
                        div(
                          style = "margin-top:10px",
                          radioButtons("soil_water_select", NULL, soil_water_availability, inline = T)
                        )
                      ),
                      navset_card_tab(
                        nav_panel(title = "Subcatchment Soil Water", uiOutput("soil_water_subcathment_ui")),
                        nav_panel(title = "Soil Water By Land Cover Map", uiOutput("soil_water_content_ui"))
                      )
                    )
                  ),
                  
                  nav_panel(
                    title = "Soil Water Lookup Table",
                    download_link("soil_water_content_table"),
                    reactableOutput("soil_water_content_table")
                  )
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
              ),
              nav_panel(
                title = "Soil Erosion and Sedimentation",
                icon = icon("hill-rockslide"),
                erosion_setting_ui
              )
            )
          )
        ),
        
        ### RAINFALL AND RIVER #############################################
        
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
        
        nav_menu(
          title = "Options",
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
    
    ### SIMULATION #############################
    
    nav_panel(
      title = "Simulation",
      icon = icon("gears"),
      navset_card_tab(
        id = "sim_panel",
        title =
          flowLayout(
            style = "color:#219ebc;font-weight:bold",
            cellArgs = list(style = "width:auto; margin:0px; height:30px;"),
            textc("Warm Up Time (days):"),
            numericInput("I_WarmUpTime", NULL, value = 730, width = "100px"),
            textc("Simulation Time (days):"),
            numericInput("ndays_input", NULL, value = 2000, width = "100px"),
            actionButton(
              "sim_run_button",
              "Run Simulation",
              icon = icon("play"),
              style = "width:auto;height:32px;padding:0px 20px;"
            )
          ),
        nav_panel(
          title = "Output",
          icon = icon("arrow-up"),
          card_body(
            class = "subpanel",
            padding = 0,
            navset_card_underline(
              nav_panel(title = "Water Balance", card_body(
                padding = 0, navset_card_pill(
                  nav_panel(title = "Cumulative Plot", card(
                    full_screen = T,
                    plotlyOutput("water_balance_plot", height = "100%")
                  )),
                  nav_panel(
                    title = "Data Table",
                    download_link("water_balance_table_output"),
                    reactableOutput("water_balance_table_output")
                  )
                )
              )),
              nav_panel(
                title = "Watershed Indicator",
                card_body(
                  padding = 0,
                  gap = 0,
                  flowLayout(
                    textc("Starting month of hydrology year:"),
                    selectInput(
                      "month_hydro_select",
                      NULL,
                      choices = month.name,
                      width = "150px"
                    ),
                    cellArgs = list(style = "width:auto; margin-top:auto; margin-bottom:0;"),
                    style = "margin:10px 0 0 20px;"
                  ),
                  
                  
                  
                  navset_card_pill(
                    nav_panel(title = "Daily River Flow", card_body(
                      h5("Hydrograph of Observed and Simulated River Flow "),
                      uiOutput("river_flow_ui")
                    )),
                    nav_panel(title = "Cumulative River Flow", card_body(
                      h5(
                        "Double Mass Curve of Cumulative Rainfall VS Cumulative River Flow (Observation and Simulation)"
                      ),
                      uiOutput("cum_river_flow_ui")
                    )),
                    nav_panel(
                      title = "Performance Test",
                      download_link("performance_table_output"),
                      reactableOutput("performance_table_output")
                    ),
                    nav_panel(
                      title = "Data Table",
                      download_link("watershed_indicator_table_output"),
                      reactableOutput("watershed_indicator_table_output")
                    )
                  )
                )
              ),
              nav_panel(title = "Buffering Indicator", card_body(
                padding = 0,
                gap = 0,
                navset_card_pill(
                  nav_panel(title = "Graph", card_body(
                    h5("Indicators Per Year"),
                    layout_column_wrap(
                      width = 0.5,
                      plotlyOutput("indicator1_plot"),
                      plotlyOutput("indicator2_plot"),
                      plotlyOutput("indicator3_plot"),
                      plotlyOutput("indicator4_plot")
                    )
                  )),
                  nav_panel(title = "Average of Indicators", card_body(
                    h5("Average of Indicators of Watershed Functions"),
                    download_link("buff_avg_table_output"),
                    reactableOutput("buff_avg_table_output")
                  )),
                  nav_panel(title = "Average of Water Balance", card_body(
                    h5("Average of Yearly Water Balance"),
                    download_link("wb_avg_table_output"),
                    reactableOutput("wb_avg_table_output")
                  )),
                  nav_panel(title = "Yearly Water Balance Data", card_body(
                    h5("Yearly Water Balance Data"),
                    download_link("wb_yr_table_output"),
                    reactableOutput("wb_yr_table_output")
                  ))
                )
              )),
              nav_panel(title = "HEPP", card_body(
                padding = 0, navset_card_pill(
                  nav_panel(title = "Cumulative Plot", plotlyOutput("hepp_plot", height = "100%")),
                  nav_panel(
                    title = "Data Table",
                    download_link("hepp_table_output"),
                    reactableOutput("hepp_table_output")
                  )
                )
              )),
              nav_panel(
                title = "",
                icon = icon("location-crosshairs"),
                card_body(
                  gap = 0,
                  style = "min-height:175px; margin:0;padding-bottom:0",
                  h4("Variable Check"),
                  p(
                    style = "padding-bottom:20px",
                    "Calculated variables with constant rain and evapotranspiration in one iteration"
                  ),
                  flowLayout(
                    cellArgs = list(style = "width:auto; margin:0px;"),
                    numericInput(
                      "test_month_input",
                      "Month:",
                      value = 1,
                      width = "80"
                    ),
                    numericInput(
                      "test_year_input",
                      "Year:",
                      value = 1990,
                      width = "80"
                    ),
                    numericInput(
                      "test_rain_day_input",
                      "I_DailyRain (mm):",
                      value = 50,
                      width = "180"
                    ),
                    numericInput(
                      "test_evapotrans_input",
                      "I_Daily_Evapotrans (mm):",
                      value = 2,
                      width = "180"
                    ),
                    numericInput(
                      "test_iteration_input",
                      "Iteration#:",
                      value = 2,
                      width = "80"
                    ),
                    actionButton("run_calibration_button", "Check!", style =
                                   "height:80px")
                  )
                ),
                card_body(
                  class = "subpanel",
                  padding = 0,
                  height = "100%",
                  navset_card_underline(
                    nav_panel(title = "All Variables", layout_columns(
                      div(
                        h5("General variables"),
                        download_link("general_table_output"),
                        reactableOutput("general_table_output")
                      ),
                      div(
                        h5("Cumulative values of land cover and subcatchment variables"),
                        download_link("cumulative_table_output"),
                        reactableOutput("cumulative_table_output")
                      )
                    )),
                    nav_panel(
                      title = "Land Cover Variables",
                      download_link("subc_lc_table_output"),
                      reactableOutput("subc_lc_table_output")
                    ),
                    nav_panel(
                      title = "Subcatchment Variables",
                      download_link("subc_table_output"),
                      reactableOutput("subc_table_output")
                    )
                  )
                )
              )
              
            )
          )
        )
      )
    ),
    
    ### FLOWPER ##########################
    
    nav_panel(
      title = "FlowPer",
      icon = icon("wave-square"),
      layout_sidebar(
        sidebar = sidebar(
          title = "Fp-Value Table",
          padding = 10,
          gap = 0,
          width = "350px",
          reactableOutput("flowper_yearly_table")
        ),
        layout_column_wrap(
          width = 0.5,
          plotlyOutput("fp1_plot"),
          plotlyOutput("fp2_plot"),
          plotlyOutput("fp3_plot"),
          plotlyOutput("fp4_plot")
        )
      )
    ),
    
    ### ABOUT ##########################
    
    nav_panel(
      title = "",
      icon = bs_icon("question-circle", size = "1.3em"),
      navset_card_tab(
        id = "info_panel",
        nav_panel(
          title = "About",
          icon = icon("circle-info"),
          card_body(includeMarkdown("docs/about.md"))
        ),
        nav_panel(
          title = "Background",
          icon = icon("book"),
          card_body(includeMarkdown("docs/background.md"))
        ),
        nav_panel(
          title = "Tutorial",
          icon = icon("book"),
          card_body(includeMarkdown("docs/manual.md"))
        ),
        nav_panel(
          title = "References",
          icon = icon("bookmark"),
          card_body(includeMarkdown("docs/references.md"))
        ),
        nav_panel(
          title = "Appendix",
          icon = icon("screwdriver-wrench"),
          card_body(includeMarkdown("docs/appendix.md"))
        )
      )
    )
    
    
  )
