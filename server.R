# 
# - soil zone should be combine elevation effect (include depth)
# - konsitensi check.. keluarin garis one one
    # tambah chek konsistensi (nima)
# ouput -> buffering indicator

# setelah warming up. trus balik lagi ke tanggal awal

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 1000 * 1024 ^ 2)
  data_dir <- paste0(tempdir(), "/data_temp")
  
  vars <-
    c("primary",
      "success",
      "info",
      "danger",
      "warning",
      "secondary",
      "dark")
  color_theme <-
    as.list(bs_get_variables(bs_current_theme(session), varnames = vars))
  
  map_color <-
    colorRampPalette(c("darkgreen", "#ffb703", "#9a130e", "#023e8a"))
  grad_color <- colorRampPalette(c("#023047", "#219ebc", "#06d6a0", "#ffb703", "#9a130e"))
  stream_color <-
    colorRampPalette(c( "#e07a5f", "#7D60E0","#606EE0","#609AE1","#0077b6", "#023e8a", "#03045e"))
  soil_color <- colorRampPalette(c("#331A05", "#a39171", "#FDEFE1"))

  water_color <-
    colorRampPalette(c("#D4FEF3", "#219ebc", "#034464"))
  
  options(reactable.theme = reactableTheme(
    style = list(fontFamily = "Arial, Helvetica, sans-serif", fontSize = "1em")
  ))
  
  f_number <- function(v, ...) {
    format(v, big.mark = ",", scientific = F, ...)
  }
  
  f_percent <- function(v) {
    sprintf("%0.1f%%", v * 100)
  }
  
  ### Conditional panel UI logic ###
  
  conditional_id <-
    c(
      "is_dem_map",
      "is_stream_map",
      "is_stream_render",
      "is_lc_df",
      "is_lake_df",
      "is_dam_df"
    )
  conditional_v <-
    c("dem_map_stars",
      "dem_stream_sf",
      "stream_render",
      "lc_df",
      "lake_df",
      "dam_df")
  
  mapply(function(id, val) {
    output[[id]] <- reactive({
      type <- suffix(val)
      if (is.null(v[[val]])) {
        return(FALSE)
      }
      if (type == "df") {
        if (nrow(v[[val]]) == 0)
          return(F)
      }
      TRUE
    })
    outputOptions(output, id, suspendWhenHidden = FALSE)
  }, conditional_id, conditional_v)
  
  is_lc_map_rendering <- reactiveVal(F)
  output$is_lc_map_rendering <- reactive({
    is_lc_map_rendering()
  })
  outputOptions(output, "is_lc_map_rendering", suspendWhenHidden = FALSE)
  
  is_spinner <- reactiveVal(F)
  output$is_spinner <- reactive({
    is_spinner()
  })
  outputOptions(output, "is_spinner", suspendWhenHidden = FALSE)
  
  is_show_3d <- reactiveVal(F)
  output$is_show_3d <- reactive({
    is_show_3d()
  })
  outputOptions(output, "is_show_3d", suspendWhenHidden = FALSE)
  
  month_cols <- data.frame(
    var = c(
      "Jan",
      "Feb",
      "Mar",
      "Apr",
      "May",
      "Jun",
      "Jul",
      "Aug",
      "Sep",
      "Oct",
      "Nov",
      "Dec"
    ),
    month = c(1:12)
  )
  
  ### VARIABLES #############################################
  
  v <- reactiveValues(
    genriver_cfg = list(),
    
    lc_map_stars_list = NULL,
    lc_map_df = NULL,
    lc_df = data.frame(
      lc_id = numeric(),
      color = character(),
      land_cover = character(),
      description = character(),
      stringsAsFactors = FALSE
    ),
    
    lc_par_df = data.frame(
      lc_id = numeric(),
      # land_cover = character(),
      I_InterceptClass  = numeric(),
      I_RelDroughtFact  = numeric(),
      I_BD_BDRefVegNow = numeric(),
      stringsAsFactors = FALSE
    ),
    lc_map_crop_stars_list = NULL,
    
    lc_evapot_df = NULL,
    evapot_month_data_df = data.frame(
      n = month_cols$month,
      month = month_cols$var,
      evapotranspiration =  rep(NA, 12),
      stringsAsFactors = FALSE
    ),
    
    ground_par_df = data.frame(
      map_id = character(),
      ws_id = numeric(),
      I_RivFlowTime = numeric(),
      I_MaxDynGWSub = numeric(),
      I_GWRelFrac = numeric(),
      stringsAsFactors = FALSE
    ),
    
    map_boundary_sf = NULL,
    
    dem_map_file = NULL,
    dem_map_stars = NULL,
    dem_crop_stars = NULL,
    dem_direction_terra = NULL,
    
    dem_flow_bb_stars = NULL,
    # dem_contour_bb_sf = NULL,
    dem_stream_bb_sf = NULL,
    
    ws_boundary_sf = NULL,
    ws_boundary_outlet_cfg = NULL,
    
    dem_flow_stars = NULL,
    # dem_contour_sf = NULL,
    dem_stream_sf = NULL,
    stream_render = NULL,
    
    watershed_df = NULL,
    watershed_map_list = NULL,
    outlet_map_list = NULL,
    
    routing_map_stars = NULL,
    routing_time_map_sf = NULL,
    routing_river_sf = NULL,
    
    watershed_map_stars = NULL,
    subcatchment_map_sf = NULL,
    
    
    slope_class_sf = NULL,

    soil_depth_stars = NULL,
    
    
    soil_map_stars = NULL,
    soil_map_sf = NULL,
    soil_layer_df = NULL,
    soil_metadata_df = NULL,
    soil_thetasat_stars = NULL,
    soil_thetasat_sf = NULL,
    
    soil_hydraulic_top_df = NULL,
    soil_hydraulic_sub_df = NULL,
    soil_water_content_df = NULL,
    

    soil_quick_flow_capacity_stars_list = NULL,
    soil_plant_available_water_stars_list = NULL,
    soil_inaccessible_water_stars_list = NULL,
    
    
    subc_soil_water_df = NULL,
    subc_lc_df = NULL,
    
    evapotran_df = data.frame(
      date = as.Date(character()),
      evapotranspiration = numeric(),
      stringsAsFactors = FALSE
    ),
    
    rain_df = data.frame(
      date = as.Date(character()),
      rainfall = numeric(),
      stringsAsFactors = FALSE
    ),
    
    river_df = data.frame(
      date = as.Date(character()),
      river_flow = numeric(),
      stringsAsFactors = FALSE
    ),
    
    rain_par_cfg = default_par(rain_par_df),
    river_par_cfg = default_par(river_par_df),
    
    infiltration_par_cfg = default_par(infiltration_par_df),
    groundwater_par_cfg = default_par(groundwater_par_df),
    waterplant_par_cfg = default_par(waterplant_par_df),
    interception_par_cfg = default_par(interception_par_df),
    soilplant_par_cfg = default_par(soilplant_par_df),
    
    stream_par_cfg = NULL,
    
    lake_df = data.frame(
      lake_id = numeric(),
      ws_id = numeric(),
      label = character(),
      stringsAsFactors = FALSE
    ),
    dam_df = data.frame(
      dam_id = numeric(),
      ws_id = numeric(),
      label = character(),
      stringsAsFactors = FALSE
    )
  )
  


  

  
  vd <- reactiveValues(
    lc_evapot_disp_df = data.frame(
      lc_id = numeric(),
      land_cover = character(),
      Jan = numeric(),
      Feb = numeric(),
      Mar = numeric(),
      Apr = numeric(),
      May = numeric(),
      Jun = numeric(),
      Jul = numeric(),
      Aug = numeric(),
      Sep = numeric(),
      Oct = numeric(),
      Nov = numeric(),
      Dec = numeric(),
      stringsAsFactors = FALSE
    ),
    evapotran_df = NULL,
    evapotran_month_df = NULL,
    rain_df = NULL,
    rain_month_df = NULL,
    river_df = NULL,
    soil_water_map_stars_list = NULL,
    lc_df_crop_list = NULL,
    cell_size = NULL,
    
    
    # soil_quick_flow_capacity = NULL,
    # plant_available_water = NULL,
    # inaccessible_water = NULL,
    # 
    
    
    slope_stars = NULL,
    slope_factor_stars = NULL,
    
    
    # subc_lc_trace_df = NULL,
    # subc_trace_df = NULL,
    calibration_vars = NULL,
    
    output_df = NULL
  )
  
  
  lc_df_col <- c("lc_id", "color", "label", "description")
  rain_df_col <- c("date", "rain")
  
  lc_legend_pal = NULL
  
  ws_id <- function(i) {
    return(paste0("ws_", i))
  }
  
  out_id <- function(i) {
    return(paste0("ou_", i))
  }
  
  get_numeric_id <- function(id) {
    return(as.numeric(substr(id, 4, 10)))
  }
  
  check_map_input <- function(map_id) {
    T
  }
  inp_lc_file <- "lc_file_1"
  out_lc_map <- "lc_map_1_out"
  lc_map_id <- "lc_1"
  
  observeEvent(input$cancel_button_dialog, {
    removeModal()
    is_spinner(F)
  })
  
  ### INPUT PARAMETES AND DATA
  
  io_file_df <- data.frame(
    var = c(
      "genriver_cfg",
      "lc_map_stars_list",
      "lc_df",
      "lc_map_df",
      "lc_map_crop_stars_list",
      
      "lc_par_df",
      "lc_evapot_df",
      "evapot_month_data_df",
      
      "ground_par_df",
      
      "map_boundary_sf",
      "ws_boundary_outlet_cfg",
      "watershed_df",
      "ws_boundary_sf",
      "ws_boundary_stars",
      
      "subcatchment_map_sf",
      
      
      "dem_direction_terra",
      "dem_flow_stars",
      # "dem_contour_sf",
      "dem_stream_sf",
      "dem_map_stars",
      "dem_crop_stars",
      "routing_map_stars",
      "routing_river_sf",
      "soil_map_sf",
      "soil_map_stars",
      "soil_layer_df",
      "soil_metadata_df",
      "soil_thetasat_stars",
      "soil_thetasat_sf",
      
      "soil_quick_flow_capacity_stars_list",
      "soil_plant_available_water_stars_list",
      "soil_inaccessible_water_stars_list",
      
      "subc_soil_water_df",
      "subc_lc_df",
      
      "evapotran_df",
      "rain_df",
      "river_df",
      "rain_par_cfg",
      "river_par_cfg",
      "infiltration_par_cfg",
      "groundwater_par_cfg",
      "waterplant_par_cfg",
      "interception_par_cfg",
      "soilplant_par_cfg",
      "stream_par_cfg",
      
      "lake_df",
      "dam_df"
    ),
    file = c(
      "genriver",
      "landcovermap",
      "landcover",
      "map_list",
      "lcmapcrop",
      
      "lc_props",
      "lc_evapot",
      "evapot_monthly",
      
      "ground_par",
      
      "map_boundary",
      "ws_boundary_outlet",
      "watershed",
      "ws_boundary",
      "ws_boundary_raster",
      
      "subcatchment",
      
      "dem_direction",
      "dem_flow",
      # "contour",
      "stream",
      "dem_bb",
      "dem_ws",
      "routing_map",
      "routing_stream",
      "soil_map",
      "soil_map",
      "soil_layer",
      "soil_metadata",
      "soil_thetasat",
      "soil_thetasat",
      
      "soil_quick_flow_capacity",
      "soil_plant_available_water",
      "soil_inaccessible_water",
      
      "subcatchment_soil_water",
      "subcatchment_lc",
      
      "evapotranspiration",
      "rain",
      "river",
      "rain_par",
      "river_par",
      "infiltration_par",
      "groundwater_par",
      "waterplant_par",
      "interception_par",
      "soilplant_par",
      "stream_par",
      
      "lake",
      "dam"
    )
  )
  
  numeric_par <- c(
    "rain_par_cfg",
    "river_par_cfg",
    "infiltration_par_cfg",
    "groundwater_par_cfg",
    "waterplant_par_cfg",
    "interception_par_cfg",
    "soilplant_par_cfg"
  )
  
  
  
  update_numeric_par <- function() {
    for(par in numeric_par) {
      id <- gsub(suffix(par), "input", par)
      update_numeric_input_ui(id, v[[par]])
    }
    
    for(var in names(v$stream_par_cfg)) {
      updateNumericInput(inputId = var, value = v$stream_par_cfg[[var]])
    }
  }
  
  
  ### LAND COVER INPUT ############################################
  
  update_lc_legend <- function(lc_df, lc_map) {
    id <- unique(as.vector(lc_map[[1]]))
    id <- sort(id[!is.na(id) & id >= 0])
    if (is.null(lc_df) || nrow(lc_df) == 0) {
      lc_df <- data.frame("lc_id" = id)
      lc_df$color <- map_color(length(id))
      lc_df$land_cover <- paste0("Landcover_", id)
      # lc_df$land_use <- paste0("Landuse_", id)
      lc_df$description <- ""
    } else {
      dif <- as.numeric(setdiff(id, lc_df$lc_id))
      if (length(dif) > 0) {
        a_df <- data.frame(
          lc_id = dif,
          color = chart_color[sample.int(length(chart_color), length(dif))],
          land_cover = paste0("Landcover_", dif),
          # land_use = paste0("Landuse_", dif),
          description = ""
        )
        lc_df <- rbind(lc_df, a_df)
        lc_df <- lc_df[order(lc_df$lc_id), ]
      }
    }
    return(lc_df)
  }
  
  #### Land Cover Map File  #########################
  
  map_id_counter <- 0
  get_next_map_id <- function() {
    map_id_counter <<- map_id_counter + 1
    return(map_id_counter)
  }
  
  observeEvent(input$rfalow_lc_map_inp, {
    dpaths <- input$rfalow_lc_map_inp$datapath
    fnames <- input$rfalow_lc_map_inp$name
    lc_df <- isolate(v$lc_df)
    for (i in 1:length(dpaths)) {
      dpath <- dpaths[i]
      fname <- fnames[i]
      is_zip <- F
      file_list <- NULL
      try(file_list <- utils::unzip(dpath, list = TRUE), silent = T)
      
      if (is.null(file_list)) {
        file_list <- list("Name" = dpath)
      } else {
        file_list$Name <- paste0(data_dir, "/", file_list$Name)
        utils::unzip(dpath, exdir = data_dir, junkpaths = T)
        is_zip <- T
      }
      mlist <- list()
      mdf <- NULL
      for (f in file_list$Name) {
        m <- NULL
        prevm <- isolate(v$lc_map_stars_list)
        ex <- suffix(f, sep = ".")
        if(ex == "tif") {
          try(m <- read_stars(f, proxy = T), silent = T)
        } 
        # else {
        #   m <- st_read(f)
        # }
        if (!is.null(m) && any(class(m) == "stars")) {
          if (is_zip) {
            ferr <- suffix(f, sep = "/")
          } else {
            ferr <- fname
          }
          mst <- st_as_stars(m)
          mid <- unique(as.vector(mst[[1]]))
          if (length(mid[mid < 0]) > 0) {
            mst[mst < 0] <- NA
            m <- mst
          }
          ps <- m |> st_bbox() |> st_as_sfc() |> st_transform(crs = "+proj=longlat +datum=WGS84") |> st_as_sf()
          bb <- isolate(v$map_boundary_sf)
          if (is.null(bb)) {
            v$map_boundary_sf <- ps
          } else {
            suppressMessages({
            int <- st_intersects(bb, ps, sparse = F)
            
            if (!all(int)) {
              showNotification(paste(
                "The map file",
                ferr,
                "is outside the previous map boundary"
              ),
              type = "warning")
              next
            }

            v$map_boundary_sf <- st_union(bb, ps) |> st_bbox() |> st_as_sfc()
            })
          }
          
          idx <- suffix(prefix(f, "."), "-")
          suppressWarnings(
          n <- as.numeric(idx)
          )
          if (is.na(n))
            n <- get_next_map_id()
          id <- paste0("lc_", n)
          while (id %in% names(prevm) || id %in% names(mlist)) {
            id <- paste0("lc_", get_next_map_id())
          }
          mlist[[id]] <- m
          
          if (is.null(mdf)) {
            mdf <- data.frame(
              map_id = c(id),
              year = c(as.numeric(suffix(id))),
              filename = ferr
            )
          } else {
            mdf <- rbind(mdf, c(id, as.numeric(suffix(id)), ferr))
          }
          lc_df <- update_lc_legend(lc_df , mst)
        }
      }
      if (length(mlist) > 0) {
        if (is.null(prevm)) {
          v$lc_map_stars_list <- mlist
          v$lc_map_df <- mdf
        } else {
          v$lc_map_stars_list <- append(prevm, mlist)
          v$lc_map_df <- rbind(isolate(v$lc_map_df), mdf)
        }
      } else {
        showNotification("File error or wrong file format", type = "error")
      }
      
    }
    v$lc_df <- lc_df
  })
  
  #### Land Cover Map Plot #########################
  
  observe({
    ms <- v$lc_map_stars_list
    if (is.null(ms))
      return()

    #table
    lc_df_edited <- update_lc_df_display()
    
    map_df <- isolate(v$lc_map_df)
    name_ms <- names(ms)
    output$lc_map_out <- renderUI({
      layout_column_wrap(width = 1, !!!lapply(name_ms, function(x) {
        card(
          card_header(
            div(class = "inline", numericInput(
              paste0("lc_map_year_", x), "Year:", map_df[map_df$map_id == x, "year"], min = 0
            )),
            tooltip_blue(
              icon("info-circle", style = "font-size:1.7rem;"),
              paste("File name:", map_df[map_df$map_id == x, "filename"])
            ),
            actionButton(
              paste0("delete_map_btn_", x),
              "",
              icon = icon("trash-can"),
              class = "toolbar_button"
            ) |> tooltip_blue("Remove the map"),
            
            class = "bg_light2 bg_theme d-flex justify-content-between"
          ),
          card_body(padding = 10, plotOutput(paste0(
            "lc_map_", x
          )))
        )
      }))
    })
    
    lapply(name_ms, function(x, ms) {
      output[[paste0("lc_map_", x)]] <- renderPlot({
        df <- v$lc_df
        plot(
          ms[[x]],
          col = df$color,
          breaks = c(-1, df$lc_id),
          key.pos = NULL,
          main = NULL
        )
      }, height = 400)
    }, ms)
    
    
    # edit year
    lapply(name_ms, function(x) {
      observeEvent(input[[paste0("lc_map_year_", x)]], {
        yr <- input[[paste0("lc_map_year_", x)]]
        df <- isolate(v$lc_map_df)
        df[df$map_id == x, "year"] <- yr
        v$lc_map_df <- df
      })
    })
    # delete map
    lapply(name_ms, function(x) {
      observeEvent(input[[paste0("delete_map_btn_", x)]], {
        df <- isolate(v$lc_map_df)
        v$lc_map_df <- df[df$map_id != x, ]
        v$lc_map_stars_list[[x]] <- NULL
        print(paste("delete", x))
      })
    })
    
  })
  
  landuse_list <- c("Forest", "Tree-based system", "Agriculture", "Settlement")
  
  update_lc_df_display <- function() {
    table_edit_server("lc_df_table",
                      isolate(v$lc_df),
                      col_type = c("numeric", "color", "text", "text"))
  }
  
  lc_df_edited <- update_lc_df_display()
  
  observe({
    v$lc_df <- lc_df_edited()
  })
  
  ### LC PROPERTIES ###############################
  
  lc_prop_cols <- data.frame(
    var = c("I_InterceptClass", "I_RelDroughtFact", "I_BD_BDRefVegNow"),
    label = c(
      "Potential Interception (mm day-1)",
      "Relative Drought Threshold",
      "BD/BDref"
    )
  )
  
  
  lc_par_df_display <- function() {
    pdf <- isolate(v$lc_par_df)
    adf <- isolate(v$lc_df)[c("lc_id", "land_cover")]
    pdf <- merge(adf, pdf, by = "lc_id", all.x = T)
    
    table_edit_server(
      "lc_props_table",
      # isolate(v$lc_par_df),
      pdf,
      col_title = c("lc_id", "Land Cover", lc_prop_cols$label),
      col_type = c("numeric", "character", rep("numeric", 3)),
      col_width = c(50, 150, 250, 250, 100),
      col_disable = c(T, T, F, F, F)
    )
  }
  
  lc_par_df_edited <- lc_par_df_display()
  
  observe({
    v$lc_par_df <- lc_par_df_edited()
  })
  
  lc_evapot_df_display <- function() {
    table_edit_server(
      "lc_evapot_table",
      isolate(vd$lc_evapot_disp_df),
      col_type = c("numeric", "character", rep("numeric", 12)),
      col_disable = c(T, T, rep(F, 12))
    )
  }
  
  lc_evapot_df_edited <- lc_evapot_df_display()
  
  observe({
    vd$lc_evapot_disp_df <- lc_evapot_df_edited()
  })
  
  
  observe({
    # if(is.null(v$lc_df)) return()
    # if(is.null(v$lc_par_df)) return()
    # v$lc_df$lc_id <- v$lc_df$LC_ID
    # v$lc_df$LC_ID <- NULL
    # v$lc_par_df$lc_id <- v$lc_par_df$LC_ID
    # v$lc_par_df$LC_ID <- NULL
    
    
    df <- v$lc_df
    if (nrow(df) == 0)
      return()
    
    pdf <- isolate(v$lc_par_df)
    if (is.null(pdf) || nrow(pdf) == 0) {
      pdf <- df[c("lc_id")]
      pdf[lc_prop_cols$var] <- NA
    } else {
      adf <- df[c("lc_id")]
      pdf <- merge(adf, pdf[c("lc_id", lc_prop_cols$var)], by = "lc_id", all.x = T)
    }
    v$lc_par_df <- pdf
    lc_par_df_edited <- lc_par_df_display()
    
    edf <- isolate(vd$lc_evapot_disp_df)
    if (nrow(edf) == 0) {
      edf <- df[c("lc_id", "land_cover")]
      edf[month_cols$var] <- NA
    } else {
      adf <- df[c("lc_id", "land_cover")]
      edf <- merge(adf, edf[c("lc_id", month_cols$var)], by = "lc_id", all.x = T)
    }
    vd$lc_evapot_disp_df <- edf
    lc_evapot_df_edited <- lc_evapot_df_display()
  })
  
  observe({
    edf <- vd$lc_evapot_disp_df
    if (nrow(edf) == 0) {
      v$lc_evapot_df <- NULL
    } else {
      df <- melt(edf[, !(names(edf) %in% c("land_cover"))], id = c("lc_id"))
      df <- merge(df, month_cols, by.x = "variable", by.y = "var")
      v$lc_evapot_df <- df[c("lc_id", "month", "value")]
    }
  })
  
  update_lc_prop <- function() {
    lc_df <- isolate(v$lc_df)[c("lc_id", "land_cover")]
    
    
    df <- isolate(v$lc_evapot_df)
    wide_df <- dcast(df, lc_id ~ month)
    # print(wide_df)
    colnames(wide_df) <- c("lc_id", month_cols$var)
    
    wide_df <- merge(lc_df, wide_df, by = "lc_id", all.x = T)
    vd$lc_evapot_disp_df <- wide_df
  }
  
  #### Evapotranspiration Monthly Data #########################
  
  evapot_month_df_display <- function() {
    table_edit_server(
      "evapot_monthly_df_table",
      isolate(v$evapot_month_data_df),
      col_type = c("numeric", "character", "numeric"),
      col_disable = c(T, T, F)
    )
  }
  
  evapot_month_df_edited <- evapot_month_df_display()
  
  observe({
    v$evapot_month_data_df <- evapot_month_df_edited()
  })
  
  output$evapot_monthly_data_plot <- renderPlotly({
    df <- v$evapot_month_data_df
    if (is.null(df) || nrow(df) == 0)
      return()
    df$month <- factor(df$month, ordered = T, levels = df$month)
    plot_ly(
      df,
      x = ~ month,
      y = ~ evapotranspiration,
      size = I(1),
      type = "bar"
    ) |>
      layout(
        yaxis = list(title = "Monthly Evapotranspiration (mm)"),
        xaxis = list(title = "Months")
      )
  })
  
  ### DEM FILE INPUT #########################
  observeEvent(input$dem_map_inp, {
    v$dem_map_file <- input$dem_map_inp
    v$dem_map_stars <- read_stars(v$dem_map_file$datapath, proxy = T)
    if (is.na(st_crs())) {
      v$dem_map_stars <- st_transform(v$dem_map_stars, crs = 4326)
    }
  })
  
  ### DEM DOWNLOAD ########################################
  
  open_topo_url <- "https://portal.opentopography.org/API/globaldem?"
  api_key_default <- "d4b532fa2c9ef65c2dad6336a851e669"
  
  observeEvent(input$get_dem_open_topo, {
    removeModal()
    bb <- st_bbox(v$map_boundary_sf)
    l <- list(
      demtype = input$demtype_inp,
      outputFormat = "GTiff",
      west = bb[["xmin"]],
      east = bb[["xmax"]],
      south = bb[["ymin"]],
      north = bb[["ymax"]],
      API_Key = api_key_default
    )
    params <- paste0(paste0(names(l), "=", l), collapse = "&")
    get_topo_url <- paste0(open_topo_url, params)
    # TODO: temporally commented
    showPageSpinner(caption = "Please wait while downloading DEM map")
    res <- GET(get_topo_url)
    print(res$all_headers)
    # f <- paste0("www/", "dem", ceiling(runif(1, 1, 99999999)), ".tif")
    f <- tempfile("dem")
    writeBin(res$content, f)
    
    # f <- "dem_sbj.tif"
    try(m <- read_stars(f, proxy = T))
    if (!is.null(m)) {
      # write_stars(m, "dem_kalsel.tif")
      v$dem_map_stars <- m
      m <- st_as_stars(m)
      val <- sort(unique(as.vector(m[[1]]))) 
      pal <- colorNumeric(grad_color(10), val, na.color = "transparent")
      leafletProxy(active_watershed_lf, session) |>
        clearShapes() |>
        addFeatures(
          v$map_boundary_sf,
          layerId = 'dem_map',
          color = color_theme$secondary,
          label = "Click here to generate the stream path",
          labelOptions = labelOptions(className = "map_label", offset = c(0, -5)),
          highlightOptions = highlightOptions(weight = 5, color = color_theme$danger),
          fillOpacity = 0,
          opacity = 1,
          weight = 1
        ) |>
        # addStarsImage(m, colors = pal, opacity = 0.8) |>
        addGeoRaster(
          m,
          colorOptions = colorOptions(palette = grad_color(200)),
          opacity = 0.6,
          layerId = 'dem_map'
        ) |>
        addLegend(pal = pal,
                  values = val,
                  title = "Elevation")
    }
    is_set_ws_boundary <<- T
    # file.remove(f)
    hidePageSpinner()
    is_spinner(F)
  })
  
  observe({
    m <- v$dem_map_stars
    if(is.null(m)) return()
    vd$cell_size <- cellSize.stars(m, unit = "ha") 
  })
  
  ### WATERSHED BOUNDARY#########################
  
  selected_ws_outlet_id <- NULL
  is_set_ws_boundary <- F
  
  observeEvent(input$set_watershed_btn, {
    removeModal()
    if (is.null(selected_ws_outlet_id))
      return()
    dem_map <- v$dem_map_stars
    ws <- v$outlet_map_list[[selected_ws_outlet_id]]
    v$ws_boundary_sf <- ws$map
    # dim <- st_dimensions(dem_map)
    # v$ws_boundary_stars <- st_rasterize(v$ws_boundary_sf,
    #                                     dx = dim$x$delta,
    #                                     dy = dim$y$delta)
    v$ws_boundary_stars <- st_rasterize(v$ws_boundary_sf, dem_map, align = T)
    v$ws_boundary_outlet_cfg <- list(lon = ws$lon,
                                     lat = ws$lat,
                                     area_m2 = st_area(ws$map))
    v$watershed_df <- NULL
    v$watershed_map_list <- NULL
    v$outlet_map_list <- NULL
    #crop stars
    v$dem_crop_stars <- crop_raster(v$dem_map_stars, v$ws_boundary_stars)
    # v$dem_contour_sf <- generate_contour_sf(v$dem_crop_stars)
    v$dem_flow_stars <- crop_raster(v$dem_flow_bb_stars, v$ws_boundary_stars)
    t <- v$genriver_cfg$flow_threshold
    v$dem_stream_sf <- generate_stream(v$dem_flow_stars, t)
    #crop land cover map
    crop_land_cover_map()
    #query soil properties
    generate_soil_map()
    #generate routing distance
    # withProgress(message = 'Generate stream routing distance', value = 0, {
    #   print("generate_routing_dist_map")
    #   # rm <- generate_routing_dist_map(v$dem_flow_stars, t, progress = setProgress)
    #   rm <- generate_river_dist(v$dem_flow_stars, t)
    # })
    
    print("generate routing distance")
    rd_map <- generate_routing_distance(v$dem_flow_stars, t)
    strm_sf <- generate_rounded_polygon(rd_map["routing"]/1000, 1)
    v$routing_river_sf <- strm_sf
    v$routing_map_stars <- rd_map
    
    print("generate subcathments")
    withProgress(message = 'Generate subcathments', value = 0, {
      subc_map <- generate_subcathments(isolate(v$dem_direction_terra),
                                       rd_map,
                                       order = 2, progress = setProgress)
    })
    subc_map$rel_area <- subc_map$area_m2/v$ws_boundary_outlet_cfg$area_m2
    subc_map <- subc_map[order(subc_map$distance),]
    subc_map$color <-  grad_color(nrow(subc_map))
    
    v$subc_lc_df <- NULL
    v$subcatchment_map_sf <- subc_map
    plot_subcathment(leafletProxy(active_watershed_lf, session))
    
    initialize_ground_par_df() 
    
    ws_id_last <<- 0
    # reset_watershed_panel_display(leafletProxy(active_watershed_lf, session))
    is_set_ws_boundary <<- F
  })
  
 
  
  
  #### Calculate land cover area by subcathment ####################
  
  # observe({
  #   subc <- v$subcatchment_map_sf
  #   if (is.null(subc))
  #     return()
  #   if(!is.null(v$subc_lc_df))
  #     return()
  #   print("Calculate land cover area by subcathment")
  #   lc_map_list <- v$lc_map_crop_stars_list
  #   dim <- st_dimensions(lc_map_list[[1]])
  #   map_ids <- names(lc_map_list)
  #   ws_id_list <- unique(subc$ws_id)
  #   df <- NULL
  #   pinc <- 1 / length(ws_id_list)
  #   withProgress(message = 'Calculating land cover area by subcatchment', value = 0, {
  #     suppressMessages(for (ws_id in ws_id_list) {
  #       incProgress(pinc)
  #       sc_sf <- subc[subc$ws_id == ws_id,]
  #       ssf <- sc_sf["ws_id"]
  #       # ssf_m <- st_rasterize(ssf, dx = dim$x$delta, dy = dim$y$delta)
  #       ssf_m <- st_rasterize(ssf, lc_map_list[[1]])
  #       for (map_id in map_ids) {
  #         lc_sub_map <- crop_raster(lc_map_list[[map_id]], ssf_m)
  #         lc_df <- as.data.frame(table(lc_sub_map))
  #         names(lc_df) <- c("lc_id", "area")
  #         lc_df$area_ratio <- lc_df$area/sum(lc_df$area)
  #         lc_df$area <- lc_df$area_ratio * sc_sf$area 
  #         lc_df$map_id <- map_id
  #         lc_df$ws_id <- ws_id
  #         if (is.null(df)) {
  #           df = lc_df
  #         } else {
  #           df <- rbind(df, lc_df)
  #         }
  #       }
  #     })
  #   })
  #   v$subc_lc_df <- df
  # })
  observe({
    subc <- v$subcatchment_map_sf
    if (is.null(subc))
      return()
    if(is.null(v$subc_lc_df))
      v$subc_lc_df <- calculate_subcatchment_land_cover_area(v$subcatchment_map_sf, v$lc_map_crop_stars_list)
    
    if(is.null(v$ground_par_df) || nrow(v$ground_par_df) == 0)
      initialize_ground_par_df() 
  })
  
  calculate_subcatchment_land_cover_area <- function(subc, lc_map_list){
    print("Calculate land cover area by subcathment")
    lc_ids <- v$lc_df$lc_id
    # ws_area <- as.numeric(v$ws_boundary_outlet_cfg$area_m2)/10000
    ws_area_m2 <- as.numeric(v$ws_boundary_outlet_cfg$area_m2)
    lc_m <- st_as_stars(lc_map_list[[1]])
    subc_stars <- st_rasterize(subc["ws_id"], lc_m, align = T)
    subc_stars <- crop_raster(subc_stars, lc_m)
    map_ids <- names(lc_map_list)
    subc_df <- as.data.frame(subc)
    #TODO: this should be aggregated if it has multiple outlets
    subc_df$area_m2 <- as.numeric(subc_df$area_m2)
    subc_df$I_RelArea <- subc_df$area_m2/ws_area_m2
    subc_df$subc_area <- subc_df$area_m2/10000
    subc_df <- subc_df[c("ws_id", "subc_area", "I_RelArea")]
    
    subcell_df <- as.data.frame(table(subc_stars))
    names(subcell_df) <- c("ws_id", "ncell_sub")
    subcell_df <- merge(subcell_df, subc_df[c("ws_id", "subc_area", "I_RelArea")], by = "ws_id")
    # ws_id_list <- unique(subc_df$ws_id)
    # df <- NULL
    # pinc <- 1 / length(ws_id_list)
    pinc <- 1 / length(map_ids)
    all_df <- NULL
    withProgress(message = 'Calculating land cover area by subcatchment', value = 0, {
      # suppressMessages(
      #   for (ws_id in ws_id_list) {
      #   incProgress(pinc)
      #   subc_r <- subc_df[subc_df$ws_id == ws_id,]
      #   area <- as.numeric(subc_r$area_m2)/10000 # area in ha
      #   
      #   I_RelArea <-  area/ws_area
      #   for (map_id in map_ids) {
      #     # lc_sub_map <- crop_raster(lc_map_list[[map_id]], ssf_m)
      #     lc_m <- st_as_stars(lc_map_list[[map_id]])
      #     lc_sub_map <- lc_m[subc_stars == ws_id]
      #     lc_df <- as.data.frame(table(lc_sub_map))
      #     names(lc_df) <- c("lc_id", "area")
      #     lc_df$area_ratio <- lc_df$area/sum(lc_df$area)
      #     lc_df$area <- lc_df$area_ratio * area 
      #     lc_df$subc_area <- area
      #     lc_df$I_RelArea <- I_RelArea
      #     lc_df$map_id <- map_id
      #     lc_df$ws_id <- ws_id
      #     if (is.null(df)) {
      #       df = lc_df
      #     } else {
      #       df <- rbind(df, lc_df)
      #     }
      #   }
      # })
      
      
      for (map_id in map_ids) {
        incProgress(pinc)
        lc_m <- st_as_stars(lc_map_list[[map_id]])
        subc_lc_m <- c(subc_stars, lc_m)
        names(subc_lc_m) <- c("ws_id", "lc_id") 
        df <- as.data.frame(table(subc_lc_m))
        names(df) <- c("ws_id", "lc_id", "ncell")
        df$ws_id <- as.integer(levels(df$ws_id))[df$ws_id]
        df$lc_id <- as.integer(levels(df$lc_id))[df$lc_id]

        dif <- setdiff(lc_ids, unique(df$lc_id))
        dif_df <- data.frame(
          ws_id = rep(subc_df$ws_id, length(dif)),
          lc_id = rep(dif, each = length(subc_df$ws_id)),
          ncell = 0
        )
        df <- rbind(df, dif_df)
        
        agg_df <- aggregate(df["ncell"], by = df["ws_id"], sum, na.rm = T)
        names(agg_df) <- c("ws_id", "ncell_sub")
        # df <- df[df$ncell > 0,]
        # subcell_df <- as.data.frame(table(subc_stars))
        # names(subcell_df) <- c("ws_id", "ncell_sub")
        # subcell_df <- merge(subcell_df, subc_df[c("ws_id", "subc_area", "I_RelArea")], by = "ws_id")
        # df <- merge(df, subcell_df, by = "ws_id")
        df <- merge(df, agg_df, by = "ws_id")
        df <- merge(df, subc_df[c("ws_id", "subc_area")], by = "ws_id")
        df$area_ratio <- df$ncell / df$ncell_sub
        df$area <- df$area_ratio * df$subc_area
        df$map_id <- map_id
        if(is.null(all_df)) {
          all_df <- df
        } else {
          all_df <- rbind(all_df, df)
        }
      }
      
    })
    return(all_df)
  }
  
  
  #### Plot Subcatchment ####################
  
  plot_subcathment <- function(lf) {
    sc <- v$subcatchment_map_sf
    if (is.null(sc))
      return(lf)
    label <- map_label(paste(
      "ID:",
      sc$ws_id,
      "<br>Area size:",
      # f_number(as.numeric(sc$area) / 10000, nsmall = 0),
      sprintf("%0.1f", sc$area/10000),
      "ha<br>Routing distance:",
      sprintf("%0.1f", sc$distance/1000),
      "km"
    ),
    "Sub-Catchment")
    
    lf |>
      clearShapes() |>
      clearMarkers() |>
      clearControls() |>
      fit_map_view(sc) |>
      plot_watershed_boundary() |>
      show_stream(v$dem_stream_sf, opacity = 1) |>
      addFeatures(
        sc,
        group = "subcatchment",
        color = "#000",
        fillColor = ~ color,
        fillOpacity = 0.6,
        opacity = 0.8,
        weight = 1,
        label = label,
        labelOptions = labelOptions(
          className = "map_label",
          offset = c(0, -10),
          direction = "top"
        ),
        highlightOptions = highlightOptions(fillOpacity = 0.8, fillColor = "#fb8500")
      )
  }
  
  plot_watershed_boundary <- function(lf, color = color_theme$secondary) {
    if (is.null(v$ws_boundary_sf)) return(lf) 
    lf |> addFeatures(
        v$ws_boundary_sf,
        layerId = 'ws_boundary',
        color = color,
        label = "Watershed area",
        labelOptions = labelOptions(className = "map_label", offset = c(0, -5)),
        # highlightOptions = highlightOptions(fillOpacity = 0.8),
        fillOpacity = 0.4,
        weight = 0
        # opacity = 0.4
      ) 
    
  }
  

  
  crop_land_cover_map <- function() {
    print("crop_land_cover_map")
    lc_ms <- isolate(v$lc_map_stars_list)
    if(is.null(lc_ms)) return()
    wsb <- isolate(v$ws_boundary_stars)
    lc_crop_ms <- list() 
    withProgress(message = 'Cropping land cover maps', value = 0, {
      ids <- names(lc_ms)
      p <- 1/(length(ids) + 1)
      for(m_id in ids) {
        incProgress(p)
        cm <- crop_raster(lc_ms[[m_id]], wsb)
        # f <- paste0(tempfile(m_id), ".tif")
        # write_stars(cm, f)
        # lc_crop_ms[[m_id]] <- read_stars(f, proxy = T)
        lc_crop_ms[[m_id]] <- stars_to_proxy(cm, m_id)
      }
    })
    v$lc_map_crop_stars_list <- lc_crop_ms
  }
  
  observe({
    mlist <- v$lc_map_crop_stars_list
    if(is.null(mlist) || is.null(vd$cell_size)) return()
    ids <- names(mlist)
    dflist <- list()
    for(id in ids) {
      m <- st_as_stars(mlist[[id]])
      df <- as.data.frame(table(m))
      colnames(df) <- c("lc_id", "area")
      df$area <- df$area * vd$cell_size
      dflist[[id]] <- df
    }
    vd$lc_df_crop_list <- dflist
  })
  

  
  # lcm <- read_stars("Lc1973cr.tif")
  # m <- st_transform(lcm, crs = st_crs(raster_boundary))
  # 
  # m2 <- crop_raster(m, raster_boundary)
  # plot(m2)
  # plot(st_crop())
  # # boundary_sf <- read_sf("b.shp")
  # # raster_map <- read_stars("ras.tif")
  # raster_boundary <- read_stars("ws_b.tif")
  # raster_map <- read_stars("dem_sbj.tif")
  
  # m <- read_stars("dem_crop.tif")
  # 
  # m <- crop_raster(raster_map, raster_boundary)
  # 
  ### GENERATE STREAM MAP #########################
  
  is_update_dem <- reactiveVal(T)
  
  observeEvent(input$process_dem, {
    toggle_popover("stream_pop1", F)
    toggle_popover("stream_pop2", F)
    removeModal()
    generate_dem_flow()
  })
  
  generate_dem_flow <- function() {
    if (is.null(v$dem_map_stars))
      return()
    withProgress(message = 'Processing DEM', value = 0, {
      if (is_update_dem()) {
        is_update_dem(F)
        v$stream_render <- NULL
        setProgress(0.1, detail = "Generating contour")
        # v$dem_contour_bb_sf <- generate_contour_sf(v$dem_map_stars)
        dem_out <- flowdem(v$dem_map_stars)
        v$dem_direction_terra <- dem_out$dem_direction_terra
        v$dem_flow_bb_stars <- dem_out$dem_flow_stars
      }
      setProgress(0.9, detail = "Generating stream path")
      t <- isolate(input$dem_flow_threshold)
      v$genriver_cfg$flow_threshold <- t
      print("DEM -> stream")
      v$dem_stream_bb_sf <- generate_stream(isolate(v$dem_flow_bb_stars), t)
      # write_sf(v$dem_stream_bb_sf, "stream.shp")
      setProgress(1, detail = "Done!")
      print("DEM -> done!")
    })
    
    leafletProxy(active_watershed_lf, session) |>
      clearShapes() |>
      clearImages() |>
      clearControls() |>
      # show_contour(v$dem_contour_bb_sf) |>
      show_stream(v$dem_stream_bb_sf)
    
  }
  
  # generate_contour_sf <- function(dem, n_legend = 5) {
  #   dem <- st_as_stars(dem)
  #   dem_min <- min(dem[[1]], na.rm = T)
  #   dem_max <- max(dem[[1]], na.rm = T)
  #   dem_n <- n_legend
  #   dem_d <- (dem_max - dem_min) / dem_n
  #   mdem <- (round((dem - dem_min) / dem_d) * dem_d) + dem_min
  #   pdem <- st_as_sf(mdem,
  #                    as_points = F,
  #                    merge = T,
  #                    connect8 = T)
  #   ps <- st_transform(pdem, crs = 4326)
  #   ps <- st_sf(geometry = ps)
  #   return(ps)
  # }
  
  #### FLOWDEM process ###########################################
  # https://github.com/KennethTM/flowdem
  flowdem <- function(m) {
    n <- 6
    dem <- rast(as(m, "Raster"))
    # Breach DEM, that is, resolve depression by "carving" through obstacles
    setProgress(1 / n, detail = "DEM breaching")
    print("DEM -> breach")
    dem_breach <- breach(dem)
    # Use fill with epsilon on breached DEM to resolve flats and ensure drainage
    setProgress(2 / n, detail = "DEM filling")
    print("DEM -> fill")
    dem_breach_fill_eps <- fill(dem_breach, epsilon = TRUE)
    # Get flow directions using the filled DEM
    setProgress(3 / n, detail = "Generating flow direction")
    print("DEM -> flow direction")
    dem_dir <- dirs(dem_breach_fill_eps, mode = "d8")
    # Get flow accumulation
    setProgress(4 / n, detail = "Calculating flow accumulation")
    print("DEM -> flow accumulation")
    dem_acc <- accum(dem_dir, mode = "d8")
    f <-  tempfile("map", fileext = ".tif")
    writeRaster(dem_acc, f)
    dem_flow <- read_stars(f, proxy = T)
    return(list(
      dem_direction_terra = dem_dir,
      dem_flow_stars = dem_flow
    ))
  }
  
  ### WATERSHED LEAFLET ########################################
  
  is_watershed_leaflet_base <- F
  base_tiles <- "Esri.WorldTopoMap"
  active_watershed_lf <- "watershed_map_leaflet"
  
  base_leaflet <- function() {
    leaflet(options = leafletOptions(attributionControl = FALSE)) |>
      addProviderTiles(base_tiles)
  }
  
  output$watershed_map_leaflet <- renderLeaflet({
    lf <- base_leaflet()
    is_watershed_leaflet_base <<- T
    
    # if (isolate(is_repaint_watershed())) {
    #   lf <- reset_watershed_panel_display(lf)
    #   is_repaint_watershed(F)
    # }
    
    if (!is.null(v$map_boundary_sf) && is.null(v$dem_stream_sf)) {
      lf <- lf |> addFeatures(
        v$map_boundary_sf,
        layerId = 'dem_boundary',
        color = color_theme$secondary,
        label = "Click here to add DEM map",
        labelOptions = labelOptions(className = "map_label", offset = c(0, -5)),
        highlightOptions = highlightOptions(fillOpacity = 0.8),
        fillOpacity = 0.3,
        opacity = 0.8
      ) |>
        addHomeButton(
          ext = st_bbox(v$map_boundary_sf),
          group = "Watershed Area",
          position = "topleft"
        )
    } else if(!is.null(v$subcatchment_map_sf)) {
      lf <- plot_subcathment(lf)
    }
    return(lf)
  })
  
  reset_watershed_panel_display <- function(lf) {
    if (is.null(v$map_boundary_sf))
      return(lf)
    print("reset watershed panel")
    lf <- lf |>
      clearShapes() |>
      clearMarkers() |>
      clearControls() |>
      # show_contour(v$dem_contour_sf) |>
      show_contour(v$dem_crop_stars) |>
      show_stream(v$dem_stream_sf) |>
      repaint_all_watershed_map() |>
      repaint_outlet() |>
      fit_map_view(v$map_boundary_sf) |>
      addHomeButton(
        ext = st_bbox(v$map_boundary_sf),
        group = "Watershed Area",
        position = "topleft"
      )
    return(lf)
  }
  
  ### WATERSHED map panel click #########################
  
  observeEvent(input$watershed_map_leaflet_shape_click, {
    click_inp <- input$watershed_map_leaflet_shape_click
    if (!is.null(click_inp$id)) {
      if (click_inp$id == "dem_boundary") {
        is_spinner(T)
        inplist <- as.list(opentopo_dataset_df$var)
        names(inplist) <- opentopo_dataset_df$label
        showModal(modalDialog(
          title = "Add DEM Map",
          selectInput(
            "demtype_inp",
            "Choose the dataset:",
            inplist,
            selected = "SRTMGL1",
            width = "100%"
          ),
          footer = tagList(
            actionButton("cancel_button_dialog", "Cancel"),
            actionButton("get_dem_open_topo", "Get DEM")
          )
        ))
      } else if (click_inp$id == "dem_map") {
        input_dialog(
          title = "Generate stream flows",
          confirm_id = "process_dem",
          confirm_label = "Generate",
          input_var = c("dem_flow_threshold"),
          input_label = c(
            "Minimum watershed area coverage (ha)"
          ),
          input_def = c(20),
          input_type = c("numeric")
        )
      }
    }
    if (!is.null(click_inp$group)) {
      if (click_inp$group == "stream") {
        active_watershed_lf <<- "watershed_map_leaflet"
        leafletProxy(active_watershed_lf, session) |>
          add_outlet_dialog(click_inp$lng, click_inp$lat)
      } else if (click_inp$group == "watershed") {
        if (is_set_ws_boundary) {
          df <- isolate(v$watershed_df)
          selected_ws_outlet_id <<- df[df$ws_id == click_inp$id, "out_id"]
          input_dialog(
            "Watershed Boundary",
            "Set this subcathment area as watershed boundary?",
            "set_watershed_btn",
            "Yes"
          )
        }
      }
    }
  })
  
  observeEvent(input$watershed_map_leaflet_marker_click, {
    click_inp <- input$watershed_map_leaflet_marker_click
    if (!is.null(click_inp$group)) {
      if (click_inp$group == "outlet") {
        delete_outlet_dialog(click_inp$id)
      }
    }
  })
  
  
  
  show_contour <- function(lf, m) {
    # if (is.null(contour_f))
    #   return(lf)
    # dem <- contour_f
    # names(dem) <- c("val", "geometry")
    # col_list <- data.frame(val = sort(unique(dem$val)))
    # col_list$color <- terrain.colors(nrow(col_list))
    # dem <- merge(dem, col_list, by = "val")
    # print("* add contour")
    # lf <- lf |>
    #   addFeatures(
    #     dem,
    #     stroke = F,
    #     fillColor = ~ color,
    #     fillOpacity = 0.2,
    #     label = map_label("Watershed area"),
    #     labelOptions = labelOptions(className = "map_label", offset = c(0, -5))
    #   ) |>
    #   fit_map_view(dem)
    
    lf <- lf |> addGeoRaster(
      m,
      colorOptions = colorOptions(palette = grad_color(200)),
      opacity = 0.5,
      layerId = 'dem_crop_map'
    ) |> fit_map_view(m)
    return(lf)
  }
  
  map_label <- function(desc = "",
                        title = "",
                        footer = "") {
    title_div <- ""
    if (title != "")
      title_div <- paste("<strong>",
                         title,
                         "</strong><hr style='margin:2px auto;'>")
    footer_div <- "</div>"
    if (footer != "")
      footer_div <- paste("<hr style='margin:2px auto;'><strong><em>", footer, "</em></strong></div>")
    paste("<div style='font-size:1.2em;'>",
          title_div,
          "<div>",
          desc,
          "</div>",
          footer_div) |>
      lapply(htmltools::HTML)
  }
  
  show_stream <- function(lf,
                          stream_f,
                          is_show_label = T,
                          opacity = 0.8, group = "stream") {
    m <- stream_f
    if (is.null(m)) {
      return(lf)
    }
    names(m) <- c("val", "geometry")
    s_list <- data.frame(val = sort(unique(m$val)))
    nr <- nrow(s_list)
    nleg <- 6 #max line width
    # colors <- stream_color(nleg)
    s_list$w <- ceiling(nleg * c(1:nr) / nr)
    s_list$color <- stream_color(nr)
    m <- merge(m, s_list, by = "val")
    if (is_show_label) {
      labels <- map_label(
        paste("Watershed contributing area: &plusmn;", f_number(m$val, nsmall = 0), "ha"),
        "Stream Path",
        "* Click here to add the outlet"
      )
      
      #stream path
      lf <- lf |> addFeatures(
        m,
        color = ~ color,
        fillOpacity = opacity,
        opacity = opacity,
        weight = ~ w,
        group = group,
        label = ~ labels,
        labelOptions = labelOptions(
          className = "map_label",
          offset = c(0, -10),
          direction = "top"
        ),
        highlightOptions = highlightOptions(color  = "red", fillColor = "red")
      )
    } else {
      lf <- lf |> addFeatures(
        m,
        color = ~ color,
        fillOpacity = opacity,
        opacity = opacity,
        weight = ~ w,
        group = group
      )
    }
    return(lf)
  }
  
  fit_map_view <- function(lf, m) {
    if (is.null(m))
      return(lf)
    bb <- as.list(st_bbox(st_transform(st_as_sfc(st_bbox(
      m
    )), 4326)))
    lf <- lf |> fitBounds(bb$xmin, bb$ymin, bb$xmax, bb$ymax)
    return(lf)
  }
  
  
  
  ### ADD OUTLET AND SUBCATCHMENT AREA #########################
  
  add_outlet_dialog <- function(lf, lon, lat) {
    selected_point[["lon"]] <<- lon
    selected_point[["lat"]] <<- lat
    lf <- lf |>
      addAwesomeMarkers(
        lon,
        lat,
        icon = awesomeIcons(
          "arrow-down",
          library = "fa",
          markerColor = "red",
          spin = T,
          iconColor = "#FFFFFF"
        ),
        layerId = "selected_point"
      )
    showModal(modalDialog(
      title = span(icon("droplet"), "Subcathment outlet"),
      h5("The selected location is:"),
      p(tags$b(
        "Lon:",
        f_number(lon, nsmall = 5),
        "Lat:",
        f_number(lat, nsmall = 5)
      )),
      h4("Add subcatchment outlet here?"),
      footer = tagList(
        actionButton("outlet_cancel", "Cancel"),
        actionButton("outlet_add", "Add", icon = icon("plus"))
      )
    ))
    return(lf)
  }
  

  
  observeEvent(input$outlet_cancel, {
    removeModal()
    leafletProxy(active_watershed_lf, session) |> removeMarker("selected_point")
  })
  
  observeEvent(input$outlet_add, {
    removeModal()
    leafletProxy(active_watershed_lf, session) |> removeMarker("selected_point") |>
      add_outlet(selected_point$lon, selected_point$lat)
  })
  
  outlet_radius <- 10
  selected_point <- list()
  
  observeEvent(input$dem_streams_out_shape_click, {
    o <- input$dem_streams_out_shape_click
    if (is.null(o$group))
      return()
    if (o$group != "stream")
      return()
    leafletProxy(active_watershed_lf, session) |>
      add_outlet_dialog(o$lng, o$lat)
  })
  
  ws_id_last <- 0
  
  add_outlet <- function(lf, lon, lat) {
    id <- ws_id_last + 1
    ws_id_last <<- id
    
    print(paste("Watershed -> outlet", id))
    m <- generate_watershed(isolate(v$dem_direction_terra), lon, lat)
    
    # #create outlet shape
    # pt.df   <- data.frame(pt = 1, x = lon, y = lat)
    # p   <- st_as_sf(pt.df, coords = c("x", "y"))
    # st_crs(p) <- 4326
    # p <- st_transform(p, crs = 7801)
    # circle <-  st_buffer(p, outlet_radius)
    # circle <- st_transform(circle, crs = 4326)
    # print("Watershed -> generate")
    # #generate watershed
    # dem_dir <- isolate(v$dem_direction_terra)
    # ws <- watershed(dem_dir, circle)
    # m <- st_as_stars(ws)
    ps <- st_union(st_as_sf(
      m,
      as_points = F,
      merge = T,
      connect8 = T
    ))
    ps <- st_transform(ps, crs = 4326)
    ps <- st_sf(geometry = ps)
    area_list <- as.numeric(st_area(ps))
    if (length(area_list) == 0 || area_list == 0) {
      showModal(modalDialog(
        title = span(icon("circle-exclamation"), "Outlet point error"),
        tags$li("Make sure the point location is on the stream path"),
        tags$li("Zoom in the map to get accurate position")
      ))
      leafletProxy(active_watershed_lf, session) |> removeMarker("selected_point")
      return()
    }
    #show and list the valid watershed
    ws_id <- ws_id(id)
    out_id <- out_id(id)
    area <- sum(as.vector(area_list)) / 10000 #convert area m2 to ha
    df <- data.frame(ws_id = ws_id)
    df$color <- chart_color[id]
    df$area <- area
    df$out_id <- out_id
    df$out_lon <- lon
    df$out_lat <- lat
    ws_df <- isolate(v$watershed_df)
    if (is.null(ws_df)) {
      ws_df <- df
    } else {
      ws_df <- rbind(ws_df, df)
    }
    v$watershed_df <- ws_df
    # watershed list
    if (is.null(v$watershed_map_list))
      v$watershed_map_list <- list()
    v$watershed_map_list[[ws_id]] <- ps
    # outlet list
    if (is.null(v$outlet_map_list))
      v$outlet_map_list <- list()
    v$outlet_map_list[[out_id]] <- list("lon" = lon,
                                        "lat" = lat,
                                        "map" = ps)
    #paint the watershed
    paint_watershed_map(lf, ws_id)
    paint_outlet(lf, out_id)
    print(paste("Watershed -> done!"))
    return(lf)
  }
  

  
  get_watershed_color <- function(ws_id) {
    df <- isolate(v$watershed_df)
    return(df[df$ws_id == ws_id, "color"][1])
  }
  
  get_watershed_label <- function(ws_id) {
    return(paste("Subcathment", ws_id))
  }
  
  get_outlet_color <- function(out_id) {
    df <- isolate(v$watershed_df)
    return(df[df$out_id == out_id, "color"][1])
  }
  
  get_outlet_label <- function(out_id) {
    return(paste("Outlet", out_id))
  }
  
  paint_watershed_map <- function(lf, ws_id) {
    m <- isolate(v$watershed_map_list[[ws_id]])
    df <- isolate(v$watershed_df)
    a <- sum(df[df$ws_id == ws_id, "area"])
    label <- map_label(paste(
      "ID:",
      ws_id,
      "<br>",
      "Area size:",
      f_number(a, digits = 5), "ha"
    ),
    "Subcatchment Area")
    lf <- lf |> removeShape(ws_id) |>
      addFeatures(
        m,
        group = "watershed",
        color = get_watershed_color(ws_id),
        fillOpacity = 0.4,
        opacity = 0.8,
        weight = 3,
        layerId = ws_id,
        label = label,
        labelOptions = labelOptions(className = "map_label", offset = c(0, -5)),
        highlightOptions = highlightOptions(fillOpacity = 0.8)
      )
    return(lf)
  }
  
  is_repaint_watershed <- reactiveVal(F)
  
  repaint_all_watershed_map <- function(lf) {
    l <- isolate(v$watershed_map_list)
    if (is.null(l))
      return(lf)
    ws_ids <- names(l)
    for (ws_id in ws_ids) {
      lf <- paint_watershed_map(lf, ws_id)
    }
    return(lf)
  }
  
  paint_outlet <- function(lf, out_id) {
    m <- isolate(v$outlet_map_list[[out_id]])
    if (is.null(m))
      return()
    df <- isolate(v$watershed_df)
    a <- sum(df[df$out_id == out_id, "area"])
    label <- map_label(
      paste(
        "ID:",
        out_id,
        "<br>",
        "Subcatchment area:",
        f_number(a, digits = 5), "ha"
      ),
      "Outlet Point",
      "* Click to remove this outlet"
    )
    lf <- lf |> removeMarker(out_id) |>
      addAwesomeMarkers(
        m$lon,
        m$lat,
        label = label,
        labelOptions = labelOptions(className = "map_label", offset = c(0, -5)),
        icon = awesomeIcons("tint", iconColor = get_outlet_color(out_id)),
        layerId = out_id,
        group = "outlet"
      )
    return(lf)
  }
  
  repaint_outlet <- function(lf) {
    l <- isolate(v$outlet_map_list)
    if (is.null(l))
      return(lf)
    ou_ids <- names(l)
    for (ou_id in ou_ids) {
      lf <- paint_outlet(lf, ou_id)
    }
    return(lf)
  }
  
  delete_outlet_dialog <- function(out_id) {
    deleted_outlet_id <<- out_id
    id <- as.numeric(tail(unlist(strsplit(out_id, "_")), 1))
    input_dialog(
      "Remove Outlet",
      paste("Outlet ID:", id, "<br>Remove this outlet?"),
      "delete_outlet_confirm_btn",
      "Remove"
    )
  }
  
  deleted_outlet_id <- NULL
  
  observeEvent(input$delete_outlet_confirm_btn, {
    removeModal()
    delete_outlet(out_id = deleted_outlet_id)
  })
  
  ### WATERSHED TABLE LIST UI #########################
  
  watershed_col <- c("ws_id", "color", "area", "distance")
  outlet_col <- c("out_id", "color", "out_lon", "out_lat", "area")
  ws_df <- NULL
  
  output$watershed_list <- renderReactable({
    # df <- v$watershed_df
    df <- v$subcatchment_map_sf
    if (is.null(df) || nrow(df) == 0)
      return()
    df <- as.data.frame(df)
    # df <- df[order(df$ws_id),]
    df$area <- df$area/10000 # m2 to ha
    df$distance <- df$distance/1000 # m to km 
    ws_df <<- aggregate(df$area, list(df$ws_id, df$color), FUN = sum)
    colnames(ws_df) <<- c("ws_id", "color", "area")
    ws_df2 <- aggregate(df$distance, list(df$ws_id), FUN = mean)
    colnames(ws_df2) <- c("ws_id", "distance")
    ws_df <<- merge(ws_df, ws_df2, by = "ws_id")
    
    # colnames(ws_df) <<- watershed_col
    # ws_df$idx <<- get_numeric_id(ws_df$ws_id)
    # ws_df <<- ws_df[order(ws_df$idx), ]
    reactable(
      ws_df[watershed_col],
      pagination = F,
      compact = TRUE,
      wrap = FALSE,
      rownames = F,
      selection = "multiple",
      onClick = "select",
      theme = reactableTheme(
        backgroundColor = "#FFFFFF00",
        headerStyle = list(backgroundColor = "rgba(240, 240, 240)")
      ),
      columns = list(
        ws_id = colDef(name = "ID", width = 40),
        color = colDef(
          name = "",
          width = 35,
          html = TRUE,
          cell = function(value) {
            div(
              style = paste0(
                "margin:auto;width:20px;height:20px;border-radius:10px 0px 10px 10px;border:2px solid ",
                value,
                ';background-color:',
                substring(value, 1, 7),
                "bf"
              )
            )
          }
        ),
        distance = colDef(
          name = "Distance (km)",
          format = colFormat(digits = 1, separators = T),
          width = 120
        ),
        area = area_coldef
      ),
      details = function(index) {
        outlet_data <- df[df$ws_id == ws_df$ws_id[index], outlet_col]
        outlet_data$idx <- get_numeric_id(outlet_data$out_id)
        outlet_data <- outlet_data[order(outlet_data$idx), ]
        div(
          style = "padding: 1rem",
          reactable(
            outlet_data[outlet_col],
            rownames = F,
            pagination = F,
            compact = TRUE,
            outlined = TRUE,
            columns = list(
              area = area_coldef,
              color = colDef(
                name = "",
                width = 35,
                html = TRUE,
                cell = function(value) {
                  div(
                    style = paste0(
                      "width:12px;height:12px;border-radius:6px;margin:auto;background-color:",
                      value
                    )
                  )
                }
              ),
              out_id = colDef(name = "Outlet", width = 60),
              out_lon = colDef(
                name = "Lon",
                format = colFormat(digits = 5),
                width = 90
              ),
              out_lat = colDef(
                name = "Lat",
                format = colFormat(digits = 5),
                width = 90
              )
            )
          )
        )
      }
    )
  })
  
  area_coldef <- colDef(
    name = "Area (ha)",
    format = colFormat(digits = 1, separators = T),
    width = 100
  )
  
  observeEvent(input$delete_outlet_btn, {
    state <- req(getReactableState("watershed_list"))
    rows <- as.vector(state$selected)
    if (length(rows) == 0)
      return()
    df <- isolate(v$watershed_df)
    for (i in rows) {
      ws_id <- ws_df[i, "ws_id"]
      delete_outlet(ws_id = ws_id)
    }
  })
  
  #### Delete outlet and watershed #######################################
  delete_outlet <- function(out_id = NULL, ws_id = NULL) {
    if (is.null(out_id) && is.null(ws_id))
      return()
    df <- isolate(v$watershed_df)
    if (is.null(ws_id)) {
      ws_id <- df[df$out_id == out_id, "ws_id"]
    }
    sdf <- df[df$ws_id == ws_id, ]
    if (nrow(sdf) == 1) {
      if (is.null(out_id))
        out_id <- df[df$ws_id == ws_id, "out_id"]
      leafletProxy(active_watershed_lf, session) |> removeShape(ws_id) |> removeMarker(out_id)
      v$outlet_map_list[[out_id]] <- NULL
      v$watershed_map_list[[ws_id]] <- NULL
      
      v$watershed_df <- df[df$ws_id != ws_id, ]
    } else {
      if (is.null(out_id)) {
        out_id_list <- df[df$ws_id == ws_id, "out_id"]
        lf <- leafletProxy(active_watershed_lf, session) |> removeShape(ws_id)
        v$watershed_map_list[[ws_id]] <- NULL
        for (id in out_id_list) {
          lf <- lf |> removeMarker(id)
          v$outlet_map_list[[id]] <- NULL
        }
        v$watershed_df <- df[df$ws_id != ws_id, ]
      } else {
        m <- isolate(v$watershed_map_list[[ws_id]])
        o <- isolate(v$outlet_map_list[[out_id]]$map)
        #TODO: it is better to re-union the rest of outlets
        m_dif <- st_difference(st_union(m), st_union(o))
        v$outlet_map_list[[out_id]] <- NULL
        sf_use_s2(FALSE)
        v$watershed_map_list[[ws_id]] <- st_union(m_dif)
        lf <- leafletProxy(active_watershed_lf, session)
        removeMarker(lf, out_id)
        paint_watershed_map(lf, ws_id)
        v$watershed_df <- df[df$out_id != out_id, ]
      }
    }
  }
  
  #### Merge outlet #############################
  observeEvent(input$merge_outlet_btn, {
    sf_use_s2(FALSE)
    state <- req(getReactableState("watershed_list"))
    rows <- as.vector(state$selected)
    df <- isolate(v$watershed_df)
    
    ws_ids <- unique(as.vector(ws_df[rows, "ws_id"]))
    new_id <- ws_ids[1]
    new_color <- get_watershed_color(new_id)
    if (length(ws_ids) <= 1)
      return()
    m1 <- NULL
    for (ws_id in ws_ids) {
      m2 <- isolate(v$watershed_map_list[[ws_id]])
      if (is.null(m1)) {
        m1 <- m2
      } else {
        m1 <- st_union(m1, m2) #st_combine()
      }
      print(paste("remove", ws_id))
      leafletProxy(active_watershed_lf, session) |> removeShape(ws_id)
      v$watershed_map_list[[ws_id]] <- NULL
    }
    v$watershed_map_list[[new_id]] <- m1
    df[df$ws_id %in% ws_ids, "ws_id"] <- new_id
    df[df$ws_id == new_id, "color"] <- get_watershed_color(new_id)
    v$watershed_df <- df
    lf <- leafletProxy(active_watershed_lf, session)
    out_ids <- df[df$ws_id == new_id, "out_id"]
    for (out_id in out_ids) {
      paint_outlet(lf, out_id)
    }
    paint_watershed_map(lf, new_id)
  })
  
  ### ROUTING MAP #######################################
  
  update_routing_time_map <- function() {
    if(is.null(v$routing_map_stars)) return(NULL)
    vel <- v$stream_par_cfg$stream_velocity #m/sec
    if(is.null(vel)) vel <- 0.4 #default
    
    rm <- st_as_stars(v$routing_map_stars)
    
    rtm <- rm/(vel * seconds_in_day)
    rtm <- round(rtm)
    msf <- st_as_sf(rtm,
                    as_points = F,
                    merge = T,
                    connect8 = T)
    msf <- st_transform(msf, crs = st_crs(rm))
    names(msf) <- c("val", "geometry")
    v$routing_time_map_sf <- msf
  }
  
  output$routing_map_leaflet <- renderLeaflet({
    if(is.null(v$routing_map_stars)) return()
    lf <- base_leaflet()
    m <- st_as_stars(v$routing_map_stars)
    val <- sort(unique(as.vector(m[[1]]))) / 1000
    pal <- colorNumeric(grad_color(10), val, na.color = "transparent")
    
    rt_map <- v$routing_time_map_sf
    if(is.null(rt_map)) {
      update_routing_time_map()
      rt_map <- v$routing_time_map_sf
    }
    labels <- map_label(
      paste(
        "<div>Routing time:",
        f_number(rt_map$val, digits = 2),
        "days</div>"
      ),
      "Routing Time Map"
    )
    
    if (!is.null(m)) {
      lf <- lf |>
        addGeoRaster(
          m,
          colorOptions = colorOptions(palette = grad_color(200)),
          opacity = 0.6,
          layerId = 'routing_map'
        ) |>
        addFeatures(
          rt_map,
          color = "#000",
          fillOpacity = 0.01,
          opacity = 0.01,
          weight = 3,
          group = "routing_time_map",
          label = ~ labels,
          labelOptions = labelOptions(
            className = "map_label",
            offset = c(0, -10),
            direction = "top"
          ),
          highlightOptions = highlightOptions(color  = "#9a130e", fillColor = "#9a130e", opacity = 0.8, fillOpacity = 0.3)
        ) |>
        addLegend(
          pal = pal,
          values = val,
          opacity = 0.6,
          title = "Distance (km)"
        ) |>
        addHomeButton(ext = st_bbox(m),
                      group = "Home",
                      position = "topleft") |>
        show_stream(v$dem_stream_sf, F) |>
        show_routing_stream()
    }
    return(lf)
  })
  
  show_routing_stream <- function(lf) {
    m <- v$routing_river_sf
    if (is.null(m)) {
      return(lf)
    }
    names(m) <- c("val", "geometry")
    s_list <- data.frame(val = sort(unique(m$val)))
    s_list$days <- routing_time_days(s_list$val * 1000)
    nr <- nrow(s_list)
    m <- merge(m, s_list, by = "val")
    labels <- map_label(
      paste(
        "<div>Routing distance: &plusmn;",
        f_number(m$val, digits = 2),
        "km</div>",
        "<div>Routing time:",
        f_number(m$days, digits = 2),
        "days</div>"
      ),
      "Stream Routing Path"
    )
    lf <- lf |>
      addFeatures(
        m,
        color = "#000",
        fillOpacity = 0.01,
        opacity = 0.01,
        weight = 3,
        group = "routing",
        label = ~ labels,
        labelOptions = labelOptions(
          className = "map_label",
          offset = c(0, -10),
          direction = "top"
        ),
        highlightOptions = highlightOptions(color  = "red", fillColor = "red", opacity = 1, fillOpacity = 1)
      )
    return(lf)
  }
  
  update_routing_map <- function() {
    leafletProxy("routing_map_leaflet", session) |>
      clearGroup("routing") |> show_routing_stream()
  }
  
  
  
  routing_time_days <- function(dist_m) {
    vel <- v$stream_par_cfg$stream_velocity #m/sec
    if(is.null(vel)) vel <- 0.4 #default
    t <- (dist_m/vel)/seconds_in_day
    return(t)
  }
  
  observeEvent(input$stream_velocity_input, {
    p <- isolate(v$stream_par_cfg)
    if(is.null(p)) p <- list()
    p$stream_velocity <- input$stream_velocity_input
    v$stream_par_cfg <- p
    update_routing_time_map()
    update_routing_map()
  })
  
  ### LAKE PARAMETERS ###############################

  output$lake_map_leaflet <- renderLeaflet({
    sc <- v$subcatchment_map_sf
    if (is.null(sc))
      return()
    label <- "<i>Click here to add Lake or DAM location</i>"
    lf <- base_leaflet() |>
      clearShapes() |>
      clearMarkers() |>
      clearControls() |>
      fit_map_view(sc) |>
      show_stream(v$dem_stream_sf, opacity = 1) |>
      paint_subcatchment(sc, "#ffb703", "#9a130e", label)
  })
  
  paint_subcatchment <- function(lf, sc, color, highlight_color, label = "") {
    label <- map_label(paste("ID:", sc$ws_id, "<p>", label, "</p>"), "Sub-Catchment")
    ids <- paste0("ws_", sc$ws_id)
    lf |> removeShape(ids) |>
      addFeatures(
        sc,
        group = "subcatchment",
        layerId = ids,
        color = "#000",
        fillColor = color,
        fillOpacity = 0.6,
        opacity = 0.8,
        weight = 1,
        label = label,
        labelOptions = labelOptions(
          className = "map_label",
          offset = c(0, -10),
          direction = "top"
        ),
        highlightOptions = highlightOptions(fillOpacity = 0.8, fillColor = highlight_color)
      )
  }
  
  selected_subc_id <- reactiveVal()
  
  observeEvent(input$lake_map_leaflet_shape_click, {
    ws_id <- as.numeric(suffix(input$lake_map_leaflet_shape_click$id))
    selected_subc_id(ws_id)
    b1 <- nrow(v$lake_df[v$lake_df$ws_id == ws_id, ]) > 0
    b2 <- nrow(v$dam_df[v$dam_df$ws_id == ws_id, ]) > 0
    input_dialog(
      title = paste("Sub-Catchment ID", ws_id),
      desc = "Please confirm the following options:",
      confirm_id = "confirm_lake_selection",
      confirm_label = "Confirm",
      input_var = c("lake_select", "dam_select"),
      input_label = c("Lake is in this sub-catchment", "DAM is in this sub-catchment"),
      input_def = c(b1, b2),
      input_type = c("boolean", "boolean")
    )
  })
  
  observeEvent(input$confirm_lake_selection, {
    ws_id <- selected_subc_id()
    sc <- v$subcatchment_map_sf
    sc <- sc[sc$ws_id == ws_id, ]
    lf <- leafletProxy("lake_map_leaflet", session)
    mod_label <- "<i>Click here to modify Lake or DAM location</i>"
    add_label <- "<i>Click here to add Lake or DAM location</i>"
    desc <- ""
    lake_df <- v$lake_df
    if (input$lake_select) {
      id <- 1
      if (length(lake_df$lake_id) > 0)
        id <- max(lake_df$lake_id) + 1
      if (nrow(lake_df) == 0 || !ws_id %in% lake_df) {
        df <- data.frame(
          lake_id = id,
          ws_id = ws_id,
          label = paste("Lake", id)
        )
        lake_df <- rbind(lake_df, df)
        desc <- "<p><b>Lake</b> is in this sub-catchment</p>"
      }
    } else {
      lake_df <- lake_df[lake_df$ws_id != ws_id, ]
    }
    v$lake_df <- lake_df
    
    dam_df <- v$dam_df
    if (input$dam_select) {
      id <- 1
      if (length(dam_df$dam_id) > 0)
        id <- max(dam_df$dam_id) + 1
      if (nrow(dam_df) == 0 || !ws_id %in% dam_df) {
        df <- data.frame(
          dam_id = id,
          ws_id = ws_id,
          label = paste("DAM", id)
        )
        dam_df <- rbind(dam_df, df)
        if(input$lake_select) {
          desc <- paste("<p><b>Lake</b> and <b>DAM</b> is in this sub-catchment</p>")
        } else {
          desc <- paste("<p><b>DAM</b> is in this sub-catchment</p>")
        }
      }
    } else {
      dam_df <- dam_df[dam_df$ws_id != ws_id, ]
    }
    v$dam_df <- dam_df
    
    if(input$lake_select || input$dam_select) {
      paint_subcatchment(lf, sc, "#034464", "#9a130e", paste(desc, mod_label))
    } else {
      paint_subcatchment(lf, sc, "#ffb703", "#9a130e", add_label)
    }
    # dam_edited <- dam_table_display()
    # print(dam_df)
    removeModal()
  })
  
  output$lake_list <- renderReactable({
    df <- v$lake_df
    if (is.null(df) || nrow(df) == 0)
      return()
    
    reactable(
      df,
      theme = reactableTheme(
        backgroundColor = "#FFFFFF00",
        headerStyle = list(backgroundColor = "rgba(240, 240, 240)")
      ),
      columns = list(
        lake_id = colDef(name = "ID", width = 40),
        ws_id = colDef(name = "WS_ID", width = 60),
        label = colDef(name = "Label")
      ),
      onClick = JS(
        "function(rowInfo, column) {
            if (column.id !== 'label') {return}
            if (window.Shiny) {
              Shiny.setInputValue('table_edit_label', 
              {row: rowInfo.index + 1, table:'lake_df'}, {priority: 'event'})
            }
          }"
      )
    )
  })
  
  edited_label <- reactiveVal()
  
  observeEvent(input$table_edit_label, {
    ed <- input$table_edit_label
    edited_label(ed)
    label <- v$lake_df[ed$row, "label"]
    updateTextInput(inputId = "lake_label", value = label)
    toggle_popover("edit_lake_label", T)
  })
  
  observeEvent(input$edit_label_confirm, {
    ed <- edited_label()
    print(ed)
    v[[ed$table]][ed$row, "label"] <- input$lake_label
    toggle_popover("edit_lake_label", F)
  })
  
  dam_edit_df <- reactable_edit_server(
    "dam_table",
    reactive(v$dam_df),
    theme = reactableTheme(
      backgroundColor = "#FFFFFF00",
      headerStyle = list(backgroundColor = "rgba(240, 240, 240)")
    ),
    columns = list(
      dam_id = colDef(name = "ID", width = 40),
      ws_id = colDef(name = "WS_ID", width = 60),
      label = colDef(name = "Label", style = list(cursor = "text"))
    ),
    editable = c(F, T, T)
  )
  
  observe({
    v$dam_df <- dam_edit_df()
  })
  
  

  ### GROUND WATER PARAMETERS ###############################
  
  initialize_ground_par_df <- function() {
    ws_ids <- as.data.frame(v$subcatchment_map_sf)$ws_id
    ws_ids <- sort(unique(ws_ids))
    map_ids <- v$lc_map_df$map_id
    map_ids_col <- rep(map_ids, each = length(ws_ids))
    ws_ids_col <- rep(ws_ids, length(map_ids))
    df <- data.frame(map_id = map_ids_col, ws_id = ws_ids_col)
    df[ground_prop_cols$var] <- NA
    v$ground_par_df <- df
    print("ground")
    ground_par_df_edited <- ground_par_df_display()
  }
  
  ground_prop_cols <- data.frame(
    var = c("I_RivFlowTime", "I_MaxDynGWSub", "I_GWRelFrac"),
    label = c(
      "Relative flow velocity",
      "Max dynamic GW store (mm)",
      "Ground water release fraction"
    )
  )
  
  
  ground_par_df_display <- function() {
    pdf <- isolate(v$ground_par_df)
    # adf <- isolate(v$lc_df)[c("lc_id", "land_cover")]
    # pdf <- merge(adf, pdf, by = "lc_id", all.x = T)
    table_edit_server(
      "ground_water_table",
      pdf,
      col_title = c("map_id", "ws_id", ground_prop_cols$label),
      col_type = c("character",rep("numeric", 4)),
      col_width = c(50, 50, 250, 250, 250),
      col_disable = c(T, T, F, F, F)
    )
  }
  
  ground_par_df_edited <- ground_par_df_display()
  
  observe({
    v$ground_par_df <- ground_par_df_edited()
  })

  ### 3D VIEW ####################################
  
  observeEvent(input$generate_3d_button, {
    is_show_3d(T)
  })
  
  output$plot3d <- renderRglwidget({
    dem <- v$dem_crop_stars
    if (is.null(dem))
      return()
    try(close3d())
    
    showPageSpinner(caption = "Please wait while rendering 3D view")
    dem <- st_as_stars(dem)
    # stream <- v$dem_stream_sf
    m_mat <- dem[[1]]
    m_mat[is.na(m_mat)] <- min(m_mat, na.rm = T)
    m_mat %>%
      sphere_shade(texture = "desert") %>%
      add_shadow(ray_shade(m_mat, zscale = 3), 0.5) %>%
      add_shadow(ambient_shade(m_mat), 0) %>%
      
      
      # add_overlay(
      #   generate_polygon_overlay(
      #     stream,
      #     extent = raster::extent(st_bbox(dem)),
      #     palette = terrain.colors,
      #     heightmap = m_mat
      #   ),
      #   alphalayer = 0.7
      # ) %>%
      
      plot_3d(
        m_mat,
        zscale = 10,
        fov = 60,
        theta = 135,
        zoom = 0.75,
        phi = 45,
        triangulate = T,
        max_tri = 15000
      )
    hidePageSpinner()
    rglwidget()
  })
  
  ### SOIL PROPERTIES ####################################
  
  observe({
    if (is.null(v$dem_crop_stars))
      return()
    generate_slope_classes()
    
    # qfc <- v[[soil_water_vdf$soil_water_v[1]]]
    # paw <- v[[soil_water_vdf$soil_water_v[2]]]
    # pwp <- v[[soil_water_vdf$soil_water_v[3]]]
    # if (is.null(qfc) || is.null(paw) || is.null(pwp)) {
    #   calculate_soil_water()
    #   calculate_sub_soil_water()
    # }
    # print(v$subc_soil_water_df)
    # if (is.null(v$subc_soil_water_df))
    #   calculate_sub_soil_water()
  })
  
  generate_slope_classes <- function(filter_width = 25, noise_removal = 1) {
    print("generate_slope_classes")
    dem <- v$dem_crop_stars
    slope <- terrain(stars_to_terra(dem))
    #TODO: convert to proxy
    vd$slope_stars <- stars_to_proxy(st_as_stars(slope))
    #smoothing the slope map
    slope2 <- focal(slope, w=filter_width, fun = "mean", na.policy="omit", na.rm=T)
    slope2 <- st_as_stars(slope2)
    min <- min(slope2[[1]], na.rm = T)
    max <- max(slope2[[1]], na.rm = T)
    #standardize slope factor in reverse
    vd$slope_factor_stars <- 1-((slope2-min)/(max - min)) 
    #get available soil types
    sdf <- v$soil_layer_df[c("SMU_ID", "SOIL", "SHARE", "TOPDEP", "ORG_CARBON")]
    sdf <- sdf[sdf$TOPDEP == 0,]
    # sdf <- sdf[order(sdf$SHARE, sdf$ORG_CARBON, decreasing = T),]
    # sdf <- sdf[c(1:nd), ]
    sdf <- sdf[order(sdf$ORG_CARBON, decreasing = T),]
    #generate slope classes
    del_elv <- max - min
    nd <- nrow(sdf)
    #class range in power scale, by default
    #TODO: to be user defined scale
    d <- min + del_elv * c(1:nd)^2/nd^2
    slope_class <- slope2
    slope_class[!is.na(slope_class)] <- 1
    for(i in 1:(nd-1)) {
      slope_class[slope2 >= d[i]] <- i+1
    }
    slope_class2 <- slope_class
    for(i in 1:noise_removal) {
      slope_class2 <- focal(stars_to_terra(slope_class2), w=filter_width, fun = "modal", na.policy="omit", na.rm=T)
    }
    slope_class2 <- st_as_stars(slope_class2)
    
    sc_sf <- st_as_sf(slope_class2,
                    merge = T,
                    connect8 = T)
    sc_sf <- st_transform(sc_sf, crs = st_crs(slope_class))
    names(sc_sf) <- c("class", "geometry")
    #create class label and associated soil types 
    d <- c(min, d)
    d <- sprintf("%0.1f\u00B0", d)
    angle <- paste(head(d, -1), "-", d[-1])
    df <- data.frame(class = c(1:nd), angle = angle)
    df$soil <- sdf$SOIL
    #merge with polygons
    sc_sf <- merge(sc_sf, df, by = "class")
    color <- soil_color(nd)
    sc_sf$color <- color[sc_sf$class]
    v$slope_class_sf <- sc_sf

  }
  
  output$soil_type_leaflet <- renderLeaflet({
    m <- v$slope_class_sf
    if (is.null(m))
      return()
    
    label <- map_label(paste(
      "Class ID:",
      m$class,
      "<br>Slope:",
      m$angle,
      "<br>Soil type:",
      m$soil
    ),
    "Soil Type Map")
    
    base_leaflet() |>
      show_stream(v$dem_stream_sf, opacity = 0.6, is_show_label = F) |>
      addFeatures(
        m,
        color = "#000",
        fillColor = ~ color,
        fillOpacity = 0.8,
        opacity = 0.8,
        weight = 1,
        group = "soil",
        label = label,
        labelOptions = labelOptions(
          className = "map_label",
          offset = c(0, -10),
          direction = "top"
        ),
        highlightOptions = highlightOptions(fillColor = "#fb8500")
        
      ) |>
      fit_map_view(v$ws_boundary_sf) |>
      addHomeButton(
        ext = st_bbox(v$ws_boundary_sf),
        group = "Home",
        position = "topleft"
      )
  })
  
  output$soil_map_leaflet <- renderLeaflet({
    lf <- base_leaflet()
    if (is.null(v$soil_map_sf)) {
      if (!is.null(v$ws_boundary_sf)) {
        lf <- lf |> addFeatures(
          v$ws_boundary_sf,
          layerId = 'ws_boundary',
          color = color_theme$secondary,
          label = "Click here to query soil properties",
          labelOptions = labelOptions(className = "map_label", offset = c(0, -5)),
          highlightOptions = highlightOptions(fillOpacity = 0.8),
          fillOpacity = 0.3,
          opacity = 0.8
        ) |>
          fit_map_view(v$ws_boundary_sf) |>
          addHomeButton(
            ext = st_bbox(v$ws_boundary_sf),
            group = "Soil Map",
            position = "topleft"
          )
      }
    } else {
      lf <- lf |> paint_soil_map() |> addHomeButton(
        ext = st_bbox(v$ws_boundary_sf),
        group = "Soil Map",
        position = "topleft"
      )
    }
    return(lf)
  })
  
  paint_soil_map <- function(lf) {
    msf <- v$soil_map_sf
    sid <- sort(unique(msf$smu_id))
    s <- v$soil_layer_df
    s <- s[s$LAYER == "D1" &
             s$SEQUENCE == 1, c("SMU_ID", "soil_type")]
    s <- s[!duplicated(s), ]
    s <- s[match(msf$smu_id, s$SMU_ID), ]
    # display the soil map
    label <- paste(
      "<div style='font-size:1.2em;'>
        <div>Soil Mapping Unit ID:",
      msf$smu_id,
      "</div>",
      "<div>Soil type:",
      s$soil_type,
      "</div>",
      "<p style='margin-top:5px;'>
        <strong><em>Click for more information</em></strong></p></div>"
    ) |> lapply(htmltools::HTML)
    lf <- lf |>
      
      addFeatures(
        msf,
        layerId = sid,
        group = "soil",
        color = chart_color[1:nrow(msf)],
        label = ~ label,
        highlightOptions = highlightOptions(fillOpacity = 0.8),
        fillOpacity = 0.6,
        stroke = F,
        popup = unlist(lapply(msf$smu_id, soil_info_ui)),
        labelOptions = labelOptions(className = "map_label", offset = c(0, -5))
      ) |>
      fit_map_view(msf)
    return(lf)
  }
  
  #### Soil Depth Map ###############################
  
  update_soil_legend <- reactiveVal(F)
  
  observe({
    if (update_soil_legend())
      update_soil_legend(F)
    min_d <- input$min_soil_depth_input
    max_d <- input$max_soil_depth_input
    v$genriver_cfg$min_soil_depth <- min_d
    v$genriver_cfg$max_soil_depth <- max_d
    
    val <- c(min_d:max_d) 
    pal <- colorNumeric(rev(soil_color(10)), val, na.color = "transparent")
    leafletProxy("soil_depth_leaflet", session) |>
      removeControl("soil_depth_legend") |>
      addLegend(layerId = "soil_depth_legend",
        pal = pal,
        values = val,
        opacity = 0.8,
        title = "Soil Depth (cm)"
      ) 
  })
  
  output$soil_depth_leaflet <- renderLeaflet({
    m <- vd$slope_factor_stars
    if (is.null(m))
      return()
    lf <- base_leaflet() |>
      addGeoRaster(
        m,
        colorOptions = colorOptions(palette = rev(soil_color(256))),
        opacity = 0.8,
        layerId = 'soil_depth_map'
      ) |>
      fit_map_view(m) |>
      addHomeButton(ext = st_bbox(m),
                    group = "Home",
                    position = "topleft")
    update_soil_legend(T)
    return(lf)
  })
  
  output$slope_map_leaflet <- renderLeaflet({
    m <- vd$slope_stars
    if (is.null(m))
      return()
    m <- st_as_stars(m)
    val <- range(m[[1]], na.rm = T)
    pal <- colorNumeric(map_color(10), val, na.color = "transparent")
    lf <- base_leaflet() |>
      addGeoRaster(
        m,
        colorOptions = colorOptions(palette = map_color(256)),
        opacity = 0.8,
        layerId = 'slope_map'
      ) |>
      fit_map_view(m) |>
      addHomeButton(ext = st_bbox(m),
                    group = "Home",
                    position = "topleft") |>
      addLegend(
        layerId = "slope_legend",
        pal = pal,
        values = val,
        opacity = 0.8,
        title = "Slope (deg)"
      )
    return(lf)
  })
  
  
  # observeEvent(input$soil_map_leaflet_shape_click, {
  #   click_inp <- input$soil_map_leaflet_shape_click
  #   if (!is.null(click_inp$id)) {
  #     if (click_inp$id == "ws_boundary") {
  #       withProgress(message = 'Please wait while generating soil map',
  #                    value = 0,
  #                    
  #                    generate_soil_map())
  #     }
  #   }
  # })
  
  soil_db_path <- "soil/HWSD2.sqlite"
  soil_map_path <- "/vsizip/soil/HWSD2_RASTER.zip/HWSD2.bil"
  
  
  generate_soil_db <- function(smu_ids) {
    db <- dbConnect(RSQLite::SQLite(), soil_db_path)
    id_str <- paste(smu_ids, collapse = ",")
    qm <- paste("SELECT * FROM HWSD2_LAYERS_METADATA")
    v$soil_metadata_df <- dbGetQuery(db, qm)
    v$soil_metadata_df$FIELD <- trimws(v$soil_metadata_df$FIELD)
    ql <- paste(
      "SELECT HWSD2_SMU_ID AS SMU_ID, D_COVERAGE.VALUE as COVERAGE,
            SEQUENCE, D_WRB4.VALUE as SOIL, SHARE,
            D_ROOT_DEPTH.VALUE as ROOT_DEPTH, PH1.VALUE as PHASE1, PH2.VALUE as PHASE2,
            D_ROOTS.VALUE as ROOTS, D_IL.VALUE as IL, D_SWR.VALUE as SWR,
            D_DRAINAGE.VALUE as DRAINAGE, AWC, LAYER, TOPDEP, BOTDEP, COARSE,
            D_TEXTURE_USDA.VALUE as TEXTURE_USDA, D_TEXTURE_SOTER.VALUE as TEXTURE_SOTER,
            SAND, SILT, CLAY, BULK, REF_BULK, ORG_CARBON,
            PH_WATER, TOTAL_N, CN_RATIO, CEC_SOIL, CEC_CLAY, CEC_EFF, TEB,
            BSAT, ALUM_SAT, ESP, TCARBON_EQ, GYPSUM, ELEC_COND",
      "FROM HWSD2_LAYERS",
      "LEFT JOIN D_WRB4 ON HWSD2_LAYERS.WRB4=D_WRB4.CODE",
      "LEFT JOIN D_COVERAGE ON HWSD2_LAYERS.COVERAGE=D_COVERAGE.CODE",
      "LEFT JOIN D_ROOT_DEPTH ON HWSD2_LAYERS.ROOT_DEPTH=D_ROOT_DEPTH.CODE",
      "LEFT JOIN D_PHASE as PH1 ON HWSD2_LAYERS.PHASE1=PH1.CODE",
      "LEFT JOIN D_PHASE as PH2 ON HWSD2_LAYERS.PHASE2=PH2.CODE",
      "LEFT JOIN D_ROOTS ON HWSD2_LAYERS.ROOTS=D_ROOTS.CODE",
      "LEFT JOIN D_IL ON HWSD2_LAYERS.IL=D_IL.CODE",
      "LEFT JOIN D_SWR ON HWSD2_LAYERS.SWR=D_SWR.CODE",
      "LEFT JOIN D_DRAINAGE ON HWSD2_LAYERS.DRAINAGE=D_DRAINAGE.CODE",
      "LEFT JOIN D_TEXTURE_USDA ON HWSD2_LAYERS.TEXTURE_USDA=D_TEXTURE_USDA.CODE",
      "LEFT JOIN D_TEXTURE_SOTER ON HWSD2_LAYERS.TEXTURE_SOTER=D_TEXTURE_SOTER.CODE",
      "WHERE HWSD2_SMU_ID IN (",
      id_str,
      ")"
    )
    s <- dbGetQuery(db, ql)
    s$soil_type <- trimws(paste0(s$SOIL, " (", s$SHARE, "%)"))
    s$soil_depth <- trimws(paste0(s$TOPDEP, "-", s$BOTDEP))
    v$soil_layer_df <- s
    dbDisconnect(db)
  }
  
  #by sequence in layer D1
  c1 <- c(
    # "SEQUENCE",
    "soil_type",
    "ROOT_DEPTH",
    "PHASE1",
    "PHASE2",
    "ROOTS",
    "IL",
    "SWR",
    "DRAINAGE",
    "AWC"
  )
  #physical soil properties
  c2 <- c(
    "soil_depth",
    "COARSE",
    "SAND",
    "SILT",
    "CLAY",
    "TEXTURE_USDA",
    "TEXTURE_SOTER",
    "BULK",
    "REF_BULK"
  )
  #chemical soil properties
  c3 <- c(
    "soil_depth",
    "ORG_CARBON",
    "PH_WATER",
    "TOTAL_N",
    "CN_RATIO",
    "CEC_SOIL",
    "CEC_CLAY",
    "CEC_EFF",
    "TEB",
    "BSAT",
    "ALUM_SAT",
    "ESP",
    "TCARBON_EQ",
    "GYPSUM",
    "ELEC_COND"
  )
  ### soil info by ID ###
  generate_soil_info <- function(smu_id) {
    s <- v$soil_layer_df[v$soil_layer_df$SMU_ID == smu_id, ]
    s <- s[order(s$SEQUENCE, s$LAYER), ]
    s1 <- s[s$LAYER == "D1", c1]
    s1 <- s1[, colSums(!is.na(s1)) > 0]
    s1 <- s1[!duplicated(s1), ]
    d <- list(general = s1)
    d$physical <- lapply(s1$soil_type, function(x) {
      df <- s[s$soil_type == x, c2]
      df[df < -0.0001] <- ""
      df[!duplicated(df), ]
    })
    d$chemical <- lapply(s1$soil_type, function(x) {
      df <- s[s$soil_type == x, c3]
      df[df < -0.0001] <- ""
      df[!duplicated(df), ]
    })
    names(d$physical) <- s1$soil_type
    names(d$chemical) <- s1$soil_type
    cg <- colnames(s1)[-1]
    hlist <- list(general = cg,
                  physical = c2[-1],
                  chemical = c3[-1])
    d$header <- lapply(hlist, function(x) {
      df <- v$soil_metadata_df[v$soil_metadata_df$FIELD %in% x, ]
      df[match(x, df$FIELD), ]
      paste0(df$DESCRIPTION, ifelse(
        is.na(df$UNIT) |
          df$UNIT == "",
        "",
        paste0(
          " <span style='color:#BBBBBB;font-weight: normal;'>[",
          gsub("\\s", "", df$UNIT),
          "]</span>"
        )
      ))
    })
    d$header$general <- c("Soil Type", d$header$general)
    d$header$physical <- c("Soil Depth", d$header$physical)
    d$header$chemical <- c("Soil Depth", d$header$chemical)
    return(d)
  }
  
  soil_fieldnames <- function(field) {
    df <- isolate(v$soil_metadata_df)
    n <- df[df$FIELD == field, "DESCRIPTION"]
    if (length(n) == 0)
      return(field)
    u <- df[df$FIELD == field, "UNIT"]
    if (length(u) != 0) {
      n <- paste0(
        n,
        " <span style='color:#BBBBBB;font-weight: normal;'>[",
        gsub("\\s", "", u),
        "]</span>"
      )
    }
    return(n)
  }
  
  to_html_table <- function(dataframe, header = NULL) {
    if (is.null(header))
      header <- colnames(dataframe)
    dataframe[is.na(dataframe)] <- ""
    div(style = "overflow:auto;margin:20px 0px 40px 0px",
        tags$table(class = "soil_table", tags$thead(tags$tr(
          lapply(header, function(x)
            tags$th(HTML(x)))
        )), tags$tbody(apply(dataframe, 1, function(x) {
          tags$tr(lapply(x, function(y)
            tags$td(y)))
        }))))
  }
  
  soil_info_ui <- function(smu_id) {
    dsource <-  unique(v$soil_layer_df$COVERAGE)
    d <- generate_soil_info(smu_id)
    p1 <- lapply(names(d$physical), function(x) {
      nav_panel(x, to_html_table(d$physical[[x]], d$header$physical))
    })
    p2 <- lapply(names(d$chemical), function(x) {
      nav_panel(x, to_html_table(d$chemical[[x]], d$header$chemical))
    })
    ui <- navset_tab(
      nav_panel("General information", div(
        h6(
          style = "margin-top:20px",
          "Soil Mapping Unit ID:",
          tags$b(smu_id),
          span(style = "margin:0px 20px"),
          "Data sources:",
          tags$b(
            paste(dsource, collapse = ","),
            "- Harmonized World Soil Database v2.0 (FAO)"
          )
        ),
        to_html_table(d$general, d$header$general)
      )),
      nav_panel(
        "Physical properties",
        div(style = "margin-top:10px", do.call(navset_underline, p1))
      ),
      nav_panel(
        "Chemical properties",
        div(style = "margin-top:10px", do.call(navset_underline, p2))
      )
    )
    return(as.character(ui))
  }
  
  generate_soil_map <- function() {
    mb <- v$ws_boundary_stars
    if (is.null(mb)) {
      return()
    }
    print("generate_soil_map")
    sf_use_s2(FALSE)
    withProgress(message = 'Generating soil map', value = 0, {
      # get global soil map
      setProgress(0.05, detail = "Get the global soil map")
      smap <- read_stars(soil_map_path, proxy = T)
      # crop to bounding box of watershed and expand a bit to avoid resolution conversion lost
      setProgress(0.1, detail = "Cropping the global soil map")
      bbsub <- st_as_stars(st_crop(smap, st_bbox(mb)))
      # convert to DEM resolution
      bbsub_h <- rescale(bbsub, mb, setProgress, is_smooth_edge = F)
      print("crop soil")
      msub <- crop_raster(bbsub_h, mb)
      # convert soil map raster to polygon
      msf <- stars_to_sf(msub)
      names(msf) <- c("smu_id", "geometry")
      # query database
      sid <- sort(unique(msf$smu_id))
      generate_soil_db(sid)
      
      v$soil_map_sf <- msf
      v$soil_map_stars <- msub
      paint_soil_map(leafletProxy("soil_map_leaflet", session))
      setProgress(0.98, detail = "Rendering map")
    })
  }
  
  ### SOIL HYDRAULIC ##############################
  
  hydro_id <- c("SMU_ID", "SHARE", "soil_type","SOIL", "soil_depth", "TOPDEP", "BOTDEP")
  hydro_prop <- c("SAND",
                  "SILT",
                  "CLAY",
                  "BULK",
                  "REF_BULK",
                  "ORG_CARBON",
                  "CEC_SOIL",
                  "PH_WATER")
  
  observe({
    sl_df <- v$soil_layer_df
    if (is.null(sl_df) || nrow(sl_df) == 0)
      return()
    #Soil properties
    sdf <- sl_df[c(hydro_id, hydro_prop)]
    sdf <- sdf[!duplicated(sdf), ]
    sdf[sdf < 0] <- NA
    if (any(is.na(sdf))) {
      agg_type_df <- aggregate(sdf[hydro_prop], list(
        SMU_ID = sdf$SMU_ID,
        soil_type = sdf$soil_type
      ), function(x)
        mean(x, na.rm = T))
      for (id in unique(agg_type_df$SMU_ID)) {
        for (st in agg_type_df$soil_type) {
          if (any(is.na(sdf[sdf$SMU_ID == id & sdf$soil_type == st, ]))) {
            for (coln in hydro_prop) {
              vdf <- sdf[sdf$SMU_ID == id & sdf$soil_type == st, coln]
              vdf[is.na(vdf)] <- agg_type_df[agg_type_df$SMU_ID == id &
                                               agg_type_df$soil_type == st, coln]
              sdf[sdf$SMU_ID == id &
                    sdf$soil_type == st, coln] <- vdf
            }
          }
        }
      }
    }
    
    sdf$layer_share <- (sdf$BOTDEP - sdf$TOPDEP)/200
    
    # sdf[hydro_prop] <- sdf[hydro_prop] * sdf$SHARE / 100
    # soil_df <- aggregate(
    #   sdf[hydro_prop],
    #   list(
    #     SMU_ID = sdf$SMU_ID,
    #     soil_depth = sdf$soil_depth,
    #     TOPDEP = sdf$TOPDEP
    #   ),
    #   sum
    # )
    
    soil_df <- sdf
    #Calculate hydraulic properties of all layers
    soil_df$soil_saturation <- pt.thetaSat.tropic(soil_df$CLAY,
                                                  soil_df$SAND,
                                                  soil_df$BULK,
                                                  soil_df$CEC_SOIL,
                                                  soil_df$PH_WATER)
    soil_df$field_capacity <- pt.theta.tropic(
      -33,
      soil_df$CLAY,
      soil_df$BULK,
      soil_df$SILT,
      soil_df$ORG_CARBON,
      soil_df$CEC_SOIL,
      soil_df$PH_WATER
    )
    soil_df$permanent_wilting_point <- pt.theta.tropic(
      -1500,
      soil_df$CLAY,
      soil_df$BULK,
      soil_df$SILT,
      soil_df$ORG_CARBON,
      soil_df$CEC_SOIL,
      soil_df$PH_WATER
    )
    
    sw_types <- unlist(soil_water_types)
    
    soil_df[sw_types] <-  soil_df[sw_types] * soil_df$layer_share
    swsub_df <- soil_df[soil_df$soil_depth != "0-20",]
    swsub_df <- aggregate(swsub_df[c("layer_share",sw_types)], list(soil = swsub_df$SOIL), sum)
    
    swsub_df["soil_quick_flow_capacity"] <- swsub_df["soil_saturation"] - swsub_df["field_capacity"]
    swsub_df["plant_available_water"] <- swsub_df["field_capacity"] - swsub_df["permanent_wilting_point"]
    
    #convert to mm of depth
    # soil_df[paste0(sw_types, "_ws")] <- soil_df[paste0(sw_types)] * 200 
    # swsub_df <- aggregate(soil_df[soil_df$SMU_ID != "0-20", paste0(sw_types, "_ws")], list(SMU_ID = soil_df$SMU_ID), sum)
    # 
    
    #LC properties
    lc_par_df <- v$lc_par_df
    #Calculate hydraulic properties of top soil
    top_soil_df <- soil_df[soil_df$soil_depth == "0-20", ]
    nsid <- nrow(top_soil_df)
    nlc <- nrow(lc_par_df)
    rep_lc <- lc_par_df[rep(seq_len(nrow(lc_par_df)), nsid), ]
    rep_s <- top_soil_df[rep(seq_len(nrow(top_soil_df)), each = nlc), ]
    df <- cbind(rep_lc, rep_s)
    df$bulk_density <- df$I_BD_BDRefVegNow * df$REF_BULK
    df$soil_saturation <- pt.thetaSat.tropic(df$CLAY,
                                             df$SAND,
                                             df$bulk_density,
                                             df$CEC_SOIL,
                                             df$PH_WATER)
    df$field_capacity <- pt.theta.tropic(-33,
                                         df$CLAY,
                                         df$bulk_density,
                                         df$SILT,
                                         df$ORG_CARBON,
                                         df$CEC_SOIL,
                                         df$PH_WATER)
    df$permanent_wilting_point <- pt.theta.tropic(
      -1500,
      df$CLAY,
      df$bulk_density,
      df$SILT,
      df$ORG_CARBON,
      df$CEC_SOIL,
      df$PH_WATER
    )
    df[sw_types] <- df[sw_types] * df$layer_share 
    # top_cols <- c("lc_id", "SMU_ID", sw_types, "bulk_density", "I_BD_BDRefVegNow")
    top_cols <- c("lc_id", "SOIL", sw_types, "bulk_density", "I_BD_BDRefVegNow")
    topsoil_df <- df[top_cols]
    
    # def_df <- soil_df[soil_df$soil_depth == "0-20", c("SMU_ID", sw_types, "BULK", "REF_BULK")]
    def_df <- soil_df[soil_df$soil_depth == "0-20", c("SOIL", sw_types, "BULK", "REF_BULK")]
    def_df$lc_id <- -999
    def_df$bulk_density <- def_df$BULK
    def_df$I_BD_BDRefVegNow <- def_df$BULK/def_df$REF_BULK
    topsoil_df <- rbind(topsoil_df, def_df[top_cols])  
    v$soil_hydraulic_top_df <- topsoil_df
    v$soil_hydraulic_sub_df <- soil_df
    
    topsoil_df["soil_quick_flow_capacity"] <- topsoil_df["soil_saturation"] - topsoil_df["field_capacity"]
    topsoil_df["plant_available_water"] <- topsoil_df["field_capacity"] - topsoil_df["permanent_wilting_point"]
    
    
    wcol <- c("soil_quick_flow_capacity", "plant_available_water", "permanent_wilting_point")
    w <- swsub_df[wcol]
    colnames(w) <- paste0(wcol, "_sub")
    w$soil <- swsub_df$soil
    
    sw_df <- topsoil_df[c("lc_id", wcol)]
    sw_df$soil <- topsoil_df$SOIL
    sw_df <- merge(sw_df, w, by = "soil")
    sw_av_df <- sw_df[c("soil", "lc_id")]
    #### calculate water availability ####
    # I_SoilSatMinFC
    sw_av_df["soil_quick_flow_capacity"] <- sw_df["soil_quick_flow_capacity"] + sw_df["soil_quick_flow_capacity_sub"]
    # I_PlantAvWat
    sw_av_df["plant_available_water"] <- sw_df["plant_available_water"] + sw_df["plant_available_water_sub"] 
    #  I_PWP
    sw_av_df["inaccessible_water"] <- sw_df["permanent_wilting_point"] + sw_df["permanent_wilting_point_sub"]
    # v$soil_water_content_df <- sw_df[c("SMU_ID", "lc_id", sw_types)]
    
    
    # #calculate soil water
    # sw_df <- topsoil_df[c("SMU_ID", "lc_id", sw_types)]
    # #convert to mm of depth
    # sw_df[paste0(sw_types, "_wt")] <- sw_df[paste0(sw_types)] * 200
    # sw_df <- merge(sw_df, swsub_df, by = "SMU_ID")
    # # sw_df[sw_types] <- sw_df[paste0(sw_types, "_w")] + sw_df[paste0(sw_types, "_ws")]
    # sw_av_df <- sw_df[c("SMU_ID", "lc_id")]

    # # I_SoilSatMinFC
    # sw_av_df["soil_quick_flow_capacity"] <- sw_df["soil_saturation_wt"] - sw_df["field_capacity_wt"] +
    #   sw_df["soil_saturation_ws"] - sw_df["field_capacity_ws"]
    # # I_PlantAvWat
    # sw_av_df["plant_available_water"] <- sw_df["field_capacity_wt"] - sw_df["permanent_wilting_point_wt"] +
    #   sw_df["field_capacity_ws"] - sw_df["permanent_wilting_point_ws"]
    # #  I_PWP
    # sw_av_df["inaccessible_water"] <- sw_df["permanent_wilting_point_wt"] + sw_df["permanent_wilting_point_ws"]
    # v$soil_water_content_df <- sw_df[c("SMU_ID", "lc_id", sw_types)]
    v$soil_water_content_df <- sw_av_df
  })
  
  # observe({
  #   wfield <- input$soil_water_select
  #   soil_df <- v$soil_water_content_df
  #   if(all(is.na(soil_df[soil_df$lc_id != -999, wfield]))) {
  #     vd$soil_water_map_stars_list <- NULL
  #     return()
  #   }
  #   # smu_ids <- unique(soil_df$SMU_ID)
  #   soil_m <- isolate(v$soil_map_stars)
  #   print("generate_soil_water_map")
  #   lc_ms <- isolate(v$lc_map_crop_stars_list)
  #   mlist <- list()
  #   val_df <- soil_df[c("SMU_ID", "lc_id", wfield)]
  #   colnames(val_df) <- c("SMU_ID", "lc_id", "value")
  #   for(mid in names(lc_ms)) {
  #     lc_m <- st_as_stars(lc_ms[[mid]])
  #     
  #     mlist[[mid]] <- lc_soil_value_map(lc_m, soil_m, val_df)
  #   }
  #   vd$soil_water_map_stars_list <- mlist
  # })
  
  lc_soil_value_map <- function(lc_map, soil_map, val_df) {
    # LC map
    lc_m <- st_as_stars(lc_map)
    wmc <- NULL
    # soil map
    soil_m <- st_as_stars(soil_map)
    smu_ids <- unique(as.vector(soil_m[[1]]))
    smu_ids <- smu_ids[!is.na(smu_ids)]
    for(smu_id in smu_ids) {
      wm <- lc_m
      df <- val_df[val_df$SMU_ID == smu_id, c("lc_id", "value")]
      defval <- df[df$lc_id == -999, "value"]
      df[is.na(df)] <- defval
      wm[soil_m != smu_id] <- NA
      wm <- reclassify_map(wm, df)
      if(is.null(wmc)) {
        wmc <- wm
      } else {
        wmc <- st_mosaic(wmc, wm)
      }
    }
    return(wmc)
  }
  
  #### Map soil water content by land cover################  

  observe({
    wfield <- input$soil_water_select
    soil_df <- v$soil_water_content_df
    if (all(is.na(soil_df[soil_df$lc_id != -999, wfield]))) {
      vd$soil_water_map_stars_list <- NULL
      return()
    }
    var <- soil_water_vdf[soil_water_vdf$col_v == wfield, "soil_water_v"]
    vd$soil_water_map_stars_list <- v[[var]]
  })
  
  
  observeEvent(input$generate_soil_water_button, {
    calculate_soil_water()
    calculate_sub_soil_water()
  })

  soil_water_vdf <- data.frame(
  soil_water_v = c("soil_quick_flow_capacity_stars_list",
  "soil_plant_available_water_stars_list", 
  "soil_inaccessible_water_stars_list"),
  col_v = c("soil_quick_flow_capacity",
  "plant_available_water",
  "inaccessible_water"))
  
  calculate_soil_water <- function() {
    print("calculate_soil_water")
    soil_df <- v$soil_water_content_df
    soil_sf <- v$slope_class_sf
    slopef_m <- vd$slope_factor_stars
    lc_ms <- v$lc_map_crop_stars_list
    if (is.null(soil_df))
      return()
    #soil depth
    min_d <- v$genriver_cfg$min_soil_depth
    max_d <- v$genriver_cfg$max_soil_depth
    sd_m <- slopef_m * (max_d - min_d) +  min_d
    
    lcmids <- names(lc_ms)
    ss <- unique(soil_df$soil)
    
    withProgress(message = 'Generating soil water map', value = 0, {
      pinc <- 1 / (length(lcmids) * length(ss))
      wflist <- as.vector(unlist(soil_water_availability))
      for (i in 1:3) {
        wf <- wflist[i]
        setProgress(0)
        mlist <- list()
        for (map_id in lcmids) {
          lc_m <- st_as_stars(lc_ms[[map_id]])
          wm <- NULL
          for (s in ss) {
            incProgress(pinc, detail = wf)
            ssf <- soil_sf[soil_sf$soil == s, "class"]
            ssf_m <- st_rasterize(ssf, lc_m, align = T)
            lcs_m <- crop_raster(lc_m, ssf_m)
            
            val_df <- soil_df[soil_df$soil == s, c("lc_id", wf)]
            val_m <- reclassify_map(lcs_m, val_df)
            
            if (is.null(wm)) {
              wm <- val_m
            } else {
              wm <- st_mosaic(wm, val_m)
            }
          }
          wm2 <- crop_raster(wm, sd_m)
          #multiply with soil depth in mm
          mlist[[map_id]] <- wm2 * sd_m * 10
        }
        var <- soil_water_vdf[soil_water_vdf$col_v == wf, "soil_water_v"]
        v[[var]] <- mlist
      }
    })
  }  

  #### Soil Water By Subcatchment ###################
  
  
  
  calculate_sub_soil_water <- function() {
    qfc <- v[[soil_water_vdf$soil_water_v[1]]]
    paw <- v[[soil_water_vdf$soil_water_v[2]]]
    pwp <- v[[soil_water_vdf$soil_water_v[3]]]
    if (is.null(qfc) || is.null(paw) || is.null(pwp))
      return()
    print('Calculating subcatchment soil water')
    # print(qfc)
    qfc <- lapply(qfc, st_as_stars)
    paw <- lapply(paw, st_as_stars)
    pwp <- lapply(pwp, st_as_stars)
    
    sw <- soil_water_vdf$col_v
    lc_map_list <- v$lc_map_crop_stars_list
    lc_map_list <- lapply(lc_map_list, st_as_stars)
    # lc_m <- st_as_stars(lc_map_list[[1]])
    lc_m <- lc_map_list[[1]]
    subc <- v$subcatchment_map_sf
    subc_stars <- st_rasterize(subc["ws_id"], lc_m, align = T)
    subc_stars <- crop_raster(subc_stars, lc_m)
    subc_lc <- v$subc_lc_df
    subc_lc[sw] <- NULL
    map_ids <- unique(subc_lc$map_id)
    # ws_ids <- sort(unique(subc_lc$ws_id))

    pinc <- 1/length(map_ids)
    all_wdf <- NULL
    
    withProgress(message = 'Calculating subcatchment soil water', value = 0, {
      # suppressMessages(
        
        for(map_id in map_ids) {
            incProgress(pinc,
                        detail = paste("LC Map:", map_id))
          lc_m <- lc_map_list[[map_id]]
          qfc_m <- qfc[[map_id]]
          paw_m <- paw[[map_id]]
          pwp_m <- pwp[[map_id]]
          
          # setProgress(0)
          # for (ws_id in ws_ids) {
          #   incProgress(pinc,
          #               detail = paste("LC Map:", map_id, "Subcatchment:", ws_id, "of", max(ws_ids)))
          #   df <- subc_lc[subc_lc$map_id == map_id &
          #                   subc_lc$ws_id == ws_id, ]
          #   sub_qfc_m <- qfc_m[subc_stars == ws_id]
          #   sub_paw_m <- paw_m[subc_stars == ws_id]
          #   sub_pwp_m <- pwp_m[subc_stars == ws_id]
          #   for (lc_id in df$lc_id) {
          #     qfc_v <- mean(sub_qfc_m[lc_m == lc_id][[1]], na.rm = T)
          #     paw_v <- mean(sub_paw_m[lc_m == lc_id][[1]], na.rm = T)
          #     pwp_v <- mean(sub_pwp_m[lc_m == lc_id][[1]], na.rm = T)
          #     
          #     subc_lc[subc_lc$map_id == map_id &
          #               subc_lc$ws_id == ws_id &
          #               subc_lc$lc_id == lc_id, sw] <- c(qfc_v, paw_v, pwp_v)
          #   }
          # }
          
          wdf <- data.frame(
            map_id = map_id,
            ws_id = as.vector(subc_stars[[1]]),
                            lc_id = as.vector(lc_m[[1]]))
          wdf[sw[1]] = as.vector(qfc_m[[1]])
          wdf[sw[2]] = as.vector(paw_m[[1]])
          wdf[sw[3]] = as.vector(pwp_m[[1]])
          wdf <- wdf[!is.na(wdf$lc_id) & !is.na(wdf$ws_id), ]
          if(is.null(all_wdf)) {
            all_wdf <- wdf
          } else {
            all_wdf <- rbind(all_wdf, wdf)
          }

        }
      
      # )
      
    })
    agg_wdf <- aggregate(all_wdf[sw], by = all_wdf[c("map_id", "ws_id", "lc_id")], mean, na.rm = T)
    subc_lc <- merge(subc_lc, agg_wdf, by = c("map_id", "ws_id", "lc_id"),
                     all.x = T)
    # print(subc_lc)
    v$subc_lc_df <- subc_lc
  }
  
  
  #### Soil Water Map ###################
  
  observe({
    lc_ms <- vd$soil_water_map_stars_list
    if(is.null(lc_ms)) {
      output$soil_water_content_ui <- renderUI(p("Please complete the land cover hydrological properties parameters"))
      return()
    }
    output$soil_water_content_ui <- renderUI({
      layout_column_wrap(width = 0.5, !!!lapply(names(lc_ms), function(x) {
        card(card_header(paste("Year:", x), class = "bg_light2"),
             plotOutput(paste0("lc_map_crop_", x)))
      }))
    })
    
    lapply(names(lc_ms), function(x, ms) {
      output[[paste0("lc_map_crop_", x)]] <- renderPlot({
        suppressMessages(
        plot(
          ms[[x]],
          col = water_color,
          breaks = "equal",
          main = NULL
        ))
      })
    }, lc_ms)
  })

  
  observe({
    df <- v$subc_soil_water_df
    if(is.null(df)) {
      output$soil_water_subcathment_ui <- renderUI(p("Please complete the land cover hydrological properties parameters"))
      return()
    }
    map_ids <- unique(df$map_id)
    output$soil_water_subcathment_ui <- renderUI({
      tagList(
        selectInput("sub_soil_water_select", NULL, soil_water_availability),
      layout_column_wrap(width = 0.5, !!!lapply(map_ids, function(x) {
        card(card_header(paste("Year:", x), class = "bg_light2"),
             plotOutput(paste0("sub_soil_water_", x)))
      }))
      )
    })
    
    lapply(map_ids, function(x) {
      output[[paste0("sub_soil_water_", x)]] <- renderPlot({
        wfield <- input$sub_soil_water_select
        subc <- v$subcatchment_map_sf["ws_id"]
        df <- v$subc_soil_water_df
        df <- df[df$map_id == x, c("ws_id", wfield)]
        df[wfield] <- round(as.numeric(df[[wfield]]))
        subc <- merge(subc, df, by = "ws_id")
        m <- subc[wfield]
        plot(
          m,
          pal = water_color,
          main = NULL,
          key.pos = 4
        )
      })
    })
  })
  
  
  #### Soil Water Table ###################
  get_lc_with_default <- function() {
    df <- isolate(v$lc_df)[c("lc_id", "land_cover")]
    df <- rbind(df, c(-999, "Default"))
    return(df)
  }
  
  hydro_prop_coldef <- function() {
    d <- lapply(hydro_prop , function(x) {
      colDef(name = soil_fieldnames(x), html = T)
    })
    names(d) <- hydro_prop
    return(d)
  }

  output$soil_hydraulic_top_table <- renderReactable({
    df <- v$soil_hydraulic_top_df
    if (is.null(df))
      return()
    hdf <- v$soil_metadata_df
    adf <- get_lc_with_default()
    df <- merge(df, adf, by = "lc_id", all.x = T)
    data <- v$soil_hydraulic_sub_df
    data <- data[data$soil_depth == "0-20", c("SOIL", hydro_prop)]
    
    cn <- hydro_prop_coldef()
    reactable(
      data,
      defaultExpanded = TRUE,
      columns = cn,
      onClick = "expand",
      details = function(index) {
        d_data <- df[df$SOIL == data$SOIL[index], ]
        reactable(
          d_data[c("lc_id",
                   "land_cover",
                   unlist(soil_water_types),
                   "bulk_density")],
          pagination = F,
          striped = T,
          borderless = TRUE,
          compact = TRUE,
          rownames = F,
          columns = append(
            list(
              lc_id = colDef(format = colFormat(digit = 0)),
              land_cover = colDef(name = "Land Cover"),
              bulk_density = colDef(name = "Bulk Density")
            ),
            soil_water_coldef()
          ),
          defaultColDef = colDef(format = colFormat(digit = 3))
        )
      }
    )
  })

  soil_water_coldef <- function(x) {
    cd <- lapply(names(soil_water_types), function(x){
      colDef(name = x)
    })
    names(cd) <- unlist(soil_water_types)
    return(cd)
  }
  
  output$soil_hydraulic_sub_table <- renderReactable({
    df <- v$soil_hydraulic_sub_df
    if (is.null(df))
      return()
    hpcd <- hydro_prop_coldef()
    hpcd <- append(hpcd, list(
      SOIL = colDef(format = colFormat(digit = 0)),
      soil_depth = colDef(name = "Soil Depth (cm)")
    ))
    data <- df[df$soil_depth != "0-20", c("SOIL", "soil_depth" , unlist(soil_water_types), hydro_prop)]
    reactable(
      data,
      defaultExpanded = TRUE,
      onClick = "expand",
      groupBy = "SOIL",
      pagination = F,
      striped = T,
      borderless = TRUE,
      compact = TRUE,
      rownames = F,
      columns = append(soil_water_coldef(), hpcd),
      defaultColDef = colDef(format = colFormat(digit = 3))
    )
  })

  output$soil_water_content_table <- renderReactable({
    df <- v$soil_water_content_df
    if (is.null(df))
      return()
    adf <- get_lc_with_default()
    df <- merge(adf, df, by = "lc_id")
    df$lc_id <- as.numeric(as.character(df$lc_id))
    df <- df[order(df$lc_id),]
    reactable(
      df,
      defaultExpanded = TRUE,
      onClick = "expand",
      groupBy = "soil",
      pagination = F,
      striped = T,
      borderless = TRUE,
      compact = TRUE,
      rownames = F,
      columns = list(
        lc_id = colDef(format = colFormat(digit = 0)),
        land_cover = colDef(name = "Land Cover"),
        soil_quick_flow_capacity = colDef(name = "Soil quick flow capacity (mm)"),
        plant_available_water = colDef(name = "Plant available water (mm)"),
        inaccessible_water = colDef(name = "Inaccessible water (mm)")
      ),
      defaultColDef = colDef(format = colFormat(digit = 3))
    )
  })
  
  stars_to_sf <- function(m) {
    msf <- st_as_sf(m,
                    as_points = F,
                    merge = T,
                    connect8 = T)
    msf <- st_transform(msf, crs = 4326)
    names(msf) <- c("val", "geometry")
    # merge polygon for each id
    sid <- sort(unique(msf$val))
    a <- lapply(sid, function(x) {
      st_as_sf(st_union(msf[msf$val == x, ]))
    })
    m_a <- do.call(rbind, a)
    st_geometry(m_a) <- "geometry"
    msf <- cbind(m_a, val = sid)
    return(msf)
  }
  
  
  
  
  
  
  ### SOIL AND PLANT WATER ##############################

  infiltration_par <- numeric_input_server("infiltration_par_input", infiltration_par_df)
  observe({
    v$infiltration_par_cfg <- infiltration_par()
  })
  
  groundwater_par <- numeric_input_server("groundwater_par_input", groundwater_par_df)
  observe({
    v$groundwater_par_cfg <- groundwater_par()
  })
  
  waterplant_par <- numeric_input_server("waterplant_par_input", waterplant_par_df)
  observe({
    v$waterplant_par_cfg <- waterplant_par()
  })
  
  interception_par <- numeric_input_server("interception_par_input", interception_par_df)
  observe({
    v$interception_par_cfg <- interception_par()
  })
  
  soilplant_par <- numeric_input_server("soilplant_par_input", soilplant_par_df)
  observe({
    v$soilplant_par_cfg <- soilplant_par()
  })

  ### Evapotranspiration DATA ######################
  
  update_evapotran_df_display <- function() {
    table_edit_server(
      "evapotran_df_table",
      isolate(v$evapotran_df),
      col_type = c("date", "numeric"),
      nrow = 365,
      allowRowModif = T,
      pagination = 365
    )
  }
  
  evapotran_df_edited <- update_evapotran_df_display()
  
  observe({
    v$evapotran_df <- evapotran_df_edited()
  })
  
  observe({
    df <- v$evapotran_df
    if (is.null(df) || is.null(nrow(df)) || nrow(df) == 0) {
      return()
    }
    df$year <- year(df$date)
    df$month <- month(df$date)
    df$day <- yday(df$date)
    vd$evapotran_df <- df
    mdf <- aggregate(df$evapotranspiration, list(df$month, df$year), sum)
    colnames(mdf) <- c("month", "year", "evapotranspiration")
    vd$evapotran_month_df <- mdf
  })
  
  output$evapotran_daily_plot <- renderPlotly({
    df <- vd$evapotran_df
    if (is.null(df) || nrow(df) == 0)
      return()
    nc <- length(unique(df$year))
    plot_ly(
      df,
      x = ~ day,
      y = ~ evapotranspiration,
      color = ~ factor(year),
      colors = grad_color(nc),
      alpha = 0.5,
      size = I(1),
      type = "scatter",
      mode = "lines+markers",
      marker = list(size = 4)
    ) |>
      layout(
        yaxis = list(title = "Daily evapotranspiration (mm)"),
        xaxis = list(title = "Days")
      )
  })
  
  output$evapotran_monthly_plot <- renderPlotly({
    df <- vd$evapotran_month_df
    if (is.null(df) || nrow(df) == 0)
      return()
    nc <- length(unique(df$year))
    plot_ly(
      df,
      x = ~ month,
      y = ~ evapotranspiration,
      color = ~ factor(year),
      colors = grad_color(nc),
      size = I(1),
      type = "bar"
    ) |>
      layout(
        yaxis = list(title = "Monthly evapotranspiration (mm)"),
        xaxis = list(title = "Months")
      )
  })
  
  output$evapotran_yearly_plot <- renderPlotly({
    df <- vd$evapotran_month_df
    if (is.null(df) || nrow(df) == 0)
      return()
    nc <- length(unique(df$month))
    plot_ly(
      df,
      x = ~ year,
      y = ~ evapotranspiration,
      color = ~ factor(month, ordered = T),
      colors = grad_color(nc),
      # alpha = 0.8,
      type = "bar"
    ) |> layout(
      barmode = "stack",
      yaxis = list(title = "Yearly evapotranspiration (mm)"),
      xaxis = list(title = "Years")
    )
  })
  
  ### RAIN DATA ######################
  
  rain_par <- numeric_input_server("rain_par_input", rain_par_df)
  observe({
    v$rain_par_cfg <- rain_par()
  })
  
  update_rain_df_display <- function() {
    table_edit_server(
      "rain_df_table",
      isolate(v$rain_df),
      col_type = c("date", "numeric"),
      nrow = 365,
      allowRowModif = T,
      pagination = 365
    )
  }
  
  date_format <- "%d-%b-%Y"
  
  rain_df_edited <- update_rain_df_display()
  
  observe({
    v$rain_df <- rain_df_edited()
  })
  
  observe({
    df <- v$rain_df
    if (is.null(df) || is.null(nrow(df)) || nrow(df) == 0) {
      return()
    }
    df$year <- year(df$date)
    df$month <- month(df$date)
    df$day <- yday(df$date)
    vd$rain_df <- df
    mdf <- aggregate(df$rainfall, list(df$month, df$year), sum)
    colnames(mdf) <- c("month", "year", "rainfall")
    vd$rain_month_df <- mdf
  })
  
  output$rain_daily_plot <- renderPlotly({
    df <- vd$rain_df
    if (is.null(df) || nrow(df) == 0)
      return()
    nc <- length(unique(df$year))
    plot_ly(
      df,
      x = ~ day,
      y = ~ rainfall,
      color = ~ factor(year),
      colors = grad_color(nc),
      alpha = 0.5,
      size = I(1),
      type = "scatter",
      mode = "lines+markers",
      marker = list(size = 4)
    ) |>
      layout(yaxis = list(title = "Daily rainfall (mm)"),
             xaxis = list(title = "Days"))
  })
  
  output$rain_monthly_plot <- renderPlotly({
    df <- vd$rain_month_df
    if (is.null(df) || nrow(df) == 0)
      return()
    nc <- length(unique(df$year))
    plot_ly(
      df,
      x = ~ month,
      y = ~ rainfall,
      color = ~ factor(year),
      colors = grad_color(nc),
      size = I(1),
      type = "bar"
    ) |>
      layout(
        yaxis = list(title = "Monthly rainfall (mm)"),
        xaxis = list(title = "Months")
      )
  })
  
  output$rain_yearly_plot <- renderPlotly({
    df <- vd$rain_month_df
    if (is.null(df) || nrow(df) == 0)
      return()
    nc <- length(unique(df$month))
    plot_ly(
      df,
      x = ~ year,
      y = ~ rainfall,
      color = ~ factor(month, ordered = T),
      colors = grad_color(nc),
      # alpha = 0.8,
      type = "bar"
    ) |> layout(
      barmode = "stack",
      yaxis = list(title = "Yearly rainfall (mm)"),
      xaxis = list(title = "Years")
    )
  })
  
  ### RIVER DATA ######################
  
  river_par <- numeric_input_server("river_par_input", river_par_df)
  observe({
    v$river_par_cfg <- river_par()
  })
  
  update_river_df_display <- function() {
    table_edit_server(
      "river_df_table",
      isolate(v$river_df),
      col_type = c("date", "numeric"),
      nrow = 365,
      allowRowModif = T,
      pagination = 365
    )
  }
  
  river_df_edited <- update_river_df_display()
  
  observe({
    v$river_df <- river_df_edited()
  })
  
  observe({
    df <- v$river_df
    if (is.null(df) || is.null(nrow(df)) || nrow(df) == 0) {
      vd$river_df <- NULL
      return()
    }
    if (is.null(v$ws_boundary_outlet_cfg$area_m2) ||
        v$ws_boundary_outlet_cfg$area_m2 <= 0)
      return()
    # print(df)
    df <- df[order(df$date), ]
    df$year <- year(df$date)
    df$month <- month(df$date)
    df$day <- yday(df$date)
    df$flow_mm <- convert_flow_to_mmdaily(df$river_flow, v$ws_boundary_outlet_cfg$area_m2)
    df$river_cum <- ave(df$flow_mm, df$year, FUN = cumsum)
    vd$river_df <- df
    # print(head(df, 10))
  })
  
  output$river_daily_plot <- renderPlotly({
    df <- vd$river_df
    if (is.null(df) || nrow(df) == 0)
      return()
    nc <- length(unique(df$year))
    plot_ly(
      df,
      x = ~ day,
      y = ~ river_flow,
      color = ~ factor(year),
      colors = grad_color(nc),
      alpha = 0.5,
      size = I(1),
      type = "scatter",
      mode = "lines+markers",
      marker = list(size = 4)
    ) |>
      layout(
        yaxis = list(title = "River flow (m<sup>3</sup>sec<sup>-1</sup>)"),
        xaxis = list(title = "Days")
      )
  })
  
  output$river_cum_desc <- renderUI({
    div(
      p(
        "The river flow is converted into milimeter (mm) unit as an asumption where",
        tags$i("the same amount"),
        "of rain is falling into watershed area, at which the river outlet was measured"
      ),
      p(
        "The observed watershed area is ",
        tags$b(
          f_number(v$ws_boundary_outlet_cfg$area_m2 / 10000, nsmall = 2),
          "ha",
          tags$sup(2, .noWS = c("before"))
        )
      )
    )
  })
  
  output$river_cum_plot <- renderPlotly({
    df <- vd$river_df
    if (is.null(df) || nrow(df) == 0)
      return()
    nc <- length(unique(df$year))
    plot_ly(
      df,
      x = ~ day,
      y = ~ river_cum,
      color = ~ factor(year),
      colors = grad_color(nc),
      alpha = 0.5,
      size = I(1),
      type = "scatter",
      mode = "lines+markers",
      marker = list(size = 4)
    ) |>
      layout(
        yaxis = list(title = "Cumulative river flow (mm)"),
        xaxis = list(title = "Days")
      )
  })
  
  ### RAIN AND RIVER DATA CONSISTENCY ######################
  
  observe({
    if (is.null(vd$rain_df) || is.null(vd$river_df))
      return()
    if (nrow(vd$rain_df) == 0 || nrow(vd$river_df) == 0)
      return()
    df <- merge(vd$rain_df, vd$river_df[c("date", "river_cum")], by = "date")
    yrlist <-  unique(df$year)
    
    output$consistency_ui <- renderUI({
      tagList(
        div(
          "The comparion of actual rainfall and river flow converted to mm unit in watershed area of:",
          f_number(v$ws_boundary_outlet_cfg$area_m2 / 10000, nsmall = 2),
          "ha",
          tags$sup(2, .noWS = c("before"))
        ),
        layout_column_wrap(width = 0.5, !!!lapply(yrlist, function(x) {
          card(card_header(paste("Year:", x), class = "bg_light2"),
               plotlyOutput(paste0("consistency_", x)))
        }))
      )
    })
    
    lapply(yrlist, function(x) {
      output[[paste0("consistency_", x)]] <- renderPlotly({
        ydf <- df[df$year == x, ]
        ydf$rain_cum <- cumsum(ydf$rainfall)
        fit <- lm(river_cum ~ rain_cum, data = ydf)
        plot_ly(
          ydf,
          x = ~ rain_cum,
          y = ~ river_cum,
          color = I(color_theme$secondary),
          type = "scatter",
          mode = "markers",
          name = 'Rainfall vs River flows'
        ) |>
          layout(
            yaxis = list(title = "Cumulative river flow (mm)"),
            xaxis = list(title = "Cumulative rainfall (mm)"),
            showlegend = F
          ) |>
          add_lines(
            x = ~ rain_cum,
            y = fitted(fit),
            color = I(color_theme$info),
            name = 'Regression Fit'
          )
      })
    })
  })
  
  ### DOWNLOAD PARAMETERS ######################
  
  
  
  output$download_params <- downloadHandler(
    filename = function() {
      paste("genriver_params.zip")
    },
    content = function(fname) {
      z <- NULL
      withProgress(message = 'Please wait while preparing the files', value = 0, {
        setProgress(0.1, detail = "Tabulated data")
        setwd(tempdir())
        fs <- save_variables(io_file_df, v)
        incProgress(0.1, detail = "watershed map")
        ws_f <- write_watershed_map("watershed")
        fs <- c(fs, ws_f)
        incProgress(0.1, detail = "outlet map")
        ou_f <- write_outlet_map("outlet")
        fs <- c(fs, ou_f)
        incProgress(0.1, detail = "bundling the files")
        z <- zip::zip(zipfile = fname, files = fs)
      })
      return(z)
    },
    contentType = "application/zip"
  )
  
  write_watershed_map <- function(f) {
    ws_l <- isolate(v$watershed_map_list)
    ws_m <- do.call(rbind, as.list(ws_l))
    ws_m <- cbind(ws_m, ws_id = names(ws_l))
    if (is.null(ws_m))
      return()
    st_write(ws_m,
             paste0(f, ".shp"),
             append = F,
             quiet = T)
    return(paste0(f, shp_ext))
  }
  
  extract_list <- function(ls, field) {
    return(lapply(ls, function(x)
      x[[field]]))
  }
  
  write_outlet_map <- function(f) {
    ou_l <- isolate(v$outlet_map_list)
    ou_ml <- extract_list(ou_l, "map")
    ou_m <- do.call(rbind, as.list(ou_ml))
    ou_lon <- unlist(extract_list(ou_l, "lon"))
    ou_lat <- unlist(extract_list(ou_l, "lat"))
    ou_id <- names(ou_l)
    ou_m <- cbind(ou_m,
                  ou_id = ou_id,
                  lon = ou_lon,
                  lat = ou_lat)
    if (is.null(ou_m))
      return()
    st_write(ou_m,
             paste0(f, ".shp"),
             append = F,
             quiet = T)
    return(paste0(f, shp_ext))
  }
  
  ### UPLOAD PARAMETERS #############################
  
  observeEvent(input$upload_parameter, {
    print(paste("Extracting the files:", input$upload_parameter$name))
    dpath <- input$upload_parameter$datapath
    upload_parameter(dpath)
    is_repaint_watershed(T)
    
    # if (is_watershed_leaflet_base) {
    #   lf <- leafletProxy(active_watershed_lf, session)
    #   reset_watershed_panel_display(lf)
    # } else {
    #   is_repaint_watershed(T)
    # }
    # v$subc_lc_df <- NULL
  })
  
  show_alert_file_error <- function(file_error) {
    showNotification(paste("File error! Or it was not a", file_error, "file!"),
                     type = "error")
  }
  
  data_dir <- paste0(tempdir(), "/data_temp")
  
  upload_parameter <- function(dpath) {
    file_list <- NULL
    try(file_list <- utils::unzip(dpath, list = TRUE), silent = T)
    if (is.null(file_list)) {
      show_alert_file_error("compressed (zip)")
      return()
    }
    utils::unzip(dpath, exdir = data_dir, junkpaths = T)
    v <- upload_variables(io_file_df, data_dir, v)
    
    fpath <- paste0(data_dir, "/watershed.shp")
    if (file.exists(fpath)) {
      # ws_m <- st_read(paste0(data_dir, "/watershed.shp"), quiet = T)
      ws_m <- st_read(fpath, quiet = T)
      ls <- list()
      for (i in c(1:nrow(ws_m))) {
        ls[[ws_m[[i, "ws_id"]]]] <- ws_m[i, "geometry"]
      }
      v$watershed_map_list <- ls
    }
    
    fpath <- paste0(data_dir, "/outlet.shp")
    if (file.exists(fpath)) {
      # ou_m <- st_read(paste0(data_dir, "/outlet.shp"), quiet = T)
      ou_m <- st_read(fpath, quiet = T)
      ou_l <- list()
      for (i in c(1:nrow(ou_m))) {
        l <- list(lon = ou_m[[i, "lon"]],
                  lat = ou_m[[i, "lat"]],
                  map = ou_m[i, "geometry"])
        ou_l[[ou_m[[i, "ou_id"]]]] <- l
      }
      v$outlet_map_list <- ou_l
    }
    v$stream_render <- NULL
    df <- isolate(v$watershed_df)
    if (!is.null(df)) {
      ids <- as.numeric(tail(unlist(strsplit(
        df$out_id, "_"
      )), 1))
      ws_id_last <<- max(ids)
    }
    
    update_lc_prop()
    evapotran_df_edited <- update_evapotran_df_display()
    rain_df_edited <- update_rain_df_display()
    river_df_edited <- update_river_df_display()
    evapot_month_df_edited <- evapot_month_df_display()
    ground_par_df_edited <- ground_par_df_display()
    # update map id counter
    map_df <- isolate(v$lc_map_df)
    map_idc <- as.numeric(unlist(lapply(map_df$map_id, suffix)))
    map_id_counter <<- max(map_idc)
    # update numeric parameters UI
    update_numeric_par()
    print("parameters uploaded!")
  }

### CALCULATION ##############

  waterbalance_input_pars <- list(
    I_RainYearStart = 0,
    I_Rain_GenSeed = 200,
    I_Rain_IntensCoefVar = 0.3,
    I_Rain_IntensMean = 10,
    I_RainIntercDripRt = 10,
    I_RainMaxIntDripDur = 0.5,
    I_RainMultiplier = 1,
    
    I_MaxDynGWConst = 100,
    I_MaxInf = 700,
    I_MaxInfSSoil = 150,
    
    I_PowerInfiltRed = 3.5,
    
    
    I_SoilPropConst = 0,
    
    I_GWRelFracConst = 0.01,
    I_GWRelFracConst_is = 0,
    I_InitRelGW = 1,
    I_InitRelSoil = 1,
    
    I_AvailWaterConst = 250,
    I_EvapotransMethod = 1,
    I_InterceptEffectonTransp = 0.1,
    I_MaxDynGWConst = 100,
    I_PercFracMultiplier = 0.05,
    I_SoilSatMinFCConst = 100,
    I_SurfLossFrac = 0,
    
    I_SoilQflowFrac = 0.1
  )
  
  stream_input_pars <- list(
    I_Tortuosity = 0.6,
    I_RiverflowDispersalFactor = 0.6,
    I_RoutVeloc_m_per_s = 0.55
  )
  
  # Hydroelectric Power Plant (HEPP)
  
  lake_input_pars <- list(
    I_TotalArea = 0,
    I_WarmUpTime = 730,
    
    L_ResrDepth = 10000,
    L_HEPP_Active = 1,
    L_QmecsSanFlow = 3,
    L_LakeBottomElev = 160,
    L_LakeElevPreHEPP = 362.3,
    L_LakeLevelFullHEPP = 362.3,
    L_LakeLevelHalfHEPP = 361.8,
    L_LakeLevelNoHEPP = 359.5,
    L_LakeOverFlowFrac = 0.1,
    L_LakeOverFlPostHEPP = 362.6,
    L_LakeOverFlPow = 4,
    
    L_m3_per_kwh = 1.584,
    L_QmecsHEPP = 47.1,
    L_QmecsSanFlow = 3
  )
  

  #' Calculate water balance
  #'
  #' @param ws_rain_df amount of rain in day on each sub catchments, its a pair of ws_id and rain in mm
  #' @param subc_df parameters associated with each sub catchments
  #' @param subc_lc_df parameters associated with each sub catchments and land covers or vegetation types
  #'
  #' @return
  #' @export
  #'
  #' @examples
  water_balance <- function(ws_rain_df,
                            I_Daily_Evapotrans,
                            subc_df,
                            subc_lc_df,
                            input_pars,
                            debug = F) {
    inp <- input_pars
    subc_lc_df$I_RainPerDay <- NULL
    subc_lc_df <- merge(subc_lc_df, ws_rain_df, by = "ws_id", all.x = T)
    subc_df$I_RainPerDay <- NULL
    subc_df <- merge(subc_df, ws_rain_df, by = "ws_id", all.x = T)
    # I_DailyRainAmount[Subcatchement,VegClass] = I_RainPerDay[Subcatchement]*I_FracVegClassNow[VegClass,Subcatchement]*I_RelArea[Subcatchement]
    subc_lc_df$I_DailyRainAmount <- subc_lc_df$I_RainPerDay * subc_lc_df$I_FracVegClassNow * subc_lc_df$I_RelArea
    
    #### D_InterceptEvap ####
    # Amount of water can be intercepted by canopy per vegetation class and subcatchment
    # I_CanIntercAreaClass = I_InterceptClass[VegClass]*I_FracVegClassNow[VegClass,Subcatchement]*I_RelArea[Subcatchement]
    subc_lc_df$I_CanIntercAreaClass <- subc_lc_df$I_InterceptClass * subc_lc_df$I_FracVegClassNow * subc_lc_df$I_RelArea
    
    # based on Calder
    # D_InterceptEvap[VegClass,Subcatchement] = if I_CanIntercAreaClass[VegClass,Subcatchement]>0 then
    # I_CanIntercAreaClass[VegClass,Subcatchement]*(1-exp(-I_DailyRainAmount[Subcatchement,VegClass]/I_CanIntercAreaClass[VegClass,Subcatchement])) else 0
    subc_lc_df$D_InterceptEvap <- ifelse(
      subc_lc_df$I_CanIntercAreaClass > 0,
      pmax(0, subc_lc_df$I_CanIntercAreaClass * (
        1 - exp(
          -subc_lc_df$I_DailyRainAmount / subc_lc_df$I_CanIntercAreaClass
        )
      )),
      0
    )
    
    # Soil saturation per vegetation class and sub catchment
    # I_SoilSatClass = if I_SoilPropConst? = 1 then (I_AvailWaterConst+I_SoilSatMinFCConst)*I_FracVegClassNow[VegClass,Subcatchement]*I_RelArea[Subcatchement] else
    #   (I_SoilSatminFCSubNow[Subcatchement]+I_AvailWatClassNow[VegClass,Subcatchement])*I_FracVegClassNow[VegClass,Subcatchement]*I_RelArea[Subcatchement]
    if (inp$I_SoilPropConst == 1) {
      subc_lc_df$I_SoilSatClass <- (inp$I_AvailWaterConst + inp$I_SoilSatMinFCConst) * subc_lc_df$I_FracVegClassNow * subc_lc_df$I_RelArea
    } else {
      subc_lc_df$I_SoilSatClass <- (subc_lc_df$I_SoilSatminFCSubNow + subc_lc_df$I_AvailWatClassNow) * subc_lc_df$I_FracVegClassNow * subc_lc_df$I_RelArea
    }
    
    # If I_BD_BDRefVegNow[Subcatchement] > 0 then
    # I_MaxInf*I_RelArea[Subcatchement]*I_FracVegClassNow[VegClass,Subcatchement]*(0.7/I_BD_BDRefVegNow[Subcatchement])^I_PowerInfiltRed else 0
    subc_lc_df$I_MaxInfArea <- ifelse(
      subc_lc_df$I_BD_BDRefVegNow > 0,
      inp$I_MaxInf * subc_lc_df$I_FracVegClassNow * subc_lc_df$I_RelArea * (0.7 / subc_lc_df$I_BD_BDRefVegNow) ^ inp$I_PowerInfiltRed,
      0
    )
    
    # D_RainInterc[VegClass,Subcatchement] = if I_FracVegClassNow[VegClass,Subcatchement]>0 and I_RelArea[Subcatchement]>0 then
    # D_InterceptEvap[VegClass,Subcatchement]/(I_FracVegClassNow[VegClass,Subcatchement]*I_RelArea[Subcatchement]) else 0
    subc_lc_df$D_RainInterc <- ifelse(
      subc_lc_df$I_FracVegClassNow > 0 & subc_lc_df$I_RelArea > 0,
      subc_lc_df$D_InterceptEvap / (subc_lc_df$I_FracVegClassNow * subc_lc_df$I_RelArea),
      0
    )
    #sum D_RainInterc for all LC by subcatchment for each LC map
    D_RainInterc_sum_df <- aggregate(subc_lc_df$D_RainInterc,
                                     list(subc_lc_df$ws_id),
                                     sum,
                                     na.rm = T)
    colnames(D_RainInterc_sum_df) <- c("ws_id", "D_RainInterc")
    subc_df$D_RainInterc <- NULL
    subc_df <- merge(subc_df,
                     D_RainInterc_sum_df,
                     by = c("ws_id"),
                     all.x = T)
    
    # D_RainIntercDelay[Subcatchement] = min(I_RainMaxIntDripDur,ARRAYSUM(D_RainInterc[*,Subcatchement])/I_RainIntercDripRt)
    subc_df$D_RainIntercDelay <- pmin(inp$I_RainMaxIntDripDur,
                                      subc_df$D_RainInterc / inp$I_RainIntercDripRt)
    # I_RainDuration[Subcatchement] = (I_RainPerDay[Subcatchement]/I_Rain_IntensMean)*MIN(MAX (0,1-3*I_Rain_IntensCoefVar, NORMAL(1,I_Rain_IntensCoefVar,I_Rain_GenSeed+11250)), 1+3*I_Rain_IntensCoefVar)
    # subc_df$I_RainDuration_rcoef <- rnorm(nrow(subc_df), 1, inp$I_Rain_IntensCoefVar)
    subc_df$I_RainDuration <- (subc_df$I_RainPerDay / inp$I_Rain_IntensMean) * pmin(pmax(
      0,
      1 - 3 * inp$I_Rain_IntensCoefVar,
      # subc_df$I_RainDuration_rcoef
      rnorm(nrow(subc_df), 1, inp$I_Rain_IntensCoefVar)
    ),
    1 + 3 * inp$I_Rain_IntensCoefVar)

    # I_RainTimeAvForInf[Subcatchement] = min(24,I_RainDuration[Subcatchement]+D_RainIntercDelay[Subcatchement] )
    subc_df$I_RainTimeAvForInf <- pmin(24, subc_df$I_RainDuration + subc_df$D_RainIntercDelay)
    subc_lc_df[c("I_RainTimeAvForInf", "I_GWRelFrac")] <- NULL
    subc_lc_df <- merge(subc_lc_df,
                        subc_df[c("ws_id", "I_RainTimeAvForInf", "I_GWRelFrac")],
                        by = c("ws_id"),
                        all.x = T)
    
    #### D_Infiltration ####
    # D_Infiltration[VegClass,Subcatchement] = if L_Lake?[Subcatchement]=1 then 0 else
    #   min(min(I_SoilSatClass[VegClass,Subcatchement]-D_SoilWater[VegClass,Subcatchement],I_MaxInfArea[VegClass,Subcatchement]*I_RainTimeAvForInf[Subcatchement]/24),
    #       I_DailyRainAmount[Subcatchement,VegClass]-
    #         D_InterceptEvap[VegClass,Subcatchement])
    subc_lc_df$D_Infiltration_1 <- subc_lc_df$I_SoilSatClass - subc_lc_df$D_SoilWater
    subc_lc_df$D_Infiltration_2 <- subc_lc_df$I_MaxInfArea * subc_lc_df$I_RainTimeAvForInf / 24
    subc_lc_df$D_Infiltration_3 <- subc_lc_df$I_DailyRainAmount - subc_lc_df$D_InterceptEvap
    
    subc_lc_df$D_Infiltration <- ifelse(subc_lc_df$L_Lake == 1, 0, pmax(
      0,
      pmin(
        subc_lc_df$D_Infiltration_1,
        subc_lc_df$D_Infiltration_2,
        subc_lc_df$D_Infiltration_3
      )
    ))
    
    #I_MaxInfSubSAreaClass = I_MaxInfSSoil*I_RelArea[Subcatchement]*I_FracVegClassNow[VegClass,Subcatchement]
    subc_lc_df$I_MaxInfSubSAreaClass <- inp$I_MaxInfSSoil * subc_lc_df$I_FracVegClassNow * subc_lc_df$I_RelArea

    #sum vars for all LC by subcatchment for each LC map
    sum_lc_vars1 <- c(
      "I_MaxInfArea",
      "I_SoilSatClass",
      "D_SoilWater",
      "I_MaxInfSubSAreaClass",
      "D_InterceptEvap",
      "D_Infiltration",
      "I_DailyRainAmount"
    )
    sum_df <- aggregate(subc_lc_df[sum_lc_vars1], list(subc_lc_df$ws_id), sum, na.rm = T)
    colnames(sum_df) <- c("ws_id", sum_lc_vars1)
    subc_df[sum_lc_vars1] <- NULL
    subc_df <- merge(subc_df, sum_df, by = c("ws_id"), all.x = T)
    
    # D_DeepInfiltration[Subcatchement] = if L_Lake?[Subcatchement]=1 then 0 else min(
    #   min(
    #     min(ARRAYSUM(
    #       I_MaxInfArea[*,Subcatchement])*I_RainTimeAvForInf[Subcatchement]/24-
    #         ARRAYSUM(I_SoilSatClass[*,Subcatchement])+
    #         ARRAYSUM(D_SoilWater[*,Subcatchement]),
    #       ARRAYSUM(I_MaxInfSubSAreaClass[*,Subcatchement])),
    #     ARRAYSUM(I_DailyRainAmount[Subcatchement,*])-ARRAYSUM(D_InterceptEvap[*,Subcatchement])-
    #       ARRAYSUM(D_Infiltration[*,Subcatchement]))
    #   ,I_MaxDynGWArea[Subcatchement]-D_GWArea[Subcatchement])
    subc_df$D_DeepInfiltration_1 <- subc_df$I_MaxInfArea * subc_df$I_RainTimeAvForInf / 24 - subc_df$I_SoilSatClass + subc_df$D_SoilWater
    subc_df$D_DeepInfiltration_2 <- subc_df$I_MaxInfSubSAreaClass
    subc_df$D_DeepInfiltration_3 <- subc_df$I_DailyRainAmount - subc_df$D_InterceptEvap - subc_df$D_Infiltration
    subc_df$D_DeepInfiltration_4 <- subc_df$I_MaxDynGWArea - subc_df$D_GWArea
    # UNIFLOW only take POSITIVE value
    subc_df$D_DeepInfiltration <- ifelse(subc_df$L_Lake == 1, 0, pmax(
      0,
      pmin(
        subc_df$D_DeepInfiltration_1,
        subc_df$D_DeepInfiltration_2,
        subc_df$D_DeepInfiltration_3,
        subc_df$D_DeepInfiltration_4
      )
    ))
    
    # D_SurfaceFlow[Subcatchement] = if L_Lake?[Subcatchement]=1 then ARRAYSUM(I_DailyRainAmount[Subcatchement,*]) else
    #   ARRAYSUM(I_DailyRainAmount[Subcatchement,*])-
    #   ARRAYSUM(D_InterceptEvap[*,Subcatchement])-ARRAYSUM(D_Infiltration[*,Subcatchement])-D_DeepInfiltration[Subcatchement]
    subc_df$D_SurfaceFlow <- ifelse(
      subc_df$L_Lake == 1,
      subc_df$I_DailyRainAmount,
      pmax(
        0,
        subc_df$I_DailyRainAmount - subc_df$D_InterceptEvap - subc_df$D_Infiltration - subc_df$D_DeepInfiltration
      )
    )
    
    # D_GWaDisch[Subcatchement] = D_GWArea[Subcatchement]*I_GWRelFrac[Subcatchement]
    subc_df$D_GWaDisch <- pmax(0, subc_df$D_GWArea * subc_df$I_GWRelFrac)
    
    # Assign subcatchment variable to lc
    subc_vars <- c(
      "D_DeepInfiltration",
      "D_SurfaceFlow",
      "D_GWaDisch",
      "I_MaxDynGWArea",
      "D_GWArea"
    )
    subc_lc_df[subc_vars] <- NULL
    subc_lc_df <- merge(subc_lc_df,
                        subc_df[c("ws_id", subc_vars)],
                        by = c("ws_id"),
                        all.x = T)
    
    # D_SoilDischarge[VegClass,Subcatchement] = D_SoilQflowRelFrac[Subcatchement]*(D_SoilWater[VegClass,Subcatchement]-I_AvailWaterClass[VegClass,Subcatchement])
    subc_lc_df$D_SoilDischarge <- pmax(
      0,
      subc_lc_df$D_SoilQflowRelFrac * (subc_lc_df$D_SoilWater - subc_lc_df$I_AvailWaterClass)
    )
    
    # I_PotEvapTransp_[VegClass,Subcatchement] = if I_EvapotransMethod=1 then I_Evapotrans*I_MultiplierEvapoTrans[VegClass]*I_FracVegClassNow[VegClass,Subcatchement]*I_RelArea[Subcatchement] else I_Daily_Evapotrans[VegClass,Subcatchement]*I_MultiplierEvapoTrans[VegClass]*I_FracVegClassNow[VegClass,Subcatchement]*I_RelArea[Subcatchement]
    # if (inp$I_EvapotransMethod == 1) {
    #   subc_lc_df$I_PotEvapTransp_ <- inp$I_Evapotrans * subc_lc_df$I_MultiplierEvapoTrans * subc_lc_df$I_FracVegClassNow * subc_lc_df$I_RelArea
    # } else {
    #   subc_lc_df$I_PotEvapTransp_ <- inp$I_Daily_Evapotrans * subc_lc_df$I_MultiplierEvapoTrans * subc_lc_df$I_FracVegClassNow *
    #     subc_lc_df$I_RelArea
    # }
    subc_lc_df$I_PotEvapTransp_ <- I_Daily_Evapotrans * subc_lc_df$I_MultiplierEvapoTrans * subc_lc_df$I_FracVegClassNow *
      subc_lc_df$I_RelArea
    
    # D_RelWaterAv[VegClass,Subcatchement] = if I_AvailWaterClass[VegClass,Subcatchement] > 0 and I_RelDroughtFact[VegClass]>0 then min(1,D_SoilWater[VegClass,Subcatchement]/(I_RelDroughtFact[VegClass]*I_AvailWaterClass[VegClass,Subcatchement])) else 1
    subc_lc_df$D_RelWaterAv <- ifelse(
      subc_lc_df$I_AvailWaterClass > 0 &
        subc_lc_df$I_RelDroughtFact > 0,
      pmin(
        1,
        subc_lc_df$D_SoilWater / (subc_lc_df$I_RelDroughtFact * subc_lc_df$I_AvailWaterClass)
      ),
      1
    )
    
    # D_Irrigation[Subcatchement,VegClass] = if D_IrrigEfficiency[Subcatchement] > 0 then
    # min(D_GWArea[Subcatchement]*D_GWUseFacility?[Subcatchement,VegClass]*D_GW_Utilization_fraction[Subcatchement]*(1-D_RelWaterAv[VegClass,Subcatchement])/D_IrrigEfficiency[Subcatchement],I_PotEvapTransp_[VegClass,Subcatchement]) else 0
    subc_lc_df$D_Irrigation <- ifelse(
      subc_lc_df$D_IrrigEfficiency > 0,
      pmin(
        subc_lc_df$D_GWArea * subc_lc_df$D_GWUseFacility * subc_lc_df$D_GW_Utilization_fraction * (1 - subc_lc_df$D_RelWaterAv) /
          subc_lc_df$D_IrrigEfficiency,
        subc_lc_df$I_PotEvapTransp_
      ),
      0
    )
    
    # D_Percolation[VegClass,Subcatchement] = if I_AvailWaterClass[VegClass,Subcatchement]>0 then
    # min(I_MaxInfSubSAreaClass[VegClass,Subcatchement],
    # min(D_SoilWater[VegClass,Subcatchement]*I_PercFracMultiplier*I_GWRelFrac[Subcatchement],
    # I_MaxDynGWArea[Subcatchement]-D_GWArea[Subcatchement]))
    # - D_IrrigEfficiency[Subcatchement]*D_Irrigation[Subcatchement,VegClass] else - D_IrrigEfficiency[Subcatchement]*D_Irrigation[Subcatchement,VegClass]
    subc_lc_df$D_Percolation1 <- subc_lc_df$I_MaxInfSubSAreaClass
    subc_lc_df$D_Percolation2 <- subc_lc_df$D_SoilWater * inp$I_PercFracMultiplier * subc_lc_df$I_GWRelFrac
    subc_lc_df$D_Percolation3 <- subc_lc_df$I_MaxDynGWArea - subc_lc_df$D_GWArea
    subc_lc_df$D_Percolation_x <- subc_lc_df$D_IrrigEfficiency * subc_lc_df$D_Irrigation
    subc_lc_df$D_Percolation <- pmax(
      0,
      ifelse(
        subc_lc_df$I_AvailWaterClass > 0,
        pmin(
          subc_lc_df$I_MaxInfSubSAreaClass,
          subc_lc_df$D_SoilWater * inp$I_PercFracMultiplier * subc_lc_df$I_GWRelFrac,
          subc_lc_df$I_MaxDynGWArea - subc_lc_df$D_GWArea
        ) - subc_lc_df$D_IrrigEfficiency * subc_lc_df$D_Irrigation,
        -subc_lc_df$D_IrrigEfficiency * subc_lc_df$D_Irrigation
      )
    )
    
    # D_WaterEvapIrrigation[Subcatchement,VegClass] = If D_IrrigEfficiency[Subcatchement]>0 then
    # D_Irrigation[Subcatchement,VegClass]*(1-D_IrrigEfficiency[Subcatchement]) else 0
    subc_lc_df$D_WaterEvapIrrigation <- pmax(0,
                                             ifelse(
                                               subc_lc_df$D_IrrigEfficiency > 0,
                                               subc_lc_df$D_Irrigation *
                                                 (1 - subc_lc_df$D_IrrigEfficiency) ,
                                               0
                                             ))
    
    # D_ActEvapTransp[VegClass,Subcatchement] = if L_Lake?[Subcatchement]=1 then 0 else
    #   ((I_PotEvapTransp_[VegClass,Subcatchement]-I_InterceptEffectonTransp*D_InterceptEvap[VegClass,Subcatchement])) *D_RelWaterAv[VegClass,Subcatchement]
    subc_lc_df$D_ActEvapTransp <- ifelse(subc_lc_df$L_Lake == 1, 0, ((
      subc_lc_df$I_PotEvapTransp_ - inp$I_InterceptEffectonTransp * subc_lc_df$D_InterceptEvap
    )
    ) * subc_lc_df$D_RelWaterAv)
    
    #### Stock ###############################
    # D_CumNegRain[Subcatchement](t) = D_CumNegRain[Subcatchement](t - dt) + (- D_InterceptEvap[VegClass,Subcatchement] - D_Infiltration[VegClass,Subcatchement] - D_DeepInfiltration[Subcatchement] - D_SurfaceFlow[Subcatchement]) * dt
    subc_lc_df$D_CumNegRain <- subc_lc_df$D_CumNegRain - subc_lc_df$D_InterceptEvap - subc_lc_df$D_Infiltration - subc_lc_df$D_DeepInfiltration - subc_lc_df$D_SurfaceFlow
    # D_CumEvapTranspClass[VegClass,Subcatchement](t) = D_CumEvapTranspClass[VegClass,Subcatchement](t - dt) + (D_ActEvapTransp[VegClass,Subcatchement] + D_InterceptEvap[VegClass,Subcatchement]) * dt
    subc_lc_df$D_CumEvapTranspClass <- subc_lc_df$D_CumEvapTranspClass +  subc_lc_df$D_ActEvapTransp +  subc_lc_df$D_InterceptEvap
    # D_SoilWater[VegClass,Subcatchement](t) = D_SoilWater[VegClass,Subcatchement](t - dt) + (D_Infiltration[VegClass,Subcatchement] - D_ActEvapTransp[VegClass,Subcatchement] - D_Percolation[VegClass,Subcatchement] - D_SoilDischarge[VegClass,Subcatchement]) * dt
    subc_lc_df$D_SoilWater <- subc_lc_df$D_SoilWater + subc_lc_df$D_Infiltration - subc_lc_df$D_ActEvapTransp - subc_lc_df$D_Percolation - subc_lc_df$D_SoilDischarge
    # D_EvapTranspClass[Subcatchement,VegClass](t) = D_EvapTranspClass[Subcatchement,VegClass](t - dt) + (D_WaterEvapIrrigation[Subcatchement,VegClass]) * dt
    subc_lc_df$D_EvapTranspClass <- subc_lc_df$D_EvapTranspClass + subc_lc_df$D_WaterEvapIrrigation
    
    #sum vars for all LC by subcatchment for each LC map
    sum_lc_vars2 <- c("D_Percolation", "D_WaterEvapIrrigation")
    sum_df <- aggregate(subc_lc_df[sum_lc_vars2], list(subc_lc_df$ws_id), sum, na.rm = T)
    colnames(sum_df) <- c("ws_id", sum_lc_vars2)
    subc_df[sum_lc_vars2] <- NULL
    subc_df <- merge(subc_df, sum_df, by = c("ws_id"), all.x = T)
    # D_GWArea[Subcatchement](t) = D_GWArea[Subcatchement](t - dt) + (D_Percolation[VegClass,Subcatchement] + D_DeepInfiltration[Subcatchement] - D_GWaDisch[Subcatchement] - D_WaterEvapIrrigation[Subcatchement,VegClass]) * dt
    subc_df$D_GWArea <- subc_df$D_GWArea + subc_df$D_Percolation + subc_df$D_DeepInfiltration - subc_df$D_GWaDisch - subc_df$D_WaterEvapIrrigation
    return(list(subc_lc_df = subc_lc_df, subc_df = subc_df))
  }
  
  ### STREAM NETWORK #############################
  
  stream_network <- function(time,
                             I_Evapotrans,
                             I_DebitTime,
                             subc_df,
                             subc_lc_df,
                             stream_pars,
                             lake_pars,
                             stock) {
    # I_Warmedup = if time = int(I_WarmUpTime) then 1 else 0
    I_Warmedup <- ifelse(time == lake_pars$I_WarmUpTime, 1 , 0)
    
    # D_EvaporReservoir[Subcatchement] = I_Evapotrans*ARRAYSUM(L_Lake?[*])
    subc_df$D_EvaporReservoir <- I_Evapotrans * sum(subc_df$L_Lake)
    
    # D_Influx_to_Resr[Subcatchement] = if I_DaminThisStream?[Subcatchement] = 1 then D_GWaDisch[Subcatchement]+ARRAYSUM(D_SoilDischarge[*,Subcatchement])+D_SurfaceFlow[Subcatchement] else 0
    subc_df$D_Influx_to_Resr <- ifelse(
      subc_df$I_DaminThisStream == 1,
      subc_df$D_GWaDisch + subc_df$D_SoilDischarge + subc_df$D_SurfaceFlow,
      0
    )
    
    # D_SubcResVol[Subcatchement](t) = D_SubcResVol[Subcatchement](t - dt) + (D_Influx_to_Resr[Subcatchement] - D_EvaporReservoir[Subcatchement] - D_SubCResOutflow[Subcatchement]) * dt
    subc_df$D_SubcResVol <- subc_df$D_SubcResVol
    + (subc_df$D_Influx_to_Resr - subc_df$D_EvaporReservoir - subc_df$D_SubCResOutflow)
    
    D_SubCResUseFrac_graph <- c(
      0.01,
      0.005,
      0.02,
      0.03,
      0.205,
      0.325,
      0.36,
      0.335,
      0.315,
      0.195,
      0.02,
      0.01,
      0.015,
      0.015,
      0.015,
      0.015,
      0.015,
      0.015,
      0.015,
      0.015
    )
    D_SubCResUseFrac <- D_SubCResUseFrac_graph[1] #?
    
    # D_SubCResOutflow[Subcatchement] = if D_SubcResVol[Subcatchement]/D_ReservoirVol[Subcatchement] > 1 then
    # D_SubcResVol[Subcatchement]-D_ReservoirVol[Subcatchement] else D_SubCResUseFrac*D_SubcResVol[Subcatchement]
    subc_df$D_SubCResOutflow <- ifelse(
      subc_df$D_SubcResVol / subc_df$D_ReservoirVol > 1,
      subc_df$D_SubcResVol - subc_df$D_ReservoirVol,
      D_SubCResUseFrac * subc_df$D_SubcResVol
    )
    
    #sum vars for all LC by subcatchment for each LC map
    sum_lc_vars <- c("D_SoilDischarge")
    sum_df <- aggregate(subc_lc_df[sum_lc_vars], list(subc_lc_df$ws_id), sum, na.rm = T)
    colnames(sum_df) <- c("ws_id", sum_lc_vars)
    subc_df[sum_lc_vars] <- NULL
    subc_df <- merge(subc_df, sum_df, by = c("ws_id"), all.x = T)
    
    # D_TotalStreamInflow[Subcatchement,ObsPoint] = (D_SurfaceFlow[Subcatchement]+D_GWaDisch[Subcatchement]*(1-D_FracGWtoLake[Subcatchement])
    # +ARRAYSUM(D_SoilDischarge[*,Subcatchement]))+D_SubCResOutflow[Subcatchement]*(1-I_DaminThisStream?[Subcatchement])
    subc_df$D_TotalStreamInflow <- subc_df$D_SurfaceFlow + subc_df$D_GWaDisch * (1 - subc_df$D_FracGWtoLake) + subc_df$D_SoilDischarge + subc_df$D_SubCResOutflow * (1 - subc_df$I_DaminThisStream)
    
    # I_RivFlowTimeNow[Subcatchement] = if I_Flag1 = 1 then I_RivFlowTime1[Subcatchement]+(I_RivFlowTime2[Subcatchement]-I_RivFlowTime1[Subcatchement])*(int(I_Simulation_Time/365)-I_InputDataYears[Start])/(I_InputDataYears[Trans1]-I_InputDataYears[Start]) ELSE
    # if I_Flag2 = 1 then I_RivFlowTime2[Subcatchement]+(I_RivFlowTime3[Subcatchement]-I_RivFlowTime2[Subcatchement])*(int(I_Simulation_Time/365)-I_InputDataYears[Trans1])/(I_InputDataYears[Trans2]-I_InputDataYears[Trans1]) else
    #   I_RivFlowTime3[Subcatchement]+(I_RivFlowTime4[Subcatchement]-I_RivFlowTime3[Subcatchement])*(int(I_Simulation_Time/365)-I_InputDataYears[Trans2])/(I_InputDataYears[End]-I_InputDataYears[Trans2])
    subc_df$I_RivFlowTimeNow <- 1 #TODO:?
    # D_RoutingTime[Subcatchement,ObsPoint] = I_RoutingDistance[Subcatchement,ObsPoint]/(I_RivFlowTimeNow[Subcatchement]*I_RoutVeloc_m_per_s*3.6*24*I_Tortuosity)
    subc_df$D_RoutingTime <- subc_df$I_RoutingDistance / (
      subc_df$I_RivFlowTimeNow *
        stream_pars$I_RoutVeloc_m_per_s * 3.6 * 24 * stream_pars$I_Tortuosity
    )
    
    # I_ReleaseFrac[Subcatchement,ObsPoint] = if D_RoutingTime[Subcatchement,ObsPoint]> 0 then
    # min(1,I_RiverflowDispersalFactor/D_RoutingTime[Subcatchement,ObsPoint]) else 1
    subc_df$I_ReleaseFrac <- ifelse(
      subc_df$D_RoutingTime > 0,
      pmin(
        1,
        stream_pars$I_RiverflowDispersalFactor / subc_df$D_RoutingTime
      ),
      1
    )
    
    # D_RivLakeSameDay[Subcatchement,ObsPoint] = if D_RoutingTime[Subcatchement,ObsPoint]>=0 and D_RoutingTime[Subcatchement,ObsPoint]<1  then (D_FeedingIntoLake?[Subcatchement])*D_TotalStreamInflow[Subcatchement,ObsPoint]*(I_ReleaseFrac[Subcatchement,ObsPoint]) else 0
    subc_df$D_RivLakeSameDay <- ifelse(
      subc_df$D_RoutingTime >= 0 & subc_df$D_RoutingTime < 1,
      subc_df$D_FeedingIntoLake * subc_df$D_TotalStreamInflow * subc_df$I_ReleaseFrac,
      0
    )
    # D_RivInflLake[Subcatchement,ObsPoint] = I_ReleaseFrac[Subcatchement,InflowLake]*D_TotRiverFlowNoDelay[Subcatchement,InflowLake]*D_FeedingIntoLake?[Subcatchement]
    subc_df$D_RivInflLake <- subc_df$I_ReleaseFrac * subc_df$D_TotRiverFlowNoDelay * subc_df$D_FeedingIntoLake
    # D_RiverFlowtoLake = ARRAYSUM(D_RivLakeSameDay[*,InflowLake])+ARRAYSUM(D_RivInflLake[*,InflowLake])
    D_RiverFlowtoLake <- sum(subc_df$D_RivLakeSameDay, na.rm = T) + sum(subc_df$D_RivInflLake, na.rm = T)
    # D_GWLakeSub[Subcatchement] = D_FracGWtoLake[Subcatchement]*D_GWaDisch[Subcatchement]
    subc_df$D_GWLakeSub <- subc_df$D_FracGWtoLake * subc_df$D_GWaDisch
    # D_GWtoLake = ARRAYSUM(D_GWLakeSub[*])
    D_GWtoLake <- sum(subc_df$D_GWLakeSub, na.rm = T)
    # L_InFlowtoLake =  D_RiverFlowtoLake+D_GWtoLake
    L_InFlowtoLake <- D_RiverFlowtoLake + D_GWtoLake
    
    # L_LakeArea[Subcatchement] = if L_Lake?[Subcatchement]=1 then L_Lake?[Subcatchement]*I_RelArea[Subcatchement] else 0
    subc_df$L_LakeArea <- ifelse(subc_df$L_Lake == 1, subc_df$L_Lake * subc_df$I_RelArea, 0)
    sum_L_LakeArea <- sum(subc_df$L_LakeArea, na.rm = T)
    
    # L_OutflTrVolPreHEPP = 1000*(L_LakeElevPreHEPP-L_LakeBottomElev)*arraysum(L_LakeArea[*])
    L_OutflTrVolPreHEPP <- 1000 * (lake_pars$L_LakeElevPreHEPP - lake_pars$L_LakeBottomElev) *
      sum_L_LakeArea
    # L_OutflTrVoPostHEPP = 1000*(L_LakeOverFlPostHEPP-L_LakeBottomElev)*arraysum(L_LakeArea[*])
    L_OutflTrVoPostHEPP <- 1000 * (lake_pars$L_LakeOverFlPostHEPP - lake_pars$L_LakeBottomElev) *
      sum_L_LakeArea
    # INIT L_LakeVol = L_OutflTrVoPostHEPP*L_HEPP_Active?+(1-L_HEPP_Active?)*L_OutflTrVolPreHEPP
    L_LakeVol <- L_OutflTrVoPostHEPP * lake_pars$L_HEPP_Active + (1 - lake_pars$L_HEPP_Active) * L_OutflTrVolPreHEPP
    # L_LakeLevel = if ARRAYSUM(L_LakeArea[*])>0 then L_LakeVol/(1000*ARRAYSUM(L_LakeArea[*])) + L_LakeBottomElev else 0
    L_LakeLevel <- ifelse(sum_L_LakeArea > 0,
                          L_LakeVol / (1000 * sum_L_LakeArea) + L_LakeBottomElev,
                          0)
    # L_Lakelevelexcess = L_LakeLevel-(1-L_HEPP_Active?)*L_LakeElevPreHEPP-L_HEPP_Active?*L_LakeOverFlPostHEPP
    L_Lakelevelexcess = L_LakeLevel - (1 - lake_pars$L_HEPP_Active) * lake_pars$L_LakeElevPreHEPP -
      lake_pars$L_HEPP_Active * lake_pars$L_LakeOverFlPostHEPP
    
    # L_SanitaryFlow = L_QmecsSanFlow* 3600*24/I_TotalArea*10^-3
    L_SanitaryFlow <- lake_pars$L_QmecsSanFlow * 3600 * 24 / (lake_pars$I_TotalArea *
                                                                10 ^ (-3))
    # L_RivOutFlow = max(L_HEPP_Active?*L_SanitaryFlow,
    # (L_LakeVol-(L_OutflTrVoPostHEPP*L_HEPP_Active?)-L_OutflTrVolPreHEPP*(1-L_HEPP_Active?))*
    # (L_LakeOverFlowFrac)*(1+L_Lakelevelexcess^L_LakeOverFlPow))
    L_RivOutFlow <- max(
      lake_pars$L_HEPP_Active * L_SanitaryFlow,
      (
        L_LakeVol - (L_OutflTrVoPostHEPP * lake_pars$L_HEPP_Active) - L_OutflTrVolPreHEPP *
          (1 - lake_pars$L_HEPP_Active)
      ) * (lake_pars$L_LakeOverFlowFrac) * (1 + L_Lakelevelexcess ^ lake_pars$L_LakeOverFlPow)
    )
    
    # D_SurfFlowRiver is D_SurfFlowObsPoint in delayed time?
    subc_df$D_SurfFlowRiver <- 0
    
    # D_SurfFlowRiver[Subcatchement,ObsPoint] = CONVEYOR OUTFLOW
    # TRANSIT TIME = if D_RoutingTime[Subcatchement,ObsPoint]>1 then
    # D_RoutingTime[Subcatchement,ObsPoint] else 0
    
    # D_SurfFlowObsPoint[Subcatchement,ObsPoint] = if D_RoutingTime[Subcatchement,ObsPoint]>=1 then D_TotalStreamInflow[Subcatchement,ObsPoint] else 0
    subc_df$D_SurfFlowObsPoint <- ifelse(subc_df$D_RoutingTime >= 1, subc_df$D_TotalStreamInflow, 0)
    # D_StreamsSurfQ[Subcatchement,ObsPoint](t) = D_StreamsSurfQ[Subcatchement,ObsPoint](t - dt) + (D_SurfFlowObsPoint[Subcatchement,ObsPoint] - D_SurfFlowRiver[Subcatchement,ObsPoint]) * dt
    subc_df$D_StreamsSurfQ <- subc_df$D_StreamsSurfQ + (subc_df$D_SurfFlowObsPoint - subc_df$D_SurfFlowRiver)
    # D_DirectSurfFkowObsPoint[Subcatchement,ObsPoint] = if D_RoutingTime[Subcatchement,ObsPoint]>=0 and D_RoutingTime[Subcatchement,ObsPoint]<1  then D_TotalStreamInflow[Subcatchement,ObsPoint]*(1-I_ReleaseFrac[Subcatchement,ObsPoint])else 0
    subc_df$D_DirectSurfFkowObsPoint <- ifelse(
      subc_df$D_RoutingTime >= 0 & subc_df$D_RoutingTime < 1,
      subc_df$D_TotalStreamInflow *
        (1 - subc_df$I_ReleaseFrac),
      0
    )
    
    # D_TotRiverFlowNoDelay[Subcatchement,ObsPoint](t) = D_TotRiverFlowNoDelay[Subcatchement,ObsPoint](t - dt) + (D_SurfFlowRiver[Subcatchement,ObsPoint] + D_DirectSurfFkowObsPoint[Subcatchement,ObsPoint] - D_RiverDelay[Subcatchement,ObsPoint] - D_RivInflLake[Subcatchement,ObsPoint]) * dt
    subc_df$D_TotRiverFlowNoDelay <- subc_df$D_TotRiverFlowNoDelay + subc_df$D_SurfFlowRiver
    + subc_df$D_DirectSurfFkowObsPoint - subc_df$D_RiverDelay - subc_df$D_RivInflLake
    
    # O_EvapoTransAcc = if arraysum(I_PotEvapTransp_[*,*])>0 then (ARRAYSUM(D_ActEvapTransp[*,*])+ARRAYSUM(D_InterceptEvap[*,*]))*I_WarmEdUp? else 0
    O_EvapoTransAcc <- ifelse(sum(subc_lc_df$I_PotEvapTransp_, na.rm = T) > 0,
                              (
                                sum(subc_lc_df$D_ActEvapTransp, na.rm = T) + sum(subc_lc_df$D_InterceptEvap, na.rm = T)
                              ) * stock$I_WarmEdUp,
                              0)
    # I_ContrSubcArea[Subcatchement] = I_RelArea[Subcatchement]*I_SubcContr?[Subcatchement]
    subc_df$I_ContrSubcArea <- subc_df$I_RelArea * subc_df$I_SubcContr
    I_ContrSubcArea_sum <- sum(subc_df$I_ContrSubcArea, na.rm = T)
    I_RFlowDataQmecs <- I_DebitTime
    # I_RFlowdata_mmday = if ARRAYSUM(I_ContrSubcArea[*])>0 then (I_RFlowDataQmecs*24*3600*10^3)/(ARRAYSUM(I_ContrSubcArea[*])*I_TotalArea
    I_RFlowdata_mmday <- ifelse(
      I_ContrSubcArea_sum > 0,
      (I_RFlowDataQmecs * 24 * 3600 * 10 ^ 3) / (I_ContrSubcArea_sum * lake_pars$I_TotalArea),
      0
    )

    # L_HEPP_Daily_Dem = L_QmecsHEPP*3600*24/I_TotalArea*10^-3
    L_HEPP_Daily_Dem <- lake_pars$L_QmecsHEPP * 3600 * 24 / lake_pars$I_TotalArea *
      10 ^ -3
    
    # L_HEPP_Outflow = IF L_LakeLevel>L_LakeLevelFullHEPP then L_HEPP_Daily_Dem else
    # IF L_LakeLevel>L_LakeLevelNoHEPP then L_HEPP_Daily_Dem*0.5*(1+
    # max(0,(L_LakeLevel-L_LakeLevelHalfHEPP)/(L_LakeLevelFullHEPP-L_LakeLevelHalfHEPP)))
    # else 0
    L_HEPP_Outflow <- ifelse(
      L_LakeLevel > lake_pars$L_LakeLevelFullHEPP,
      L_HEPP_Daily_Dem,
      ifelse(
        L_LakeLevel > lake_pars$L_LakeLevelNoHEPP,
        L_HEPP_Daily_Dem * 0.5 * (1 + max(
          0,
          (L_LakeLevel - lake_pars$L_LakeLevelHalfHEPP) / (
            lake_pars$L_LakeLevelFullHEPP - lake_pars$L_LakeLevelHalfHEPP
          )
        )),
        0
      )
    )
    # L_HEPPWatUseFlow = if L_HEPP_Active?=1 then L_HEPP_Outflow else 0
    L_HEPPWatUseFlow <- ifelse(lake_pars$L_HEPP_Active == 1, L_HEPP_Outflow, 0)
    # L_HEPP_Kwh = 1000*I_TotalArea*L_HEPPWatUseFlow/L_m3_per_kwh
    L_HEPP_Kwh <- 1000 * lake_pars$I_TotalArea * L_HEPPWatUseFlow / lake_pars$L_m3_per_kwh
    # O_LastYearHEPP = if O_LastYHepp>0 then (O_ThisYHepp-O_LastYHepp)/(365*L_HEPP_Daily_Dem) else 0
    O_LastYearHEPP <- ifelse(
      stock$O_LastYHepp > 0,
      (stock$O_ThisYHepp - stock$O_LastYHepp) / (365 * L_HEPP_Daily_Dem),
      0
    )
    # O_BYP = if O_LastYearHEPP>0 and O_LastYearHEPP>O_BestYyHEPP then -O_BestYyHEPP+O_LastYearHEPP else 0
    O_BYP <- ifelse(O_LastYearHEPP > 0 &&
                      O_LastYearHEPP > O_BestYyHEPP,
                    -O_BestYyHEPP + O_LastYearHEPP,
                    0)
    # Yearly_Tick = if I_WarmEdUp? = 1 and mod(time,365) = 0 then 1 else 0
    Yearly_Tick <- ifelse(stock$I_WarmEdUp == 1 &&
                            (time %% 365) == 0, 1, 0)
    # O_HeppUseF0 = Yearly_Tick*L_CumHEPPUse
    O_HeppUseF0 <- Yearly_Tick * stock$L_CumHEPPUse
    # O_HeppUseF1 = Yearly_Tick*O_ThisYHepp
    O_HeppUseF1 <- Yearly_Tick * stock$O_ThisYHepp
    # O_HeppUseF2 = Yearly_Tick*O_LastYHepp
    O_HeppUseF2 <- Yearly_Tick * stock$O_LastYHepp
    # I_WUcorrection = if time = int(I_WarmUpTime+1) then 1 else 0
    I_WUcorrection <- ifelse(time == (lake_pars$I_WarmUpTime + 1), 1, 0)
    # O_Reset? = if I_Warmedup = 1 or I_WUcorrection = 1  then 1 else 0
    O_Reset <- ifelse(I_Warmedup == 1 && I_WUcorrection == 1, 1, 0)
    # L_RestartH = O_Reset?*L_CumHEPPUse
    L_RestartH <- O_Reset * stock$L_CumHEPPUse
    # O_WYP = if O_LastYearHEPP>0 and O_LastYearHEPP<O_WorstYHEPP then -O_WorstYHEPP+O_LastYearHEPP else 0
    O_WYP <- ifelse(O_LastYearHEPP > 0 &&
                      O_LastYearHEPP < O_WorstYHEPP,
                    -O_WorstYHEPP + O_LastYearHEPP,
                    0)
    # O_SurfQFlowAcc = ARRAYSUM(D_SurfaceFlow[*])*I_WarmEdUp?
    O_SurfQFlowAcc <- sum(subc_df$D_SurfaceFlow, na.rm = T) * stock$I_WarmEdUp
    # O_BaseFlowAcc = ARRAYSUM(D_GWaDisch[*])*I_WarmEdUp?
    O_BaseFlowAcc = sum(subc_df$D_GWaDisch, na.rm = T) * stock$I_WarmEdUp
    # O_SoilQFlowAcc = ARRAYSUM(D_SoilDischarge[*,*])*I_WarmEdUp?
    O_SoilQFlowAcc <- sum(subc_lc_df$D_SoilDischarge, na.rm = T) * stock$I_WarmEdUp
    # O_RainAcc = ARRAYSUM(I_DailyRainAmount[*,*])*I_WarmEdUp?
    O_RainAcc <- sum(subc_lc_df$I_DailyRainAmount, na.rm = T) * stock$I_WarmEdUp
    # O_PercAcc = ARRAYSUM(D_Percolation[*,*])*I_WarmEdUp?
    O_PercAcc <- sum(subc_lc_df$D_Percolation, na.rm = T) * stock$I_WarmEdUp
    # O_DeepInfAcc = ARRAYSUM(D_DeepInfiltration[*])*I_WarmEdUp?
    O_DeepInfAcc <- sum(subc_df$D_DeepInfiltration, na.rm = T) * stock$I_WarmEdUp
    # O_IntercAcc = ARRAYSUM(D_InterceptEvap[*,*])*I_WarmEdUp?
    O_IntercAcc <- sum(subc_lc_df$D_InterceptEvap, na.rm = T) * stock$I_WarmEdUp
    # O_InfAcc = ARRAYSUM(D_Infiltration[*,*])*I_WarmEdUp?
    O_InfAcc <- sum(subc_lc_df$D_Infiltration, na.rm = T) * stock$I_WarmEdUp
    # O_SoilWaterTot = ARRAYSUM(D_SoilWater[*,*])
    O_SoilWaterTot <- sum(subc_lc_df$D_SoilWater, na.rm = T)
    # O_Ch_EvapoTran[MeasurePeriod] = IF TIME = int(O_StartMDay[MeasurePeriod]) THEN ARRAYSUM(D_CumEvapTranspClass[*,*]) ELSE 0
    O_Ch_EvapoTran <- sum(subc_lc_df$D_CumEvapTranspClass, na.rm = T)
    # O_TotStreamFlow = O_CumBaseFlow+O_CumSoilQFlow+O_CumSurfQFlow
    O_TotStreamFlow <- stock$O_CumBaseFlow + stock$O_CumSoilQFlow + stock$O_CumSurfQFlow
    # O_FrBaseFlow = if O_TotStreamFlow > 0 then O_CumBaseFlow/O_TotStreamFlow   else 0
    O_FrBaseFlow <- ifelse(O_TotStreamFlow > 0, stock$O_CumBaseFlow / O_TotStreamFlow, 0)
    # O_FrSoilQuickFlow = if O_TotStreamFlow > 0 then O_CumSoilQFlow/O_TotStreamFlow   else 0
    O_FrSoilQuickFlow <- ifelse(O_TotStreamFlow > 0, stock$O_CumSoilQFlow /
                                  O_TotStreamFlow, 0)
    # O_FrSurfQuickflow = if O_TotStreamFlow > 0 then O_CumSurfQFlow/O_TotStreamFlow   else 0
    O_FrSurfQuickflow <- ifelse(O_TotStreamFlow > 0, stock$O_CumSurfQFlow /
                                  O_TotStreamFlow, 0)
    
    #### Stock #######################
    # I_WarmEdUp?(t) = I_WarmEdUp?(t - dt) + (I_Warmedup) * dt
    stock$I_WarmEdUp <- stock$I_WarmEdUp + I_Warmedup
    # O_CumRain(t) = O_CumRain(t - dt) + (O_RainAcc) * dt
    stock$O_CumRain <- stock$O_CumRain + O_RainAcc
    # O_CumPercolation(t) = O_CumPercolation(t - dt) + (O_PercAcc) * dt
    stock$O_CumPercolation <- stock$O_CumPercolation + O_PercAcc
    # O_CumDeepInfilt(t) = O_CumDeepInfilt(t - dt) + (O_DeepInfAcc) * dt
    stock$O_CumDeepInfilt <- stock$O_CumDeepInfilt + O_DeepInfAcc
    # O_BestYyHEPP(t) = O_BestYyHEPP(t - dt) + (O_BYP) * dt
    stock$O_BestYyHEPP <- stock$O_BestYyHEPP + O_BYP
    # O_LastYHepp(t) = O_LastYHepp(t - dt) + (O_HeppUseF1 - O_HeppUseF2) * dt
    stock$O_LastYHepp <- stock$O_LastYHepp + (O_HeppUseF1 - O_HeppUseF2)
    # O_ThisYHepp(t) = O_ThisYHepp(t - dt) + (O_HeppUseF0 - O_HeppUseF1) * dt
    stock$O_ThisYHepp <- stock$O_ThisYHepp + (O_HeppUseF0 - O_HeppUseF1)
    # L_CumHEPPUse(t) = L_CumHEPPUse(t - dt) + (L_HEPPWatUseFlow - L_RestartH) * dt
    stock$L_CumHEPPUse <- stock$L_CumHEPPUse + (L_HEPPWatUseFlow - L_RestartH)
    # O_WorstYHEPP(t) = O_WorstYHEPP(t - dt) + (O_WYP) * dt
    stock$O_WorstYHEPP <- stock$O_WorstYHEPP + O_WYP
    # O_CumSurfQFlow(t) = O_CumSurfQFlow(t - dt) + (O_SurfQFlowAcc) * dt
    stock$O_CumSurfQFlow <- stock$O_CumSurfQFlow + O_SurfQFlowAcc
    # O_CumSoilQFlow(t) = O_CumSoilQFlow(t - dt) + (O_SoilQFlowAcc) * dt
    stock$O_CumSoilQFlow <- stock$O_CumSoilQFlow + O_SoilQFlowAcc
    # O_CumBaseFlow(t) = O_CumBaseFlow(t - dt) + (O_BaseFlowAcc) * dt
    stock$O_CumBaseFlow <- stock$O_CumBaseFlow  + O_BaseFlowAcc
    
    ### Output ######################
    
    stream_vars <- list()
    stream_vars$L_InFlowtoLake <- L_InFlowtoLake
    stream_vars$L_RivOutFlow <- L_RivOutFlow
    stream_vars$O_EvapoTransAcc <- O_EvapoTransAcc
    stream_vars$I_RFlowdata_mmday <- I_RFlowdata_mmday
    stream_vars$L_SanitaryFlow <- L_SanitaryFlow
    stream_vars$L_OutflTrVolPreHEPP <- L_OutflTrVolPreHEPP
    stream_vars$L_Lakelevelexcess <- L_Lakelevelexcess
    stream_vars$L_OutflTrVoPostHEPP <- L_OutflTrVoPostHEPP
    stream_vars$L_LakeVol <- L_LakeVol
    stream_vars$L_OutflTrVoPostHEPP <- L_OutflTrVoPostHEPP
    stream_vars$I_RFlowDataQmecs <- I_RFlowDataQmecs
    stream_vars$L_LakeLevel <- L_LakeLevel
    stream_vars$L_HEPP_Outflow <- L_HEPP_Outflow
    stream_vars$L_HEPPWatUseFlow <- L_HEPPWatUseFlow
    stream_vars$L_HEPP_Kwh <- L_HEPP_Kwh
    
    stream_vars$O_SurfQFlowAcc <- O_SurfQFlowAcc
    stream_vars$O_BaseFlowAcc <- O_BaseFlowAcc
    stream_vars$O_SoilQFlowAcc <- O_SoilQFlowAcc
    stream_vars$O_TotStreamFlow <- O_TotStreamFlow
    stream_vars$O_FrBaseFlow <- O_FrBaseFlow
    stream_vars$O_FrSoilQuickFlow <- O_FrSoilQuickFlow
    stream_vars$O_FrSurfQuickflow <- O_FrSurfQuickflow
    stream_vars$O_RainAcc <- O_RainAcc
    stream_vars$O_PercAcc <- O_PercAcc
    stream_vars$O_DeepInfAcc <- O_DeepInfAcc
    
    stream_vars$O_IntercAcc <- O_IntercAcc
    stream_vars$O_InfAcc <- O_InfAcc
    stream_vars$O_SoilWaterTot <- O_SoilWaterTot
    stream_vars$O_Ch_EvapoTran <- O_Ch_EvapoTran
    
    return(
      list(
        subc_lc_df = subc_lc_df,
        subc_df = subc_df,
        stream_vars = stream_vars,
        stock = stock
      )
    )
  }
  
  get_rain_map <- function() {
    
  }
  
  get_closest_index <- function(x, v) {
    if (x >= max(v))
      return(length(v))
    if (x <= min(v))
      return(1)
    l <- max(which(v <= x))
    h <- min(which(v >= x))
    return(unique(c(l, h)))
  }
  
  get_lc_vars_of_year <- function(year, subc_lc_df, lc_map_df, id_cols) {
    map_years <- sort(lc_map_df$year)
    yr_idx <- get_closest_index(year, map_years)
    map_ref <- lc_map_df[lc_map_df$year %in% map_years[yr_idx], "map_id"]
    if (length(map_ref) == 1) {
      dd <- subc_lc_df[subc_lc_df$map_id == map_ref, ]
      dd$map_id <- NULL
      return(dd)
    }
    yr1 <- map_years[yr_idx[1]]
    yr2 <- map_years[yr_idx[2]]
    f1 <- (yr2 - year) / (yr2 - yr1)
    f2 <- (year - yr1) / (yr2 - yr1)
    
    d1 <- subc_lc_df[subc_lc_df$map_id == map_ref[1], ]
    d2 <- subc_lc_df[subc_lc_df$map_id == map_ref[2], ]
    d1$map_id <- NULL
    d2$map_id <- NULL
    # id_cols <- c("ws_id", "lc_id")
    vars <- names(d1)
    vars <- vars[!vars %in% id_cols]
    vars1 <- paste0(vars, "_1")
    vars2 <- paste0(vars, "_2")
    names(d1) <- c(id_cols, vars1)
    names(d2) <- c(id_cols, vars2)
    dd <- merge(d1, d2, by = id_cols, all = T)
    dd[is.na(dd)] <- 0
    dd[vars] <- dd[vars1] * f1 + dd[vars2] * f2
    return(dd[c(id_cols, vars)])
  }
  
  get_map_subc_lc_df <- function() {
    map_subc_lc_df <- v$subc_lc_df
    map_subc_lc_df$I_FracVegClassNow <- map_subc_lc_df$area_ratio
    map_subc_lc_df$I_SoilSatminFCSubNow <- map_subc_lc_df$soil_quick_flow_capacity
    map_subc_lc_df$I_PlantAvWatSub <- map_subc_lc_df$plant_available_water
    map_subc_lc_df$I_PWPSub <- map_subc_lc_df$inaccessible_water
    map_subc_lc_df <- map_subc_lc_df[c(
      "map_id",
      "ws_id",
      "lc_id",
      "I_FracVegClassNow",
      "I_SoilSatminFCSubNow",
      "I_PlantAvWatSub",
      "I_PWPSub"
    )]
    return(map_subc_lc_df)
  }
  
  get_genriver_pars <- function() {
    inp <- waterbalance_input_pars
    inp$I_Rain_IntensMean <- v$rain_par_cfg$rain_intensity_mean
    inp$I_Rain_IntensCoefVar <- v$rain_par_cfg$rain_intensity_coef
    inp$I_Rain_GenSeed <- v$rain_par_cfg$rain_randseed
    inp$I_RainMaxIntDripDur <- v$interception_par_cfg$interception_drip_duration_max
    inp$I_RainIntercDripRt <- v$interception_par_cfg$interception_drip_rt
    inp$I_MaxInf <- v$infiltration_par_cfg$soil_max_infiltration
    inp$I_PowerInfiltRed <- v$infiltration_par_cfg$soil_infiltration_red
    inp$I_MaxInfSSoil <- v$infiltration_par_cfg$soil_max_infiltration_subsoil
    inp$I_InitRelGW <- v$groundwater_par_cfg$groundwater_rel_init
    inp$I_InitRelSoil <- v$soilplant_par_cfg$soil_water_initial
    
    ### Initial Sub-catchment vars #############
    I_TotalArea <- v$ws_boundary_outlet_cfg$area_m2
    subc_area_df <- as.data.frame(v$subcatchment_map_sf)[c("ws_id", "area_m2", "distance")]
    
    subc_area_df$I_RelArea <- subc_area_df$area_m2 / I_TotalArea
    subc_area_df$I_RoutingDistance <- subc_area_df$distance / 1000 # m to km
    subc_df <- subc_area_df[c("ws_id", "I_RelArea", "I_RoutingDistance")]
    
    ### INITIALIZATION ##################################
    map_subc_lc_df <- get_map_subc_lc_df()
    lc_map_df <- v$lc_map_df
    
    rain_df <- v$rain_df
    d <- rain_df[1, "date"]
    year_now <- year(d)
    wslcid_cols <- c("ws_id", "lc_id")
    yr_subc_lc_df <- get_lc_vars_of_year(year_now, map_subc_lc_df, lc_map_df, wslcid_cols)
    yr_subc_lc_df$I_AvailWatClassNow <- yr_subc_lc_df$I_PlantAvWatSub
    
    ground_par_df <- v$ground_par_df
    wsid_cols <- c("ws_id")
    yr_ground_par_df <- get_lc_vars_of_year(year_now, ground_par_df, lc_map_df, wsid_cols)
    subc_df <- merge(subc_df, yr_ground_par_df, by = "ws_id", all.x = T)
    
    subc_lc_df <- yr_subc_lc_df
    
    subc_lc_df$D_CumNegRain <- 0
    subc_lc_df$D_CumEvapTranspClass <- 0
    subc_lc_df <- merge(subc_lc_df, subc_area_df[c("ws_id", "I_RelArea")], by = "ws_id", all.x = T)
    lc_par_df <- v$lc_par_df[c("lc_id",
                               "I_InterceptClass",
                               "I_RelDroughtFact",
                               "I_BD_BDRefVegNow")]
    subc_lc_df <- merge(subc_lc_df, lc_par_df, by = "lc_id", all.x = T)
    # I_AvailWaterClass = if I_SoilPropConst? = 1 then I_AvailWaterConst*I_FracVegClassNow[VegClass,Subcatchement]*I_RelArea[Subcatchement] else
    # (I_AvailWatClassNow[VegClass,Subcatchement])*I_FracVegClassNow[VegClass,Subcatchement]*I_RelArea[Subcatchement]
    if (inp$I_SoilPropConst == 1) {
      subc_lc_df$I_AvailWaterClass <- inp$I_AvailWaterConst * subc_lc_df$I_FracVegClassNow * subc_lc_df$I_RelArea
    } else {
      subc_lc_df$I_AvailWaterClass <- subc_lc_df$I_AvailWatClassNow * subc_lc_df$I_FracVegClassNow * subc_lc_df$I_RelArea
    }
    # D_SoilWater = I_AvailWaterClass[VegClass,Subcatchement]*I_InitRelSoil
    subc_lc_df$D_SoilWater <- subc_lc_df$I_AvailWaterClass * inp$I_InitRelSoil
    subc_lc_df$D_EvapTranspClass <- 0
    subc_lc_df$D_SoilQflowRelFrac <- inp$I_SoilQflowFrac
    subc_df$D_StreamsSurfQ <- 0
    subc_df$D_TotRiverFlowNoDelay <- 0
    # I_MaxDynGWact = if I_GWRelFracConst? = 1 then I_MaxDynGWConst else I_MaxDynGwSubNow[Subcatchement]
    subc_df$I_MaxDynGWact <- subc_df$I_MaxDynGWSub
    # I_MaxDynGWArea = I_MaxDynGWact[Subcatchement]*I_RelArea[Subcatchement]
    subc_df$I_MaxDynGWArea <- subc_df$I_MaxDynGWact * subc_df$I_RelArea
    # D_GWArea[Subcatchement] = I_MaxDynGWArea[Subcatchement]*I_InitRelGW
    subc_df$D_GWArea <- subc_df$I_MaxDynGWArea * inp$I_InitRelGW
    subc_df$D_IrrigEfficiency <- 0
    subc_lc_df$D_IrrigEfficiency <- 0
    subc_lc_df$D_GWUseFacility <- 1
    subc_df$D_GW_Utilization_fraction <- 0.02
    subc_lc_df$D_GW_Utilization_fraction <- 0.02
    
    #### Evapot init #######################
    month_now <- month(d)
    lc_evapot_df <- v$lc_evapot_df[v$lc_evapot_df$month == month_now, c("lc_id", "value")]
    names(lc_evapot_df) <- c("lc_id", "I_MultiplierEvapoTrans")
    subc_lc_df <- merge(subc_lc_df, lc_evapot_df, by = "lc_id", all.x = T)
    
    #### Stream and Lake init #######################
    lake_input_pars$I_TotalArea <- I_TotalArea
    lake_input_pars$I_WarmUpTime <- input$I_WarmUpTime
    #TODO: will be parameterize through UI
    subc_df$L_Lake <- 0
    subc_lc_df$L_Lake <- 0 #TODO: this should be subcat
    
    subc_df$D_FracGWtoLake <- 0
    subc_df$I_DaminThisStream <- 0
    subc_df$D_FeedingIntoLake <- 1
    # D_ReservoirVol[Subcatchement] = L_ResrDepth*L_Lake?[Subcatchement]*I_RelArea[Subcatchement]
    subc_df$D_ReservoirVol <- lake_input_pars$L_ResrDepth * subc_df$L_Lake * subc_df$I_RelArea
    # D_SubcResVol[Subcatchement] = D_ReservoirVol[Subcatchement]*I_DaminThisStream?[Subcatchement]
    subc_df$D_SubcResVol <- subc_df$D_ReservoirVol * subc_df$I_DaminThisStream
    subc_df$I_SubcContr <- 1
    
    return(
      list(
        year_now = year_now,
        month_now = month_now,
        subc_lc_df = subc_lc_df,
        subc_df = subc_df,
        waterbalance_input_pars = waterbalance_input_pars,
        lake_input_pars = lake_input_pars
      )
    )
  }
  
  default_stock_vars <- list(
    O_CumRain = 0,
    O_CumPercolation = 0,
    O_CumDeepInfilt = 0,
    O_BestYyHEPP = 0,
    O_LastYHepp = 0,
    O_ThisYHepp = 0,
    L_CumHEPPUse = 0,
    O_WorstYHEPP = 1,
    O_CumSurfQFlow = 0,
    O_CumBaseFlow = 0,
    O_CumSoilQFlow = 0,
    I_WarmEdUp = 0
  )
  
  run_genriver <- function(ndays = 100, progress) {
    pars <- get_genriver_pars()
    subc_df <- pars$subc_df
    subc_lc_df <- pars$subc_lc_df
    inp <- pars$waterbalance_input_pars
    lake_input_pars <- pars$lake_input_pars
    year_now <- pars$year_now
    month_now <- pars$month_now
    rain_df <- v$rain_df
    lc_map_df <- v$lc_map_df
    map_subc_lc_df <- get_map_subc_lc_df()
    ground_par_df <- v$ground_par_df
    stock <- default_stock_vars
    
    #### Output table ################
    output_df <- NULL
    
    ### SIMULATION LOOP ##################################
    set.seed(inp$I_Rain_GenSeed + 11250)
    I_Simulation_Time <- ndays
    for (t in 1:I_Simulation_Time) {
      progress(t / I_Simulation_Time,
               detail = paste("Day", t, "of", I_Simulation_Time))
      
      #### Daily Rainfall input #####################
      d <- rain_df[t, "date"]
      #TODO: could be use spatial explicit rain per sub catchment
      I_DailyRain <- rain_df[t, "rainfall"] * inp$I_RainMultiplier
      ws_rain_df <- subc_df[c("ws_id")]
      ws_rain_df$I_RainPerDay <- I_DailyRain
      
      ### Daily Evapotranspiration input #####################
      I_Daily_Evapotrans <- v$evapotran_df[v$evapotran_df$date == d, "evapotranspiration"]
      I_DebitTime <- v$river_df[v$river_df$date == d, "river_flow"]
      
      if (year_now != year(d)) {
        year_now <- year(d)
        yr_subc_lc_df <- get_lc_vars_of_year(year_now, map_subc_lc_df, lc_map_df, c("ws_id", "lc_id"))
        yr_subc_lc_df$I_AvailWatClassNow <- yr_subc_lc_df$I_PlantAvWatSub
        subc_lc_df[names(yr_subc_lc_df)] <- yr_subc_lc_df
        
        yr_ground_par_df <- get_lc_vars_of_year(year_now, ground_par_df, lc_map_df, c("ws_id"))
        subc_df[names(yr_ground_par_df)] <- yr_ground_par_df
      }
      
      ### Daily Evapotranspiration potential input #####################
      if (month_now != month(d)) {
        month_now <- month(d)
        lc_evapot_df <- v$lc_evapot_df[v$lc_evapot_df$month == month_now, c("lc_id", "value")]
        names(lc_evapot_df) <- c("lc_id", "I_MultiplierEvapoTrans")
        subc_lc_df$I_MultiplierEvapoTrans <- NULL
        subc_lc_df <- merge(subc_lc_df,
                            lc_evapot_df,
                            by = "lc_id",
                            all.x = T)
      }
      
      ### Compute water balance #####################
      wb <- water_balance(ws_rain_df, I_Daily_Evapotrans, subc_df, subc_lc_df, inp)
      sn <- stream_network(
        t,
        I_Daily_Evapotrans,
        I_DebitTime,
        wb$subc_df,
        wb$subc_lc_df,
        stream_input_pars,
        lake_input_pars,
        stock
      )
      
      ### Compile output ######################
      base_df <- data.frame(day = t,
                            date = d,
                            I_DailyRain = I_DailyRain)
      df <- base_df
      df[names(sn$stream_vars)] <- unlist(sn$stream_vars)
      df[names(sn$stock)] <- unlist(sn$stock)
      if (is.null(output_df)) {
        output_df <- df
      } else {
        output_df <- rbind(output_df, df)
      }
      ### Params for next loop ############
      subc_lc_df <- wb$subc_lc_df
      subc_df <- wb$subc_df
      stock <- sn$stock
    }
    vd$output_df <- output_df
  }
  
  # lc_intp_pars <- c("I_FracVegClassNow", "I_SoilSatminFCSubNow", "I_PlantAvWatSub", "I_PWPSub")
  # lc_stock_vars <- c("D_CumNegRain",
  #                    "D_CumEvapTranspClass",
  #                    "D_SoilWater",
  #                    "D_EvapTranspClass")
  # subc_stock_vars <- c("D_GWArea")
  # subc_output_list <- c("D_SurfaceFlow", "D_GWaDisch")
  # lc_output_list <- c("D_SoilDischarge")
  
  water_balance_plot_output <- c(
    "day",
    "O_CumRain",
    "O_CumPercolation",
    "O_CumDeepInfilt",
    "O_CumBaseFlow",
    "O_CumSoilQFlow"
  )
  
  water_balance_output <- c(
    "day",
    "date",
    "I_DailyRain",
    "O_IntercAcc",
    "O_EvapoTransAcc",
    "O_SoilQFlowAcc",
    "O_InfAcc",
    "O_PercAcc",
    "O_DeepInfAcc",
    "O_BaseFlowAcc",
    "O_SurfQFlowAcc",
    "L_InFlowtoLake",
    "I_RFlowdata_mmday"
  )
  
  watershed_indicator_output <- c(
    "day",
    "date",
    "I_DailyRain",
    "I_RFlowdata_mmday",
    "L_InFlowtoLake",
    "O_BaseFlowAcc",
    "O_IntercAcc",
    "O_SoilQFlowAcc",
    "O_SurfQFlowAcc",
    "L_RivOutFlow",
    "O_EvapoTransAcc"
  )
  
  hepp_plot_output <- c("day",
                        "L_HEPPWatUseFlow",
                        "L_HEPP_Kwh",
                        "L_LakeVol",
                        "L_LakeLevel")
  
  hepp_output <- c(
    "day",
    "date",
    "O_BestYyHEPP",
    "O_WorstYHEPP",
    "L_CumHEPPUse",
    "O_FrBaseFlow",
    "O_FrSoilQuickFlow",
    "O_FrSurfQuickflow"
  )
  
  observeEvent(input$sim_run_button, {
    # observe({
    if (is.null(vd$lc_df_crop_list) || is.null(v$lc_par_df))
      return()
    if (is.null(v$subc_soil_water_df))
      return()
    subc_lc_df <- v$subc_lc_df
    if (is.null(subc_lc_df))
      return()
    if (is.null(subc_lc_df$soil_quick_flow_capacity))
      return()
    if (is.null(v$subcatchment_map_sf))
      return()
    
    ndays <- input$ndays_input
    withProgress(message = 'Running Simulation', value = 0, {
      run_genriver(ndays, progress = setProgress)
    })
    
  })
  
  ### CALIBRATION #######################
  
  observe({
    if (is.null(vd$lc_df_crop_list) || is.null(v$lc_par_df))
      return()
    subc_sw_df <- v$subc_soil_water_df
    if (is.null(subc_sw_df))
      return()
    subc_lc_df <- v$subc_lc_df
    if (is.null(subc_lc_df))
      return()
    if (is.null(subc_lc_df$soil_quick_flow_capacity))
      return()
    if (is.null(v$subcatchment_map_sf))
      return()
    
    pars <- get_genriver_pars()
    subc_df <- pars$subc_df
    subc_lc_df <- pars$subc_lc_df
    inp <- pars$waterbalance_input_pars
    lake_input_pars <- pars$lake_input_pars
    
    I_DailyRain <- input$test_rain_day_input
    I_Daily_Evapotrans <- input$test_evapotrans_input
    year_now <- input$test_year_input
    month_now <- input$test_month_input
    n_iteration <- input$test_iteration_input
    
    stock <- default_stock_vars
    
    lc_map_df <- v$lc_map_df
    ground_par_df <- v$ground_par_df
    map_subc_lc_df <- get_map_subc_lc_df()
    
    yr_subc_lc_df <- get_lc_vars_of_year(year_now, map_subc_lc_df, lc_map_df, c("ws_id", "lc_id"))
    yr_subc_lc_df$I_AvailWatClassNow <- yr_subc_lc_df$I_PlantAvWatSub
    
    subc_lc_df[names(yr_subc_lc_df)] <- yr_subc_lc_df
    yr_ground_par_df <- get_lc_vars_of_year(year_now, ground_par_df, lc_map_df, c("ws_id"))
    subc_df[names(yr_ground_par_df)] <- yr_ground_par_df
    
    lc_evapot_df <- v$lc_evapot_df[v$lc_evapot_df$month == month_now, c("lc_id", "value")]
    names(lc_evapot_df) <- c("lc_id", "I_MultiplierEvapoTrans")
    subc_lc_df$I_MultiplierEvapoTrans <- NULL
    subc_lc_df <- merge(subc_lc_df, lc_evapot_df, by = "lc_id", all.x = T)
    
    ws_rain_df <- subc_df[c("ws_id")]
    ws_rain_df$I_RainPerDay <- I_DailyRain
    for (i in 1:n_iteration) {
      wb <- water_balance(ws_rain_df,
                          I_Daily_Evapotrans,
                          subc_df,
                          subc_lc_df,
                          inp,
                          debug = T)
      I_DebitTime <- 0
      sn <- stream_network(
        i,
        I_Daily_Evapotrans,
        I_DebitTime,
        wb$subc_df,
        wb$subc_lc_df,
        stream_input_pars,
        lake_input_pars,
        stock
      )
      # output
      subc_df <- sn$subc_df
      subc_lc_df <- sn$subc_lc_df
      stock <- sn$stock
    }
    
    sum_lc_vars <- c(
      "I_MaxInfArea",
      "I_SoilSatClass",
      "D_SoilWater",
      "I_MaxInfSubSAreaClass",
      "D_InterceptEvap",
      "D_Infiltration",
      "I_DailyRainAmount",
      "D_Percolation",
      "D_WaterEvapIrrigation",
      "D_SoilDischarge"
    )
    
    c_subc_lc <- colnames(subc_lc_df)
    c_subc_lc <- c_subc_lc[!c_subc_lc %in% c("ws_id", "lc_id")]
    c_subc_lc <- sort(c_subc_lc)
    
    c_subc <- colnames(subc_df)
    c_subc <- c_subc[!c_subc %in% c("ws_id", sum_lc_vars)]
    c_subc <- sort(c_subc)
    
    c_subc_lc <- c_subc_lc[!c_subc_lc %in% c_subc]
    c_subc_lc <- c("ws_id", "lc_id", c_subc_lc)
    c_subc <- c("ws_id", c_subc)
    
    vd$calibration_vars <- list(
      subc_lc_df = subc_lc_df[c_subc_lc],
      subc_df = subc_df[c_subc],
      output_vars = c(sn$stream_vars, sn$stock)
    )
  })
  
  output$general_table_output <- renderReactable({
    dlist <- vd$calibration_vars$output_vars
    if (is.null(dlist))
      return()
    df <- data.frame(variables = names(dlist), value = unlist(dlist))
    df <- df[order(df[[1]]), ]
    reactable(
      df,
      striped = T,
      borderless = TRUE,
      compact = TRUE,
      rownames = F,
      pagination = F,
      defaultColDef = colDef(
        width = 200,
        cell = function(value) {
          paste0(format(value, digits = 4))
        }
      )
    )
  })
  
  output$cumulative_table_output <- renderReactable({
    df <- vd$calibration_vars$subc_lc_df
    sdf <- vd$calibration_vars$subc_df
    if (is.null(df) || is.null(sdf))
      return()
    vdf <- df[, !(names(df) %in% c("ws_id", "lc_id"))]
    acc_vdf <- colSums(vdf, na.rm = T)
    vsdf <- sdf[, !(names(sdf) %in% c("ws_id"))]
    acc_vsdf <- colSums(vsdf, na.rm = T)
    acc <- c(acc_vdf, acc_vsdf)
    aggt_df <- data.frame(variables = names(acc), value = as.vector(acc))
    aggt_df <- aggt_df[order(aggt_df[[1]]), ]
    reactable(
      aggt_df,
      striped = T,
      borderless = TRUE,
      compact = TRUE,
      rownames = F,
      pagination = F,
      defaultColDef = colDef(
        width = 200,
        cell = function(value) {
          paste0(format(value, digits = 4))
        }
      )
    )
  })
  
  output$subc_lc_table_output <- renderReactable({
    df <- vd$calibration_vars$subc_lc_df
    if (is.null(df))
      return()
    df <- df[order(df$ws_id, df$lc_id), ]
    reactable(
      df,
      onClick = "expand",
      groupBy = c("ws_id"),
      striped = T,
      borderless = TRUE,
      compact = TRUE,
      rownames = F,
      pagination = F,
      columns = list(
        lc_id = colDef(
          aggregate = JS("function(values, rows) {return ''}"),
          footer = "Cumulative"
        ),
        ws_id = colDef(footer = "", format = colFormat(digits = 0))
      ),
      defaultColDef = colDef(
        width = 200,
        aggregate = "sum",
        format = colFormat(digits = 3),
        cell = function(value) {
          paste0(format(value, digits = 3))
        },
        footer = function(values)
          format(sum(values, na.rm = T), digits = 4),
        footerStyle = list(fontWeight = "bold")
      )
    )
  })
  
  output$subc_table_output <- renderReactable({
    df <- vd$calibration_vars$subc_df
    if (is.null(df))
      return()
    df <- df[order(df$ws_id), ]
    reactable(
      df,
      striped = T,
      borderless = TRUE,
      compact = TRUE,
      rownames = F,
      pagination = F,
      columns = list(ws_id = colDef(footer = "Cumulative")),
      defaultColDef = colDef(
        width = 200,
        cell = function(value) {
          paste0(format(value, digits = 4))
        },
        footer = function(values)
          sprintf("%.2f", sum(values)),
        footerStyle = list(fontWeight = "bold")
      )
      
    )
  })
  
  ### Water Balance Output ##################
  
  output$water_balance_plot <- renderPlotly({
    df <- vd$output_df
    if (is.null(df) || nrow(df) == 0)
      return()
    df <- df[water_balance_plot_output]
    df <- df[df$day > input$I_WarmUpTime,]
    plotly_multiple_axis(df)
  })
  
  reactable_daily_data <- function(df) {
    reactable(
      df,
      striped = T,
      borderless = TRUE,
      compact = TRUE,
      rownames = F,
      defaultPageSize = 50,
      columns = list(day = colDef(
        format = colFormat(digits = 0), width = 50
      )),
      defaultColDef = colDef(format = colFormat(digits = 3))
    )
  }
  
  output$water_balance_table_output <- renderReactable({
    df <- vd$output_df
    if (is.null(df))
      return()
    df <- df[water_balance_output]
    reactable_daily_data(df)
  })
  
  output$watershed_indicator_table_output <- renderReactable({
    df <- vd$output_df
    if (is.null(df))
      return()
    df <- df[watershed_indicator_output]
    reactable_daily_data(df)
  })
  
  output$hepp_plot <- renderPlotly({
    df <- vd$output_df
    if (is.null(df) || nrow(df) == 0)
      return()
    df <- df[hepp_plot_output]
    plotly_multiple_axis(df)
  })
  
  output$hepp_table_output <- renderReactable({
    df <- vd$output_df
    if (is.null(df))
      return()
    df <- df[hepp_output]
    reactable_daily_data(df)
  })
 
}
