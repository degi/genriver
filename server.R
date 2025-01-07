#
# - soil zone should be combine elevation effect (include depth)
# - konsitensi check.. keluarin garis one one
# tambah chek konsistensi (nima)
# ouput -> buffering indicator

# setelah warming up. trus balik lagi ke tanggal awal

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 1000 * 1024 ^ 2)
  data_dir <- paste0(tempdir(), "/data_temp")
  
  map_color <-
    colorRampPalette(c("darkgreen", "#ffb703", "#9a130e", "#023e8a"))
  grad_color <- colorRampPalette(c("#023047", "#219ebc", "#06d6a0", "#ffb703", "#9a130e"))
  slope_color <- colorRampPalette(c("#034464", "#06d6a0", "#ffb703"))
  stream_color <-
    colorRampPalette(c(
      "#e07a5f",
      "#7D60E0",
      "#606EE0",
      "#609AE1",
      "#0077b6",
      "#023e8a",
      "#03045e"
    ))
  # soil_color <- colorRampPalette(c("#331A05", "#a39171", "#FDEFE1"))
  soil_color <- colorRampPalette(c("darkgreen", "#ffb703", "#9a130e", "#023e8a"))
  depth_color <- colorRampPalette(c("#FDEFE1", "#a39171", "#331A05"))
  water_color <- colorRampPalette(c("#D4FEF3", "#219ebc", "#034464"))
  
  options(reactable.theme = reactableTheme(
    style = list(fontFamily = "Arial, Helvetica, sans-serif", fontSize = "1em")
  ))
  
  #not working for vector of number
  f_number <- function(v, ...) {
    format(v, big.mark = ",", scientific = F, ...)
  }
  
  f_percent <- function(v) {
    sprintf("%0.1f%%", v * 100)
  }
  
  soil_db_path <- "soil/HWSD2.sqlite"
  soil_map_path <- "/vsizip/soil/HWSD2_RASTER.zip/HWSD2.bil"
  
  ### Conditional panel UI logic ###
  
  conditional_id <-
    c(
      "is_dem_map",
      "is_stream_map",
      "is_lc_df",
      "is_subcatchment",
      "is_lake_df",
      "is_dam_df"
    )
  conditional_v <-
    c(
      "dem_map_stars",
      "dem_stream_sf",
      "lc_df",
      "subcatchment_map_sf",
      "lake_df",
      "dam_df"
    )
  
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
    soil_cfg = list(),
    
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
    dem_stream_bb_sf = NULL,
    
    ws_boundary_sf = NULL,
    
    dem_flow_stars = NULL,
    dem_stream_sf = NULL,
    
    watershed_map_list = NULL,
    outlet_map_list = NULL,
    
    routing_map_stars = NULL,
    routing_time_map_sf = NULL,
    routing_river_sf = NULL,
    
    watershed_map_stars = NULL,
    subcatchment_map_sf = NULL,
    subcatchment_desc_df = NULL,
    outlet_map_sf = NULL,
    
    
    soil_segments_sf = NULL,
    soil_segments_df = NULL,
    soil_segments_global_sf = NULL,
    soil_segments_global_df = NULL,
    soil_type_df = NULL,
    
    
    
    
    
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
    ),
    
    slope_sf = NULL
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
    # lc_df_crop_list = NULL,
    cell_size = NULL,
    
    subcatchment_map_sf = NULL,
    
    slope_stars = NULL,
    slope_factor_stars = NULL,
    
    slope_std_stars = NULL,
    elevation_std_stars = NULL,
    soil_depth_std_stars = NULL,
    soil_depth_stars = NULL,
    
    calibration_vars = NULL,
    
    output_df = NULL,
    performance_df = NULL,
    buffering_df = NULL,
    wb_ind_avg = NULL,
    wb_avg = NULL,
    wb_yearly = NULL,
    
    soil_type_global_df = NULL,
    soil_water_content_global_df = NULL,
    soil_water_content_user_df = NULL,
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
  
  ### INPUT PARAMETES AND DATA
  
  io_file_df <- data.frame(
    var = c(
      "genriver_cfg",
      "soil_cfg",
      
      "lc_map_stars_list",
      "lc_df",
      "lc_map_df",
      
      "lc_par_df",
      "lc_evapot_df",
      "evapot_month_data_df",
      
      "ground_par_df",
      
      "map_boundary_sf",
      "ws_boundary_sf",
      
      "subcatchment_map_sf",
      "subcatchment_desc_df",
      "outlet_map_sf",
      
      "dem_map_stars",
      
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
      "dam_df",
      
      "soil_segments_df",
      "soil_type_df"
    ),
    file = c(
      "genriver",
      "soil",
      
      "landcovermap",
      "landcover",
      "map_list",
      
      "lc_props",
      "lc_evapot",
      "evapot_monthly",
      
      "ground_par",
      
      "map_boundary",
      "ws_boundary",
      
      "subcatchment",
      "subcatchment_desc",
      "outlet",
      
      "dem_bb",
      
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
      "dam",
      
      "soil_segments",
      "soil_type"
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
    for (par in numeric_par) {
      id <- gsub(suffix(par), "input", par)
      update_numeric_input_ui(id, v[[par]])
    }
    
    for (var in names(v$stream_par_cfg)) {
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
      lc_df$description <- ""
    } else {
      dif <- as.numeric(setdiff(id, lc_df$lc_id))
      if (length(dif) > 0) {
        a_df <- data.frame(
          lc_id = dif,
          color = chart_color[sample.int(length(chart_color), length(dif))],
          land_cover = paste0("Landcover_", dif),
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
    toggle_popover("add_lc_pop", show = F)
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
        if (ex == "tif") {
          try(m <- read_stars(f, proxy = T), silent = T)
        }
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
                showNotification(
                  paste(
                    "The map file",
                    ferr,
                    "is outside the previous map boundary"
                  ),
                  type = "warning"
                )
                next
              }
              
              v$map_boundary_sf <- st_union(bb, ps) |> st_bbox() |> st_as_sfc()
            })
          }
          
          idx <- suffix(prefix(f, "."), "-")
          suppressWarnings(n <- as.numeric(idx))
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
    
    map_df <- isolate(v$lc_map_df)
    name_ms <- names(ms)
    output$lc_map_out <- renderUI({
      layout_column_wrap(width = 1, !!!lapply(name_ms, function(x) {
        card(
          class = "gray_bg",
          full_screen = T,
          card_body(
            gap = "5px",
            padding = "10px",
            div(
              span(
                span("Year:", tags$b(textOutput(
                  paste0("lc_year_label_", x), inline = T
                ))) |>
                  
                  popover(
                    numericInput(
                      paste0("lc_map_year_", x),
                      "Map Acquisition Year:",
                      map_df[map_df$map_id == x, "year"],
                      min = 0
                    )
                  ),
                info("Click to edit the map acquisition year")
                # bs_icon("info-circle") |>
                #   tooltip_blue("Click to edit the map acquisition year")
              ) ,
              actionButton(
                paste0("delete_map_btn_", x),
                "Remove",
                icon = icon("trash-can"),
                class = "menu_button"
              ) |> tooltip_blue("Remove the map"),
              
              class = "bg_light2 bg_theme d-flex justify-content-between"
            ),
            card_body(padding = 0, plotOutput(paste0(
              "lc_map_", x
            ))) |>
              tooltip_blue(paste("File name:", map_df[map_df$map_id == x, "filename"]))
          )
        )
      }))
    })
    
    lapply(name_ms, function(x, ms) {
      output[[paste0("lc_map_", x)]] <- renderPlot({
        df <- v$lc_df
        # par(bg = "#F6F6F6")
        suppressMessages(plot(
          ms[[x]],
          col = df$color,
          breaks = c(-1, df$lc_id),
          key.pos = NULL,
          main = NULL
        ))
      }, bg = "transparent")
      
      observeEvent(input[[paste0("lc_map_year_", x)]], {
        yr <- input[[paste0("lc_map_year_", x)]]
        df <- isolate(v$lc_map_df)
        df[df$map_id == x, "year"] <- yr
        v$lc_map_df <- df
      })
      
      output[[paste0("lc_year_label_", x)]] <- renderText(v$lc_map_df[v$lc_map_df$map_id == x, "year"])
      
      observeEvent(input[[paste0("delete_map_btn_", x)]], {
        df <- isolate(v$lc_map_df)
        v$lc_map_df <- df[df$map_id != x, ]
        v$lc_map_stars_list[[x]] <- NULL
        print(paste("delete", x))
      })
    }, ms)
    
  })
  
  landuse_list <- c("Forest", "Tree-based system", "Agriculture", "Settlement")
  lc_df_edited <- table_edit_server("lc_df_table",
                                    reactive(v$lc_df),
                                    col_type = c("numeric", "color", "text", "text"))
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
  
  
  lc_par_df_display_ed <- function() {
    d <- reactive({
      if (is.null(v$lc_df) || is.null(v$lc_par_df))
        return()
      merge(v$lc_df[c("lc_id", "land_cover")], v$lc_par_df[names(v$lc_par_df) != "land_cover"], by = "lc_id", all.x = T)
    })
    
    table_edit_server(
      "lc_props_table",
      d,
      col_title = c("lc_id", "Land Cover", lc_prop_cols$label),
      col_type = c("numeric", "character", rep("numeric", 3)),
      col_width = c(50, 150, 250, 250, 100),
      col_disable = c(T, T, F, F, F)
    )
  }
  
  lc_par_df_edited <- lc_par_df_display_ed()
  
  observe({
    v$lc_par_df <- lc_par_df_edited()
  })
  
  lc_evapot_df_edited <- table_edit_server(
    "lc_evapot_table",
    reactive(vd$lc_evapot_disp_df),
    col_type = c("numeric", "character", rep("numeric", 12)),
    col_disable = c(T, T, rep(F, 12))
  )
  
  observe({
    vd$lc_evapot_disp_df <- lc_evapot_df_edited()
  })
  
  
  observe({
    df <- v$lc_df
    if (is.null(df) || nrow(df) == 0)
      return()
    
    pdf <- isolate(v$lc_par_df)
    if (is.null(pdf) || nrow(pdf) == 0) {
      pdf <- df[c("lc_id")]
      # pdf[lc_prop_cols$var] <- NA
      pdf[lc_prop_cols$var[1]] <- 3
      pdf[lc_prop_cols$var[2]] <- 0.6
      pdf[lc_prop_cols$var[3]] <- 1
    } else {
      adf <- df[c("lc_id")]
      pdf <- merge(adf, pdf[c("lc_id", lc_prop_cols$var)], by = "lc_id", all.x = T)
    }
    v$lc_par_df <- pdf
    edf <- isolate(vd$lc_evapot_disp_df)
    if (is.null(edf) || nrow(edf) == 0) {
      edf <- df[c("lc_id", "land_cover")]
      # edf[month_cols$var] <- NA
      edf[month_cols$var] <- 1
    } else {
      adf <- df[c("lc_id", "land_cover")]
      edf <- merge(adf, edf[c("lc_id", month_cols$var)], by = "lc_id", all.x = T)
    }
    vd$lc_evapot_disp_df <- edf
  })
  
  observe({
    edf <- vd$lc_evapot_disp_df
    if (is.null(edf) || nrow(edf) == 0) {
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
  
  evapot_month_df_edited <- table_edit_server(
    "evapot_monthly_df_table",
    reactive(v$evapot_month_data_df),
    col_type = c("numeric", "character", "numeric"),
    col_disable = c(T, T, F)
  )
  
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
    print("Get DEM")
    removeModal()
    thres <- max(input$dem_flow_threshold, 1)
    v$genriver_cfg$flow_threshold <- thres
    api_key <- input$user_api_key
    if (api_key == "") {
      api_key <- api_key_default
    }
    bb <- st_bbox(v$map_boundary_sf)
    l <- list(
      demtype = input$demtype_inp,
      outputFormat = "GTiff",
      west = bb[["xmin"]],
      east = bb[["xmax"]],
      south = bb[["ymin"]],
      north = bb[["ymax"]],
      API_Key = api_key
    )
    params <- paste0(paste0(names(l), "=", l), collapse = "&")
    get_topo_url <- paste0(open_topo_url, params)
    # TODO: temporally commented
    showPageSpinner(caption = "Please wait while downloading DEM map")
    f <- NULL
    m <- NULL
    res <- GET(get_topo_url)
    if (res$status == 401) {
      showNotification("DEM download failed!", type = "error")
      showModal(modalDialog(title = "DEM Download Failed", markdown(dem_fail)))
    } else if (res$status == 200) {
      showNotification("DEM download is sucsesfull!", type = "message")
      f <- tempfile("dem")
      writeBin(res$content, f)
    }
    # f <- "dem_sbj.tif"
    if (!is.null(f)) {
      try(m <- read_stars(f, proxy = T))
    }
    if (!is.null(m) && !is.null(f)) {
      v$dem_map_stars <- m
      v$genriver_cfg$dem <- l
      m <- st_as_stars(m)
      val <- sort(unique(as.vector(m[[1]])))
      pal <- colorNumeric(grad_color(10), val, na.color = "transparent")
      leafletProxy(active_watershed_lf, session) |>
        clearShapes() |>
        addFeatures(
          v$map_boundary_sf,
          layerId = 'dem_map',
          color = theme_color$secondary,
          label = "Click here to generate the stream path",
          labelOptions = labelOptions(className = "map_label", offset = c(0, -5)),
          highlightOptions = highlightOptions(weight = 5, color = theme_color$danger),
          fillOpacity = 0,
          opacity = 1,
          weight = 1
        ) |>
        addGeoRaster(
          m,
          colorOptions = colorOptions(palette = grad_color(200)),
          opacity = 0.6,
          layerId = 'dem_map'
        ) |>
        addLegend(pal = pal,
                  values = val,
                  title = "Elevation")
      generate_dem_flow(v$genriver_cfg$flow_threshold)
      is_set_ws_boundary <<- T
    }
    hidePageSpinner()
    is_spinner(F)
  })
  
  observe({
    m <- v$dem_map_stars
    if (is.null(m))
      return()
    vd$cell_size <- cellSize.stars(m, unit = "ha")
  })
  
  ### WATERSHED BOUNDARY#########################
  
  selected_ws_outlet_id <- NULL
  is_set_ws_boundary <- F
  
  observeEvent(input$set_watershed_btn, {
    removeModal()
    print("set watershed boundary")
    sf <- v$outlet_map_sf[1, ]
    v$outlet_map_sf <- NULL
    v$ws_boundary_sf <- sf
    is_set_ws_boundary <<- F
  })
  
  
  observe({
    sf <- v$ws_boundary_sf
    dem_map <- v$dem_map_stars
    if (is.null(sf) ||
        is.null(dem_map))
      return()
    fthres <- isolate(v$genriver_cfg$flow_threshold)
    if (is.null(isolate(v$dem_flow_bb_stars))) {
      generate_dem_flow(fthres)
    }
    v$ws_boundary_stars <- st_rasterize(st_sf(st_geometry(sf)), dem_map, align = T)
    print("DEM crop")
    v$dem_crop_stars <- crop_raster(dem_map, v$ws_boundary_stars)
    print("DEM flow crop")
    v$dem_flow_stars <- crop_raster(isolate(v$dem_flow_bb_stars), v$ws_boundary_stars)
    print("Generate stream map")
    v$dem_stream_sf <- generate_stream(v$dem_flow_stars, fthres)
    leafletProxy(active_watershed_lf, session) |> clearMarkers() |>
      clearControls() |>
      clearShapes() |>
      fit_map_view(v$dem_stream_sf) |>
      plot_watershed_boundary() |>
      show_stream(v$dem_stream_sf, opacity = 1)
    isolate(if (is.null(isolate(v$outlet_map_sf))) {
      is_update_watershed_table(T)
      process_generate_subcatchment()
    })
    #crop land cover map
    isolate(suppressWarnings(crop_land_cover_map()))
    #query soil properties
    isolate(generate_soil_map())
  })
  
  
  process_generate_subcatchment <- function(min_sub_area = 20) {
    print("generate routing distance")
    min_sub_area <- max(min_sub_area, 1)
    v$genriver_cfg$flow_threshold <- min_sub_area
    rd_map <- generate_routing_distance(v$dem_flow_stars, min_sub_area)
    strm_sf <- generate_rounded_polygon(rd_map["routing"] / 1000, 1)
    v$routing_river_sf <- strm_sf
    v$routing_map_stars <- rd_map
    print("generate subcathments")
    withProgress(message = 'Generate subcathments', value = 0, {
      subc_map <- generate_subcathments(
        isolate(v$dem_direction_terra),
        rd_map,
        order = 2,
        min_sub_area = min_sub_area,
        progress = setProgress
      )
    })
    subc_map$rel_area <- as.numeric(subc_map$area_m2 / v$genriver_cfg$total_area_m2)
    subc_map <- subc_map[order(subc_map$distance), ]
    subc_map$color <-  grad_color(nrow(subc_map))
    subc_map$ws_id <- subc_map$out_id
    # reset the correlated vars
    v$subc_lc_df <- NULL
    v$subcatchment_map_sf <- NULL
    default_outlet_map_sf(subc_map)
    v$outlet_map_sf <- subc_map
    update_subcatchment_outlet()
  }
  
  crop_land_cover_map <- function() {
    print("crop_land_cover_map")
    lc_ms <- isolate(v$lc_map_stars_list)
    if (is.null(lc_ms))
      return()
    wsb <- isolate(v$ws_boundary_stars)
    lc_crop_ms <- list()
    withProgress(message = 'Cropping land cover maps', value = 0, {
      ids <- names(lc_ms)
      p <- 1 / (length(ids) + 1)
      for (m_id in ids) {
        incProgress(p)
        cm <- crop_raster(lc_ms[[m_id]], wsb)
        lc_crop_ms[[m_id]] <- stars_to_proxy(cm, m_id)
      }
    })
    v$lc_map_crop_stars_list <- lc_crop_ms
  }
  
  output$ws_area <- renderText(f_number(v$genriver_cfg$total_area_m2 / 10000, digits = 5))
  
  output$ws_unclassified_area <- renderText({
    df <- as.data.frame(v$subcatchment_map_sf)
    a <- v$genriver_cfg$total_area_m2 / 10000 - sum(df[df$ws_id != 999, "area"])
    f_number(a, digits = 5)
  })
  
  default_outlet_map_sf <- reactiveVal()
  
  #### Calculate land cover area by subcathment ####################
  
  check_subcatchment_map_sf <- function() {
    subc <- v$subcatchment_map_sf
    if (is.null(subc) || is.null(v$ws_boundary_sf))
      return()
    sf999 <- subc[subc$ws_id == 999, ]
    subc_sf <- subc[subc$ws_id != 999, ]
    a <- v$genriver_cfg$total_area_m2 / 10000
    rem_a <- a - sum(subc_sf$area)
    if (nrow(sf999) == 0 || sf999$area != rem_a) {
      sf_use_s2(FALSE)
      suppressMessages({
        sg <- st_union(st_geometry(subc_sf))
        g <- st_union(st_difference(st_geometry(v$ws_boundary_sf), sg))
      })
      sf <- st_sf(geometry = st_sfc(g))
      sf$ws_id <- 999
      sf$color <- "green"
      sf$area <- rem_a
      sf$rel_area <- rem_a / a
      sf$distance <- 0
      sf$n_outlet <- 0
      sf$uncheck <- 1
      sf$label <- "unclassified"
      v$subcatchment_map_sf <- rbind(subc_sf, sf)
    }
  }
  
  calculate_subcatchment_land_cover_area <- function() {
    print("Calculate land cover area by subcathment")
    check_subcatchment_map_sf()
    subc <- v$subcatchment_map_sf
    lc_map_list <- v$lc_map_crop_stars_list
    lc_ids <- v$lc_df$lc_id
    ws_area <- v$genriver_cfg$total_area_m2 / 10000
    lc_m <- st_as_stars(lc_map_list[[1]])
    subc_stars <- st_rasterize(subc["ws_id"], lc_m, align = T)
    subc_stars <- crop_raster(subc_stars, lc_m)
    map_ids <- names(lc_map_list)
    subc_df <- as.data.frame(subc)
    print("area")
    #TODO: this should be aggregated if it has multiple outlets
    subc_df$I_RelArea <- subc_df$area / ws_area
    subc_df <- subc_df[c("ws_id", "area", "I_RelArea")]
    
    subcell_df <- as.data.frame(table(subc_stars))
    names(subcell_df) <- c("ws_id", "ncell_sub")
    subcell_df <- merge(subcell_df, subc_df[c("ws_id", "area", "I_RelArea")], by = "ws_id")
    
    pinc <- 1 / length(map_ids)
    all_df <- NULL
    print("progress")
    withProgress(message = 'Calculating land cover area by subcatchment', value = 0, {
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
        df <- merge(df, agg_df, by = "ws_id")
        df <- merge(df, subc_df[c("ws_id", "area")], by = "ws_id")
        df$area_ratio <- df$ncell / df$ncell_sub
        df$area <- df$area_ratio * df$area
        df$map_id <- map_id
        if (is.null(all_df)) {
          all_df <- df
        } else {
          all_df <- rbind(all_df, df)
        }
      }
    })
    return(all_df)
  }
  
  
  
  
  
  
  # observe({
  #   mlist <- v$lc_map_crop_stars_list
  #   if(is.null(mlist) || is.null(vd$cell_size)) return()
  #   ids <- names(mlist)
  #   dflist <- list()
  #   for(id in ids) {
  #     m <- st_as_stars(mlist[[id]])
  #     df <- as.data.frame(table(m))
  #     colnames(df) <- c("lc_id", "area")
  #     df$area <- df$area * vd$cell_size
  #     dflist[[id]] <- df
  #   }
  #   vd$lc_df_crop_list <- dflist
  # })
  
  ### GENERATE STREAM MAP #########################
  
  is_update_dem <- reactiveVal(T)
  
  # observeEvent(input$process_dem, {
  #   removeModal()
  #   t <- isolate(input$dem_flow_threshold)
  #   v$genriver_cfg$flow_threshold <- t
  #   generate_dem_flow(t)
  # })
  #
  # observe({
  #   if (is.null(v$dem_map_stars))
  #     return()
  #   generate_dem_flow()
  # })
  
  generate_dem_flow <- function(flow_threshold = 20) {
    if (is.null(v$dem_map_stars))
      return()
    withProgress(message = 'Processing DEM', value = 0, {
      # if (is_update_dem()) {
      #   is_update_dem(F)
      dem_out <- flowdem(v$dem_map_stars)
      v$dem_direction_terra <- dem_out$dem_direction_terra
      v$dem_flow_bb_stars <- dem_out$dem_flow_stars
      # }
      setProgress(0.9, detail = "Generating stream path")
      print("DEM -> stream")
      v$dem_stream_bb_sf <- generate_stream(isolate(v$dem_flow_bb_stars), flow_threshold)
      # write_sf(v$dem_stream_bb_sf, "stream.shp")
      setProgress(1, detail = "Done!")
      print("DEM -> done!")
    })
    
    leafletProxy(active_watershed_lf, session) |>
      clearShapes() |>
      clearImages() |>
      clearControls() |>
      show_stream(v$dem_stream_bb_sf)
  }
  
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
  
  ### WATERSHED MAP LEAFLET ########################################
  
  is_watershed_leaflet_base <- F
  base_tiles <- "Esri.WorldTopoMap"
  active_watershed_lf <- "watershed_map_leaflet"
  update_subcatchment <- reactiveVal(F)
  
  base_leaflet <- function() {
    leaflet(options = leafletOptions(attributionControl = FALSE)) |>
      addProviderTiles(base_tiles)
  }
  
  repaint_subcatchment_map <- function() {
    leafletProxy(active_watershed_lf, session) |>
      clearMarkers() |>
      clearControls() |>
      clearShapes() |>
      fit_map_view(isolate(v$dem_stream_sf)) |>
      addHomeButton(
        ext = st_bbox(v$dem_stream_sf),
        group = "Home",
        position = "topleft"
      ) |>
      plot_watershed_boundary() |>
      show_stream(isolate(v$dem_stream_sf), opacity = 1) |>
      plot_subcatchment()
  }
  
  output$watershed_map_leaflet <- renderLeaflet({
    lf <- base_leaflet()
    is_watershed_leaflet_base <<- T
    
    if (!is.null(v$map_boundary_sf) && is.null(v$dem_stream_sf)) {
      lf <- lf |> addFeatures(
        v$map_boundary_sf,
        layerId = 'dem_boundary',
        color = theme_color$secondary,
        label = "Click here to add DEM map",
        labelOptions = labelOptions(className = "map_label", offset = c(0, -5)),
        highlightOptions = highlightOptions(fillOpacity = 0.8),
        fillOpacity = 0.3,
        opacity = 0.8
      ) |>
        addHomeButton(
          ext = st_bbox(v$map_boundary_sf),
          group = "Home",
          position = "topleft"
        )
    }
    return(lf)
  })
  
  plot_subcatchment <- function(lf,
                                ws_id = NULL,
                                color = NULL,
                                fillOpacity = 0.6) {
    sf <- v$subcatchment_map_sf
    if (is.null(sf))
      return(lf)
    sf <- sf[sf$ws_id != 999, ]
    if (!is.null(ws_id)) {
      layer_id <- paste0("ws_", ws_id)
      lf <- lf |> removeShape(layer_id)
      sf <- sf[sf$ws_id %in% ws_id, ]
    }
    if (!is.null(color)) {
      sf$color <- color
    }
    label <- map_label(
      paste(
        "ID:",
        sf$ws_id,
        "<br>Area size:",
        sprintf("%0.1f", sf$area),
        "ha<br>Routing distance:",
        sprintf("%0.1f", sf$distance),
        "km"
      ),
      "Sub-Catchment"
    )
    sf$layer_id <- paste0("ws_", sf$ws_id)
    lf |>
      addFeatures(
        sf,
        group = "subcatchment",
        layerId = ~ layer_id,
        color = "#000",
        fillColor = ~ color,
        fillOpacity = fillOpacity,
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
  
  plot_watershed_boundary <- function(lf,
                                      color = theme_color$secondary,
                                      opacity = 0.2) {
    if (is.null(v$ws_boundary_sf))
      return(lf)
    lf |> addFeatures(
      v$ws_boundary_sf,
      layerId = 'ws_boundary',
      color = color,
      label = "Watershed area",
      labelOptions = labelOptions(className = "map_label", offset = c(0, -5)),
      fillOpacity = opacity,
      stroke = F
    )
  }
  
  ### WATERSHED map click #########################
  
  observeEvent(input$watershed_map_leaflet_shape_click, {
    click_inp <- input$watershed_map_leaflet_shape_click
    # print(click_inp)
    if (!is.null(click_inp$id)) {
      if (click_inp$id == "dem_boundary") {
        is_spinner(T)
        inplist <- as.list(opentopo_dataset_df$var)
        names(inplist) <- opentopo_dataset_df$label
        
        show_input_dialog(
          title = "Get Digital Elevation Model (DEM) Data",
          desc = dem_info,
          confirm_id = "get_dem_open_topo",
          confirm_label = "Get DEM Data",
          custom_input = selectInput(
            "demtype_inp",
            "Choose the dataset",
            inplist,
            selected = "SRTMGL1",
            width = "100%"
          ),
          input_var = c("user_api_key", "dem_flow_threshold"),
          input_label = c(
            "OpenTopography API key",
            "Minimum contributing area of delineated streams (ha)"
          ),
          input_def = c(NA, 50),
          input_type = c("character", "numeric"),
          input_info = c(dem_api_info, "")
        )
      } else {
        if (is_set_ws_boundary) {
          show_input_dialog(
            "Watershed Boundary",
            "Set this subcathment area as watershed boundary?",
            "set_watershed_btn",
            "Yes"
          )
          return()
        }
        ws_df <- v$subcatchment_map_sf
        ws_id <- as.numeric(suffix(click_inp$id))
        s <- which(ws_df$ws_id == ws_id)
        state <- req(getReactableState("watershed_list"))
        rows <- as.vector(state$selected)
        if (ws_id %in% selected_ws_ids) {
          selected <- rows[rows != s]
        } else {
          selected <- c(s, rows)
        }
        ws_df$uncheck <- 1
        ws_df[selected, "uncheck"] <- 0
        updateReactable("watershed_list",
                        selected = selected,
                        data = ws_df)
      }
    }
    if (!is.null(click_inp$group)) {
      if (click_inp$group == "stream") {
        active_watershed_lf <<- "watershed_map_leaflet"
        leafletProxy(active_watershed_lf, session) |>
          add_outlet_dialog(click_inp$lng, click_inp$lat)
      }
    }
  })
  
  # map_label <- function(desc = "",
  #                       title = "",
  #                       footer = "") {
  #   title_div <- ""
  #   if (title != "")
  #     title_div <- paste(title,
  #                        "<hr style='margin:2px auto;font-size:1.5em;'>")
  #   footer_div <- "</div>"
  #   if (footer != "")
  #     footer_div <- paste("<hr style='margin:2px auto;'><strong><em>",
  #                         footer,
  #                         "</em></strong></div>")
  #   paste("<div style='font-size:1.2em;'>",
  #         title_div,
  #         "<div>",
  #         desc,
  #         "</div>",
  #         footer_div) |>
  #     lapply(htmltools::HTML)
  # }
  
  show_stream <- function(lf,
                          stream_f,
                          is_show_label = T,
                          opacity = 0.8,
                          group = "stream") {
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
        paste(
          "Watershed contributing area: &plusmn;",
          f_number(m$val, nsmall = 0),
          "ha"
        ),
        "Stream Path",
        "* Click here to add a stream outlet"
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
      h6("The selected location is:"),
      p(tags$b(
        "Lon:",
        f_number(lon, nsmall = 5),
        "Lat:",
        f_number(lat, nsmall = 5)
      )),
      h5("Add subcatchment outlet here?"),
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
    leafletProxy(active_watershed_lf, session) |> removeMarker("selected_point")
    suppressMessages(add_outlet(selected_point$lon, selected_point$lat))
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
  
  add_outlet <- function(lon, lat) {
    print(paste("add outlet (lon, lat):", lon, lat))
    m <- generate_watershed(isolate(v$dem_direction_terra), lon, lat)
    suppressMessages({
      ps <- st_union(st_as_sf(
        m,
        as_points = F,
        merge = T,
        connect8 = T
      ))
    })
    ps <- st_transform(ps, crs = 4326)
    area_list <- as.numeric(st_area(ps))
    if (length(area_list) == 0 || area_list == 0) {
      showModal(modalDialog(
        title = span(icon("circle-exclamation"), "Outlet Point Error"),
        tags$li("Make sure the point location is on the stream path"),
        tags$li("Zoom in the map to get accurate position")
      ))
      leafletProxy(active_watershed_lf, session) |> removeMarker("selected_point")
      return()
    }
    new_subcatchment <<- list("shape" = ps,
                              "lon" = lon,
                              "lat" = lat)
    ws_sf <- isolate(v$subcatchment_map_sf)
    if (is.null(ws_sf)) {
      confirm_add_subcacthment()
      return()
    }
    ws_sf <- ws_sf[ws_sf$ws_id != 999, ]
    suppressMessages({
      ints <- unlist(st_intersects(ps, ws_sf))
      toc <- unlist(st_touches(ps, ws_sf))
    })
    overlapped_subc <<- sort(ws_sf$ws_id[ints[!ints %in% toc]])
    
    if (length(overlapped_subc) > 0) {
      ids <- paste(as.vector(overlapped_subc), collapse = ", ")
      show_input_dialog(
        title = span(icon("circle-exclamation"), "Sub-Catchment Overlaps"),
        desc = div(
          div(
            "The newly added sub-catchment area overlaps with sub-catchments (ID):",
            tags$b(ids)
          ),
          h5("Replace the overlapped sub-catchment?", style = "margin-top:20px")
        ),
        confirm_id = "confirm_replace_subcathment"
      )
      leafletProxy(active_watershed_lf, session) |> removeMarker("selected_point")
    } else {
      confirm_add_subcacthment()
    }
  }
  
  overlapped_subc <- c()
  new_subcatchment <- NULL
  
  observeEvent(input$confirm_replace_subcathment, {
    removeModal()
    ws_sf <- isolate(v$subcatchment_map_sf)
    ws_sf <- ws_sf[!ws_sf$ws_id %in% overlapped_subc, ]
    v$subcatchment_map_sf <- ws_sf
    layer_id <- paste0("ws_", overlapped_subc)
    leafletProxy(active_watershed_lf, session) |> removeShape(layer_id)
    confirm_add_subcacthment()
  })
  
  confirm_add_subcacthment <- function() {
    df <- st_sf(geometry = new_subcatchment$shape)
    area_list <- as.numeric(st_area(df))
    id <- 1
    ws_sf <- isolate(v$outlet_map_sf)
    if (!is.null(ws_sf) && !is_set_ws_boundary) {
      id <- max(ws_sf$out_id) + 1
    }
    df$ws_id <- id
    df$color <- get_color(id)
    df$area_m2 <- sum(as.vector(area_list))
    df$out_id <- id
    df$out_lon <- new_subcatchment$lon
    df$out_lat <- new_subcatchment$lat
    # TODO: temp
    df$rel_area <- 0
    df$distance <- 0
    
    df$area <- as.numeric(df$area_m2) / 10000 # m2 to ha
    df$n_outlet <- 1
    df$uncheck <- 1
    df$label <- paste("Subc", id)
    
    if (is.null(ws_sf) || is_set_ws_boundary) {
      ws_sf <- df[outlet_columns]
    } else {
      ws_sf <- rbind(ws_sf, df[outlet_columns])
    }
    
    v$outlet_map_sf <- ws_sf
    if (is_set_ws_boundary) {
      v$subcatchment_map_sf <- df[subcathment_columns]
      v$genriver_cfg$total_area_m2 <- df$area_m2
    } else {
      v$subcatchment_map_sf <- rbind(v$subcatchment_map_sf, df[subcathment_columns])
    }
    plot_subcatchment(leafletProxy(active_watershed_lf, session), id)
  }
  
  subcathment_columns <- c("ws_id",
                           "color",
                           "area",
                           "rel_area",
                           "distance",
                           "n_outlet",
                           "uncheck",
                           "label")
  outlet_columns <- c("out_id",
                      "out_lon",
                      "out_lat",
                      "area_m2",
                      "rel_area",
                      "distance",
                      "ws_id",
                      "color")
  
  
  paint_outlet <- function(lf, out_id) {
    sf <- isolate(v$outlet_map_sf)
    if (is.null(sf))
      return(lf)
    sf <- sf[sf$out_id %in% out_id, ]
    label <- map_label(
      paste(
        "ID:",
        sf$out_id,
        "<br>Lon:",
        f_number(sf$out_lon, digits = 8),
        "<br>Lat:",
        f_number(sf$out_lat, digits = 8),
        "<br>Contributed Area:",
        f_number(sf$area_m2 / 10000, digits = 3),
        "ha"
      ),
      "Outlet Point"
    )
    lf <- lf |> removeMarker(out_id) |>
      addAwesomeMarkers(
        sf$out_lon,
        sf$out_lat,
        label = label,
        labelOptions = labelOptions(className = "map_label", offset = c(0, -5)),
        icon = awesomeIcons(
          "tint",
          markerColor = "pink",
          iconColor = theme_color$danger
        ),
        layerId = paste0("out_", out_id),
        group = "outlet"
      )
    return(lf)
  }
  
  ### WATERSHED TABLE #########################
  
  is_update_watershed_table <- reactiveVal(F)
  
  update_watershed_table <- function() {
    print("upddate table")
    default_outlet_map_sf(v$outlet_map_sf)
    is_update_watershed_table(T)
    update_subcatchment_outlet()
  }
  
  update_subcatchment_outlet <- function() {
    print("upddate outlet")
    
    sf <- v$outlet_map_sf
    if (is.null(sf) || nrow(sf) == 0) {
      v$subcatchment_map_sf <- NULL
    } else {
      sf$area <- as.numeric(sf$area_m2) / 10000 # m2 to ha
      sf$distance <- as.numeric(sf$distance) / 1000 # m to km
      sf$n_outlet <- 1
      sf$uncheck <- 1
      sf$label <- paste("Subc", sf$ws_id)
      v$subcatchment_map_sf <- sf[subcathment_columns]
    }
    leafletProxy(active_watershed_lf, session) |>
      clearGroup(group = "subcatchment") |>
      clearMarkers() |>
      plot_subcatchment()
    
    # updateReactable("watershed_list", selected = NA)
  }
  
  observe({
    check_subcatchment_map_sf()
    sf <- v$subcatchment_map_sf
    updateReactable("watershed_list", data = sf[sf$ws_id != 999, ])
  })
  
  output$watershed_list <- renderReactable({
    print("build table")
    # if(is.null(default_outlet_map_sf())) return()
    if (isolate(is_update_watershed_table())) {
      ws_df <- isolate(as.data.frame(v$subcatchment_map_sf)[subcathment_columns])
      ws_df <- ws_df[ws_df$ws_id != 999, ]
      is_update_watershed_table(F)
    } else {
      return()
    }
    isolate(repaint_subcatchment_map())
    reactable(
      ws_df,
      pagination = F,
      compact = TRUE,
      wrap = FALSE,
      rownames = F,
      selection = "multiple",
      defaultSorted = c("uncheck", "ws_id"),
      # onClick = "select",
      theme = reactableTheme(
        backgroundColor = "#FFFFFF00",
        headerStyle = list(backgroundColor = "rgba(255, 255, 255)")
      ),
      columns = list(
        uncheck = colDef(width = 0),
        n_outlet = colDef(width = 0),
        rel_area = colDef(width = 0),
        
        ws_id = colDef(name = "ID", width = 40),
        color = colDef(
          name = "",
          width = 30,
          html = TRUE,
          cell = JS(
            "function(cellInfo) {
              return `<div style='margin:auto;width:20px;height:20px; border-radius:10px 0px 10px 10px;
              border:1px solid black; background-color:${cellInfo.value}'><div>`}"
          )
        ),
        distance = colDef(
          name = "Distance (km)",
          format = colFormat(digits = 1, separators = T),
          width = 0
        ),
        area = colDef(
          name = "Area (ha)",
          format = colFormat(digits = 1, separators = T),
          width = 100
        )
      ),
      details = colDef(
        html = TRUE,
        #TODO: add more detail info if necessary
        details = JS(
          "function(rowInfo) {
            return `<div style='margin:10px'>
            Routing distance: <b>${Number(rowInfo.values.distance).toFixed(2)}</b> km` +
            `<br>Number of outlets: <b>${rowInfo.values.n_outlet}</b><div>`
          }"
        )
      )
      
    )
  })
  
  
  observeEvent(input$reset_subcathment, {
    print("reset outlet")
    toggle_popover("subcatchment_popover", F)
    # updateReactable("watershed_list", selected = NA, data = v$subcatchment_map_sf)
    v$outlet_map_sf <- default_outlet_map_sf()
    update_subcatchment_outlet()
  })
  
  observeEvent(input$regenerate_subcathment, {
    toggle_popover("subcatchment_popover", F)
    show_input_dialog(
      title = "Regenerate Sub-Catchment",
      confirm_id = "confirm_generate_subc",
      confirm_label = "Generate",
      input_var = c("subc_area_threshold"),
      input_label = c("Minimum sub-catchment area (ha)"),
      input_def = c(v$genriver_cfg$flow_threshold),
      input_type = c("numeric")
    )
  })
  
  observeEvent(input$confirm_generate_subc, {
    removeModal()
    process_generate_subcatchment(input$subc_area_threshold)
    leafletProxy(active_watershed_lf, session) |>
      clearGroup(group = "subcatchment") |>
      plot_subcatchment()
    updateReactable("watershed_list", selected = NA)
  })
  
  # output$download_subcathment <- downloadHandler(
  #   filename = function() {
  #     paste("subcatchment_map.zip")
  #   },
  #   content = function(fname) {
  #     setwd(tempdir())
  #     ws_f <- "subcathments"
  #     ws_m <- isolate(v$subcatchment_map_sf)
  #     st_write(ws_m,
  #              paste0(ws_f, ".shp"),
  #              append = F,
  #              quiet = T)
  #     ws_f <- paste0(ws_f, shp_ext)
  #     ou_f <- "outlets"
  #     ou_m <- isolate(v$outlet_map_sf)
  #     st_write(ou_m,
  #              paste0(ou_f, ".shp"),
  #              append = F,
  #              quiet = T)
  #     ou_f <- paste0(ou_f, shp_ext)
  #     return(zip::zip(zipfile = fname, files = c(ws_f, ou_f)))
  #   },
  #   contentType = "application/zip"
  # )
  #
  output$download_subcathment <- download_as_zip("subcatchment_map.zip",
                                                 c("subcatchment_map_sf", "outlet_map_sf"),
                                                 v)
  output$download_stream <- download_as_zip("stream_map.zip", "dem_stream_sf", v)
  output$download_dem <- download_as_zip("dem_map.zip", "dem_map_stars", v)
  
  #### Subcatchment selection #####
  
  selected_ws_ids <- c()
  selected_deleted_ws_ids <- F
  
  observe({
    state <- req(getReactableState("watershed_list"))
    rows <- as.vector(state$selected)
    ws_df <- isolate(v$subcatchment_map_sf)
    if (selected_deleted_ws_ids) {
      selected_ws_ids <<- c()
      selected_deleted_ws_ids <<- F
      updateReactable("watershed_list", selected = NA, data = ws_df)
      return()
    }
    if (is.null(rows)) {
      # deselect last selection
      if (length(selected_ws_ids) > 0) {
        out_df <- as.data.frame(isolate(v$outlet_map_sf))
        out_df <- out_df[out_df$ws_id %in% selected_ws_ids, ]
        # print(out_df$out_id)
        leafletProxy(active_watershed_lf, session) |>
          plot_subcatchment(selected_ws_ids) |>
          removeMarker(paste0("out_", out_df$out_id))
        selected_ws_ids <<- c()
      } else {
        return()
      }
    } else {
      selids <- ws_df$ws_id[rows]
      ids <- setdiff(selids, selected_ws_ids)
      if (length(ids) > 0) {
        out_df <- as.data.frame(isolate(v$outlet_map_sf))
        out_df <- out_df[out_df$ws_id %in% ids, ]
        leafletProxy(active_watershed_lf, session) |>
          plot_subcatchment(ids, theme_color$danger, fillOpacity = 0.9) |>
          paint_outlet(out_df$out_id)
        
      } else {
        ids <- setdiff(selected_ws_ids, selids)
        if (length(ids) > 0) {
          # deselect
          out_df <- as.data.frame(isolate(v$outlet_map_sf))
          out_df <- out_df[out_df$ws_id %in% ids, ]
          leafletProxy(active_watershed_lf, session) |>
            plot_subcatchment(ids) |>
            removeMarker(paste0("out_", out_df$out_id))
        }
      }
      selected_ws_ids <<- selids
      ws_df[rows, "uncheck"] <- 0
      updateReactable("watershed_list", selected = rows, data = ws_df)
    }
    # updateReactable("watershed_list", selected = rows, data = ws_df)
  })
  
  #### Delete subcatchment #######################################
  
  observeEvent(input$delete_outlet_btn, {
    state <- req(getReactableState("watershed_list"))
    rows <- as.vector(state$selected)
    if (length(rows) == 0)
      return()
    selected_deleted_ws_ids <<- T
    df <- v$subcatchment_map_sf
    ids <- df$ws_id[rows]
    v$subcatchment_map_sf <- df[!df$ws_id %in% ids, ]
    out_ids <- v$outlet_map_sf[v$outlet_map_sf$ws_id %in% ids, "out_id"]
    v$outlet_map_sf <- v$outlet_map_sf[!v$outlet_map_sf$ws_id %in% ids, ]
    # remove subc paint
    layer_id <- paste0("ws_", ids)
    leafletProxy(active_watershed_lf, session) |> removeShape(layer_id) |>
      clearMarkers()
  })
  
  #### Merge subcatchment #############################
  
  observeEvent(input$merge_outlet_btn, {
    sf_use_s2(FALSE)
    state <- req(getReactableState("watershed_list"))
    rows <- as.vector(state$selected)
    if (length(rows) == 0)
      return()
    selected_deleted_ws_ids <<- T
    sf <- v$subcatchment_map_sf[rows, ]
    df <- as.data.frame(v$subcatchment_map_sf[rows, ])
    ws_id <- min(df$ws_id)
    ws_color <- df$color[1]
    #TODO: harusnya bikin satu row aja
    sf$ws_id <- ws_id
    sf$color <- df$color[1]
    sf$distance <- df$distance * df$n_outlet
    ws_df <- aggregate(sf[c("area", "distance", "n_outlet")], as.data.frame(sf)[c("ws_id", "color")], sum)
    ws_df$distance <- ws_df$distance / ws_df$n_outlet
    ws_df$uncheck <- 1
    #TODO: hitung
    ws_df$rel_area <- ws_df$area * 10000 / v$genriver_cfg$total_area_m2
    ws_df$label <- paste("Subc", ws_df$ws_id)
    v$subcatchment_map_sf <- rbind(v$subcatchment_map_sf[-rows, ], ws_df[subcathment_columns])
    v$outlet_map_sf[v$outlet_map_sf$ws_id %in% df$ws_id, "ws_id"] <- ws_id
    # remove paint
    layer_id <- paste0("ws_", df$ws_id)
    leafletProxy(active_watershed_lf, session) |> removeShape(layer_id) |>
      clearMarkers() |>
      plot_subcatchment(ws_id)
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
    label <- map_label(paste("ID:", sc$ws_id, "<p>", label, "</p>"),
                       "Sub-Catchment")
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
    show_input_dialog(
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
        if (input$lake_select) {
          desc <- paste("<p><b>Lake</b> and <b>DAM</b> is in this sub-catchment</p>")
        } else {
          desc <- paste("<p><b>DAM</b> is in this sub-catchment</p>")
        }
      }
    } else {
      dam_df <- dam_df[dam_df$ws_id != ws_id, ]
    }
    v$dam_df <- dam_df
    
    if (input$lake_select || input$dam_select) {
      paint_subcatchment(lf, sc, "#034464", "#9a130e", paste(desc, mod_label))
    } else {
      paint_subcatchment(lf, sc, "#ffb703", "#9a130e", add_label)
    }
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
  
  # initialize_ground_par_df <- function() {
  observe({
    #TODO: only if the IDs changing, otherwise ignore!
    if (is.null(v$subcatchment_map_sf) ||
        is.null(v$lc_map_df))
      return()
    print("initialize_ground_par_df")
    ws_ids <- as.data.frame(v$subcatchment_map_sf)$ws_id
    ws_ids <- sort(unique(ws_ids))
    map_ids <- v$lc_map_df$map_id
    map_ids_col <- rep(map_ids, each = length(ws_ids))
    ws_ids_col <- rep(ws_ids, length(map_ids))
    df <- data.frame(map_id = map_ids_col, ws_id = ws_ids_col)
    df[ground_prop_cols$var[1]] <- ground_prop_cols$default[1]
    df[ground_prop_cols$var[2]] <- ground_prop_cols$default[2]
    df[ground_prop_cols$var[3]] <- ground_prop_cols$default[3]
    v$ground_par_df <- df
  })
  
  ground_prop_cols <- data.frame(
    var = c("I_RivFlowTime", "I_MaxDynGWSub", "I_GWRelFrac"),
    default = c(1, 300, 0.1),
    label = c(
      "Relative flow velocity",
      "Max dynamic GW store (mm)",
      "Ground water release fraction"
    )
  )
  
  ground_par_df_edited <- table_edit_server(
    "ground_water_table",
    reactive(v$ground_par_df),
    col_title = c("map_id", "ws_id", ground_prop_cols$label),
    col_type = c("character", rep("numeric", 4)),
    col_width = c(50, 50, 250, 250, 250),
    col_disable = c(T, T, F, F, F)
  )
  
  observe({
    v$ground_par_df <- ground_par_df_edited()
  })
  
  ### 3D VIEW ####################################
  
  output$ws3d_plot <- renderPlotly({
    dem_ws <- v$dem_crop_stars
    if (is.null(dem_ws))
      return()
    delta_m <- log(cellSize.stars(dem_ws))
    plot_ly(
      z = dem_ws[[1]],
      colorscale = 'Portland',
      colorbar = list(title = "<b>Elevation</b>", x = 0.9, y = 0.9),
      hoverinfo = "z",
      name = "ASL",
      hovertemplate =  "Elevation: %{z} m"
    ) |> add_surface() |>
      layout(
        scene = list(
          aspectratio = list(x = delta_m, y = delta_m, z = 1),
          camera = list(eye = list(
            x = 4, y = 4, z = 4
          ))
        ),
        margin = list(
          l = 0,
          r = 0,
          b = 0,
          t = 0,
          pad = 0
        )
      )
  })
  
  # observeEvent(input$generate_3d_button, {
  #   is_show_3d(T)
  # })
  # 
  # output$plot3d <- renderRglwidget({
  #   dem <- v$dem_crop_stars
  #   if (is.null(dem))
  #     return()
  #   try(close3d())
  #   showPageSpinner(caption = "Please wait while rendering 3D view")
  #   dem <- st_as_stars(dem)
  #   m_mat <- dem[[1]]
  #   m_mat[is.na(m_mat)] <- min(m_mat, na.rm = T)
  #   m_mat %>%
  #     sphere_shade(texture = "desert") %>%
  #     add_shadow(ray_shade(m_mat, zscale = 3), 0.5) %>%
  #     add_shadow(ambient_shade(m_mat), 0) %>%
  #     plot_3d(
  #       m_mat,
  #       zscale = 10,
  #       fov = 60,
  #       theta = 135,
  #       zoom = 0.75,
  #       phi = 45,
  #       triangulate = T,
  #       max_tri = 15000
  #     )
  #   hidePageSpinner()
  #   rglwidget()
  # })
  
  ### SOIL PROPERTIES ####################################
  
  # observe({
  #   if (is.null(v$dem_crop_stars))
  #     return()
  #   generate_slope_classes()
  # })
  
  
  
  
  generate_slope_classes <- function(filter_width = 25,
                                     noise_removal = 1) {
    print("generate_slope_classes")
    dem <- v$dem_crop_stars
    if (is.null(dem))
      return()
    slope <- terrain(stars_to_terra(dem))
    #TODO: convert to proxy
    vd$slope_stars <- stars_to_proxy(st_as_stars(slope))
    #smoothing the slope map
    slope2 <- focal(
      slope,
      w = filter_width,
      fun = "mean",
      na.policy = "omit",
      na.rm = T
    )
    slope2 <- st_as_stars(slope2)
    min <- min(slope2[[1]], na.rm = T)
    max <- max(slope2[[1]], na.rm = T)
    #standardize slope factor in reverse
    vd$slope_factor_stars <- 1 - ((slope2 - min) / (max - min))
    #get available soil types
    sdf <- v$soil_layer_df[c("SMU_ID", "SOIL", "SHARE", "TOPDEP", "ORG_CARBON")]
    sdf <- sdf[sdf$TOPDEP == 0, ]
    # sdf <- sdf[order(sdf$SHARE, sdf$ORG_CARBON, decreasing = T),]
    # sdf <- sdf[c(1:nd), ]
    sdf <- sdf[order(sdf$ORG_CARBON, decreasing = T), ]
    #generate slope classes
    del_elv <- max - min
    nd <- nrow(sdf)
    #class range in power scale, by default
    #TODO: to be user defined scale
    d <- min + del_elv * c(1:nd) ^ 2 / nd ^ 2
    slope_class <- slope2
    slope_class[!is.na(slope_class)] <- 1
    for (i in 1:(nd - 1)) {
      slope_class[slope2 >= d[i]] <- i + 1
    }
    slope_class2 <- slope_class
    for (i in 1:noise_removal) {
      slope_class2 <- focal(
        stars_to_terra(slope_class2),
        w = filter_width,
        fun = "modal",
        na.policy = "omit",
        na.rm = T
      )
    }
    slope_class2 <- st_as_stars(slope_class2)
    
    sc_sf <- st_as_sf(slope_class2, merge = T, connect8 = T)
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
    # print("done slope")
    # print(sc_sf)
    # v$slope_class_sf <- sc_sf
  }
  
  #### Soil Type Leaflet ####################
  
  output$soil_type_leaflet <- renderLeaflet({
    m <- v$dem_stream_sf
    if (is.null(m))
      return()
    print("soil_type_leaflet")
    bb <- st_bbox(m)
    s <- (bb[["xmax"]] - bb[["xmin"]]) / 5
    bb <- st_bbox(c(
      xmin = bb[["xmin"]] + s,
      xmax = bb[["xmax"]] + s,
      ymax = bb[["ymax"]],
      ymin = bb[["ymin"]]
    ), crs = st_crs(bb))
    base_leaflet() |> fit_map_view(bb) |>
      addHomeButton(st_bbox(bb), "Home", "topleft") |>
      show_stream(m, opacity = 0.6, is_show_label = F) |> repaint_segments()
  })
  
  
  repaint_segments <- function(lf = NULL, segment_id = NULL) {
    if (isolate(v$soil_cfg$soil_type_select) == "soil_type_user") {
      m <- isolate(v$soil_segments_sf)
      st_df <- isolate(v$soil_type_df)
    } else {
      m <- isolate(v$soil_segments_global_sf)
      st_df <- isolate(vd$soil_type_global_df)
    }
    if (is.null(m))
      return()
    print("repaint_segments")
    if (is.null(lf))
      lf <- leafletProxy("soil_type_leaflet", session)
    if (!is.null(segment_id)) {
      m <- m[m$segment_id %in% segment_id, ]
      lf <- lf |> removeShape(paste0("segment_", segment_id))
      # print(m)
    } else {
      lf <- lf |> clearGroup("segment")
    }
    
    st_df$soil_color <- st_df$color
    # print(st_df)
    m <- merge(m, st_df[c("soil_id", "soil_color", "SOIL")], by = "soil_id", all.x = T)
    m[is.na(m$soil_color), "soil_color"] <-  ""
    m[!is.na(m$soil_id), "color"] <-  ""
    m$color <- paste(m$color, m$soil_color)
    get_soil <- function(x) {
      if (is.na(x["soil_id"])) {
        return("<div class='highlight_label'>Click here to set the soil type</div>")
      }
      paste0("Soil type: <div style='font-size:1.5em;'>[",
             x["soil_id"],
             "] ",
             x["SOIL"],
             "</div>")
    }
    
    label <- map_label(
      paste(
        "<div>Segment ID:",
        m$segment_id,
        "<br>Class ID:",
        m$class,
        "<br>Area:",
        lapply(m$area, function(x) {
          f_number(x, digits = 3)
        }),
        "ha</div>",
        apply(m[c("soil_id", "SOIL")], 1, get_soil)
      ),
      "Soil Map"
    )
    ids <- paste0("segment_", m$segment_id)
    
    lf |>
      addFeatures(
        m,
        color = "#000",
        fillColor = ~ color,
        fillOpacity = 0.8,
        opacity = 0.5,
        weight = 1,
        group = "segment",
        layerId = ids,
        label = label,
        labelOptions = labelOptions(
          className = "map_label",
          offset = c(0, -10),
          direction = "top"
        ),
        highlightOptions = highlightOptions(fillColor = "#fb8500")
      )
  }
  
  
  selected_segment <- NULL
  
  observeEvent(input$soil_type_leaflet_shape_click, {
    inp <- input$soil_type_leaflet_shape_click
    if (inp$group == "segment" &&
        input$soil_type_select == "soil_type_user") {
      df <- v$soil_type_df
      if (nrow(df) == 0) {
        showModal(
          modalDialog(
            title = "The Soil Type List Is Empty!",
            "Please fill in the soil type including its properties on the provided tabel"
          )
        )
        nav_select("soil_type_panel", "Soil Type List")
        return()
      }
      selected_segment <<- suffix(inp$id)
      
      ops <- as.list(df$soil_id)
      names(ops) <- df$SOIL
      show_input_dialog(
        "Set Soil Type",
        custom_input = selectInput(
          "soil_segment_select",
          paste("Select soil type for segment", selected_segment),
          ops
        ),
        confirm_id = "confirm_soil_type"
      )
    }
  })
  
  observeEvent(input$confirm_soil_type, {
    removeModal()
    soil_id <- input$soil_segment_select
    # print(soil_id)
    
    v$soil_segments_df[v$soil_segments_df$segment_id == selected_segment, "soil_id"] <- soil_id
    v$soil_segments_sf[v$soil_segments_sf$segment_id == selected_segment, "soil_id"] <- soil_id
    repaint_segments(segment_id = selected_segment)
  })
  
  #"Slope (\u00B0)"
  output$slope_map_plot_title <- renderText({
    m <- st_as_stars(vd$slope_stars)
    min <- min(m[[1]], na.rm = T)
    max <- max(m[[1]], na.rm = T)
    paste0("Slope Map (", f_number(min, digits = 3),"\u00B0 - ", f_number(max, digits = 3), "\u00B0)")
  })
  
  output$slope_map_plot <- renderPlot({
    if (is.null(vd$slope_stars))
      return()
    suppressMessages(plot(
      vd$slope_stars,
      main = NULL,
      col = slope_color(100),
      breaks = "equal"
    ))
  }, bg = "transparent")
  
  output$elevation_map_plot_title <- renderText({
    m <- st_as_stars(v$dem_crop_stars)
    min <- min(m[[1]], na.rm = T)
    max <- max(m[[1]], na.rm = T)
    paste0("Elevation Map (", f_number(min, digits = 3)," - ", f_number(max, digits = 3), " m)")
  })
  
  output$elevation_map_plot <- renderPlot({
    if (is.null(v$dem_crop_stars))
      return()
    suppressMessages(plot(
      v$dem_crop_stars,
      main = NULL,
      col = terrain.colors(100),
      breaks = "equal"
    ))
  }, bg = "transparent")
  
  output$depth_map_plot_title <- renderText({
    min <- input$min_soil_depth_input
    max <- input$max_soil_depth_input
    paste0("Soil Depth Map (", f_number(min, digits = 3)," - ", f_number(max, digits = 3), " cm)")
  })
  
  output$depth_map_plot <- renderPlot({
    m <- vd$soil_depth_stars
    if (is.null(m))
      return()
    if (mean(m[[1]], na.rm = T) == isolate(v$soil_cfg$depth_max)) {
      suppressMessages(plot(
        m,
        main = NULL,
        col = depth_color(3)[3],
        breaks = "equal"
      ))
    } else {
      suppressMessages(plot(
        m,
        main = NULL,
        col = depth_color(100),
        breaks = "equal"
      ))
    }
  }, bg = "transparent")
  
  output$soil_map_leaflet <- renderLeaflet({
    lf <- base_leaflet()
    if (is.null(v$soil_map_sf)) {
      if (!is.null(v$ws_boundary_sf)) {
        lf <- lf |> addFeatures(
          v$ws_boundary_sf,
          layerId = 'ws_boundary',
          color = theme_color$secondary,
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
  
  observe({
    v$soil_cfg$nclass <- input$n_class_input
    v$soil_cfg$depth_min <- max(0, input$min_soil_depth_input)
    v$soil_cfg$depth_max <- max(10, input$max_soil_depth_input)
    v$soil_cfg$depth_coe <- input$depth_coe_segment_input
    v$soil_cfg$depth_exp <- input$depth_exp_segment_input
    v$soil_cfg$slope_filter_width <- input$slope_filter_input
    v$soil_cfg$slope_coe <- input$slope_coe_input
    v$soil_cfg$slope_exp <- input$slope_exp_input
    v$soil_cfg$elevation_filter_width <- input$elevation_filter_input
    v$soil_cfg$elevation_coe <- input$elevation_coe_input
    v$soil_cfg$elevation_exp <- input$elevation_exp_input
    v$soil_cfg$noise_removal <- input$soil_noise_input
    # reset soil water
    v$subc_lc_df <- NULL
  })
  
  
  update_soil_ui <- function() {
    updateNumericInput(session, "n_class_input", value = v$soil_cfg$nclass)
    updateNumericInput(session,
                       "min_soil_depth_input",
                       value = v$soil_cfg$depth_min)
    updateNumericInput(session,
                       "max_soil_depth_input",
                       value = v$soil_cfg$depth_max)
    updateNumericInput(session,
                       "top_soil_prop_input",
                       value = v$soil_cfg$top_soil_prop * 100)
    updateNumericInput(session,
                       "depth_coe_segment_input",
                       value = v$soil_cfg$depth_coe)
    updateNumericInput(session,
                       "depth_exp_segment_input",
                       value = v$soil_cfg$depth_exp)
    updateNumericInput(session,
                       "slope_filter_input",
                       value = v$soil_cfg$slope_filter_width)
    updateNumericInput(session, "slope_coe_input", value = v$soil_cfg$slope_coe)
    updateNumericInput(session, "slope_exp_input", value = v$soil_cfg$slope_exp)
    updateNumericInput(session,
                       "elevation_filter_input",
                       value = v$soil_cfg$elevation_filter_width)
    updateNumericInput(session,
                       "elevation_coe_input",
                       value = v$soil_cfg$elevation_coe)
    updateNumericInput(session,
                       "elevation_exp_input",
                       value = v$soil_cfg$elevation_exp)
    updateNumericInput(session,
                       "soil_noise_input",
                       value = v$soil_cfg$noise_removal)
    updateSelectInput(session,
                      "soil_type_select",
                      selected = v$soil_cfg$soil_type_select)
    # reset soil water
    v$subc_lc_df <- NULL
  }
  
  
  observe({
    dem <- v$dem_crop_stars
    if (is.null(dem))
      return()
    slope <- terrain(stars_to_terra(dem))
    vd$slope_stars <- stars_to_proxy(st_as_stars(slope))
  })
  
  observe({
    if (is.null(vd$slope_stars))
      return()
    slope <- stars_to_terra(vd$slope_stars)
    #smoothing the slope map
    w <- input$slope_filter_input
    if (is.na(w))
      return()
    if (w <= 1) {
      slope2_stars <- st_as_stars(vd$slope_stars)
    } else {
      if (w %% 2 == 0)
        w <- w + 1
      w <- max(w, 3)
      slope2 <- focal(
        slope,
        w = w,
        fun = "mean",
        na.policy = "omit",
        na.rm = T
      )
      slope2_stars <- st_as_stars(slope2)
    }
    min <- min(slope2_stars[[1]], na.rm = T)
    max <- max(slope2_stars[[1]], na.rm = T)
    vd$slope_std_stars <- ((slope2_stars - min) / (max - min))
  })
  
  output$slope_std_plot <- renderPlot({
    if (is.null(vd$slope_std_stars))
      return()
    suppressMessages(plot(
      vd$slope_std_stars,
      main = NULL,
      breaks = "equal",
      key.pos = NULL
    ))
  }, bg = "transparent")
  
  observe({
    if (is.null(v$dem_crop_stars))
      return()
    m <- stars_to_terra(v$dem_crop_stars)
    w <- input$elevation_filter_input
    if (is.na(w))
      return()
    if (w <= 1) {
      m2_stars <- st_as_stars(v$dem_crop_stars)
    } else {
      if (w %% 2 == 0)
        w <- w + 1
      w <- max(w, 3)
      m2 <- focal(
        m,
        w = w,
        fun = "mean",
        na.policy = "omit",
        na.rm = T
      )
      m2_stars <- st_as_stars(m2)
    }
    min <- min(m2_stars[[1]], na.rm = T)
    max <- max(m2_stars[[1]], na.rm = T)
    vd$elevation_std_stars <- ((m2_stars - min) / (max - min))
  })
  
  output$elevation_std_plot <- renderPlot({
    if (is.null(vd$elevation_std_stars))
      return()
    suppressMessages(plot(
      vd$elevation_std_stars,
      main = NULL,
      breaks = "equal",
      key.pos = NULL
    ))
  }, bg = "transparent")
  
  observe({
    if (is.null(vd$slope_std_stars) ||
        is.null(vd$elevation_std_stars))
      return()
    sf <- 1 - vd$slope_std_stars
    ef <- 1 - vd$elevation_std_stars
    st_dimensions(ef) <- st_dimensions(sf)
    d <- input$slope_coe_input * sf ^ input$slope_exp_input + input$elevation_coe_input * ef ^ input$elevation_exp_input
    if (sum(d[[1]], na.rm = T) > 0) {
      min <- min(d[[1]], na.rm = T)
      max <- max(d[[1]], na.rm = T)
      m <- ((d - min) / (max - min))
      min_d <- input$min_soil_depth_input
      m2 <- min_d + m * (input$max_soil_depth_input - min_d)
      vd$soil_depth_std_stars <- m
      vd$soil_depth_stars <- m2
    } else {
      d[!is.na(d)] <- 1
      vd$soil_depth_std_stars <- d
      vd$soil_depth_stars <- d * input$max_soil_depth_input
    }
  })
  
  #### Soil Segments ################
  
  # classify_map <- function(m, nclass, name = "class") {
  #   sd <- c(1:nclass)/nclass
  #   mc <- m
  #   mc[!is.na(mc)] <- 1
  #   for (i in 2:nclass) {
  #     mc[m > sd[i-1]] <- i
  #   }
  #   sf <- st_as_sf(mc, merge = T, connect8 = T)
  #   sf <- st_transform(sf, crs = st_crs(m))
  #   names(sf) <- c(name, "geometry")
  #   return(sf)
  # }
  
  observe({
    sm <- vd$slope_std_stars
    em <- vd$elevation_std_stars
    dm <- vd$soil_depth_std_stars
    if (is.null(sm) || is.null(em) || is.null(dm))
      return()
    print("update soil_segments_sf")
    dm <- 1 - dm
    
    st_dimensions(em) <- st_dimensions(sm)
    st_dimensions(dm) <- st_dimensions(sm)
    
    a_s <- input$slope_coe_segment_input
    b_s <- input$slope_exp_segment_input
    a_e <- input$elevation_coe_segment_input
    b_e <- input$elevation_exp_segment_input
    a_d <- input$depth_coe_segment_input
    b_d <- input$depth_exp_segment_input
    nclass <- input$n_class_input
    
    noise_removal <- input$noise_rep_input
    filter_width <- input$noise_filter_input
    
    if (is.na(filter_width) || is.na(noise_removal))
      return()
    
    d <- a_s * sm ^ b_s  + a_e * em ^ b_e + a_d * dm ^ b_d
    min <- min(d[[1]], na.rm = T)
    max <- max(d[[1]], na.rm = T)
    m <- ((d - min) / (max - min))
    
    nclass <- max(1, nclass)
    sd <- c(1:nclass) / nclass
    mc <- m
    mc[!is.na(mc)] <- 1
    if (nclass > 1) {
      for (i in 2:nclass) {
        mc[m > sd[i - 1]] <- i
      }
    }

    if (filter_width > 1 && noise_removal > 0) {
      mc2 <- stars_to_terra(mc)
      if (filter_width %% 2 == 0)
        filter_width <- filter_width + 1
      for (i in 1:noise_removal) {
        mc2 <- focal(
          mc2,
          w = filter_width,
          fun = "modal",
          na.policy = "omit",
          na.rm = T
        )
      }
      mc <- st_as_stars(mc2)
    }
    
    sf <- st_as_sf(mc, merge = T, connect8 = T)
    sf <- st_transform(sf, crs = st_crs(m))
    names(sf) <- c("class", "geometry")
    color <- gray.colors(nclass, 0.1)
    sf$color <- color[sf$class]
    sf$area <- as.numeric(st_area(sf) / 10000)
    sf <- sf[order(sf$area, decreasing = T), ]
    sf$segment_id <- c(1:nrow(sf))
    sf$soil_id <- NA
    print("update soil_segments_sf")
    v$soil_segments_sf <- sf
    update_soil_segment_table()
    update_soil_map_global()
    repaint_segments()
  })
  
  update_soil_segment_table <- function() {
    print("update_soil_segment_table")
    sf <- isolate(v$soil_segments_sf)
    if (is.null(sf))
      return()
    df <- isolate(v$soil_segments_df)
    if (is.null(df) || nrow(df) == 0) {
      sf <- sf[c("segment_id", "class", "area", "soil_id")]
      sf$soil_id <- NA
    } else {
      sf <- merge(sf[c("segment_id", "color", "class", "area")], df[c("segment_id", "soil_id")], by = c("segment_id"), all.x = T)
      v$soil_segments_sf <- sf
    }
    v$soil_segments_df <- as.data.frame(sf)[c("segment_id", "class", "area", "soil_id")]
  }
  
  soil_mapped_table_edit <- table_edit_server(
    "soil_mapped_table_user",
    reactive(v$soil_segments_df),
    col_type = rep("numeric", 4),
    col_disable = c(T, T, T, F)
  )
  
  observe({
    v$soil_segments_df <- soil_mapped_table_edit()
  })
  
  observe({
    v$soil_segments_sf$soil_id <- v$soil_segments_df$soil_id
  })

  output$soil_mapped_table_global <- renderReactable({
    df <- as.data.frame(v$soil_segments_global_sf)
    reactable(
      df[c("segment_id", "class", "area", "soil_id")],
      columns = list(
        segment_id = colDef(name = "ID", width = 40),
        class = colDef(name = "Class ID"),
        area = colDef(name = "Area (ha)", format = colFormat(digit = 0)),
        soil_id = colDef(name = "Soil ID")
      ),
      pagination = F
    )
  })
  
  update_soil_map_global <- function() {
    print("update_soil_map_global")
    sf <- isolate(v$soil_segments_sf)
    if (is.null(sf))
      return()
    msf <- isolate(v$soil_map_sf)
    sid <- sort(unique(msf$smu_id))
    suppressWarnings(suppressMessages(intsf <- st_intersection(sf, msf)))
    sl_df <- v$soil_layer_df
    s1 <- sl_df[sl_df$LAYER == "D1", c("SMU_ID", "SOIL", "SHARE", "ORG_CARBON")]
    s1 <- s1[, colSums(!is.na(s1)) > 0]
    s1 <- s1[!duplicated(s1), ]
    s1 <- merge(s1,
                vd$soil_type_global_df[c("soil_id", "color", "SOIL")],
                by = "SOIL",
                all.x = T)
    prevseg <- c()
    for (smu_id in sid) {
      sdf <- s1[s1$SMU_ID == smu_id, ]
      sdf <- sdf[order(sdf$SHARE, sdf$ORG_CARBON, decreasing = T), ]
      map <- intsf[intsf$smu_id == smu_id, ]
      # intersected area
      map$area_x <- as.numeric(st_area(map) / 10000)
      map$a_ratio <- map$area_x / map$area
      # get only intersected segment with more than 50% of original area
      map <- map[map$a_ratio > 0.5, ]
      map <- map[!map$segment_id %in% prevseg, ]
      map <- map[order(map$class, map$segment_id), ]
      map$cumsum <- cumsum(map$area)
      prevseg <- c(prevseg, map$segment_id)
      tot_area <- sum(map$area)
      prevshare <- 0
      for (soil_id in sdf$soil_id) {
        totshare <- prevshare + tot_area * sdf[sdf$soil_id == soil_id, "SHARE"] /
          100
        m2 <- map[map$cumsum > prevshare &
                    map$cumsum <= totshare + 0.00001, ]
        sf[sf$segment_id %in% m2$segment_id, "soil_id"] <- soil_id
        sf[sf$segment_id %in% m2$segment_id, "color"] <- sdf[sdf$soil_id == soil_id, "color"]
        prevshare <- totshare
      }
    }
    v$soil_segments_global_sf <- sf
  }
  
  #### Soil Type Table ###############
  
  get_soil_db_metadata <- function() {
    db <- dbConnect(RSQLite::SQLite(), soil_db_path)
    qm <- paste("SELECT * FROM HWSD2_LAYERS_METADATA")
    metadata_df <- dbGetQuery(db, qm)
    metadata_df$FIELD <- trimws(metadata_df$FIELD)
    dbDisconnect(db)
    return(metadata_df)
  }
  
  soil_db_metadata <- get_soil_db_metadata()
  
  get_soil_db_header <- function(x) {
    df <- soil_db_metadata[soil_db_metadata$FIELD %in% x, ]
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
  }
  
  div_h_style <- "<div style='font-weight:bold;text-align:center;background-color:#ECF9FC;color:#219ebc;padding:5px 0'>"
  hydro_prop_h <- unlist(lapply(hydro_prop, get_soil_db_header))
  soil_type_h <- c("ID", "-", "Soil", hydro_prop_h, hydro_prop_h)
  
  soil_type_table_edit <- table_edit_server(
    "soil_type_table",
    reactive(v$soil_type_df),
    col_title = soil_type_h,
    col_type = c("numeric", "character", "character", rep("numeric", 16)),
    allowRowModif = T,
    nrow = 10,
    digits = 2,
    nestedHeaders = list(data.frame(
      title = paste0(
        div_h_style,
        c("Soil Type</div>", "Top Soil Properties</div>", "Sub Soil Properties</div>")
      ),
      colspan = c(3, 8, 8)
    ))
  )
  
  observe({
    v$soil_type_df <- soil_type_table_edit()
  })
  
  observe({
    top_soil <- min(1, max(0, input$top_soil_prop_input / 100))
    v$soil_cfg$top_soil_prop <- top_soil
    vd$soil_water_content_user_df <- calculate_soil_water_table(v$soil_type_df, top_soil)
    isolate(update_soil_water_content())
    # reset soil water
    v$subc_lc_df <- NULL
  })
  
  soil_type_top_cols <- c("soil_id", "color", "SOIL", hydro_prop)
  soil_type_sub_cols <- c("soil_id", "color", "SOIL", paste0("SUB_", hydro_prop))
  soil_type_cols <- c(soil_type_top_cols, paste0("SUB_", hydro_prop))
  
  observe({
    if (is.null(v$soil_type_df)) {
      v$soil_type_df <- setNames(data.frame(matrix(ncol = 19, nrow = 0)), soil_type_cols)
    }
  })
  
  output$soil_type_global_table <- renderReactable({
    df <- vd$soil_type_global_df
    if (is.null(df))
      return()
    h1 <- hydro_prop_coldef(hydro_prop)
    h2 <- h1
    names(h2) <- paste0("SUB_", hydro_prop)
    
    reactable(
      df,
      columns = c(
        list(
          soil_id = colDef(name = "ID", format = colFormat(digit = 0), width = 40),
          SOIL = colDef(name = "Soil", width = 160),
          color = colDef(
            name = "",
            width = 30,
            html = TRUE,
            cell = JS(
              "function(cellInfo) {
              return `<div style='margin:auto;width:20px;height:20px; border-radius:3px;
              background-color:${cellInfo.value}'><div>`}"
            )
          )
        ),
        h1,
        h2
      ),
      columnGroups = list(
        colGroup(name = "Soil Type", columns = c("soil_id", "color", "SOIL"), headerClass = "soil_group"),
        colGroup(name = "Top Soil Properties", columns = hydro_prop, headerClass = "soil_group"),
        colGroup(name = "Sub Soil Properties", columns = paste0("SUB_", hydro_prop), headerClass = "soil_group")
      ),
      defaultColDef = colDef(format = colFormat(digit = 2))
    )
  })
  
  observeEvent(input$import_soil_button, {
    v$soil_type_df <- vd$soil_type_global_df
  })
  
  
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
    
    sdf$layer_share <- (sdf$BOTDEP - sdf$TOPDEP) / max(sdf$BOTDEP)
    
    data_top <- sdf[sdf$soil_depth == "0-20", c("SOIL", hydro_prop)]
    data_sub <- sdf[sdf$soil_depth != "0-20", c("SOIL", hydro_prop, "layer_share")]
    
    data_sub[hydro_prop] <- data_sub[hydro_prop] *  data_sub$layer_share
    data_sub <- aggregate(data_sub[c(hydro_prop, "layer_share")], by = data_sub["SOIL"], sum, na.rm = T)
    data_sub[hydro_prop] <- data_sub[hydro_prop] / data_sub$layer_share
    data_sub <- data_sub[c("SOIL", hydro_prop)]
    
    # data_sub <- aggregate(data_sub[hydro_prop], by = data_sub["SOIL"], mean, na.rm = T)
    names(data_sub) <- c("SOIL", paste0("SUB_", hydro_prop))
    df <- merge(data_top, data_sub, by = c("SOIL"))
    df$soil_id <- c(1:nrow(data_top))
    df$color <- get_color(df$soil_id)
    vd$soil_type_global_df <- df[soil_type_cols]
    vd$soil_water_content_global_df <- calculate_soil_water_table(vd$soil_type_global_df, sdf[sdf$soil_depth == "0-20", "layer_share"])
    update_soil_water_content()
  })
  
  
  calculate_soil_water_table <- function(soil_df, top_depth) {
    if (is.null(soil_df))
      return()
    soil_df <- soil_df[complete.cases(soil_df), ]
    if (nrow(soil_df) == 0)
      return()
    print("calculate_soil_water_table")
    #LC properties
    lc_par_df <- v$lc_par_df
    #Calculate hydraulic properties of top soil
    top_soil_df <- soil_df[soil_type_top_cols]
    nsid <- nrow(top_soil_df)
    nlc <- nrow(lc_par_df)
    rep_lc <- lc_par_df[rep(seq_len(nrow(lc_par_df)), nsid), ]
    rep_s <- top_soil_df[rep(seq_len(nrow(top_soil_df)), each = nlc), ]
    df <- cbind(rep_lc, rep_s)
    df$bulk_density <- df$I_BD_BDRefVegNow * df$REF_BULK
    df$part <- "top"
    
    sub_soil_df <- soil_df[soil_type_sub_cols]
    colnames(sub_soil_df) <- soil_type_top_cols
    sub_soil_df$bulk_density <- sub_soil_df$BULK
    sub_soil_df$part <- "sub"
    sub_soil_df$lc_id <- ""
    scols <- c(soil_type_top_cols, "bulk_density", "lc_id", "part")
    df <- rbind(df[scols], sub_soil_df[scols])
    
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
    # I_SoilSatMinFC
    df["soil_quick_flow_capacity"] <- df["soil_saturation"] - df["field_capacity"]
    # I_PlantAvWat
    df["plant_available_water"] <- df["field_capacity"] - df["permanent_wilting_point"]
    # I_PWP
    df["inaccessible_water"] <- df["permanent_wilting_point"]
    
    swt <- c("soil_quick_flow_capacity",
             "plant_available_water",
             "inaccessible_water")
    
    top_df <- df[df$part == "top", c("soil_id", "SOIL", "lc_id", swt)]
    names(top_df) <- c("soil_id", "soil", "lc_id", paste0("top_", swt))
    sub_df <- df[df$part == "sub", c("soil_id", swt)]
    names(sub_df) <- c("soil_id", paste0("sub_", swt))
    
    all_df <- merge(top_df,
                    sub_df,
                    by = c("soil_id"),
                    all.x = T)
    all_df[swt] <- all_df[paste0("top_", swt)] * top_depth + all_df[paste0("sub_", swt)] * (1 -
                                                                                              top_depth)
    return(all_df[c("soil_id", "soil", "lc_id", swt)])
  }
  
  observeEvent(input$soil_type_select, {
    v$soil_cfg$soil_type_select <- input$soil_type_select
    update_soil_water_content()
    repaint_segments()
    # reset soil water
    v$subc_lc_df <- NULL
  })
  
  update_soil_water_content <- function() {
    s <- isolate(v$soil_cfg$soil_type_select)
    if (is.null(s))
      return()
    if (s == "soil_type_user") {
      v$soil_water_content_df <- vd$soil_water_content_user_df
    } else {
      v$soil_water_content_df <- vd$soil_water_content_global_df
    }
  }
  
  
  
  
  
  update_soil_legend <- reactiveVal(F)
  
  # observe({
  #   if (update_soil_legend())
  #     update_soil_legend(F)
  #   min_d <- input$min_soil_depth_input
  #   max_d <- input$max_soil_depth_input
  #   v$genriver_cfg$min_soil_depth <- min_d
  #   v$genriver_cfg$max_soil_depth <- max_d
  #
  #   val <- c(min_d:max_d)
  #   pal <- colorNumeric(rev(soil_color(10)), val, na.color = "transparent")
  #   leafletProxy("soil_depth_leaflet", session) |>
  #     removeControl("soil_depth_legend") |>
  #     addLegend(
  #       layerId = "soil_depth_legend",
  #       pal = pal,
  #       values = val,
  #       opacity = 0.8,
  #       title = "Soil Depth (cm)"
  #     )
  # })
  
  # output$soil_depth_leaflet <- renderLeaflet({
  #   m <- vd$slope_factor_stars
  #   if (is.null(m))
  #     return()
  #   lf <- base_leaflet() |>
  #     addGeoRaster(
  #       m,
  #       colorOptions = colorOptions(palette = rev(soil_color(256))),
  #       opacity = 0.8,
  #       layerId = 'soil_depth_map'
  #     ) |>
  #     fit_map_view(m) |>
  #     addHomeButton(ext = st_bbox(m),
  #                   group = "Home",
  #                   position = "topleft")
  #   update_soil_legend(T)
  #   return(lf)
  # })
  #
  # output$slope_map_leaflet <- renderLeaflet({
  #   m <- vd$slope_stars
  #   if (is.null(m))
  #     return()
  #   m <- st_as_stars(m)
  #   val <- range(m[[1]], na.rm = T)
  #   pal <- colorNumeric(grad_color(10), val, na.color = "transparent")
  #   lf <- base_leaflet() |>
  #     addGeoRaster(
  #       m,
  #       colorOptions = colorOptions(palette = grad_color(256)),
  #       opacity = 0.8,
  #       layerId = 'slope_map'
  #     ) |>
  #     fit_map_view(m) |>
  #     addHomeButton(ext = st_bbox(m),
  #                   group = "Home",
  #                   position = "topleft") |>
  #     addLegend(
  #       layerId = "slope_legend",
  #       pal = pal,
  #       values = val,
  #       opacity = 0.8,
  #       title = "Slope (deg)"
  #     )
  #   return(lf)
  # })
  
  
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
  
  # get_soil_header <- function(x) {
  #   df <- v$soil_metadata_df[v$soil_metadata_df$FIELD %in% x, ]
  #   df[match(x, df$FIELD), ]
  #   paste0(df$DESCRIPTION, ifelse(
  #     is.na(df$UNIT) |
  #       df$UNIT == "",
  #     "",
  #     paste0(
  #       " <span style='color:#BBBBBB;font-weight: normal;'>[",
  #       gsub("\\s", "", df$UNIT),
  #       "]</span>"
  #     )
  #   ))
  # }
  
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
    d$header <- lapply(hlist, get_soil_db_header)
    # d$header <- lapply(hlist, function(x) {
    #   df <- v$soil_metadata_df[v$soil_metadata_df$FIELD %in% x, ]
    #   df[match(x, df$FIELD), ]
    #   paste0(df$DESCRIPTION, ifelse(
    #     is.na(df$UNIT) |
    #       df$UNIT == "",
    #     "",
    #     paste0(
    #       " <span style='color:#BBBBBB;font-weight: normal;'>[",
    #       gsub("\\s", "", df$UNIT),
    #       "]</span>"
    #     )
    #   ))
    # })
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
  
  # hydro_id <- c("SMU_ID",
  #               "SHARE",
  #               "soil_type",
  #               "SOIL",
  #               "soil_depth",
  #               "TOPDEP",
  #               "BOTDEP")
  # hydro_prop <- c("SAND",
  #                 "SILT",
  #                 "CLAY",
  #                 "BULK",
  #                 "REF_BULK",
  #                 "ORG_CARBON",
  #                 "CEC_SOIL",
  #                 "PH_WATER")
  
  # observe({
  #   sl_df <- v$soil_layer_df
  #   if (is.null(sl_df) || nrow(sl_df) == 0)
  #     return()
  #   #Soil properties
  #   sdf <- sl_df[c(hydro_id, hydro_prop)]
  #   sdf <- sdf[!duplicated(sdf), ]
  #   sdf[sdf < 0] <- NA
  #   if (any(is.na(sdf))) {
  #     agg_type_df <- aggregate(sdf[hydro_prop], list(
  #       SMU_ID = sdf$SMU_ID,
  #       soil_type = sdf$soil_type
  #     ), function(x)
  #       mean(x, na.rm = T))
  #     for (id in unique(agg_type_df$SMU_ID)) {
  #       for (st in agg_type_df$soil_type) {
  #         if (any(is.na(sdf[sdf$SMU_ID == id & sdf$soil_type == st, ]))) {
  #           for (coln in hydro_prop) {
  #             vdf <- sdf[sdf$SMU_ID == id & sdf$soil_type == st, coln]
  #             vdf[is.na(vdf)] <- agg_type_df[agg_type_df$SMU_ID == id &
  #                                              agg_type_df$soil_type == st, coln]
  #             sdf[sdf$SMU_ID == id &
  #                   sdf$soil_type == st, coln] <- vdf
  #           }
  #         }
  #       }
  #     }
  #   }
  #   sdf$layer_share <- (sdf$BOTDEP - sdf$TOPDEP) / 200
  #   soil_df <- sdf
  #   #Calculate hydraulic properties of all layers
  #   soil_df$soil_saturation <- pt.thetaSat.tropic(soil_df$CLAY,
  #                                                 soil_df$SAND,
  #                                                 soil_df$BULK,
  #                                                 soil_df$CEC_SOIL,
  #                                                 soil_df$PH_WATER)
  #   soil_df$field_capacity <- pt.theta.tropic(
  #     -33,
  #     soil_df$CLAY,
  #     soil_df$BULK,
  #     soil_df$SILT,
  #     soil_df$ORG_CARBON,
  #     soil_df$CEC_SOIL,
  #     soil_df$PH_WATER
  #   )
  #   soil_df$permanent_wilting_point <- pt.theta.tropic(
  #     -1500,
  #     soil_df$CLAY,
  #     soil_df$BULK,
  #     soil_df$SILT,
  #     soil_df$ORG_CARBON,
  #     soil_df$CEC_SOIL,
  #     soil_df$PH_WATER
  #   )
  #
  #   sw_types <- unlist(soil_water_types)
  #   soil_df[sw_types] <-  soil_df[sw_types] * soil_df$layer_share
  #   # print(head(soil_df))
  #
  #   swsub_df <- soil_df[soil_df$soil_depth != "0-20", ]
  #   swsub_df <- aggregate(swsub_df[c("layer_share", sw_types)], list(soil = swsub_df$SOIL), sum)
  #   swsub_df["soil_quick_flow_capacity"] <- swsub_df["soil_saturation"] - swsub_df["field_capacity"]
  #   swsub_df["plant_available_water"] <- swsub_df["field_capacity"] - swsub_df["permanent_wilting_point"]
  #
  #   #LC properties
  #   lc_par_df <- v$lc_par_df
  #   #Calculate hydraulic properties of top soil
  #   top_soil_df <- soil_df[soil_df$soil_depth == "0-20", ]
  #   nsid <- nrow(top_soil_df)
  #   nlc <- nrow(lc_par_df)
  #   rep_lc <- lc_par_df[rep(seq_len(nrow(lc_par_df)), nsid), ]
  #   rep_s <- top_soil_df[rep(seq_len(nrow(top_soil_df)), each = nlc), ]
  #   df <- cbind(rep_lc, rep_s)
  #   df$bulk_density <- df$I_BD_BDRefVegNow * df$REF_BULK
  #   df$soil_saturation <- pt.thetaSat.tropic(df$CLAY,
  #                                            df$SAND,
  #                                            df$bulk_density,
  #                                            df$CEC_SOIL,
  #                                            df$PH_WATER)
  #   df$field_capacity <- pt.theta.tropic(-33,
  #                                        df$CLAY,
  #                                        df$bulk_density,
  #                                        df$SILT,
  #                                        df$ORG_CARBON,
  #                                        df$CEC_SOIL,
  #                                        df$PH_WATER)
  #   df$permanent_wilting_point <- pt.theta.tropic(
  #     -1500,
  #     df$CLAY,
  #     df$bulk_density,
  #     df$SILT,
  #     df$ORG_CARBON,
  #     df$CEC_SOIL,
  #     df$PH_WATER
  #   )
  #   df[sw_types] <- df[sw_types] * df$layer_share
  #   # print("--------")
  #   # print(head(df))
  #   # print("--------")
  #   top_cols <- c("lc_id",
  #                 "SOIL",
  #                 sw_types,
  #                 "bulk_density",
  #                 "I_BD_BDRefVegNow")
  #   topsoil_df <- df[top_cols]
  #
  #   def_df <- soil_df[soil_df$soil_depth == "0-20", c("SOIL", sw_types, "BULK", "REF_BULK")]
  #   def_df$lc_id <- -999
  #   def_df$bulk_density <- def_df$BULK
  #   def_df$I_BD_BDRefVegNow <- def_df$BULK / def_df$REF_BULK
  #   topsoil_df <- rbind(topsoil_df, def_df[top_cols])
  #   v$soil_hydraulic_top_df <- topsoil_df
  #   v$soil_hydraulic_sub_df <- soil_df
  #
  #   topsoil_df["soil_quick_flow_capacity"] <- topsoil_df["soil_saturation"] - topsoil_df["field_capacity"]
  #   topsoil_df["plant_available_water"] <- topsoil_df["field_capacity"] - topsoil_df["permanent_wilting_point"]
  #
  #
  #   wcol <- c(
  #     "soil_quick_flow_capacity",
  #     "plant_available_water",
  #     "permanent_wilting_point"
  #   )
  #   w <- swsub_df[wcol]
  #   colnames(w) <- paste0(wcol, "_sub")
  #   w$soil <- swsub_df$soil
  #
  #   sw_df <- topsoil_df[c("lc_id", wcol)]
  #   sw_df$soil <- topsoil_df$SOIL
  #   sw_df <- merge(sw_df, w, by = "soil")
  #   sw_av_df <- sw_df[c("soil", "lc_id")]
  #   #### calculate water availability ####
  #   # I_SoilSatMinFC
  #   sw_av_df["soil_quick_flow_capacity"] <- sw_df["soil_quick_flow_capacity"] + sw_df["soil_quick_flow_capacity_sub"]
  #   # I_PlantAvWat
  #   sw_av_df["plant_available_water"] <- sw_df["plant_available_water"] + sw_df["plant_available_water_sub"]
  #   #  I_PWP
  #   sw_av_df["inaccessible_water"] <- sw_df["permanent_wilting_point"] + sw_df["permanent_wilting_point_sub"]
  #   print("global")
  #   print(head(sw_av_df))
  #   vd$soil_water_content_global_df <- sw_av_df
  # })
  #
  
  
  
  
  
  
  
  lc_soil_value_map <- function(lc_map, soil_map, val_df) {
    # LC map
    lc_m <- st_as_stars(lc_map)
    wmc <- NULL
    # soil map
    soil_m <- st_as_stars(soil_map)
    smu_ids <- unique(as.vector(soil_m[[1]]))
    smu_ids <- smu_ids[!is.na(smu_ids)]
    for (smu_id in smu_ids) {
      wm <- lc_m
      df <- val_df[val_df$SMU_ID == smu_id, c("lc_id", "value")]
      defval <- df[df$lc_id == -999, "value"]
      df[is.na(df)] <- defval
      wm[soil_m != smu_id] <- NA
      wm <- reclassify_map(wm, df)
      if (is.null(wmc)) {
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
    # calculate_soil_water()
    calculate_soil_water_map()
    # calculate_sub_soil_water()
  })
  
  soil_water_vdf <- data.frame(
    soil_water_v = c(
      "soil_quick_flow_capacity_stars_list",
      "soil_plant_available_water_stars_list",
      "soil_inaccessible_water_stars_list"
    ),
    col_v = c(
      "soil_quick_flow_capacity",
      "plant_available_water",
      "inaccessible_water"
    )
  )
  
  # calculate_soil_water <- function() {
  #   print("calculate_soil_water")
  #
  #   soil_df <- v$soil_water_content_df
  #   soil_sf <- v$slope_class_sf
  #   slopef_m <- vd$slope_factor_stars
  #   lc_ms <- v$lc_map_crop_stars_list
  #   if (is.null(soil_df)) {
  #     showNotification("Soil properties are not set", type = "error")
  #     return()
  #   }
  #
  #   if (is.null(soil_sf)) {
  #     generate_slope_classes()
  #     soil_sf <- v$slope_class_sf
  #     slopef_m <- vd$slope_factor_stars
  #     # return()
  #   }
  #   #soil depth
  #   min_d <- v$genriver_cfg$min_soil_depth
  #   max_d <- v$genriver_cfg$max_soil_depth
  #   sd_m <- slopef_m * (max_d - min_d) +  min_d
  #
  #   lcmids <- names(lc_ms)
  #   ss <- unique(soil_df$soil)
  #   withProgress(value = 0, {
  #     pinc <- 1 / (length(lcmids) * length(ss))
  #     wflist <- as.vector(unlist(soil_water_availability))
  #     for (i in 1:3) {
  #       wf <- wflist[i]
  #       setProgress(
  #         0,
  #         message = paste0("Calculating soil water (", i, " of 3):"),
  #         detail = wf
  #       )
  #       mlist <- list()
  #       for (map_id in lcmids) {
  #         lc_m <- st_as_stars(lc_ms[[map_id]])
  #         wm <- NULL
  #         for (s in ss) {
  #           incProgress(pinc)
  #           ssf <- soil_sf[soil_sf$soil == s, "class"]
  #           ssf_m <- st_rasterize(ssf, lc_m, align = T)
  #           lcs_m <- crop_raster(lc_m, ssf_m)
  #           val_df <- soil_df[soil_df$soil == s, c("lc_id", wf)]
  #           val_m <- reclassify_map(lcs_m, val_df)
  #           if (is.null(wm)) {
  #             wm <- val_m
  #           } else {
  #             wm <- st_mosaic(wm, val_m)
  #           }
  #         }
  #         wm2 <- crop_raster(wm, sd_m)
  #         #multiply with soil depth in mm
  #         mlist[[map_id]] <- wm2 * sd_m * 10
  #       }
  #       var <- soil_water_vdf[soil_water_vdf$col_v == wf, "soil_water_v"]
  #       v[[var]] <- mlist
  #     }
  #   })
  # }
  
  calculate_soil_water_map <- function() {
    print("calculate_soil_water_map")
    if (isolate(v$soil_cfg$soil_type_select) == "soil_type_user") {
      soil_sf <- isolate(v$soil_segments_sf)
    } else {
      soil_sf <- isolate(v$soil_segments_global_sf)
    }
    soil_sf <- soil_sf[c("soil_id")]
    soil_df <- v$soil_water_content_df
    
    lc_ms <- v$lc_map_crop_stars_list
    lc_m <- st_as_stars(lc_ms[[1]])
    sd_m <- vd$soil_depth_stars
    soil_m <- st_rasterize(soil_sf, sd_m)
    
    lcmids <- names(lc_ms)
    ss <- unique(soil_df$soil_id)
    withProgress(value = 0, {
      pinc <- 1 / (length(lcmids) * length(ss))
      wflist <- as.vector(unlist(soil_water_availability))
      for (i in 1:3) {
        wf <- wflist[i]
        setProgress(
          0,
          message = paste0("Calculating soil water map (", i, " of 3):"),
          detail = wf
        )
        mlist <- list()
        for (map_id in lcmids) {
          lc_m <- st_as_stars(lc_ms[[map_id]])
          lc_m <- crop_raster(lc_m, sd_m)
          wm <- NULL
          for (s in ss) {
            incProgress(pinc)
            #get the LC water value list of each soil type
            val_df <- soil_df[soil_df$soil_id == s, c("lc_id", wf)]
            val_df$lc_id <- as.numeric(val_df$lc_id)
            val_m <- reclassify_map(lc_m, val_df)
            val_m[soil_m != s] <- 0
            if (is.null(wm)) {
              wm <- val_m
            } else {
              wm <- wm + val_m
            }
          }
          # multiply with soil depth in mm
          mlist[[map_id]] <- wm * sd_m * 10
        }
        var <- soil_water_vdf[soil_water_vdf$col_v == wf, "soil_water_v"]
        v[[var]] <- mlist
      }
    })
    calculate_sub_soil_water()
  }
  
  #### Soil Water By Subcatchment ###################
  
  calculate_sub_soil_water <- function() {
    qfc <- v[[soil_water_vdf$soil_water_v[1]]]
    paw <- v[[soil_water_vdf$soil_water_v[2]]]
    pwp <- v[[soil_water_vdf$soil_water_v[3]]]
    if (is.null(qfc) || is.null(paw) || is.null(pwp))
      return()
    if (is.null(v$subc_lc_df)) {
      v$subc_lc_df <- calculate_subcatchment_land_cover_area()
    }
    print("Calculating subcatchment soil water")
    # print(qfc)
    qfc <- lapply(qfc, st_as_stars)
    paw <- lapply(paw, st_as_stars)
    pwp <- lapply(pwp, st_as_stars)
    
    sw <- soil_water_vdf$col_v
    lc_map_list <- v$lc_map_crop_stars_list
    lc_map_list <- lapply(lc_map_list, st_as_stars)
    lc_m <- lc_map_list[[1]]
    subc <- v$subcatchment_map_sf
    subc_stars <- st_rasterize(subc["ws_id"], lc_m, align = T)
    subc_stars <- crop_raster(subc_stars, lc_m)
    subc_lc <- v$subc_lc_df
    subc_lc[sw] <- NULL
    map_ids <- unique(subc_lc$map_id)
    
    pinc <- 1 / length(map_ids)
    all_wdf <- NULL
    
    withProgress(message = 'Calculating subcatchment soil water', value = 0, {
      for (map_id in map_ids) {
        incProgress(pinc, detail = paste("LC Map:", map_id))
        lc_m <- lc_map_list[[map_id]]
        qfc_m <- qfc[[map_id]]
        paw_m <- paw[[map_id]]
        pwp_m <- pwp[[map_id]]
        
        wdf <- data.frame(
          map_id = map_id,
          ws_id = as.vector(subc_stars[[1]]),
          lc_id = as.vector(lc_m[[1]])
        )
        wdf[sw[1]] = as.vector(qfc_m[[1]])
        wdf[sw[2]] = as.vector(paw_m[[1]])
        wdf[sw[3]] = as.vector(pwp_m[[1]])
        wdf <- wdf[!is.na(wdf$lc_id) & !is.na(wdf$ws_id), ]
        if (is.null(all_wdf)) {
          all_wdf <- wdf
        } else {
          all_wdf <- rbind(all_wdf, wdf)
        }
        
      }
    })
    
    agg_wdf <- aggregate(all_wdf[sw], by = all_wdf[c("map_id", "ws_id", "lc_id")], mean, na.rm = T)
    subc_lc <- merge(subc_lc,
                     agg_wdf,
                     by = c("map_id", "ws_id", "lc_id"),
                     all.x = T)
    # print(subc_lc)
    v$subc_lc_df <- subc_lc
    
    showNotification("Soil water calculation done!", type = "message")
  }
  
  
  #### Soil Water Map ###################
  
  observe({
    lc_ms <- vd$soil_water_map_stars_list
    map_df <- v$lc_map_df
    
    if (is.null(lc_ms)) {
      output$soil_water_content_ui <- renderUI(p(
        "Please complete the land cover hydrological properties parameters"
      ))
      return()
    }
    output$soil_water_content_ui <- renderUI({
      layout_column_wrap(width = 0.5, !!!lapply(names(lc_ms), function(x) {
        card(full_screen = T, plotOutput(paste0("lc_map_crop_", x)))
      }))
    })
    
    lapply(names(lc_ms), function(x, ms) {
      output[[paste0("lc_map_crop_", x)]] <- renderPlot({
        suppressMessages(plot(
          ms[[x]],
          col = water_color,
          breaks = "equal",
          main = map_df[map_df$map_id == x, "year"]
        ))
      })
    }, lc_ms)
  })
  
  
  observe({
    df <- v$subc_lc_df
    map_df <- v$lc_map_df
    if (is.null(df)) {
      output$soil_water_subcathment_ui <- renderUI(p(
        "Please complete the land cover hydrological properties parameters"
      ))
      return()
    }
    map_ids <- unique(df$map_id)
    output$soil_water_subcathment_ui <- renderUI({
      layout_column_wrap(width = 0.5, !!!lapply(map_ids, function(x) {
        card(full_screen = T, plotOutput(paste0("sub_soil_water_", x)))
      }))
      # )
    })
    
    lapply(map_ids, function(x) {
      output[[paste0("sub_soil_water_", x)]] <- renderPlot({
        wfield <- input$soil_water_select
        subc <- v$subcatchment_map_sf["ws_id"]
        df <- v$subc_lc_df
        df <- df[df$map_id == x, c("ws_id", wfield)]
        df[wfield] <- round(as.numeric(df[[wfield]]))
        subc <- merge(subc, df, by = "ws_id")
        m <- subc[wfield]
        plot(m,
             pal = water_color,
             main = map_df[map_df$map_id == x, "year"],
             key.pos = 4)
      })
    })
  })
  
  
  #### Soil Water Table ###################
  get_lc_with_default <- function() {
    df <- isolate(v$lc_df)[c("lc_id", "land_cover")]
    df <- rbind(df, c(-999, "Default"))
    return(df)
  }
  
  hydro_prop_coldef <- function(prop) {
    d <- lapply(prop , function(x) {
      colDef(name = get_soil_db_header(x), html = T)
      # colDef(name = soil_fieldnames(x), html = T)
    })
    names(d) <- prop
    return(d)
  }
  
  # output$soil_hydraulic_top_table <- renderReactable({
  #   df <- v$soil_hydraulic_top_df
  #   if (is.null(df))
  #     return()
  #   hdf <- v$soil_metadata_df
  #   adf <- get_lc_with_default()
  #   df <- merge(df, adf, by = "lc_id", all.x = T)
  #   data <- v$soil_hydraulic_sub_df
  #   data <- data[data$soil_depth == "0-20", c("SOIL", hydro_prop)]
  #
  #   cn <- hydro_prop_coldef()
  #   reactable(
  #     data,
  #     defaultExpanded = TRUE,
  #     columns = cn,
  #     onClick = "expand",
  #     details = function(index) {
  #       d_data <- df[df$SOIL == data$SOIL[index], ]
  #       reactable(
  #         d_data[c("lc_id",
  #                  "land_cover",
  #                  unlist(soil_water_types),
  #                  "bulk_density")],
  #         pagination = F,
  #         striped = T,
  #         borderless = TRUE,
  #         compact = TRUE,
  #         rownames = F,
  #         columns = append(
  #           list(
  #             lc_id = colDef(format = colFormat(digit = 0)),
  #             land_cover = colDef(name = "Land Cover"),
  #             bulk_density = colDef(name = "Bulk Density")
  #           ),
  #           soil_water_coldef()
  #         ),
  #         defaultColDef = colDef(format = colFormat(digit = 3))
  #       )
  #     }
  #   )
  # })
  
  # soil_water_coldef <- function(x) {
  #   cd <- lapply(names(soil_water_types), function(x) {
  #     colDef(name = x)
  #   })
  #   names(cd) <- unlist(soil_water_types)
  #   return(cd)
  # }
  
  # output$soil_hydraulic_sub_table <- renderReactable({
  #   df <- v$soil_hydraulic_sub_df
  #   if (is.null(df))
  #     return()
  #   hpcd <- hydro_prop_coldef(hydro_prop)
  #   hpcd <- append(hpcd, list(
  #     SOIL = colDef(format = colFormat(digit = 0)),
  #     soil_depth = colDef(name = "Soil Depth (cm)")
  #   ))
  #   data <- df[df$soil_depth != "0-20", c("SOIL",
  #                                         "soil_depth" ,
  #                                         unlist(soil_water_types),
  #                                         hydro_prop)]
  #   reactable(
  #     data,
  #     defaultExpanded = TRUE,
  #     onClick = "expand",
  #     groupBy = "SOIL",
  #     pagination = F,
  #     striped = T,
  #     borderless = TRUE,
  #     compact = TRUE,
  #     rownames = F,
  #     columns = append(soil_water_coldef(), hpcd),
  #     defaultColDef = colDef(format = colFormat(digit = 3))
  #   )
  # })
  
  output$soil_water_content_table <- renderReactable({
    df <- v$soil_water_content_df
    if (is.null(df))
      return()
    adf <- get_lc_with_default()
    df <- merge(adf, df, by = "lc_id")
    df$lc_id <- as.numeric(as.character(df$lc_id))
    df <- df[order(df$lc_id), ]
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
    if (is.null(m))
      return()
    msf <- st_as_sf(m,
                    as_points = F,
                    merge = T,
                    connect8 = T)
    msf <- st_transform(msf, crs = 4326)
    names(msf) <- c("val", "geometry")
    # merge polygon for each id
    sid <- sort(unique(msf$val))
    suppressMessages({
      a <- lapply(sid, function(x) {
        st_as_sf(st_union(msf[msf$val == x, ]))
      })
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
  
  evapotran_df_edited <- table_edit_server(
    "evapotran_df_table",
    reactive(v$evapotran_df),
    col_type = c("date", "numeric"),
    nrow = 365,
    allowRowModif = T,
    pagination = 365
  )
  
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
  
  date_format <- "%d-%b-%Y"
  
  rain_df_edited <- table_edit_server(
    "rain_df_table",
    reactive(v$rain_df),
    col_type = c("date", "numeric"),
    nrow = 365,
    allowRowModif = T,
    pagination = 365
  )
  
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
  
  river_df_edited <- table_edit_server(
    "river_df_table",
    reactive(v$river_df),
    col_type = c("date", "numeric"),
    nrow = 365,
    allowRowModif = T,
    pagination = 365
  )
  
  observe({
    v$river_df <- river_df_edited()
  })
  
  observe({
    df <- v$river_df
    if (is.null(df) || is.null(nrow(df)) || nrow(df) == 0) {
      vd$river_df <- NULL
      return()
    }
    if (is.null(v$genriver_cfg$total_area_m2) ||
        v$genriver_cfg$total_area_m2 <= 0)
      return()
    df <- df[order(df$date), ]
    df$year <- year(df$date)
    df$month <- month(df$date)
    df$day <- yday(df$date)
    df$flow_mm <- convert_flow_to_mmdaily(df$river_flow, v$genriver_cfg$total_area_m2)
    df$river_cum <- ave(df$flow_mm, df$year, FUN = cumsum)
    vd$river_df <- df
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
          f_number(v$genriver_cfg$total_area_m2 / 10000, nsmall = 2),
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
          f_number(v$genriver_cfg$total_area_m2 / 10000, nsmall = 2),
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
          color = I(theme_color$secondary),
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
            color = I(theme_color$warning),
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
    update_lc_prop()
    # update map id counter
    map_df <- isolate(v$lc_map_df)
    map_idc <- as.numeric(unlist(lapply(map_df$map_id, suffix)))
    map_id_counter <<- max(map_idc)
    # update numeric parameters UI
    update_numeric_par()
    update_watershed_table()
    update_soil_ui()
    print("parameters uploaded!")
    showNotification("Parameters uploaded!", type = "message")
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
    I_SoilQflowFrac = 0.1,
    I_WarmUpTime = 730
  )
  
  stream_input_pars <- list(
    I_Tortuosity = 0.6,
    I_RiverflowDispersalFactor = 0.6,
    I_RoutVeloc_m_per_s = 0.55,
    I_SurfLossFrac = 0
  )
  
  # Hydroelectric Power Plant (HEPP)
  
  lake_input_pars <- list(
    I_TotalArea = 0,
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
        subc_lc_df$D_SoilWater / (
          subc_lc_df$I_RelDroughtFact * subc_lc_df$I_AvailWaterClass
        )
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
                             I_WarmUpTime,
                             I_Evapotrans,
                             I_DebitTime,
                             subc_df,
                             subc_lc_df,
                             stream_pars,
                             lake_pars,
                             stock,
                             stream_delay_df) {
    # I_Warmedup = if time = int(I_WarmUpTime) then 1 else 0
    I_Warmedup <- ifelse(time == I_WarmUpTime, 1 , 0)
    
    # D_EvaporReservoir[Subcatchement] = I_Evapotrans*ARRAYSUM(L_Lake?[*])
    subc_df$D_EvaporReservoir <- I_Evapotrans * sum(subc_df$L_Lake)
    
    # D_Influx_to_Resr[Subcatchement] = if I_DaminThisStream?[Subcatchement] = 1 then D_GWaDisch[Subcatchement]+ARRAYSUM(D_SoilDischarge[*,Subcatchement])+D_SurfaceFlow[Subcatchement] else 0
    subc_df$D_Influx_to_Resr <- ifelse(
      subc_df$I_DaminThisStream == 1,
      subc_df$D_GWaDisch + subc_df$D_SoilDischarge + subc_df$D_SurfaceFlow,
      0
    )
    
    
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
    D_SubCResUseFrac <- D_SubCResUseFrac_graph[1] #TODO: of daily rain
    
    # D_SubCResOutflow[Subcatchement] = if D_SubcResVol[Subcatchement]/D_ReservoirVol[Subcatchement] > 1 then
    # D_SubcResVol[Subcatchement]-D_ReservoirVol[Subcatchement] else D_SubCResUseFrac*D_SubcResVol[Subcatchement]
    # TODO: modified -> what if subc_df$D_ReservoirVol == 0?
    subc_df$D_SubCResOutflow <- ifelse(
      subc_df$D_ReservoirVol == 0,
      0,
      ifelse(
        subc_df$D_SubcResVol / subc_df$D_ReservoirVol > 1,
        subc_df$D_SubcResVol - subc_df$D_ReservoirVol,
        D_SubCResUseFrac * subc_df$D_SubcResVol
      )
    )
    
    # D_SubcResVol[Subcatchement](t) = D_SubcResVol[Subcatchement](t - dt) + (D_Influx_to_Resr[Subcatchement] - D_EvaporReservoir[Subcatchement] - D_SubCResOutflow[Subcatchement]) * dt
    subc_df$D_SubcResVol <- subc_df$D_SubcResVol + (
      subc_df$D_Influx_to_Resr - subc_df$D_EvaporReservoir - subc_df$D_SubCResOutflow
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
    # ini fraksi kecepatan 0-1 untuk masing2 tahun land cover map.. kalau lebih cepat < 1, kalau gak ada perubahan = 1
    
    
    
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
    
    #### Stream Delay ############################
    
    if (is.null(stream_delay_df)) {
      stream_delay_df <- subc_df[c("ws_id", "D_TotalStreamInflow", "D_RoutingTime")]
    } else {
      df <- subc_df[c("ws_id", "D_TotalStreamInflow", "D_RoutingTime")]
      stream_delay_df <- rbind(stream_delay_df, df)
    }
    
    # print(head(stream_delay_df))
    stream_now <-  stream_delay_df[stream_delay_df$D_RoutingTime < 1, ]
    stream_delay_df <- stream_delay_df[stream_delay_df$D_RoutingTime >= 1, ]
    stream_delay_df$D_RoutingTime <- stream_delay_df$D_RoutingTime - 1
    # print(head(stream_now$ws_id))
    # print(head(stream_delay_df))
    
    stream_now_agg <- aggregate(stream_now$D_TotalStreamInflow,
                                list(stream_now$ws_id),
                                sum,
                                na.rm = T)
    colnames(stream_now_agg) <- c("ws_id", "D_SurfFlowRiver")
    
    # print(stream_now_agg)
    
    subc_df$D_SurfFlowRiver <- NULL
    subc_df <- merge(subc_df,
                     stream_now_agg,
                     by = c("ws_id"),
                     all.x = T)
    subc_df[is.na(subc_df$D_SurfFlowRiver), "D_SurfFlowRiver"] <- 0
    # D_SurfFlowRiver is D_SurfFlowObsPoint in delayed time?
    # subc_df$D_SurfFlowRiver <- 0
    
    # D_SurfFlowRiver[Subcatchement,ObsPoint] = CONVEYOR OUTFLOW
    # TRANSIT TIME = if D_RoutingTime[Subcatchement,ObsPoint]>1 then
    # D_RoutingTime[Subcatchement,ObsPoint] else 0
    
    
    # This isolated vars
    # D_SurfFlowObsPoint[Subcatchement,ObsPoint] = if D_RoutingTime[Subcatchement,ObsPoint]>=1 then D_TotalStreamInflow[Subcatchement,ObsPoint] else 0
    subc_df$D_SurfFlowObsPoint <- ifelse(subc_df$D_RoutingTime >= 1, subc_df$D_TotalStreamInflow, 0)
    # TODO: Unknown correlation to this variable
    # D_StreamsSurfQ[Subcatchement,ObsPoint](t) = D_StreamsSurfQ[Subcatchement,ObsPoint](t - dt) + (D_SurfFlowObsPoint[Subcatchement,ObsPoint] - D_SurfFlowRiver[Subcatchement,ObsPoint]) * dt
    subc_df$D_StreamsSurfQ <- subc_df$D_StreamsSurfQ + (subc_df$D_SurfFlowObsPoint - subc_df$D_SurfFlowRiver)
    
    
    # D_DirectSurfFkowObsPoint[Subcatchement,ObsPoint] = if D_RoutingTime[Subcatchement,ObsPoint]>=0 and D_RoutingTime[Subcatchement,ObsPoint]<1  then D_TotalStreamInflow[Subcatchement,ObsPoint]*(1-I_ReleaseFrac[Subcatchement,ObsPoint])else 0
    subc_df$D_DirectSurfFkowObsPoint <- ifelse(
      subc_df$D_RoutingTime >= 0 & subc_df$D_RoutingTime < 1,
      subc_df$D_TotalStreamInflow *
        (1 - subc_df$I_ReleaseFrac),
      0
    )
    
    # D_RiverDelay[Subcatchement,ObsPoint] = I_ReleaseFrac[Subcatchement,InflowLake]*D_TotRiverFlowNoDelay[Subcatchement,InflowLake]*(1-D_FeedingIntoLake?[Subcatchement])
    subc_df$D_RiverDelay <- subc_df$I_ReleaseFrac * subc_df$D_TotRiverFlowNoDelay *
      (1 - subc_df$D_FeedingIntoLake)
    # print("before:")
    # print(sum(subc_df$D_TotRiverFlowNoDelay))
    # D_TotRiverFlowNoDelay[Subcatchement,ObsPoint](t) = D_TotRiverFlowNoDelay[Subcatchement,ObsPoint](t - dt) + (D_SurfFlowRiver[Subcatchement,ObsPoint] + D_DirectSurfFkowObsPoint[Subcatchement,ObsPoint] - D_RiverDelay[Subcatchement,ObsPoint] - D_RivInflLake[Subcatchement,ObsPoint]) * dt
    subc_df$D_TotRiverFlowNoDelay <- subc_df$D_TotRiverFlowNoDelay + subc_df$D_SurfFlowRiver + subc_df$D_DirectSurfFkowObsPoint - subc_df$D_RiverDelay - subc_df$D_RivInflLake
    # df <- subc_df[c("D_TotRiverFlowNoDelay", "D_SurfFlowRiver", "D_DirectSurfFkowObsPoint", "D_RiverDelay", "D_RivInflLake")]
    # df$t <- time
    # if(is.null(temp_df)) {
    #   temp_df <<- df
    # } else {
    #   temp_df <<- rbind(temp_df, df)
    # }
    #
    # print("after")
    # print(sum(subc_df$D_TotRiverFlowNoDelay))
    # print("---")
    # print(sum(subc_df$D_SurfFlowRiver)+sum(subc_df$D_DirectSurfFkowObsPoint))
    # print(sum(subc_df$D_RivInflLake))
    
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
    O_BYP <- ifelse(
      O_LastYearHEPP > 0 &&
        O_LastYearHEPP > O_BestYyHEPP,
      -O_BestYyHEPP + O_LastYearHEPP,
      0
    )
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
    I_WUcorrection <- ifelse(time == (I_WarmUpTime + 1), 1, 0)
    # O_Reset? = if I_Warmedup = 1 or I_WUcorrection = 1  then 1 else 0
    O_Reset <- ifelse(I_Warmedup == 1 && I_WUcorrection == 1, 1, 0)
    # L_RestartH = O_Reset?*L_CumHEPPUse
    L_RestartH <- O_Reset * stock$L_CumHEPPUse
    # O_WYP = if O_LastYearHEPP>0 and O_LastYearHEPP<O_WorstYHEPP then -O_WorstYHEPP+O_LastYearHEPP else 0
    O_WYP <- ifelse(
      O_LastYearHEPP > 0 &&
        O_LastYearHEPP < O_WorstYHEPP,
      -O_WorstYHEPP + O_LastYearHEPP,
      0
    )
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
    O_FrBaseFlow <- ifelse(O_TotStreamFlow > 0,
                           stock$O_CumBaseFlow / O_TotStreamFlow,
                           0)
    # O_FrSoilQuickFlow = if O_TotStreamFlow > 0 then O_CumSoilQFlow/O_TotStreamFlow   else 0
    O_FrSoilQuickFlow <- ifelse(O_TotStreamFlow > 0,
                                stock$O_CumSoilQFlow /
                                  O_TotStreamFlow,
                                0)
    # O_FrSurfQuickflow = if O_TotStreamFlow > 0 then O_CumSurfQFlow/O_TotStreamFlow   else 0
    O_FrSurfQuickflow <- ifelse(O_TotStreamFlow > 0,
                                stock$O_CumSurfQFlow /
                                  O_TotStreamFlow,
                                0)
    
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
    
    stream_vars$D_RiverFlowtoLake <- D_RiverFlowtoLake
    stream_vars$D_GWtoLake <- D_GWtoLake
    
    return(
      list(
        subc_lc_df = subc_lc_df,
        subc_df = subc_df,
        stream_vars = stream_vars,
        stock = stock,
        stream_delay_df = stream_delay_df
      )
    )
  }
  
  temp_df <- NULL
  
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
    # print("sim parameters")
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
    
    stream_pars <- stream_input_pars
    if (!is.null(v$river_par_cfg)) {
      stream_pars$I_RoutVeloc_m_per_s <- v$river_par_cfg$I_RoutVeloc_m_per_s
      stream_pars$I_Tortuosity <- v$river_par_cfg$I_Tortuosity
      stream_pars$I_RiverflowDispersalFactor <- v$river_par_cfg$I_RiverflowDispersalFactor
      stream_pars$I_SurfLossFrac <- v$river_par_cfg$I_SurfLossFrac
    }
    
    ### Initial Sub-catchment vars #############
    I_TotalArea <- v$genriver_cfg$total_area_m2 #
    subc_area_df <- as.data.frame(v$subcatchment_map_sf)[c("ws_id", "area", "distance")]
    subc_area_df$area <- subc_area_df$area * 10000 # ha to m2
    subc_area_df$I_RelArea <- subc_area_df$area / I_TotalArea
    subc_area_df$I_RoutingDistance <- subc_area_df$distance  #/ 1000 # m to km
    subc_df <- subc_area_df[c("ws_id", "I_RelArea", "I_RoutingDistance")]
    
    ### INITIALIZATION ##################################
    # print("* init")
    map_subc_lc_df <- get_map_subc_lc_df()
    lc_map_df <- v$lc_map_df
    
    rain_df <- v$rain_df
    d <- rain_df[1, "date"]
    year_now <- year(d)
    wslcid_cols <- c("ws_id", "lc_id")
    yr_subc_lc_df <- get_lc_vars_of_year(year_now, map_subc_lc_df, lc_map_df, wslcid_cols)
    yr_subc_lc_df$I_AvailWatClassNow <- yr_subc_lc_df$I_PlantAvWatSub
    
    # print("* ground")
    ground_par_df <- v$ground_par_df
    wsid_cols <- c("ws_id")
    yr_ground_par_df <- get_lc_vars_of_year(year_now, ground_par_df, lc_map_df, wsid_cols)
    subc_df <- merge(subc_df,
                     yr_ground_par_df,
                     by = "ws_id",
                     all.x = T)
    
    subc_lc_df <- yr_subc_lc_df
    
    subc_lc_df$D_CumNegRain <- 0
    subc_lc_df$D_CumEvapTranspClass <- 0
    subc_lc_df <- merge(subc_lc_df,
                        subc_area_df[c("ws_id", "I_RelArea")],
                        by = "ws_id",
                        all.x = T)
    lc_par_df <- v$lc_par_df[c("lc_id",
                               "I_InterceptClass",
                               "I_RelDroughtFact",
                               "I_BD_BDRefVegNow")]
    subc_lc_df <- merge(subc_lc_df, lc_par_df, by = "lc_id", all.x = T)
    # print("* water")
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
    # print("* evapot")
    month_now <- month(d)
    lc_evapot_df <- v$lc_evapot_df[v$lc_evapot_df$month == month_now, c("lc_id", "value")]
    names(lc_evapot_df) <- c("lc_id", "I_MultiplierEvapoTrans")
    subc_lc_df <- merge(subc_lc_df, lc_evapot_df, by = "lc_id", all.x = T)
    
    #### Stream and Lake init #######################
    # print("* stream")
    lake_input_pars$I_TotalArea <- I_TotalArea
    I_WarmUpTime <- input$I_WarmUpTime
    #TODO: will be parameterize through UI
    subc_df$L_Lake <- 0
    subc_lc_df$L_Lake <- 0 #TODO: this should be subcat
    
    subc_df$D_FracGWtoLake <- 0
    subc_df$I_DaminThisStream <- 0
    subc_df$D_FeedingIntoLake <- 1
    # D_ReservoirVol[Subcatchement] = L_ResrDepth*L_Lake?[Subcatchement]*I_RelArea[Subcatchement]
    subc_df$D_ReservoirVol <- lake_input_pars$L_ResrDepth * subc_df$L_Lake * subc_df$I_RelArea
    # INIT D_SubcResVol[Subcatchement] = D_ReservoirVol[Subcatchement]*I_DaminThisStream?[Subcatchement]
    subc_df$D_SubcResVol <- subc_df$D_ReservoirVol * subc_df$I_DaminThisStream
    subc_df$I_SubcContr <- 1
    # print("* return")
    return(
      list(
        year_now = year_now,
        month_now = month_now,
        subc_lc_df = subc_lc_df,
        subc_df = subc_df,
        waterbalance_input_pars = inp,
        stream_input_pars = stream_pars,
        lake_input_pars = lake_input_pars,
        I_WarmUpTime = I_WarmUpTime
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
    
    output_df <- NULL
    stream_delay_df <- NULL
    
    ### SIMULATION LOOP ##################################
    
    print(paste("* Simulation:", ndays, "iteration"))
    set.seed(inp$I_Rain_GenSeed + 11250)
    I_Simulation_Time <- ndays + pars$I_WarmUpTime
    for (t in 1:I_Simulation_Time) {
      # print(t)
      if (t <= pars$I_WarmUpTime) {
        day <- t
        day_end <- pars$I_WarmUpTime
        w <- "WARMING UP -"
      } else {
        day <- (t - pars$I_WarmUpTime)
        day_end <- ndays
        w <- ""
      }
      progress(day / day_end, detail = paste(w, "Day", day, "of", day_end))
      #### Daily Rainfall input #####################
      date <- rain_df[day, "date"]
      if (is.na(date))
        next
      #TODO: could be use spatial explicit rain per sub catchment
      I_DailyRain <- rain_df[day, "rainfall"] * inp$I_RainMultiplier
      if (is.na(I_DailyRain))
        next
      ws_rain_df <- subc_df[c("ws_id")]
      ws_rain_df$I_RainPerDay <- I_DailyRain
      
      ### Daily Evapotranspiration input #####################
      I_Daily_Evapotrans <- v$evapotran_df[v$evapotran_df$date == date, "evapotranspiration"]
      I_DebitTime <- v$river_df[v$river_df$date == date, "river_flow"]
      
      ### Yearly data changes #####################
      if (year_now != year(date)) {
        year_now <- year(date)
        # print(year_now)
        yr_subc_lc_df <- get_lc_vars_of_year(year_now,
                                             map_subc_lc_df,
                                             lc_map_df,
                                             c("ws_id", "lc_id"))
        # print(yr_subc_lc_df)
        
        yr_subc_lc_df$I_AvailWatClassNow <- yr_subc_lc_df$I_PlantAvWatSub
        subc_lc_df[names(yr_subc_lc_df)] <- yr_subc_lc_df
        
        yr_ground_par_df <- get_lc_vars_of_year(year_now, ground_par_df, lc_map_df, c("ws_id"))
        # print(yr_ground_par_df)
        subc_df[names(yr_ground_par_df)] <- yr_ground_par_df
      }
      
      ### Daily Evapotranspiration potential input #####################
      if (month_now != month(date)) {
        month_now <- month(date)
        # print(month_now)
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
        pars$I_WarmUpTime,
        I_Daily_Evapotrans,
        I_DebitTime,
        wb$subc_df,
        wb$subc_lc_df,
        pars$stream_input_pars,
        lake_input_pars,
        stock,
        stream_delay_df
      )
      
      ### Compile output ######################
      if (t > pars$I_WarmUpTime) {
        base_df <- data.frame(day = day,
                              date = date,
                              I_DailyRain = I_DailyRain)
        df <- base_df
        df[names(sn$stream_vars)] <- unlist(sn$stream_vars)
        df[names(sn$stock)] <- unlist(sn$stock)
        if (is.null(output_df)) {
          output_df <- df
        } else {
          output_df <- rbind(output_df, df)
        }
      }
      
      ### Params for next loop ############
      subc_lc_df <- sn$subc_lc_df
      subc_df <- sn$subc_df
      stock <- sn$stock
      stream_delay_df <- sn$stream_delay_df
    }
    
    vd$output_df <- output_df
    print("* Simulation done")
    showNotification("Simulation done!", type = "message")
  }
  
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
  
  ### RUN Simulation Start ##############
  
  max_sim_days <- reactiveVal()
  
  # soil_water_dalog <- input_dialog(
  #   "Soil Properties Not Available",
  #   "The soil water properties is not ready<br><br>
  #   Please click 'Confirm' to run the <b>soil water calculations</b>",
  #   "run_soil_water_confirm_btn"
  # )
  
  # observeEvent(input$run_soil_water_confirm_btn, {
  #   removeModal()
  #   calculate_soil_water()
  #   calculate_sub_soil_water()
  # })
  
  
  
  observeEvent(input$sim_run_button, {
    #TODO: disable button if not ready, or show the step to be ready
    if (is.null(v$lc_par_df)) {
      showNotification("Land cover map was not set! Please complete the parameters",
                       type = "error")
      return()
    }
    if (is.null(v$subcatchment_map_sf)) {
      showNotification("Watershed area was not set! Please complete the parameters",
                       type = "error")
      return()
    }
    
    subc_lc_df <- v$subc_lc_df
    if (is.null(subc_lc_df)) {
      showNotification(
        "Soil water properties was not set, it will be calculated before starting the simulation",
        type = "warning"
      )
      # calculate_soil_water()
      # calculate_sub_soil_water()
      calculate_soil_water_map()
    }

    if(is.null(max_sim_days())) return()
    
    ndays <- input$ndays_input
    if(ndays <= 0) return()
    if (ndays > max_sim_days()) {
      show_input_dialog(
        "Number of Simulation Days Error",
        paste(
          "Maximum number of simulation days is<b>",
          max_days,
          "</b><br>based on Rainfall and River data availability.<br>",
          "Click 'Confirm' and run again the simulation"
        ),
        "max_days_confirm_btn"
      )
      return()
    }
    
    withProgress(message = 'Running Simulation', value = 0, {
      run_genriver(ndays, progress = setProgress)
    })
    
  })
  
  observeEvent(input$max_days_confirm_btn, {
    updateNumericInput(session, "ndays_input", value = max_sim_days())
    removeModal()
  })
  
  observe({
    rain_df <- v$rain_df
    river_df <- v$river_df
    if (is.null(rain_df) ||
        is.null(river_df) ||
        nrow(rain_df) == 0 || nrow(river_df) == 0) {
      updateNumericInput(session, "ndays_input", value = 0)
      return()
    }
    max_days <- min(nrow(rain_df), nrow(river_df))
    updateNumericInput(session, "ndays_input", value = max_days)
    max_sim_days(max_days)
  })
  
  
  ### CALIBRATION #######################
  
  observeEvent(input$run_calibration_button, check_variables())
  
  check_variables <- function() {
    if (is.null(v$lc_par_df))
      return()
    if (is.null(v$subcatchment_map_sf))
      return()
    
    
    subc_lc_df <- v$subc_lc_df
    if (is.null(subc_lc_df)) {
      showNotification(
        "Soil water properties is not ready, it will be calculated before starting the simulation",
        type = "warning"
      )
      # calculate_soil_water()
      # calculate_sub_soil_water()
      calculate_soil_water_map()
    }
    
    print("calibration vars")
    
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
    stream_delay_df <- NULL
    stream_par <- pars$stream_input_pars
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
        0,
        I_Daily_Evapotrans,
        I_DebitTime,
        wb$subc_df,
        wb$subc_lc_df,
        stream_par,
        lake_input_pars,
        stock,
        stream_delay_df
      )
      # output
      subc_df <- sn$subc_df
      subc_lc_df <- sn$subc_lc_df
      stock <- sn$stock
      stream_delay_df <- sn$stream_delay_df
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
  }
  
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
  
  ### Watershed Indicator Output ##################
  
  output$watershed_indicator_table_output <- renderReactable({
    df <- vd$output_df
    if (is.null(df))
      return()
    df <- df[watershed_indicator_output]
    reactable_daily_data(df)
  })
  
  criteria <- c("very good", "good", "satisfactory", "unsatisfactory")
  
  biased_criteria <- function(x) {
    if (is.null(x) || is.na(x))
      return(NA)
    x <- abs(x)
    if (x < 10) {
      return(criteria[1])
    } else if (x < 15) {
      return(criteria[2])
    } else if (x < 25) {
      return(criteria[3])
    } else {
      return(criteria[4])
    }
  }
  
  nse_criteria <- function(x) {
    if (is.null(x) || is.na(x))
      return(NA)
    x <- abs(x)
    if (x <= 1 && x > 0.75) {
      return(criteria[1])
    } else if (x <= 0.75 && x > 0.65) {
      return(criteria[2])
    } else if (x <= 0.65 && x > 0.5) {
      return(criteria[3])
    } else {
      return(criteria[4])
    }
  }
  
  criteria_color <- function(x) {
    if (is.null(x) || is.na(x))
      return(NULL)
    if (x == criteria[1]) {
      return("#219ebc")
    } else if (x == criteria[2]) {
      return("#06d6a0")
    } else if (x == criteria[3]) {
      return("#fb8500")
    } else {
      return("#9a130e")
    }
  }
  
  
  
  observe({
    df <- vd$output_df
    if (is.null(df))
      return()
    df <- df[watershed_indicator_output]
    nyear <- unique(year(df$date))
    m <- which(month.name == input$month_hydro_select)
    
    output$river_flow_ui <- renderUI({
      layout_column_wrap(width = 0.5, !!!lapply(nyear, function(x) {
        card(plotlyOutput(paste0("river_flow_plot_", x)), full_screen = T)
      }))
    })
    
    lapply(nyear, function(x, df) {
      output[[paste0("river_flow_plot_", x)]] <-
        renderPlotly({
          if (m == 1) {
            ydf <- df[year(df$date) == x, ]
            title <- paste("Year", x)
          } else {
            start <- date(paste(x, m, 1, sep = "-"))
            end <- date(paste(x + 1, m, 1, sep = "-"))
            ydf <- df[date(df$date) >= start &
                        date(df$date) < end, ]
            title <- paste(month.name[m], x, "-", month.name[m - 1], x +
                             1)
          }
          if (nrow(ydf) == 0)
            return(NULL)
          # ydf$day <- ydf$day - min(ydf$day) + 1
          ydf$date <- as.Date(ydf$date)
          plot_ly(
            ydf,
            x = ~ date,
            y = ~ I_RFlowdata_mmday,
            color = I(theme_color$secondary),
            type = "scatter",
            mode = "lines",
            name = 'Observation',
            line = list(color = chart_color[1])
          ) |>
            add_trace(
              y = ~ L_InFlowtoLake,
              name = "Simulation",
              line = list(color = chart_color[2])
            ) |>
            layout(
              title = title,
              yaxis = list(title = "River Flow (mm)"),
              xaxis = list(title = "Date"),
              hovermode = 'x',
              showlegend = T
            )
        })
    }, df)
  })
  
  
  observe({
    df <- vd$output_df
    if (is.null(df))
      return()
    df <- df[watershed_indicator_output]
    nyear <- unique(year(df$date))
    m <- which(month.name == input$month_hydro_select)
    
    output$cum_river_flow_ui <- renderUI({
      layout_column_wrap(width = 0.5, !!!lapply(nyear, function(x) {
        card(plotlyOutput(paste0("cum_river_flow_plot_", x)), full_screen = T)
      }))
    })
    
    lapply(nyear, function(x, df) {
      output[[paste0("cum_river_flow_plot_", x)]] <-
        renderPlotly({
          if (m == 1) {
            ydf <- df[year(df$date) == x, ]
            title <- paste("Year", x)
          } else {
            start <- date(paste(x, m, 1, sep = "-"))
            end <- date(paste(x + 1, m, 1, sep = "-"))
            ydf <- df[date(df$date) >= start &
                        date(df$date) < end, ]
            title <- paste(month.name[m], x, "-", month.name[m - 1], x +
                             1)
          }
          if (nrow(ydf) == 0)
            return(NULL)
          ydf$rain_cum <- cumsum(ydf$I_DailyRain)
          ydf$obs_cum <- cumsum(ydf$I_RFlowdata_mmday)
          ydf$sim_cum <- cumsum(ydf$L_InFlowtoLake)
          plot_ly(
            ydf,
            x = ~ rain_cum,
            y = ~ obs_cum,
            color = I(theme_color$secondary),
            type = "scatter",
            mode = "lines",
            name = 'Observation',
            line = list(color = chart_color[1])
          ) |>
            add_trace(
              y = ~ sim_cum,
              name = "Simulation",
              line = list(color = chart_color[2])
            ) |>
            layout(
              title = title,
              yaxis = list(title = "Cumulative River Flow (mm)"),
              xaxis = list(title = "Cumulative Rainfall (mm)"),
              hovermode = 'x',
              showlegend = T
            )
        })
    }, df)
  })
  
  #### Watershed Performance #########################
  
  observe({
    df <- vd$output_df
    if (is.null(df))
      return()
    df <- df[c("date", "I_RFlowdata_mmday", "L_InFlowtoLake")]
    m <- which(month.name == input$month_hydro_select)
    df$n <- 1
    df$m <- month(df$date)
    df$year <- year(df$date)
    df[df$m >= m, "yr_shift"] <- 0
    df[df$m < m, "yr_shift"] <- -1
    df$year2 <- df$year + df$yr_shift
    
    df$y <- df$I_RFlowdata_mmday
    df$yi <- df$L_InFlowtoLake
    df$dev <- df$yi - df$y
    df$dev_sq <- df$dev ^ 2
    df$dev_rat <- 100 * df$dev / df$y
    df[df$y == 0, "dev_rat"] <- NA
    
    mean_df <- aggregate(df[c("y", "dev_rat", "dev_sq")], list(df$year2), mean, na.rm = T)
    colnames(mean_df) <- c("year2", "y_mean", "mean_error", "dev_sq_mean")
    df <- merge(df, mean_df[c("year2", "y_mean")], by = "year2")
    df$dev_mean <- df$y - df$y_mean
    df$dev_mean_sq <- df$dev_mean ^ 2
    
    yr_df <- aggregate(df[c("n", "y", "dev", "dev_sq", "dev_mean_sq", "dev_rat")], list(df$year2), sum, na.rm = T)
    colnames(yr_df) <- c("year2",
                         "n",
                         "y_sum",
                         "dev_sum",
                         "dev_sq_sum",
                         "dev_mean_sq_sum",
                         "dev_rat_sum")
    yr_df$hyear <- paste0(yr_df$year2, "-", yr_df$year2 + 1)
    
    yr_df <- merge(yr_df, mean_df[c("year2", "mean_error", "dev_sq_mean")], by = "year2")
    yr_df$biased <- 100 * yr_df$dev_sum / yr_df$y_sum
    yr_df$nse <- 1 - (yr_df$dev_sq_sum / yr_df$dev_mean_sq_sum)
    yr_df$rmse <- sqrt(yr_df$dev_sq_mean)
    
    suppressWarnings(r <- sapply(split(df, df$year2), function(x)
      cor(x$y, x$yi)))
    r_df <- as.data.frame(r)
    r_df$year2 <- rownames(r_df)
    yr_df <- merge(yr_df, r_df, by = "year2")
    yr_df$biased_criteria <- sapply(yr_df$biased, biased_criteria)
    yr_df$nse_criteria <- sapply(yr_df$nse, nse_criteria)
    
    yr_df[sapply(yr_df, is.infinite)] <- NA
    vd$performance_df <- yr_df[unlist(performance_fields)]
  })
  
  performance_fields <- list(
    data = c("hyear", "n"),
    value = c("biased", "nse", "r", "mean_error", "rmse"),
    criteria = c("biased_criteria", "nse_criteria")
  )
  
  fcriteria <- function(value, index, name) {
    color <- criteria_color(value)
    list(
      color = color,
      fontWeight = "bold",
      background = paste0(color, "15")
    )
  }
  
  fvcriteria <- function(v) {
    list(background = paste0(criteria_color(v), "15"))
  }
  
  output$performance_table_output <- renderReactable({
    df <- vd$performance_df
    if (is.null(df))
      return()
    reactable(
      df,
      striped = T,
      borderless = TRUE,
      rownames = F,
      pagination = F,
      columns = list(
        hyear = colDef(name = "Year"),
        n = colDef(format = colFormat(digits = 0), width = 60),
        biased = colDef(
          name = "Biased (%)",
          style = function(value, index, name) {
            fvcriteria(biased_criteria(value))
          }
        ),
        nse = colDef(
          name = "NSE",
          style = function(value, index, name) {
            fvcriteria(nse_criteria(value))
          }
        ),
        mean_error = colDef(name = "Mean Error"),
        rmse = colDef(name = "RMSE (mm)"),
        biased_criteria = colDef(
          name = "Biased (%)",
          style = fcriteria,
          width = 120,
          headerStyle = list(background = "lightgray")
        ),
        nse_criteria = colDef(
          name = "NSE",
          style = fcriteria,
          width = 120,
          headerStyle = list(background = "lightgray")
        )
        
      ),
      columnGroups = list(
        colGroup(name = "Data", columns = performance_fields$data),
        colGroup(name = "Value", columns = performance_fields$value),
        colGroup(
          name = "Criteria",
          columns = performance_fields$criteria,
          headerStyle = list(background = "lightgray")
        )
      ),
      defaultColDef = colDef(format = colFormat(digits = 3))
    )
  })
  
  #### Buffering Indicator ############
  # Yearly Indicators of Watershed Functions
  
  observe({
    idf <- vd$output_df
    if (is.null(idf))
      return()
    idf$m <- month(idf$date)
    idf$year <- year(idf$date)
    idf$n <- 1
    
    mean_df <- aggregate(idf[c("I_DailyRain", "I_RFlowdata_mmday", "L_InFlowtoLake")], list(idf$year), mean, na.rm = T)
    colnames(mean_df) <- c("year", "PAvg", "QAvg", "QsAvg")
    idf <- merge(idf, mean_df, by = "year")
    idf$PabAvg <- pmax(idf$I_DailyRain - idf$PAvg, 0)
    idf$QabAvg <- pmax(idf$I_RFlowdata_mmday - idf$QAvg, 0)
    idf$QsabAvg <- pmax(idf$I_RFlowdata_mmday - idf$QsAvg, 0)
    
    sum_df <- aggregate(idf[c(
      "n",
      "I_DailyRain",
      "I_RFlowdata_mmday",
      "L_InFlowtoLake",
      "PabAvg",
      "QabAvg",
      "QsabAvg",
      "O_IntercAcc",
      "O_EvapoTransAcc",
      "O_SurfQFlowAcc",
      "O_SoilQFlowAcc",
      "O_BaseFlowAcc"
    )], list(idf$year), sum, na.rm = T)
    colnames(sum_df) <- c(
      "year",
      "n",
      "Ptot",
      "Qtot",
      "Qstot",
      "PabAvg",
      "QabAvg",
      "QsabAvg",
      "SumInterceptionE",
      "SumET_SoilplusVeg",
      "SumRO",
      "SumSoilQFlow",
      "Baseflow"
    )
    
    max_df <- aggregate(idf[c("I_DailyRain", "I_RFlowdata_mmday", "L_InFlowtoLake")], list(idf$year), max, na.rm = T)
    colnames(max_df) <- c("year", "PdailyMax", "QdailyMax", "QsdailyMax")
    sum_df <- merge(sum_df, max_df, by = "year")
    
    sum_mt_df <- aggregate(idf[c("I_RFlowdata_mmday", "L_InFlowtoLake")], list(idf$m, idf$year), sum, na.rm = T)
    colnames(sum_mt_df) <- c("m", "year", "Qmonthly", "Qsmonthly")
    
    mt_max_df <- aggregate(sum_mt_df[c("Qmonthly", "Qsmonthly")], list(sum_mt_df$year), max, na.rm = T)
    colnames(mt_max_df) <- c("year", "QmonthlyMax", "QsmonthlyMax")
    sum_df <- merge(sum_df, mt_max_df, by = "year")
    
    mt_min_df <- aggregate(sum_mt_df[c("Qmonthly", "Qsmonthly")], list(sum_mt_df$year), min, na.rm = T)
    colnames(mt_min_df) <- c("year", "QmonthlyMin", "QsmonthlyMin")
    sum_df <- merge(sum_df, mt_min_df, by = "year")
    
    
    
    sum_df$TotDischargeFrac <- sum_df$Qtot / sum_df$Ptot
    sum_df$BufferingIndicator <- 1 - (sum_df$QabAvg / sum_df$PabAvg)
    sum_df$RelBufferingIndicator <- 1 - (sum_df$QabAvg / sum_df$Qtot) / (sum_df$PabAvg /
                                                                           sum_df$Ptot)
    sum_df$BufferingPeak <- 1 - (sum_df$QdailyMax - sum_df$Qtot / sum_df$n) /
      (sum_df$PdailyMax - sum_df$Ptot / sum_df$n)
    sum_df$HighestMonthFrac <- 12 * sum_df$QmonthlyMax / sum_df$Qtot
    sum_df$LowestMonthFrac <- 12 * sum_df$QmonthlyMin / sum_df$Qtot
    
    sum_df$TotDischargeFrac_s <- sum_df$Qstot / sum_df$Ptot
    sum_df$BufferingIndicator_s <- 1 - (sum_df$QsabAvg / sum_df$PabAvg)
    sum_df$RelBufferingIndicator_s <- 1 - (sum_df$QsabAvg / sum_df$Qstot) / (sum_df$PabAvg /
                                                                               sum_df$Ptot)
    sum_df$BufferingPeak_s <- 1 - (sum_df$QsdailyMax - sum_df$Qstot / sum_df$n) /
      (sum_df$PdailyMax - sum_df$Ptot / sum_df$n)
    sum_df$HighestMonthFrac_s <- 12 * sum_df$QsmonthlyMax / sum_df$Qtot
    sum_df$LowestMonthFrac_s <- 12 * sum_df$QsmonthlyMin / sum_df$Qtot
    
    sum_df$OverlandFlowFrac <- sum_df$SumRO / sum_df$Ptot
    sum_df$SoilQflowFrac <- sum_df$SumSoilQFlow / sum_df$Ptot
    sum_df$SlowFlowFrac_Meth1 <- sum_df$TotDischargeFrac_s - sum_df$OverlandFlowFrac - sum_df$SoilQflowFrac
    sum_df$SlowFlowFrac_Meth2 <- (sum_df$Ptot - sum_df$SumInterceptionE - sum_df$SumET_SoilplusVeg -
                                    sum_df$SumRO) / sum_df$Ptot
    
    sum_df[sapply(sum_df, is.infinite)] <- NA
    vd$buffering_df <- sum_df
    
    ##### Average of Indicators of Watershed Functions #################
    
    avg_df <- data.frame(var = buff_var_df$label)
    
    min_v <- sapply(sum_df, min, na.rm = T)
    mean_v <- sapply(sum_df, mean, na.rm = T)
    max_v <- sapply(sum_df, max, na.rm = T)
    
    avg_df$min <- min_v[buff_var_df$obs_var]
    avg_df$avg <- mean_v[buff_var_df$obs_var]
    avg_df$max <- max_v[buff_var_df$obs_var]
    
    avg_df$min_s <- min_v[buff_var_df$sim_var]
    avg_df$avg_s <- mean_v[buff_var_df$sim_var]
    avg_df$max_s <- max_v[buff_var_df$sim_var]
    
    vd$wb_ind_avg <- avg_df
    
    #### Average of Water Balance  ###########
    
    avg_wb_df <- data.frame(var = water_balance_avg)
    wb_field <- c("Ptot", "", "Qtot", "", "", "")
    wb_field_sim <- c("Ptot",
                      "SumET_SoilplusVeg",
                      "Qstot",
                      "SumRO",
                      "SumSoilQFlow",
                      "Baseflow")
    avg_wb_df$min <- min_v[wb_field]
    avg_wb_df$avg <- mean_v[wb_field]
    avg_wb_df$max <- max_v[wb_field]
    
    avg_wb_df$min_s <- min_v[wb_field_sim]
    avg_wb_df$avg_s <- mean_v[wb_field_sim]
    avg_wb_df$max_s <- max_v[wb_field_sim]
    
    ratio_df <- avg_wb_df[, -1]
    c1 <- colnames(ratio_df)
    c2 <- paste0("r_", c1)
    colnames(ratio_df) <- c2
    ratio_df[, ] <- NA
    for (r in 2:6) {
      ratio_df[r, ] <- avg_wb_df[r, -1] / avg_wb_df[1, -1]
    }
    c12 <- as.vector(rbind(c1, c2))
    avg_wb_df <- cbind(avg_wb_df, ratio_df)[c("var", c12)]
    vd$wb_avg <- avg_wb_df
    
    wb_data_field <- append(wb_field_sim, "Qtot", after = 2)
    vd$wb_yearly <- sum_df[c("year", wb_data_field)]
    
  })
  
  buff_var_df <- data.frame(
    label = c(
      "Total Discharge Fraction",
      "Buffering Indicator",
      "Relative Buffering Indicator",
      "Buffering Peak Event",
      "Highest Month Fraction",
      "Overland Flow Fraction",
      "Soil Quick Flow Fraction",
      "Slow Flow Fraction",
      "Lowest Month Fraction"
    ),
    obs_var = c(
      "TotDischargeFrac",
      "BufferingIndicator",
      "RelBufferingIndicator",
      "BufferingPeak",
      "HighestMonthFrac",
      "",
      "",
      "",
      "LowestMonthFrac"
    ),
    sim_var = c(
      "TotDischargeFrac_s",
      "BufferingIndicator_s",
      "RelBufferingIndicator_s",
      "BufferingPeak_s",
      "HighestMonthFrac_s",
      "OverlandFlowFrac",
      "SoilQflowFrac",
      "SlowFlowFrac_Meth1",
      "LowestMonthFrac_s"
    )
  )
  
  water_balance_avg <- c(
    "Precipitation (mm)",
    "Evapotranspiration (mm)",
    "RiverFlow (mm)",
    "* RunOff (mm)",
    "* SoilQFlow (mm)",
    "* BaseFlow (mm)"
  )
  water_balance_labels <- append(water_balance_avg, "RiverFlow - Observed (mm)", after = 2)
  
  
  output$buff_avg_table_output <- renderReactable({
    df <- vd$wb_ind_avg
    if (is.null(df))
      return()
    reactable(
      df,
      striped = T,
      borderless = TRUE,
      rownames = F,
      pagination = F,
      columns = list(
        var = colDef(
          name = "",
          width = 250,
          align = "left"
        ),
        min = colDef(name = "Min", width = 100),
        avg = colDef(name = "Average", width = 100),
        max = colDef(
          name = "Max",
          width = 100,
          class = "border_right"
        ),
        min_s = colDef(name = "Min", width = 100),
        avg_s = colDef(name = "Average", width = 100),
        max_s = colDef(name = "Max", width = 100)
      ),
      columnGroups = list(
        colGroup(name = "Observed", columns = c("min", "avg", "max")),
        colGroup(
          name = "Simulated",
          columns = c("min_s", "avg_s", "max_s")
        )
      ),
      
      defaultColDef = colDef(format = colFormat(digits = 2), align = "center")
    )
  })
  
  output$wb_avg_table_output <- renderReactable({
    df <- vd$wb_avg
    if (is.null(df))
      return()
    percent_col <- colDef(
      name = "",
      width = 80,
      format = colFormat(
        percent = TRUE,
        digits = 1,
        prefix = "(",
        suffix = ")"
      ),
      class = "border_right"
    )
    reactable(
      df,
      striped = T,
      borderless = TRUE,
      rownames = F,
      pagination = F,
      columns = list(
        var = colDef(
          name = "",
          width = 200,
          sticky = "left"
        ),
        min = colDef(name = "Min", width = 80),
        r_min = percent_col,
        avg = colDef(name = "Average", width = 80),
        r_avg = percent_col,
        max = colDef(name = "Max", width = 80),
        r_max = percent_col,
        min_s = colDef(name = "Min", width = 80),
        r_min_s = percent_col,
        avg_s = colDef(name = "Average", width = 80),
        r_avg_s = percent_col,
        max_s = colDef(name = "Max", width = 80),
        r_max_s = percent_col
      ),
      columnGroups = list(
        colGroup(
          name = "Observed",
          columns = c("min", "avg", "max", "r_min", "r_avg", "r_max")
        ),
        colGroup(
          name = "Simulated",
          columns = c("min_s", "avg_s", "max_s", "r_min_s", "r_avg_s", "r_max_s")
        )
      ),
      defaultColDef = colDef(format = colFormat(digits = 1))
    )
  })
  
  output$wb_yr_table_output <- renderReactable({
    df <- vd$wb_yearly
    if (is.null(df))
      return()
    reactable(
      df,
      striped = T,
      borderless = TRUE,
      rownames = F,
      pagination = F,
      columns = list(year = colDef(
        name = "Year",
        format = colFormat(digits = 0),
        width = 80
      )),
      defaultColDef = colDef(format = colFormat(digits = 1))
    )
  })
  
  
  generate_ind_plot <- function(vars) {
    df <- vd$buffering_df
    if (is.null(df) || nrow(df) == 0)
      return(NULL)
    idf <- df[vars]
    fig <- plot_ly(x = df$year)
    for (i in 1:ncol(idf)) {
      fig <- fig |> add_trace(
        y = idf[[i]],
        type = "scatter",
        mode = "lines+markers",
        # name = labels[i-1])
        name = buff_var_df[buff_var_df$sim_var == vars[i], "label"]
      )
    }
    fig <- fig |>   layout(
      hovermode = 'x',
      legend = list(orientation = 'h', y = 100),
      #x = 0.5, y = 100),
      yaxis = list(title = "Indicators"),
      xaxis = list(title = "Year")
    )
    return(fig)
  }
  
  output$indicator1_plot <- renderPlotly({
    var <- c("OverlandFlowFrac", "TotDischargeFrac_s")
    generate_ind_plot(var)
  })
  
  output$indicator2_plot <- renderPlotly({
    var <- c("OverlandFlowFrac", "HighestMonthFrac_s")
    generate_ind_plot(var)
  })
  
  output$indicator3_plot <- renderPlotly({
    var <- c("SoilQflowFrac",
             "SlowFlowFrac_Meth1",
             "LowestMonthFrac_s")
    generate_ind_plot(var)
  })
  
  output$indicator4_plot <- renderPlotly({
    var <- c("BufferingIndicator_s",
             "RelBufferingIndicator_s",
             "BufferingPeak_s")
    generate_ind_plot(var)
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
  
  
  ### FLOWPER ################
  flowper_yearly <- reactiveVal()
  flowper_period <- reactiveVal()
  
  output$flowper_yearly_table <- renderReactable({
    df <- v$river_df
    rdf <- v$rain_df
    if (is.null(df) || is.null(rdf))
      return()
    
    df$year <- year(df$date)
    df$month <- month(df$date)
    plist <- list(c(1:3), c(4:6), c(7:9), c(10:12))
    for (i in 1:4) {
      df[df$month %in% plist[[i]], "period"] <- i
    }
    fp_year <- flowper(df, "river_flow", c("year"))
    fp_period <- flowper(df, "river_flow", c("year", "period"))

    
      rdf$year <- year(df$date)
      rdf$month <- month(df$date)
      for (i in 1:4) {
        rdf[rdf$month %in% plist[[i]], "period"] <- i
      }
      rain_yearly <- aggregate(rdf$rainfall, by = rdf["year"], sum, na.rm = T)
      colnames(rain_yearly) <- c("year", "rainfall")
      rain_period <- aggregate(rdf$rainfall, by = rdf[c("year", "period")], sum, na.rm = T)
      colnames(rain_period) <- c("year", "period", "rainfall")
      
      fp_year <- merge(fp_year, rain_yearly, by = "year", all.x = T)
      fp_period <- merge(fp_period,
                         rain_period,
                         by = c("year", "period"),
                         all.x = T)
      flowper_yearly(fp_year)
      flowper_period(fp_period)

    reactable(
      fp_year[c("year", "rainfall", "flowper")],
      columns = list(
        year = colDef(name = "Year", width = 60),
        rainfall = colDef(
          name = "Rainfall (mm)",
          format = colFormat(digits = 0),
          width = 120
        ),
        flowper = colDef(
          name = "Fp Value",
          format = colFormat(digits = 2),
          width = 80
        )
      ),
      columnGroups = list(colGroup(
        name = "Annual Data", columns = c("rainfall", "flowper")
      )),
      defaultColDef = colDef(align = "center"),
      pagination = F,
      details = function(index) {
        p_data <- fp_period[fp_period$year == fp_year$year[index], c("period", "rainfall", "flowper")]
        div(
          style = "padding:10px",
          tags$i("Seasonal Data", style = "color:#219ebc"),
          reactable(
            p_data,
            style = "background-color:#ECF9FC",
            columns = list(
              period = colDef(
                name = "Period",
                width = 65,
                align = "center"
              ),
              rainfall = colDef(name = "Rainfall (mm)", format = colFormat(digits = 0)),
              flowper = colDef(name = "Fp Value", format = colFormat(digits = 2))
            ),
            defaultColDef = colDef(align = "center")
            # outlined = TRUE
          )
        )
      }
    )
  })
  
  output$fp1_plot <- renderPlotly({
    df <- flowper_yearly()
    if (is.null(df))
      return()
    plot_ly(
      x = df$year,
      y = df$flowper,
      type = "scatter",
      mode = "lines+markers"
    ) |>
      layout(
        hovermode = 'x',
        yaxis = list(title = "Fp Value"),
        xaxis = list(title = "Year"),
        title = "Annual Fp-Value Over Time"
      )
  })
  
  output$fp2_plot <- renderPlotly({
    df <- flowper_period()
    if (is.null(df))
      return()
    df$period <- as.character(df$period)
    plot_ly(
      data = df ,
      x =  ~ year,
      y = ~ flowper,
      color = ~ period,
      type = "scatter",
      mode = "lines+markers"
    ) |>
      layout(
        hovermode = 'x',
        legend = list(
          orientation = 'h',
          y = 1,
          title = list(text = "<b>Period:</b>")
        ),
        yaxis = list(title = "Fp Value"),
        xaxis = list(title = "Year"),
        title = "Seasonal Fp-Value Over Time"
      )
  })
  
  output$fp3_plot <- renderPlotly({
    df <- flowper_yearly()
    if (is.null(df))
      return()
    plot_ly(
      x = df$rainfall,
      y = df$flowper,
      type = "scatter",
      mode = "markers"
    ) |>
      layout(
        hovermode = 'x',
        yaxis = list(title = "Fp Value"),
        xaxis = list(title = "Rainfall (mm)"),
        title = "Annual Fp-Value VS Rainfall"
      )
  })
  
  output$fp4_plot <- renderPlotly({
    df <- flowper_period()
    if (is.null(df))
      return()
    df$period <- as.character(df$period)
    plot_ly(
      data = df ,
      x =  ~ rainfall,
      y = ~ flowper,
      color = ~ period,
      type = "scatter",
      mode = "markers"
    ) |>
      layout(
        hovermode = 'x',
        legend = list(
          orientation = 'h',
          y = 1,
          title = list(text = "<b>Period:</b>")
        ),
        yaxis = list(title = "Fp Value"),
        xaxis = list(title = "Rainfall (mm)"),
        title = list(text = paste("Seasonal Fp-Value VS Rainfall"))
      )
  })
}
