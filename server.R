

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
    colorRampPalette(c(
      "#e07a5f",
      rep("#57cc99", 2),
      rep("#38a3a5", 4),
      rep("#0096c7", 8),
      rep("#0077b6", 16),
      rep("#023e8a", 32),
      rep("#03045e", 64)
    ))
  
  options(reactable.theme = reactableTheme(
    style = list(fontFamily = "Arial, Helvetica, sans-serif", fontSize = "1em")
  ))
  
  f_number <- function(v, unit = "", ...) {
    paste(format(v, big.mark = ",", scientific = F, ...), unit)
  }
  
  f_percent <- function(v) {
    sprintf("%0.1f%%", v * 100)
  }
  
  ### Conditional panel UI logic ###
  
  conditional_id <-
    c("is_dem_map",
      "is_stream_map",
      "is_stream_render",
      "is_contour",
      "is_lc_df")
  conditional_v <-
    c("dem_map_stars",
      "dem_stream_sf",
      "stream_render",
      "dem_contour_sf",
      "lc_df")
  
  mapply(function(id, val) {
    output[[id]] <- reactive({
      if (is.null(v[[val]]))
        return(FALSE)
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
  
  v <- reactiveValues(
    genriver_cfg = list(),
    
    lc_map_stars_list = NULL,
    lc_map_df = NULL,
    lc_df = data.frame(
      LC_ID = numeric(),
      color = character(),
      land_cover = character(),
      description = character(),
      stringsAsFactors = FALSE
    ),
    lc_par_df = data.frame(
      LC_ID = numeric(),
      land_cover = character(),
      intercept_class  = numeric(),
      rel_drought_fact  = numeric(),
      rel_bd = numeric(),
      stringsAsFactors = FALSE
    ),
    lc_evapot_df = NULL,
    evapot_month_data_df = data.frame(
      n = month_cols$month,
      month = month_cols$var,
      evapotranspiration =  rep(NA, 12),
      stringsAsFactors = FALSE
    ),
    
    map_boundary_sf = NULL,
    
    dem_map_file = NULL,
    dem_map_stars = NULL,
    dem_crop_stars = NULL,
    dem_direction_terra = NULL,
    
    dem_flow_bb_stars = NULL,
    dem_contour_bb_sf = NULL,
    dem_stream_bb_sf = NULL,
    
    ws_boundary_sf = NULL,
    ws_boundary_outlet_cfg = NULL,
    
    dem_flow_stars = NULL,
    dem_contour_sf = NULL,
    dem_stream_sf = NULL,
    stream_render = NULL,
    
    watershed_df = NULL,
    watershed_map_list = NULL,
    outlet_map_list = NULL,
    
    routing_map_stars = NULL,
    routing_river_sf = NULL,
    
    soil_map_stars = NULL,
    soil_map_sf = NULL,
    soil_layer_df = NULL,
    soil_metadata_df = NULL,
    soil_thetasat_stars = NULL,
    soil_thetasat_sf = NULL,
    
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
    )
  )
  
  vd <- reactiveValues(
    lc_evapot_disp_df = data.frame(
      LC_ID = numeric(),
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
    river_df = NULL
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
      
      "lc_par_df",
      "lc_evapot_df",
      "evapot_month_data_df",
      
      "map_boundary_sf",
      "ws_boundary_outlet_cfg",
      "watershed_df",
      "ws_boundary_sf",
      "dem_direction_terra",
      "dem_flow_stars",
      "dem_contour_sf",
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
      "evapotran_df",
      "rain_df",
      "river_df"
    ),
    file = c(
      "genriver",
      "landcovermap",
      "landcover",
      "map_list",
      
      "lc_props",
      "lc_evapot",
      "evapot_monthly",
      
      "map_boundary",
      "ws_boundary_outlet",
      "watershed",
      "ws_boundary",
      "dem_direction",
      "dem_flow",
      "contour",
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
      "evapotranspiration",
      "rain",
      "river"
    )
  )
  
  ### LAND COVER INPUT ############################################
  
  update_lc_legend <- function(lc_df, lc_map) {
    id <- sort(unique(lc_map[[1]]))
    if (is.null(lc_df)) {
      lc_df <- data.frame("lc_id" = id)
      lc_df$color <- map_color(length(id))
      lc_df$land_cover <- paste0("Landcover_", id)
      lc_df$land_use <- paste0("Landuse_", id)
      lc_df$description <- ""
    } else {
      dif <- as.numeric(setdiff(id, lc_df$lc_id))
      df <- lc_df
      if (length(dif) > 0) {
        a_df <- data.frame(
          lc_id = dif,
          color = chart_color[sample.int(length(chart_color), length(dif))],
          land_cover = paste0("Landcover_", dif),
          land_use = paste0("Landuse_", dif),
          description = ""
        )
        df <- rbind(lc_df, a_df)
      }
    }
  }
  
  ### INPUT R-FALLOW #########################
  map_id_counter <- 0
  get_next_map_id <- function() {
    map_id_counter <<- map_id_counter + 1
    return(map_id_counter)
  }
  
  observeEvent(input$rfalow_lc_map_inp, {
    dpaths <- input$rfalow_lc_map_inp$datapath
    fnames <- input$rfalow_lc_map_inp$name
    
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
        try(m <- read_stars(f, proxy = T), silent = T)
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
          }
          idx <- suffix(prefix(f, "."), "-")
          n <- as.numeric(idx)
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
  })
  
  observe({
    ms <- v$lc_map_stars_list
    if (is.null(ms))
      return()
    lc_df <- isolate(v$lc_df)
    if(is.null(lc_df) || nrow(lc_df) == 0) {
      ids <- unlist(lapply(ms, function(x) {
        unique(as.vector(st_as_stars(x)[[1]]))
      }))
      id <- unique(ids)
      id <- sort(id[!is.na(id) & id >= 0])
      if (length(id) == 0) {
        lc_df = data.frame(
          LC_ID = numeric(),
          color = character(),
          land_cover = character(),
          description = character(),
          stringsAsFactors = FALSE
        )
      } else {
        lc_df <- data.frame("LC_ID" = id)
        lc_df$color <- map_color(length(id))
        lc_df$land_cover <- paste0("Landcover_", id)
        lc_df$description <- ""
      }
      v$lc_df <- lc_df
    }
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
          breaks = c(-1, df$LC_ID),
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
    var = c("intercept_class", "rel_drought_fact", "rel_bd"),
    label = c(
      "Potential Interception (mm day-1)",
      "Relative Drought Threshold",
      "BD/BDref"
    )
  )
  
  
  lc_par_df_display <- function() {
    table_edit_server(
      "lc_props_table",
      isolate(v$lc_par_df),
      col_title = c("LC_ID", "Land Cover", lc_prop_cols$label),
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
    df <- v$lc_df
    if (nrow(df) == 0)
      return()
    pdf <- isolate(v$lc_par_df)
    if (is.null(pdf) || nrow(pdf) == 0) {
      pdf <- df[c("LC_ID", "land_cover")]
      pdf[lc_prop_cols$var] <- NA
    } else {
      adf <- df[c("LC_ID", "land_cover")]
      pdf <- merge(adf, pdf[c("LC_ID", lc_prop_cols$var)], by = "LC_ID", all.x = T)
    }
    v$lc_par_df <- pdf
    lc_par_df_edited <- lc_par_df_display()
    
    edf <- isolate(vd$lc_evapot_disp_df)
    if (nrow(edf) == 0) {
      edf <- df[c("LC_ID", "land_cover")]
      edf[month_cols$var] <- NA
    } else {
      adf <- df[c("LC_ID", "land_cover")]
      edf <- merge(adf, edf[c("LC_ID", month_cols$var)], by = "LC_ID", all.x = T)
    }
    vd$lc_evapot_disp_df <- edf
    lc_evapot_df_edited <- lc_evapot_df_display()
  })
  
  observe({
    edf <- vd$lc_evapot_disp_df
    if (nrow(edf) == 0) {
      vd$lc_evapot_df <- NULL
    } else {
      df <- melt(edf[, !(names(edf) %in% c("land_cover"))], id = c("LC_ID"))
      df <- merge(df, month_cols, by.x = "variable", by.y = "var")
      vd$lc_evapot_df <- df[c("LC_ID", "month", "value")]
    }
  })
  
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
  
  ### INPUT DEM MAP #########################
  observeEvent(input$dem_map_inp, {
    v$dem_map_file <- input$dem_map_inp
    v$dem_map_stars <- read_stars(v$dem_map_file$datapath, proxy = T)
    if (is.na(st_crs())) {
      v$dem_map_stars <- st_transform(v$dem_map_stars, crs = 4326)
    }
  })
  
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
        v$dem_contour_bb_sf <- generate_contour_sf(v$dem_map_stars)
        dem_out <- flowdem(v$dem_map_stars)
        v$dem_direction_terra <- dem_out$dem_direction_terra
        v$dem_flow_bb_stars <- dem_out$dem_flow_stars
      }
      setProgress(0.9, detail = "Generating stream path")
      t <- isolate(input$dem_flow_threshold)
      v$genriver_cfg$flow_threshold <- t
      print("DEM -> stream")
      v$dem_stream_bb_sf <- generate_stream(isolate(v$dem_flow_bb_stars), t)
      setProgress(1, detail = "Done!")
      print("DEM -> done!")
    })
    
    leafletProxy(active_watershed_lf, session) |>
      clearShapes() |>
      clearImages() |>
      clearControls() |>
      show_contour(v$dem_contour_bb_sf) |>
      show_stream(v$dem_stream_bb_sf)
    
  }
  
  generate_contour_sf <- function(dem, n_legend = 5) {
    dem <- st_as_stars(dem)
    dem_min <- min(dem[[1]], na.rm = T)
    dem_max <- max(dem[[1]], na.rm = T)
    dem_n <- n_legend
    dem_d <- (dem_max - dem_min) / dem_n
    mdem <- (round((dem - dem_min) / dem_d) * dem_d) + dem_min
    pdem <- st_as_sf(mdem,
                     as_points = F,
                     merge = T,
                     connect8 = T)
    ps <- st_transform(pdem, crs = 4326)
    ps <- st_sf(geometry = ps)
    return(ps)
  }
  
  #### FLOWDEM process ###########################################
  # https://github.com/KennethTM/flowdem
  flowdem <- function(m) {
    n <- 6
    dem <- rast(as(m, "Raster"))
    # Breach DEM, that is, resolve depression by "carving" through obstacles
    setProgress(1 / n, detail = "Breaching DEM")
    print("DEM -> breach")
    dem_breach <- breach(dem)
    # Use fill with epsilon on breached DEM to resolve flats and ensure drainage
    setProgress(2 / n, detail = "Filling DEM")
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
  
  
  #
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
    
    if (isolate(is_repaint_watershed())) {
      lf <- reset_watershed_panel_display(lf)
      is_repaint_watershed(F)
    }
    
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
    }
    return(lf)
  })
  
  reset_watershed_panel_display <- function(lf) {
    if(is.null(v$map_boundary_sf)) return(lf)
    print("reset watershed panel")
    lf <- lf |>
      clearShapes() |>
      clearMarkers() |>
      clearControls() |>
      show_contour(v$dem_contour_sf) |>
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
            "Minimum threshold of flow accumulation (contributing cells)"
          ),
          input_def = c(100),
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
  
  ### DEM GET ########################################
  
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
    # res <- GET(get_topo_url, shinyhttr::progress(session, id = "pb"))
    res <- GET(get_topo_url)
    print(res$all_headers)
    setwd(tempdir())
    f <- paste0(tempfile("dem"), ".tif")
    writeBin(res$content, f)
    
    # f <- "dem.tif"
    try(m <- read_stars(f))
    if (!is.null(m)) {
      v$dem_map_stars <- m
      val <- sort(unique(as.vector(m[[1]])))
      pal <- colorNumeric(
        c(
          "#264653",
          "#2a9d8f",
          "#e9c46a",
          "#f4a261",
          "#e76f51",
          "#ec8c74",
          "#f0a390",
          "#FFF"
        ),
        val,
        na.color = "transparent"
      )
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
        addStarsImage(m, colors = pal, opacity = 0.8) |>
        addLegend(pal = pal,
                  values = val,
                  title = "Elevation")
    }
    is_set_ws_boundary <<- T
    
    hidePageSpinner()
    is_spinner(F)
  })
  
  show_contour <- function(lf, contour_f) {
    if(is.null(contour_f)) return(lf)
    dem <- contour_f
    names(dem) <- c("val", "geometry")
    col_list <- data.frame(val = sort(unique(dem$val)))
    col_list$color <- terrain.colors(nrow(col_list))
    dem <- merge(dem, col_list, by = "val")
    print("* add contour")
    lf <- lf |>
      addFeatures(
        dem,
        stroke = F,
        fillColor = ~ color,
        fillOpacity = 0.2,
        label = map_label("Watershed area"),
        labelOptions = labelOptions(className = "map_label", offset = c(0, -5))
      ) |>
      fit_map_view(dem)
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
    footer_div <- ""
    if (footer != "")
      footer_div <- paste("<strong><em>", footer, "</em></strong></div>")
    paste("<div style='font-size:1.2em;'>",
          title_div,
          "<p>",
          desc,
          "</p>",
          footer_div) |>
      lapply(htmltools::HTML)
  }
  
  show_stream <- function(lf,
                          stream_f,
                          is_show_label = T,
                          opacity = 0.8) {
    m <- stream_f
    if (is.null(m)) {
      return(lf)
    }
    names(m) <- c("val", "geometry")
    s_list <- data.frame(val = sort(unique(m$val)))
    nr <- nrow(s_list)
    nleg <- 6
    # colors <- stream_color(nleg)
    s_list$w <- ceiling(nleg * c(1:nr) / nr)
    s_list$color <- stream_color(nr)
    m <- merge(m, s_list, by = "val")
    if (is_show_label) {
      labels <- map_label(
        paste("Contributing cell of flows:", f_number(m$val)),
        "Stream path",
        "* Click here to add the outlet"
      )
      
      #stream path
      lf <- lf |> addFeatures(
        m,
        color = ~ color,
        fillOpacity = opacity,
        opacity = opacity,
        weight = ~ w,
        group = "stream",
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
        group = "stream"
      )
    }
    return(lf)
  }
  
  fit_map_view <- function(lf, m) {
    if(is.null(m)) return(lf)
    bb <- as.list(st_bbox(st_transform(st_as_sfc(st_bbox(
      m
    )), 4326)))
    lf <- lf |> fitBounds(bb$xmin, bb$ymin, bb$xmax, bb$ymax)
    return(lf)
  }
  
  ### SET WATERSHED BOUNDARY#########################
  
  selected_ws_outlet_id <- NULL
  is_set_ws_boundary <- F
  
  observeEvent(input$set_watershed_btn, {
    removeModal()
    if (is.null(selected_ws_outlet_id))
      return()
    ws <- v$outlet_map_list[[selected_ws_outlet_id]]
    v$ws_boundary_sf <- ws$map
    v$ws_boundary_outlet_cfg <- list(lon = ws$lon,
                                     lat = ws$lat,
                                     area = st_area(ws$map))
    
    v$watershed_df <- NULL
    v$watershed_map_list <- NULL
    v$outlet_map_list <- NULL
    
    v$dem_crop_stars <- crop_raster(v$dem_map_stars, v$ws_boundary_sf)
    v$dem_contour_sf <- generate_contour_sf(v$dem_crop_stars)
    v$dem_flow_stars <- crop_raster(v$dem_flow_bb_stars, v$ws_boundary_sf)
    v$dem_stream_sf <- generate_stream(v$dem_flow_stars)
    
    withProgress(message = 'Generating stream routing distance', value = 0, {
      rm <- generate_routing_dist_map(v$dem_flow_stars, progress = setProgress)
    })
    strm <- rm[v$dem_flow_stars >= v$genriver_cfg$flow_threshold] / 1000
    strm_sf <- generate_rounded_polygon(strm, 1)
    #generate routing distance
    v$routing_river_sf <- strm_sf
    v$routing_map_stars <- rm
    #query soil properties
    generate_soil_map()
    #TODO: terra::terrain() for generating slope map
    ws_id_last <<- 0
    reset_watershed_panel_display(leafletProxy(active_watershed_lf, session))
    is_set_ws_boundary <<- F
  })
  
  crop_raster <- function(raster_map, boundary_sf) {
    raster_map <- st_as_stars(raster_map)
    dim <- st_dimensions(raster_map)
    b_raster <- st_rasterize(boundary_sf,
                             dx = dim$x$delta,
                             dy = dim$y$delta)
    raster_map <- st_crop(raster_map, st_bbox(b_raster))
    st_dimensions(raster_map) <- st_dimensions(b_raster)
    raster_map[is.na(b_raster)] <- NA
    return(raster_map)
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
    #create outlet shape
    pt.df   <- data.frame(pt = 1, x = lon, y = lat)
    p   <- st_as_sf(pt.df, coords = c("x", "y"))
    st_crs(p) <- 4326
    p <- st_transform(p, crs = 7801)
    circle <-  st_buffer(p, outlet_radius)
    circle <- st_transform(circle, crs = 4326)
    print("Watershed -> generate")
    #generate watershed
    dem_dir <- isolate(v$dem_direction_terra)
    ws <- watershed(dem_dir, circle)
    m <- st_as_stars(ws)
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
      f_number(a, "ha", nsmall = 2)
    ),
    "Subcatchment area")
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
        f_number(a, "ha", nsmall = 2)
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
  
  watershed_col <- c("ws_id", "color", "area")
  outlet_col <- c("out_id", "color", "out_lon", "out_lat", "area")
  ws_df <- NULL
  
  output$watershed_list <- renderReactable({
    df <- v$watershed_df
    if (is.null(df) || nrow(df) == 0)
      return()
    ws_df <<- aggregate(df$area, list(df$ws_id, df$color), FUN = sum)
    colnames(ws_df) <<- watershed_col
    ws_df$idx <<- get_numeric_id(ws_df$ws_id)
    ws_df <<- ws_df[order(ws_df$idx), ]
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
        ws_id = colDef(name = "ID", width = 60),
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
    format = colFormat(digits = 2, separators = T),
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
  
  output$routing_map_leaflet <- renderLeaflet({
    lf <- base_leaflet()
    
    m <- v$routing_map_stars
    val <- sort(unique(as.vector(m[[1]]))) / 1000
    pal <- colorNumeric(grad_color(10), val, na.color = "transparent")
    if (!is.null(m)) {
      lf <- lf |>
        addGeoRaster(
          m,
          colorOptions = colorOptions(palette = grad_color(200)),
          opacity = 0.6,
          layerId = 'routing_map'
        ) |>
        addLegend(
          pal = pal,
          values = val,
          opacity = 0.6,
          title = "Distance (km)"
        ) |>
        addHomeButton(ext = st_bbox(m),
                      group = "Routing distance map",
                      position = "topleft") |>
        show_routing_river(v$routing_river_sf)
    }
    return(lf)
  })
  
  show_routing_river <- function(lf, route_f) {
    m <- route_f
    if (is.null(m)) {
      return(lf)
    }
    names(m) <- c("val", "geometry")
    s_list <- data.frame(val = sort(unique(m$val)))
    nr <- nrow(s_list)
    s_list$color <- grad_color(nr)
    m <- merge(m, s_list, by = "val")
    
    labels <- map_label(paste("Distance to the main outlet:", f_number(m$val), "km"),
                        "Stream routing path")
    lf <- lf |>
      show_stream(v$dem_stream_sf, F, 0.5) |>
      addFeatures(
        m,
        color = ~ color,
        fillOpacity = 0.5,
        opacity = 0.5,
        weight = 1,
        group = "route",
        label = ~ labels,
        labelOptions = labelOptions(
          className = "map_label",
          offset = c(0, -10),
          direction = "top"
        ),
        highlightOptions = highlightOptions(color  = "red", fillColor = "red")
      )
    return(lf)
  }
  
  ### 3D view ####################################
  
  output$plot3d <- renderRglwidget({
    dem <- v$dem_crop_stars
    if (is.null(dem))
      return()
    try(close3d())
    showPageSpinner(caption = "Please wait while rendering")
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
  
  observeEvent(input$soil_map_leaflet_shape_click, {
    click_inp <- input$soil_map_leaflet_shape_click
    if (!is.null(click_inp$id)) {
      if (click_inp$id == "ws_boundary") {
        withProgress(message = 'Please wait while generating soil map',
                     value = 0,
                     
                     generate_soil_map())
      }
    }
  })
  
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
    d$header$general <- c("Soil type", d$header$general)
    d$header$physical <- c("Soil_depth", d$header$physical)
    d$header$chemical <- c("Soil_depth", d$header$chemical)
    return(d)
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
    mb <- v$ws_boundary_sf
    if (is.null(mb)) {
      return()
    }
    sf_use_s2(FALSE)
    withProgress(message = 'Generating soil map', value = 0, {
      #get global soil map
      setProgress(0.05, detail = "Get the global soil map")
      setwd(default_wd)
      smap <- read_stars(soil_map_path, proxy = T)
      smap_crs <- st_crs(smap)
      #crop to bounding box of watershed and expand a bit to avoid resolution conversion lost
      setProgress(0.1, detail = "Cropping the global soil map")
      
      bbsf <- st_as_sfc(st_bbox(v$dem_flow_stars))
      bbsub <- st_as_stars(smap[bbsf])
      #convert to DEM resolution
      bbsub_h <- rescale(bbsub, v$dem_flow_stars, setProgress)
      msub <- bbsub_h[mb]
      #convert soil map raster to polygon
      msf <- stars_to_sf(msub)
      names(msf) <- c("smu_id", "geometry")
      
      # query database
      sid <- sort(unique(msf$smu_id))
      generate_soil_db(sid)
      
      v$soil_map_sf <- msf
      v$soil_map_stars <- msub
      paint_soil_map(leafletProxy("soil_map_leaflet", session))
      
      calculate_hydroprop()
      setProgress(0.98, detail = "Rendering map")
    })
  }
  
  calculate_hydroprop <- function() {
    print("calculate theta sat")
    df <- isolate(v$soil_layer_df)
    msub <- isolate(v$soil_map_stars)
    
    tc <- c("SMU_ID", "SHARE", "soil_type")
    sc <- c("SAND",
            "SILT",
            "CLAY",
            "BULK",
            "REF_BULK",
            "ORG_CARBON",
            "CEC_SOIL",
            "PH_WATER")
    sdf <- df[df$soil_depth == "0-20", c(tc, sc)]
    sdf <- sdf[!duplicated(sdf), ]
    sdf[sc] <- sdf[sc] * sdf$SHARE / 100
    sumdf <- aggregate(sdf[sc], list(SMU_ID = sdf$SMU_ID), sum)
    sumdf$thetasat <- pt.thetaSat.tropic(sumdf$CLAY,
                                         sumdf$SAND,
                                         sumdf$BULK,
                                         sumdf$CEC_SOIL,
                                         sumdf$PH_WATER)
    theta_m <- msub
    for (id in sumdf$SMU_ID) {
      theta_m[msub == id] <- sumdf[sumdf$SMU_ID == id, ]$thetasat
    }
    
    theta_sf <- stars_to_sf(theta_m)
    # names(theta_sf) <- c("thetasat", "geometry")
    v$soil_thetasat_sf <- theta_sf
    v$soil_thetasat_stars <- theta_m
  }
  
  stars_to_sf <- function(m) {
    msf <- st_as_sf(m,
                    as_points = F,
                    merge = T,
                    connect8 = T)
    msf <- st_transform(msf, crs = 4326)
    names(msf) <- c("val", "geometry")
    # merge polygon for each smu_id
    sid <- sort(unique(msf$val))
    a <- lapply(sid, function(x) {
      st_as_sf(st_union(msf[msf$val == x, ]))
    })
    m_a <- do.call(rbind, a)
    st_geometry(m_a) <- "geometry"
    msf <- cbind(m_a, val = sid)
    return(msf)
  }
  
  
  
  output$soil_thetasat_leaflet <- renderLeaflet({
    m <- v$soil_thetasat_sf
    if (is.null(m))
      return()
    labels <- map_label(paste("Theta Sat:", f_number(m$val)), "Soil hydraulic properties")
    lf <- base_leaflet()
    lf <- lf |> addFeatures(
      m,
      group = "thetasat",
      color = chart_color[1:nrow(m)],
      label = ~ labels,
      highlightOptions = highlightOptions(fillOpacity = 0.8),
      fillOpacity = 0.6,
      stroke = F,
      labelOptions = labelOptions(className = "map_label", offset = c(0, -5))
    )
    return(lf)
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
    if (is.null(v$ws_boundary_outlet_cfg$area) ||
        v$ws_boundary_outlet_cfg$area <= 0)
      return()
    # print(df)
    df <- df[order(df$date), ]
    df$year <- year(df$date)
    df$month <- month(df$date)
    df$day <- yday(df$date)
    df$flow_mm <- convert_flow_to_mmdaily(df$river_flow, v$ws_boundary_outlet_cfg$area)
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
          f_number(v$ws_boundary_outlet_cfg$area / 1000000, nsmall = 2),
          "km",
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
          f_number(v$ws_boundary_outlet_cfg$area / 1000000, nsmall = 2),
          "km",
          tags$sup(2, .noWS = c("before"))
        ),
        layout_column_wrap(width = 1 / 2, !!!lapply(yrlist, function(x) {
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
    if (is_watershed_leaflet_base) {
      lf <- leafletProxy(active_watershed_lf, session)
      reset_watershed_panel_display(lf)
    } else {
      is_repaint_watershed(T)
    }
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
    
    evapotran_df_edited <- update_evapotran_df_display()
    rain_df_edited <- update_rain_df_display()
    river_df_edited <- update_river_df_display()
    # update map id counter
    map_df <- isolate(v$lc_map_df)
    map_idc <- as.numeric(unlist(lapply(map_df$map_id, suffix)))
    map_id_counter <<- max(map_idc)
    print("parameters uploaded!")
  }
  
  
}
