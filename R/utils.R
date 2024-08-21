## GENERAL UTILITY LIBRARY #############

shp_ext <- c(".shp", ".shx", ".prj", ".dbf")

write_data <- function(data, fname, type) {
  if(is.null(data)) return()
  if (type == "df") {
    if(nrow(data) == 0) return()
    fname <- paste0(fname, ".csv")
    write.csv(data, fname, row.names = F, na = "")
  } else if (type == "cfg") {
    fname <- paste0(fname, ".yaml")
    write_yaml(data, fname)
  } else if (type == "stars") {
    fname <- paste0(fname, ".tif")
    write_stars(data, fname)
  } else if (type == "terra") {
    fname <- paste0(fname, ".tif")
    writeRaster(data, fname, T)
  } else if (type == "sf") {
    st_write(data,
             paste0(fname, ".shp"),
             append = F,
             quiet = T)
    fname <- paste0(fname, shp_ext)
  }
  return(fname)
}

save_variables <- function(io_file_df, vlist) {
  fs <- c()
  for(i in 1:nrow(io_file_df)) {  
    f <- io_file_df[i, "file"]
    vr <- unlist(strsplit(io_file_df[i, "var"], split = ".", fixed = T))
    vrlab <- vr[1]
    data <- vlist[[vrlab]]
    i <- 2
    #trace the variable within the list for a nested variable
    while (i <= length(vr)) {
      vrlab <- vr[i]
      data <- vlist[[vrlab]]
      i <- i + 1
    }
    if (!is.null(data)) {
      # type <- tail(unlist(strsplit(vrlab, "_")), 1)
      type <- suffix(vrlab)
      if (type == "list") {
        # type2 <- tail(unlist(strsplit(vrlab, "_")), 2)[1]
        type2 <- suffix(vrlab, n = 2)[1]
        ns <- names(vlist[[vrlab]])
        for (n in ns) {
          data <- vlist[[vrlab]][[n]]
          f2 <- paste0(f, "_", n)
          fs <- c(fs, write_data(data, f2, type2))
        }
      } else {
        fs <- c(fs, write_data(data, f, type))
      }
      
    }
  }
  return(fs)
}

read_data <- function(fpath, type) {
  data <- NULL
  if (type == "stars") {
    fpath <- paste0(fpath, ".tif")
    if (file.exists(fpath))
      data <- suppressWarnings(read_stars(fpath))
  } else if (type == "terra") {
    fpath <- paste0(fpath, ".tif")
    if (file.exists(fpath))
      data <- rast(fpath)
  } else if (type == "sf") {
    fpath <- paste0(fpath, ".shp")
    if (file.exists(fpath))
      data <- st_read(fpath, quiet = T)
  } else if (type == "df") {
    fpath <- paste0(fpath, ".csv")
    if (file.exists(fpath))
      data <- read.csv(fpath)
  } else if (type == "cfg") {
    fpath <- paste0(fpath, ".yaml")
    if (file.exists(fpath))
      data <- read_yaml(fpath)
  }
  return(data)
}

upload_variables <- function(io_file_df, data_dir = "/", vlist = NULL) {
  if(is.null(vlist))
    vlist <- list()
  for(i in 1:nrow(io_file_df)) {  
    f <- io_file_df[i, "file"]
    fpath <- paste0(data_dir, "/", f)
    iv <- unlist(strsplit(io_file_df[i,"var"], split = ".", fixed = T))
    vr <- tail(iv, 1)
    # type <- tail(unlist(strsplit(vr, "_")), 1)
    type <- suffix(vr)
    if (type == "list") {
      # type2 <- tail(unlist(strsplit(vr, "_")), 2)[1]
      type2 <- suffix(vr, n = 2)[1]
      fs <- list.files(data_dir)
      fs <- fs[substr(fs, 1, nchar(f)) == f]
      for (fn in fs) {
        ff <- unlist(strsplit(fn, ".", fixed = T))[1]
        id <- gsub("^.*?_", "", ff)
        fpath <- paste0(data_dir, "/", ff)
        data <- read_data(fpath, type2)
        vlist[[iv[1]]][[id]] <- data
      }
    } else {
      data <- read_data(fpath, type)
      i1 <- iv[1]
      if (length(iv) == 1) {
        vlist[[i1]] <- data
      } else if (length(iv) == 2) {
        i2 <- iv[2]
        vlist[[i1]][[i2]] <- data
      } else if (length(iv) == 3) {
        i2 <- iv[2]
        i3 <- iv[3]
        vlist[[i1]][[i2]][[i3]] <- data
      }
    }
  }
  return(vlist)
}

suffix <- function(v, sep = "_", n = 1) {
  tail(unlist(strsplit(v, sep, fixed = T)), n)
}

prefix <- function(v, sep = "_", n = 1) {
  p <- unlist(strsplit(v, sep, fixed = T))
  return(p[1:min(length(p), n)])
}

##############

chart_color <- c(
  paletteer_d("ggthemes::calc"),
  paletteer_d("ggsci::schwifty_rickandmorty"),
  paletteer_d("ggsci::default_uchicago"),
  paletteer_d("ggthemes::Classic_10"),
  paletteer_d("ggsci::default_jco"),
  paletteer_d("ggsci::springfield_simpsons"),
  paletteer_d("ggsci::light_uchicago"),
  paletteer_d("ggthemes::stata_s2color"),
  paletteer_d("RColorBrewer::Set1"),
  paletteer_d("RColorBrewer::Set2"),
  paletteer_d("RColorBrewer::Set3"),
  paletteer_dynamic("cartography::multi.pal", 20)
)

light_color <- c(
  paletteer_d("ggsci::legacy_tron"),
  paletteer_d("ggthemes::Superfishel_Stone"),
  paletteer_d("ggthemes::Classic_10_Light"),
  paletteer_d("ggpomological::pomological_palette"),
  paletteer_d("ggthemes::Tableau_10"),
  paletteer_d("ggthemes::Classic_10_Medium"),
  paletteer_dynamic("cartography::pastel.pal", 20)
)

get_color <- function(idx = NULL, is_light = F) {
  if (is_light) {
    cl <- light_color
  } else {
    cl <- chart_color
  }
  max_idx <- length(cl)
  if (is.null(idx))
    idx <- sample.int(max_idx, 1)
  idx <- abs(idx)
  idx <- idx %% max_idx
  idx[idx == 0] <- max_idx
  return(cl[idx])
}

#TODO: to compare with JSON method, which one is faster
list_to_df <- function(d) {
  id <- names(d)
  if (is.null(id))
    id <- c(1:length(d))
  df <- NULL
  for (i in id) {
    e <- d[[i]]
    edf <- data.frame(id = i)
    if (class(e) == "list") {
      f <- names(e)
      if (is.null(f))
        f <- c(1:length(e))
      for (x in f) {
        edf[[x]] <- e[[x]]
      }
    } else {
      edf[["x"]] <- e
    }
    if (is.null(df)) {
      df <- edf
    } else {
      df <- rbind(df, edf)
    }
  }
  return(df)
}

list_to_df_json <- function(d) {
  return(jsonlite::fromJSON(jsonlite::toJSON(d)))
}

day_to_sec <- 86400

convert_flow_to_mmdaily <- function(flow_m3psec, area_m2) {
  return((flow_m3psec/area_m2)*day_to_sec*1000)
}

