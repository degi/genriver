####################################
### FUNCTION LIBRARY OF GENRIVER ###
####################################



#' Generate routing distance from the flow map of watershed analysis
#'
#' The algorithm trace the path from the final outlet, which is the cell
#' with highest flow counts.
#' to the lower flow cell, but the highest within the starting cell neighbors.
#' The trace will always start from the highest flows cell of the neighbors path.
#'
#' @param flow_map a stars raster map of flow counts
#' @return a routing distance map
#' @export
#'


stars_to_proxy <- function(m, f = "") {
  f <- paste0(tempfile(f), ".tif")
  write_stars(m, f)
  return(read_stars(f, proxy = T))
}


stars_to_terra <- function(m) {
  sr = as(m, "Raster")
  return(rast(sr))
}

cellSize.stars <- function(m, ...) {
  cellSize(stars_to_terra(m), ...)[1]$area
} 


generate_stream <- function(flow_map, threshold = 50) {
  if (is.null(flow_map)) {
    return()
  }
  m <- st_as_stars(flow_map)
  #converted flow celss to ha area
  mr <- m * cellSize.stars(m, unit = "ha") 
  mr[mr < threshold] <- NA
  ps <- generate_rounded_polygon(mr, 100, threshold)
  return(ps)
}

generate_rounded_polygon <- function(raster_map, rounding = 100, minval = 0) {
  mr <- raster_map
  mr <- round(mr / rounding) * rounding
  mr[mr == 0] <- minval
  ps <- st_as_sf(mr,
                 as_points = F,
                 merge = T,
                 connect8 = T) |>
    st_transform(crs = 4326)
  ps <- st_sf(geometry = ps)
  return(ps)
}

dcross <- sqrt(2)
ncell_dist_factor <- c(dcross, 1, dcross, 1, 0, 1, dcross, 1, dcross)

generate_river_dist <- function(flow_map, threshold = 20) {
  # res <- st_res(flow_map)
  # p <- st_sfc(st_point(c(0, 0)), st_point(c(res[1], 0)), crs = st_crs(flow_map))
  # wres <- st_distance(p)[1, 2]
  
  cs <- cellSize.stars(flow_map)
  wres <- sqrt(cs)
  dmat <- matrix(ncell_dist_factor * wres, nrow = 3, ncol = 3)
  
  #get the matrix from the map and convert to area size scale
  
  # mr <- flow_map * cellSize.stars(m, unit = "ha") 
  # mr[mr < threshold] <- NA
  # 
  m <- flow_map * cs/10000
  
  # m_ha <- m * cs/10000
  # print(m_ha)
  m[m < threshold] <- NA
  m <- m[[1]]
  md <- matrix(NA, nrow(m), ncol(m))
  #start from the highest flow cell
  o <- which(m == max(m, na.rm = T), arr.ind = TRUE)[1, ]
  dist <- 0
  #dataframe for second biggest flow cells
  m2 <- data.frame(
    row = as.numeric(),
    col = as.numeric(),
    val = as.numeric(),
    stringsAsFactors = FALSE
  )
  max2 <- 1
  while (T) {
    #assign the distance to the distance map (md)
    md[o[1], o[2]] <- dist
    #remove the cell from the main map by assigning NA
    m[o[1], o[2]] <- NA
    n <- get_neighbors(m, o)
    #loop for the next second biggest flows
    while (all(is.na(n)) && nrow(m2) > 0) {
      mx_idx <- which(m2[, 3] == max(m2[, 3], na.rm = T), arr.ind = TRUE)
      o <- c(m2[mx_idx[1], 1], m2[mx_idx[1], 2])
      m2 <- m2[-mx_idx, ]
      if (is.na(m[o[1], o[2]]))
        next
      #Assign the distance to the neighbor shortest path
      nd <- get_neighbors(md, o)
      fd <- which(nd == min(nd, na.rm = T), arr.ind = TRUE)
      ridx <- o[1] + fd[1, 1] - 2
      cidx <- o[2] + fd[1, 2] - 2
      dist <- md[ridx, cidx] + dmat[fd[1, 1], fd[1, 2]]
      #assign
      md[o[1], o[2]] <- dist
      m[o[1], o[2]] <- NA
      n <- get_neighbors(m, o)
    }
    #find the max neighbors
    if(all(is.na(n))) {
      break
    } else {
      f <- which(n == max(n, na.rm = T), arr.ind = TRUE)
    }

    #remove the first max value
    n[f[1, 1], f[1, 2]] <- NA
    #find index of 2nd max neighbors
    if(!all(is.na(n))) {
      f2 <- which(n == max(n, na.rm = T), arr.ind = TRUE)
      ridx <- o[1] + f2[1, 1] - 2
      cidx <- o[2] + f2[1, 2] - 2
      m2 <- rbind(m2, c(ridx, cidx, m[ridx, cidx]))
      max2 <- max(max2, m[ridx, cidx])
    }
    #add distance from the next point
    dist <- md[o[1], o[2]] + dmat[f[1, 1], f[1, 2]]
    #connect to the next largest neighbor
    o[1] <- o[1] + f[1, 1] - 2
    o[2] <- o[2] + f[1, 2] - 2
  }
  flow_map[[1]] <- md
  print(all(is.na(m)))
  m_res <- flow_map
  m_res[[1]] <- m
  plot(m_res)
  return(flow_map)
}

# profiling lib
# library(profvis)

generate_routing_dist_map <- function(flow_map, river_threshold = 20, progress = NULL) {
  if(is.null(progress)) progress <- function(x) {}
  #set the cell width from the map resolution in m unit
  res <- st_res(flow_map)
  p <- st_sfc(st_point(c(0, 0)), st_point(c(res[1], 0)), crs = st_crs(flow_map))
  wres <- st_distance(p)[1, 2]
  ndist <- ncell_dist_factor * wres
  progress(0.1)
  #generate routing distance for the main river
  rm <- generate_river_dist(flow_map, river_threshold)
  progress(0.2)
  #iterate until all of the cells calculated
  prog <- 0.2
  isfull <- F
  while (!isfull) {
    prog <- prog + 0.01
    prev_rm <- rm
    #remove the cell that has been calculated
    m <- flow_map[[1]]
    m[!is.na(rm[[1]])] <- NA
    #get the 8 neighboring layer of cells
    m_list <- get_neighbor_layers(m)
    #find the highest flow for each neighboring cells
    mx <- do.call(pmax, c(na.rm = T, m_list))
    #is the middle cell has the highest flow
    ismx <- which_matrix(m_list[[5]], mx)
    #find the shortest route for each cells
    rm_list <- get_neighbor_layers(rm[[1]])
    rmin <- do.call(pmin, c(na.rm = T, rm_list))
    #which neighbors index has the lowest route
    isrm_list <- lapply(rm_list, which_matrix, mx = rmin)
    #add with distance to the center cell based on the neighboring index lowest route
    distm <- matrix(NA, nrow(m), ncol(m))
    for (i in 1:9) {
      distm[isrm_list[[i]]] <-  ndist[i]
    }
    rmin <- rmin + distm
    #create the stars object
    ismx_st <- rm
    rmin_st <- rm
    rmx <- rm
    ismx_st[[1]] <- ismx
    rmin_st[[1]] <- rmin
    #add the distance
    rmin_st[is.na(ismx_st)] <- NA
    rmx[!is.na(rmin_st)] <- 0
    rmin_st[!is.na(rm)] <- 0
    rm <- rmx + rmin_st
    #check progress
    mat1 <- prev_rm[[1]]
    mat2 <- rm[[1]]
    mat1[is.na(mat1)] <- -1
    mat2[is.na(mat2)] <- -1
    isfull <- all(mat1 == mat2, na.rm = T)
    progress(prog)
  }
  return(rm)
}


test_m <- function() {
  flow <- read_stars("flow.tif")
  # flow_map <- flow[, 100:200, 100:200]
  flow_map <- flow
  start.time <- Sys.time()
  rm <- generate_routing_dist_map(flow_map)
  print(Sys.time() - start.time)
  plot(rm, breaks = "equal", col = terrain.colors(500))
  
  stream <- generate_stream(flow)
  colnames(stream[[1]]) <- "flow"
  plot(stream, lwd = 1, col = terrain.colors(500))
  
  strm <- rm[stream]/1000
  strm_sf <- generate_rounded_polygon(strm, 1)
  dval <- unique(strm_sf[[1]])
  plot(strm_sf, breaks = "equal", col = terrain.colors(500))
  

  stream <- st_rename_col(stream, 1, "flow")
  strm_sf <- st_rename_col(strm_sf, 1, "dist")
  
  a <- st_union(stream, strm_sf)
  
  

}

st_rename_col <- function(sf, idx, new_name) {
  cn <- colnames(sf)
  names(sf)[names(sf) == cn[idx]] = new_name
  return(sf)
}

which_matrix <- function(my, mx, nr = NULL, nc = NULL) {
  if (is.null(nr))
    nr <- nrow(my)
  if (is.null(nc))
    nc <- ncol(my)
  mx_arr <- which(mx == my)
  arrM <- c(rep(NA, nr * nc))
  arrM[mx_arr] <- T
  return(matrix(arrM, nrow = nr, ncol = nc))
}

get_neighbor_layers <- function(m) {
  m1 <- m |> shift_cell("right") |> shift_cell("down")
  m2 <- m |> shift_cell("right")
  m3 <- m |> shift_cell("right") |> shift_cell("up")
  m4 <- m |> shift_cell("down")
  m5 <- m
  m6 <- m |> shift_cell("up")
  m7 <- m |> shift_cell("left") |> shift_cell("down")
  m8 <- m |> shift_cell("left")
  m9 <- m |> shift_cell("left") |> shift_cell("up")
  return(list(m1, m2, m3, m4, m5, m6, m7, m8, m9))
}


shift_cell <- function(m, to, ncell = 1) {
  nr <- nrow(m)
  nc <- ncol(m)
  if (to == "down") {
    ms <- rbind(matrix(NA, nrow = ncell, ncol = nc), m[-c((nr - ncell + 1):nr), ])
  } else if (to == "up") {
    ms <- rbind(m[-c(1, ncell), ], matrix(NA, nrow = ncell, ncol = nc))
  } else if (to == "right") {
    ms <- cbind(matrix(NA, nrow = nr, ncol = ncell), m[, -c((nc - ncell + 1):nc)])
  } else if (to == "left") {
    ms <- cbind(m[, -c(1:ncell)], matrix(NA, nrow = nr, ncol = ncell))
  }
  return(ms)
}





get_neighbors = function(m, pos) {
  r <- nrow(m)
  c <- ncol(m)
  i = max(1, pos[1] - 1):min(r, pos[1] + 1)
  j = max(1, pos[2] - 1):min(c, pos[2] + 1)
  mn <- m[i, j]
  if (length(mn) == 9)
    return(mn)
  v2 <- c(NA, NA)
  v3 <- c(NA, NA, NA)
  if (pos[1] == 1) {
    if (pos[2] == 1) {
      mn <- cbind(v2, mn)
    } else if (pos[2] == c) {
      mn <- cbind(mn, v2)
    }
    mn <- rbind(v3, mn)
  } else if (pos[1] == r) {
    if (pos[2] == 1) {
      mn <- cbind(v2, mn)
    } else if (pos[2] == c) {
      mn <- cbind(mn, v2)
    }
    mn <- rbind(mn, v3)
  } else if (pos[2] == 1) {
    mn <- cbind(v3, mn)
  } else if (pos[2] == c) {
    mn <- cbind(mn, v3)
  }
  return(mn)
}

#' upscale the map m to match with the scale of map m_scale
#' and smoothing the edge border
#'
#' @param m target map
#' @param m_scale the scale reference map
#'
#' @return the processed map
#' @export
#'
#' @examples
rescale <- function(m, m_scale, progress = NULL, is_smooth_edge = T) {
  if(is.null(progress)) progress <- function(x, y){}
  d_scale <- st_dimensions(m_scale)$x$delta
  bb <- st_bbox(m)
  nprog <- 1
  if(is_smooth_edge) {
    div <- 3
    d <- st_dimensions(m)$x$delta
    n <- floor(log(d / d_scale, div))
    nprog <- n + 1
    for (x in 1:n) {
      progress(x / (nprog + 1), paste("Rescaling the map step", x, "of", nprog))
      d <- st_dimensions(m)$x$delta
      grid = st_as_stars(bb, dx = d / div)
      m = st_warp(m, grid)
      # write_stars(m, "soil1.tif")
      m[[1]] <- nfilter(m[[1]], median)
    }
  }
  progress(nprog / (nprog + 1),
           paste("Rescaling the map step", nprog, "of", nprog))
  grid <- st_as_stars(bb, dx = d_scale)
  m <- st_warp(m, grid)
  if(is_smooth_edge) {
    m[[1]] <- nfilter(m[[1]], median)
  }
  return(m)
}

#' low pas filter of matrix m with 3x3 matrix and function func
#' for all 3x3 neighboring values. egde and NA are ignored.
#'
#' @param m targeted matrix
#' @param func function
#'
#' @return the filtered matrix
#' @export
#'
#' @examples
nfilter <- function(m, func) {
  # soil <- read_stars("soil1.tif")
  # m <- soil[[1]] 
  
  m2 <- m
  
  # m2 <- matrix(NA, nr, nc)
  #TODO: iterate only on to those row/column that hasa more than one unique value
  #option: duplicate the matrix by 8, shift one cell to all direction, subtract by the main matrix
  #sum up all, get only the cell with value != 0
  
  nlist <- get_neighbor_layers(m)
  # print(nlist)
  #find the highest and lowest value for each neighboring cells
  max_m <- do.call(pmax, c(na.rm = T, nlist))
  min_m <- do.call(pmin, c(na.rm = T, nlist))
  #find the cells that are not homogeneous
  diffm <- min_m != max_m
  cell_list <- which(diffm, arr.ind = TRUE)
  for(i in 1:nrow(cell_list)) {
    r <- cell_list[i, "row"]
    c <- cell_list[i, "col"]
    vs <- get_neighbor_vals(nlist, r, c)
    # print(vs)
    m2[r,c] <- dominance_val(vs)
    
  }

  return(m2)
}

get_neighbor_vals <- function(nlist, r, c) {
  c(nlist[[1]][r, c], nlist[[2]][r, c], nlist[[3]][r, c], nlist[[4]][r, c],
    nlist[[5]][r, c], nlist[[6]][r, c], nlist[[7]][r, c], nlist[[8]][r, c], nlist[[9]][r, c]) 
}

dominance_val <- function(v) {
  vt <- as.data.frame(table(v))
  x <- which(vt[[2]] == max(vt[[2]]), arr.ind = TRUE)
  return(as.numeric(as.character(vt[x[1], 1])))
}

nfilter_old <- function(m, func) {
  nr <- nrow(m)
  nc <- ncol(m)
  m2 <- matrix(NA, nr, nc)
  #TODO: iterate only on to those row/column that hasa more than one unique value
  #option: duplicate the matrix by 8, shift one cell to all direction, subtract by the main matrix
  #sum up all, get only the cell with value != 0
  for (i in 1:nr) {
    for (j in 1:nc) {
      n <- get_neighbors(m, c(i, j))
      n <- as.vector(n[!is.na(n) & is.finite(n)])
      if (length(n) == 0)
        next
      m2[i, j] <- func(n)
    }
  }
  return(m2)
}



### map
map_factor_to_numeric <- function(map) {
  v <- unlist(map[[1]])
  nr <- nrow(map[[1]])
  map[[1]] <- matrix(as.numeric(levels(v))[v], nr)
  return(map)
}

reclassify_map <- function(map, fromto_df) {
  fromto_df <- fromto_df[order(fromto_df[, 1]), ]
  from <- fromto_df[[1]]
  to <- fromto_df[[2]]
  map2 <- cut(map, c(min(from) - 1, from), labels = to)
  map2 <- map_factor_to_numeric(map2)
  return(map2)
}


crop_raster <- function(raster_map,
                        raster_boundary,
                        extent_fill = NA) {
  sf_use_s2(FALSE)
  raster_map <- st_as_stars(raster_map)
  if (st_crs(raster_map) != st_crs(raster_boundary)) {
    suppressMessages(raster_map <- st_transform(raster_map, crs = st_crs(raster_boundary)))
  }
  dim_b <- st_dimensions(raster_boundary)
  dim_m <- st_dimensions(raster_map)
  if (is.na(dim_m$x$delta) ||
      is.na(dim_m$y$delta) ||
      dim_b$x$delta != dim_m$x$delta || dim_b$y$delta != dim_m$y$delta) {
    # set the same resolution of the input map as the boundary
    bb <- st_bbox(raster_map)
    grid <- st_as_stars(bb, dx = dim_b$x$delta, dy = dim_b$y$delta)
    suppressMessages({
      raster_map <- st_warp(raster_map, grid)
    })
  }
  # make sure input map is equal or larger than the boundary
  x_bound <- raster_boundary
  x_bound[[1]] <- extent_fill
  raster_map <- st_mosaic(x_bound, raster_map)
  # crop the by bounding box
  suppressMessages(raster_map2 <- st_crop(raster_map, st_bbox(raster_boundary)))
  # grid = st_as_stars(st_bbox(raster_boundary), dx = dim_b$x$delta, dy = dim_b$y$delta)
  grid <- raster_boundary
  grid[[1]] <- 0
  raster_map2 = st_warp(raster_map2, grid)
  raster_map2[is.na(raster_boundary)] <- NA
  return(raster_map2)
}


generate_routing_distance <- function(flow_map, threshold = 20) {
  flow_map <- st_as_stars(flow_map)
  cs <- cellSize.stars(flow_map)
  wres <- sqrt(cs)
  dmat <- matrix(ncell_dist_factor * wres, nrow = 3, ncol = 3)
  m <- flow_map * cs / 10000
  m[m < threshold] <- NA
  m <- m[[1]]
  md <- matrix(NA, nrow(m), ncol(m))
  mo <- matrix(NA, nrow(m), ncol(m))
  mt <- matrix(NA, nrow(m), ncol(m))
  md_last <- matrix(NA, nrow(m), ncol(m))
  #start from the highest flow cell
  o <- which(m == max(m, na.rm = T), arr.ind = TRUE)[1, ]
  dist <- 0
  #dataframe for the branches stream start point
  m2 <- NULL
  order <- 1
  trace <- F
  while (T) {
    #assign the distance to the distance map (md)
    md[o[1], o[2]] <- dist
    mo[o[1], o[2]] <- order
    last_dist <- c(o, dist)
    #remove the cell from the main map by assigning NA
    m[o[1], o[2]] <- NA
    n <- get_neighbors(m, o)
    #if no more neighbors, go to the next branches
    if (all(is.na(n)) && nrow(m2) > 0) {
      #record the upstream point
      md_last[last_dist[1], last_dist[2]] <- last_dist[3]
      while (nrow(m2) > 0 && all(is.na(n))) {
        # h <- which(m2[[3]] == max(m2[[3]], na.rm = T), arr.ind = TRUE)[1]
        h <- 1
        o <- c(m2[h, 1], m2[h, 2])
        order <- m2[h, 4] 
        m2 <- m2[-h, ]
        if (is.na(m[o[1], o[2]])) {
          next
        }
        n <- get_neighbors(m, o)
        if(all(is.na(n))) next
      }
      mt[o[1], o[2]] <- order
      nd <- get_neighbors(md, o)
      if(!all(is.na(nd))) {
        fd <- which(nd == min(nd, na.rm = T), arr.ind = TRUE)
        ridx <- o[1] + fd[1, 1] - 2
        cidx <- o[2] + fd[1, 2] - 2
        dist <- md[ridx, cidx] + dmat[fd[1, 1], fd[1, 2]]
      }
      next
    }

    if (all(is.na(n)) && nrow(m2) == 0) {
      if (all(is.na(m))) {
        break
      } else {
        #in case of undetected stream
        o <- which(m == max(m, na.rm = T), arr.ind = TRUE)[1, ]
        n <- get_neighbors(m, o)
        no <- get_neighbors(mo, o)
        # print("undetected")
        order <- 0
        if(!all(is.na(no))) {
          order <- min(no, na.rm = T) + 1
        }
        mt[o[1], o[2]] <- order

      }
    }
    f <- which(n == max(n, na.rm = T), arr.ind = TRUE)
    #remove the max value
    n[f[1, 1], f[1, 2]] <- NA
    
    o_next <- shift_idx(o, c(f[1,1], f[1,2]))
    while(is_contain_idx(m2, o_next) && !all(is.na(n))) {
      f <- which(n == max(n, na.rm = T), arr.ind = TRUE)
      o_next <- shift_idx(o, c(f[1,1], f[1,2]))
      n[f[1, 1], f[1, 2]] <- NA
    }
    #store the coordinates of for the next branches
    if (!all(is.na(n))) {
      f2 <- which(!is.na(n), arr.ind = T)
      for (i in 1:nrow(f2)) {
        ridx <- o[1] + f2[i, 1] - 2
        cidx <- o[2] + f2[i, 2] - 2
        if (is.null(m2) || nrow(m2) == 0) {
          m2 <- data.frame(
            row = ridx,
            col = cidx,
            val = m[ridx, cidx],
            order = order + 1
          )
        } else {
          check_df <- m2[m2$row == ridx & m2$col == cidx, ]
          if (nrow(check_df) == 0) {
            m2 <- rbind(m2, c(ridx, cidx, m[ridx, cidx], order + 1))
          }
        }
      }
    }
    #add distance from the next point
    if (is.na(md[o[1], o[2]])) {
      #if starting new point, get the sortest path
      nd <- get_neighbors(md, o)
      if(!all(is.na(nd))) {
        fd <- which(nd == min(nd, na.rm = T), arr.ind = TRUE)
        ridx <- o[1] + fd[1, 1] - 2
        cidx <- o[2] + fd[1, 2] - 2
        dist <- md[ridx, cidx] + dmat[fd[1, 1], fd[1, 2]]
      } else {
        dist <- NA
      }
    } else {
      dist <- md[o[1], o[2]] + dmat[f[1, 1], f[1, 2]]
    }
    #connect to the next largest neighbor
    o[1] <- o[1] + f[1, 1] - 2
    o[2] <- o[2] + f[1, 2] - 2
  }
  # routing <- flow_map
  # routing[[1]] <- md
  # orderm <- flow_map
  # orderm[[1]] <- mo
  # outlet <- flow_map
  # outlet[[1]] <- mt
  # upstream <- flow_map
  # upstream[[1]] <- md_last
  # rd_map <- c(flow_map, routing, orderm, outlet, upstream)
  # names(rd_map) <- c("flow", "routing", "order", "outlet", "upstream")
  # return(rd_map)
  
  flow_map[[2]] <- md
  flow_map[[3]] <- mo
  flow_map[[4]] <- mt
  flow_map[[5]] <- md_last
  names(flow_map) <- c("flow", "routing", "order", "outlet", "upstream")
  return(flow_map)
}

is_contain_idx <- function(df, cell) {
  if (is.null(df) || nrow(df) == 0) return(F) 
  check_df <- df[df$row == cell[1] & df$col == cell[2],]
  return(nrow(check_df) != 0) 
}

shift_idx <- function(o, s) {
  o[1] <- o[1] + s[1] - 2
  o[2] <- o[2] + s[2] - 2
  return(o)
}

generate_watershed <- function(direction_map, lon, lat, outlet_radius = 10) {
  #create outlet shape
  pt.df   <- data.frame(pt = 1, x = lon, y = lat)
  p   <- st_as_sf(pt.df, coords = c("x", "y"))
  st_crs(p) <- 4326
  p <- st_transform(p, crs = 7801)
  circle <-  st_buffer(p, outlet_radius)
  circle <- st_transform(circle, crs = 4326)
  #generate watershed
  ws <- watershed(direction_map, circle)
  m <- st_as_stars(ws)
  return(m)
}

generate_subcathments <- function(direction_map,
                                  routing_dist_map,
                                  order,
                                  min_sub_area = NULL,
                                  progress = NULL) {
  if (is.null(progress))
    progress <- function(x, y) {
    }
  if (class(direction_map) == "stars") {
    direction_map <- stars_to_terra(direction_map)
  }
  flow_map <- routing_dist_map["flow"]
  fm <- flow_map[routing_dist_map["outlet"] == order]
  routing_map <- routing_dist_map["routing"]
  dm <- routing_map[!is.na(fm)]
  df <- as.data.frame(c(fm, dm))
  colnames(df) <- c("out_lon", "out_lat", "flow", "routing")
  df <- df[!is.na(df[[3]]), ]
  df$area <- df[[3]] * cellSize.stars(flow_map)
  if (!is.null(min_sub_area)) {
    df <- df[df$area >= min_sub_area*10000, ]
  }
  #find the last outlet
  dfm <- st_dimensions(fm)
  last_o <- which(df$routing == max(df$routing, na.rm = T), arr.ind = TRUE) 
  row <- ceiling((df[last_o,"out_lon"]-dfm$x$offset)/dfm$x$delta)
  col <- ceiling((df[last_o,"out_lat"]-dfm$y$offset)/dfm$y$delta)
  nord <- get_neighbors(routing_dist_map["order"][[1]], c(row, col))
  nrou <- get_neighbors(routing_dist_map["routing"][[1]], c(row, col))
  nrou[nord != (order-1)] <- NA
  ni <- which(nrou == max(nrou, na.rm = T), arr.ind = T)
  
  ridx <- row + ni[1, 1] - 2
  cidx <- col + ni[1, 2] - 2
  rcoo <- dfm$x$offset + (ridx-0.5)*dfm$x$delta
  ccoo <- dfm$y$offset + (cidx-0.5)*dfm$y$delta
  area <- flow_map[[1]][ridx, cidx] * cellSize.stars(flow_map)
  last_outlet <- c(rcoo, ccoo, flow_map[[1]][ridx, cidx], routing_map[[1]][ridx, cidx], area)
  if (is.null(min_sub_area) ) {
    df <- rbind(df, last_outlet)
  } else {
    if(area >=  min_sub_area*10000) {
      df <- rbind(df, last_outlet)
    }
  }
  df <- df[order(df[[3]], decreasing = T), ]
  df$out_id <- c(1:nrow(df))
  df$distance <- NA
  m <- flow_map
  m[!is.na(m)] <- NA
  nsc <- nrow(df)
  for (i in 1:nsc) {
    progress(i / nsc, paste("Generating subcathment", i, "of", nsc))
    wm <- generate_watershed(direction_map, df[i, 1], df[i, 2])
    wm[!is.na(wm)] <- i
    m <- st_mosaic(m, wm)
    #calculate mean of subcatchment routing distance
    up_m <- routing_dist_map["upstream"]
    wm2 <- crop_raster(wm, up_m)
    up_m[is.na(wm2)] <- NA
    df[i, "distance"] <- mean(up_m[[1]], na.rm = T)
    if(is.na(df[i, "distance"])) {
      df[i, "distance"] <- get_map_value(routing_dist_map["routing"], df[i, "out_lon"], df[i, "out_lat"])
    }
  }
  ps <- st_as_sf(m,
                 as_points = F,
                 merge = T,
                 connect8 = T)
  names(ps) <- c("out_id", "geometry")
  # ps$area <- as.numeric(st_area(ps))/10000 #in ha
  ps$area_m2 <- st_area(ps) #in m2
  ps <- merge(ps, df[c("out_id", "out_lon", "out_lat", "distance")], by = "out_id")
  outlet_columns <- c("out_id",
                      "out_lon",
                      "out_lat",
                      "area_m2",
                      "distance")
  return(ps[outlet_columns])
}

get_map_value <- function(map, lon, lat) {
  pc <- st_sfc(st_point(c(lon, lat)))
  p <- st_as_sf(pc, crs = st_crs(map))
  v <- st_extract(map, p)
  return(v[[1]])
}

flowper <- function(df, vcol, group_col) {
  df$Qt <- df[[vcol]]
  df$Qt1 <- c(df[[vcol]][-1], NA) 
  df <- df[df$Qt > 0 & df$Qt1 > 0,]
  df$Qt_Qt1 <- df$Qt/df$Qt1
  # calculate thq quartile
  ds <- aggregate(df$Qt_Qt1, df[group_col], quantile, na.rm = T)
  ds <- cbind(ds[group_col], as.data.frame(ds[["x"]]))
  colnames(ds) <- c(group_col, "min", "quartile_1", "med", "quartile_3", "max")
  ds$range <- ds$quartile_3 - ds$quartile_1
  ds$above_limit <- ds$quartile_3 + ds$range * 1.5
  ds$below_limit <- ds$quartile_1 - ds$range * 1.5
  
  df <- merge(df, ds[c(group_col, "above_limit")], by = group_col, all.x = T)
  df$Qtc <- df$Qt
  df$Qtc1 <- df$Qt1
  df <- df[!is.na(df$Qt_Qt1), ] 
  df[df$Qt_Qt1 > df$above_limit, "Qtc"] <- NA
  df[df$Qt_Qt1 > df$above_limit, "Qtc1"] <- NA
  df$Qtc1_Qtc <- df$Qtc1/df$Qtc
  
  fp <- aggregate(df$Qtc1_Qtc, df[group_col], min, na.rm = T)
  colnames(fp) <- c(group_col, "flowper")
  ds <- merge(ds, fp, by = group_col)
  return(ds)
}





library(mapview)
library(stars)
library(terra)
library(flowdem)

tes <- function() {
  
  # setwd("D:/google_drive/ecomodels/data/genriver/sumberjaya/genriver_sbj")
  setwd("D:/google_drive/ecomodels/data/genriver/sumberjaya/pars/genriver_tes")
  dem_map <- read_stars("dem_bb.tif")
  dem_ws <- read_stars("dem_ws.tif")
  ws_boundary_sf <- st_read("ws_boundary.shp")
  ws_boundary_stars <- st_rasterize(ws_boundary_sf, dem_map, align = T)
  
  flow_map <- read_stars("dem_flow.tif")
  rd <- generate_routing_distance(flow_map)
  direction_map <- rast("dem_direction.tif")
  sf_use_s2(FALSE)
  sc <- generate_subcathments(direction_map, rd, 2)
  sc <- generate_subcathments(direction_map, rd, 2, min_sub_area = 200)

  m <- rd["order"]
  om <- rd["outlet"]
  om[om != 2] <- NA
  mapview(list(sc["distance"],m,om), col.regions = rainbow)
  
  rm <- rd["routing"]
  
  dem <- read_stars("dem_ws.tif")
  dem2 <- stars_to_terra(dem)
  dem2<- focal(dem2, w=25, fun = "mean", na.policy="omit", na.rm=T)
  # plot(dem2)
  dem2 <- st_as_stars(dem2)
  min <- min(dem2[[1]], na.rm = T)
  max <- max(dem2[[1]], na.rm = T)
  del_elv <- max - min
  nd <- 4
  d <- c(1:nd)^2/nd^2
  dem2 <- dem2 - min
  seg_m <- dem2
  seg_m[!is.na(seg_m)] <- 1
  for(i in 1:(nd-1)) {
    seg_m[dem2 >= del_elv * d[i]] <- i+1
  }
  plot(seg_m)
  
  slope <- terrain(stars_to_terra(dem))
  slope2 <- focal(slope, w=15, fun = "mean", na.policy="omit", na.rm=T)
  slope2 <- st_as_stars(slope2)
  min <- min(slope2[[1]], na.rm = T)
  max <- max(slope2[[1]], na.rm = T)
  del_elv <- max - min
  nd <- 4
  d <- min + del_elv * c(1:nd)^2/nd^2
  # slope2 <- slope2 - min
  slope_class <- slope2
  slope_class[!is.na(slope_class)] <- 1
  for(i in 1:(nd-1)) {
    slope_class[slope2 >= d[i]] <- i+1
  }
  plot(slope_class)
  
  fter <- ter
  for(i in 1:5) {
    fter <- focal(fter, w=15, fun = "mean", na.policy="omit", na.rm=T)
  }
  plot(fter)
  plot(dem)
  
  ### SOIL HYDRAULIC #############
  setwd("D:/google_drive/ecomodels/data/genriver/sumberjaya/genriver_sbj_5")
  sl_df <- read.csv("soil_layer.csv")
  lc_par_df <- read.csv("lc_props.csv")
  
  subc <- st_read("subcatchment.shp")
  lc_m <- read_stars("lcmapcrop_lc_1.tif")
  lc_m2 <- read_stars("lcmapcrop_lc_2.tif")
  
  subc_stars <- st_rasterize(subc["ws_id"], lc_m, align = T)
  subc_stars <- crop_raster(subc_stars, lc_m)
  
  qfc <- read_stars("soil_quick_flow_capacity_lc_1.tif")
  qfc <- crop_raster(qfc, lc_m)
  
  subc_arr <- as.vector(subc_stars[[1]])
  lc_m_arr <- as.vector(lc_m[[1]])
  qfc_arr <- as.vector(qfc[[1]])
  qfc_df <- data.frame(ws_id = subc_arr, lc_id = lc_m_arr, qfc = qfc_arr)
  qfc_df <- qfc_df[!is.na(qfc_df$lc_id), ]
  aggregate(qfc_df["qfc"], by = qfc_df[c("ws_id", "lc_id")], mean, na.rm = T)
    
  lc <- c(lc_m, lc_m2)
  ws <- subc["ws_id"]
  ssf <- ws[ws$ws_id == 2,]
  ssf1 <- ws[ws$ws_id == 1,]
  lcs <- lc[ssf]
  mean(lcs[[1]], na.rm = T)
  plot(lcs[2])
  plot(subc)
  
  qfc <- read_stars("qfc.tif")
  plot(qfc)
  qfc_sub <- qfc[ssf]
  qfc_sub <- qfc[ssf1]
  plot(qfc_sub)
  
  st_bbox(subc)

  dim <- st_dimensions(qfc)
  ssf_m <- st_rasterize(ssf, qfc, align = T)
  qfc_sub <- crop_raster(qfc, ssf_m)
  lc_sub <- crop_raster(lc_m, ssf_m)
  m <- c(lc_sub, qfc_sub)
  names(m) <- c("lc", "qfc")
  
  qfc2 <- crop_raster(qfc, lc_m)
  
  ws_stars <- st_rasterize(ws, dx = dim$x$delta, dy = dim$y$delta)
  ws_stars2 <- st_rasterize(ws, lc_m)

  qfc_sub2 <- qfc2[ws_stars == 2]
  lc_sub2 <- lc_m[ws_stars == 2]
  mm <- c(ws_stars, lc_m)
  a <- qfc2[ws_stars2 == 2 & lc_m == 6]
  
  # b <- qfc_sub2[lc_sub2 == 6]
  # plot(lc_sub2)
  # 
  # qfc_sub3 <- qfc2[ssf]
  # lc_sub3 <- lc_m[ssf]
  # c <- qfc_sub3[lc_sub3 == 6]
  qfc <- read_stars("soil_plant_available_water-lc_1.tif")
  
  ### CALCULATE #############
  setwd("D:/google_drive/ecomodels/data/genriver/sumberjaya/genriver_sbj_14")
  subc_lc_df <- read.csv("subcatchment_lc.csv")
  lc_map_df <- read.csv("map_list.csv")
  
  ###########################################
  ### TEST PLOTLY #########################
  setwd("D:/google_drive/ecomodels/data/genriver/sumberjaya/pars/genriver_pars_3")
  setwd("C:/Degi/GDrive_code/data/genriver/sumberjaya/pars/genriver_pars_3")
  dem_map <- read_stars("dem_bb.tif")
  sf <- st_read("ws_boundary.shp")
  subc_sf <- st_read("subcatchment.shp")
  
  
  ws_boundary_stars <- st_rasterize(st_sf(st_geometry(sf)), dem_map, align = T)
  dem_ws <- crop_raster(dem_map, ws_boundary_stars)
  # subc_sf <- subc_sf[order(subc_sf$ws_id),]
  subc_stars <- st_rasterize(subc_sf["ws_id"], dem_map, align = T)
  subc_info <- subc_stars
  subc_stars <- subc_stars + 1
  subc_stars[subc_stars == 1000] <- 0
  color <- as.data.frame(subc_sf)$color

  df <- as.data.frame(subc_sf)
  df$info <- paste0("[", df$ws_id,"] ", df$label, "<br>Area:", round(df$area), " ha")
  subc_info <- apply(subc_stars[[1]], c(1,2), function(x, d) {
    if(is.na(x)) {
      NA
    } else {
      d[d$ws_id == x, "info"]
    }
  }, df)
  
  df <- st_coordinates(m)
  x <- sort(unique(df$x))
  y <- sort(unique(df$y))
  
  plot_ly(
    colors = c("#034464", "#9a130e", color),
    showscale = F,
    hovertemplate =  "<b>Subcathment ID: %{text}</b><br>Elevation: %{z} m<extra></extra>"
  ) |>
    layout(scene = list(aspectratio = list(
      x = 1, y = 1, z = 0.2
    ))) |> add_surface(z = dem_ws[[1]],
                       surfacecolor = subc_stars[[1]],
                       text = t(subc_info[[1]]))
  

  
  m <- matrix(c(1,2,3,4,5,6,7,8,9),3,3)
  
  dd <- data.frame(a = c(2:4), b = "x")
  
  m2 <- apply(m, c(1,2), function(x, d){
    d[d$a == x, "b"]
  }, dd)
}



# dimension(s):
#   from  to offset      delta refsys x/y
# x    1 940  104.3  0.0002778 WGS 84 [x]
# y    1 862 -4.924 -0.0002778 WGS 84 [y]
# Simple feature collection with 117 features and 7 fields
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 104.3082 ymin: -5.16375 xmax: 104.5699 ymax: -4.929583


google_earth_engine <- function() {
  library(rgee)
  # ee_install(py_env = "rgee")
  # ee_Initialize()
  ee_Initialize(user = 'degi.ecomodels@gmail.com')
  srtm <- ee$Image("USGS/SRTMGL1_003")

}
