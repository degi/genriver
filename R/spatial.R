####################################
### FUNCTION LIBRARY OF GENRIVER ###
####################################






# generate_stream <- function(dem_flow, t = 100) {
#   if (is.null(dem_flow)) {
#     return()
#   }
#   ### Generate the stream map and classify based on the flow accumulation in log scale ###
#   m <- st_as_stars(dem_flow)
#   mx <- max(m[[1]], na.rm = T)
#   p <- list()
#   base = 10
#   vround = 1000
#   i <- round(log(t, base)) + 1
#   x1 <- t
#   x2 <- base ^ i
#   mr <- m
#   mr[m < t] <- NA
#   mr <- round(mr / vround) * vround
#   mr[mr == 0] <- t
#   ps <- st_as_sf(mr,
#                  as_points = F,
#                  merge = T,
#                  connect8 = T) |>
#     st_transform(crs = 4326)
#   ps <- st_sf(geometry = ps)
#   return(ps)
# }


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
#' @examples
# generate_routing_dist <- function(flow_map, progress = NULL) {
#   # sink("myfile.log", append=TRUE, split=TRUE)
#   m <- flow_map[[1]]
#   # if(final_row_idx < 0 || final_row_idx > nrow(m))
#   #   return(NULL)
#   # if(final_col_idx < 0 || final_col_idx > ncol(m))
#   #   return(NULL)
#   md <- matrix(NA, nrow(m), ncol(m))
#   o <- which(m == max(m, na.rm = T), arr.ind = TRUE)[1, ]
#   
#   # o <- c(final_row_idx, final_col_idx)
#   dist <- 0
#   m2 <- matrix(NA, 0, 3)
#   max2 <- 1
#   #loop until all m map cells replaced by NA
#   while (length(m[!is.na(m)]) > 0) {
#     vm <- m[o[1], o[2]]
#     #check whether the starting point is still exist more then second biggest neighbors
#     if (is.na(vm) || vm < max2) {
#       if (!is.na(vm))
#         m2 <- rbind(m2, c(o[1], o[2], m[o[1], o[2]]))
#       #TODO: m2 mungkin lebih efisient sebagai matrix peta juga, trus gak perlu di order, tinggal cari max positionnnya aja
#       m2 <- m2[order(m2[, 3], decreasing = T), ]
#       m2 <- validate_matrix(m2, 1, 3)
#       #otherwise get the starting point fron the second largest neighbors list (m2)
#       if (nrow(m2) > 0) {
#         o <- m2[1, c(1, 2)]
#       } else {
#         a <- which(!is.na(m), arr.ind = TRUE)
#         if (nrow(a) > 0) {
#           o <- a[1, c(1, 2)]
#         }
#       }
#       #make sure the starting point exist, otherwise remove from the list
#       while (is.na(m[o[1], o[2]])) {
#         m2 <- m2[-1, ]
#         m2 <- validate_matrix(m2, 1, 3)
#         if (nrow(m2) > 0) {
#           o <- m2[1, c(1, 2)]
#         } else {
#           a <- which(!is.na(m), arr.ind = TRUE)
#           if (nrow(a) > 0) {
#             o <- a[1, c(1, 2)]
#           }
#           break
#         }
#       }
#       m2 <- m2[-1, ]
#       m2 <- validate_matrix(m2, 1, 3)
#       if (nrow(m2) > 0) {
#         #Noted the next max neighbor flows
#         max2 <- m2[1, 3] #2nd max neighbor
#         #Make sure it exist otherwise remove from the list
#         while (is.na(m[m2[1, 1], m2[1, 2]])) {
#           m2 <- m2[-1, ]
#           m2 <- validate_matrix(m2, 1, 3)
#           if (nrow(m2) > 0) {
#             max2 <- m2[1, 3]
#           } else {
#             max2 <- 0
#             break
#           }
#         }
#       }
#       #Assign the distance to the neighbor shortest path
#       nd <- get_neighbors(md, o)
#       fd <- which(nd == min(nd, na.rm = T), arr.ind = TRUE)
#       ridx <- o[1] + fd[1, 1] - 2
#       cidx <- o[2] + fd[1, 2] - 2
#       dist <- md[ridx, cidx] + 1
#     }
#     #assign the distance to the distance map (md)
#     md[o[1], o[2]] <- dist
#     #remove the cell from the main map by assigning NA
#     m[o[1], o[2]] <- NA
#     n <- get_neighbors(m, o)
#     if (all(is.na(n)))
#       next
#     #find index of max neighbors
#     f <- which(n == max(n, na.rm = T), arr.ind = TRUE)
#     n[f[1, 1], f[1, 2]] <- 0
#     #find index of 2nd max neighbors
#     f2 <- which(n == max(n, na.rm = T), arr.ind = TRUE)
#     ridx <- o[1] + f2[1, 1] - 2
#     cidx <- o[2] + f2[1, 2] - 2
#     m2 <- rbind(m2, c(ridx, cidx, m[ridx, cidx]))
#     max2 <- max(max2, m[ridx, cidx])
#     #add distance from the next point
#     dist <- md[o[1], o[2]] + 1
#     #connect to the next largest neighbor
#     o[1] <- o[1] + f[1, 1] - 2
#     o[2] <- o[2] + f[1, 2] - 2
#     #if the next point has flow = 1 then find the closest path
#     next_f <- m[o[1], o[2]]
#     if (!is.na(next_f) && next_f == 1) {
#       nd <- get_neighbors(md, o)
#       fd <- which(nd == min(nd, na.rm = T), arr.ind = TRUE)
#       ridx <- o[1] + fd[1, 1] - 2
#       cidx <- o[2] + fd[1, 2] - 2
#       dist <- md[ridx, cidx] + 1
#     }
#   }
#   flow_map[[1]] <- md
#   return(flow_map)
# }

# validate_matrix <- function(m, nrow, ncol) {
#   if (!any(class(m) == "matrix")) {
#     m <- matrix(m, nrow, ncol)
#   }
#   return(m)
# }


generate_stream <- function(flow_map, threshold = 100) {
  if (is.null(flow_map)) {
    return()
  }
  m <- st_as_stars(flow_map)
  mr <- m
  mr[m < threshold] <- NA
  ps <- generate_rounded_polygon(mr, 1000, threshold)
  return(ps)
}

generate_rounded_polygon <- function(raster_map, rounding = 1000, minval = 0) {
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
  res <- st_res(flow_map)
  p <- st_sfc(st_point(c(0, 0)), st_point(c(res[1], 0)), crs = st_crs(flow_map))
  wres <- st_distance(p)[1, 2]
  dmat <- matrix(ncell_dist_factor * wres, nrow = 3, ncol = 3)
  #get the matrix from the map
  m <- flow_map[[1]]
  m[m < threshold] <- NA
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
    # if (nrow(f) == 0) {
    #   break
    # }
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
rescale <- function(m, m_scale, progress = NULL) {
  
  div = 3
  d <- st_dimensions(m)$x$delta
  d_scale <- st_dimensions(m_scale)$x$delta
  n <- floor(log(d / d_scale, div))
  bb <- st_bbox(m)
  nprog <- n + 1
  for (x in 1:n) {
    progress(x / (nprog + 1), paste("Rescaling the map step", x, "of", nprog))
    d <- st_dimensions(m)$x$delta
    grid = st_as_stars(bb, dx = d / div)
    m = st_warp(m, grid)
    # write_stars(m, "soil1.tif")
    m[[1]] <- nfilter(m[[1]], median)
  }
  progress(nprog / (nprog + 1),
           paste("Rescaling the map step", nprog, "of", nprog))
  grid = st_as_stars(bb, dx = d_scale)
  m = st_warp(m, grid)
  m[[1]] <- nfilter(m[[1]], median)
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