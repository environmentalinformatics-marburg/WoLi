# Description:
# Author: Alice Ziegler
# Date: 2017-10-17 09:01:15

########################################################################################
###Documentation
########################################################################################

########################################################################################
########################################################################################

#'
points_query <- function( dat_path, r_pnts = 10, db_layers = c("kili", "kili2"), db = "http://192.168.191.183:8081",
                    db_login = "user:password", location){
  library(rPointDB)
  library(dplyr)
  remotesensing <- RemoteSensing$new(db, db_login)
  points_all_lay <- lapply(db_layers, function(i){
    pointdb <- remotesensing$lidar(i)
    points_lay <- lapply(location$plotID, function(j){
      extent <- extent_radius(x = location$x_pnt[j], y = location$y_pnt[j], r = r_pnts)
      points <- pointdb$query(ext = extent)
      if (nrow(points)!= 0){
        points$plotID <- location$plotID[j]
        points$layer <- i
      }
      return(points)
    })
    points_lay_bnd <- do.call(rbind, points_lay)
  })
  points_all <- do.call(rbind, points_all_lay)
  rm(points_all_lay)

  plt_min <- setNames(aggregate(z ~ plotID, points_all, min), c("plotID", "h_min"))
  pnts <- left_join(points_all, plt_min, by = "plotID")
  rm(points_all)
  pnts$h_rel <- pnts$z-pnts$h_min
  pnts$landuse <- substr(pnts$plotID, 1, 3)
  pnts <- pnts[,c(which(colnames(pnts) == "plotID"), which(colnames(pnts) == "landuse"),
                  which(colnames(pnts) == "x") : which(colnames(pnts) == "classificationFlags"),
                  which(colnames(pnts) == "layer") : which(colnames(pnts) == "h_rel"))]

  save(pnts, file = paste0(dat_path, "points_", r_pnts, "m.RData"))
  #write(paste0("r_pnts = ", r_pnts), file = paste0(dat_path, "points_", r_pnts, "m.txt"))


}
