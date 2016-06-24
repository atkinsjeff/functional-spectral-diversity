library(raster)
library(rhdf5)
library(rgdal)
library(neonAOP)
library(ggplot2)
library(dplyr)

# input: plot centroid locations and E and N, and plot side length
# return: plot extent as a spatial polygon
get_plot_extent <- function(plot_centroid_E, plot_centroid_N, plot_side_length) {
  # plot_side length is half of the total side
  
  # create extents as a polygon
  xMin <- plot_centroid_E - plot_side_length
  xMax <- plot_centroid_E + plot_side_length
  yMin <- plot_centroid_N - plot_side_length
  yMax <- plot_centroid_N + plot_side_length
  
  plt_ext <- as(extent(c(xMin, xMax, yMin, yMax)), "SpatialPolygons")
  crs(plt_ext) <- CRS("+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  plt_ext
}

get_h5_filename <-function(plt_ext, drivePath){
  h5_filename <- extract_plt_filename(plt_ext, drivePath)

  f <- h5_filename[1]
}

clip_by_chm <- function(all_h_data, chm, chm_lim, plt_ext) {
  # extract CHM within plot
  plt_chm <- crop(chm, extent(plt_ext))
  plt_chm[plt_chm<chm_lim] <- NA
  
  # plot(plt_chm)
  # use chm to mask out 
  # mask stack
  clip_h_data <- mask(all_h_data, plt_chm)
}

get_good_bands <- function(filename) {
  # pick bands
  # get useful wavelengths from sarah's file
  good_bands_file <- read.csv(filename, stringsAsFactors=FALSE)
  bands_filterd <- good_bands_file[good_bands_file$noise==0, ]
}

extract_hyper_stack <- function(f, plt_ext, chm, chm_lim, bands_filterd) {
  # get wavelengths and extent from h5 file
  h5_ext <- create_extent(f)
  h5_ext_poly <- as(extent(h5_ext), "SpatialPolygons")
  crs(h5_ext_poly) <- crs(plt_ext)
  
  # get r and c number from E and N extents
  index_bounds <- calculate_index_extent(extent(plt_ext), h5_ext)
  
  all_h_data <- create_stack(f, bands = bands_filterd$band, 
                             epsg = epsg, subset = TRUE, dims = index_bounds)
  
  clip_h_data <- clip_by_chm(all_h_data, chm, chm_lim, plt_ext)
}

average_plot_spectra <- function(h_data, bands_filterd) {
  # get spectra for each band
  spectra <- extract_av_refl(h_data, 
                             aFun = mean)
  spectra <- as.data.frame(spectra)
  
  spectra$wavelength <- bands_filterd$nanometer
  
  spectra
}


same_extent <- function(ras1, ras2) {
  if (extent(ras1) == extent(ras2)) {
    print("extents match")
  } else {
    print("extents are different, cropping data")
    overlap <- raster::intersect(extent(ras1), extent(ras2))
    print(overlap)
    
    # crop lidar data
    ras1 <- crop(ras1, overlap)
    ras2 <- crop(ras2, overlap)
  }
  # creat stack to return
  ras_stack <- stack(ras1, ras2)
}