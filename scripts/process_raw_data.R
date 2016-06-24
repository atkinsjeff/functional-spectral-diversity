extract_plots <- function() {
  
  
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