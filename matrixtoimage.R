matrixtoimg <- function(new) {
  library(spatstat)
  new = as.matrix(new)
  
  
  bl = readRDS("templatenan.rds") #keeps the template
  new[new == "NaN"] <- 0
  new[bl] <- "#C7BFC180"
  new[new == "NaN"] <- "#C7BFC180"
  
  # new[new == 0] <- "#00000000"
  # new[new == 0.01] <- "#0000FEFF"
  # new[new == 0.5] <- "#3265FEFF"
  # new[new == 1] <- "#7F7F00FF"
  # new[new == 2] <- "#FECB00FF"
  # new[new == 4] <- "#FE9800FF"
  # new[new == 8] <- "#FE0000FF"
  # new[new == 16] <- "#FE00FEFF"
  # new[new == 32] <- "#E5FEFEFF"
  
  new[new<0.01] <- "#00000000"
  new[0.01<=new & new<0.5] <- "#0000FEFF"
  new[0.5<=new & new<1] <- "#3265FEFF"
  new[1<=new & new<2] <- "#7F7F00FF"
  new[2<=new & new<4] <- "#FECB00FF"
  new[4<=new & new<8] <- "#FE9800FF"
  new[8<=new & new<16] <- "#FE0000FF"
  new[16<=new & new<32] <- "#FE00FEFF"
  new[32<=new] <- "#E5FEFEFF"
  new[bl] <- "#C7BFC180"
  l = ncol(new)
  new = new[l:1,]
  plot(im(new))
}

matr_to_img <- function(new) {
  library(spatstat)
  new = as.matrix(new)
  new[new<0.01] <- "#00000000"
  new[0.01<=new & new<0.5] <- "#0000FEFF"
  new[0.5<=new & new<1] <- "#3265FEFF"
  new[1<=new & new<2] <- "#7F7F00FF"
  new[2<=new & new<4] <- "#FECB00FF"
  new[4<=new & new<8] <- "#FE9800FF"
  new[8<=new & new<16] <- "#FE0000FF"
  new[16<=new & new<32] <- "#FE00FEFF"
  new[32<=new] <- "#E5FEFEFF"
  l = ncol(new)
  new = new[l:1,]
  plot(im(new))
}
  