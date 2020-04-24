library('Matrix')
library(spatstat)
library(imager)
library(rsvg)

img_to_matr = function(directory) {
  image_list <- list.files(path = directory, pattern = "*.png", full.names = TRUE) #loads images
  l <- length(image_list)
  rain_data <- array(rep(NaN, 500*500*l), c(500, 500, l)) #initializing
  for (i in 1:l) {
    im <- load.image(image_list[i])
    y <- rgb(im[,,1], im[,,2], im[,,3], alpha = im[,,4])
    rain_data[,,i] <- matrix(unlist(y), ncol = 500, byrow = TRUE)
  }
  # #C7BFC180 = grey, #00000000 = white, #0000FEFF = dark blue, #3265FEFF = blue, #7F7F00FF = green, #FECB00FF = yellow, #FE9800FF = orange, #FE0000FF = red, #FE00FEFF = purple, #E5FEFEFF = v.light blue
  rain_data[rain_data == "#C7BFC180"] <- 0
  rain_data[rain_data == "#00000000"] <- 0
  rain_data[rain_data == "#0000FEFF"] <- 0.255
  rain_data[rain_data == "#3265FEFF"] <- 0.75
  rain_data[rain_data == "#128DDFFF"] <- 0.75 #pixel colour used for compatibility with latex graphicx
  rain_data[rain_data == "#7F7F00FF"] <- 1.5
  rain_data[rain_data == "#FECB00FF"] <- 3
  rain_data[rain_data == "#FE9800FF"] <- 6
  rain_data[rain_data == "#FE0000FF"] <- 12
  rain_data[rain_data == "#FE00FEFF"] <- 24
  rain_data[rain_data == "#E5FEFEFF"] <- 48
  return(rain_data)
}

ex_search = function(steps,upper_bx,lower_bx,upper_by,lower_by,x0,x1) {
  x1 = Matrix(as.vector((x1[lower_by:upper_by,lower_bx:upper_bx])),sparse = TRUE)
  x0 = Matrix(as.vector((x0[(lower_by-steps[2]):(upper_by-steps[2]),(lower_bx-steps[1]):(upper_bx-steps[1])])),sparse = TRUE)
  S = as.numeric(crossprod(x1 - x0))
  return(S)
}

err_map <- function(x0,x1,xbounds,ybounds,k,cond) {
  
   if (cond == TRUE) {
     x1 = log2(x1) #so all pixels have a fairer weight
     x0 = log2(x0)
     x1[x1 == "-Inf"] <- -7  #computing -infinity is difficult so I compute -7 at the zeros instead
     x0[x0 == "-Inf"] <- -7
   }

  upper_bx = xbounds[2]
  lower_bx = xbounds[1]
  upper_by = ybounds[2]
  lower_by = ybounds[1]
  dx = 3.774 #km
  dy = 2.886 # initializing all variables
  v_max = 140 #max speed of clouds in km/h (relevant in prediction)
  t_dist = k/4   #image-spaced advect (advection between k images(15 min intervals))
  maxsteps_x = floor(t_dist*v_max/dx)
  maxsteps_y = floor(t_dist*v_max/dy)
  xaxis = seq(-maxsteps_x*dx/t_dist,maxsteps_x*dx/t_dist,by=dx/t_dist)
  yaxis = seq(-maxsteps_y*dy/t_dist,+maxsteps_y*dy/t_dist,by=dy/t_dist)
  
  errormap = matrix(0, ncol = 2*maxsteps_x + 1, nrow = 2*maxsteps_y + 1)
  for (i in -maxsteps_x:maxsteps_x) {
    for (j in -maxsteps_y:maxsteps_y) {
      errormap[j+maxsteps_y+1,i+maxsteps_x+1] = ex_search(c(i,j),upper_bx,lower_bx,upper_by,lower_by,x0,x1)
    }
  }
  return(list(map = errormap, xaxis = xaxis, yaxis = yaxis))
}

matrixtoimg <- function(new) {
  library(spatstat)
  new = as.matrix(new)
  
  
  bl = readRDS("templatenan.rds") #keeps the template
  new[new == "NaN"] <- 0
  new[bl] <- "#C7BFC180"
  new[new == "NaN"] <- "#C7BFC180"
  
  new[new<0.01] <- "#00000000"
  new[0.01<=new & new<0.5] <- "#0000FEFF"
  new[0.5<=new & new<1] <- "#128DDFFF"
  new[1<=new & new<2] <- "#7F7F00FF"
  new[2<=new & new<4] <- "#FECB00FF"
  new[4<=new & new<8] <- "#FE9800FF"
  new[8<=new & new<16] <- "#FE0000FF"
  new[16<=new & new<32] <- "#FE00FEFF"
  new[32<=new] <- "#E5FEFEFF"
  new[bl] <- "#C7BFC180"
  l = ncol(new)
  new = new[l:1,]
  plot(im(new),main="")
}

matr_to_img <- function(x) {
  new = as.matrix(x)
  new[new == "NaN"] <- 0
  new[x<0.01]<- "#00000000"
  new[0.01<=x & x<0.5] <- "#0000FEFF"
  new[0.5<=x & x<1] <- "#128DDFFF" #"#3265FEFF" this colour confuses latex?
  new[1<=x & x<2] <- "#7F7F00FF"
  new[2<=x & x<4] <- "#FECB00FF"
  new[4<=x & x<8] <- "#FE9800FF"
  new[8<=x & x<16] <- "#FE0000FF"
  new[16<=x & x<32] <- "#FE00FEFF"
  new[32<=x] <- "#E5FEFEFF"
  l = ncol(new)
  new = new[l:1,]
  plot(im(new),main="")
}

cutoutdisp <- function(x,xbounds,ybounds) {
  library(spatstat)
  new = as.matrix(x)
  new[new == "NaN"] <- 0
  new[x<0.01]<- "#00000000"
  new[0.01<=x & x<0.5] <- "#0000FEFF"
  new[0.5<=x & x<1] <- "#128DDFFF" #"#3265FEFF" this colour confuses latex?
  new[1<=x & x<2] <- "#7F7F00FF"
  new[2<=x & x<4] <- "#FECB00FF"
  new[4<=x & x<8] <- "#FE9800FF"
  new[8<=x & x<16] <- "#FE0000FF"
  new[16<=x & x<32] <- "#FE00FEFF"
  new[32<=x] <- "#E5FEFEFF"
  
  new[(ybounds[1]-2):(ybounds[1]-1),(xbounds[1]-2):(xbounds[2]+2)] = "#FF0000FF" #bottom
  new[(ybounds[2]+1):(ybounds[2]+2),(xbounds[1]-2):(xbounds[2]+2)] = "#FF0000FF" #top
  new[ybounds[1]:ybounds[2],(xbounds[1]-2):(xbounds[1]-1)] = "#FF0000FF" #left
  new[ybounds[1]:ybounds[2],(xbounds[2]+1):(xbounds[2]+2)] = "#FF0000FF" #right
  
  l = ncol(new)
  new = new[l:1,]
  plot(im(new),main="")
  img = rsvg("ukmap.svg")
  rasterImage(img,0,0,500,500)
}

mtisto = function(x) {
  new = as.matrix(x)
  new[new == "NaN"] <- 0
  new[x<0.01]<- "#00000000"
  new[0.01<=x & x<0.5] <- "#0000FEFF"
  new[0.5<=x & x<1] <- "#128DDFFF" #"#3265FEFF" this colour confuses latex?
  new[1<=x & x<2] <- "#7F7F00FF"
  new[2<=x & x<4] <- "#FECB00FF"
  new[4<=x & x<8] <- "#FE9800FF"
  new[8<=x & x<16] <- "#FE0000FF"
  new[16<=x & x<32] <- "#FE00FEFF"
  new[32<=x] <- "#E5FEFEFF"
  l = ncol(new)
  new = new[l:1,]
  return(new)
}

project <- function(x,k,vx,vy) {
  
  #### PARAMETERS ####
  #x = radar image in the form of a matrix
  #k = no of image-spaced advection (advection between k images)
  #vx = Velocity along x-axis
  #vy = Velocity along y-axis
  
  dx = 3.774 #km
  dy = 2.886 # initializing all variables
  
  if (vx != 0) {
    dtx = dx/abs(vx)
  } else {
    dtx = 0
  } # hours (dt decided based on vx and dx to keep C = 1)
  if (vy != 0) {
    dty = dy/abs(vy)
  } else {
    dty = 0
  } # hours (dt decided based on vx and dx to keep C = 1)
  
  tmin = 0
  tmax = k/4 #hours
  txsteps = (tmax - tmin)/dtx
  tysteps = (tmax - tmin)/dty
  
  #CREATES MY DERIVATIVE MATRICES
  u = Matrix(x, sparse = TRUE)
  I = Diagonal(500)
  D = bandSparse(n=500, k=c(-1,0), diagonals=list(rep(-1,499), rep(1,500)))
  if (vx < 0) {
    Dx = kronecker(I,-1*t(D)) #decides form of forward upwind in x-direction based on vx
  } else {
    Dx = kronecker(I,D)
  }
  if (vy < 0) {
    Dy = kronecker(-1*t(D),I) #decides form of forward upwind in y-direction based on vy
  } else {
    Dy = kronecker(D,I)
  }
  
  u = Matrix(as.vector(t(u)), sparse = TRUE)  #turn u into a column vector in order of rows
  I = Diagonal(250000) 
  Ax = I - Dx*(vx*dtx/dx) #propogator in x
  Ay = I - Dy*(vy*dty/dy) #propogator in y
  # Axx = Reduce(`%*%`, rep(list(Ax), txsteps)) #faster for larger no. of iterations
  # Ayy = Reduce(`%*%`, rep(list(Ay), tysteps))
  Axx = Diagonal(250000)
  Ayy = Diagonal(250000)
  if (vx != 0) {
    for (i in 1:txsteps) {
      Axx = Axx%*%Ax
    }
  }
  if (vy != 0) {
    for (i in 1:tysteps) {
      Ayy = Ayy%*%Ay
    }
  }
  A = Axx%*%Ayy #Advection propogator
  
  #ADVECTION
  u = A%*%u
  
  #IMAGE PROCESSING
  u = matrix(u, nrow=500, ncol=500, byrow = TRUE)
  return(u)
}
  
project_w_noise <- function(x,k,vx,vy,vari) {
  
  dx = 3.774 #km
  dy = 2.886 # initializing all variables
  
  if (vx != 0) {
    dtx = dx/abs(vx)
  } else {
    dtx = 0
  } # hours (dt decided based on vx and dx to keep C = 1)
  if (vy != 0) {
    dty = dy/abs(vy)
  } else {
    dty = 0
  } # hours (dt decided based on vx and dx to keep C = 1)
  
  v_max = 120 #max speed of clouds in km/h (relevant in prediction)
  #k = 1 #image-spaced advect (advection between k images)
  
  tmin = 0
  tmax = k/4 #hours
  txsteps = (tmax - tmin)/dtx
  tysteps = (tmax - tmin)/dty
  
  #CREATES MY DERIVATIVE MATRICES
  u = Matrix(x, sparse = TRUE)
  I = Diagonal(500)
  D = bandSparse(n=500, k=c(-1,0), diagonals=list(rep(-1,499), rep(1,500)))
  if (vx < 0) {
    Dx = kronecker(I,-1*t(D)) #decides form of forward upwind in x-direction based on vx
  } else {
    Dx = kronecker(I,D)
  }
  if (vy < 0) {
    Dy = kronecker(-1*t(D),I) #decides form of forward upwind in y-direction based on vy
  } else {
    Dy = kronecker(D,I)
  }
  
  u = Matrix(as.vector(t(u)), sparse = TRUE)  #turn u into a column vector in order of rows
  I = Diagonal(250000) 
  Ax = I - Dx*(vx*dtx/dx) #propogator in x
  Ay = I - Dy*(vy*dty/dy) #propogator in y
  # Axx = Reduce(`%*%`, rep(list(Ax), txsteps)) #faster for larger no. of iterations
  # Ayy = Reduce(`%*%`, rep(list(Ay), tysteps))
  Axx = Diagonal(250000)
  Ayy = Diagonal(250000)
  if (vx != 0) {
    for (i in 1:floor(txsteps)) {
      Axx = Axx%*%Ax
    }
  }
  if (vy != 0) {
    for (i in 1:floor(tysteps)) {
      Ayy = Ayy%*%Ay
    }
  }
  A = Axx%*%Ayy #Advection propogator
  
  #ADVECTION
  u = A%*%u
  
  #ADDITION OF NOISE
  E = rnorm(250000,mean=0,sd=sqrt(vari)) #IID noise
  u = log2(u) 
  u = matrix(u)
  u[u == "-Inf"] <- -7
  u = u + k*E
  u = 2^(u)
  #IMAGE PROCESSING
  u = matrix(u, nrow=500, ncol=500, byrow = TRUE)
  u[u<0] = 0
  return(u)
}  
  
se <-function(a,di) {
  # Used in bayesian inference to find grid location from unique vector location
  tauloc = floor((a-1)/(di[1]*di[2])) + 1
  r = mod(a-1,di[1]*di[2])
  vxloc = floor((r)/di[1]) + 1
  vyloc = mod(r,di[1]) + 1
  return(c(vxloc,vyloc,tauloc))
}

logscore <- function(pred,actual) {
  predsds = apply(pred,1,sd)
  predmeans = rowMeans(pred) #compares a disribution of mappings with a point estimate mapping to get the log score
  logsc = dnorm(actual,mean = predmeans, sd = predsds, log = TRUE)
  return(sum(logsc))
}

err_score <- function(pred,actual) {
  #pred = reassign(pred)
  #actual = reassign(actual)
  pred = as.vector(pred)
  actual = as.vector(actual)
  return(sum(crossprod(pred-actual)))
}

lse <- function(x0,x1,xbounds,ybounds,k,cond) {
  #least squares estimation
  e = err_map(x0,x1,xbounds,ybounds,k,cond)
  loc = which(e$map == min(e$map), arr.ind=TRUE) #finds location of minimum in heat map
  vx = e$xaxis[loc[2]]
  vy = e$yaxis[loc[1]]
  variance = e$map[loc]/((ybounds[2]-ybounds[1]+1)*(xbounds[2]-xbounds[1]+1))
  return(c(vx,vy,variance))
}

llgrid <- function(x1,x0,k,xbounds,ybounds,taus) {
  #retrieves likelihood function/grid
  v_max = 140
  errormap =  err_map(x0,x1,xbounds,ybounds,k,FALSE)
  d = dim(errormap$map)
  L = array(rep(NaN, d[1]*d[2]*length(taus)), c(d[1], d[2], length(taus))) #initializing
  for (i in 1:length(taus)) {
    #L[,,i] = -((d[1]*d[2])/2)*log(2*pi*exp(taus[i])) - (exp(-taus[i])/2)*errormap$map #log-likelihood function
    L[,,i] = -(((ybounds[2]-ybounds[1]+1)*(xbounds[2]-xbounds[1]+1))/2)*log(2*pi*exp(taus[i])) - (exp(-taus[i])/2)*errormap$map 
  }
  return(list(Llmatrix = L, vxs = errormap$xaxis, vys = errormap$yaxis))
}

bayesinf = function(x1,x0,k,xbounds,ybounds,num,taus) {
  out = llgrid(x1,x0,k,xbounds,ybounds,taus)
  L = out$Llmatrix
  llvector = as.vector(L) #changes y then x then z
  uniqueno = seq(1,length(llvector),1)
  llvector = llvector - max(llvector)
  llvector = exp(llvector)
  a = sample(uniqueno,num,prob=llvector, replace = TRUE)
  di = dim(L)
  total = ((ybounds[2]-ybounds[1]+1)*(xbounds[2]-xbounds[1]+1))
  
  paras = matrix(0, nrow=num,ncol=3)
  for (i in 1:num) {
    loc = se(a[i],di) #turns uniqueno into grid locations in llmatrix
    vxloc = loc[1]
    vyloc = loc[2]
    tauloc = loc[3]
    
    vari = exp(taus[tauloc])
    paras[i,] = c(out$vxs[vxloc],out$vys[vyloc],vari)
  }
  colnames(paras) <- c("Vx","Vy","Variance")
  return(paras)
}

project_w_scnoise <- function(x,k,vx,vy,vari) {
  
  dx = 3.774 #km
  dy = 2.886 # initializing all variables
  
  if (vx != 0) {
    dtx = dx/abs(vx)
  } else {
    dtx = 0
  } # hours (dt decided based on vx and dx to keep C = 1)
  if (vy != 0) {
    dty = dy/abs(vy)
  } else {
    dty = 0
  } # hours (dt decided based on vx and dx to keep C = 1)
  
  v_max = 120 #max speed of clouds in km/h (relevant in prediction)
  #k = 1 #image-spaced advect (advection between k images)
  
  tmin = 0
  tmax = k/4 #hours
  txsteps = (tmax - tmin)/dtx
  tysteps = (tmax - tmin)/dty
  
  #CREATES MY DERIVATIVE MATRICES
  u = Matrix(x, sparse = TRUE)
  I = Diagonal(500)
  D = bandSparse(n=500, k=c(-1,0), diagonals=list(rep(-1,499), rep(1,500)))
  if (vx < 0) {
    Dx = kronecker(I,-1*t(D)) #decides form of forward upwind in x-direction based on vx
  } else {
    Dx = kronecker(I,D)
  }
  if (vy < 0) {
    Dy = kronecker(-1*t(D),I) #decides form of forward upwind in y-direction based on vy
  } else {
    Dy = kronecker(D,I)
  }
  
  u = Matrix(as.vector(t(u)), sparse = TRUE)  #turn u into a column vector in order of rows
  I = Diagonal(250000) 
  Ax = I - Dx*(vx*dtx/dx) #propogator in x
  Ay = I - Dy*(vy*dty/dy) #propogator in y
  # Axx = Reduce(`%*%`, rep(list(Ax), txsteps)) #faster for larger no. of iterations
  # Ayy = Reduce(`%*%`, rep(list(Ay), tysteps))
  Axx = Diagonal(250000)
  Ayy = Diagonal(250000)
  if (vx != 0) {
    for (i in 1:floor(txsteps)) {
      Axx = Axx%*%Ax
    }
  }
  if (vy != 0) {
    for (i in 1:floor(tysteps)) {
      Ayy = Ayy%*%Ay
    }
  }
  A = Axx%*%Ayy #Advection propogator
  
  #ADVECTION
  u = A%*%u
  
  #ADDITION OF NOISE
  E = rnorm(250000,mean=0,sd=sqrt(vari)) #IID noise
  I = Diagonal(500)
  C = bandSparse(n=500, k=c(-1,1), diagonals=list(rep(1,499), rep(1,499)))
  C2 = bandSparse(n=250000, k=c(-500,500), diagonals=list(rep(1,249999), rep(1,249999)))
  initD = kronecker(I,C)
  
  D = initD + C2
  I2 = Diagonal(250000)
  D = I2 - (1/4)*D
  E = solve(D,E)
  
  
  u = u + k*E
  
  #IMAGE PROCESSING
  u = matrix(u, nrow=500, ncol=500, byrow = TRUE)
  u[u<0] = 0
  return(u)
}

ppm = function(paras, x1, xbounds, ybounds, k) {
  m = length(paras[,1])
  ppmvec = matrix(0,nrow = (xbounds[2]-xbounds[1]+1)*(ybounds[2]-ybounds[1]+1))
  for (i in 1:m) {
    x2 = project(x1,k,paras[i,1],paras[i,2])
    ppmvec = ppmvec + as.vector(x2[ybounds[1]:ybounds[2],xbounds[1]:xbounds[2]])
  }
  ppmvec = ppmvec/m
  ppmvec = matrix(ppmvec, ncol = (xbounds[2]-xbounds[1]+1), nrow = (ybounds[2]-ybounds[1]+1) )
  return(ppmvec)
}









  