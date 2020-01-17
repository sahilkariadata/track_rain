library(Matrix)
#rain_data[y,x,t] 
dx = 2 #km
dy = 2 # initializing all variables
vx = 56 #multiples of 8 up to 120 
vy = 56

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
k = 2 #image-spaced advect (advection between k images)

tmin = 0
tmax = k/4 #hours
txsteps = (tmax - tmin)/dtx
tysteps = (tmax - tmin)/dty
u = matrix(as.numeric(rain_data[,,1]), ncol = 500)
u[u == "NaN"] = 0
u0 = u

#CREATES MY DERIVATIVE MATRICES
u = Matrix(u, sparse = TRUE)
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
matr_to_img(u0)
matr_to_img(u)


