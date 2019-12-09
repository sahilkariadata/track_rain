library(Matrix)
#rain_data[y,x,t] 
dt = 1/60 #hours
dx = 2 #km
dy = 2 # initializing all variables 
vx = 120 #max speed of clouds in km/h
vy = 0
tmin = 0
tmax = 1/4 #hours
tsteps = (tmax - tmin)/dt
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
Ax = I - Dx*(vx*dt/dx) #propogator in x
Ay = I - Dy*(vy*dt/dy) #propogator in y
A = Ax%*%Ay #Advection propogator

#ADVECTION
for (i in 1:tsteps) {
  u = A%*%u     #u(t+2) = A^2 * u(t) works for advecting further
}
#IMAGE PROCESSING
u = matrix(u, nrow=500, ncol=500, byrow = TRUE)
matr_to_img(u0)
matr_to_img(u)

