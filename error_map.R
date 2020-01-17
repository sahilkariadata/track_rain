library('Matrix')
library(spatstat)
#x1 = u
#x0 = u0
x1 = matrix(as.numeric(rain_data[,,2]), ncol = 500, nrow = 500)
x0 = matrix(as.numeric(rain_data[,,1]), ncol = 500, nrow = 500)
upper_bx = 410
lower_bx = 361
upper_by = 300
lower_by = 251
dx = 2 #km
dy = 2 # initializing all variables
v_max = 120 #max speed of clouds in km/h (relevant in prediction)
k = 1 #image-spaced advect (advection between k images)
t_dist = k/4
maxsteps_x = t_dist*v_max/dx
maxsteps_y = t_dist*v_max/dy

errormap = matrix(0, ncol = 2*maxsteps_x + 1, nrow = 2*maxsteps_y + 1)
for (i in -maxsteps_x:maxsteps_x) {
  for (j in -maxsteps_y:maxsteps_y) {
    errormap[j+maxsteps_y+1,i+maxsteps_x+1] = ex_search(c(i,j))
  }
}
xaxis = seq(-v_max,v_max,by=dx/t_dist)
yaxis = seq(-v_max,v_max,by=dy/t_dist)
plot(im(errormap), main ="Squared error for different x and y velocities", xlab = "Vx (km/h)", ylab = "Vy (km/h)")
axis(1, at = 1:(2*maxsteps_x+1), labels = xaxis)
axis(2, at = 1:(2*maxsteps_y+1), labels = yaxis)

