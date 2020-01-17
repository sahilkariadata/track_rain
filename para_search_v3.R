library('Matrix')
library(NMOF)
# x1 = u
# x0 = u0
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

ex_search = function(steps) {
  x1 = Matrix(as.vector((x1[lower_by:upper_by,lower_bx:upper_bx])),sparse = TRUE)
  x0 = Matrix(as.vector((x0[(lower_by-steps[2]):(upper_by-steps[2]),(lower_bx-steps[1]):(upper_bx-steps[1])])),sparse = TRUE)
  S = as.numeric(crossprod(x1 - x0))
  return(S)
}
res = gridSearch(ex_search,levels = list(a = -maxsteps_x:maxsteps_x, b = -maxsteps_y:maxsteps_y))
vx = dx*res$minlevels[1]/t_dist
vy = dy*res$minlevels[2]/t_dist
print(vx)
print(vy)


