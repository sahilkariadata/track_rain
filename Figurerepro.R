rain_data = img_to_matr("data/exampledata")
x0 = matrix(as.numeric(rain_data[,,1]), ncol = 500, nrow = 500)
x1 = matrix(as.numeric(rain_data[,,2]), ncol = 500, nrow = 500)
x2 = matrix(as.numeric(rain_data[,,3]), ncol = 500, nrow = 500)
x3 = matrix(as.numeric(rain_data[,,4]), ncol = 500, nrow = 500)

###### Figure 4.1 #######
xbounds = c(361,410)
ybounds = c(251,300)
k = 1
e1 = err_map(x0,x1,xbounds,ybounds,k,FALSE)
e2 = err_map(x0,x1,xbounds,ybounds,k,TRUE)

plot(im(e1$map), main ="")
axis(1, at = 1:(length(e1$xaxis)), labels = e1$xaxis, pos=0.45)
axis(2, at = 1:(length(e1$yaxis)), labels = e1$yaxis, pos=0.45)
title( xlab = "Vx (km/h)", ylab = "Vy (km/h)",line = 1.5)

plot(im(e2$map), main ="")
axis(1, at = 1:(length(e2$xaxis)), labels = e2$xaxis, pos=0.45)
axis(2, at = 1:(length(e2$yaxis)), labels = e2$yaxis, pos=0.45)
title( xlab = "Vx (km/h)", ylab = "Vy (km/h)",line = 1.5)

##### Table in Section 4.1 #####
for (i in 1:6) {
  xbef = matrix(as.numeric(rain_data[,,i]), ncol = 500, nrow = 500)
  xcheck = matrix(as.numeric(rain_data[,,i+1]), ncol = 500, nrow = 500)
  v = lse(xbef,xcheck,xbounds,ybounds,1,TRUE)
  print(v)
}

##### Figure 4.2 #####
taus = seq(-4, 2, by=0.2)
out = llgrid(x1,x0,1,c(361,410),c(251,300),taus)
L = out$Llmatrix
bestparams = which(L==max(L),arr.ind=TRUE)
print(bestparams)
bestvariance = exp(taus[bestparams[3]])
print(bestvariance)

plot(im(L[,,bestparams[3]]),main="")
axis(1, at = 1:(length(out$vxs)), labels = out$vxs, pos=0.45)
axis(2, at = 1:(length(out$vys)), labels = out$vys, pos=0.45)
title("", xlab = "Vx (km/h)", ylab = "Vy (km/h)",line=1.5)
plot(im(L[,bestparams[2],]),main="")
axis(1, at = 1:(length(taus)), labels = taus,pos=0.45)
axis(2, at = 1:(length(out$vys)), labels = out$vys,pos=0.45)
title("", ylab = "Vy (km/h)", xlab = "tau",line=1.5)
plot(im(L[bestparams[1],,]),main="")
axis(1, at = 1:(length(taus)), labels = taus,pos=0.45)
axis(2, at = 1:(length(out$vxs)), labels = out$vxs,pos=0.45)
title("", ylab = "Vx (km/h)", xlab = "tau",line=1.5)

##### Figures 6.0-6.1 ######
x0 = matrix(0,ncol=500,nrow=500)
x1 = matrix(0,ncol=500,nrow=500)
x0[200:205,200:205] = 4 #down is positive
x0[210:215,200:205] = 4
x1[205:210,205:210] = 4

taus = seq(-4, 2, by=0.2)
out = llgrid(x1,x0,1,c(181,230),c(181,230),taus)
L = out$Llmatrix
bestparams = which(L==max(L),arr.ind=TRUE)
print(bestparams)

plot(im(L[,,bestparams[3]]),main="")
axis(1, at = 1:(length(out$vxs)), labels = out$vxs, pos=0.45)
axis(2, at = 1:(length(out$vys)), labels = out$vys, pos=0.45)
title("", xlab = "Vx (km/h)", ylab = "Vy (km/h)",line=1.5)

xbounds = c(181,230)
ybounds = c(181,230)
taus = seq(-4, 2, by=0.2)
bayesinf(x1,x0,k,xbounds,ybounds,10,taus)


