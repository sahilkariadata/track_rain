rain_data = img_to_matr("data/stormciara1")
# x0 = matrix(as.numeric(rain_data[,,15]), ncol = 500, nrow = 500)
# x1 = matrix(as.numeric(rain_data[,,16]), ncol = 500, nrow = 500)
# x2 = matrix(as.numeric(rain_data[,,17]), ncol = 500, nrow = 500)
# x3 = matrix(as.numeric(rain_data[,,18]), ncol = 500, nrow = 500)
# x4 = matrix(as.numeric(rain_data[,,19]), ncol = 500, nrow = 500)
# x5 = matrix(as.numeric(rain_data[,,20]), ncol = 500, nrow = 500)
# x6 = matrix(as.numeric(rain_data[,,21]), ncol = 500, nrow = 500)
# x7 = matrix(as.numeric(rain_data[,,22]), ncol = 500, nrow = 500)
# 
# xbounds =  c(121,170)
# ybounds =  c(101,150)    #definite = used as example no.1

# x0 = matrix(as.numeric(rain_data[,,1]), ncol = 500, nrow = 500)
# x1 = matrix(as.numeric(rain_data[,,2]), ncol = 500, nrow = 500)
# x2 = matrix(as.numeric(rain_data[,,3]), ncol = 500, nrow = 500)
# x3 = matrix(as.numeric(rain_data[,,4]), ncol = 500, nrow = 500)
# x4 = matrix(as.numeric(rain_data[,,5]), ncol = 500, nrow = 500)
# x5 = matrix(as.numeric(rain_data[,,6]), ncol = 500, nrow = 500)
# 
# xbounds =  c(101,150)
# ybounds =  c(351,400)     #good = example no.2 storm data where most of 50x50 is filled and peak is not normal and very spread

x0 = matrix(as.numeric(rain_data[,,1]), ncol = 500, nrow = 500)
x1 = matrix(as.numeric(rain_data[,,2]), ncol = 500, nrow = 500)
x2 = matrix(as.numeric(rain_data[,,3]), ncol = 500, nrow = 500)
x3 = matrix(as.numeric(rain_data[,,4]), ncol = 500, nrow = 500)
x4 = matrix(as.numeric(rain_data[,,5]), ncol = 500, nrow = 500)
x5 = matrix(as.numeric(rain_data[,,6]), ncol = 500, nrow = 500)
x6 = matrix(as.numeric(rain_data[,,7]), ncol = 500, nrow = 500)
x7 = matrix(as.numeric(rain_data[,,8]), ncol = 500, nrow = 500)

xbounds =  c(101,150)
ybounds =  c(321,370)     #maybe = example no.3 very strong at k=1, try error maps at k=2, also bayesinf at k=2

# rain_data = img_to_matr("data/stormciara2")
# 
# x0 = matrix(as.numeric(rain_data[,,6]), ncol = 500, nrow = 500)
# x1 = matrix(as.numeric(rain_data[,,7]), ncol = 500, nrow = 500)
# x2 = matrix(as.numeric(rain_data[,,8]), ncol = 500, nrow = 500)
# x3 = matrix(as.numeric(rain_data[,,9]), ncol = 500, nrow = 500)
# 
# xbounds =  c(101,150)
# ybounds =  c(51,100)          strong example like no.1 sparse data

# rain_data = img_to_matr("data/example2")
# 
# x0 = matrix(as.numeric(rain_data[,,1]), ncol = 500, nrow = 500)
# x1 = matrix(as.numeric(rain_data[,,2]), ncol = 500, nrow = 500)
# x2 = matrix(as.numeric(rain_data[,,3]), ncol = 500, nrow = 500)
# x3 = matrix(as.numeric(rain_data[,,4]), ncol = 500, nrow = 500)
# 
# xbounds =  c(171,220)
# ybounds =  c(151,200)
# 
# rain_data = img_to_matr("data/exampledata")
# 
# x0 = matrix(as.numeric(rain_data[,,1]), ncol = 500, nrow = 500)
# x1 = matrix(as.numeric(rain_data[,,2]), ncol = 500, nrow = 500)
# x2 = matrix(as.numeric(rain_data[,,3]), ncol = 500, nrow = 500)
# x3 = matrix(as.numeric(rain_data[,,4]), ncol = 500, nrow = 500)
# x4 = matrix(as.numeric(rain_data[,,5]), ncol = 500, nrow = 500)
# x5 = matrix(as.numeric(rain_data[,,6]), ncol = 500, nrow = 500)
# x6 = matrix(as.numeric(rain_data[,,7]), ncol = 500, nrow = 500)
# x7 = matrix(as.numeric(rain_data[,,8]), ncol = 500, nrow = 500)
# 
# xbounds =  c(301,350) #c(101,150)
# ybounds =  c(101,150) #c(251,300)(vgood prediction could be used)     #used as example 2


par(mar=c(0.7, 0.1, 0.7, 0.1), mfrow=c(2,3),
    oma = c(0.2, 0.2, 0.2, 0.2))
matr_to_img(x0[ybounds[1]:ybounds[2],xbounds[1]:xbounds[2]])
matr_to_img(x1[ybounds[1]:ybounds[2],xbounds[1]:xbounds[2]])
matr_to_img(x2[ybounds[1]:ybounds[2],xbounds[1]:xbounds[2]])
matr_to_img(x3[ybounds[1]:ybounds[2],xbounds[1]:xbounds[2]])
matr_to_img(x4[ybounds[1]:ybounds[2],xbounds[1]:xbounds[2]])
matr_to_img(x5[ybounds[1]:ybounds[2],xbounds[1]:xbounds[2]])
matr_to_img(x6[ybounds[1]:ybounds[2],xbounds[1]:xbounds[2]])
matr_to_img(x7[ybounds[1]:ybounds[2],xbounds[1]:xbounds[2]])



# k=1
# taus = seq(0, 2, by=0.1)
# Lout = llgrid(x1,x0,k,xbounds,ybounds,taus)
# L = Lout$Llmatrix
# bestparams = which(L==max(L),arr.ind=TRUE)
# print(c(Lout$vxs[bestparams[2]],Lout$vys[bestparams[1]],taus[bestparams[3]]))
# 
# plot(im(L[,,bestparams[3]]),main="")
# axis(1, at = 1:(length(Lout$vxs)), labels = Lout$vxs, pos=0.45)
# axis(2, at = 1:(length(Lout$vys)), labels = Lout$vys, pos=0.45)
# title("", xlab = "Vx (km/h)", ylab = "Vy (km/h)", line=1.5)
# 
# taus = seq(0, 2, by=0.1)
# Lout = llgrid(x2,x1,k,xbounds,ybounds,taus)
# L = Lout$Llmatrix
# bestparams = which(L==max(L),arr.ind=TRUE)
# print(c(Lout$vxs[bestparams[2]],Lout$vys[bestparams[1]],taus[bestparams[3]]))
# 
# plot(im(L[,,bestparams[3]]),main="")
# axis(1, at = 1:(length(Lout$vxs)), labels = Lout$vxs, pos=0.45)
# axis(2, at = 1:(length(Lout$vys)), labels = Lout$vys, pos=0.45)
# title("", xlab = "Vx (km/h)", ylab = "Vy (km/h)", line=1.5)
# 
# taus = seq(0, 2, by=0.1)
# Lout = llgrid(x3,x2,k,xbounds,ybounds,taus)
# L = Lout$Llmatrix
# bestparams = which(L==max(L),arr.ind=TRUE)
# print(c(Lout$vxs[bestparams[2]],Lout$vys[bestparams[1]],taus[bestparams[3]]))
# 
# plot(im(L[,,bestparams[3]]),main="")
# axis(1, at = 1:(length(Lout$vxs)), labels = Lout$vxs, pos=0.45)
# axis(2, at = 1:(length(Lout$vys)), labels = Lout$vys, pos=0.45)
# title("", xlab = "Vx (km/h)", ylab = "Vy (km/h)", line=1.5)
# 
# taus = seq(0, 2, by=0.1)
# Lout = llgrid(x4,x3,k,xbounds,ybounds,taus)
# L = Lout$Llmatrix
# bestparams = which(L==max(L),arr.ind=TRUE)
# print(c(Lout$vxs[bestparams[2]],Lout$vys[bestparams[1]],taus[bestparams[3]]))
# 
# plot(im(L[,,bestparams[3]]),main="")
# axis(1, at = 1:(length(Lout$vxs)), labels = Lout$vxs, pos=0.45)
# axis(2, at = 1:(length(Lout$vys)), labels = Lout$vys, pos=0.45)
# title("", xlab = "Vx (km/h)", ylab = "Vy (km/h)", line=1.5)




# plot(im(L[,bestparams[2],]),main="")
# axis(1, at = 1:(length(Lout$vxs)), labels = Lout$vxs, pos=0.45)
# axis(2, at = 1:(length(Lout$vys)), labels = Lout$vys, pos=0.45)
# title("", ylab = "Vy (km/h)", xlab = "tau", line=1.5)
# 
# plot(im(L[bestparams[1],,]),main="")
# axis(1, at = 1:(length(Lout$vxs)), labels = Lout$vxs, pos=0.45)
# axis(2, at = 1:(length(Lout$vys)), labels = Lout$vys, pos=0.45)
# title("", ylab = "Vx (km/h)", xlab = "tau", line=1.5)

par(mar=c(2, 2.5,0, 2.5), mfrow=c(1,2),
    oma = c(0, 0.2, 0.2, 0.2))

e = err_map(x0,x1,xbounds,ybounds,1,TRUE)
plot(im(e$map), main ="")
axis(1, at = 1:(length(e$xaxis)), labels = e$xaxis, pos=0.45)
axis(2, at = 1:(length(e$yaxis)), labels = e$yaxis, pos=0.45)
title( xlab = "Vx (km/h)", ylab = "Vy (km/h)",line = 1)
print(lse(x0,x1,xbounds,ybounds,1,TRUE))

e = err_map(x1,x2,xbounds,ybounds,1,TRUE)
plot(im(e$map), main ="")
axis(1, at = 1:(length(e$xaxis)), labels = e$xaxis, pos=0.45)
axis(2, at = 1:(length(e$yaxis)), labels = e$yaxis, pos=0.45)
title( xlab = "Vx (km/h)", ylab = "Vy (km/h)",line = 1)
print(lse(x1,x2,xbounds,ybounds,1,TRUE))

e = err_map(x2,x3,xbounds,ybounds,1,TRUE)
plot(im(e$map), main ="")
axis(1, at = 1:(length(e$xaxis)), labels = e$xaxis, pos=0.45)
axis(2, at = 1:(length(e$yaxis)), labels = e$yaxis, pos=0.45)
title( xlab = "Vx (km/h)", ylab = "Vy (km/h)",line = 1)
print(lse(x2,x3,xbounds,ybounds,1,TRUE))

e = err_map(x3,x4,xbounds,ybounds,1,TRUE)
plot(im(e$map), main ="")
axis(1, at = 1:(length(e$xaxis)), labels = e$xaxis, pos=0.45)
axis(2, at = 1:(length(e$yaxis)), labels = e$yaxis, pos=0.45)
title( xlab = "Vx (km/h)", ylab = "Vy (km/h)",line = 1)
print(lse(x3,x4,xbounds,ybounds,1,TRUE))

e = err_map(x4,x5,xbounds,ybounds,1,TRUE)
plot(im(e$map), main ="")
axis(1, at = 1:(length(e$xaxis)), labels = e$xaxis, pos=0.45)
axis(2, at = 1:(length(e$yaxis)), labels = e$yaxis, pos=0.45)
title( xlab = "Vx (km/h)", ylab = "Vy (km/h)",line = 1)
print(lse(x4,x5,xbounds,ybounds,1,TRUE))

e = err_map(x5,x6,xbounds,ybounds,1,TRUE)
plot(im(e$map), main ="")
axis(1, at = 1:(length(e$xaxis)), labels = e$xaxis, pos=0.45)
axis(2, at = 1:(length(e$yaxis)), labels = e$yaxis, pos=0.45)
title( xlab = "Vx (km/h)", ylab = "Vy (km/h)",line = 1)
print(lse(x5,x6,xbounds,ybounds,1,TRUE))

e = err_map(x6,x7,xbounds,ybounds,1,TRUE)
plot(im(e$map), main ="")
axis(1, at = 1:(length(e$xaxis)), labels = e$xaxis, pos=0.45)
axis(2, at = 1:(length(e$yaxis)), labels = e$yaxis, pos=0.45)
title( xlab = "Vx (km/h)", ylab = "Vy (km/h)",line = 1)
print(lse(x6,x7,xbounds,ybounds,1,TRUE))

paras = bayesinf(x1,x0,k,xbounds,ybounds,10,taus)
print(paras)

x2p = ppm(paras,x1,xbounds,ybounds,k)
matr_to_img(x2p)

