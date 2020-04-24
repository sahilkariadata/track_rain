xbounds =  c(101,150)
ybounds =  c(321,370)   
start = 1
x0 = matrix(as.numeric(rain_data[,,start]), ncol = 500, nrow = 500)
x1 = matrix(as.numeric(rain_data[,,start+1]), ncol = 500, nrow = 500)
k = 1

### GET PARAMETERS BETWEEN x0 AND x1 ###

taus = seq(-4, 2, by=0.1)
out = llgrid(x1,x0,k,xbounds,ybounds,taus)
L = out$Llmatrix
# bestparams = colMeans(bayesinf(x1,x0,k,xbounds,ybounds,1000,taus))
bestparams = which(L==max(L),arr.ind=TRUE)
VX = out$vxs[bestparams[2]]
VY = out$vys[bestparams[1]]
variance = exp(taus[bestparams[3]])
# VX = bestparams[1]
# VY = bestparams[2]
# variance = bestparams[3]

no = 6
escore = replicate(no,0)
escorealt = replicate(no,0)
n = replicate(no,0) #number of pixels on y=x line
nalt = replicate(no,0) #number of pixels on y=x line
pers = replicate(no,0)
for (i in 1:no) {
  xnew = project(x1,k*i,VX,VY)
  #matr_to_img(xnew[ybounds[1]:ybounds[2],xbounds[1]:xbounds[2]])
  xnewalt = project_w_noise(x1,k*i,VX,VY,variance)
  #matr_to_img(xnewalt[ybounds[1]:ybounds[2],xbounds[1]:xbounds[2]])
  xact = matrix(as.numeric(rain_data[,,start+1+i]), ncol = 500, nrow = 500)
  
  escore[i] = err_score(xnew[ybounds[1]:ybounds[2],xbounds[1]:xbounds[2]],xact[ybounds[1]:ybounds[2],xbounds[1]:xbounds[2]])
  escorealt[i] = err_score(xnewalt[ybounds[1]:ybounds[2],xbounds[1]:xbounds[2]],xact[ybounds[1]:ybounds[2],xbounds[1]:xbounds[2]])
  pers[i] = err_score(x1[ybounds[1]:ybounds[2],xbounds[1]:xbounds[2]],xact[ybounds[1]:ybounds[2],xbounds[1]:xbounds[2]])
  
  # xnewcomp = log2(as.vector(xnew[ybounds[1]:ybounds[2],xbounds[1]:xbounds[2]]))
  # xnewcomp[xnewcomp == "-Inf"] = -7
  # xactcomp = log2(as.vector(xact[ybounds[1]:ybounds[2],xbounds[1]:xbounds[2]]))
  # xactcomp[xactcomp == "-Inf"] = -7
  xnewcomp = mtisto(xnew[ybounds[1]:ybounds[2],xbounds[1]:xbounds[2]])
  xactcomp = mtisto(xact[ybounds[1]:ybounds[2],xbounds[1]:xbounds[2]])
  num = replicate((ybounds[2]-ybounds[1]+1)*(xbounds[2]-xbounds[1]+1),0)
  num[xnewcomp == xactcomp] <- 1
  n[i] = sum(num) 
  
  xnewcompalt = mtisto(xnewalt[ybounds[1]:ybounds[2],xbounds[1]:xbounds[2]])
  numalt = replicate((ybounds[2]-ybounds[1]+1)*(xbounds[2]-xbounds[1]+1),0)
  numalt[xnewcompalt == xactcomp] <- 1
  nalt[i] = sum(numalt)
  #plot(xnewcomp,xactcomp) 
  #abline(coef = c(0,1))
  #arrows(x0=x-sds[,1], y0=y, x1=x+sds[,1], y1=y, code=3, angle=90, length=0.001)
}
