


# EXAMPLE 1
####################################
rain_data = img_to_matr("data/stormciara1")
xbounds =  c(121,170)
ybounds =  c(101,150)    
start = 15
####################################

# # EXAMPLE 2
# ####################################
# rain_data = img_to_matr("data/exampledata")
# xbounds =  c(301,350) #c(101,150)
# ybounds =  c(101,150)  
# start = 1
# ####################################

# # EXAMPLE 3
# ####################################
# rain_data = img_to_matr("data/stormciara1")
# xbounds =  c(101,150)
# ybounds =  c(321,370)    
# start = 1
# ####################################

############################################################################################
x0 = matrix(as.numeric(rain_data[,,start]), ncol = 500, nrow = 500)
x1 = matrix(as.numeric(rain_data[,,start+1]), ncol = 500, nrow = 500)
k=1
par(mar=c(0.7, 0.1, 0.7, 0.1), mfrow=c(2,3),
    oma = c(0.2, 0.2, 0.2, 0.2))
### GET PARAMETERS BETWEEN x0 AND x1 ###

taus = seq(-4, 2, by=0.1)
out = llgrid(x1,x0,k,xbounds,ybounds,taus)
L = out$Llmatrix
bestparams = which(L==max(L),arr.ind=TRUE)
VX = out$vxs[bestparams[2]]
VY = out$vys[bestparams[1]]
variance = exp(taus[bestparams[3]])


# Advect 'no' images into the future from x1 with different models and compares to actual images using performance indicators 

no = 6 
escore = replicate(no,0)  #deterministic numerical score
escorealt = replicate(no,0)  #probabilistic numerical score
n = replicate(no,0)   #deterministic visual score
nalt = replicate(no,0)  #probabilistic visual score
pers = replicate(no,0)  #persistence

# advects image 0.25*i hours into the future and gets the performance indictors of each
for (i in 1:no) {
  xnew = project(x1,k*i,VX,VY)
  #matr_to_img(xnew[ybounds[1]:ybounds[2],xbounds[1]:xbounds[2]]) #to produce deterministic predictions
  xnewalt = project_w_noise(x1,k*i,VX,VY,variance)
  #matr_to_img(xnewalt[ybounds[1]:ybounds[2],xbounds[1]:xbounds[2]]) #to produce IID probabilistic predictions
  xact = matrix(as.numeric(rain_data[,,start+1+i]), ncol = 500, nrow = 500)
  matr_to_img(xact[ybounds[1]:ybounds[2],xbounds[1]:xbounds[2]])
  
  escore[i] = err_score(xnew[ybounds[1]:ybounds[2],xbounds[1]:xbounds[2]],xact[ybounds[1]:ybounds[2],xbounds[1]:xbounds[2]])
  escorealt[i] = err_score(xnewalt[ybounds[1]:ybounds[2],xbounds[1]:xbounds[2]],xact[ybounds[1]:ybounds[2],xbounds[1]:xbounds[2]])
  pers[i] = err_score(x1[ybounds[1]:ybounds[2],xbounds[1]:xbounds[2]],xact[ybounds[1]:ybounds[2],xbounds[1]:xbounds[2]])
  
  xnewcomp = mtisto(xnew[ybounds[1]:ybounds[2],xbounds[1]:xbounds[2]])
  xactcomp = mtisto(xact[ybounds[1]:ybounds[2],xbounds[1]:xbounds[2]])
  num = replicate((ybounds[2]-ybounds[1]+1)*(xbounds[2]-xbounds[1]+1),0)
  num[xnewcomp == xactcomp] <- 1
  n[i] = sum(num) 
  
  xnewcompalt = mtisto(xnewalt[ybounds[1]:ybounds[2],xbounds[1]:xbounds[2]])
  numalt = replicate((ybounds[2]-ybounds[1]+1)*(xbounds[2]-xbounds[1]+1),0)
  numalt[xnewcompalt == xactcomp] <- 1
  nalt[i] = sum(numalt)
}
