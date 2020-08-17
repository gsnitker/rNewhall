# need to get delepetion req into teh correct order based on the moasiture matrix
# decide if saturation column drains first or is just another column

# build each layer to from the mm.max, which is the max amount of water that each slot can hold
mm.max = c()

for (i in 1:num.rows){
  aws.mm = AWS[i]
  bd.mm = BD[i]
  wc.033.mm = WC.033[i]
  wc.15.mm = WC.15[i]
  wc.sat.mm = WC.sat[i]
  wc.ret.mm = WC.ret[i]
  
  mm.raw = c((wc.15.mm - wc.ret.mm), rep((aws.mm/num.col),  times = num.col), (wc.sat.mm - wc.033.mm)) 
  mm.max = append(mm.max, mm.raw)
}

mm.max = as.vector(mm.max)
mm.max[is.nan(mm.max)] <- 0# remove NaN from mm.max. These are casued by shallow soil depth
mm.matrix.max = matrix(mm.max, nrow = num.rows, ncol = (num.rows + 2), byrow = T)

# Hydraulic connectivity (controls depletion and accretion processes)
# basically create the k curve and then set a multiplier value (adjustable through parameter sweeps) for teh cost fo extarcting water at very low K values
# mm.rosetta.attr = rosetta.approx(mean(sand), mean(silt), mean(clay))

# possibily shoudl use teh water retention curve rather than ksat
# vg.params <- data.frame(theta_r=0.0337, theta_s=0.4864, alpha=-1.5814, npar=0.1227)
# vg.model <- KSSL_VG_model(vg.params)
# p.model <- xyplot(phi ~ theta, data=vg.model$VG_curve, type=c('l', 'g'), scales=list(alternating=3, x=list(tick.number=10), y=list(log=10, tick.number=10)), yscale.components=yscale.components.logpower, ylab=expression(Matric~~Potential~~(kPa)), xlab=expression("Volumetric Water Content" (cm^{3}/cm^{3})), par.settings = list(plot.line=list(col='RoyalBlue', lwd=2)))
# p.model

# Set amount of water to be moved down the profile each day
#per.infil = rescale(Ksat, c(.01, .1)) # now varible rates based on soil properties
percolation.percent = per.infil.val # now varible rates based on soil properties

# percolation Coeffficient
perc.coeff = c()
for (p in 1:num.rows){
  all.rosetta = rosetta.approx(sand[p], silt[p], clay[p])
  perc.coeff.raw = all.rosetta$`Rel. Percolation`
  perc.coeff  = append(perc.coeff,perc.coeff.raw )
}




# note saturation clumn purposely left out isnce it has no cost to remove water from
K.all = c()

for (i in 1:num.rows){
  vwc = c()
  for (g in 1:(num.col +2)){
    vwc.raw = WC.ret[i] + sum(mm.matrix.max[i,1:g])
    vwc = append(vwc, vwc.raw) 
  }
  vwc
  
  #K.calc
  
  Se = (vwc-WC.ret[i])/(WC.sat[i]-WC.ret[i]) # effective saturation, unitless
  Se
  K = Ksat[i]*(Se^L[i])*(1-(1-(Se^(1/(1-(1/N[i])))))^(1-(1/N[i])))^2 # hydraulic conductivity, cm/d from van Genuchten (1980)
  K.all = append(K.all, K)
}

K.all= as.vector(K.all)
K.all[is.nan(K.all)] <- 0# remove NaN from mm.max. These are casued by shallow soil depth
K.matrix = matrix(K.all, nrow = num.rows, ncol = (num.col +2), byrow = T)
K.matrix

# plot(K.matrix[1,], type = "l")
# lines(K.matrix[2,], type = "l")
# lines(K.matrix[5,], type = "l")
# lines(K.matrix[7,], type = "l")
# lines(K.matrix[10,], type = "l")

K.rev = c()
for (q in 1:num.rows){
  K.rev.raw = (1-K.matrix[q,]) *perc.coeff[q] * k.scale
  K.rev = append(K.rev, K.rev.raw )
}
K.rev= as.vector(K.rev)
K.rev[is.nan(K.rev)] <- 0# remove NaN from mm.max. These are casued by shallow soil depth
K.rev.matrix= matrix(K.rev, nrow = num.rows, ncol = (num.col +2), byrow = T)

# plot(K.rev.matrix[1,], type = "l")
# lines(K.rev.matrix[2,], type = "l")
# lines(K.rev.matrix[5,], type = "l")
# lines(K.rev.matrix[7,], type = "l")
# lines(K.rev.matrix[10,], type = "l")

K.rev.matrix = cbind(K.rev.matrix[,1:(num.col +1)],c(rep(Sat.K, 10))) # add sat column back in with the same value as FC
K.rev.matrix

# plot(K.rev.matrix[1,], type = "l", ylim=c(0,max(K.rev.matrix)))
# lines(K.rev.matrix[2,], type = "l")
# lines(K.rev.matrix[5,], type = "l")
# lines(K.rev.matrix[7,], type = "l")
# lines(K.rev.matrix[10,], type = "l")

K.calc <<- function(row){
  vwc = WC.15[row] + sum(mm.matrix.val[row,])
  Se = (vwc-WC.ret[row])/(WC.sat[row]-WC.ret[row]) # effective saturation, unitless
  K = Ksat[row]*(Se^L[row])*(1-(1-(Se^(1/(1-(1/N[row])))))^(1-(1/N[row])))^2 # hydraulic conductivity, cm/d from van Genuchten (1980)
  return(K)
}




# # creation depletion requires
#generate suction resistance based on the range of values (0:1) from .33 bar (FC)
#to 15 bar (WP), saturation is always 0.
# suction.function = function(x){ # define function to create suction matrix
#   result= c(seq(1.5, .033, length.out = 10), 0)
#   result= c(rescale(sigmoid(seq(1.5, .033, length.out = 10)), to = c(.033, 1.5)), 0)
#   return(result)
# }
 #rotate <- function(x) t(apply(x, 2, rev)) # matrix rotation function to align
# suction.req = matrix(unlist(lapply(1:num.rows, suction.function)), nrow = num.rows, ncol = 11, byrow = T)
# root.zone.req = rotate(rotate(rotate(matrix(rep(seq((2*(num.rows*row.depth)/100), 1, length.out = (num.col +1)), times = num.rows), nrow = num.rows, ncol = (num.col +1), byrow = T))))
# root.zone.req = cbind(root.zone.req, root.zone.req[,(num.col)])
root.zone.req = (0-log(10:1)+(log(10)+1)) # inversed log with depth from 1 to 3.3
root.zone.req  = rescale(root.zone.req, c(rootzone.low,rootzone.high))
K.rev.matrix* root.zone.req
depletion.req = as.vector(t(K.rev.matrix* root.zone.req))

# # Depletion order - create "slants" or diagonals per original Newhall 
depletion.order = c()
ordered.matrix = matrix(1:(num.rows*(num.col +2)),nrow = num.rows,ncol =  (num.col +2), byrow = T)
ordered.matrix = ordered.matrix[,-(num.col +2)]
for (i in num.rows:-num.rows){
  add = odiag(ordered.matrix, i)
  depletion.order = c(depletion.order, add)
}
depletion.order = c(seq(12,length(mm.max), by = 12),depletion.order)
# plot(depletion.req[depletion.order])

# 

#  depletion.order = c()
#  ordered.matrix = matrix(1:(num.rows*(num.col +1)),nrow = num.rows,ncol =  (num.col +1), byrow = T)
# for (i in 10:-10){
#           add = odiag(ordered.matrix, i)
#           depletion.order = c(depletion.order, add)}

# # Accretion Order
# accretion.raw = c(1:(num.rows  * (num.col +2)))
# accretion.raw = accretion.raw[-seq(11,length(mm.max), 11)]
# #accretion.order = c(accretion.raw, seq(11,length(mm.max),11))
# accretion.order = c(accretion.raw, seq(length(mm.max),11, -11))
accretion.order = c(1:(num.rows  * (num.col +2)))


# Accretion Req
# accretion.req.matrix  = rescale(K.matrix, 1:2)ncol = 11
# accretion.req = rep(as.vector(accretion.req.matrix[1,]), 10)

  
  