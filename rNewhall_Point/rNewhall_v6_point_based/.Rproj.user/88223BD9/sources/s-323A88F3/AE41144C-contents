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
  
  mm.raw = c(rep((aws.mm/num.col),  times = num.col), (wc.sat.mm - wc.033.mm))
  mm.max = append(mm.max, mm.raw)
}

mm.max = as.vector(mm.max)
mm.max[is.nan(mm.max)] <- 0# remove NaN from mm.max. These are casued by shallow soil depth
mm.matrix.max = matrix(mm.max, nrow = num.rows, ncol = 11, byrow = T)

# Hydraulic connectivity (controls depletion and accretion processes)
# basically create teh k curve and then set a multiplier value (adjustable through parameter sweeps) for teh cost fo extarcting water at very low K values
# mm.rosetta.attr = rosetta.approx(mean(sand), mean(silt), mean(clay))
# 
# 
# vwc = sum(WC.15) 
# vwc
# Se.mm.wp = (vwc-sum(WC.ret))/(sum(WC.sat)-sum(WC.ret)) # effective saturation, unitless
# Se = seq(Se.mm.wp, 1, by= ((1-Se.mm.wp)/(num.rows*(num.col+1))))
# K = mm.rosetta.attr$`Ksat [cm/day]`*(Se^mm.rosetta.attr$L)*(1-(1-(Se^(1/(1-(1/mm.rosetta.attr$n)))))^(1-(1/mm.rosetta.attr$n)))^2 # hydraulic conductivity, cm/d from van Genuchten (1980):
# 
# K.rescale = rescale(K, to = c(1,5)) # value of 5 is adjustable, set like this for continuity with original newhall model
# plot(K.rescale)
# depletion.req = K.rescale 

# # creation depletion requires
#generate suction resistance based on the range of values (0:1) from .33 bar (FC)
#to 15 bar (WP), saturation is always 0.
suction.function = function(x){ # define function to create suction matrix
  result= c(seq(1.5, .033, length.out = 10), 0)
  result= c(rescale(sigmoid(seq(1.5, .033, length.out = 10)), to = c(.033, 1.5)), 0)
  return(result)
}
rotate <- function(x) t(apply(x, 2, rev)) # matrix rotation function to align
suction.req = matrix(unlist(lapply(1:num.rows, suction.function)), nrow = num.rows, ncol = 11, byrow = T)
root.zone.req = rotate(rotate(rotate(matrix(rep(seq((2*(num.rows*row.depth)/100), 1, length.out = num.rows), times = num.rows), nrow = 10, ncol = num.rows, byrow = T))))
root.zone.req = cbind(root.zone.req, root.zone.req[,10])
depletion.req = as.vector(t(suction.req + root.zone.req))

# # Depletion order - create "slants" or diagonals per original Newhall 
depletion.order = c()
ordered.matrix = matrix(1:(num.rows*(num.col +1)),nrow = num.rows,ncol =  (num.col +1), byrow = T)
ordered.matrix = ordered.matrix[,-11]
for (i in 9:-9){
  add = odiag(ordered.matrix, i)
  depletion.order = c(depletion.order, add)
}
depletion.order = c(seq(11,length(mm.max), by = 11),depletion.order)
# 

#  depletion.order = c()
#  ordered.matrix = matrix(1:(num.rows*(num.col +1)),nrow = num.rows,ncol =  (num.col +1), byrow = T)
# for (i in 10:-10){
#           add = odiag(ordered.matrix, i)
#           depletion.order = c(depletion.order, add)}

# # Accretion Order
accretion.raw = c(1:(num.rows  * 11))
accretion.raw = accretion.raw[-seq(11,length(mm.max), 11)]
#accretion.order = c(accretion.raw, seq(11,length(mm.max),11))
accretion.order = c(accretion.raw, seq(length(mm.max),11, -11))



# Accretion Req
# set mm spinup conditions
mm = mm.max

# Set amount of water to be moved down the profile each day
per.infil = 0.0
