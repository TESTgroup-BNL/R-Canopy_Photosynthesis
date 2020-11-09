#################################################################################################### 
#  Test leaf photosynthesis response Purpose: 1) Test response to light 2) Test
#  response to temperature 3) Test leaf photosynthesis response to PSII (maximum
#  intrinsic quantum yield) and Phi (curvature factor)
####################################################################################################

#---------------- Close all devices and delete all variables. -------------------------------------#
rm(list = ls(all = TRUE))  # clear workspace
graphics.off()  # close any open graphics
closeAllConnections()  # close any open connections to files

## Load library
library(CanopyPhotosynthesis)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
# Test default run
V25 <- 40  # %Bonan et al. 2012
J25 <- 1.64 * V25 + 29.1  # % Bonan et al. 2012
PAR <- 2000
Tleaf <- 30
Topt <- 35
AmbCO2 <- 380
Ci <- AmbCO2 * 0.7
Press <- 10^5
PII_in <- 0.8  # shouldn't this be PSII_in??
Phi_in <- 0.75

# quick test
FvCB <- Func_Leaf_FvCB_Photosynthesis_Model(V25, J25, Tleaf, Topt, PAR, Ci, Press, 
  PII_in, Phi_in)

#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
## Test of the response of photosynthesis to light

PAR0 <- seq(10, 2000, 10)
Vc <- seq(100, 10, -10)
An <- array(data = NA, dim = c(length(PAR0), length(Vc)))
for (i in 1:length(PAR0)) {
  PAR <- PAR0[i]
  for (j in 1:length(Vc)) {
    V25 <- Vc[j]
    J25 <- 1.64 * V25 + 29.1
    FvCB <- Func_Leaf_FvCB_Photosynthesis_Model(V25, J25, Tleaf, Topt, PAR, Ci, 
      Press, PII_in, Phi_in)
    An[i, j] <- FvCB$Anet
  }
}


# Plot results
dev.new()
matplot(PAR0, An, type = "l", xlab = "PAR (umols/m2/s)")
box(lwd = 2.2)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
## Quick test of photo to temperature
PAR <- 2000
V25 <- 60  # Bonan et al. 2012
J25 <- 1.64 * V25 + 29.1  # Bonan et al. 2012
Tl <- seq(10, 40, 1)

An <- array(data = NA, length(Tl))
Rd <- array(data = NA, length(Tl))
Vcmax <- array(data = NA, length(Tl))
Jmax <- array(data = NA, length(Tl))
Gamma_star <- array(data = NA, length(Tl))
Kc <- array(data = NA, length(Tl))
Ko <- array(data = NA, length(Tl))
for (i in 1:length(Tl)) {
  Tleaf <- Tl[i]
  FvCB <- Func_Leaf_FvCB_Photosynthesis_Model(V25, J25, Tleaf, Topt, PAR, Ci, Press, 
    PII_in, Phi_in)
  An[i] <- FvCB$Anet
  Rd[i] <- FvCB$Rd
  Vcmax[i] <- FvCB$Vcmax
  Jmax[i] <- FvCB$Jmax
  Gamma_star[i] <- FvCB$Gamma_star
  Kc[i] <- FvCB$Kc
  Ko[i] <- FvCB$Ko
}

dev.new()
cexaxis <- 1.5
cexlab <- 1.5
par(mfrow = c(2, 4), mar = c(4.1, 5.4, 0.4, 0.4), oma = c(0.1, 0.6, 0.1, 0.1))
plot(Tl, An, type = "l", lwd = 3, ylab = "Net Photosynthesis (umols/m2/s)", xlab = "", 
  cex.axis = cexaxis, cex.lab = cexlab)
box(lwd = 2.2)
plot(Tl, Rd, type = "l", lwd = 3, ylab = "Rd (umols/m2/s)", xlab = "", cex.axis = cexaxis, 
  cex.lab = cexlab)
box(lwd = 2.2)
plot(Tl, Vcmax, type = "l", lwd = 3, ylab = "Vcmax (umols/m2/s)", xlab = "", cex.axis = cexaxis, 
  cex.lab = cexlab)
box(lwd = 2.2)
box(lwd = 2.2)
plot(Tl, Jmax, type = "l", lwd = 3, ylab = "Jmax (umols/m2/s)", xlab = "", cex.axis = cexaxis, 
  cex.lab = cexlab)
box(lwd = 2.2)
plot(Tl, Gamma_star, type = "l", lwd = 3, ylab = "Gamma_star", xlab = "", cex.axis = cexaxis, 
  cex.lab = cexlab)
box(lwd = 2.2)
plot(Tl, Kc, type = "l", lwd = 3, ylab = "Kc", xlab = "", cex.axis = cexaxis, cex.lab = cexlab)
box(lwd = 2.2)
plot(Tl, Ko, type = "l", lwd = 3, ylab = "Ko", xlab = "", cex.axis = cexaxis, cex.lab = cexlab)
box(lwd = 2.2)
#--------------------------------------------------------------------------------------------------#
## EOF
