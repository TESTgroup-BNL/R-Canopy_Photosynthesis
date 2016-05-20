#--------------------------------------------------------------------------------------------------#
##' 
##' Leaf level FvCB Photosynthesis model (Farquhar et al. 1980)
##' 
##' @title Func_Leaf_FvCB_Photosynthesis_Model
##' 
##' @param Vcmax25 Vcmax at 25 degrees C
##' @param Jmax25 Jmax at 25 degrees C
##' @param Tleaf Leaf temperature
##' @param Topt Temperature optimum for Jmax
##' @param I incident light
##' @param Ci internal CO2 concentration in umols/mol or ppm
##' @param Press Atmospheric pressure in Pa
##' @param PSII_in Input PSII for maximum quantum yield
##' @param Phi_in Input curvature factor for light response function
##' 
Func_Leaf_FvCB_Photosynthesis_Model <- function(Vcmax25, Jmax25, Tleaf, Topt, I, Ci, Press, PSII_in,
                                                Phi_in){
  print("in development")
  
  Scaled_parameters <- Func_Temperature_Response(V25, J25, T, Topt, Press)
  Vcmax <- Scaled_parameters$Vcmax
  Jmax <- Scaled_parameters$Jmax
  Tau_star <- Scaled_parameters$Tau_star
  Kc <- Scaled_parameters$Kc
  Ko <- Scaled_parameters$Ko
  PSII <- Scaled_parameters$PSII
  Phi <- Scaled_parameters$Phi
  Rd <- Scaled_parameters$Rd
  Vomax <- Scaled_parameters$Vomax
  
  # !!! WHY ARE WE SWITCHING FROM T SCALED TO INPUT HERE?
  PSII <- PSII_in
  Phi <- Phi_in
  # !!!
  
  C <- Ci*Pres/(101325)
  O <- 210*Pres/(101325) # % mmol/mol
  
  # model photosynthesis reference from Bernacchi et al. 2013
  Wc <- Vcmax*C/(C+Kc*(1+O/Ko)) # Rubisco limited photosynthesis
  Wp <- 0.5*Vcmax # using default or non-direct observed TPU
  
  f <- 0.15 # spectral correction factor
  I_used <- I*(1-f)*0.5*PSII # the absorbed light used for photosynthesis process
  
  J <- I_used+Jmax-sqrt((I_used+Jmax)^2-4*Phi*I_used*Jmax)
  J <- J/(2*Phi)
  
  Wj <- J*C/(4*C+8*Tau_star)
  Wc <- Wc*(1-Tau_star/C)
  Wj <- Wj*(1-Tau_star/C)
  
  #!!!! FUNCTION NOT YET COMPLETE!
  output <- list(Anet=An, Rd=Rd, Vo=Vo)
  
}