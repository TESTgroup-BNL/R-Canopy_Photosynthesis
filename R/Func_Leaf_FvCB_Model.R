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
##' @return List containing: Anet - Net photosynthetic rate (Agross-Rd,umol/m2/s) at leaf temperature, 
##' Agross - Gross photosynthetic rate (umol/m2/s) at leaf temperature, Rd - Leaf respiration rate (umol/m2/s) at leaf temperature, 
##' Wc - Rubisco limited photosynthetic rate (umol/m2/s), Wj - RuBP limited photosynthetic rate (umol/m2/s),
##' Wp - TPU limited photosynthetic rate (0.5*Vcmax, umol/m2/s), Vcmax - Maximum rate of RuBP carboxylation (umol/m2/s) 
##' at leaf temperature, Jmax - Maximum rate of electron transport (umol/m2/s) at leaf temperature,
##' Vo - , Vomax - Maximum rate of oxygen evolution (umol/m2/s) at leaf temperature, Gamma* - at leaf temperature,
##' Kc - , Ko - , PSII - , Phi - at leaf temperature
##' 
##' @references Long and Bernacchi, 2003; Medlyn et al., 2002; Bernacchi et al., 2013
##' 
##' @export
##' @author Jin Wu
##' @author Shawn Serbin
##' 
Func_Leaf_FvCB_Photosynthesis_Model <- function(Vcmax25, Jmax25, Tleaf, Topt, I, 
                                                Ci, Press, PSII_in,Phi_in){

  #print("Running leaf photosyntheis")
  #cat("Function call was:  ", deparse(match.call()), "\n", sep = "")
  num.args <- nargs()
  if (num.args<9)
    stop(paste0("Missing function arguments. Number of arguments ", print(num.args)," less than the total required 9"))

  Scaled_parameters <- Func_Temperature_Response(Vcmax25, Jmax25, Tleaf, Topt, Press)
  Vcmax <- Scaled_parameters$Vcmax
  Jmax <- Scaled_parameters$Jmax
  Gamma_star <- Scaled_parameters$Gamma_star
  Kc <- Scaled_parameters$Kc
  Ko <- Scaled_parameters$Ko
  PSII <- Scaled_parameters$PSII
  Phi <- Scaled_parameters$Phi
  Rd <- Scaled_parameters$Rd
  Vomax <- Scaled_parameters$Vomax # TODO: Remove Vo, Vomax from code
  
  # For now we are fixing these at their input values. In the future we can use the temperature scaled values
  PSII <- PSII_in
  Phi <- Phi_in
  # !!!
  
  C <- Ci*Press/(101325)
  O <- 210*Press/(101325) # % mmol/mol
  
  # model photosynthesis reference from Bernacchi et al. 2013, Long and Bernacchi 2003
  Wc <- Vcmax*C/(C+Kc*(1+O/Ko)) # Rubisco limited photosynthesis
  Wp <- 0.5*Vcmax # using default or non-direct observed TPU
  
  f <- 0.15 # spectral correction factor
  I_used <- I*(1-f)*0.5*PSII # the absorbed light used for photosynthesis process
  
  J <- I_used+Jmax-sqrt((I_used+Jmax)^2-4*Phi*I_used*Jmax)
  J <- J/(2*Phi)
  
  # Equation 6 in Long and Bernacchi, 2003;  Multiply limitation states by 1-Gama*/Ci
  # Instead could just use form similar to medlyn et al 2002 where Gamma* has been multiplied through
  Wj <- J*C/(4*C+8*Gamma_star)
  Wc <- Wc*(1-Gamma_star/C)
  Wj <- Wj*(1-Gamma_star/C)
  
  # modeled gross photo
  Agross <- min(Wc,Wj,Wp)
  
  # !! WHAT IS THIS FOR?
  Vo <- Vomax*O/(O+Ko*(1+C/Kc))
  # !!
  
  # modeled net photo
  An <- Agross-Rd
  
  output <- list(Anet=An, Agross=Agross, Rd=Rd, Wc=Wc, Wj=Wj, Wp=Wp, Vcmax=Vcmax, Jmax=Jmax,
                 Vo=Vo, Vomax=Vomax, Gamma_star=Gamma_star, Kc=Kc, Ko=Ko, PSII=PSII, Phi=Phi)
  return(output)
  
} # End of function
#--------------------------------------------------------------------------------------------------#
### EOF