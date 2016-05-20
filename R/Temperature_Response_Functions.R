#--------------------------------------------------------------------------------------------------#
# Functions for applying temperature response functions to photosynthesis parameters
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
##'
##' @description Bernacchi temperature response function for photosynthesis parameters
##' @details Bernacchi temperature response function for photosynthesis parameters
##' 
##' @title Func_Temperature_Bernacchi
##' 
##' @param delta_H Activation energy
##' @param c Scaling constant
##' @param T leaf temperature in degrees C
##'
##' @return temperature scale factor
##'
##' @export
##' @author Jin Wu
##' @author Shawn Serbin
##'
Func_Temperature_Bernacchi <- function(delta_H,c,T){
  Tk <- T+273.15
  R <- 8.3144598 # J/K/mol, universal gas constant
  delta_H <- delta_H*1000 # unit conversion from kJ/mol to J/mol
  scale <- exp(c-delta_H/(R*Tk)) #scale factor
  return(scale)
}
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
##'
##' @title Func_Temperature_June
##' @description June temperature response function for photosynthesis parameters
##' @details June temperature response function for photosynthesis parameters
##' 
##' @param P25 Vcmax/Jmax at 25 degrees C
##' @param Topt Temperature optimum of Vcmax/Jmax
##' @param T leaf temperature in degrees C
##'
##' @references Bernacchi et al. 2013 and June et al. 2004
##'
##' @export
##' @author Jin Wu
##' @author Shawn Serbin
##'
Func_Temperature_June <- function(P25,Topt,T){
  # Omega--parameters controlling the temperature sensitivity of Jmax
  Omega <- 11.6+0.18*Topt # empirical relationship from June et al. 2004; Fig. 4
  scale <- ((T-Topt)/Omega)^2
  Jmax <- P25*exp(-scale)
  return(Jmax)
}
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
##'
##' @description Temperature response functions for scaling leaf-level photosynthesis parameters
##' @details Temperature response functions for scaling leaf-level photosynthesis parameters
##' 
##' @title Func_Temperature_Response
##'
##' @param VC25 Vcmax at 25 degrees C
##' @param J25 Jmax at 25 degrees C
##' @param T leaf temperature in degrees C
##' @param Topt Temperature optimum of Vcmax/Jmax
##' @param Press Atmospheric pressure in Pa
##'
##' @references Bernacchi et al., 2002, 2003, 2013
##'
##' @return List containing: Vcmax - Vcmax at leaf temperature, Jmax - Jmax at leaf temperature,
##' Tau_star - Tau* at leaf temperature, Kc - Kc MM constant at leaf temperature, Ko - Ko MM constant at leaf temperature,
##' PSII - PSII at leaf temperature, Phi - Phi at leaf temperature, Rd - leaf respiration at leaf temperature,
##' Vomax - Vomax (max oxygen evolution) at leaf temperature
##'
##' @export
##' @author Jin Wu
##' @author Shawn Serbin
##'
Func_Temperature_Response <- function(V25, J25, T, Topt, Press){
  
  # Temperature scaling function
  # !!! THIS SECTION NEEDS REVIEW 
  Jmax_25 <- Func_Temperature_June(J25, Topt, 25) # needed?
  Jmax <- Func_Temperature_June(J25, Topt, T) # reference: June et al. 2004; Bernacchi et al. 2013
  Jmax <- Jmax/Jmax_25*J25  #!! Unclear why we have this step?? !!
  # !!! 
  
  # Temperature functions for Vcmax, Tau_star, Ko, Kc, Rd
  delta_H <- 65.33
  c <- 26.35 
  s1 <- Func_Temperature_Bernacchi(delta_H, c, T) # Reference: Bernacchi et al. 2001; Bernacchi et al. 2013
  sl_25 <- Func_Temperature_Bernacchi(delta_H, c, 25) # Reference: Bernacchi et al. 2001; Bernacchi et al. 2013
  Vcmax <- V25*s1/sl_25 
  rm(delta_H, c, s1, sl_25)
  
  # should remove these hardcoded values here for delta_H and c
  delta_H <- 37.83
  c <- 19.02
  s1 <- Func_Temperature_Bernacchi(delta_H, c, T) # reference: Bernacchi et al. 2001; Bernacchi et al. 2013
  sl_25 <- Func_Temperature_Bernacchi(delta_H, c, 25) # reference: Bernacchi et al. 2001; Bernacchi et al. 2013
  Tau_star <- 42.75*s1/sl_25 # umol/mol
  rm(delta_H, c, s1, sl_25)
  
  delta_H <- 79.43
  c <- 38.05 
  s1 <- Func_Temperature_Bernacchi(delta_H, c, T) # reference: Bernacchi et al. 2001; Bernacchi et al. 2013
  sl_25 <- Func_Temperature_Bernacchi(delta_H, c, 25) # reference: Bernacchi et al. 2001; Bernacchi et al. 2013
  Kc <- 404.9*s1/sl_25 # umol/mol
  rm(delta_H, c, s1, sl_25)
  
  delta_H <- 36.38;
  c <- 20.30; 
  s1 <- Func_Temperature_Bernacchi(delta_H, c, T) # reference: Bernacchi et al. 2001; Bernacchi et al. 2013
  sl_25 <- Func_Temperature_Bernacchi(delta_H, c, 25) # reference: Bernacchi et al. 2001; Bernacchi et al. 2013
  Ko <- 278.4*s1/sl_25 # mmol/mol
  rm(delta_H, c, s1, sl_25)
  
  Phi <- 0.76+0.018*T-3.7*(1e-4)*T^2 # growth temperature=25 from Bernacchi et al. 2003 % Curvature factor between An-PAR
  PSII <- 0.352+0.022*T-3.4*(1e-4)*T^2 # maximum quantumn yield; using light adapted version from Bernacchi et al. 2003
  
  delta_H <- 46.39
  c <- 18.72
  s1 <- Func_Temperature_Bernacchi(delta_H, c, T) # reference: Bernacchi et al. 2001; Bernacchi et al. 2013
  sl_25 <- Func_Temperature_Bernacchi(delta_H, c, 25) # reference: Bernacchi et al. 2001; Bernacchi et al. 2013
  Rd <- 0.015*V25*s1/sl_25 # umol/mol
  rm(delta_H, c, s1, sl_25)
  
  # Oxygen
  Oxygen <- 210*Press/(101325) # mmol/mol

  #!!!!
  # not very sure for Vomax modeling  !!! DO WE NEED THIS??
  Vomax <- Vcmax*Ko*Tau_star/(0.5*Kc*Oxygen) # according to equation 7 in Bernacchi et al. 2001
  Vomax25 <- V25*278.4*42.75/(0.5*404.9*Oxygen)

  delta_H <- 60.11
  c <- 22.98 
  s1 <- Func_Temperature_Bernacchi(delta_H, c, T) # reference: Bernacchi et al. 2001; Bernacchi et al. 2013
  sl_25 <- Func_Temperature_Bernacchi(delta_H, c, 25) # reference: Bernacchi et al. 2001; Bernacchi et al. 2013
  Vomax <- Vomax25*s1/sl_25  # umol/mol
  rm(delta_H, c, s1, sl_25)
  #!!!
  #!!!!
  
  output <- list(Vcmax=Vcmax, Jmax=Jmax, Tau_star=Tau_star, Kc=Kc, Ko=Ko, PSII=PSII, Phi=Phi, Rd=Rd,
                 Vomax=Vomax)
  return(output)
  
  
} ## End of function
#--------------------------------------------------------------------------------------------------#
### EOF