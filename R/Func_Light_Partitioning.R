#--------------------------------------------------------------------------------------------------#
##'
##'
##' Weiss and Norman, 1985 light partitioning approach
##' 
##' @title Func_Light_Partitioning
##' 
##' @description Function to partitioning incident radiatoin into direct and diffuse radiation, 
##' based on the Weiss and Norman, 1985 light partitioning approach
##' 
##' @param SZA solar zenith angle, in degrees
##' @param P air pressure in ??
##' @param PAR measured total PAR? 
##' 
##' @return List containing: SZA - solar zenith angle, PAR - PAR, SV - 
##' 
##'  @references Weiss and Norman, 1985
##'  
##' @export
##' @author Jin Wu
##' @author Shawn Serbin
##' 
Func_Func_Light_Partitioning <- function(SZA, P, PAR){
  
  RT=PAR*0.219/(0.46)
  
  theta <- SZA/(180*pi)
  P0 <- 101325 # unit: pa
  m <- 1/cos(theta)
  
  # follow Beer's law, the expected visble direct beam radiation under clear sky, RDV
  RDV <- 600*exp(-0.185*(P/P0)*m)*cos(theta) # unit: W/m2

  # expected visible diffuse radiation under clear sky; RdV
  RdV <- 0.4*(600-RDV/cos(theta))*cos(theta)

  
}