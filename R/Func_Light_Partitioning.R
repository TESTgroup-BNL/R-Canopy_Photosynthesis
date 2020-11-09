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
##' @param P Atmospheric Pressure, in pa
##' @param PAR measured total PAR?  umol/m2/s
##' 
##' @return List containing: SZA - solar zenith angle, PAR - PAR, SV - total Visible light, 
##' SN - total NIR light, Ratio - the ratio between total measured light and total modeled light, 
##' fV - fraction of visble direct beam, fN - fraction of NIR direct beam, 
##' Model_DV - direct visible light, Model_dV - diffuse visible light, Model_DN - direct NIR light, 
##' Model_dN - diffuse NIR light
##' 
##' @references Weiss and Norman, 1985
##'  
##' @export
##' @author Jin Wu
##' @author Shawn Serbin
##' 
Func_Light_Partitioning <- function(SZA, P, PAR) {
  
  RT <- PAR * 0.219/(0.46)
  
  theta <- (SZA/180) * pi
  P0 <- 101325  # unit: pa
  m <- 1/cos(theta)
  
  # follow Beer's law, the expected visble direct beam radiation under clear sky,
  # RDV
  RDV <- 600 * exp(-0.185 * (P/P0) * m) * cos(theta)  # unit: W/m2
  
  # expected visible diffuse radiation under clear sky; RdV
  RdV <- 0.4 * (600 - RDV/cos(theta)) * cos(theta)
  
  # define parameter antilog10
  antilog10 <- 10^(-1.195 + 0.4459 * log(m)/log(10) - 0.0345 * (log(m)/log(10)) * 
    (log(m)/log(10)))
  
  # w--is water absorption in the near infreared for 10 mm of precipitable water,
  # under clear sky (adapted from Wang, 1976)
  w <- 1320 * antilog10  # !!why is the incidient rad fixed at 1320??!!
  
  # expected direct-beam of near-infrared radiation under clear sky
  RDN <- (720 * exp(-0.06 * (P/P0) * m) - w) * cos(theta)
  
  # expected diffuse-beam of near-infrared radiation under clear sky
  RdN <- 0.6 * (720 - RDN/cos(theta) - w) * cos(theta)
  
  # total visible light expected under clear sky
  RV <- RDV + RdV
  
  # total nir light expected under clear sky
  RN <- RDN + RdN
  
  # total Visible light
  SV <- RT * (RV/(RV + RN))
  
  # total NIR light
  SN <- RT * (RN/(RV + RN))
  
  # the ratio between total measured light and total modeled light
  Ratio <- RT/(RV + RN)
  
  A <- 0.9
  B <- 0.7
  C <- 0.88
  D <- 0.68
  
  s1 <- (A - Ratio)/B
  s1[s1 < 0] <- 0
  s1a <- 1 - s1^(2/3)
  
  s2 <- (C - Ratio)/D
  s2[s2 < 0] <- 0
  s2a <- 1 - s2^(2/3)
  
  fV <- RDV/RV * s1a  # fraction of visble direct beam
  fN <- RDN/RN * s2a  # fraction of NIR direct beam
  
  ## Build output list
  output <- list(SZA = SZA, PAR = PAR, SV = SV * 4.57, SN = SN * 4.57, Ratio = Ratio, 
    fV = fV, fN = fN, Model_DV = SV * fV * 4.57, Model_dV = SV * 4.57 - SV * 
      fV * 4.57, Model_DN = SN * fN * 4.57, ModeldN = SN * 4.57 - SN * fN * 
      4.57)
  return(output)
  
}  ## End of Function
#--------------------------------------------------------------------------------------------------#
### EOF
