#--------------------------------------------------------------------------------------------------#
##'
##' Goal: Use revised DF1997 model to partition canopy LAI into sunlit/shade leaves LAI and 
##' partition canopy Vcmax into sunlit/shade leaves Vcmax clumping index was added to original 
##' DF1997 model, following the reference from Ryu et al. 2011
##' 
##' @title Func_Canopy_Radiation_Transfer
##' 
##' @description Function for revised DF1997 model to partition canopy LAI into sunlit/shade leaves LAI and
##' partition canopy Vcmax into sunlit/shade leaves Vcmax clumping index was added to original
##' DF1997 model, following the reference from Ryu et al. 2011
##' 
##' @param FLAG Model version controller;  0--Lloyd et al. 2010 Model for Vcmax-LAI relationship; 
##' 1--Mercado et al. 2006 Model for Vcmax-LAI relationship in the tropics
##' @param SZA solar zenith angle, in degrees
##' @param LAI Canopy leaf area index
##' @param Ib0 direct beam at canopy top
##' @param Id0 diffuse irradiance at canopy top
##' @param Vcmax0_25 Vcmax at reference 25 centi-degree for canopy top leaves
##' @param CI Clumping inedx; 0.63 for tropical evergreen forests (Chen et al, 2005)
##' 
##' @return List containing: PAR0 - ; Ib0 - ; Id0 - ; Lsun - Sunlit LAI; Lshade - Shade LAI; Ic - Canopy total absorbed irradiance;
##' Isun - Sunlit leaf absorbed irradiance; Ishade - Shade leaf absorbed irradiance; Vc - Canopy total Vcmax;
##' Vcsun - Sunlit leaf Vcmax; Vcshade - Shade leaf Vcmax
##'
##' @references dePury and Farquhar, 1997; Ryu et al., 2011
##' 
##' @export
##' @author Jin Wu
##' @author Shawn Serbin
##'
Func_Canopy_Radiation_Transfer <- function(FLAG, SZA, LAI, Ib0, Id0, Vcmax0_25, CI){

  ## 1 Sun/shade LAI paritioning
  G <- 0.5 # the parameter used in Depury and Farquhar's model
  kb <- G/cos(SZA/180*pi) # extinction coefficient; G refers to G function, could be 0.5 to simplify it
  Lsun <- (1-exp(-kb*LAI*CI))/kb # Sun LAI
  Lshade <- LAI-Lsun # Shade LAI

  ## 2 Total irradiance absorbed by the canopy (Eq. 13; dePury and Farquhar, 1997) -TODO: enable the updadte of the reflectance coefficients
  rocb <- 0.029 # canopy reflection coefficient for beam PAR; assuming it is the same as for diffuse PAR; 0.036 is apparently too low
  rocd <- 0.036 # canopy reflection coefficient for diffuse PAR; assuming it is the same as for beam PAR; 0.036 is apparently too low
  kb1 <- 0.46/cos(SZA/180*pi) # beam and scattered beam PAR extinction coefficient
  kd1 <- 0.719 # diffuse and scattered diffuse PAR exintinction coefficient
  Ic <- (1-rocb)*Ib0*(1-exp(-kb1*LAI*CI))+(1-rocd)*Id0*(1-exp(-kd1*LAI*CI)) # canopy-scale irradiance absorbed
  Ic <- (1-rocb)*Ib0*(1-exp(-kb1*LAI*CI))+(1-rocd)*Id0*(1-exp(-kd1*LAI*CI))
  
  ## 3 Calculating sunlit leave irradiance
  sigma <- 0.15 # leaf scattering coefficient of pAR, ro1+tao1; where ro1=0.1 for leaf reflection; tao1=0.05 for leaf transmissivity
  Isun1 <- Ib0*(1-sigma)*(1-exp(-kb*LAI*CI)) # direct beam, sunlit leaves
  Isun2 <- Id0*(1-rocd)*(1-exp(-(kd1+kb)*LAI*CI))*kd1/(kd1+kb) # diffuse beam, sunlit leaves
  Isun3 <- Ib0*((1-rocb)*(1-exp(-(kb1+kb)*LAI*CI))*kb1/(kb1+kb)-(1-sigma)*(1-exp(-2*kb*LAI*CI))/2) # scattering beam absored by sunlit leaves
  Isun <- Isun1+Isun2+Isun3 # 

  Ishade <- Ic-Isun
  
  ## 4 Photosynthetic capacity of sunlit and shaded leaf fraction
  # Vertical distribution of leaf-level Vcmax
  if (FLAG==0) {
    # updated relationship based on the Lloyd et al. 2010, cieted by Bonan et al. 2014
    kn <- exp(0.00963*Vcmax0_25-2.43)
    Vcsun <- CI*Vcmax0_25/(kn+kb.*CI)*(1-exp(-(kb*CI+kn)*LAI)) # sunlit leaves Vcmax
    Vc <- Vcmax0_25/kn*(1-exp(-kn*LAI)) # canopy scale vcmax
    Vcshade <- Vc-Vcsun
  } else {
    # updated relationship based on the Mercado et al. 2006
    kn <- 0.1823
    Vcsun <- CI*Vcmax0_25/(kn+kb*CI)*(1-exp(-(kb*CI+kn)*LAI)) # sunlit leaves Vcmax
    Vc <- Vcmax0_25/kn*(1-exp(-kn*LAI)) # canopy scale vcmax
    Vcshade <- Vc-Vcsun 
  }

  
  ## Build output list
  output <- list(PAR0=Ib0+Id0,Ib0=Ib0,Id0=Id0,Lsun=Lsun,Lshade=Lshade,Ic=Ic, Isun=Isun,Ishade=Ishade,
                 Vc=Vc,Vcsun=Vcsun,Vcshade=Vcshade)
  return(output)
  
} ## End of Function
#--------------------------------------------------------------------------------------------------#
### EOF