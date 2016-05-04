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
##' @return List containing: Lsun - Sunlit LAI; Lshade - Shade LAI; Ic - Canopy total absorbed irradiance;
##' Isun - Sunlit leaf absorbed irradiance; Ishade - Shade leaf absorbed irradiance; Vc - Canopy total Vcmax;
##' Vcsun - Sunlit leaf Vcmax; Vcshade - Shade leaf Vcmax
##'
##' @references dePury and Farquhar, 1997; Ryu et al., 2011
##' 
##' @export
##' @author Jin Wu
##' @author Shawn Serbin

Func_Canopy_Radiation_Transfer <- function(FLAG, SZA, LAI, Ib0, Id0, Vcmax0_25, CI){
  print("in development")
  #pi <- 3.1415926 # nescessary? can also just do pi
  
  ## 1 Sun/shade LAI paritioning
  G <- 0.5 # the parameter used in Depury and Farquhar's model
  kb <- G/cos(SZA/180*pi) # extinction coefficient; G refers to G function, could be 0.5 to simplify it
  Lsun <- (1-exp(-kb*LAI*CI))/kb # Sun LAI
  Lshade <- LAI-Lsun # Shade LAI


  output <- list(Lsun=Lsun,Lshade=Lshade)
  return(output)
} ## End of Function
#--------------------------------------------------------------------------------------------------#
### EOF