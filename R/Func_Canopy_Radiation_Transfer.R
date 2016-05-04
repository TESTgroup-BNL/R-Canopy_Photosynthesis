#--------------------------------------------------------------------------------------------------#
##'
##' Goals: using revised DF1997 model to partition canopy LAI into sunlit/shade leaves LAI and 
##' partition canopy Vcmax into sunlit/shade leaves Vcmax clumping index was added to original 
##' DF1997 model, following the reference from Ryu et al. 2011
##' 
##' @title Func_Canopy_Radiance_Transfer
##' 
##' @description Function for revised DF1997 model to partition canopy LAI into sunlit/shade leaves LAI and
##' partition canopy Vcmax into sunlit/shade leaves Vcmax clumping index was added to original
##' DF1997 model, following the reference from Ryu et al. 2011
##' 
##' Inputs
##' @param FLAG Model version controller;  0--Lloyd et al. 2010 Model for Vcmax-LAI relationship; 
##' 1--Mercado et al. 2006 Model for Vcmax-LAI relationship in the tropics
##' @param SZA solar zenith angle, in degrees

Func_Canopy_Radiance_Transfer <- function(FLAG, SZA, LAI, Ib0, Id0, Vcmax0_25, CI){
  print("in development")
  
} ## End of Function
#--------------------------------------------------------------------------------------------------#
### EOF