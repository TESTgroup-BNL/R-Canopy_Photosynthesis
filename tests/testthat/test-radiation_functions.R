
context("check output from radiation and scaling functions to confirm they are still working properly")

# Tests (evolving)
test_that("Radiation functions are working correctly", {
  # expect_equal(three_times(pi),9.4247, tolerance=1e-8)
  
  # Test light partitioning function
  expect_equal(lapply(Func_Light_Partitioning(SZA = 30, P = 10^5, PAR = 1320), 
    round, 2)$SV, 1335.96, tolerance = 0.01)
  expect_equal(lapply(Func_Light_Partitioning(SZA = 30, P = 10^5, PAR = 1320), 
    round, 2)$SN, 1535.99, tolerance = 0.01)
  expect_equal(lapply(Func_Light_Partitioning(SZA = 30, P = 10^5, PAR = 1320), 
    round, 2)$Ratio, 0.64, tolerance = 0.01)
  expect_equal(lapply(Func_Light_Partitioning(SZA = 30, P = 10^5, PAR = 1320), 
    round, 2)$fV, 0.44, tolerance = 0.01)
  expect_equal(lapply(Func_Light_Partitioning(SZA = 30, P = 10^5, PAR = 1320), 
    round, 2)$fN, 0.47, tolerance = 0.01)
  expect_equal(lapply(Func_Light_Partitioning(SZA = 30, P = 10^5, PAR = 1320), 
    round, 2)$Model_DV, 582.2, tolerance = 0.01)
  expect_equal(lapply(Func_Light_Partitioning(SZA = 30, P = 10^5, PAR = 1320), 
    round, 2)$Model_dV, 753.76, tolerance = 0.01)
  expect_equal(lapply(Func_Light_Partitioning(SZA = 30, P = 10^5, PAR = 1320), 
    round, 2)$Model_DN, 722.88, tolerance = 0.01)
  expect_equal(lapply(Func_Light_Partitioning(SZA = 30, P = 10^5, PAR = 1320), 
    round, 2)$ModeldN, 813.12, tolerance = 0.01)
  
  # Test radiation transfer function
  expect_equal(lapply(Func_Canopy_Radiation_Transfer(FLAG = 1, SZA = 30, LAI = 6, 
    Ib0 = 582.2005, Id0 = 722.8758, Vcmax0_25 = 40, CI = 0.63), round, 2)$PAR0, 
    1305.08)
  expect_equal(lapply(Func_Canopy_Radiation_Transfer(FLAG = 1, SZA = 30, LAI = 6, 
    Ib0 = 582.2005, Id0 = 722.8758, Vcmax0_25 = 40, CI = 0.63), round, 2)$Lsun, 
    1.54)
  expect_equal(lapply(Func_Canopy_Radiation_Transfer(FLAG = 1, SZA = 30, LAI = 6, 
    Ib0 = 582.2005, Id0 = 722.8758, Vcmax0_25 = 40, CI = 0.63), round, 2)$Lshade, 
    4.46)
  expect_equal(lapply(Func_Canopy_Radiation_Transfer(FLAG = 1, SZA = 30, LAI = 6, 
    Ib0 = 582.2005, Id0 = 722.8758, Vcmax0_25 = 40, CI = 0.63), round, 2)$Ic, 
    1140.25)
  expect_equal(lapply(Func_Canopy_Radiation_Transfer(FLAG = 1, SZA = 30, LAI = 6, 
    Ib0 = 582.2005, Id0 = 722.8758, Vcmax0_25 = 40, CI = 0.63), round, 2)$Isun, 
    845.17)
  expect_equal(lapply(Func_Canopy_Radiation_Transfer(FLAG = 1, SZA = 30, LAI = 6, 
    Ib0 = 582.2005, Id0 = 722.8758, Vcmax0_25 = 40, CI = 0.63), round, 2)$Ishade, 
    295.08)
  expect_equal(lapply(Func_Canopy_Radiation_Transfer(FLAG = 1, SZA = 30, LAI = 6, 
    Ib0 = 582.2005, Id0 = 722.8758, Vcmax0_25 = 40, CI = 0.63), round, 2)$Vc, 
    145.93)
  expect_equal(lapply(Func_Canopy_Radiation_Transfer(FLAG = 1, SZA = 30, LAI = 6, 
    Ib0 = 582.2005, Id0 = 722.8758, Vcmax0_25 = 40, CI = 0.63), round, 2)$Vcsun, 
    44.41)
  expect_equal(lapply(Func_Canopy_Radiation_Transfer(FLAG = 1, SZA = 30, LAI = 6, 
    Ib0 = 582.2005, Id0 = 722.8758, Vcmax0_25 = 40, CI = 0.63), round, 2)$Vcshade, 
    101.52)
  
})
