
context("check output from temperature scale functions is working properly")

# Tests (evolving)
test_that("Temperature response functions are scaling parameters correctly", {
  
  # Bernacchi scaling to 25 properly
  expect_equal(round(Func_Temperature_Bernacchi(65.33,26.35,25),2),1)
  
  # Bernacchi scaling to 30 properly
  expect_equal(round(Func_Temperature_Bernacchi(65.33,26.35,30),2),1.54)
  #expect_equal(round(Func_Temperature_Bernacchi(65.33,26.35,30),2),5) # obvious failure, to test the test
  
  # Medlyn
  expect_equal(round(Func_Temperature_Medlyn(25,25,40,65.33),2),40)
  expect_equal(round(Func_Temperature_Medlyn(25,40,40,65.33),2),141.35)
               
})
