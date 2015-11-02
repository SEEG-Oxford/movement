library(movement)
context("Utility functions")

test_that("text is capitalised", {
	expect_equal(simplifytext("teststring"), "TESTSTRING")
})

test_that("spaces are replaced with underscores", {
	expect_equal(simplifytext("string with spaces"), "STRING_WITH_SPACES")
})

# TODO
# test_that("as.location_dataframe removes duplicate origins", {
# 	testdata <- data.frame(origin=c(1,1,2,3,4), pop_origin=c(10,10,20,30,40), lat_origin=c(-1,-1,0,1,2), long_origin=c(-5,-5,-4,-3,-2))
# 	expect_equal(nrow(as.location_dataframe(testdata)), 4)
# })

test_that("as.location_dataframe creates correct class types", {
  testdata <- data.frame(location=c(1,2,3,4), population=c(10,20,30,40), x=c(-1,0,1,2), y=c(-5,-4,-3,-2))
  expect_is(as.location_dataframe(testdata), 'data.frame')
  expect_is(as.location_dataframe(testdata), 'location_dataframe')
})

test_that("as.location_dataframe creates data.frame with 4 columns", {
  testdata <- data.frame(location=c(1,2,3,4), population=c(10,20,30,40), x=c(-1,0,1,2), y=c(-5,-4,-3,-2))
	expect_equal(ncol(as.location_dataframe(testdata)), 4)
})

test_that("as.location_dataframe creates data.frame with correct location column", {
  testdata <- data.frame(location=c(1,2,3,4), population=c(10,20,30,40), x=c(-1,0,1,2), y=c(-5,-4,-3,-2))
	expect_equal(as.location_dataframe(testdata)$location, c(1,2,3,4))
})

test_that("as.location_dataframe creates data.frame with correct pop column", {
  testdata <- data.frame(location=c(1,2,3,4), population=c(10,20,30,40), x=c(-1,0,1,2), y=c(-5,-4,-3,-2))
	expect_equal(as.location_dataframe(testdata)$population, c(10,20,30,40))
})

test_that("as.location_dataframe creates data.frame with correct x (lat) column", {
  testdata <- data.frame(location=c(1,2,3,4), population=c(10,20,30,40), x=c(-1,0,1,2), y=c(-5,-4,-3,-2))
	expect_equal(as.location_dataframe(testdata)$x, c(-1,0,1,2))
})

test_that("as.location_dataframe creates data.frame with correct y (long) column", {
  testdata <- data.frame(location=c(1,2,3,4), population=c(10,20,30,40), x=c(-1,0,1,2), y=c(-5,-4,-3,-2))
	expect_equal(as.location_dataframe(testdata)$y, c(-5,-4,-3,-2))
})

test_that("as.movement_matrix.data.frame returns error for a non-square matrix", {
	testdata <- data.frame(origin=c("a","a"), destination=c("b", "c"), movement=c(1,2))
	expect_error(as.movement_matrix(testdata), "Expected a square matrix!")
})

test_that("as.movement_matrix.data.frame returns warning when movement contais non-integer values", {
  testdata <- data.frame(origin=c("a","b"), destination=c("b", "a"), movement=c(1.9,2.1))
  expect_warning(as.movement_matrix(testdata), 
                 "The given data.frame contains non-integer values. Rounding was used to return a valid movement_matrix object.")
})

test_that("as.movement_matrix.data.frame returns a rounded matrix when data.frame contains non-integer values", {
  testdata <- data.frame(origin=c("a","b"), destination=c("b", "a"), movement=c(1.9,2.1))
  expected_matrix <- matrix(c(0,2,2,0),nrow=2,dimnames=list(c("a","b"),c("a","b")))
  class(expected_matrix)  <- c('matrix', 'movement_matrix')
  expect_equal(as.movement_matrix(testdata), expected_matrix)
})

test_that("as.movement_matrix.data.frame returns a matrix of the correct dimensions", {
	testdata <- data.frame(origin=c("a","b"), destination=c("b", "a"), movement=c(1,2))
	expect_equal(dim(as.movement_matrix(testdata)), c(2,2))
})

test_that("as.movement_matrix.data.frame returns the correct matrix", {
	testdata <- data.frame(origin=c("a","b"), destination=c("b", "a"), movement=c(1,2))
	expectedmatrix <- matrix(c(0,1,2,0),nrow=2,dimnames=list(c("a","b"),c("a","b")))
	class(expectedmatrix)  <- c('matrix', 'movement_matrix')
	expect_equal(as.movement_matrix(testdata), expectedmatrix)
})

test_that("as.movement_matrix.matrix returns warning for non-integer movement_matrix", {
  non_integer_test_matrix <- matrix(c(0.23,1.222,2.001,0),nrow=2,dimnames=list(c("a","b"),c("a","b")))
  expect_warning(as.movement_matrix(non_integer_test_matrix), 
                "The given movement_matix contains non-integer values. Rounding was used to return a valid movement_matrix object.")
})

test_that("as.movement_matrix.matrix returns rounded movement_matrix when given non-integer matrix", {
  non_integer_test_matrix <- matrix(c(0.97, 1.42, 3, 2.12),nrow=2,dimnames=list(c("a","b"),c("a","b")))
  expected_matrix  <- matrix(c(1, 1, 3, 2),nrow=2,dimnames=list(c("a","b"),c("a","b")))
  class(expected_matrix)  <- c('matrix', 'movement_matrix')
  expect_equal(as.movement_matrix(non_integer_test_matrix), expected_matrix)
})

test_that("as.movement_matrix.matrix returns error for a non-square matrix", {
  "test: as.movement_matrix.matrix returns error for a non-square matrix"
  testmatrix <- matrix(c(0,1,2,0,2,3),nrow=2,dimnames=list(c("a","b"),c("a","b", "c")))
  expect_error(as.movement_matrix(testmatrix), "Expected a square matrix!")
})

test_that("as.movement_matrix.matrix returns the correct matrix", {
  testmatrix <- matrix(c(0,1,2,0),nrow=2,dimnames=list(c("a","b"),c("a","b")))
  expectedmatrix  <- testmatrix
  class(expectedmatrix)  <- c('matrix', 'movement_matrix')
  expect_true(is.movement_matrix(expectedmatrix))
  expect_false(is.movement_matrix(testmatrix))
  
  actualmatrix  <- as.movement_matrix(testmatrix)
  expect_true(is.movement_matrix(actualmatrix))
  expect_equal(actualmatrix, expectedmatrix)
})

test_that("correlate regions works for small test case", {
	testdataframe <- data.frame(name=c("a", "b", "c", "d"), location=c(1,2,3,4), pop=c(10,20,30,40), lon=c(-5,-4,-3,-2), lat=c(-1,0,1,2))
	testregionlist <- data.frame(V1=c(1,2,3,4),V2=c("a","b","c","d"))
	testmovementdata <- data.frame(origin=c(1,1,1,2,2,2,3,3,3,4,4,4), destination=c(2,3,4,1,3,4,1,2,4,1,2,3), movement=c(1,2,3,4,5,6,7,8,9,10,11,12))
	actual <- correlateregions(testdataframe, testregionlist, testmovementdata)
	expectedlocations <- data.frame(name=c("a", "b", "c", "d"), pop=c(10,20,30,40), lon=c(-5,-4,-3,-2), lat=c(-1,0,1,2))
	expectedobserved <- matrix(c(0,1,2,3,4,0,5,6,7,8,0,9,10,11,12,0), nrow=4, dimnames=list(c("a", "b", "c", "d"),c("a", "b", "c", "d")))
	expect_equivalent(actual$locations, expectedlocations)
	expect_equivalent(actual$observed, expectedobserved)
})

test_that("correlate regions removes locations from dataframe when not present as movement data", {
	testdataframe <- data.frame(name=c("a", "b", "c", "d", "e"), location=c(1,2,3,4,5), pop=c(10,20,30,40,50), lon=c(-5,-4,-3,-2,-1), lat=c(-1,0,1,2,3))
	testregionlist <- data.frame(V1=c(1,2,3,4),V2=c("a","b","c","d"))
	testmovementdata <- data.frame(origin=c(1,1,1,2,2,2,3,3,3,4,4,4), destination=c(2,3,4,1,3,4,1,2,4,1,2,3), movement=c(1,2,3,4,5,6,7,8,9,10,11,12))
	actual <- correlateregions(testdataframe, testregionlist, testmovementdata)
	expectedlocations <- data.frame(name=c("a", "b", "c", "d"), pop=c(10,20,30,40), lon=c(-5,-4,-3,-2), lat=c(-1,0,1,2))
	expectedobserved <- matrix(c(0,1,2,3,4,0,5,6,7,8,0,9,10,11,12,0), nrow=4, dimnames=list(c("a", "b", "c", "d"),c("a", "b", "c", "d")))
	expect_equivalent(actual$locations, expectedlocations)
	expect_equivalent(actual$observed, expectedobserved)
})

test_that("correlate regions removes locations from movement data when not present in dataframe", {
	testdataframe <- data.frame(name=c("a", "b", "c"), location=c(1,2,3), pop=c(10,20,30), lon=c(-5,-4,-3), lat=c(-1,0,1))
	testregionlist <- data.frame(V1=c(1,2,3,4),V2=c("a","b","c","d"))
	testmovementdata <- data.frame(origin=c(1,1,1,2,2,2,3,3,3,4,4,4), destination=c(2,3,4,1,3,4,1,2,4,1,2,3), movement=c(1,2,3,4,5,6,7,8,9,10,11,12))
	actual <- correlateregions(testdataframe, testregionlist, testmovementdata)
	expectedlocations <- data.frame(name=c("a", "b", "c"), pop=c(10,20,30), lon=c(-5,-4,-3), lat=c(-1,0,1))
	expectedobserved <- matrix(c(0,1,2,4,0,5,7,8,0), nrow=3, dimnames=list(c("a", "b", "c"),c("a", "b", "c")))
	expect_equivalent(actual$locations, expectedlocations)
	expect_equivalent(actual$observed, expectedobserved)
})

test_that("correlate regions throws an error is any of the parameters are not dataframes", {
	testdataframe <- data.frame(name=c("a", "b", "c"), location=c(1,2,3), pop=c(10,20,30), lon=c(-5,-4,-3), lat=c(-1,0,1))
	testregionlist <- data.frame(V1=c(1,2,3,4),V2=c("a","b","c","d"))
	testmovementdata <- data.frame(origin=c(1,1,1,2,2,2,3,3,3,4,4,4), destination=c(2,3,4,1,3,4,1,2,4,1,2,3), movement=c(1,2,3,4,5,6,7,8,9,10,11,12))
	expect_error(correlateregions(1, testregionlist, testmovementdata),".*Parameter 'location' must be a data.frame!.*")
	expect_error(correlateregions(testdataframe, 1, testmovementdata),".*Parameter 'regionlist' must be a data.frame!.*")
	expect_error(correlateregions(testdataframe, testregionlist, 1),".*Parameter 'movementdata' must be a data.frame!.*")

})

test_that("createobservedmatrixfromcsv can correctly process csv file", {
	expectedobserved <- matrix(c(0,300,500,100,0,600,200,400,0), nrow=3)
	actual <- createobservedmatrixfromcsv("data/observedmatrixtest.csv", "origin", "destination", "movement")
	expect_equal(actual, expectedobserved)
})

test_that("rasterizeShapeFile returns a raster", {
	actual <- rasterizeShapeFile("data/ne_110m_admin_1_states_provinces.shp",1)
	print(class(actual))
	expect_true(is(actual,"RasterLayer"))
})

test_that("logTransform correctly transforms into original variable",{
  x1  <- 12.23
  trans1  <- logTransform(x1, FALSE)
  inverse1  <- logTransform(trans1, TRUE)
  expect_equal(x1, inverse1)  
})

test_that("logTransform correctly transforms constraint variables to unconstraint variables",{
  x  <- 0.1 # positive constraint
  trans  <- logTransform(x)
  expect_true(trans < 0)
})

test_that("unitTransform correctly transforms into original variable",{
  x1  <- 0.4
  trans1  <- unitTransform(x1, FALSE)
  inverse1  <- unitTransform(trans1, TRUE)
  expect_equal(x1, inverse1)  
})

test_that("unitTransform correctly transforms constraint variables to unconstraint variables",{
  x1  <- 0.9
  trans1  <- unitTransform(x1, FALSE)
  expect_true(trans1 > 1)  
  
  x2  <- 0.1
  trans2  <- unitTransform(x2, FALSE)
  expect_true(trans2 < -1)  
  
  x3  <- 1
  trans3  <- unitTransform(x3, FALSE)
  expect_true(is.infinite(trans3))  
  
  x4  <- 0
  trans3  <- unitTransform(x3, FALSE)
  expect_true(is.infinite(trans3))   
})

test_that("identityTransform transformation correctly does not affect the input value",{
  x  <- 12.3
  trans  <- identityTransform(x, FALSE)
  expect_equal(x, trans)  
  
  inverse  <- identityTransform(x, TRUE)
  expect_equal(x, inverse)  
})

test_that("transformFluxObjectParameters correctly returns original parameter list using identityTransform", {
  originalParams  <- c(alpha = 1, beta = 2, gamma = 3)
  transform  <- c(alpha = identityTransform, beta = identityTransform, gamma = identityTransform)
  actualTransformedParams  <- transformFluxObjectParameters(originalParams, transform, FALSE)
  expect_equal(originalParams, actualTransformedParams, check.attributes = FALSE)
})

test_that("transformFluxObjectParameters correctly returns transformed parameter list ", {
  originalParams  <- c(alpha = 1, beta = 1, gamma = 1)
  expectedTransformedParams  <- c(0, beta = Inf, gamma = 1)
  transform  <- c(alpha = logTransform, beta = unitTransform, gamma = identityTransform)
  actualTransformedParams  <- transformFluxObjectParameters(originalParams, transform, FALSE)
  expect_equal(expectedTransformedParams, actualTransformedParams, check.attributes = FALSE)
})

test_that("transformFluxObjectParameters correctly returns inverse transformed parameter list ", {
  originalParams  <- c(0, beta = Inf, gamma = 1)
  expectedTransformedParams  <- c(alpha = 1, beta = 1, gamma = 1)
  transform  <- c(alpha = logTransform, beta = unitTransform, gamma = identityTransform)
  actualTransformedParams  <- transformFluxObjectParameters(originalParams, transform, TRUE)
  expect_equal(expectedTransformedParams, actualTransformedParams, check.attributes = FALSE)
})

