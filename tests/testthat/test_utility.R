library(movement)
context("Utility functions")

test_that("text is capitalised", {
	expect_equal(simplifytext("teststring"), "TESTSTRING")
})

test_that("spaces are replaced with underscores", {
	expect_equal(simplifytext("string with spaces"), "STRING_WITH_SPACES")
})

test_that("as.locationdataframe removes duplicate origins", {
	testdata <- data.frame(origin=c(1,1,2,3,4), pop_origin=c(10,10,20,30,40), lat_origin=c(-1,-1,0,1,2), long_origin=c(-5,-5,-4,-3,-2))
	expect_equal(nrow(as.locationdataframe(testdata)), 4)
})

test_that("as.locationdataframe creates correct class types", {
  testdata <- data.frame(origin=c(1,1,2,3,4), pop_origin=c(10,10,20,30,40), lat_origin=c(-1,-1,0,1,2), long_origin=c(-5,-5,-4,-3,-2))
  expect_is(as.locationdataframe(testdata), 'data.frame')
  expect_is(as.locationdataframe(testdata), 'locationdataframe')
})

test_that("as.locationdataframe creates data.frame with 4 columns", {
	testdata <- data.frame(origin=c(1,1,2,3,4), pop_origin=c(10,10,20,30,40), lat_origin=c(-1,-1,0,1,2), long_origin=c(-5,-5,-4,-3,-2))
	expect_equal(ncol(as.locationdataframe(testdata)), 4)
})

test_that("as.locationdataframe creates data.frame with correct location column", {
	testdata <- data.frame(origin=c(1,1,2,3,4), pop_origin=c(10,10,20,30,40), lat_origin=c(-1,-1,0,1,2), long_origin=c(-5,-5,-4,-3,-2))
	expect_equal(as.locationdataframe(testdata)$location, c(1,2,3,4))
})

test_that("as.locationdataframe creates data.frame with correct pop column", {
	testdata <- data.frame(origin=c(1,1,2,3,4), pop_origin=c(10,10,20,30,40), lat_origin=c(-1,-1,0,1,2), long_origin=c(-5,-5,-4,-3,-2))
	expect_equal(as.locationdataframe(testdata)$population, c(10,20,30,40))
})

test_that("as.locationdataframe creates data.frame with correct lat column", {
	testdata <- data.frame(origin=c(1,1,2,3,4), pop_origin=c(10,10,20,30,40), lat_origin=c(-1,-1,0,1,2), long_origin=c(-5,-5,-4,-3,-2))
	expect_equal(as.locationdataframe(testdata)$x, c(-1,0,1,2))
})

test_that("as.locationdataframe creates data.frame with correct lon column", {
	testdata <- data.frame(origin=c(1,1,2,3,4), pop_origin=c(10,10,20,30,40), lat_origin=c(-1,-1,0,1,2), long_origin=c(-5,-5,-4,-3,-2))
	expect_equal(as.locationdataframe(testdata)$y, c(-5,-4,-3,-2))
})

test_that("as.movementmatrix returns error for a non-square matrix", {
	testdata <- data.frame(origin=c("a","a"), destination=c("b", "c"), movement=c(1,2))
	expect_error(as.movementmatrix(testdata), "Expected a square matrix!")
})

test_that("as.movementmatrix returns a matrix of the correct dimensions", {
	testdata <- data.frame(origin=c("a","b"), destination=c("b", "a"), movement=c(1,2))
	expect_equal(dim(as.movementmatrix(testdata)), c(2,2))
})

test_that("as.movementmatrix returns the correct matrix", {
	testdata <- data.frame(origin=c("a","b"), destination=c("b", "a"), movement=c(1,2))
	expectedmatrix <- matrix(c(0,1,2,0),nrow=2,dimnames=list(c("a","b"),c("a","b")))
	class(expectedmatrix)  <- c('matrix', 'movementmatrix')
	expect_equal(as.movementmatrix(testdata), expectedmatrix)
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

test_that("unityTransform correctly transforms into original variable",{
  x1  <- 0.4
  trans1  <- unityTransform(x1, FALSE)
  inverse1  <- unityTransform(trans1, TRUE)
  expect_equal(x1, inverse1)  
})

test_that("unityTransform correctly transforms constraint variables to unconstraint variables",{
  x1  <- 0.9
  trans1  <- unityTransform(x1, FALSE)
  expect_true(trans1 > 1)  
  
  x2  <- 0.1
  trans2  <- unityTransform(x2, FALSE)
  expect_true(trans2 < -1)  
  
  x3  <- 1
  trans3  <- unityTransform(x3, FALSE)
  expect_true(is.infinite(trans3))  
  
  x4  <- 0
  trans3  <- unityTransform(x3, FALSE)
  expect_true(is.infinite(trans3))   
})

test_that("identityTransform transformation correctly does not affect the input value",{
  x  <- 12.3
  trans  <- identityTransform(x, FALSE)
  expect_equal(x, trans)  
  
  inverse  <- identityTransform(x, TRUE)
  expect_equal(x, inverse)  
})

