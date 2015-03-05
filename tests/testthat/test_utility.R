library(movement)
context("Utility functions")

test_that("text is capitalised", {
	expect_equal(simplifytext("teststring"), "TESTSTRING")
})

test_that("spaces are replaced with underscores", {
	expect_equal(simplifytext("string with spaces"), "STRING_WITH_SPACES")
})

# not sure why this one fails as it appears to work correctly in code
test_that("non ascii characters are converted", {
	expect_equal(simplifytext("Ö"), "O")
})

test_that("as.locationdataframe removes duplicate origins", {
	testdata <- data.frame(origin=c(1,1,2,3,4), pop_origin=c(10,10,20,30,40), lat_origin=c(-1,-1,0,1,2), long_origin=c(-5,-5,-4,-3,-2))
	expect_equal(nrow(as.locationdataframe(testdata)), 4)
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
	expect_equal(as.locationdataframe(testdata)$pop, c(10,20,30,40))
})

test_that("as.locationdataframe creates data.frame with correct lat column", {
	testdata <- data.frame(origin=c(1,1,2,3,4), pop_origin=c(10,10,20,30,40), lat_origin=c(-1,-1,0,1,2), long_origin=c(-5,-5,-4,-3,-2))
	expect_equal(as.locationdataframe(testdata)$lat, c(-1,0,1,2))
})

test_that("as.locationdataframe creates data.frame with correct lon column", {
	testdata <- data.frame(origin=c(1,1,2,3,4), pop_origin=c(10,10,20,30,40), lat_origin=c(-1,-1,0,1,2), long_origin=c(-5,-5,-4,-3,-2))
	expect_equal(as.locationdataframe(testdata)$lon, c(-5,-4,-3,-2))
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
	expect_equal(as.movementmatrix(testdata), expectedmatrix)
})

test_that("correlate regions works for small test case", {
	testdataframe <- data.frame(name=c("a", "b", "c", "d"), location=c(1,2,3,4), pop=c(10,20,30,40), lon=c(-5,-4,-3,-2), lat=c(-1,0,1,2))
	testregionlist <- data.frame(V1=c(1,2,3,4),V2=c("a","b","c","d"))
	testmovementdata <- data.frame(origin=c(1,1,1,2,2,2,3,3,3,4,4,4), destination=c(2,3,4,1,3,4,1,2,4,1,2,3), movement=c(1,2,3,4,5,6,7,8,9,10,11,12))
	actual <- correlateregions(testdataframe, testregionlist, testmovementdata)
	expectedlocations <- data.frame(name=c("a", "b", "c", "d"), pop=c(10,20,30,40), lon=c(-5,-4,-3,-2), lat=c(-1,0,1,2))
	expectedobserved <- matrix(c(0,1,2,3,4,5,0,6,7,8,9,0,10,11,12,0), nrow=4, dimnames=list(c("a", "b", "c", "d"),c("a", "b", "c", "d")))
	expect_equal(actual$locations, expectedlocations)
})

test_that("correlate regions removes locations from dataframe when not present as movement data", {
	testdataframe <- data.frame(name=c("a", "b", "c", "d", "e"), location=c(1,2,3,4,5), pop=c(10,20,30,40,50), lon=c(-5,-4,-3,-2,-1), lat=c(-1,0,1,2,3))
	testregionlist <- data.frame(V1=c(1,2,3,4),V2=c("a","b","c","d"))
	testmovementdata <- data.frame(origin=c(1,1,1,2,2,2,3,3,3,4,4,4), destination=c(2,3,4,1,3,4,1,2,4,1,2,3), movement=c(1,2,3,4,5,6,7,8,9,10,11,12))
	actual <- correlateregions(testdataframe, testregionlist, testmovementdata)
	expectedlocations <- data.frame(name=c("a", "b", "c", "d"), pop=c(10,20,30,40), lon=c(-5,-4,-3,-2), lat=c(-1,0,1,2))
	expectedobserved <- matrix(c(0,1,2,3,4,5,0,6,7,8,9,0,10,11,12,0), nrow=4, dimnames=list(c("a", "b", "c", "d"),c("a", "b", "c", "d")))
	expect_equal(actual$locations, expectedlocations)
})