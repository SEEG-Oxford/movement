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