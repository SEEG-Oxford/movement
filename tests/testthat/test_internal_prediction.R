library(movement)
context("Internal prediction and optimisation methods")

	
test_that("get.network.fromdataframe returns population list", {
	testdataframe <- data.frame(name=c("a", "b", "c", "d"), origin=c(1,2,3,4), pop_origin=c(10,20,30,40), long_origin=c(-5,-4,-3,-2), lat_origin=c(-1,0,1,2))
	expectedpopulation <- c(10,20,30,40)
	expect_equal(get.network.fromdataframe(testdataframe)$population, expectedpopulation)
})

test_that("get.network.fromdataframe returns distance matrix", {	
	testdataframe <- data.frame(name=c("a", "b", "c", "d"), origin=c(1,2,3,4), pop_origin=c(10,20,30,40), long_origin=c(-5,-4,-3,-2), lat_origin=c(-1,0,1,2))
	d <- function(x, y) {
		p1 = testdataframe$long_origin[x]
		p2 = testdataframe$lat_origin[x]
		q1 = testdataframe$long_origin[y]
		q2 = testdataframe$lat_origin[y]
		return(sqrt((p1-q1)^2 + (p2-q2)^2))
	}
	expecteddistance <- matrix(c(d(1,1),d(1,2),d(1,3),d(1,4),d(2,1),d(2,2),d(2,3),d(2,4),d(3,1),d(3,2),d(3,3),d(3,4),d(4,1),d(4,2),d(4,3),d(4,4)), nrow=4, dimnames=list(c(1,2,3,4),c(1,2,3,4)))
	
	actual <- get.network.fromdataframe(testdataframe)$distance_matrix
	expect_equal(actual, expecteddistance)
})

test_that("get.network.fromdataframe returns coordinates", {	
	testdataframe <- data.frame(name=c("a", "b", "c", "d"), origin=c(1,2,3,4), pop_origin=c(10,20,30,40), long_origin=c(-5,-4,-3,-2), lat_origin=c(-1,0,1,2))
	
	expectedcoords <- matrix(c(-5,-4,-3,-2,-1,0,1,2), nrow=4, dimnames=list(c(),c("x","y")))
	
	actual <- get.network.fromdataframe(testdataframe)$coordinates
	expect_equal(actual, expectedcoords)
})

test_that("get.network.fromdataframe returns locations", {	
	testdataframe <- data.frame(name=c("a", "b", "c", "d"), origin=c(1,2,3,4), pop_origin=c(10,20,30,40), long_origin=c(-5,-4,-3,-2), lat_origin=c(-1,0,1,2))
	
	expectedlocations <- c(1,2,3,4)
	
	actual <- get.network.fromdataframe(testdataframe)$locations
	expect_equal(actual, expectedlocations)
})