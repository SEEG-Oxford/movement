library(movement)
context("Utility functions")

test_that("text is capitalised", {
	expect_equal(simplifytext("teststring"), "TESTSTRING")
})

test_that("spaces are replaced with underscores", {
	expect_equal(simplifytext("string with spaces"), "STRING_WITH_SPACES")
})

test_that("as.location_dataframe removes duplicate locations when all values are the same", {
  testdata <- data.frame(location=c(1,1,2,3,4), population=c(10,10,20,30,40), x=c(-1,-1,0,1,2), y=c(-5,-5,-4,-3,-2))
	expect_equal(nrow(as.location_dataframe(testdata)), 4)
})

test_that("as.location_dataframe removes duplicate locations with different population values", {
  testdata <- data.frame(location=c(1,1,2,3,4), population=c(10,5,20,30,40), x=c(-1,-1,0,1,2), y=c(-5,-5,-4,-3,-2))
  expect_equal(nrow(as.location_dataframe(testdata)), 4)
})

test_that("as.location_dataframe print warning when removing duplicate locations", {
  testdata <- data.frame(location=c(1,1,2,3,4), population=c(10,5,20,30,40), x=c(-1,-1,0,1,2), y=c(-5,-5,-4,-3,-2))
  expect_warning(as.location_dataframe(testdata), "Warning: The following duplicated rows were removed from the location data frame:")
})

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
  testdata <- data.frame(location=c(1,1,2,3,4), population=c(10,5,20,30,40), x=c(-1,-1,0,1,2), y=c(-5,-5,-4,-3,-2))
	expect_equal(as.location_dataframe(testdata)$location, c(1,2,3,4))
})

test_that("as.location_dataframe creates data.frame with correct population column", {
  testdata <- data.frame(location=c(1,1,2,3,4), population=c(10,5,20,30,40), x=c(-1,-1,0,1,2), y=c(-5,-5,-4,-3,-2))
	expect_equal(as.location_dataframe(testdata)$population, c(10,20,30,40))
})

test_that("as.location_dataframe creates data.frame with correct x (lat) column", {
  testdata <- data.frame(location=c(1,1,2,3,4), population=c(10,5,20,30,40), x=c(-1,-1,0,1,2), y=c(-5,-5,-4,-3,-2))
	expect_equal(as.location_dataframe(testdata)$x, c(-1,0,1,2))
})

test_that("as.location_dataframe creates data.frame with correct y (long) column", {
  testdata <- data.frame(location=c(1,1,2,3,4), population=c(10,5,20,30,40), x=c(-1,-1,0,1,2), y=c(-5,-5,-4,-3,-2))
	expect_equal(as.location_dataframe(testdata)$y, c(-5,-4,-3,-2))
})

test_that("as.movement_matrix.data.frame returns error for a non-square matrix", {
	testdata <- data.frame(origin=c("a","a"), destination=c("b", "c"), movement=c(1,2))
	expect_error(as.movement_matrix(testdata), "Expected a square matrix!")
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

test_that("as.data.frame.movement_matrix returns an error when given a non-square matrix", {
  non_square_matrix  <- matrix(c(0,1,2,0,0,1),nrow=2,dimnames=list(c("a","b"),c("a","b", "c")))
  class(non_square_matrix) <- c('matrix', 'movement_matrix')
  expect_true(is.movement_matrix(non_square_matrix))  
  expect_error(as.data.frame.movement_matrix(non_square_matrix), "Error: expected square matrix.")
})

test_that("as.data.frame.movement_matrix returns an error when given matrix is not movement_matrix object", {
  non_movment_matrix  <- matrix(c(0,1,2,0),nrow=2,dimnames=list(c("a","b"),c("a","b")))
  expect_false(is.movement_matrix(non_movment_matrix))  
  expect_error(as.data.frame.movement_matrix(non_movment_matrix), "Error: expected a movement_matrix object.")
})

test_that("as.data.frame.movement_matrix correctly returns the data.frame", {
  testmatrix  <- matrix(c(0,1,2,0),nrow=2,dimnames=list(c("a","b"),c("a","b")))
  testmatrix  <- as.movement_matrix(testmatrix)
  expected_data.frame  <- data.frame(origin=c("a","b"), destination=c("b", "a"), movement=c(2,1), stringsAsFactors = FALSE)
  actual_data.frame  <- as.data.frame.movement_matrix(testmatrix) 
  expect_equal(expected_data.frame, actual_data.frame)
})

test_that("as.data.frame.movement_matrix returns data.frame with default origin/destinations when missing row / column names", {
  testmatrix  <- matrix(c(0,1,2,0),nrow=2)
  testmatrix  <- as.movement_matrix(testmatrix)
  expected_data.frame  <- data.frame(origin=c("1","2"), destination=c("2", "1"), movement=c(2,1), stringsAsFactors = FALSE)
  actual_data.frame  <- as.data.frame.movement_matrix(testmatrix) 
  expect_equal(expected_data.frame, actual_data.frame)
})

test_that("as.data.frame.movement_matrix correctly returns the data.frame for 4x4 matrix", {
  row1 <- c(0,1,2,3)
  row2 <- c(4,0,5,6)
  row3 <- c(7,8,0,9)
  row4 <- c(10,11,12,0)
  testmatrix  <- matrix(rbind(row1,row2,row3,row4), nrow = 4,dimnames=list(c("a","b","c", "d"),c("a","b","c","d")))
  testmatrix  <- as.movement_matrix(testmatrix)
  expected_data.frame  <- data.frame(origin = c("a", "a", "a", "b", "b", "b", "c", "c", "c", "d", "d", "d"), 
                                     destination = c("b", "c","d","a","c","d", "a", "b", "d", "a", "b", "c"), 
                                     movement=c(1,2,3,4,5,6,7,8,9,10,11,12), 
                                     stringsAsFactors = FALSE)
  actual_data.frame  <- as.data.frame.movement_matrix(testmatrix) 
  expect_equal(expected_data.frame, actual_data.frame)
})


test_that("as.data.frame.movement_matrix print warning when missing row / column names", {
  testmatrix  <- matrix(c(0,1,2,0),nrow=2)
  testmatrix  <- as.movement_matrix(testmatrix)
  expect_warning(as.data.frame.movement_matrix(testmatrix), "The given movement_matrix has no row or column names defined to identify the origins and destinations.")
})

test_that("consistencyCheckMovementMatrixLocationDataframe returns correctly true for correct locationData and movement matrix with character locations", {
  testMatrix  <- matrix(c(0,1,2,0),nrow=2, dimnames=list(c("a","b"),c("a","b")))
  testMatrix  <- as.movement_matrix(testMatrix)
  testLocationData <- data.frame(location=c("a","b"), population=c(10,20), x=c(-1,-1), y=c(-5,-5))
  testLocationData  <- as.location_dataframe(testLocationData)
  expect_true(consistencyCheckMovementMatrixLocationDataframe(testMatrix, testLocationData))
})

test_that("consistencyCheckMovementMatrixLocationDataframe returns correctly true for correct locationData and movement matrix with character locations 2", {
  testMatrix  <- matrix(c(0,1,2,0,3,4,6,2,5,6,3,1,4,5,7,7),nrow=4, dimnames=list(c("UK","DEN", "GER", "FRA"),c("UK","DEN", "GER", "FRA")))
  testMatrix  <- as.movement_matrix(testMatrix)
  testLocationData <- data.frame(location=c("UK","DEN", "GER", "FRA"), population=c(100010,212000, 2810231, 2720903), x=c(-1,-1, 1, 2), y=c(-5,-5, 3, 4))
  testLocationData  <- as.location_dataframe(testLocationData)
  expect_true(consistencyCheckMovementMatrixLocationDataframe(testMatrix, testLocationData))
})

test_that("consistencyCheckMovementMatrixLocationDataframe returns correctly true for correct locationData and movement matrix with numeric locations 2", {
  testMatrix  <- matrix(c(0,1,2,0,4,5,5,6,7),nrow=3, dimnames=list(c("4335","4426", "4427"),c("4335","4426", "4427")))
  testMatrix  <- as.movement_matrix(testMatrix)
  testLocationData <- data.frame(location=c(4335,4426, 4427), population=c(10,20,19), x=c(-1,-1,-2), y=c(-5,-5,-6))
  testLocationData  <- as.location_dataframe(testLocationData)
  expect_true(consistencyCheckMovementMatrixLocationDataframe(testMatrix, testLocationData))
})

test_that("consistencyCheckMovementMatrixLocationDataframe returns correctly false for locationData with more entries than movement matrix", {
  testMatrix  <- matrix(c(0,1,2,0),nrow=2)
  testMatrix  <- as.movement_matrix(testMatrix)
  testLocationData <- data.frame(location=c(1,2,3), population=c(10,20,40), x=c(-1,-1,0), y=c(-5,-5,-4))
  testLocationData  <- as.location_dataframe(testLocationData)
  expect_false(consistencyCheckMovementMatrixLocationDataframe(testMatrix, testLocationData))
})

test_that("consistencyCheckMovementMatrixLocationDataframe returns correctly false for movement matrix with missing row/column names", {
  testMatrix  <- matrix(c(0,1,2,0),nrow=2)
  testMatrix  <- as.movement_matrix(testMatrix)
  testLocationData <- data.frame(location=c(1,2), population=c(10,20), x=c(-1,-1), y=c(-5,-5))
  testLocationData  <- as.location_dataframe(testLocationData)
  expect_false(consistencyCheckMovementMatrixLocationDataframe(testMatrix, testLocationData))
})

test_that("consistencyCheckMovementMatrixLocationDataframe returns correctly false for movement matrix with missing row names", {
  testMatrix  <- matrix(c(0,1,2,0),nrow=2)
  colnames(testMatrix) <- c("a","b")
  testMatrix  <- as.movement_matrix(testMatrix)
  testLocationData <- data.frame(location=c(1,2), population=c(10,20), x=c(-1,-1), y=c(-5,-5))
  testLocationData  <- as.location_dataframe(testLocationData)
  expect_false(consistencyCheckMovementMatrixLocationDataframe(testMatrix, testLocationData))
})

test_that("consistencyCheckMovementMatrixLocationDataframe returns correctly false for movement matrix with missing column names", {
  testMatrix  <- matrix(c(0,1,2,0),nrow=2)
  rownames(testMatrix) <- c("a","b")
  testMatrix  <- as.movement_matrix(testMatrix)
  testLocationData <- data.frame(location=c(1,2), population=c(10,20), x=c(-1,-1), y=c(-5,-5))
  testLocationData  <- as.location_dataframe(testLocationData)
  expect_false(consistencyCheckMovementMatrixLocationDataframe(testMatrix, testLocationData))
})

test_that("consistencyCheckMovementMatrixLocationDataframe returns false for non-matching locations in column names", {
  testMatrix  <- matrix(c(0,1,2,0),nrow=2, dimnames=list(c("a","b"),c("abc","def")))
  testMatrix  <- as.movement_matrix(testMatrix)
  testLocationData <- data.frame(location=c("abc","def"), population=c(10,20), x=c(-1,-1), y=c(-5,-5))
  testLocationData  <- as.location_dataframe(testLocationData)
  expect_false(consistencyCheckMovementMatrixLocationDataframe(testMatrix, testLocationData))
})

test_that("consistencyCheckMovementMatrixLocationDataframe returns false for non-matching locations in row names", {
  testMatrix  <- matrix(c(0,1,2,0),nrow=2, dimnames=list(c("abc","def"),c("a","b")))
  testMatrix  <- as.movement_matrix(testMatrix)
  testLocationData <- data.frame(location=c("abc","def"), population=c(10,20), x=c(-1,-1), y=c(-5,-5))
  testLocationData  <- as.location_dataframe(testLocationData)
  expect_false(consistencyCheckMovementMatrixLocationDataframe(testMatrix, testLocationData))
})

test_that("consistencyCheckMovementMatrixLocationDataframe returns false for non-matching locations ", {
  testMatrix  <- matrix(c(0,1,2,0),nrow=2, dimnames=list(c("a","b"),c("a","b")))
  testMatrix  <- as.movement_matrix(testMatrix)
  testLocationData <- data.frame(location=c("abc","def"), population=c(10,20), x=c(-1,-1), y=c(-5,-5))
  testLocationData  <- as.location_dataframe(testLocationData)
  expect_false(consistencyCheckMovementMatrixLocationDataframe(testMatrix, testLocationData))
})

test_that("toCamelCase correctly returns phrase when split by dot",{
  test_string  <- "this.text"
  expected_return  <- "thisText"
  actual_return  <- toCamelCase(test_string, "\\.")
  expect_equal(expected_return, actual_return)  
})

test_that("toCamelCase correctly returns phrase when split by space",{
  test_string  <- "this text"
  expected_return  <- "thisText"
  actual_return  <- toCamelCase(test_string, " ")
  expect_equal(expected_return, actual_return)  
})

test_that("toCamelCase correctly returns phrase when split by space with multiple words",{
  test_string  <- "this text is a dummy test for testing"
  expected_return  <- "thisTextIsADummyTestForTesting"
  actual_return  <- toCamelCase(test_string, " ")
  expect_equal(expected_return, actual_return)  
})

test_that("toCamelCase correctly returns phrase when split by underscore",{
  test_string  <- "this_text"
  expected_return  <- "thisText"
  actual_return  <- toCamelCase(test_string, "_")
  expect_equal(expected_return, actual_return)  
})

test_that("toCamelCase correctly returns phrase when split by underscore",{
  test_string  <- "this_text"
  expected_return  <- "thisText"
  actual_return  <- toCamelCase(test_string, "_")
  expect_equal(expected_return, actual_return)  
})

test_that("toCamelCase correctly returns phrase when split by dot using fixed option for split function",{
  test_string  <- "this.text"
  expected_return  <- "thisText"
  actual_return  <- toCamelCase(test_string, ".", fixed = TRUE)
  expect_equal(expected_return, actual_return)  
})

test_that("toCamelCase correctly returns phrase even when used by capital lettes",{
  test_string  <- "THIS TEXT"
  expected_return  <- "thisText"
  actual_return  <- toCamelCase(test_string, " ")
  expect_equal(expected_return, actual_return)  
})
