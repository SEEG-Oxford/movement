library(movement)
context("Visualisation methods")

test_that("plot.prediction_model passes correct parameters to show.prediction", {
	movement_predictions = list(net="network",movement_matrix="move")
  class(movement_predictions)  <- "movement_predictions"
	with_mock(`movement:::show.prediction` = function(x,y,...) return(paste(x,y,...,sep=',',collaspe=',')),
            actual_predictions  <- plot(movement_predictions),
            expect_equal(actual_predictions, "network,move,,"))
})

test_that("summary.movement_model returns a summary.movement_model object", {
  dummy_training_results  <- list(flux_model = list(params = "flux_params"))
  dummy_optimisation_results  <- list(hessian = matrix(c(1,2,3,4), nrow = 2), optimised_params = "optimised_params")
  dummy_movement_model <- list(optimisation_results = dummy_optimisation_results,
                              training_results = dummy_training_results,
                              coefficients = "coefficients",
                              df_null = "df_null",
                              df_residual = "df_residual", 
                              null_deviance = "null_deviance",
                              deviance = "deviance",
                              aic = "aic") 
  class(dummy_movement_model)  <- 'movement_model' 
  actual_summary_model  <- summary(dummy_movement_model)
  expect_is(actual_summary_model, "summary.movement_model")  
})

test_that("summary.movement_model returns a correct summary.movement_model object", {
  dummy_flux_model  <- list(name = "flux_model_name", params = "flux_params")
  dummy_training_results  <- list(flux_model = dummy_flux_model)
  dummy_hessian_matrix  <- matrix(c(1,2,3,4), nrow = 2)
  dummy_optimisation_results  <- list(hessian = dummy_hessian_matrix, optimised_params = "optimised_params")
  dummy_movement_model <- list(optimisation_results = dummy_optimisation_results,
                               training_results = dummy_training_results,
                               coefficients = "coefficients",
                               df_null = "df_null",
                               df_residual = "df_residual", 
                               null_deviance = "null_deviance",
                               deviance = "deviance",
                               aic = "aic") 
  class(dummy_movement_model)  <- 'movement_model'   
  
  expected_summary_model = list(model = dummy_flux_model,
                                deviance_resid = 1,
                                coefficients = "flux_params",
                                null_deviance = "null_deviance",
                                resid_deviance = "deviance",
                                aic = "aic",
                                df_null = "df_null",
                                df_residual = "df_residual", 
                                trans_coeff = "optimised_params",
                                trans_coeff_std_errors = sqrt(abs(diag(solve(dummy_hessian_matrix))))
                                )
  class(expected_summary_model)  <- 'summary.movement_model'

  actual_summary_model  <- summary(dummy_movement_model)  
  expect_equal(actual_summary_model, expected_summary_model)  
})

test_that("summary.movement_model returns NA value for stderror is hessian cannot be used to calculate a std error", {
  dummy_flux_model  <- list(name = "flux_model_name", params = "flux_params")
  dummy_training_results  <- list(flux_model = dummy_flux_model)
  dummy_hessian_matrix  <- matrix(c(1,1,1,1), nrow = 2)
  dummy_optimisation_results  <- list(hessian = dummy_hessian_matrix, optimised_params = "optimised_params")
  dummy_movement_model <- list(optimisation_results = dummy_optimisation_results,
                               training_results = dummy_training_results,
                               coefficients = "coefficients",
                               df_null = "df_null",
                               df_residual = "df_residual", 
                               null_deviance = "null_deviance",
                               deviance = "deviance",
                               aic = "aic") 
  class(dummy_movement_model)  <- 'movement_model'   
  
  actual_summary_model  <- summary(dummy_movement_model)
  expect_true(is.na(actual_summary_model$trans_coeff_std_errors))
})

test_that("summary.movement_model prints a message to the user of function cannot calculate a std error", {
  dummy_flux_model  <- list(name = "flux_model_name", params = "flux_params")
  dummy_training_results  <- list(flux_model = dummy_flux_model)
  dummy_hessian_matrix  <- matrix(c(1,1,1,1), nrow = 2)
  dummy_optimisation_results  <- list(hessian = dummy_hessian_matrix, optimised_params = "optimised_params")
  dummy_movement_model <- list(optimisation_results = dummy_optimisation_results,
                               training_results = dummy_training_results,
                               coefficients = "coefficients",
                               df_null = "df_null",
                               df_residual = "df_residual", 
                               null_deviance = "null_deviance",
                               deviance = "deviance",
                               aic = "aic") 
  class(dummy_movement_model)  <- 'movement_model'   
  
  expect_warning(summary(dummy_movement_model), "ERROR while calculating the standard error:")
})
