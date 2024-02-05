library(doParallel)
library(foreach)

runParallelODE <- function(climate_list, xstart, times, model_function, params) {
  # Set up parallel processing with doParallel on PC
  cl <- makeCluster(detectCores())
  # for cluster, needs to match #SBATCH cpus per task
  # cl <- makeCluster(detectCores(20))
  registerDoParallel(cl)
  
  
  
  # Define the inner function for parallel execution
  runODE <- function(climate_var, params, model_function) {
    params$climate_var <- climate_var
    as.data.frame(
      ode(
        xstart,
        times,
        model_function,
        params
      )
    )
  }
  
  # Run the parallel foreach loop with explicit export of necessary variables
  out_list <- foreach(
    climate_var = climate_list,
    .packages = c("deSolve")
  ) %dopar% {
    runODE(climate_var, params, model_function)
  }
  
  stopCluster(cl)  # Stop the parallel processing cluster
  
  # add list element names
  names(out_list) <- names(climate_list)
  
  # return result
  return(out_list)
}
