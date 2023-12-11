# simulate birth rates
simulate_seasonal_birth_rates <- function(time_points, baseline_rate, amplitude, period, phase_shift = 0) {
  # Create a sine wave with the specified amplitude, period, and phase shift
  seasonal_effect <- amplitude * sin(2 * pi * time_points / period + phase_shift)
  
  # Simulate birth rates by combining the baseline rate with the seasonal effect
  birth_rates <- baseline_rate * (1 + seasonal_effect)
  
  # Ensure birth rates are non-negative
  birth_rates[birth_rates < 0] <- 0
  
  # Return the simulated birth rates
  return(birth_rates)
}