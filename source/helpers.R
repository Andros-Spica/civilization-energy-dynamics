get_standardise_differences <- function(variable) {
  differences <- (calculate_differences(variable))
  standardise_differences <- differences / diff(range(differences))
  return(standardise_differences)
}

calculate_differences <- function(x) {
  n <- length(x)
  diffs <- numeric(n)
  diffs[1] <- 0  # Set the first value to 0
  diffs[2:n] <- diff(x)  # Calculate differences for the rest
  return(diffs)
}
