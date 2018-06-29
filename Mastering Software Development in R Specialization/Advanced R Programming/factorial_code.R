library(purrr)
library(microbenchmark)

# Part 1: Factorial Function
# Version 1:
Factorial_loop <- function(x) {
  if (x == 0)
    return (1)
  
  res <- 1
  for (num in 1:x) {
    res = res * num
  }
  return (res)
}
a <- Factorial_loop(5)

# Version 2:
Factorial_reduce <- function(x) {
  if (x == 0)
    return (1)
  
  return(Reduce(`*`, 1:x))
}
b <- Factorial_reduce(5)

# Version 3:
Factorial_func <- function(x) {
  if (x == 0)
    return (1)
  
  return (Factorial_func(x - 1) * x)
}
c <- Factorial_func(5)

# Version 4:
Factorial_mem <- function(x) {
  if (x == 0)
    return (1)
  if (x == 1)
    return (1)
  
  arr <- c(rep(1, x))
  for (num in 2:x) {
    arr[num] <- arr[num - 1] * num
  }
  return (arr[length(arr)])
}
d <- Factorial_mem(5)

# use the microbenchmark package to time the operation of these functions and provide a summary of their performance
# factorial_output.txt: a text file that contains the results of your comparison of the four different implementations.
sink("factorial_output.txt")

get_benchmark <- function(x) {
  microbenchmark(map_dbl(x, Factorial_loop),
                 map_dbl(x, Factorial_reduce),
                 map_dbl(x, Factorial_func),
                 map_dbl(x, Factorial_mem))
}

ranges <- list(`range 1:10` = 1:10,
               `range 1:50` = 1:50,
               `range 1:100` = 1:100)

range_results <- map(ranges, get_benchmark)
range_results

sink()
