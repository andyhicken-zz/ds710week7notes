# Write a for loop to create a vector containing the first 10 powers of 3. Pre-allocate the memory you will use

tri <- function() {
  powersOf3 = numeric(10)

  for (i in 1:10) {
    powersOf3[i] = 3**i
  }
  
}

# try doing the same using vector calculations

tri2 <- function() {
  powersOfThree = numeric(10)
  powers = c(1:10)
  powersOfThree = 3**powers 
  return(powersOfThree)
}

library(microbenchmark)

microbenchmark(tri(), tri2())
