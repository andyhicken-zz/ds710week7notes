# efficiency has three aspects:
# time to write the code
# time to run the code
# amount of memory used: you can save time by storing more data in memory, but you wouldn't 
#   want to do this with large data files that consume computer's memory.

# strategies:
# avoid repetition
# vector calculations > for loops
# use existing functions
# plan ahead for memory use

# here's an example of avoiding a loop inherent in Which by saving the indices it returns:

goodCar <- which(Mileage == m)
print(Price[goodCar])
print(Mileage[goodCar])

# vector calculations are like this:
x = c(1, 4, 9, 16, 25, 36, 49, 64, 81, 100)
y = sqrt(x)

# timing code microbenchmmark
install.packages("microbenchmark")
library(microbenchmark)

# just wrap your function calls in microbenchmark():
microbenchmark(sqrt(x), MySqrt(x))
# the 'cld' column retruned here will tell you if there's a statistically significant
# difference between the two functions. If two functions have the same letter, they 
# are statisically identical. 


vSumFirstNElements <- function(aVector, n) {
  
  # Return a vector where the first element is the sum of the first `n`
  # elements of the input vector, and the rest of the vector is a copy of
  # the other elements of the input vector.
  
  # This is the one-line version.
  
  if (n < 1){
    return(aVector)
  } else {
    return ( c(sum(aVector[1:n]), aVector[n+1:(length(aVector)-n)]) )
  }  
  
}

vSumFirstNElementsLoop <- function(aVector, n) {
  
  # Return a vector where the first element is the sum of the first `n`
  # elements of the input vector, and the rest of the vector is a copy of
  # the other elements of the input vector.
  
  # This is the loop-based version.
  sumN = 0
  
  for(ii in 1:n){ 
    sumN = sumN + aVector[ii]  
  }
  
  return ( c(sumN, aVector[n+1:(length(aVector)-n)]) )
  
}

# here is some data from assignment 7:
x <- c(3, 7, 8, 3, 7, 3, 3, 6, 2, 3, 3, 2, 3, 4, 3, 8, 10, 2, 3, 3, 7, 4, 2, 10, 6, 3, 4, 9, 3, 6, 4, 2, 4, 2, 6, 4, 3, 7, 5, 2, 5, 4, 8, 7, 4, 2, 6, 4, 4, 3, 3, 7, 2, 7, 3, 4, 2, 11, 2, 6, 5, 4, 8, 2, 3, 7, 2, 4, 6, 4, 3, 5, 6, 2, 3, 5, 5, 5, 5, 6, 5, 4, 8, 8, 7, 2, 3, 8, 7, 2, 3, 6, 3, 6, 2, 3, 9, 3, 6, 4, 3, 3, 7, 3, 5, 2, 9, 3, 8, 8, 2, 6, 4, 3, 4, 5, 2, 3, 3, 4, 2, 7, 5, 6, 8, 4, 3, 7, 6, 6, 5, 2, 3, 6, 12, 6, 6, 2, 5, 5, 5, 6, 2, 5, 2, 3, 1, 7, 6, 3, 5, 4, 4, 1, 6, 3, 1, 7)

length(x)

microbenchmark(vSumFirstNElements(x, 75), vSumFirstNElementsLoop(x, 75))

library(ggformula)
# in those results, lq and uq are the lower and upper quartile. You can view this boxplot like this:
timing = microbenchmark(vSumFirstNElements(x, 75), vSumFirstNElementsLoop(x, 75))
gf_boxplot(time ~ expr, data = timing)

# exception to the rule that existing R functions are faster than home-built ones:
# existing functions with many different versions or arguments.
# they can be more about coding efficiency. 

# the Rcpp package is an interface for writing your own functions in C++ 

# PLAN AHEAD FOR THE SIZE OF A VARIABLE
# each time you append to a variable, R has to find more space. 

# example of code that increases the size of a variable on the fly:

doubling <- function(seed, n) {
      x = seed 
      for (i in 2:n) {
        x = c(x, 2*x[i-1])
      }
      return(x)
}
# example of code that grabs all its space at once
doubling_fast <- function(seed, n) {
  x = numeric(n) # initializes a 10-length numeric vector
  x[1] = seed
  for(i in 2:n) {
    x[i] = 2*x[i-1]
  }
  return(x)
}

doubling(1, 10)
doubling_fast(1, 10)


microbenchmark(doubling(1,1000), doubling_fast(1,1000))


# ways to preallocate memory:
# numeric(L) or vector(L)
# maxtrix(, nr, nc)
# read_csv("whatever", n_max = 804)