whichDiff <- function(successes, total, alpha = .05, corrected = TRUE){
  # Conducts multiple 2-sample tests of proportions to determine which pairs of categories have significantly different probabilities of success.
  # successes should be a vector of the number of successes in each category.
  # total should be a vector of the total number of attempts in each category.  It should have the same length as successes, and the categories should be listed in the same order as in successes.
  # alpha is the significance level.
  # If corrected = TRUE, the function performs a Bonferroni correction to the significance level to maintain a familywise error rate of not more than alpha.
  
  # Returns NULL if there are no significantly different pairs.
  # Otherwise, returns a matrix in which each column gives the indices of 2 categories which are significantly different.

  # Initialize matrix of significantly different pairs
  diffPairs = matrix( , nr = 2, nc = 0)
  
  n = length(successes)
  alpha_fixed = alpha
  if(corrected){
    # Bonferroni-corrected significance level
    # to account for multiple testing
    alpha_fixed = alpha/(n*(n-1)/2)
  }
  
  # Loop over all pairs of categories for 1 variable
  for(ii in 2:n){
    for(jj in 1:(ii-1)){
      successPair = successes[c(ii,jj)]
      totalPair = total[c(ii,jj)]
      result = prop.test(successPair, totalPair)
      
      if(result$p.value < alpha_fixed){
        # This pair is significantly different,
        # so add it to the matrix of differences
        diffPairs = cbind(diffPairs, c(jj, ii))
      }
    } # end iteration over jj
  } # end iteration over ii
  
  if(dim(diffPairs)[2] > 0){
    return(diffPairs)
  }
  else{
    return(NULL)
  }
} # end of function whichDiff