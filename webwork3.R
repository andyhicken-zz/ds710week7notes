PearsonSkew <- function(x, ...) 
{
  skewness <- (3 * (mean(x, ...) - median(x, ...))) / (sd(x, ...))
  
  lvsr = ""
  
  if (skewness > 0) {
    lvsr = "right-skewed"
  } else if (skewness < 0) {
    lvsr = "left-skewed"
  } else {
    lvsr = "not skewed"
  }
  
  return(c(skewness, lvsr))
    
}

library(readr)

ames <- read_csv("AmesHousing.csv")

PearsonSkew(ames$`Lot Area`)

ames2 <- data.frame(ames[,45:54])

apply(ames2, 2, PearsonSkew, na.rm=T)