library(readr)
library(dplyr)
library(ggformula)
library(stringr)

bb <- read.csv('babyboom.csv')
head(bb)

summary(bb$Sex)

scratch <- bb$Sex
summary(scratch)
scratch <- str_replace(scratch, "boy", "M")
summary(scratch)
scratch

SexToFM <- function(vectorOfStrings) {
  vectorOfStrings <- str_replace(vectorOfStrings, "boy", "M")  
  vectorOfStrings <- str_replace(vectorOfStrings, "Male", "M")  
  vectorOfStrings <- str_replace(vectorOfStrings, "female", "F")
  vectorOfStrings <- str_replace(vectorOfStrings, "girl", "F")
  vectorOfStrings <- str_replace(vectorOfStrings, "not recorded", "NA")   
  return(vectorOfStrings)
}

scratch <-  SexToFM(bb$Sex)

female = c("F", "female", "girl")

attach(bb)
Sex %in% female


SexToFM2 <- function(Strings) {
  # Create a new function, SexToFM2, that uses the %in% function 
  # to replace each string by "F", "M", or NA using only 6 lines of code.
  female = c("F", "female", "girl")
  male = c("boy", "Male", "M")
  # blah = c("not recorded", "etc.")
  
  Strings[Strings %in% female] <- "F"
  Strings[Strings %in% male] <- "M"
  Strings[Strings %in% c("not recorded")] <- NA
  
  # set any empty strings to NA  
  Strings[which(Strings == "")] <- NA
    
  return(Strings)
}

cleanSex <-  SexToFM2(Sex)


cbind(as.character(Sex), as.character(cleanSex))

bb$Sex <- cleanSex

# find out what kind of variable Time is
summary(Time)
class(Time)

# make it be numeric
Time2 <- as.numeric(Time)
Time2
cbind(Time, Time2)

Time[1:5]
Time2[1:5]
Time

# the above shows a DANGEROUS PITFALL when reading data into R.
# due to the text values mixed in with integers, Time was read in as categorical.
# R then assigns integers to each factor in the categorical data. 
# ergo, you can end up working with data that seems "wrong."

# here's the none-too-elegant recommended way to convert factors to numeric:
Time3 <- as.numeric(levels(Time))[Time]
Time3
Time

# also possible, and equivalent in this case, at least
Time4 <- as.numeric(levels(Time))[as.integer(Time)]
Time4


bb$Time <- Time3

# examine Time for bad values
summary(Time)

summary(bb)
# histogram
gf_histogram(~ Weight,  data = bb)
gf_histogram(~ Time,  data = bb)

gf_boxplot(~ Time,  data = bb)


# some values for Time are greater than 2400. Find them.
which(Time > 2400)
# row 26. Look at it. 
bb[26,]

# set to NA
bb$Time[26] <- NA
bb[26,]


# check for decimal times
attach(bb)
trunc(Time) == Time
which(trunc(Time) != Time)

# check for times where the minute value is >59
Time %% 100
which ((Time %% 100) > 59)

# value 14 has an issue apparently
bb[14,]

# yep
bb[14,]$Time <- NA

bb[14,]

# now look at Weight
summary(Weight)

bb[which(Weight < 1000),]

gf_histogram(~Weight, data=bb)
gf_boxplot(~Weight, data=bb)

bb[which(Weight < 1000),] <- NA
# NOTE: the above wasn't really the right way to do it; oops

summary(bb$Time)
which( is.na(bb$Time))


# create a variable that contains only the non-missing values of Time

Time5 <- bb$Time[which( !is.na(bb$Time) )]
summary(Time5)
length(Time5)
length(bb$Time)
summary(bb$Time)

sd(Time5)

sd(bb$Time, na.rm = T)

Time <- bb$Time
Time
Time[14]

summary(bb$Sex)
summary(bb$Time)
summary(bb$Weight)

bb_clean <- na.omit(bb)

summary(bb_clean)
bb_clean
length(bb_clean$Sex)


# get rid of them old variables they're confusing

rm(Time, Sex, Weight)

length (which(bb_clean$Time < 1200))

# create a new column indicating whether a baby was born in the morning
bb_clean <- bb_clean %>% mutate( morning_baby = Time < 1200) 
summary(bb_clean$morning_baby)

sort( bb_clean$Time )


# sort the whole data frame in order by time 
bb_clean <- arrange( bb_clean, Time )
head(bb_clean)
