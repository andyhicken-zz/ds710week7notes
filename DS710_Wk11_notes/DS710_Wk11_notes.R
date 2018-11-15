library(readr)
library(dplyr)
library(ggformula)
library(microbenchmark)
library(beepr)

# ---EFFICIENCY WHEN WORKING WITH LARGE DATA SETS---
# use rm() to clean up variables when done with them

ls() # lists variables currently in memory

# rm (list = ls()) # delete all the variables

gc() # garbage collection: free up memory from deleted variables

# you can also free up memory by using the base R GUI rather than R studio

# here's a trick to print progress to the screen. 

for (i in 1:10000) {
  # yada yada yada
  if (i %% 100 == 0) {
    print(i)
    flush.console() # you have to flush the buffer or it might not work. 
  }
}


# here's another version that saves the shit to disc every 100 things. 

somevals <- numeric(100)

for (i in 1:10000) {
  # yada yada yada
  if (i %% 100 == 0) {
    print(i)
  }
  
  somevals[i %% 100 ] <- i # so we are only saving 100 vals at a time to memory
  
  if (i %% 100 == 0) {
    write_csv(data.frame(somevals), "somevals.csv", append = T) # the append is obviously key. 
    
    # or you could save 100 values at a time, like this:
    filename <- paste("somevals", i-99, "_to_", i, ".csv", sep = "")
    write_csv(data.frame(somevals), filename) 
  }
  
  
}


# another good way to save files is as .RData, a compressed file format unique to R
# especially good if you want to save multiple variables
save(somevals, i, filename, file = "somevals.RData") 
# you can save whatever variables this way.
# - helpful if they don't fit together as one dataframe


# this will beep when reached. You actually have to have a blank line beneath it. 
alarm()
# it might not be working because of Windows.


# there's also this which is in the beepr library:

beep(2)


# libraries that will tweet at you when your analysis is ready:
# Link: http://www.r-bloggers.com/r-job-notifications-using-twitter/
# Link: https://cran.r-project.org/web/packages/twitteR/twitteR.pdf


# recommended libraries for large files: ff, bigmemory