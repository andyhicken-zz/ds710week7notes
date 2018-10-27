install.packages("ggplot2movies")
library(ggplot2movies)
library(dplyr)
library(ggformula)

movies %>% summarise(n())
movies
View(movies)

# scatterplot! 
movies %>% 
  gf_point(length ~ year, col =~ budget) %>%
  gf_refine(scale_y_log10(), 
            scale_color_gradient(low = "red", high = "green")) %>%
  gf_smooth(length ~ year)


# create a subset of movies with only those with known budgets
knownBudget <- movies %>%
  filter(!is.na(budget))
View(knownBudget)
View(movies)
knownBudget %>% summarise(n())

# now plot it
knownBudget %>% 
  gf_point(length ~ year, col =~ log10(budget)) %>%
  gf_refine(scale_y_log10(), 
            scale_color_gradient(low = "red", high = "green")) %>%
  gf_smooth(length ~ year)

# Create a new variable, genre, to classify movies as action, comedy, or 
# other
knownBudget <- 
  knownBudget %>%
  mutate(genre = ifelse(Action, "action", 
                        ifelse(Comedy, "comedy", "other")))

knownBudget %>% select(Action, Comedy, genre)

knownBudget %>%
  filter(genre != "other") %>% 
  gf_point(length ~ year, 
           color =~ genre, alpha = .25) %>%
  gf_refine(scale_y_log10()) %>%
  gf_smooth(length ~ year) 

# make a histogram for movie lengths, one face for action and one for comedy
# align them vertically
knownBudget %>%
  filter(genre != "other") %>% 
  gf_histogram(~ log10(length), fill =~ genre, position = position_dodge()) 


# look at ratings
movies %>% gf_bar(~mpaa)

# find the average length of each movie with each rating
len_by_mpaa <- movies %>% 
  group_by(mpaa) %>% 
  summarise(ave_len = mean(length))


len_by_mpaa <- len_by_mpaa %>% arrange(ave_len)

len_by_mpaa

