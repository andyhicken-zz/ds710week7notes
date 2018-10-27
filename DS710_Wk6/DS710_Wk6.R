install.packages("dplyr")
library(dplyr)


install.packages("ggformula")
library(ggformula)

library(readr)
cps = read_csv("cps.csv")
head(cps)

# the dplyr pipe symbol is %>%
# example:

mySummary <- 
  cps %>% 
  summarise(meanWage = mean(wage), numObs = n() )

mySummary

# n() is a summary statistic that just works with summarise.
# it finds the number of observations
# here are some more functions like that: 
# min, max, sum, sd, median, quantile, IQR (interquartile range), n_distinct,
# first, last, nth, any, all


# here's nth(). It's going to return the 3rd entry in the wage column.
cps %>% summarise(nth(wage, 3))
head(cps)
# yep, that works. 

# quantile returns the %th value in a column.
# e.g., finding the 40th percentile value for wage:

cps %>% summarise(quantile(wage, .4))


# any() and all() are logical. 
# any() if any values match the condition
# all() if all do. 

cps %>% summarise(all(wage >1))
# that said false. Really?
cps %>% summarise(min(wage))
# yep. 

# now, the union column tells you whether a person is in a union or not. 
# this will give you two rows, one for each of the two values found in
# the union column
cps %>% 
  group_by(union) %>% 
  summarise(mean = mean(wage))
# basically a pivot table. 

# we can use filter() to select a subset of rows. Like a data filter.
YoungCollege <- 
  cps %>%
  filter(age < 30 & educ > 12)

# how many people are left in the YoungCollege dataframe? 
# I know how to do that.
YoungCollege %>% summarise(n())
# 72. 

# select() will just grab a few columns.
cpsSimple <- 
  cps %>%
  select(wage, age, sector)

# mutate() creates new columns using expressions! 
cps <- 
  cps %>%
  mutate(EducExper = educ + exper)

summary(cps$EducExper)

# you can use ifelse() inside mutate() to create categorical variables
cps <- 
  cps %>% 
  mutate(HSgrade = ifelse(educ >= 12, "HS", "No grad"))

# then we can check that it worked:
cps %>% select(educ, HSgrade)

# add an inflation-adjusted wage column, wage_ai
cps <- 
  cps %>%
  mutate(wage_ai = wage * 2.37)

cps %>% select(wage, wage_ai)

# find the max of that 
cps %>% summarise(max(wage_ai))

# or, fancily doing it all in one line with pipes:
cps %>% 
  mutate(wage2018 = wage * 2.37) %>%
  summarise(max(wage2018))

# group people into three different categories, A-B-C using case_when();
# note the nonsensical tilda. 
# whichever expression evaluates true first gets executed
cps <- 
  cps %>%
  mutate(AdCampaign = case_when(
    age < 30 ~ "A",
    sector == "sales" | sector == "service" ~ "B",
    TRUE ~ "C"
  ) )

cps %>% select(age, sector, AdCampaign)        

# here's a variation that assigns an NA to what was group C
# - it has to be a string because the other two are strings, thus
# the _character_
cps <- 
  cps %>%
  mutate(AdCampaign = case_when(
    age < 30 ~ "A",
    sector == "sales" | sector == "service" ~ "B",
    TRUE ~ NA_character_
  ) )


# here's how to do the equivalent of really long "ors" using
# "the logical in", %in% 
sector %in% c("sales", "service", "clerical", "manuf") 
# (that would replace the sector statements above)



# OK, now we're moving to a hypothetical discussion of time series
FakeSales = data.frame(year = 2010:2015, sales = (0:5)^2)
FakeSales

# lag() and lead() shift every observation later and earlier, respectively:
FakeSales <- 
  FakeSales %>% 
  mutate(previous = lag(sales),
        future = lead(sales))

FakeSales
# you can shift more than one place by adding an n=, like 
# lag(sales, n=2)

# here's a weird trick to arbitrarily mix up the order of the rows
FakeSales = FakeSales[c(1, 4, 3, 5, 6, 2),]

FakeSales

# despite that weirdness, we can still get the lag to execute 
# using order_by
FakeSales <- 
  FakeSales %>% 
  mutate(previous = lag(sales, order_by = year))

FakeSales

# and this will just unscramble everything
FakeSales <- FakeSales %>% arrange(year)

FakeSales

# so arrange is like sort. 
# Here's sorting by the median education of each sector.
cps %>% 
  group_by(sector) %>% 
  summarise(med = median(educ)) %>%
  arrange(med)

# OK, for demo purposes, building a new dataframe with no clerical sector
wage_df <- 
  cps %>% 
  group_by(sector) %>%
  summarise(meanWage = mean(wage)) %>%
  filter(sector != "clerical")

wage_df

# here's an inner join,
# returns all columns of x and y 
# all rows of x that have matches in y 
# if a row of x has multiple matches in y then it's duplicated
# by default, matching is done on all columns that have the same names
cpsInner <- inner_join(x = cpsSimple, y = wage_df)
# ^both of the above havea column "sector," so the match was done on that.
# you can add a by="sector" to specify it. 
# if they don't have the same name, you can do something like 
# by = c("sector" = "job")

# other joins: left_join returns all the rows of x whether or not 
# there's a match in y
cpsLeft <- left_join(x = cpsSimple, y = wage_df)

cpsLeft



# ----------------------------
# GGFORMULA BEGINS HERE
# ----------------------------

# generally, y is on the left off the tilda and x's on the right:
# y ~ x1, x2, x3

# here's a linear regression.
lm(wage ~ educ, data = cps)

# scatterplot
gf_point(wage ~ educ, data = cps)

# here's how to add various labels: you pipe the graph to gf_labs()
# the pipe symbol goes with ggformula - happens to be the same as in dplyr


gf_point(wage ~ educ, data = cps) %>% 
  gf_labs(title = "Wage as a function of education",
          subtitle = "1985 data",
          x = "Education (years)",
          y = "Wage (%/hour)",
          caption = "Source: 1985 Current Population Survey" )

# gg comes from Grammar of Graphics
# glyph: one graphical unit representing one data point 
# ("one case of the data")
# aesthetic: how the glyph looks (e.g., points vs. diamonds)
# scale: a mapping between aesthetics and value of a variable 
# (such as Republican states are red and Democratic states are blue)

# adding a color to some graph is often col = "whatever"
gf_point(wage ~ educ, col = "blue", data = cps) %>% 
  gf_labs(title = "Wage as a function of education",
          subtitle = "1985 data",
          x = "Education (years)",
          y = "Wage (%/hour)",
          caption = "Source: 1985 Current Population Survey" )

# mapping color to a variable adds a tilda (as a function of)
gf_point(wage ~ educ, col =~ age, data = cps) %>% 
  gf_labs(title = "Wage as a function of education",
          subtitle = "1985 data",
          x = "Education (years)",
          y = "Wage (%/hour)",
          caption = "Source: 1985 Current Population Survey" )

# to spec the gradient colors, pipe it all into gf_refine, like this:
gf_point(wage ~ educ, col =~ age, data = cps) %>% 
  gf_labs(title = "Wage as a function of education",
          subtitle = "1985 data",
          x = "Education (years)",
          y = "Wage (%/hour)",
          caption = "Source: 1985 Current Population Survey" ) %>%
  gf_refine(scale_color_gradient(low = "brown", high = "gray"))

# here's specing colors for categorical variables with scale_color_manual
# and also adding a label for the color scale key
# and adding a shape
# and a size
# and an alpha, which makes things transparent
gf_point(wage ~ educ, shape =~ race, size =~ exper, alpha = .5, col =~ union, data = cps) %>%
  gf_refine(scale_color_manual(values = c("red", "blue"))) %>%
  gf_labs(color = "Union member?")

# adding smoothing curves to a scatterplot:
gf_point(wage ~ educ, col =~ sex, data = cps) %>% 
  gf_smooth(wage ~educ, col =~ sex)


# facets are subgraphs. One version takes your data and divides it into
# two buckets based on a categorical variable.
# Then you graph the same thing for each.
gf_point(wage ~ educ | union, data = cps) %>% 
  gf_smooth(wage ~ educ)

# you can use gf_facet wrap with a pipe to do this, too: 
gf_point(wage ~ educ, data = cps) %>% 
  gf_facet_grid(sector ~ union) %>% 
  gf_smooth(wage ~ educ)

# you can make your buckets based on two variables with a facet grid: 
gf_point(wage ~ educ | union, data = cps) %>% 
  gf_facet_grid(sex ~ union) %>% 
  gf_smooth(wage ~ educ)
# (going before the squiggle means you're on the vertical axis - you're y)

# suddenly we're careening into histograms
gf_histogram(~ wage, data = cps) %>% 
  gf_facet_grid(union ~ .)

# here's how to do that with a single plot:
gf_histogram(~wage, fill =~ union, data = cps)

# same thing but with the bars "dodging" each other
gf_histogram(~wage, fill =~ union, position = position_dodge(), data = cps)
# ^hmm, pretty. 

# here's a bar chart with the counts of a categorical variable: 
gf_bar(~ sector, fill =~ union, data = cps)

# what about bar charts where you don't want a count, but 
# bars whose height is determine by another variable? 
# like let's make a dataframe of mean wage by sector:
sector_df <- 
  cps %>% 
  group_by(sector) %>%
  summarise(meanWage = mean(wage))
View(sector_df)

gf_col(meanWage ~ sector, data = sector_df)

# sort the columns in order by wage:
sector_df %>% 
  mutate(sector = reorder(sector, meanWage)) %>% 
  gf_col(meanWage ~ sector)
