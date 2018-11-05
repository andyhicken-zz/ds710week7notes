# Example: is there a difference between the proportion of unemployed people in WI and MN? 

prop.test( c(19, 17), n = c(500, 400), alternative = "two.sided")


# The CDC recommends that adults get at least 150 minutes of moderate-intensity exercise per week. 
# You suspect that people in your city typically do not meet this recommended amount, 
# so you gather data to test your suspicions. You ask 70 people in your city to wear an activity monitor
# for a week, and record the number of minutes of exercise each person does in the vector minutes. 

# 44% of Americans have blood type O; 42% have type A; 10% have type B; and 4% have type AB.
# A random sample of 100 people at your local blood donation center reveals the following counts:
#  O 	A 	B 	AB
# 43 	44 	11 	2

propBlood = c(.44, .42, .1, .04)
bloodTypeCount = c(43, 44, 11, 2)

chisq.test(bloodTypeCount, p = propBlood)
