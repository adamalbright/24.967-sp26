# The Maine automobile accidents example
# Agresti (2002), p. 327
gender = c("female", "female", "female", "female", "female", "female", "female", "female", "male", "male", "male", "male", "male", "male", "male", "male")
location = c("urban", "urban", "urban", "urban", "rural", "rural", "rural", "rural", "urban", "urban", "urban", "urban", "rural", "rural", "rural", "rural")
seatbelt = c("no", "no", "yes", "yes", "no", "no", "yes", "yes", "no", "no", "yes", "yes", "no", "no", "yes", "yes")
injury = c("no", "yes", "no", "yes", "no", "yes", "no", "yes", "no", "yes", "no", "yes", "no", "yes", "no", "yes")
count = c(7287, 996, 11587, 759, 3246, 973, 6134, 757, 10381, 812, 10969, 380, 6123, 1084, 6693, 513)

accidents = data.frame(cbind(gender,location,seatbelt,injury,count))
accidents$count = as.integer(as.character(accidents$count))

# Agresti: "It is natural to treat injury (I) as a response variable and gender (G), location (L), and seat-belt use (S) as explanatory variables".  That is, we wish to predict whether or not there was an injury, based on the remaining factors.  That is a binomial 

# To run a logistic regression with glm, we can reshape to get a "yes" count and "no" count
accidents2 = reshape(accidents, v.names="count", timevar="injury",idvar=c("gender","location","seatbelt"),direction="wide")

# glm assumes that we model (successes, failures)
accidents.lr = glm(cbind(count.yes,count.no) ~ gender + location + seatbelt, data=accidents2, family=binomial(link = "logit"))

# Exercise: interpret the intercept

# Now we examine the relation between logit and loglinear models. 
# In order to do this, it's useful to start more simply, with a model that has just one factor 

# Look at the data
xtabs(count~injury+seatbelt,data=accidents)
# Fit a logistic regression for probability of injury
accidents.s.lr = glm(cbind(count.yes,count.no) ~ seatbelt, data=accidents2, family=binomial(link="logit"))
summary(accidents.s.lr)

# A simple equivalent: model the counts of "injury-yes"
accidents.s.lm = glm(count~injury*seatbelt, data=accidents, family=poisson(link="log"))
summary(accidents.s.lm)

# Now do the same, but with two factors
accidents.gl.lr = glm(cbind(count.yes,count.no) ~ gender + location, data=accidents2, family=binomial(link="logit"))
summary(accidents.gl.lr)

accidents.gl.lm = glm(count~injury*gender + injury*location + gender*location, data=accidents, family=poisson(link="log"))
summary(accidents.gl.lm)

# Now for the full model, which had gender, location, and seatbelt. (repeated)
accidents.gls.lr = glm(cbind(count.yes,count.no) ~ gender + location + seatbelt, data=accidents2, family=binomial(link = "logit"))
summary(accidents.gls.lr)

accidents.gls.lm = glm(count ~ injury*gender + injury*location + injury*seatbelt + gender*location*seatbelt, data=accidents, family=poisson(link="log"))
summary(accidents.gls.lm)



# Finally, try one with an interaction
accidents.gls.interact.lr = glm(cbind(count.yes,count.no) ~ gender + location*seatbelt, data=accidents2, family="binomial")
summary(accidents.gls.interact.lr)

# The equivalent loglinear model
accidents.gls.interact.lm = glm(count~ injury*gender + injury*location*seatbelt + gender*location*seatbelt, data=accidents, family=poisson(link="log"))
summary(accidents.gls.interact.lm)