# read in the CVC data
CVC = read.csv("engCVC.csv")

#remove rows with missing values
CVC = filter(CVC, !is.na(F2C1))

# declare factors
CVC$C1 = factor(CVC$C1)
CVC$V = factor(CVC$V)
CVC$C2 = factor(CVC$C2)
CVC$rate = factor(CVC$rate)
CVC$speaker = factor(CVC$speaker)

# plot F2C1 against F2V for words where C1 is [b]

# Base R - pch sets the plot symbol. Here we use different plot symbols for each speaker
plot(F2C1~F2V, data=subset(CVC, C1=="b"), pch=as.numeric(CVC$speaker))

# plot with ggplot2
library(tidyverse)

# scatter plot
spb = ggplot(filter(CVC, C1=="b"), 
             aes(x=F2V, y = F2C1, shape=speaker, col=speaker)) + 
  geom_point()

print(spb)

# make this look less ugly
# theme_bw(base_size=14) eliminates grey background and increases font sizes
# the additional theme function eliminates the grid lines, retaining lines at
# the top and right of the graph
spb = spb +
  theme_bw(base_size=14)+
  theme(axis.line = element_line(color='black'),
      plot.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank())
# or
spb = spb +
  theme_classic(base_size=14)

# fit a linear model to these data
lmod = lm(F2C1~F2V, data = filter(CVC, C1=="b"))

# examine the model
summary(lmod)

# add best fitting line to the scatterplot - two methods

# 1. have ggplot calculate the regression line

# a. aes(col=speaker, shape=speaker) has to be moved to geom_point() 
# otherwise geom_smooth will fit a separate line to each speaker
spb_line = ggplot(filter(CVC, C1=="b"), 
            aes(x=F2V, y = F2C1)) + 
            geom_point(aes(col=speaker, shape=speaker)) +
            theme_classic(base_size = 14)

# b. add linear fit using geom_smooth()
spb_line = spb_line + geom_smooth(method = "lm", se=FALSE)

# 2. Or extract the slope and intercept from the fitted lm model and use 
# those to plot the line - ensures we are plotting our model
spb_line = spb_line + geom_abline(slope=coef(lmod)[["F2V"]], intercept = coef(lmod)[["(Intercept)"]])

print(spb_line)

# fit multiple regression model
lmm = lm(F2C1~F2V+F2C2, data = filter(CVC, C1=="b"))

lmm2 = lm(F2C2~F2V+F2C1, data = subset(CVC, C1=="b"))


# fit multiple regression model with categorical predictor
lmc = lm(F2C1~F2V+rate, data = subset(CVC, C1=="b"))

# add interaction terms
lmci = lm(F2C1~F2V*rate, data = subset(CVC, C1=="b"))

# use a LRT to compare to model without interaction (lmc)
anova(lmc,lmci, test="Chisq")


# check encoding of V factor
contrasts(CVC$V)

# plot data for all consonants with best fitting lines
sp_all = ggplot(CVC, 
                aes(x=F2V, y = F2C1, col=C1)) + 
  geom_point(aes(shape=speaker)) +
  geom_smooth(method="lm", se=FALSE)+
  theme_classic(base_size=14)
print(sp_all)
