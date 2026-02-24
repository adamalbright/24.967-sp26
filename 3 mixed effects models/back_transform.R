library(tidyverse)

# Function to back-transform coefficients of a simple linear model fitted to 
# scaled variables. This function is only applicable to models withou interactions.
# rescale.coefs()is a function that takes three arguments, beta,m,s and returns 
# a vector of back-transformed coefficients.
#   beta is a vector of the coefficients of the transformed model.
#   -	Can be obtained with coef(<model>)
#   m is a vector of mi for the x variables. If a variable has not been scaled, mi = 0
#   s is a vector of si for the x variables. If a variable has not been scaled, si = 1

rescale.coefs = function(beta,m,s) {
  # back-transformed variables will be written into beta2 
  beta2 = beta ## inherit names etc.
  # back-transformed coefficients other than the first (the intercept) 
  # are equal to the corresponding coefficients of the transformed model 
  # divided by the relevant s value. 
  # vector[-1] is vector with the first element removed.
  beta2[-1] = beta[-1]/s
  # calculate the back-transformed intercept using the back-transformed
  # betas and m
  beta2[1] = beta[1]-sum(beta2[-1]*m)
  return(beta2)
}

# Example
# load VCV data
VCV = read.csv("/Users/flemming/Documents/Class\ Materials/24.967\ Experimental\ phonology/VCV\ data/VCVformants.csv")

VdV = filter(VCV, C == "d")
# fit a model of CF2 of [d] based on two independent variables, VpF2, VdF2
VdVmod = lm(CF2~VpF2+VdF2, VdV)
# look at the coefficients
coef(VdVmod)

# create normalized VpF2_scale and VdF2_scale variables
VdV = mutate(VdV, VpF2_scale = scale(VpF2), VdF2_scale = scale(VdF2))

# fit the equivalent model using the normalized variables
VdV_scale_mod = lm(CF2~VpF2_scale+VdF2_scale, VdV)
# extract the coefficients
beta = coef(VdV_scale_mod)
# look at them
beta

# backtransform the coefficients
# calculate a vector of means of VpF2 and VdF2
m = select(VdV,VpF2, VdF2) %>% summarize_all(mean)

# calculate a vector of means of VpF2 and VdF2
s = select(VdV,VpF2, VdF2) %>% summarize_all(sd)


# get back-transformed coefficients
rescale.coefs(beta, m, s)

#compare to unscaled model coefficients
coef(VdVmod)

# back transform coefficients of a model with interactions
library(lme4)

# fit a lme model to the VCV data, with C, VpF2 and their interaction
# keep random effects simple to ensure convergence
# you can experiment with more complex structures to see if models with convergence 
# warnings actually have different coefficients from a convergent model based on
# scaled VpF2
VCVmod = lmer(CF2~VpF2*C+(1+C|file), data = VCV)
# look at the fixed effect coefficients
fixef(VCVmod)

# fit the equivalent model to VpF2_scale
# create scaled variable
VCV = mutate(VCV, VpF2_scale = scale(VpF2))

VCVmod_scale = lmer(CF2~VpF2_scale*C+(1+C|file), data = VCV)

# extract fixed effects coefficient
coefs = fixef(VCVmod_scale)

m = mean(VCV$VpF2)
s = sd(VCV$VpF2)

# b0
coefs[1]-coefs[2]*m/s

# b1 coefficient of VpF2
coefs[2]/s

# b2 coefficient of Cd - back-transform using coefficient of VpF2_scale:Cd
coefs[3]-coefs[5]*m/s

# b3 coefficient of Cg - back-transform using coefficient of VpF2_scale:Cg
coefs[4]-coefs[6]*m/s

# b4 coefficient of VpF2:Cd
coefs[5]/s

# b5 coefficient of VpF2:Cg
coefs[6]/s

# compare to coefficients of unscaled model
fixef(VCVmod)
