# R commands for the examples and plots from the second lecture on 
# linear mixed effects models
# hypothesis testing, random effects structure, convergence problems

# libraries
library(lme4)
library(tidyverse)

# load affricate data
affricate_data <- read.csv("2\ Linear\ Models/affricate_data.csv")

# exclude subject vv
aff = subset(affricate_data, !(subject=="vv"))

# declare factors
aff$subject = factor(aff$subject)
aff$word = factor(aff$word)
aff$affricate = factor(aff$affricate)
aff$context = factor(aff$context)

# Fit a linear mixed model with random intercepts and slopes by subject
lmem = lmer(intensity~affricate+(1+affricate|subject), data=aff)
summary(lmem)

# perform t-tests using lmerTest
library(lmerTest)
library(pbkrtest)

# Add random intercept by item
lmem2 = lmer(intensity~affricate+(1+affricate|subject)+(1|word), data=aff)
# t-tests with Satterthwaite approximation to degrees of freedom
summary(lmem2)

# t-tests with Kenward-Roger approximation to degrees of freedom
summary(lmem2, ddf="Kenward-Roger")

# use LRT to test the effect of affricate
lmemr = lmer(intensity~1+(1+affricate|subject)+(1|word), data=aff)
anova(lmem2,lmemr)

# use parametric bootstrap LRT of the same hypothesis
PBmodcomp(lmem2, lmemr)

# random effects structure

# affricates
# fit linear model with affricate*context
# contrast code affricate and context
contrasts(aff$affricate) = contr.sum(levels(aff$affricate))
contrasts(aff$context) = contr.sum(levels(aff$context))


lm_aff = lm(intensity~affricate*context, data=aff)
summary(lm_aff)

# fit linear mixed model with maximal random effects structure
# singular fit
lme_aff_max = lmer(intensity~affricate*context+(1+affricate*context|subject)+(context|word), data=aff)

# simplify
lme_aff_red1 = lmer(intensity~affricate*context+(1+affricate+context|subject)+(context|word), data=aff)
summary(lme_aff_red1)

#backwards selection of random effects, following Matuschek et al
lme_aff_red2 = lmer(intensity~affricate*context+(1+affricate|subject)+(context|word), data=aff)
anova(lme_aff_red1, lme_aff_red2, refit=FALSE)

lme_aff_red3 = lmer(intensity~affricate*context+(1+context|subject)+(context|word), data=aff)
anova(lme_aff_red1, lme_aff_red3, refit=FALSE)

lme_aff_red4 = lmer(intensity~affricate*context+(1+context|subject)+(1|word), data=aff)
anova(lme_aff_red1, lme_aff_red4, refit=FALSE)

# Singular fit, convergence problems and strategies
# read in the CVC data
CVC = read.csv("2 Linear Models/engCVC.csv")

CVC$F2V = as.numeric(CVC$F2V)

# fit a mixed effects model to estimate locus equations for all C1's 
# singular fit
lmec_max = lmer(F2C1~C1*F2V+(C1*F2V|speaker), data = CVC)

# simplify - still singular
lmec2 = lmer(F2C1~C1*F2V+(C1+F2V|speaker), data = CVC)

# singular
lmec3 = lmer(F2C1~C1*F2V+(C1*F2V||speaker), data = CVC)

# singular
lmec4 = lmer(F2C1~C1*F2V+(C1|speaker), data = CVC)
summary(lmec4)

# convergence warnings
lmec5 = lmer(F2C1~C1*F2V+(F2V|speaker), data = CVC)

# increase number of iterations - still warnings
lmec5 = lmer(F2C1~C1*F2V+(F2V|speaker), data = CVC,
              control=lmerControl(optCtrl=list(maxeval=2000)))

# scale and center F2V 
CVC$F2Vscale = scale(CVC$F2V)

lmec4scale = lmer(F2C1~C1*F2Vscale+(C1|speaker), data = CVC)
summary(lmec4scale)

lmec5scale = lmer(F2C1~C1*F2Vscale+(F2Vscale|speaker), data = CVC)
summary(lmec5scale)

m = mean(CVC$F2V, na.rm=TRUE)
s = sd(CVC$F2V, na.rm=TRUE)

# back-transform coefficients
coefs = fixef(lmec5scale)

# b0
b0 = coefs[1]-coefs[4]*m/s
# b_d
b_d = coefs[2]-coefs[5]*m/s
# b_g
b_g = coefs[3]-coefs[6]*m/s
# b_F2V
b_F2V = coefs[4]/s
# b_d:F2V
b_dF2V = coefs[5]/s
# b_g:F2V
b_gF2V = coefs[6]/s

c(b0, b_d, b_g, b_F2V, b_dF2V, b_gF2V)

# compare coefficients of model with unscaled F2V 
fixef(lmec5)


## 3. recompute gradient and Hessian with Richardson extrapolation
devfun <- update(lmec5, devFunOnly=TRUE)
if (isLMM(lmec5)) {
  pars <- getME(lmec5,"theta")
} else {
  ## GLMM: requires both random and fixed parameters
  pars <- getME(lmec5, c("theta","fixef"))
}
if (require("numDeriv")) {
  cat("hess:\n"); print(hess <- hessian(devfun, unlist(pars)))
  cat("grad:\n"); print(grad <- grad(devfun, unlist(pars)))
  cat("scaled gradient:\n")
  print(scgrad <- solve(chol(hess), grad))
}
## compare with internal calculations:
lmec5@optinfo$derivs

# Try many optimizers
# To access all optimizers, install optimx and dfoptim packages
lmec5_all = allFit(lmec5)                      
summary(lmec5_all)

# use blmer to avoid singular fit/convergence problems
# default priors work
library(blme)
blmem = blmer(intensity~affricate*context+(affricate*context|subject)+(context|word), data=aff)
summary(blmem)


