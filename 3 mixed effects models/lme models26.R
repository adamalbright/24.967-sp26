# R commands for the examples and plots from the second lecture on 
# linear mixed effects models
# hypothesis testing, random effects structure, convergence problems

# libraries
library(lme4)
library(tidyverse)

# load affricate data
affricate_data <- read.csv("~/Documents/Class Materials/24.963 linguistic phonetics/affricates experiment/affricate_data.csv")

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
CVC = read.csv("~/Documents/Class Materials/24.967 experimental phonology/engCVC.csv")

# fit a mixed effects model to estimate locus equations for all C1's 
# singular fit
lmec_max = lmer(F2C1~C1*F2V+(C1*F2V|speaker), data = CVC)

# simplify - still singular
lmec2 = lmer(F2C1~C1*F2V+(C1+F2V|speaker), data = CVC)

# singular
lmec3 = lmer(F2C1~C1*F2V+(C1*F2V||speaker), data = CVC)

# singular
lmec4 = lmer(F2C1~C1*F2V+(C1|speaker), data = CVC)

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
lmec5 = allFit(lmec5)                      
summary(lmec5)

# use blmer to avoid singular fit/convergence problems
# default priors work
library(blme)
blmem = blmer(intensity~affricate*context+(affricate*context|subject)+(context|word), data=aff)
summary(blmem)

# scaling and centering a variable for each subject

# using dplyr
CVC = mutate(CVC, .by=speaker, F2Vnorm = scale(F2V))

# using tapply
# centering by subject
CVC$F2Vnorm = CVC$F2V-tapply(CVC$F2V,CVC$speaker,mean,na.rm=T)[CVC$speaker]

#normalizing by subject
CVC$F2Vnorm = (CVC$F2V-tapply(CVC$F2V,CVC$speaker,mean,na.rm=T)[CVC$speaker])/tapply(CVC$F2V,CVC$speaker,sd,na.rm=T)[CVC$speaker]

# Fit linear mixed models to the CVC data where C1 is [b] or [d]
CVCbd = filter(CVC, C1 =="b"|C1=="d")
CVCbd$C1 = factor(CVCbd$C1)

CVCbd$F2Vscale = scale(CVCbd$F2V)

lmbd = lm(F2C1~C1*F2V, data=CVCbd)
summary(lmbd)

lmebd_int = lmer(F2C1~C1*F2V+(1|speaker), data = CVCbd)
summary(lmebd_int)

lmebd_intC1 = lmer(F2C1~C1*F2V+(1+C1|speaker), data = CVCbd)
summary(lmebd_intC1)

lmebd_max = lmer(F2C1~C1*F2V+(C1*F2V|speaker), data = CVCbd)

lmebd2 = lmer(F2C1~C1*F2V+(C1+F2V|speaker), data = CVCbd)
summary(lmebd2)

lmebd2_all = allFit(lmebd2)
summary(lmebd2_all)

lmebd2scale = lmer(F2C1~C1*F2Vscale+(C1+F2Vscale|speaker), data = CVCbd)
summary(lmebd2scale)

# try fitting models with normalized F2V
lmebd2norm = lmer(F2C1~C1*F2Vnorm+(C1+F2Vnorm|speaker), data = CVCbd)
summary(lmebd2norm)

# singular
lmec_norm_max = lmer(F2C1~C1*F2Vnorm+(C1*F2Vnorm|speaker), data = CVC)

# convergence warning
lmec_norm2 = lmer(F2C1~C1*F2Vnorm+(C1+F2Vnorm|speaker), data = CVC)

lmec_norm3 = lmer(F2C1~C1*F2Vnorm+(C1|speaker), data = CVC)


# try all optimizers
lmec_norm2_all = allFit(lmec_norm2)
summary(lmec_norm2_all)

# model F2C2
CVC$C2=factor(CVC$C2)
contrasts(CVC$C2) = contr.sum(levels(CVC$C2))

# use blmer to try to avoid singular fit/convergence problems
# default priors
blmec = blmer(F2C1~C1*F2V+(C1*F2V|speaker), data = CVCbd)

blmecscale = blmer(F2C1~C1*F2Vscale+(C1*F2Vscale|speaker), data = CVCbd)

blmecnorm = blmer(F2C1~C1*F2Vnorm+(C1*F2Vnorm|speaker), data = CVCbd)
summary(blmecnorm)

blmec = blmer(F2C1~C1*F2Vnorm+(C1*F2Vnorm|speaker), data = CVCbd)
summary(blmec)

## rosa's roses data
schwa = read.csv("schwa_minimalpairs.csv")

schwa$word = tolower(schwa$word)
schwa$category = tolower(schwa$category)

schwa$speaker = factor(schwa$speaker)

schwa_lme = lmer(F1~category+(1+category|speaker), schwa)

schwa_lme2 = lmer(F1~category+(1+category|speaker)+(1|word), schwa)
summary(schwa_lme2)

schwa_lme3 = lmer(F1~category+(1+category|speaker), schwa)

anova(schwa_lme2, schwa_lme3)

schwa_lme4 = lmer(F1~category+(1|speaker)+(1|word), schwa)

anova(schwa_lme2, schwa_lme4)


schwa_lme2 = lmer(F1~category+(1+category|speaker)+(1+category|pair), schwa)
summary(schwa_lme2)

schwa_lme3 = lmer(F1~category+(1+category|speaker)+(1|pair), schwa)
summary(schwa_lme3)

schwaF2_lme2 = lmer(F2~category+(1+category|speaker)+(1|pair), schwa)
summary(schwaF2_lme2)
