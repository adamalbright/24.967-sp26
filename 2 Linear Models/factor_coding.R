# factor coding script
library(ggplot2)

# read in the affricate data
affricate_data <- read.csv("~/Documents/Class Materials/24.963 linguistic phonetics/affricates experiment/affricate_data.csv")

# exclude subject vv
aff = subset(affricate_data, !(subject=="vv"))

# declare factors
aff$affricate = factor(aff$affricate)
aff$context = factor(aff$context)

# fit linear model
lm_intensity = lm(intensity~affricate*context, data=aff)
summary(lm_intensity)

# use contrast coding
contrasts(aff$affricate) = contr.sum(levels(aff$affricate))
contrasts(aff$context) = contr.sum(levels(aff$context))

# use dummy coding
contrasts(aff$affricate) = contr.treatment(levels(aff$affricate))
contrasts(aff$context) = contr.treatment(levels(aff$context))

contrasts(aff$affricate)
contrasts(aff$context)

ggplot(data=aff, 
        aes(x=affricate, y=intensity, color=context)) +
        geom_boxplot() +
        theme_classic()

# read in the CVC data
CVC = read.csv("engCVC.csv")

# declare factors
CVC$C1 = factor(CVC$C1)
CVC$V = factor(CVC$V)
CVC$C2 = factor(CVC$C2)
CVC$rate = factor(CVC$rate)
CVC$speaker = factor(CVC$speaker)

ggplot(CVC, aes(x=V, y = F2V))+
  geom_boxplot()+
  theme_classic(base_size=14)

vmod = lm(F2V~V, data = CVC)

contrasts(CVC$V) = contr.sum(levels(CVC$V))

vmod_sum = lm(F2V~V, data = CVC)
summary(vmod_sum)
