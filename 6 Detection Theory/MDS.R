# Commands for performing MDS starting from d parameters estimated using BCM

# use loglinear BCM.qmd to fit a BCM
# e.g. start from the Redford Diehl data, loaded as 'rd'

# exclude sh
rd_no_sh = subset(rd, !(stimulus == "sh") & !(response == "sh"))
rd_no_sh$stimulus = factor(rd_no_sh$stimulus)
rd_no_sh$response = factor(rd_no_sh$response)

# use distmatrix function from BCM.qmd
d = distmatrix(rd_no_sh)

rdmodel_no_sh=glm(count~context*(stimulus+response+d), data=rd_no_sh,family=poisson(link="log"))
summary(rdmodel_no_sh)

# extract d parameters for base context from model
dcoef = -coef(rdmodel_no_sh)[14:28]

# Generate distance matrix from d parameters

# N = number of stimuli
# all
N = nlevels(rd_no_sh$stimulus)

# S = list of stimuli
# all
Ss = levels(rd_no_sh$stimulus)
# stops
Ss = levels(rd_no_sh$stimulus)

# initialize distance matrix with rows and columns labeled with stimuli, all values set to 0
dist = matrix(rep(0,N^2),nrow=N, ncol=N, dimnames = list(Ss,Ss))

# fill out distance matrix with d parameters
for (i in 1:length(dcoef)){
  cname1 = unlist(strsplit(names(dcoef)[i],"[.]"))[1]
  s1 = substr(cname1,2,nchar(cname1))
  s2 = unlist(strsplit(names(dcoef)[i],"[.]"))[2]
  dist[s1,s2]=dcoef[i]
  dist[s2,s1]=dcoef[i]
}

# using classical metric MDS to generate a configuration of points for the stimuli in ndims dimensions
ndims = 3
space = cmdscale(dist, k=ndims)

# plot the fitted points
# use lattice package 'scatterplot matrix' function splom to plot all pairs of dimensions against eacj other
library(lattice)
splom(space, pch=Ss)

# if ndims =2 a regular plot is better
plot(space, pch=Ss, asp=1)

# 3D scatterplot
# install package scatterplot3d
library(scatterplot3d)
scatterplot3d(space, type="h", pch = 1:nrow(space))
legend("topright",legend=rownames(space), pch=1:nrow(space))
# or plot first letter of segment label
scatterplot3d(space, type="h", pch = rownames(space))

# compare BCM and MDS distance matrices
# BCM distance matrix
dist
# use dist() function to calculate Euclidean distances between fitted points
dist(space)

# use Sammon's least-squares MDS
library(MASS)
sammond = sammon(dist, k=ndims)
space = sammond$points

# check distance matrix 'dist' for violations of triangle inequality
for(i in 1:(N-2)){
	for(j in 2:(N-1)){
		for(k in 3:N){
			dij = dist[i,j]
			dik = dist[i,k]
			djk = dist[j,k]
			if(dij > dik+djk){print(paste("violation",i, j, k))}
			if(dik > dij+djk){print(paste("violation",i, k, k))}
			if(djk > dij+dik){print(paste("violation",j, k, i))}
		}
	}
}

