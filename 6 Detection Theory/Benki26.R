# Analyze Benki data
library(tidyverse)

#read confusion matrix
Benki = read_csv("6 Detection Theory/Benki_all.csv")

# prepare subset of the data to use Detection Theory to test for an effect of position on perceptual dustance
# select a subset of the data with two stimuli that appear in both onset and coda
# with the corresponding response categories
# Example:
pt_data = Benki %>%
  select(position, stimulus, p, t) %>%
  filter(stimulus %in% c("p","t")) %>%
  group_by(position, stimulus) %>%
  summarize(p = sum(p), t = sum(t))

# Prepare data to fit a loglinear BCM
# reshape confusion matrix into long format using pivot_longer() from tidyverse
Benki_long = pivot_longer(Benki, names_to = "response", values_to="count",
                          cols=c(p, t, k, s, h, f, b, d, g, z, v, m, n, r, l, w, j, Null, Other))

# select a subset of the data with the same values for 'stimulus' and 'response' in coda and onset
stops = c("p","t","k")
stops_data=filter(Benki_long, stimulus %in% stops & response %in% stops)

stops_data$stimulus = factor(stops_data$stimulus)
stops_data$response = factor(stops_data$response)
stops_data$position = factor(stops_data$position)

# Function to generate matrix of distance variables.
# Run the code to generate the function, then you can use the function to generate a d matrix for your data set
# using d = distmatrix(data, "stimulus_var", "response_var")
distmatrix=function(data, stimulus_var="stimulus", response_var="response"){
  # 'cats' is the list of categories
  cats = levels(data[[stimulus_var]])
  n = length(cats)
  # initialize distance matrix, d, as an array of 0s, with the same number of rows as data, 
  # and enough columns for the dummy distance variables
  d = array(0,dim=c(length(data[[stimulus_var]]),0.5*n*(n-1)))
  # initialize a vector to hold the names of the dummy distance variables
  dvars = rep.int(0,0.5*n*(n-1))
  # initialize col, to keep track of which column of d is currently being populated
  col=0
  # Main loop: Enumerate all the pairs of distinct stimulus categories, and define a column of d for each
  # loop through the cats list from first to penultimate item on the list.
  for (i in 1:(n-1)){
    # cat1 is the ith category
    cat1=cats[i]
    # for each cat1, loop through the cats list from the next category to the end of the list
    for (j in (i+1):n){
      # increment col, the current column of d
      col=col+1
      # cat2 is the jth category
      cat2=cats[j]
      # the name of the current column is the concatenation of cat1 and cat2 joined by a period
      dvars[col]=paste(cat1,cat2,sep=".")
      # for each row in the dataframe, the cat1cat2 distance variable has value 1 if stimulus is cat1 and response is cat2
      # or stimulus is cat2 and response is cat1. Otherwise the value remains at 0.
      for (i in 1:length(data[[stimulus_var]])){
        if((data[i,stimulus_var]==cat1 & data[i,response_var]==cat2)|(data[i,stimulus_var]==cat2 & data[i,response_var]==cat1)){
          d[i,col]=1}
      }
    }
  }
  # set the column names of d to the names stored in dvars
  colnames(d)=dvars
  return(d)}