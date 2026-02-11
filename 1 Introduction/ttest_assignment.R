# open data file. Replace the path
gv = read.csv("~/Documents/Class Materials/24.963 linguistic phonetics/glide experiment/data22.csv")

# exclude outlier
gv = filter(gv, is.na(glideF1)|glideF1 > 150)

# extract vector of F1 values for [j]
F1j = gv %>% filter(glide=="j") %>% pull(glideF1)

# extract vector of F1 values for [i]
F1i = gv %>% filter(vowel=="i") %>% pull(vowelF1)

