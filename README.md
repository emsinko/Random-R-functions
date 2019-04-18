# Random-R-functions
Custom R functions 


# R packages: 

https://www.analyticsvidhya.com/blog/2019/04/8-useful-r-packages-data-science/?utm_source=facebook.com&utm_medium=social&fbclid=IwAR3Dn-InenLlcXXeqx0Co89c-DgoOSmHPAOd78vjp5aAXHKAHliIIIXj7jo

# Esquisse
install.packages("esquisse")

#Load the package in R

library(esquisse)

esquisse::esquisser()

# Memer

devtools::install_github("sctyner/memer")

library(memer)

meme_list()


meme_get("ExpandingBrain") %>% 
  meme_text_brain("Sharing memes online", 
                  "Making your own memes", 
                  "Making memes in R", 
                  "Using rtweet to share your memes", 
                  size = 17);
