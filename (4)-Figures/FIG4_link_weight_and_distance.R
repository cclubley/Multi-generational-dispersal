# -------------------------------------------------------------------------
# --------------------- Edge weight and seaway distance -------------------
# -------------------------------------------------------------------------
# Load the required packages --------------------------------------------------
{ library(reshape2)
  library(igraph)
  library(rworldxtra)
  library(raster)
  library(maps)
  library(ggplot2)
  library(sdmpredictors)
  #library(rSDM)
  library(gdistance)
  #library(SDraw)
  library(ccplot)
  library(tidyverse)
  library(broom)
  #install.packages("remotes")
  #remotes::install_github("OnofriAndreaPG/aomisc")
  library(aomisc)
  library(wesanderson)
  library(ggExtra)
  library(dplyr)
}

# Read the data -----------------------------------------------------------

setwd("C:/Users/charl/Documents/PhD/Chapters/(3) Dispersal Limitation/(4) Results/(1) Graph-level analysis/(Fig 4) Link weight and distance")
distances2 <- read.csv("./Distances_no_zero.csv")

# Load the new weighted averages
setwd("C:/Users/charl/Documents/PhD/Chapters/(3) Dispersal Limitation/(4) Results/(1) Graph-level analysis/(Fig 3) Averaged link weight/New averaged link weights")
connectivity <- read.csv("Weighted_average_graph_new.csv")

new_data <- merge(distances2, connectivity, by=(c("Release_poly", "Settlement_poly")))
new_data$Count.x <- NULL

# Summarise and plot ------------------------------------------------------

# Plot distances with count > 0 and no self-recruitment
ggplot(data=new_data, aes(x=Distance, y=Count.y))+
  geom_point()

# Summary, all edges with weight > 0 and no self-recruitment
summary(new_data$Distance)
plotrix::std.error(new_data$Distance)
# The greatest distance between two nodes with a connection was 923.1514 km
# The minimum distance (not including self-recruitment) was 11.92 km
# The average distance was 206.35 km 

dist <- filter(new_data, Distance <=260)
(nrow(dist)/nrow(new_data))*100
# 75% of links are 260 km or less apart

dist <- filter(new_data, Distance <=175)
(nrow(dist)/nrow(new_data))*100
# 50% of links are 175 km or less apart

dist <- filter(new_data, Distance <=115)
(nrow(dist)/nrow(new_data))*100
# 25% of links are 115 km or less apart

# Fit an exponential decay curve ------------------------------------------

# https://douglas-watson.github.io/post/2018-09_exponential_curve_fitting/
# https://tjmurphy.github.io/jabstb/nonlinearreplicates.html 

# Nonlinear regression is a statistical method to fit nonlinear models to the 
# kinds of data sets that have nonlinear relationships between independent and 
# dependent variables.

# In nonlinear regression we fit a model formula to pairs of X,Y data from an 
# experiment. The best fitting model parameters responsible for giving nonlinear 
# shape to the relationship between X and Y are then determined by the regression
# method.

# Best fit of the regression curve is determined by a minimization of the sum of 
# the squared residuals between the model values for the response and 
# experimentally-derived values.

# Exponential decay using self-starting functions

# y(t) ~ yf + (y0 - yf)e-at

# The measured y value starts at y0 and decays towards yf at a rate of a

ggplot(data=new_data, aes(x=Distance, y=Count.y))+
  geom_point()

# 'nls' is the standard R base function to fit non-linear equations.
# However trying to fit exponential decay with nls doesn't work if you pick a 
# bad initial guess for the rate constant (a).

# A solution is to use a self-starting function - a special function for curve 
# fitting that guesses its own starting parameters. 
# The asymptotic regression function 'SSasymp' is equivalet to exponential decay.
# y and x in the formula are replaced by Count and Distance.
fit <- nls(Count.y~SSasymp(Distance, yf, y0, alpha), data=new_data)
fit
# Instead of fitting the rate constant directly, it searched for the logarithm 
# of a:
# y(t) ~ yf + (y0 - yf) * e ^ (-exp(a)) * t

# Extract the parameters
paras <- summary(fit)$coeff[,1]
paras

# alpha = exp(alpha)
exp(-4.473272)

# y(t) ~ -885.67 + (301106 - -885.67) * e ^ -0.011*t

fit
# The fact the regression converges to a solution suggests a good fit, but 
# that's not always the case. Nonlinear regression resolves parameter values by
# an iterative algorithm that converges onto the solution (a stable minimization 
# of residual error). Here, this took 5 cycles.

# Non-linear models do not have an intercept term, so most authors advocate 
# against the use of R-squared in nls analysis. 
# We can calculate pseudo-R-squared, but it cannot be interpreted as the 
# proportion of variance explained by the model.
aomisc::R2nls(fit)$PseudoR2
# 0.17

# Graphically check the fitted curve
ggplot(data=augment(fit), aes(x=Distance, y=Count.y))+
  geom_point()+
  geom_line(aes(y=.fitted), col="red")

## Trying an alternative method

## Use 'NLS.expoDecay' from the 'aomisc' package instead
#model <- nls(Count ~ NLS.expoDecay(Distance, a, k),
#data = distances2)
#summary(model)

#ggplot(data=augment(model), aes(x=Distance, y=Count))+
#geom_point()+
#geom_line(aes(y=.fitted), col="red")

## Compare the two methods
#anova(fit, model, test="F")
## No significant difference - the two models are essentially the same
## Let's go with the 'fit' model as it uses a self-start function

plot <- ggplot(data=augment(fit), aes(x=Distance, y=Count.y))+
  geom_point()+
  geom_line(aes(y=.fitted), col="red", size=1)+
  cc_theme()

#Add a marginal density plot
plot2 <- ggMarginal(plot, type="density")
plot2


