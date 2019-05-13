# install.packages("rEDM")
## ye2015equation
library(rEDM)
data(tentmap_del)
str(tentmap_del)
ts <- tentmap_del
lib <- c(1, 100)
pred <- c(201, 500)

simplex_output <- simplex(ts, lib, pred)
str(simplex_output)
par(mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))  # set up margins for plotting
plot(simplex_output$E, simplex_output$rho, type = "l", xlab = "Embedding Dimension (E)", 
     ylab = "Forecast Skill (rho)")


simplex_output <- simplex(ts, lib, pred, E = 2, tp = 1:10)
