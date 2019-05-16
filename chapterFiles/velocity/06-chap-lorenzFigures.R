library(deSolve)
library(scatterplot3d)
require(tidyverse)
source(here::here("chapterFiles/velocity/06-chap-velocity_helperFuns.R"))
# Parameters for the solver 
param <- c(alpha = 10,
           beta = 8/3,
           c = 26.48)

# Initial state 
yini <- c(x = 0.01, y = 0.0, z = 0.0)

# Lorenz function
lorenz <- function(Time, State, Param) {
  with(as.list(c(State, Param)), {
    xdot <- alpha * (y - x)
    ydot <- x * (c - z) - y
    zdot <- x*y - beta*z
    return(list(c(xdot, ydot, zdot)))
  })
}

# PLOT RESULTS ------------------------------------------------------------
# Run function to plot
runIt <- function(times) {
  out <- as.data.frame(ode(func = lorenz, y = yini, parms = param, times = times))
  
  scatterplot3d(x=out[,2],
                y=out[,3],
                z=out[,4],
                color="black",
                type="l",
                box=TRUE,
                highlight.3d=F,
                grid=F,
                axis=T,
                xlab="x",
                ylab="y",
                zlab="z",
                main=NULL, 
                angle=55) 

}

# Run All function combining functions to plot
runAll <- function() {
  runIt(seq(0, 100, by=0.01))
  }

# Command to produce graphical output
runAll()



# GET DATA RESULTS ------------------------------------------------------------
# Run function to plot
runIt <- function(times) {
  out <- as.data.frame(ode(func = lorenz, y = yini, parms = param, times = times))
  return(out)
  # scatterplot3d(x=out[,2],
  #               y=out[,3],
  #               z=out[,4],
  #               color="black",
  #               type="l",
  #               box=TRUE,
  #               highlight.3d=F,
  #               grid=F,
  #               axis=T,
  #               xlab="x",
  #               ylab="y",
  #               zlab="z",
  #               main=NULL, 
  #               angle=55) 
  
}


# Run All function combining functions to plot
runAll <- function() {
  runIt(seq(0, 100, by=0.01))
}

# Command to produce graphical output
out<- runAll() %>% 
  gather("key","value", -time)


p <- ggplot(out, aes(x=time, y=value))+
  geom_line()+
  facet_wrap(~key, ncol=1)+
  theme_Publication()


ggsave(p,filename=here::here("chapterFiles/velocity/figsCalledInDiss/lorenz3D_timeseries.png"), 
       height= 5, width=8)
