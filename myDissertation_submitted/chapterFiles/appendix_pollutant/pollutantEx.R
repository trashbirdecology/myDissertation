
# Setup -------------------------------------------------------------------

# Load packages
library(tidyverse)
library(lhs)
library(viridis)

theme_set(theme_bw())

# Define pollutant rate of change functions -------------------------------

# Saturating sequestration function
f_of_P <- function(P, m = 5, q = 6) {
  P * m ^ q / (m ^ q + P ^ q)
}

# Rate of change of pollutant in region 1
f_dP1dt <- function(P1, P2, M1 = 2, c1 = 0.05, D = 0.1, s = 0.5) {
    M1 + D * (P2 - P1) - c1 * P1 - s * f_of_P(P1)
  }

# Rate of change of pollutant in region 2
f_dP2dt <-
  function(P1, P2, M2 = 0.75, c2 = 0.1, D = 0.1, s = 0.5) {
    M2 + D * (P1 - P2) - c2 * P2 - s * f_of_P(P2)
  }

# Total rate of change in pollutants
f_dPdt <- function(P1, P2, M1 = 2) {
  sqrt(f_dP1dt(P1, P2, M1)^2 + f_dP2dt(P1, P2)^2)
}


# Visualize pollutant rates of change -------------------------------------

# Size of grid (n x n)
n <- 100

# Parameter vectors
P1 <- seq(0, 40, length.out = n)
P2 <- seq(0, 20, length.out = n)

# Evaluate rates over grid
pollutantRates <- 
  expand.grid(P1 = P1, P2 = P2) %>% 
  mutate(dP1dt = f_dP1dt(P1, P2),
         dP2dt = f_dP2dt(P1, P2),
         dPdt = sqrt(dP1dt^2 + dP2dt^2))

# Contour plot of rates
ggplot(pollutantRates, aes(P1, P2)) + 
  geom_raster(aes(fill = dPdt)) +
  geom_contour(aes(z = dPdt), color = "white", binwidth = 0.2) +
  scale_fill_viridis()

# Find minimum rates of change in pollutants (optimization) ---------------
# Bounds on parameters P1 and P2 (used to scale)
LB <- c(0, 0)
UB <- c(40, 20)

# Convert parameters to 0 to 1 range
x_to_u <- function(x) {
  (x - LB) / (UB - LB)
}

# Convert from 0 to 1 range back to parameter range
u_to_x <- function(u) {
  u*(UB - LB) + LB 
}

f_obj <- function(u) {
  x <- u_to_x(u)
  f_dPdt(x[1], x[2])
}

# Define staring points for optimiation (Latin Hypercube Sampling)
starts <- randomLHS(1000, 2)

# Preallocate empty arrays
fmin <- numeric(nrow(starts))
flag <- numeric(nrow(starts))
uOpt <- matrix(nrow = nrow(starts), ncol = 2)
xOpt <- matrix(nrow = nrow(starts), ncol = 2)

# Loop over starting locations performing optimization
for(i in 1:nrow(starts)) {
  opt <- optim(starts[i,], f_obj, control = list(maxit = 1e4, reltol = 1e-8))
  uOpt[i,] <- opt$par
  xOpt[i,] <- u_to_x(opt$par)
  fmin[i] <- opt$value
}
possibleOpt <- unique(round(uOpt*1e3)/1e3)

# Restart optimizer at each possible optimum
starts <- possibleOpt
fmin <- numeric(nrow(starts))
flag <- numeric(nrow(starts))
uOpt <- matrix(nrow = nrow(starts), ncol = 2)
xOpt <- matrix(nrow = nrow(starts), ncol = 2)
for(i in 1:nrow(starts)) {
  opt <- optim(starts[i,], f_obj, control = list(maxit = 1e4, reltol = 1e-8, abstol = 1e-8))
  uOpt[i,] <- opt$par
  xOpt[i,] <- u_to_x(opt$par)
  fmin[i] <- opt$value
}

# Collect local optimums
uniqueMins <- unique(round(xOpt*1e2)/1e2)
localMin <- tibble(P1 = uniqueMins[,1], P2 = uniqueMins[,2]) %>% 
  mutate(dPdt = f_dPdt(P1, P2))

# Plot local optimums
ggplot(pollutantRates, 
       aes(P1, P2)) + 
  geom_raster(aes(fill = dPdt)) +
  geom_contour(aes(z = dPdt), color = "white", binwidth = 0.2) +
  scale_fill_viridis() +
  geom_point(data = localMin, color = "red")

# Solve system differential equations -------------------------------------

# Model parameters
r <- 1        # renewal rate of the ecosystem service
h <- 0.5      #  effects of extracting or using the ecosystem service
k <- 0.5      # impact of the pollutant on renewal of the ecosystem service
c1 <- 0.05    # decay rate 1
c2 <- 0.1     # decay rate 2
D <- 0.1      # transport rate between region j and region i
s <- 0.5      # maximum sequestration rate
m <- 5        # pollutant level at with sequestration is half the max rate
q <- 6        # determines slope of f(P) near P = m
sigma <- 0.05 # standard deviation of shocks to the system

# Time vector
n <- 36
dt <- 1/n
t <- seq(0, 1e3, dt)

# Weiner noise process
dW <- function(dt) {
  rnorm(1, mean = 0, sd = sqrt(dt))
}

# Preallocate arrays
S1 = numeric(length(t)-1)
S2 = numeric(length(t)-1)
P1 = numeric(length(t)-1)
P2 = numeric(length(t)-1)
M1 = numeric(length(t)-1)
M2 = numeric(length(t)-1)

# Initial conditions
S1[1] = 1.055
S2[1] = 2.478
P1[1] = 3.790
P2[1] = 1.614
M1[1] <- 2.0
M2[1] <- 0.75

# Euler–Maruyama method
for(i in 1:(length(t)-1)){
  
  # Change in pollution discharge
  dM1 <- ((2.1 - 2.0)/1e3) * dt
  dM2 <- 0

  # Change in ecosystem services
  dS1 <- ((r / (k * P1[i])) - h * S1[i]) * dt
  dS2 <- ((r / (k * P2[i])) - h * S2[i]) * dt
  
  # Change in pollutants
  dP1 <- (M1[i] + D*(P2[i] - P1[i]) - c1*P1[i] - s*f_of_P(P1[i])) * dt + sigma*dW(dt)
  dP2 <- (M2[i] + D*(P1[i] - P2[i]) - c2*P2[i] - s*f_of_P(P2[i])) * dt + sigma*dW(dt)
  
  # Pollution discharge
  M1[i+1] <- M1[i] + dM1
  M2[i+1] <- M2[i] + dM2
  
  # Pollutants
  P1[i+1] <- P1[i] + dP1
  P2[i+1] <- P2[i] + dP2
  
  # Ecosystem services
  S1[i+1] <- S1[i] + dS1
  S2[i+1] <- S2[i] + dS2
}

# Ecosystem services results
services <- as_tibble(cbind(t, S1, S2))

# Pollutants results
pollutants <- as_tibble(cbind(t, P1, P2))

# Merge results
systemState <- left_join(services, pollutants)

# Plot data
plotData <- rbind(services %>% 
                    gather(parameter, value, -t) %>% 
                    mutate(group = "ecosystem services"), 
                  pollutants %>% 
                    gather(parameter, value, -t) %>% 
                    mutate(group = "pollutants"))

# Plot system state variables over time
ggplot(data = plotData,
       aes(x = t, y = value, color = parameter)) +
  geom_line() +
  facet_wrap(~group, scales = "free_y") 

# Plot pollutant path
ggplot(pollutantRates, 
       aes(P1, P2)) + 
  geom_raster(aes(fill = dPdt)) +
  geom_contour(aes(z = dPdt), color = "white", binwidth = 0.2) +
  scale_fill_viridis() +
  geom_point(data = localMin, color = "red") +
  geom_line(data = as_tibble(cbind(t, P1, P2)), color = "red")

# Calculate pollutant rate of change along path
M1 <- c(2, 2.1)
n <- length(t)
m <- length(M1)
pollutantRatesPath <-
  tibble(
    t = rep(t, m),
    P1 = rep(P1, m),
    P2 = rep(P2, m),
    M1 = rep(M1, n)
  ) %>%
  mutate(dPdt = f_dPdt(P1, P2, M1))

# Plot pollutant rate of change along path
ggplot(data = pollutantRatesPath,
       aes(x = t, y = dPdt, group = M1, color = M1)) +
  geom_line()


# Calculate distance metrics ----------------------------------------------

distanceMetrics <- 
  systemState %>% 
  # Randomly sample 1% of data
  sample_frac(0.01) %>% 
  # Convert data to long form
  gather(variable, value, -t) %>% 
  # Distance between variables
  arrange(variable, t) %>% 
  group_by(variable) %>%
  mutate(dx = value - lag(value)) %>%
  ungroup() %>% 
  na.omit(dx) %>%
  # Sum of distances (across variables at each time)
  group_by(t) %>%
  summarize(ds = sqrt(sum(dx ^ 2))) %>%
  ungroup() %>%
  # Calculate cumulative ds and derivatives
  mutate(s = cumsum(ds),
         dsdt = ((s - lag(s)) / (t - lag(t))),
         d2sdt2 = ((dsdt - lag(dsdt)) / (t - lag(t)))) %>%
  ungroup()

# Plot distance over time
ggplot(data = distanceMetrics,
       aes(x = t, y = s)) +
  geom_line() +
  labs(x = "time", y = "distance travelled")

# 
ggplot(data = distanceMetrics,
       aes(x = t, y = dsdt)) +
  geom_line() +
  geom_smooth(method = "loess", span = 0.2)

