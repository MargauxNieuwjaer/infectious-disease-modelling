install.packages("tidyverse")
install.packages("tidyr")
install.packages("deSolve")
# LOAD THE PACKAGES:
library(ggplot2)
library(tidyr)
library(deSolve)

# MODEL INPUTS:
# Vector storing the initial number of people in each compartment (at timestep 0)
initial_state_values <- c(S = 999999,  # the whole population we are modelling is susceptible to infection
                          I = 1,       # the epidemic starts with a single infected person
                          R = 0)       # there is no prior immunity in the population
# Vector storing the parameters describing the transition rates in units of days^-1
parameters <- c(lambda = 0.2,  # the force of infection, which acts on susceptibles
                gamma = 0.1)   # the rate of recovery, which acts on those infected

# TIMESTEPS:

# Vector storing the sequence of timesteps to solve the model at
times <- seq(from = 0, to = 60, by = 1)   # from 0 to 60 days in daily intervals

# SIR MODEL FUNCTION: 

# The model function takes as input arguments (in the following order): time, state and parameters
sir_model <- function(time, state, parameters) {  
  
  with(as.list(c(state, parameters)), {  # tell R to unpack variable names from the state and parameters inputs
    
    # The differential equations
    dS <- -lambda * S               # people move out of (-) the S compartment at a rate lambda (force of infection)
    dI <- lambda * S - gamma * I    # people move into (+) the I compartment from S at a rate lambda, 
    # and move out of (-) the I compartment at a rate gamma (recovery)
    dR <- gamma * I                 # people move into (+) the R compartment from I at a rate gamma
    
    # Return the number of people in the S, I and R compartments at each timestep 
    # (in the same order as the input state variables)
    return(list(c(dS, dI, dR))) 
  })
  
}

# MODEL OUTPUT (solving the differential equations):

# Solving the differential equations using the ode integration algorithm
output <- as.data.frame(ode(y = initial_state_values, 
                            times = times, 
                            func = sir_model,
                            parms = parameters))
# Printing the model output returns a dataframe with columns time (containing the times vector), 
# S (containing the number of susceptible people at each timestep),
# I (containing the number of infected people at each timestep) and 
# R (containing the number of recovered people at each timestep).

output_long <- pivot_longer(output, cols = -time, names_to = "variable", values_to = "value")                 # turn output dataset into long format

#PLOT OUTPUT
ggplot(data = output_long,                                               # specify object containing data to plot
       aes(x = time, y = value, colour = variable, group = variable)) +  # assign columns to axes and groups
  geom_line() +                                                          # represent data as lines
  xlab("Time (days)")+                                                   # add label for x axis
  ylab("Number of people") +                                             # add label for y axis
  labs(colour = "Compartment")    



