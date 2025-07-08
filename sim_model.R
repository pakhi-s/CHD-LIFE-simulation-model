## 17.01.2025

## last updates = 31.05.2025

## This is to produce outputs for the simulation model manuscript. 
## Some data have been exported from TreeAge, others are from CHDLIFE database.

## packages needed
library(tidyverse)
library(readr)
library(tidyr)
library(dplyr)
library(ggpubr)
library(ggplot2)

## State probability graph
markov_data <- read.csv("C:/Users/sharmap6/OneDrive - Queensland University of Technology/Desktop/CHD LIFE/Manuscript/Exported data from TreeAge/30May/markov_summary.csv")
colnames(markov_data)

# Reshape data from wide to long format
data_long <- markov_data %>%
  pivot_longer(
    cols = c("PreDischargePathway", "RoutineScreeningPathway", "ScreeningAppointment",
             "Assessment", "InterventionSingleDomain", "InterventionMultipleDomains", "LostToFollowUp"),      
    names_to = "State",             
    values_to = "Probability"       
  )

# Renamed stage to months 
data_long <- data_long %>%
  rename(Months = Stage)


# Combined plot
ggplot(data_long, aes(x = Months, y = Probability, color = State)) +
  geom_line() +
  labs(title = "State probabilities over 5 years", x = "Months", y = "Probability") +
  theme_minimal()

# Four separate plots for different stages 
plot1 <- ggplot(filter(data_long, State %in% c('PreDischargePathway', 'RoutineScreeningPathway', 'ScreeningAppointment')), 
                aes(x = Months, y = Probability, color = State)) +
  geom_line() +
  labs(title = "State probabilities of pre-discharge pathway, routine screening pathway and appointments over 5 years", x = "Months", y = "Probability") +
  theme_minimal()

plot2 <- ggplot(filter(data_long, State %in% c('Assessment')), 
                aes(x = Months, y = Probability, color = State)) +
  geom_line() +
  labs(title = "State probability of assessment over 5 years", x = "Months", y = "Probability") +
  theme_minimal()

plot3 <- ggplot(filter(data_long, State %in% c('InterventionSingleDomain', 'InterventionMultipleDomains')), 
                aes(x = Months, y = Probability, color = State)) +
  geom_line() +
  labs(title = "State probabilities of single and multiple domain intevrentions over 5 years", x = "Months", y = "Probability") +
  theme_minimal()

plot4 <- ggplot(filter(data_long, State %in% c('LostToFollowUp')), 
                aes(x = Months, y = Probability, color = State)) +
  geom_line() +
  labs(title = "State probability of lost to follow-up over 5 years", x = "Months", y = "Probability") +
  theme_minimal()

# Arrange the four plots 
ggarrange(plot1, plot2, plot3, plot4, 
          ncol = 2, nrow = 2, 
          labels = c("A", "B", "C", "D"))



## cumulative costs - health services and out-of-pocket stacked plot

data_long_cost<- markov_data %>%
  pivot_longer( cols = c("HealthServices", "OutOfPocket"),
    names_to = "Category",    
    values_to = "Cost")

# Renamed stage to months 
data_long_cost <- data_long_cost %>%
  rename(Months = Stage)


ggplot(data_long_cost, aes(x = Months, y = Cost, fill = Category)) +
  geom_area(alpha = 0.7) +labs(title = "Health services and out of pocket cumulative costs", x = "Months", y = "Cost (AUD)", fill = "Category") +
  scale_fill_manual(values = c("HealthServices" = "#498C1C", "OutOfPocket" = "#F47FC5")) + theme_minimal()


# Normalise cost to sum to 1 at each month
data_long_cost <- data_long_cost %>%
  group_by(Months) %>%
  mutate(Cost = Cost / sum(Cost))

ggplot(data_long_cost, aes(x = Months, y = Cost, fill = Category)) +
  geom_area(alpha = 0.7) +
  labs(title = "Health services and out of pocket cumulative costs", 
       x = "Months", 
       y = "Proportion",
       fill = "Category") +
  scale_fill_manual(values = c("HealthServices" = "#498C1C", "OutOfPocket" = "#F47FC5")) +
  theme_minimal()


##################################################################################################


# 27.05.2025

# Estimate 95% CI using mean probabilities and beta distribution


alpha <- 1
beta <- 9

ci <- qbeta(c(0.025, 0.975), shape1 = alpha, shape2 = beta)

print(ci)










