# Cara Gallagher
# July 17th, 2023
# Bat sensory network model
# Try-it-yourself demo


##################################################
# Packages:
library(tidyverse)

##################################################

#### Load and prepare outputs #### 

# First load output file
## A number of initial rows are skipped as NetLogo provides header information at the start of table output files
Outputs <- read_csv("TIY_outfile.csv", skip = 6)

# Select the relevant columns
Outputs <- Outputs %>% 
  select("[run number]", "n-bats", "no-patch", "network-sizes", "nearest-neighbor-distance", "food-found-ticks-list")

# Rename columns
colnames(Outputs) <- c("runNumber", "nBats","nPatch", "networkSizes", "nearestNeighborDistance", "indFoodFound")

#### Outputs

#### Network sizes #### 

NetSizeOut <- Outputs %>% 
  select(runNumber, nBats, nPatch, networkSizes)
  
# separate list into individual points
## this will take only the first 9000 points and discard the rest
## but this is fine for demonstration purposes - all can be included in full analyses
nam <- c(as.character(1:9000))

# as output is composed of sublists of: 
## 1) the timestep of the datapoint (ticks), 
## 2) the ID of the simulated bat,
## and 3) the network size
# outputs must be processed before plotting
NetSizeOut <- NetSizeOut %>% 
  separate(networkSizes, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -runNumber, -nBats, -nPatch) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(runNumber, num) %>% 
  drop_na() %>% 
  group_by(runNumber) %>% 
  mutate(order = rep(1:3, times = (max(row_number()) / 3)), num = rep(1:(max(row_number()) / 3), each = 3)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(ticks = `1`, ID = `2`, networkSizes = `3`) %>% 
  group_by(nBats, nPatch) %>% 
  summarise(medianNS = median(networkSizes), NS25 = quantile(networkSizes, probs = 0.25), NS75 = quantile(networkSizes, probs = 0.75))

# plot outputs
# plots show the median value as a point and the 25-75% CI as a linerange
NetSizeOutPlot <- ggplot(NetSizeOut, aes(x = as.factor(nPatch), col = as.factor(nBats))) +
  geom_linerange(aes(ymin = NS25, ymax = NS75), linewidth = 1, position=position_dodge(width = 0.75)) +
  geom_point(aes(y = medianNS), size = 2.5, position=position_dodge(width = 0.75)) +
  theme_classic() +
  theme(text = element_text(size = 16, color = "grey10")) +
  labs(x = "Resource patch density (N patches)", y = "Network size (N bats)", col = "Number of\nsimulated bats")
NetSizeOutPlot


#### Nearest neighbor distance #### 

NNDOut <- Outputs %>% 
  select(runNumber, nBats, nPatch, nearestNeighborDistance)

# separate list into individual points
## this will take only the first 9000 points and discard the rest
## but this is fine for demonstration purposes - all can be included in full analyses
nam <- c(as.character(1:9000))

# as output is composed of sublists of: 
## 1) the timestep of the datapoint (ticks), 
## 2) the ID of the simulated bat,
## and 3) the distance to the nearest bat
# outputs must be processed before plotting
NNDOut <- NNDOut %>% 
  separate(nearestNeighborDistance, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -runNumber, -nBats, -nPatch) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(runNumber, num) %>% 
  drop_na() %>% 
  group_by(runNumber) %>% 
  mutate(order = rep(1:3, times = (max(row_number()) / 3)), num = rep(1:(max(row_number()) / 3), each = 3)) %>% 
  pivot_wider(names_from = order, values_from = value) %>% 
  rename(ticks = `1`, ID = `2`, nearestNeighborDistance = `3`) %>% 
  group_by(nBats, nPatch) %>% 
  summarise(medianNND = median(nearestNeighborDistance), NND25 = quantile(nearestNeighborDistance, probs = 0.25), NND75 = quantile(nearestNeighborDistance, probs = 0.75))

# plot outputs
# plots show the median value as a point and the 25-75% CI as a linerange
NNDOutPlot <- ggplot(NNDOut, aes(x = as.factor(nPatch), col = as.factor(nBats))) +
  geom_linerange(aes(ymin = NND25, ymax = NND75), linewidth = 1, position=position_dodge(width = 0.75)) +
  geom_point(aes(y = medianNND), size = 2.5, position=position_dodge(width = 0.75)) +
  theme_classic() +
  theme(text = element_text(size = 16, color = "grey10")) +
  labs(x = "Resource patch density (N patches)", y = "Distance to nearest\nneighbor (m)", col = "Number of\nsimulated bats")
NNDOutPlot


#### Time until individual bats found a food cell #### 

TimeIndOut <- Outputs %>% 
  select(runNumber, nBats, nPatch, indFoodFound)

# as output is composed of a list of ticks where bats found food
# outputs must be processed before plotting
nam <- c(as.character(1:max(TimeIndOut$nBats)))

TimeIndOut <- TimeIndOut %>% 
  separate(indFoodFound, nam, sep = " ") %>% 
  gather(key = "num", value = "value", -runNumber, -nBats, -nPatch) %>% 
  mutate(value = str_replace(value,"\\[",""), value = str_replace(value,"\\]",""),
         value = as.numeric(value),
         num = as.numeric(num)) %>% 
  arrange(runNumber, num) %>% 
  drop_na() %>% 
  rename(indFoodFound = value) %>% 
  mutate(indFoodFound = indFoodFound / 7.5) %>%  # convert to minutes
  group_by(nBats, nPatch) %>% 
  summarise(medianFF = median(indFoodFound), FF25 = quantile(indFoodFound, probs = 0.25), FF75 = quantile(indFoodFound, probs = 0.75))

# plot outputs
# plots show the median value as a point and the 25-75% CI as a linerange
TimeIndOutPlot <- ggplot(TimeIndOut, aes(x = as.factor(nPatch), col = as.factor(nBats))) +
  geom_linerange(aes(ymin = FF25, ymax = FF75), linewidth = 1, position=position_dodge(width = 0.75)) +
  geom_point(aes(y = medianFF), size = 2.5, position=position_dodge(width = 0.75)) +
  theme_classic() +
  theme(text = element_text(size = 16, color = "grey10")) +
  labs(x = "Resource patch density (N patches)", y = "Time until individual bats found\na resource cell (mins)", col = "Number of\nsimulated bats")
TimeIndOutPlot

  
#### All plots ####
NetSizeOutPlot
NNDOutPlot
TimeIndOutPlot
