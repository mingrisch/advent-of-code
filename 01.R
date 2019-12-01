# day one

library(tidyverse)

# data was downloaded manually.

df <- read_csv('01-input.txt', col_names = "mass") %>% 
  mutate(fuel_req = (mass %/% 3) - 2) %>% 
  summarise(demand = sum(fuel_req))

# part 2:

demand = df$demand

input = demand

requirement = input %/% 3 -2

while (requirement>0) {
  input = input + requirement
  requirement = input %/% 3 -2
  
  print(requirement)
}
