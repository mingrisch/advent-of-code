# day one

library(tidyverse)


# data was downloaded manually.

# part 1 
df <- read_csv('01-input.txt', col_names = "mass") %>% 
  mutate(fuel_req = (mass %/% 3) - 2) %>% 
  summarise(demand = sum(fuel_req))


# part 2:


modules_df  <- read_csv('01-input.txt', col_names = "module")
modules <- modules_df$module

fuel_amount <- function(fuel){
  fuel %/% 3 -2
}

fuel_requirement <- function(module){
  
  fuel_amounts = vector()
  fr = module
  while (fr>0) {
    fr = fuel_amount(fr)
    if (fr>0) fuel_amounts=c(fuel_amounts, fr)
    #print(fr)

  }
  sum(fuel_amounts)
}

# test cases:
fuel_requirement(1969)
fuel_requirement(100756)

# now we loop over all modules
# we are using purrr::map, ffs

required_fuels <- purrr::map_dbl(modules, fuel_requirement)

required_fuel <- sum(required_fuels)
