#06 
library(data.tree)
library(tidyverse)


testcase <- read_delim("06-test.csv", delim = ")", col_names = c("node","child")) 

tree <- data.tree::FromDataFrameNetwork(testcase)

# how many nodes above node? get number of parent nodes
orbit_count <- sum(tree$Get("level")-1)

orbitcount <- function(inputfile){
  df <- read_delim(inputfile, delim=")", col_names=c("node","child"))
  tree <- data.tree::FromDataFrameNetwork(df)
  sum(tree$Get("level")-1)
}

assert_that(are_equal( orbitcount("06-test.csv"),42))

solution = orbitcount("input06.txt")


# part 2
orbits_df <- read_delim("input06.txt", delim=")", col_names = c("node","child"))
orbits = read_delim("input06.txt", delim = ")", col_names = c("node","child")) %>% 
  FromDataFrameNetwork()

pathyou <- orbits$FindNode(name="YOU")$path
pathsan <- orbits$FindNode(name="SAN")$path

orbit_transfers <- function(node){
  you <- FindNode(node, name="YOU")
  san <- FindNode(node, name="SAN")
  
  if (is.null(you) | is.null(san)) return (NA)
  
  #unfortunately, this does not work properly
  # we want the path from node to you/san, these are the root paths
   pathyou <- you$path
   pathsan <- san$path
   
  lyou <- length(pathyou)-1
  lsan <- length(pathsan)-1
  
  result = lyou + lsan

  result
}

orbits$Get(orbit_transfers)
ot <- orbit_transfers(orbits)

print(min(ot))

for (i in seq_along(pathyou)){
  if (pathyou[i]==pathsan[i]) {
    print(c(i,pathyou[i], pathsan[i], length(pathyou)-i, length(pathsan)-i,length(pathyou)-i + length(pathsan)-i -2) )
  }
} 

  