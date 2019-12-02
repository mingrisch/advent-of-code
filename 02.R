# Day 2
library(assertthat)

process_code <- function(intcode, startpos) {
  mycode <- intcode
  instruction <- mycode[startpos]
  ## assert that instruction != 99 - we should not have got here 
  p1 <- mycode[startpos + 1]
  p2 <- mycode[startpos + 2]
  target <- mycode[startpos +3]
  
  assert_that(instruction==1 | instruction==2)
  # case instruction == 1 : add operation
  if (instruction == 1) {
    mycode[target] =  mycode[p1] + mycode[p2]
  }
  else if (instruction==2){
    mycode[target] =  mycode[p1] * mycode[p2]
  }
  # case instrucdtion == 2 : multiply operation
  mycode
}
  
  
  
run_code <- function(intcode){
  mycode <- intcode
  startpos = 1 # ffs
  while (intcode[startpos]!=99) {
    process_code(mycode, startpos)
    startpos = startpos+ 4
  }
  mycode
}
  
intcode <- vector() # das ist das programm




# sanity test cases


# test cases frmo day 2
v1 = c(1,0,0,0,99)
v2 = c(2,0,0,0,99)

assert_that(are_equal(run_code(v1), v2))

# (1 + 1 = 2).
2,3,0,3,99 becomes 2,3,0,6,99 (3 * 2 = 6).
2,4,4,5,99,0 becomes 2,4,4,5,99,9801 (99 * 99 = 9801).
1,1,1,4,99,5,6,0,99 becomes 30,1,1,4,2,5,6,0,99.
