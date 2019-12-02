# Day 2
library(assertthat)

process_code <- function(intcode, startpos) {
  mycode <- intcode
  instruction <- mycode[startpos]
  ## assert that instruction != 99 - we should not have got here 
  p1 <- mycode[startpos + 1] +1
  p2 <- mycode[startpos + 2] +1 
  target <- mycode[startpos +3] +1 
  
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
  while (mycode[startpos]!=99) {
    mycode = process_code(mycode, startpos)
    startpos = startpos+ 4
  }
  mycode
}


#run_code(c(99))
# sanity test cases


# test cases frmo day 2
#v1 = c(1,0,0,0,99)
#v2 = c(2,0,0,0,99)

#run_code(v1)
assert_that(are_equal(run_code(v1), v2))
assert_that(are_equal(run_code(c(2,3,0,3,99)), c(2,3,0,6,99)))
assert_that(are_equal(run_code(c(2,4,4,5,99,0)), c(2,4,4,5,99,9801)))
assert_that(are_equal(run_code(c(1,1,1,4,99,5,6,0,99)), c(30,1,1,4,2,5,6,0,99)))


# solution
intcode <- as.integer(scan('02-input.txt', what="integer", sep =","))
intcode[2]=12
intcode[3]=2
run_code(intcode)

# part2

moonlander <- function(noun, verb){
  intcode <- as.integer(scan('02-input.txt', what="integer", sep =","))
  intcode[2] = noun
  intcode[3] = verb
  intcode = run_code(intcode)
  intcode[1]
}
assert_that(are_equal(moonlander(12,2),9581917))


# objective: find (noun, verb)  for the result 19690720
for (i in seq(0, 200)) {
  for (j in seq(0,200)){
    res = moonlander(i, j)
    if (res == 19690720) {
      print(c(i,j))
      return()
    }
  
  }
}

print(100*i+j)
