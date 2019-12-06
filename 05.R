# Day 2
library(assertthat)

get_parameter_value <- function(intcode, position, mode){
  if (mode=="0") {return(intcode[position])}
  else if (mode=="1") {
    target = intcode[position]
    return(intcode[target])
  }
}

process_code <- function(intcode, startpos,input=input) {
  mycode <- intcode
  instruction <- mycode[startpos]
  #to do:
  # determine opcode (last two digits)
  opcode = instruction%%100
  modes = instruction %/% 100
  
  if (opcode %in% c(1,2)) {
    modes = formatC(modes, width=3, format="d", flag = 0)
  }
  
  else if (opcode %in% c(3,4)) {
    modes = formatC(modes, width=1, format="d", flag = 0)
  }
  

  # this is, like, pointer handling. fuck.
  # 
  
  # if (instruction==99){
  #   output = "HALT"
  #   increment = 0
  #   }

  p1 <- mycode[startpos + 1] +1
  p2 <- mycode[startpos + 2] +1 
  target <- mycode[startpos +3] +1 
  output=NA
  # assert_that(instruction==1 | instruction==2)
  # case instruction == 1 : add operation
  if (opcode == 1) {
    mycode[target] =  mycode[p1] + mycode[p2]
    increment = 4
  }
  else if (opcode==2){
    mycode[target] =  mycode[p1] * mycode[p2]
    increment = 4
  }
  else if (opcode==3){
    mycode[p1] = input 
    increment = 2
  }
  else if (opcode==4){
    output=mycode[mycode[startpos+1]+1]
    increment = 2
  }
  # case instrucdtion == 2 : multiply operation
  print(glue::glue("Instruction {instruction} yields output {output}"))
  list(mycode, increment, output)
} 

  
  
  
run_code <- function(intcode, input=NA){
  mycode <- intcode
  startpos = 1 # ffs
  while (mycode[startpos]!=99) {
    return_value  = process_code(mycode, startpos,input=input)
    mycode=return_value[[1]]
    increment = return_value[[2]]
    output = return_value[[3]]
    startpos = startpos+ increment
  }
  print(glue::glue("Output is {output}"))
  mycode
}


#run_code(c(99))
# sanity test cases


# test cases frmo day 2
v1 = c(1,0,0,0,99)
v2 = c(2,0,0,0,99)


# test cases for instruction 3
assert_that(are_equal(run_code(c(3,1,99), input = 1), c(3,1,99)))
assert_that(are_equal(run_code(c(3,0,99), input = 0), c(0,0,99)))

# test cases for instruction 4 - o shit
# run_code(c(4,4,0,0,99)) # output should be 4

assert_that(are_equal(run_code(c(3,0,4,0,99), input =27), c(27,0,4,0,99)))



# proceed with parametric mode. 



#run_code(v1)
assert_that(are_equal(run_code(v1), v2))
assert_that(are_equal(run_code(c(2,3,0,3,99)), c(2,3,0,6,99)))
assert_that(are_equal(run_code(c(2,4,4,5,99,0)), c(2,4,4,5,99,9801)))
assert_that(are_equal(run_code(c(1,1,1,4,99,5,6,0,99)), c(30,1,1,4,2,5,6,0,99)))




intcode <- as.integer(scan('input-05.txt', what="integer", sep =","))

run_code(intcode, input=1)
