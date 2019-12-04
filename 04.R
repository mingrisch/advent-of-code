# day v4 - six digit passwords
library(stringr)
 
minpwd = 236491
maxpwd = 713787

validpwds = vector()


is_valid <- function(cand){
  v_cand = as.integer(unlist(str_split(as.character(cand),"")))
  diff_cand = v_cand[1:5] - v_cand[2:6]
  # length 6
  if (length(v_cand)==6){
    # never decreasing
    if (all(diff_cand<=0)){
      # two adjacent numbers equal
      if (any(diff_cand==0)){
          print(cand)
          return(TRUE)
      
      }
    }
  }
  FALSE
}




assert_that(!is_valid(11123))
assert_that(is_valid(111111))
assert_that(!is_valid(223450))
assert_that(!is_valid(123789))


# solution part 1
for (candidate in seq(minpwd, maxpwd,1)){
  if (is_valid(candidate)) {
    validpwds = c(validpwds, candidate)
  }
}


#validpwds

print(length(validpwds))

# part2


# solution part 2

validpwds = vector()
is_valid_2 <- function(cand){
  v_cand = as.integer(unlist(str_split(as.character(cand),"")))
  diff_cand = v_cand[1:5] - v_cand[2:6]
  # length 6
  if (length(v_cand)==6){
    # never decreasing
    if (all(diff_cand<=0)){
      # two adjacent numbers equal
      if (any(diff_cand==0)){
        # only two adjacent numbers
        # (find a single zero in the difference vector)
        #if (str_detect(str_c(as.character(abs(diff_cand)), collapse=''),"[1-9,^]*0[1-9,$]*")){
        if (str_detect(str_c(as.character(abs(diff_cand)), collapse=''),"(^|[^0])0($|[^0])")){
            print(cand)
          return(TRUE)
          
        }
      }
    }
  }
  FALSE
}

assert_that(is_valid_2(112233))
assert_that(!is_valid_2(123444))
assert_that(is_valid_2(111122))
assert_that(is_valid_2(113445))


for (candidate in seq(minpwd, maxpwd,1)){
  if (is_valid_2(candidate)) {
    validpwds = c(validpwds, candidate)
  }
}


#validpwds

print(length(validpwds))
_