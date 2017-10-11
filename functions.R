#----------1a
# Sum values in a column of a data frame.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d, provided as a string
#
# RETURN VALUE:
# if the specified column exists and contains numbers, returns the sum of
# all values in the column; otherwise, returns NULL
sum_column <- function(d, var) {
  result <- NULL # Set a default value to return
  x <- d[[var]] # Access to a column in d
  if(!is.null(x)) { # Test the existence of the column
    if(is.numeric(x)) { #Test if x contains numeric values and...
      my_sum(x) -> result #...if true we sum all values to get the result
    }
  }
  return(result)
}
#The output should be the following:
## [1] 876.5
## NULL
## [1] 1520

#----------1b
# Sum values in a vector.
#
# ARGUMENTS:
# x: a vector
#
# RETURN VALUE:
# if the vector contains numbers, returns the sum of
# all values; otherwise, returns NULL
my_sum <- function(x) {
  result <- NULL #default value to return
  if(is.numeric(x)) { #X numerical verification...
    future_sum <- 0 #Establishing a sum variable
    for(v in 1:length(x)) { #...if true we iterate on its length starting from index 1...
      future_sum <- future_sum + x[v] #...and we add every value to our sum
    }
    result <- future_sum #Result become sum
  }
  return(result)
}
#The output should be the following:
## [1] 876.5
## NULL
## [1] 1520

#----------1c
# Sum values in a vector and divide the result by a number.
#
# ARGUMENTS:
# x: a vector
# k: a number
#
# RETURN VALUE:
# if the vector contains numbers, returns the sum of
# all values divided by the number; otherwise, returns NULL
sum_divided_by <- function(x, k) {
  result <- NULL #Initializing the result to NULL
  if (is.numeric(x) && is.numeric(k) && length(k)==1) { #Verifying if x and k have numeric values and k is just 1 number
    result <- my_sum(x)/k #Applying my_sum to the vector and dividing by k
  }
  return(result)
}
#The output should be the following:
  ## [1] 73.04167
  ## NULL
  ## NULL
  ## [1] -126.6667

#----------1d
# Calculate the mean of a vector.
#
# ARGUMENTS:
# x: a vector
# 
# RETURN VALUE:
# if the vector contains numbers, returns its mean:
# the sum all values divided by the vector length; otherwise, returns NULL
my_mean <- function(v) {
  mean <- NULL
  if(is.numeric(v)) {
    k <- length(v)
    mean <- sum_divided_by(v, k)
  }
  return(mean)
}
#Write your own function my_mean to calculate the mean of a vector (the sum divided by the number of elements), making use of your function sum_divided_by, and not making use of the built-in functions that calculate the sum or the mean. It should return NULL for non-numeric vectors
#It should give the output:
  ## [1] 5.843333
  ## NULL
  ## [1] 28.14815