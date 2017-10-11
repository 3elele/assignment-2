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
  if (!is.null(x)) { # Test the existence of the column
    if (is.numeric(x)) { #Test if x contains numeric values and...
      sum(x) -> result #...if true we sum all values to get the result
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
  if (is.numeric(x)) { #X numerical verification...
    s <- 0 #Establishing a sum variable
    for(v in 1:length(x)) { #...if true we iterate on its values...
      s <- s + x[v] #...and we add every value to our result
    }
    result <- s
  }
  return(result)
}
#The output should be the following:
## [1] 876.5
## NULL
## [1] 1520

#----------1c

#Write a function called sum_divided_by that takes two arguments: a vector x and a number k. It returns the sum of the elements of x divided by the number k. If either x or k are non-numeric, it should return NULL. You can do anything you want, with one requirement: to calculate the sum of the elements in x, you must use your function my_sum. Make sure you write a comment at the top, and, in this case too, write a comment above every line explaining what it does.
#In order to verify that your function works, add a new chunk to your Rmd that contains the following code:
#The output should be the following:
  ## [1] 73.04167
  ## NULL
  ## NULL
  ## [1] -126.6667
#  (Technically, in R, all numbers are vectors that simply happen to be of length one. This means that you could accidentally put a longer vector in place of k, instead of just a number, and you wouldn’t know there was a problem until you looked at the strange result. You don’t need to worry about this problem. Just make sure that your comment at the top of the function explains what k is supposed to be.)