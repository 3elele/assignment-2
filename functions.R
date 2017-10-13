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
    k <- length(v) #Creating a variable k which corresponds to the vector length
    mean <- sum_divided_by(v, k) #Calling the sum_divided_by function...
    #...with our vector and our number as args in order to calculate the vector mean
  }
  return(mean)
}
#It should give the output:
  ## [1] 5.843333
  ## NULL
  ## [1] 28.14815

##----------2a
# Return a violin plot.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the dependent variable, provided as a
#     string
# grouping_var: the name of a column of d containing a grouping variable,
#               provided as a string
#
# RETURN VALUE:
# A ggplot plot object containing a violin plot, grouped by the values
# of the grouping variable.
#
grouped_violin_plot <- function(d, var, grouping_var) {
  # Create the base ggplot object
  p <- ggplot2::ggplot(d, ggplot2::aes_string(y=var,
                                              x=grouping_var,
                                              fill=grouping_var))
  p <- p + ggplot2::geom_violin() #Add Violin plot visualisation
  return(p)
}

###----------3a
# Difference in the medians between two groups.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the dependent variable, provided as a string
# grouping_var: the name of a column of d containing a grouping variable, provided as a string
# group1: the value of grouping_var that corresponds to the first group
# group2: the value of grouping_var that corresponds to the second group
#
# RETURN VALUE:
# The median value of var for the first group, minus the median value of var for the second
# group.
#
difference_in_medians <- function(d, var, grouping_var, group1, group2) {
  d_1 <- dplyr::filter(d, get(grouping_var) == group1) #Sorting the column we want to acces the data
  d_2 <- dplyr::filter(d, get(grouping_var) == group2) #Same column sorting for group2
  med_1 <- median(unlist(d_1[var])) #Converting our column (list) into a vector
  med_2 <- median(unlist(d_2[var])) #And converting our vector in a numerical vector to be sure
  result <- med_1 - med_2 #Difference between the two medians applied to result variable
  return(result)
}
#It should give the following output:
  ## [1] -0.2
  ## [1] 0

###----------3b
# Randomize the order of a column.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the variable to randomize,
#      provided as a string
#
# RETURN VALUE:
# A data frame or tibble exactly the same as d, except with the order of
# var permuted randomly.
#
randomize <- function(d, var) {
  d[[var]] <- sample(d[[var]])
  return(d)
}
#You should get the following output:
  ##  [1] 3.5 3.0 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1
  ##  [1] 3.4 2.8 3.0 2.8 3.2 2.8 3.4 2.7 2.5 2.9
  ##  [1] versicolor versicolor setosa     versicolor versicolor setosa
  ##  [7] versicolor setosa     setosa     setosa
  ## Levels: setosa versicolor virginica
  ##  [1] 3.5 3.0 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1

###----------3c
# Perform a permutation test for two groups.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of the column in d on which the test statistic will be calculated,
#      provided as a string
# grouping_var: the name of the column in d which gives the grouping
# group1: the value of grouping_var corresponding to the first group
# group2: the value of grouping_var corresponding to the second group
# statistic: a function yielding a test statistic, which takes as input
#            a data frame, the name of a variable on which to calculate the
#            test statistic, the name of a grouping variable, the value of
#            the grouping variable corresponding to the first group, and
#            the value of the grouping variable corresponding to the second
#            group
# n_samples: the number of permutation samples to draw (default: 9999)
#
# RETURN VALUE:
#
# A list containing two elements:
#
#  - observed: the value of statistic() in d
#  - permuted: a vector containing the values of statistic() under n_samples
#              permutations
#
permutation_twogroups <- function(d, var, grouping_var, group1, group2, statistic,
                                  n_samples=9999) {
  observed_statistic <- statistic(d, var, grouping_var, group1, group2)
  permutation_statistics <- rep(0, n_samples)
  for (i in 1:n_samples) {
    permutations <- randomize(d, var)
    permutation_statistics[i] <- statistic(permutations, var, grouping_var, group1, group2) 
  }
  result <- list(observed=observed_statistic,
                 permuted=permutation_statistics)
  return(result)
}
#You should get the following output.
  ## $observed
  ## [1] -0.2
  ## $permuted
  ##  [1]  0.10 -0.05  0.00  0.00  0.00  0.00  0.00  0.00  0.10 -0.10
  ##
  ## $observed
  ## [1] -0.2
  ## $permuted
  ##  [1]  0.00  0.00  0.00  0.00  0.05  0.00 -0.10  0.05  0.00 -0.05
  ##
  ## $observed
  ## [1] 0
  ## $permuted
  ##  [1]  0.00  0.00  0.00  0.00  0.00  0.00 -0.05  0.00  0.00  0.00
  ## [1] 0
  ## [1] 0