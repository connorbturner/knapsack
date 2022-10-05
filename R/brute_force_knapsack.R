#' Brute Force Solution for Knapsack Problem
#'
#' @param x Dataset as a dataframe
#' @param W Kanapsack size
#'
#' @return The knapsack with the largest value of the elements added to the knapsack
#' @export
#'
#' @examples brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
#' brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
#' brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)
#' brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)
#' 
brute_force_knapsack <- function(x, W){
  # Checking for correct Inputs
  has.neg <- apply(x, 2, function(row) any(row < 0))
  if (length(which(has.neg)) != 0){
    stop("Error: Dataframe should be with only positive values")
  }
  
  if (!is.data.frame(x)){
    stop("Error: x is not a data frame")
  }
  
  if (ncol(x) != 2){
    stop("Error: x must contain only 2 columns w and v")
  }
  
  if (names(x[1]) != "w" ||
      names(x[2]) != "v"){
    stop("Column names should be w and v ")
  }
  
  #getting no of elements is the dataframe
  n <-  nrow(x)
  
  # Defining two nul lists to store values and elements of different combinations
  values <- NULL
  element_p <- vector(mode = "list")
  
  # getting df of combinations
  com <- lapply(c(1:(2^n-1)),function(x){ as.integer((intToBits(x)))})
  
  for (item in com){
    #Getting first combination and multiplying with a vector from 1:n to get the raw numbers
    combin <- (item[1:n]) * c(1:n)
    
    #getting the sum of  weight and value of the combination
    s <- apply(x[combin,],2,sum)
    
    # if weight is less than W appending to the predifined lists
    if (s[1] < W){
      s <- unname(s)
      values <- append(values,s[2])
      ele <- unlist(sapply(combin,function(x) {x[x!=0]}))
      element_p <- append(element_p,list(ele))

    }
  }
  #Selecting max value and corresponding elements
  value = max(values)
  value_index <- match(value,values)
  elements <- unlist(element_p[value_index])
  #Creating a named list as the output
  result <- list(Value = value, Elements = elements)
  return(result)
}