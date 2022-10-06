#' @title Brute Force Solution for the Knapsack Problem
#'
#' @param x A data.frame with two columns: w (the weights of the objects) and
#' v (the values of the objects)
#' @param W The maximum weight the knapsack can hold
#'
#' @return A named list with two elements: value (which displays the highest
#' value that can be held in the knapsack) and elements (a vector listing the
#' item numbers in said knapsack)
#'
#' @examples brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
#' brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
#' brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)
#' brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)
#'
#' @export
#'

brute_force_knapsack <- function(x, W){

  # Before we start, we must ensure that the inputs are in the correct format:

  # Checks if W is positive
  if (W <= 0){
    stop("Error: 'W' cannot be less than or equal to 0")
  }

  # Checks if all entries in x are positive
  has.neg <- apply(x, 2, function(row) any(row < 0))
  if (length(which(has.neg)) != 0){
    stop("Error: 'x' contains negative entries")
  }

  # Checks if x is a data.frame
  if (!is.data.frame(x)){
    stop("Error: 'x' is not a data.frame")
  }

  # Checks if x contains only two columns
  if (ncol(x) != 2){
    stop("Error: data.frame 'x' should contain only 2 columns, 'w' and 'v'")
  }

  # Checks if x has proper column names
  if (colnames(x)[1] != "w" || colnames(x)[2] != "v"){
    stop("Error: 'x' contains invalid column names. colnames(x) should be 'w' and 'v'.")
  }

  # First, we set a variable n equal to the number of rows and define an empty
  # vector for combination values and an empty list for element combinations
  n <-  nrow(x)
  valuevec <- c()
  elementlist <- list()

  # Next, we create a data frame consisting of all the possible combination
  # possibilities for our knapsack:
  combinations <- lapply(c(1:(2^n-1)),function(x){ as.integer((intToBits(x)))})

  # Then, we define each combination, find the value and weight for each,
  # determine which ones can fit in the knapsack, and record eligible options:
  for (item in combinations){

    # Defines what elements are in this combination
    combo <- (item[1:n]) * c(1:n)

    # Gives us the total weight and value of this combination
    s <- apply(x[combo,], 2, sum)

    # If the weight of this combination fits is less than W, we append the
    # value to our value vector and append the combination to our element list
    if (s[1] < W){
      s <- unname(s)
      valuevec <- append(valuevec,s[2])
      ele <- unlist(sapply(combo, function(x) {x[x != 0]}))
      elementlist <- append(elementlist, list(ele))
    }
  }

  # Finally, we find the eligible combination with the highest value and
  # return this information as a named list:
  value = max(valuevec)
  valindex <- match(value, valuevec)
  elements <- unlist(elementlist[valindex])
  returnlist <- list("value" = round(value, 0), "elements" = elements)

  return(returnlist)
}
