#' @title A Dynamic Programming Model for the Knapsack Problem
#'
#' @param x A data.frame with two columns: w (the weights of the objects) and
#' v (the values of the objects)
#' @param W The maximum weight the knapsack can hold
#'
#' @return A named list with two elements: value (which displays the highest
#' value that can be held in the knapsack) and elements (a vector listing the
#' item numbers in said knapsack)
#'
#' @examples knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
#'
#' @export
#'

knapsack_dynamic <- function(x, W){

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

  # First, we create a variable n to indicate the number of rows and two
  # matrices: mat will store the current values as we go through the loop and
  # mem will serve as our dynamic memory:
  n <- nrow(x)
  mat <- matrix(0, nrow = (n + 1), ncol = (W + 1))
  mem <- matrix(0, nrow = 1, ncol = (n + 1))
  w = as.vector(x[, "w"])
  v = as.vector(x[, "v"])

  # Next, we iterate through each row in x and viable weight up to W to fill
  # out our two matrices:
  for (i in 2:(n + 1)){
    for (j in 1:W){

      # If the weight of the current object is less than the current weight
      # (defined by j), we skip over it
      if (w[i] > j || i == (n + 1)){
        mat[i, j] <-  mat[(i - 1), j]
      }

      # If the weight is less than j, we determine if the value is worth
      # placing it in the knapsack
      else{
        mat[i, j] <-
          max(mat[(i - 1), j],
              mat[(i - 1), (j - w[i])] + v[i])

        # We then record this entry in memory if it is not currently recorded
        if ((mat[i, j] %in% mem) == FALSE && i != (n + 1)){
          prevval <- mat[(i - 1), (j - w[i])]
          previndex <- match(prevval, mem)
          addmemrow <- c(mat[i, j], mem[previndex, 2:(n + 1)])
          addmemrow[i + 1] <- 1
          mem <- rbind(mem, addmemrow)
        }
      }
    }
  }

  # Finally, we pull our maximum value from mat, pull our optimal combination
  # from mem, and return the results in a named list:
  value <- mat[n, W]
  index <- match(mat[n, W], mem[, 1])
  combination <- mem[index, 2:(n + 1)]
  elements <- 1:n
  elements <- elements[which(combination == 1)]

  returnlist = list("value" = round(value, 0), "elements" = elements)
  return(returnlist)
}
