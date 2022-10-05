mem <- NULL
knapsack_dynamic <- function(x, W){
  
  # Checking for correct Inputs
  has.neg <- apply(x, 2, function(row) any(row < 0))
  if (length(which(has.neg)) != 0){
    stop("Error: Dataframe should be with only positive values")
  }
  if (W<0){
    stop("Error: Knapzack size cannot be negative")
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
  
  
  if(length(mem) != 0){
    return(mem)
  }
  n <- nrow(x)
  if (n==0 || W ==0){
    result <- 0
  }
  else if(x$w[n] > W){
    x <- x[-nrow(x),]
    result <- knapsack_dynamic(x,W)
  }
  else{
    #Getting values of the nth element
    value <- x$v[n]
    weight <- x$w[n]
    #Removing nth row from the data frame
    x <- x[-nrow(x),]
    
    v1 <- knapsack_dynamic(x,W)
    v2 <- value + knapsack_dynamic(x,W - weight)
    
    result <- max(v1,v2)
    
  }
  mem <- append(mem,result)
  return(result)
}

knapsack_dynamic (x = knapsack_objects[1:8,], W = 3500)
#' knapsack_dynamic(x = knapsack_objects[1:12,], W = 3500)
#' knapsack_dynamic(x = knapsack_objects[1:8,], W = 2000)
#' knapsack_dynamic(x = knapsack_objects[1:12,], W = 2000)


