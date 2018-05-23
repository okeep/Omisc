dxY1<-function (c0, c1, c2, c3, c4, c5) 
{
  .value <- c0 + c2 + 3 * c4
  .grad <- array(0, c(length(.value), 6L), list(NULL, c("c0", 
                                                        "c1", "c2", "c3", "c4", "c5")))
  .grad[, "c0"] <- 1
  .grad[, "c1"] <- 0
  .grad[, "c2"] <- 1
  .grad[, "c3"] <- 0
  .grad[, "c4"] <- 3
  .grad[, "c5"] <- 0
  attr(.value, "gradient") <- .grad
  .value
}