dxY2<-function (c0, c1, c2, c3, c4, c5)
{
  .expr5 <- 24 * c2
  .expr8 <- 6 * c1
  .expr10 <- c3 + 5 * c5
  .expr18 <- 70 * c3
  .value <- c1^2 + 2 * c2^2 + .expr5 * c4 + .expr8 * .expr10 +
    3 * (5 * c3^2 + 32 * c4^2 + .expr18 * c5 + 315 * c5^2)
  .grad <- array(0, c(length(.value), 6L), list(NULL, c("c0",
                                                        "c1", "c2", "c3", "c4", "c5")))
  .grad[, "c0"] <- 0
  .grad[, "c1"] <- 2 * c1 + 6 * .expr10
  .grad[, "c2"] <- 2 * (2 * c2) + 24 * c4
  .grad[, "c3"] <- .expr8 + 3 * (5 * (2 * c3) + 70 * c5)
  .grad[, "c4"] <- .expr5 + 3 * (32 * (2 * c4))
  .grad[, "c5"] <- .expr8 * 5 + 3 * (.expr18 + 315 * (2 * c5))
  attr(.value, "gradient") <- .grad
  .value
}
