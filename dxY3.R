dxY3<-function (c0, c1, c2, c3, c4, c5) 
{
  .expr2 <- 3 * c0
  .expr3 <- c1^2
  .expr6 <- c0^2
  .expr7 <- 3 * .expr6
  .expr10 <- 9 * c0
  .expr11 <- c2^2
  .expr16 <- 18 * c0
  .expr17 <- .expr16 * c1
  .expr20 <- 90 * c1
  .expr21 <- .expr20 * c2
  .expr24 <- 45 * c0
  .expr25 <- c3^2
  .expr28 <- 315 * c2
  .expr31 <- 9 * .expr6
  .expr34 <- 45 * .expr3
  .expr37 <- 90 * c0
  .expr38 <- .expr37 * c2
  .expr41 <- 315 * .expr11
  .expr44 <- 630 * c1
  .expr45 <- .expr44 * c3
  .expr48 <- 2835 * .expr25
  .expr51 <- 315 * c0
  .expr52 <- c4^2
  .expr55 <- 2835 * c2
  .expr61 <- .expr37 * c1
  .expr64 <- .expr44 * c2
  .expr67 <- 5670 * c2
  .expr68 <- .expr67 * c3
  .expr71 <- 5670 * c1
  .expr72 <- .expr71 * c4
  .expr75 <- 62370 * c3
  .expr76 <- .expr75 * c4
  .expr79 <- 2835 * c0
  .expr80 <- c5^2
  .expr83 <- 405405 * c4
  .expr88 <- 2 * c0
  .expr102 <- 90 * c2
  .expr111 <- 2 * c1
  .expr131 <- 2 * c2
  .expr153 <- 2 * c3
  .expr173 <- 2 * c4
  .expr191 <- 2 * c5
  .value <- c0^3 + .expr2 * .expr3 + .expr7 * c2 + .expr10 * 
    .expr11 + 15 * .expr11 + .expr17 * c3 + .expr21 * c3 + 
    .expr24 * .expr25 + .expr28 * .expr25 + .expr31 * c4 + 
    .expr34 * c4 + .expr38 * c4 + .expr41 * c4 + .expr45 * 
    c4 + .expr48 * c4 + .expr51 * .expr52 + .expr55 * .expr52 + 
    10395 * c4^3 + .expr61 * c5 + .expr64 * c5 + .expr68 * 
    c5 + .expr72 * c5 + .expr76 * c5 + .expr79 * .expr80 + 
    .expr83 * .expr80
  .grad <- array(0, c(length(.value), 6L), list(NULL, c("c0", 
                                                        "c1", "c2", "c3", "c4", "c5")))
  .grad[, "c0"] <- .expr7 + 3 * .expr3 + 3 * .expr88 * c2 + 
    9 * .expr11 + 18 * c1 * c3 + 45 * .expr25 + 9 * .expr88 * 
    c4 + .expr102 * c4 + 315 * .expr52 + .expr20 * c5 + 2835 * 
    .expr80
  .grad[, "c1"] <- .expr2 * .expr111 + .expr16 * c3 + .expr102 * 
    c3 + 45 * .expr111 * c4 + 630 * c3 * c4 + .expr37 * c5 + 
    630 * c2 * c5 + 5670 * c4 * c5
  .grad[, "c2"] <- .expr7 + .expr10 * .expr131 + 15 * .expr131 + 
    .expr20 * c3 + 315 * .expr25 + .expr37 * c4 + 315 * .expr131 * 
    c4 + 2835 * .expr52 + .expr44 * c5 + 5670 * c3 * c5
  .grad[, "c3"] <- .expr17 + .expr21 + .expr24 * .expr153 + 
    .expr28 * .expr153 + .expr44 * c4 + 2835 * .expr153 * 
    c4 + .expr67 * c5 + 62370 * c4 * c5
  .grad[, "c4"] <- .expr31 + .expr34 + .expr38 + .expr41 + 
    .expr45 + .expr48 + .expr51 * .expr173 + .expr55 * .expr173 + 
    10395 * (3 * .expr52) + .expr71 * c5 + .expr75 * c5 + 
    405405 * .expr80
  .grad[, "c5"] <- .expr61 + .expr64 + .expr68 + .expr72 + 
    .expr76 + .expr79 * .expr191 + .expr83 * .expr191
  attr(.value, "gradient") <- .grad
  .value
}