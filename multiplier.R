multiplier <- function(x) { 
  m = 1
  if (x == "H") {
    m = 10
  }
  if (x == "K") {
    m = 1000
  }
  if (x == "M") {
    m = 1000000
  }
  if (x == "B") {
    m = 1000000000
  }
  m
}
