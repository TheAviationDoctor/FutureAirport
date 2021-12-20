# Define the function

fn_test <- function(i) {

  W <- 9.806665 * dt[i, m]
  todr <- W / 2

}

# Load the data

dt <- fread("output.csv")

dt[, todr:= sapply(1:nrow(dt), fn_test)]

dt
