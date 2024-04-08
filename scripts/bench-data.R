library(survival)

# Helpers

# Resample rows to create an augmented dataset,
# with replacement. Take only numeric vars.
gen_data <- function(n_row) {
  data(diabetic, package="survival")

  cs <- sapply(diabetic, FUN = \(x) is.numeric(x) || is.integer(x))
              
  rs <- sample.int(nrow(diabetic), n_row, replace = TRUE)

  diabetic[rs, cs]

}

# COXPH artificial dataset



d <- gen_data(1000) 

write.csv(d, "./data/bench-data.csv", row.names = FALSE) 


