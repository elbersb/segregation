
boot_se <- function(x) {
  n_boot <- length(x)
  1 / (n_boot - 1) * sum((x - mean(x))^2)
}
