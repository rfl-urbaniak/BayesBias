library(cmdstanr)

install_cmdstan(
  dir = NULL,
  cores = getOption("mc.cores", 2),
  quiet = FALSE,
  overwrite = FALSE,
  version = NULL,
  release_url = NULL,
  cpp_options = list(),
  check_toolchain = TRUE
)