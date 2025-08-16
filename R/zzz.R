.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "letsRept ", utils::packageVersion(pkgname), " loaded \U1F98E \U1F40D \U1F40A \U1F422\n",
    "Please cite both letsRept and The Reptile Database when using this package.\n",
    "Run citation(\"letsRept\") for details."
  )
}