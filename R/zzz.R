.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "letsRept ", utils::packageVersion(pkgname), " loaded 🦎\n",
    "Please cite both letsRept and The Reptile Database when using this package.\n",
    "Run citation(\"letsRept\") for details."
  )
}