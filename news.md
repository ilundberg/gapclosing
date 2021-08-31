# NEWS file for the gap closing package

# quantileplot 1.0.1
* Patch a coding error in cross-fitting ranger estimation.
* Modify fit_ridge to return the fitted model object in parallel to other estimators.

# quantileplot 1.0.0
* First stable release in preparation to submit to CRAN.
* This update reduces computational complexity of the vignette.
* Small changes to comply with R CMD check.
* Increment version number to mark first stable release

# quantileplot 0.0.3
* This development version is available on GitHub.
* This is a beta version and the API may change before submission to CRAN.
* This update adds functionality to coerce a gapclosing object to a data frame of estimates, as well as functionality to return to a list. It also simplifies the bootstrap code by making use of these functions.

# quantileplot 0.0.2
* This development version is available on GitHub.
* This is a beta version and the API may change before submission to CRAN.
* This update changes the code base from base R to tidyverse.
* The plot_two_categories() function is now disparityplot()
* The plot(), print(), and summary() functionality has changed
* The bootstrap default is now a simple random sample rather than stratified

# quantileplot 0.0.1.9000
* This development version is available on GitHub.
* This is a beta version and the API may change before submission to CRAN.
