
# added in just to stop notes/errors with devtools::check()
utils::globalVariables(c("yhat", "lower", "upper"))

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Thank you for using rmsMD.\n",
    "Please cite the following paper if you use this package:\n",
    "Tingle SJ, Kourounis G, Elliot S, Harrison EM. ",
    "\"Non-linear regression modelling for medical professionals; making curved paths straightforward.\" ",
    "Postgraduate Medical Journal, qgaf183. 3 Nov. 2025, doi:10.1093/postmj/qgaf183\n"
  )
}
