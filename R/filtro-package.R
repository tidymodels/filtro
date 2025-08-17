#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rlang
#' @importFrom stats complete.cases
#' @importFrom tidyr pivot_wider
## usethis namespace: end
NULL

#' @importFrom generics fit
#' @export
generics::fit

#' @importFrom generics required_pkgs
#' @export
generics::required_pkgs

# enable usage of <S7_object>@name in package code
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
NULL

# Avoid false positive discovery of missing global variables

utils::globalVariables(c("engine", "parsnip", ".d_overall"))
