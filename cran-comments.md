## R CMD check results

0 errors | 0 warnings | 1 note

* NOTE: CRAN incoming feasibility reports the URL https://ror.org/03wc8by49 (in `man/filtro-package.Rd`) as invalid with status 429 (Too Many Requests). The ror.org web page returns 429 to automated requests, but the ID is valid and resolves at https://api.ror.org/v2/organizations/03wc8by49.

This is a patch release that fixes the errors and the Rd `\usage` note reported on the CRAN check page for r-devel-linux. It restores the scoring generics when used with the development version of S7, which fixes the `could not find function "fit"` errors in examples and vignettes. It also removes development notices from the documentation and adds the "Supported By Posit" badge to the package website.
