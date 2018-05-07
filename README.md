estprod
==========

[![Build Status](https://travis-ci.org/rrremedio/estprod.svg?branch=master)](https://travis-ci.org/rrremedio/estprod) [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/estprod)](https://cran.r-project.org/package=estprod)
[![CRAN_Downloads_Badge](https://cranlogs.r-pkg.org/badges/estprod)](https://cran.r-project.org/package=estprod)

Estimation of production functions by the Olley-Pakes, Levinsohn-Petrin and Wooldridge methodologies.

Getting Started
---------------

Basic syntax is formed by a multipart formula which must be specified in the following order (additional controls are optional):

```R
olley_pakes(data = estprod_data, y ~ free | capital | proxy | controls, exit = ~exit, id = "id", time = "year", bootstrap = TRUE, reps =2)
	
levinsohn_petrin(data = estprod_data, y ~ free | capital | proxy | controls, exit = ~exit, id = "id", time = "year", bootstrap = TRUE, reps = 2, gross = FALSE)

wooldridge(data = estprod_data, var1 ~ var2 | var3 | var4 | var5,  id = "id", time = "year", bootstrap = TRUE)
```

- ```id``` and ```time``` are panel dimensions parameters.

- ```exit``` is an optional formula indicator of the last firm's period. 

- ```bootstrap``` is logical indicating if bootstraped standard errors should be calculated.

- ```reps``` is number of bootstrap replications.

- ```gross``` is a logical which indicates if dependent variable is gross output. Only in ```levinsohn_petrin```.

Installing
--------
- The latest released version from [CRAN](https://CRAN.R-project.org/package=estprod) with

	```R
	install.packages("estprod")
	```
-  The current version from [github](https://github.com/rrremedio/estprod) with

	```R
	devtools::install_github("rrremedio/estprod")
	```
Authors
--------
Rodrigo R Remédio

License
--------

This project is licensed under the GPL-3.
