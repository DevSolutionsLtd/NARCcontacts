
<!-- README.md is generated from README.Rmd. Please edit that file -->
NARCcontacts
============

An R package to make managing data a little easier for 'Nehemians'.

Installation
------------

You can install NARCcontacts from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("DevSolutionsLtd/NARCcontacts")
```

Instructions for use
--------------------

To harmonise the data, move all the Excel files that contain contact details into a directory (folder). For instance if the folder is called *Contacts*, then do this in the R console

``` r
library(NARCcontacts)
harmonise_narc_excel("path/to/Contacts")
```

Disclaimer
----------

The package is developed for a particular client, but with some tweaking can be extended for more generic use.
