# Installing and loading the package

`devtools::install_github("liibre/rocc")`

```{r setup}
library(rocc)
```

# Downloading occurrence data from R
Currently available:

- [speciesLink](http://www.splink.org.br/) using function `rspeciesLink`
- [GBIF](https://www.gbif.org/) using function `rgbif2` (wrapper of `rgbif` function in the package **rgbif**)

Installing and loading the package:

# Basic taxonomic cleaning

Performs basic taxonomic cleaning: generates a valid name from a "raw" string and checks taxonomy according to Brazilian Flora 2020.


# Short example

For more details, see `vignettes` folder. 
