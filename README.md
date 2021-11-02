# Rocc package: management and analysis of species occurrence data

<img src="https://raw.githubusercontent.com/liibre/Rocc_logo/master/figs/Rocc_logo.png" align="right" alt="" width="120" />

## Installing and loading the package

```
devtools::install_github("liibre/Rocc")
library(Rocc)
```

## Downloading and binding occurrence data from R

- `rspeciesLink()` downloads occurrences directly from [speciesLink API](https://api.splink.org.br/)

- `rgbif2()` downloads occurrences from [GBIF](https://www.gbif.org/), a wrapper function of `rgbif()` in the package **rgbif**

- `bind_dwc()` formats an output of occurrence data based on the current [DarwinCore standard](https://dwc.tdwg.org/terms/) and bind data from the different sources such as GBIF and speciesLink

## Basic taxonomic cleaning

- `check_string()` identifies open nomenclature, infraspecies categories, authorship, undetermined species, hybrid species, non-ascii characters in species name, digits in species name. Performs a check in the given name and returns a flag of the status and a string correction. For taxonomic check see `suggest_flora()` and `check_flora()`

- `suggest_flora()` trims and corrects typo of a given species name

- `check_flora()` makes a request to [Brazilian Flora 2020 API](http://servicos.jbrj.gov.br/flora/) and returns accepted name and synonyms

## Direct query to Brazilian Flora 2020 database

- `search_flora()` searches in the List of Species of the Brazilian Flora 2020 database (by endemism, life form, habitat or vegetation type)

- `update_flora()` downloads, updates and formats the data from the ipt server of the List of Species of the Brazilian Flora

## Additional functions

+ `getGADM()` downloads shapefiles for administrative units for countries from [https://gadm.org/data.html](https://gadm.org/data.html) in either sf or sp formats

+ `getGAZ()` downloads the corresponding DIVA-GIS gazetteer from [http://www.diva-gis.org/gData](http://www.diva-gis.org/gData) 

+ `getWDPA()` downloads shapefiles from the the IUCN Global Database for Protected Areas for each country ([https://www.protectedplanet.net](https://www.protectedplanet.net))


## Examples

For more details, see `Articles` section.
