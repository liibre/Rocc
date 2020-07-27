# Installing and loading the package

`devtools::install_github("saramortara/rocc")`

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



- [x] `rspeciesLink()` - Sara
- [x] `rgbif2()` - Sara
- [x] `check_string()` - Sara
- [x] `search_flora()` - Andrea
- [x] `update_flora()` - Andrea
- [x] `check_flora()` - Sara
- [x] create function to generate all synomys for a scientific name based on Brazilian Flora 2020
- [ ] check inconsistencies in `name_backbone()` e `occ_search()` from **rgbif**
  - taxonomy backbone
- [ ] check inconsistencies in `rgbif2()` - Sara
  - multiple keys
- [ ] check output from `check_flora()` - Sara & Andrea
- [ ] `bind_data()` function to format fields and bind different search results - Sara
- [ ] check `fixField()` from plantR
- [ ] `rjabot()` from [API](http://servicos.jbrj.gov.br/jabot/) - API not working... waiting...

## Notes

- `rgbif` has its own [backbone taxonomy](https://www.gbif.org/dataset/d7dddbf4-2cf0-4f39-9b2a-bb099caae36c)
- `rgbif` backbone != `flora`
- hiearchy of name checking for us:
  - [Flora do Brasil 2020](http://floradobrasil.jbrj.gov.br/reflora/listaBrasil/ConsultaPublicaUC/ResultadoDaConsultaNovaConsulta.do#CondicaoTaxonCP)
  - [World Checklist of Vascular Plants](https://wcvp.science.kew.org/)
  - [Taxonomic Name Resolution Service](http://tnrs.iplantcollaborative.org/)
