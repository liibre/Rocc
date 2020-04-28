# Downloading occurrence data from R

Currently available:

- [speciesLink](http://www.splink.org.br/)
- [jabot](http://jabot.jbrj.gov.br/v3/consulta.php)
- [GBIF](https://www.gbif.org/) (using the package **rgbif**)

Installing and loading the package:

`devtools::install_github("saramortara/rocc")`

```{r setup}
library(rocc)
```

See examples in `vignettes` folder. 

## TODO

- [x] `rspeciesLink()`
- [x] `rjabot()`
- [x] `rgbif2()`
- [x] `check_string()`
- [x] `search_flora()`
- [x] `check_flora()`
- [x] create function to generate all synomys for a scientific name based on Brazilian Flora 2020
- [x] check inconsistencies in `name_backbone()` e `occ_search()` from **rgbif**
  - taxonomy backbone
- [ ] check inconsistencies in `rgbif2()`
  - NULL key
  - multiple keys
- [ ] function to bind different search results
- [ ] check `fixField()` from plantR

## Notes

- `rgbif` has its own [backbone taxonomy](https://www.gbif.org/dataset/d7dddbf4-2cf0-4f39-9b2a-bb099caae36c)
- `rgbif` backbone != `flora`
- hiearchy of name checking for us:
  - [Flora do Brasil 2020](http://floradobrasil.jbrj.gov.br/reflora/listaBrasil/ConsultaPublicaUC/ResultadoDaConsultaNovaConsulta.do#CondicaoTaxonCP)
  - [World Checklist of Vascular Plants](https://wcvp.science.kew.org/)
  - [Taxonomic Name Resolution Service](http://tnrs.iplantcollaborative.org/)
