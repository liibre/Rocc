# Routine to check synonyms of a **plant** species name
# Check routine:
# 1. Flora 2020
# 2. Kew - World Checklist of Vascular Plants
# 3. TNRS - Taxonomic Name Resolution Service
# Function will receive a species scientific name
# Function will use other existing and stable functions
# Function will return accepted name, synonyms, ref database for each synonym

devtools::document()
devtools::check()

# loading packages
library(taxize)
library(flora)

?taxize
#pow_lookup
#pow_search

a <- pow_search("Aspidosperma limae")

aa <- pow_search("Aspidosperma melanocalyx")

aaa <- pow_search("Aspidosperma spruceanum")
a$data

aa$data
aaa$data


b <- get.taxa("Aspidosperma limae")

bb <- get.taxa("Aspidosperma melanocalyx")


kew
kew <- read_delim("data/KewNamesBackbone-oct-2019.txt",
                  "|", escape_double = FALSE, trim_ws = TRUE)

names(kew)
head(kew$taxon_name)
