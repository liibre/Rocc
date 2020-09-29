# Script to generate fieldNames table
library(dplyr)

# Reading data base results ----------------------------------------------------
## gbif
df_gbif <- read.csv("vignettes/results/Asplenium_truncorum_gbif.csv")
## species link
df_splink <- read.csv("vignettes/results/Asplenium_truncorum_splink.csv")

# Standard names from Darwin Core ----------------------------------------------
# from reposaitory: https://github.com/tdwg/dwc
dwc_dic <- read.csv("https://raw.githubusercontent.com/tdwg/dwc/master/vocabulary/term_versions.csv")

dwc <- dwc_dic %>%
  select(c("label", "definition", "organized_in")) %>%
  filter(organized_in != "") %>%
  mutate(low_dwc = tolower(label), dwc = label) %>%
  select(dwc, low_dwc, definition) %>%
  distinct()

# Creating base for fieldNames data frame -------------------------------------
## Column low_dwc will be always used for merge
df <- dwc %>%
  select(dwc, low_dwc) %>%
  distinct()

# Adding speciesLink equivalences ----------------------------------------------
cols_splink <- names(df_splink)
low_splink <- tolower(cols_splink)

splink <- data.frame(low_dwc = low_splink, speciesLink = cols_splink) %>%
  left_join(df, ., by = "low_dwc")

# Adding gbif equivalences -----------------------------------------------------
cols_gbif <- names(df_gbif)
low_gbif <- tolower(cols_gbif)

fields_dwc <- data.frame(low_dwc = low_gbif, gbif = cols_gbif) %>%
  left_join(splink, ., by = "low_dwc") %>%
  left_join(dwc, ., by = c("dwc", "low_dwc"))

usethis::use_data(fields_dwc,
                  overwrite = TRUE,
                  internal = TRUE)
