#### Testing search from speciesLink API ####

# versao de teste por Sara Mortara, baseado no tutorial do pacote httr em https://datascienceplus.com/accessing-web-data-json-in-r-using-httr/

#### loading function to make simple url request #### 
source("R/spLink_url.R")

#### Example 1 #####
# same as in http://api.splink.org.br/records/ScientificName/Eugenia platyphylla/Chaetocalyx acutifolia/scope/plants

sp1 <- "Eugenia platyphylla"
sp2 <- "Chaetocalyx acutifolia"

ex01 <- spLink_url(filename = "ex01",
                     scientificname =  c(sp1, sp2), 
                     Scope="plants")
#Warning: 1 parsing failure.
#row             col               expected actual         file
#4558 collectornumber no trailing characters  ,1087 literal data

# faz a busca 
head(ex01$data)
dim(ex01$data)
str(ex01$data)

ex01$url

# especies requisitadas estao no banco
## lista de especies da busca
ex01.sp <- unique(ex01$data$scientificname)
## checando se as sp requisitadas estão no banco --> TRUE :)
c(sp1, sp2)%in%ex01.sp

#### Example 2 ####
# http://api.splink.org.br/records/CollectionCode/uec/scientificname/Rauvolfia sellowii/Cantinoa althaeifolia/Images/yes

ex02 <- spLink_url(filename = "ex02",
                   collectioncode = "uec",
                   scientificname = c("Rauvolfia sellowii", "Cantinoa althaeifolia"), 
                   Images="Yes")

# de novo especies nao estao no output
c("Rauvolfia sellowii", "Cantinoa althaeifolia")%in%ex02$data$scientificname

ex02$url
# faz a busca 
head(ex02$data)
dim(ex02$data)
str(ex02$data)

# checando o campo collectioncode
unique(ex02$data$collectioncode)

#### Example 3 ####
# testando CoordinatesQuality

ex03 <- spLink_url(filename = "ex03",
                   scientificname = "Tillandsia stricta", 
                   CoordinatesQuality = "Good")

# de novo especies nao estao no output
"Tillandsia stricta"%in%ex03$data$scientificname

ex03$url
# faz a busca 
dim(ex03$data)
head(ex03$data)

names(ex03$data)


ex03b <- spLink_url(filename = "ex03b",
                   scientificname = "Tillandsia stricta", 
                   CoordinatesQuality = "Bad")

# de novo especies nao estao no output
"Tillandsia stricta"%in%ex03b$data$scientificname

ex03$url
# faz a busca 
dim(ex03$data)
head(ex03$data)

names(ex03$data)
