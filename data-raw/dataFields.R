## code to prepare `dataFields` dataset
dataFields <- jsonlite::fromJSON("https://biocache-ws.vtatlasoflife.org/index/fields"
                                 ,flatten = TRUE)

# Re-order names #
dataFields <- dataFields[,c('name','dataType','indexed','stored',
                            'multivalue','docvalue','downloadName',
                            'description','dwcTerm','classs','jsonName',
                            'downloadDescription','i18nValues','info')]

usethis::use_data(dataFields, overwrite = TRUE)
