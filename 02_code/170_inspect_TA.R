## INSPECT Typical ASSEMBLAGES

pacman::p_load(data.table, dplyr)


# which species occur in all TA
data <- readRDS("01_data/15_typical_assemblages.rds")
uniqueN(data$type)
table(data$typical_taxa)%>%sort

#- which type is  Allolobophora chlorotica missing from?
unique(data$type)[which(unique(!data$type %in% data[typical_taxa ==  "Allolobophora chlorotica", as.character(type)]))] 
unique(as.character(data$type))[which(unique(!as.character(data$type) %in% data[typical_taxa ==  "Aporrectodea rosea", as.character(type)]))] 
data[typical_taxa ==  "Aporrectodea rosea"]

dplyr::setdiff(y,x)
x <- data[typical_taxa ==  "Aporrectodea rosea", as.character(type)]
y <- unique(data$type)
