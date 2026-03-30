library(rentrez)

load("pubmed_query_forLoad.RData")

pub_count <- data.frame()

for (i in 1:length(query_array)) {
  for (ii in 1:nrow(query_array[i])) {
    query <- query_array[ii,i]  
    aa <- entrez_search(db="pubmed", term=query)
    pub_count[ii,i] <- aa$count
  }
}

pub_count
