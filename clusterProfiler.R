library(clusterProfiler)
x <- psych::read.clipboard()
x <- as.character(x[, 1])
# bitr_kegg(x, "")
t <- enrichKEGG(x, organism = "ko", keyType = "uniprot")
t <- KEGGREST::keggLink("ko", "pathway")



# fisher.test
# phyper
# binom.test