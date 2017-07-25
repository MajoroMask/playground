library(ropls)
data(sacurine)
attach(sacurine)
for (i in 1:10) {
    set.seed(123)
    sacurine.oplsda <- opls(
        dataMatrix, sampleMetadata[, "gender"], predI = 1, ortho = 1, 
        permI = 200, printL = F, plotL = F
    )
    plot(sacurine.oplsda, typeVc = 'permutation')
}


