library(topGO)
# prepare data ----
data(ALL, package = "ALL")  # Acute lymphoblastic leukemia(急性淋巴性白血病)
data(geneList)  # 带着好多东西，包括数据，包括差异筛选函数
affyLib <- paste(annotation(ALL), "db", sep = ".")
if (!(affyLib %in% rownames(installed.packages()))) {
    BiocInstaller::biocLite(pkgs = affyLib)
}
library(package = affyLib, character.only = TRUE)

GOdata <- new(  # 创建topGOdata对象
    "topGOdata", 
    description = "Simple session", ontology = "BP", 
    allGenes = geneList, geneSel = topDiffGenes, 
    nodeSize = 10, 
    annot = annFUN.db, affyLib = affyLib
)

r.fisher <- runTest(  # 检验一波
    GOdata, algorithm = "classic", statistic = "fisher"
)
r.ks <- runTest(  # 换个统计方法
    GOdata, algorithm = "classic", statistic = "ks"
)
r.ks.elim <- runTest(  # 换个算法
    GOdata, algorithm = "elim", statistic = "ks"
)

con <- GenTable(  # 整理统计结果，出data frame
    GOdata, classicFisher = r.fisher, 
    classicKS = r.ks, 
    elimKS = r.ks.elim, 
    orderBy = "elimKS", 
    ranksOf = "classicFisher", 
    topNodes = 50
)

pValue.classic <- score(r.ks)
pValue.elim <- score(r.ks.elim)[names(pValue.classic)]
gstat <- termStat(GOdata, names(pValue.classic))
gSize <- gstat$Annotated / max(gstat$Annotated) * 4
gCol <- colMap(gstat$Significant)
plot(
    pValue.classic, pValue.elim, 
    xlab = "p-value classic", ylab = "p-value elim",
    pch = 19, cex = gSize
)

showSigOfNodes(
    GOdata, score(r.ks.elim), firstSigNodes = 50, useInfo = 'all'
)


