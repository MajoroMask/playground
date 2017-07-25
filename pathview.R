library(pathview)
# first example
data(gse16873.d)  # 基因组信息
data(demo.paths)  # 通路信息
i <- 1
pv.out.png <- pathview(
    gene.data = gse16873.d[, 1], pathway.id = demo.paths$sel.paths[i], 
    species = "hsa", out.suffix = "gse16873", 
    kegg.native = T,  # T输出原图，png格式
    same.layer = F,  # 可提速，代价是文件大小
    kegg.dir = "/Volumes/SN_DataPort/Adb/KEGG"
)
pv.out.pdf <- pathview(
    gene.data = gse16873.d[, 1], pathway.id = demo.paths$sel.paths[i], 
    species = "hsa", out.suffix = "gse16873", 
    kegg.native = F,  # F输出重构，pdf格式
    same.layer = F,  # F可将图例和图分成两页
    sign.pos = demo.paths$spos[i],  # pathview签名位置
    split.group = F,  # T将合并的基因拆分
    kegg.dir = "/Volumes/SN_DataPort/Adb/KEGG"
)
# combine compounds & genes
sim.cpd.data <- sim.mol.data(mol.type = "cpd", nmol = 3000)  # 模拟数据
data(cpd.simtypes)
i <- 3
pv.out.png <- pathview(
    gene.data = gse16873.d[, 1], cpd.data = sim.cpd.data, 
    pathway.id = demo.paths$sel.paths[i], species = "hsa", 
    out.suffix = "gse16873.cpd", keys.align = "y", 
    kegg.native = T, key.pos = demo.paths$kpos1[i],
    same.layer = F, 
    kegg.dir = "/Volumes/SN_DataPort/Adb/KEGG"
)
pv.out.pdf <- pathview(
    gene.data = gse16873.d[, 1], cpd.data = sim.cpd.data, 
    pathway.id = demo.paths$sel.paths[i], species = "hsa", 
    out.suffix = "gse16873.cpd", keys.align = "y", 
    kegg.native = F, key.pos = demo.paths$kpos2[i],
    sign.pos = demo.paths$spos[i], 
    cpd.lab.offset = demo.paths$offs[i], 
    same.layer = F, 
    kegg.dir = "/Volumes/SN_DataPort/Adb/KEGG"
)
# multiple states of sample
set.seed(10)
sim.cpd.data2 <- matrix(
    sample(sim.cpd.data, 18000, replace = T), ncol = 6
)
rownames(sim.cpd.data2) <- names(sim.cpd.data)
colnames(sim.cpd.data2) <- paste("exp", 1:6, sep = "")
i <- 3
pv.out <- pathview(  # 三组基因，两组代谢
    gene.data = gse16873.d[, 1:3], cpd.data = sim.cpd.data2[, 1:2], 
    pathway.id = demo.paths$sel.paths[i], species = "hsa", 
    out.suffix = "gse16873.cpd.3-2s", keys.align = "y", 
    kegg.native = T, match.data = F, multi.state = T, same.layer = F, 
    kegg.dir = "/Volumes/SN_DataPort/Adb/KEGG"
)

pv.out <- pathview(
    gene.data = gse16873.d[, 1:3],
    cpd.data = sim.cpd.data2[, 1:2], pathway.id = demo.paths$sel.paths[i], 
    species = "hsa", out.suffix = "gse16873.cpd.3-2s", keys.align = "y", 
    kegg.native = T, match.data = F, multi.state = T, same.layer = F, 
    key.pos = demo.paths$kpos2[i], sign.pos = demo.paths$spos[i], 
    limit = list(gene = 3, cpd = 3), 
    bins = list(gene = 30, cpd = 30), 
    cex = 0.2, res = 300, new.signature = F, plot.col.key = F, 
    kegg.dir = "/Volumes/SN_DataPort/Adb/KEGG"
)



