library(cisPath)
getMappingFile(
    sprotFile = 'd:/Adb/sp/201608_sp_Mus_musculus_detail.txt', 
    output = 'd:/Adb/sp/201608_sp_Mus_musculus_IDmapping.txt'
)
formatSTRINGPPI(
    input = 'd:/Adb/STRING/10090.protein.links.v10.txt', 
    mappingFile = 'd:/Adb/sp/201608_sp_Mus_musculus_IDmapping.txt', 
    taxonId = '10090', 
    output = 'd:/Adb/STRING/10090.protein.links.v10.cisPath.txt'
)
dep <- read.table('input.txt', stringsAsFactors = F)$V1
networkView(
    infoFile = 'd:/Adb/STRING/10090.protein.links.v10.cisPath.txt', 
    proteinNames = dep, outputDir = './out', swissProtID = T
)