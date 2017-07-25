library(KEGGREST)
ptw <- c("hsa00440", "hsa01100")
x <- c("C00037", "C00249", "C00334", "C00208", "C00158", "C03251", "C00134", "C16440", "C00253", "C00208", "C00093", "C00116", "C00037", "C00387", "C07272", "C01235", "C06464", "C00170", "C00346", "C09099", "C00319", "C05901", "C05580", "C00392", "C00191")
url <- color.pathway.by.objects(
    paste0("path:", ptw), 
    x, 
    rep("red", length = length(x)), 
    rep("red", length = length(x))
)
url.ori <- sub(paste0(ptw, "_.*?\\.png"), paste0(ptw, "\\.png"), url)
download.file(url.ori, "./temp.png", mode = "wb")
