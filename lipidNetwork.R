library(stringr)
library(ggplot2)
library(networkD3)
df.ori <- openxlsx::read.xlsx('in/Lcell-neg.xlsx')
df.ori <- df.ori[!is.na(df.ori$`hits.(reverse)`), c(2, 9, 11:14, 18)]
colnames(df.ori)[3:7] <- c('a1', 'a2', 'b1', 'b2', 'desc')
df.ori$score <- as.numeric(gsub('^.*score\\{|\\}adduct.*', '', df.ori$desc))
df.ori$type <- gsub('^.*compound name\\{|\\}(name)?.*$', '', df.ori$desc)
df.ori$length <- as.numeric(stringr::str_extract(df.ori$type, '\\d+'))
df.ori$un <- stringr::str_extract(df.ori$type, '\\:.*\\/')
df.ori$un <- as.numeric(stringr::str_extract(df.ori$un, '\\d'))
df.ori$type <- gsub('\\(.*$', '', df.ori$type)
m.cor <- cor(t(df.ori[, 3:6]))
colnames(m.cor) <- df.ori$name
rownames(m.cor) <- df.ori$name
t <- pheatmap::pheatmap(
    m.cor, border_color = NA, show_rownames = F, show_colnames = F, 
    color = colorRampPalette(c("dodgerblue3", "gainsboro", "firebrick3"))(100)
)
m <- t$tree_row$merge
temp <- list()
i <- 1
while(i < nrow(m)) {
    if(m[i, 1] < 0 & m[i, 2] < 0) {
        temp[[i]] <- c(-m[i, 1], -m[i, 2])
    } else if(m[i, 1] > 0 & m[i, 2] > 0) {
        temp[[i]] <- c(temp[[m[i, 1]]], temp[[m[i, 2]]])
    } else {
        temp[[i]] <- c(-m[i, 1], temp[[m[i, 2]]])
    }
    i <- i + 1
}
temp2 <- c(m[nrow(m), 1], m[nrow(m), 2])
while(length(temp2) < 8) {
    temp2 <- as.numeric(
        sapply(
            temp2, function(ele) {
                c(m[ele, 1], m[ele, 2])
            }
        )
    )
}
df.ori$cluster <- rep(0, nrow(df.ori))
lapply(
    1:length(temp[temp2]), function(i) {
        df.ori$cluster[temp[temp2][[i]]] <<- i
    }
)
rm(temp, temp2, i, m)
p1 <- ggplot(df.ori, aes(cluster)) + 
    geom_bar(aes(fill = type))
df.link <- reshape2::melt(m.cor)
x <- which(df.link$value == 1)
x <- unlist(
    sapply(
        1:length(x), function(i) {
            seq(from = x[i], length.out = i)
        }
    )
)
df.link <- df.link[-x, ]
df.link <- df.link[df.link$value > 0.8 & df.link$value != 1, ]
df.link$Var1 <- as.numeric(factor(df.link$Var1, levels = df.ori$name))
df.link$Var2 <- as.numeric(factor(df.link$Var2, levels = df.ori$name))
df.link[, -3] <- df.link[, -3] - 1
MyClickScript <- paste0(
    'alert("You clicked " + d.name + " which is in row " + ', 
    '(d.index + 1) +  " of your original R data frame");'
)
fn <- forceNetwork(
    Links = df.link, Nodes = df.ori, Source = 'Var1', Target = 'Var2', 
    Value = 'value', NodeID = 'name', Group = 'type', 
    Nodesize = 'length', 
    # colourScale = JS('d3.scale.ordinal().range(["#f5160b","#0752e7"])'), 
    radiusCalculation = JS("Math.pow(d.nodesize, 2) / 30"), 
    linkColour = "#ababab", 
    opacity = 0.8, zoom = T, legend = T, clickAction = MyClickScript
)
# saveNetwork(fn, 'test.html', selfcontained = T)


