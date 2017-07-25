ROCforLC <- function() {
    # 给LC数据做ROC曲线
    # 需要读取正负离子模式下的SIMCA.csv和差异代谢物筛选表
    # 对每个差异代谢物进行单独的ROC曲线绘制，使用{plotROC}包
    # 分别在正负离子模式的结果目录下创建ROC Curve文件夹，为每组对比创建ROC曲线
    # 函数本身没有返回值
    # 
    require(reshape2)
    require(ggplot2)
    require(plotROC)
    lapply(
        c("POS", "NEG"), function(pol) {
            # 对两种模式循环
            load(file.path(l.a$out.temp, paste0(pol, ".l.a.RData")))
            # df.roc <- df.out.SIMCA
            lapply(
                1:length(l.a$list.compare), function(i) {
                    # 对每组对比循环
                    # 
                    df.sub.roc <- FormatROCData(i)  # 数据格式整理
                    list.roc <- ROC(df.sub.roc)  # 给一个物质制作ROC曲线
                    dir <- file.path(
                        l.a$output, "results", pol, "ROC Curve", l.a$subDir[i]
                    )
                    # 输出文件夹
                    OutputROC(list.roc, dir)
                }
            )
        }
    )
}
# 一级子函数 ----
FormatROCData <- function(i) {
    # 整理数据表格
    # 只能给Biotree脚本用，这个函数移植性很差
    # 
    df.deg <- list.compare.result[[i]]
    df.deg <- df.deg[eval(l.a$deg.exp), c("id", "MS2 name")]
    df.deg <- df.deg[!is.na(df.deg$`MS2 name`), ]
    pair <- l.a$list.compare[[i]]
    df.roc <- df.out.SIMCA
    df.sub <- df.roc[
        mapply(`|`, l.a$group[[pair[1]]], l.a$group[[pair[2]]]), 
        c("classid", df.deg$id)
        ]
    colnames(df.sub) <- c("classid", df.deg$`MS2 name`)
    df.sub[, -1] <- scale(df.sub[, -1])
    df.sub$D <- as.numeric(
        factor(
            df.sub$classid, 
            levels = names(l.a$group)[c(pair[2], pair[1])]
        )
    ) - 1
    df.sub <- reshape2::melt(df.sub, id.vars = c('classid', 'D'))
    df.sub$variable <- as.character(df.sub$variable)
    return(df.sub)
}
ROC <- function(df.in) {
    # 
    list.roc <- lapply(  # 给输入表格的每一个物质创建一个ROC曲线
        unique(df.in$variable), function(m) {
            df.sub <- df.in[df.in$variable == m, ]
            list.p <- CreateROCPlot(df.sub, auc.stir = T)  # 作图函数
            m.sub <- gsub("/|:|\\*|\\?|\"|<|>|\\|", "", m, perl = T)
            return(c(m, m.sub, list.p))
        }
    )
    # 
    # 以下部分可以添加代谢物组合
    # list.total <- CreateROCPlot(df.in)  # 相当于取全集
    # list.roc <- append(list.roc, list(c("total", "total", list.total)))
    # 
    return(list.roc)
}
OutputROC <- function(list.roc, dir) {
    # 图片和图表输出
    # 
    if(!dir.exists(dir)) dir.create(dir, recursive = T)
    lapply(
        list.roc, function(list.p) {
            ggsave(
                file.path(dir, paste0(list.p[[2]], '.jpg')), list.p[[3]], 
                dpi = 600, height = 7, width = 7, units = "in"
            )
            ggsave(
                file.path(dir, paste0(list.p[[2]], '.pdf')), list.p[[3]], 
                dpi = 600, height = 7, width = 7, units = "in"
            )
        }
    )
    df.aucs <- data.frame(
        "MS2 name" = sapply(list.roc, `[[`, 1, USE.NAMES = F), 
        "pic name" = sapply(list.roc, `[[`, 2, USE.NAMES = F), 
        "auc" = sapply(list.roc, `[[`, 4, USE.NAMES = F), 
        "mode" = sapply(list.roc, `[[`, 5, USE.NAMES = F), 
        check.names = F
    )
    df.aucs <- df.aucs[order(df.aucs$`pic name`), ]
    write.csv(df.aucs, file = file.path(dir, "auc.csv"), row.names = F)
}
# 二级子函数 ----
CreateROCPlot <- function(df.in, auc.stir = F) {
    # 作图函数，返回一个列表，包括图和一些参数
    # 启用auc.stir会把AUC小于等于0.5的和谐掉
    # 
    TempFun <- function() {
        p <- ggplot(df.in, aes(d = D, m = value)) + 
            geom_roc(labelround = 2) + 
            # geom_rocci() +  # 增加置信区间，样品数量多的时候好用
            style_roc(theme = theme_bw)
    }
    p <- TempFun()
    auc <- calc_auc(p)$AUC
    mode <- "forward"
    if(auc.stir) {
        if(auc < 0.5) {
            df.in$D <- as.numeric(!as.logical(df.in$D))
            p <- TempFun() + labs(x = "1 - false positive fraction")
            auc <- calc_auc(p)$AUC
            mode <- "reverse"
        }
    }
    if(auc >= 0.5) {
        p <- p +
            annotate(
                'label', x = 0.9, y = 0.1, fill = '#e31a1c', color = 'white', 
                label = paste0('AUC = ', round(auc, digits = 2))
            )
    } else {
        p <- p +
            annotate(
                'label', x = 0.1, y = 0.9, fill = '#1f78b4', color = 'white', 
                label = paste0('AUC = ', round(auc, digits = 2))
            )
    }
    return(list(p, auc, mode))
}