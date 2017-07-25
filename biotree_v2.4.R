#!/usr/bin/env R
################################################################################
# Version 2.3
# Su Na 2016-07-26
# Encoding in UTF-8
# source("biotree_v2.3.R", encoding='UTF-8')
################################################################################
# 2016-08-24
# update to v2.3
#     DEG-PEAK输出的similarity取整和排序
#     修复一些bug
# 
# 2016-08-09
# update to v2.2
#     更新代码格式，优化代码逻辑
#     适应无内标数据
#     更新DEG-PEAK表和MEAN表的输出，支持直接输出.xlsx格式
#     优化SIMCA.csv，增加classID和观测排序
# 
# 2016-07-26
# update to v2.1
#     尝试适应无质控数据
# 
# 2016-07-18
# update to v2.0
#     模块化代码
#     尝试增加结果文件夹，使结果自动化程度更高
# 
# 2016-07-13
# update to v1.2
#     增加BeforeSIMCA()：创建SIMCA分析用的文件夹
#     增加OutputMEAN()：创建MEAN表
#     修改参数格式
#     修改了DataReshape()，添加参数整形
#
# 2016-07-11
# update to v1.1
#     修改变量名称以保留中间结果
# 
#     bug fix:
#         if(length(which(is.na(line.sub))) >= 0.5 * group.sub) {
#         if(length(which(is.na(line.sub))) > 0.5 * group.sub) {
#
# 2016-07-04
#     v1.0
#     尝试阅读并修改脚本
#     读取GC-MS搜库原始结果，进行数据整形和统计分析
################################################################################
require(fdrtool)
require(xlsx)
################################################################################
# Global variable (l.a)
#
# qc：质控组个数，本次实验有多少组QC（即混样组），没有就设NA
# is：内标序号，内标物的Analyte序号，没有就设NA
# group：分组信息，包含分组信息的字符串，每组实验个数按顺序用空格隔开
# sample：每组名称，由于分析执行单和compare表中的组别名称可能会有差异，以
#     分析执行单为准
# compare：差异比较策略，包含group间两两比较的信息，用冒号连接，空格隔开，分子
#     在前，分母在后
# filter：过滤法，
#     'rsd': relative standard deviation, or CV
#     'sfw': interquartile range, 四分位距
#     注意：使用rsd法时，qc不能为NA
# norm：归一化方法，
#     'neibiao'：利用内标进行归一化
#     'mianji'：利用TIC进行归一化（存疑）
# task：重要，标明需要做哪些分析的参数，必有basic和stat，用空格隔开
# input：输入文件
# output：输出文件位置
# outSIMCA：SIMCA分析结果所在文件夹
# outList: 分析过程中产生临时列表的位置
# outKEGG：KEGG和通路分析
# outHeatmap: DEG做heatmap的位置
# outResult：用户报告相关文件所在文件夹
# 
# rm(list = ls(all.names = T))  # 清空workspace
l.a <- list(
    qc         = 7,  # integer or NA
    is         = 352L,  # integer or NA
    group      = '9 10 9',  # string, eg: '10 10 10'
    sample     = 'S M T',  # string, eg: 'sample1 sample2 sample3'
    compare    = '2:1 3:2',  # string, eg: '1:2 1:3 2:3'
    filter     = 'sfw',  # string, 'rsd' or 'sfw'
    norm       = 'neibiao',  # string, 'neibiao' or 'mianji'
    task       = 'basic stat kegg heatmap ma', # string
    input      = './in',  # string
    output     = './out',  # string
    outWS      = './workspace',  # string
    outSIMCA   = '../02_SIMCA',  # string
    outList    = '../03_list',   # string
    outKEGG    = '../03_KEGG',  # string
    outHeatmap = '../04_Heatmap',  # string
    outResult  = '../results'  # string
)
################################################################################
ArgsVarify <- function() {
    # 确认参数和参数整形，这些参数都是上面赋值过的
    #
    print(noquote(sprintf("%-30s %s", names(l.a), l.a)))
    # 以下为参数整形部分
    group <- as.numeric(Str2Vector(l.a$group, split2 = NA))
    group.length <- cumsum(group)
    # 分组信息
    list.group <- mapply(function(group.sub, group.length.sub) {
        # 真·分组信息，返回分组包含分组开始列和结束列的列表
        #
        start <- group.length.sub - group.sub + 1
        stop <- group.length.sub
        return(c(start, stop))
    }, group.sub = group, group.length.sub = group.length, SIMPLIFY = F)
    sample <- Str2Vector(l.a$sample, split2 = NA)
    list.compare <- Str2Vector(l.a$compare, split2 = ':')
    subDir <- sapply(1:length(list.compare), function(i) {
        # 为每一个对比组设置二级子文件夹名称
        # 形如01_S02vsS01这样，有编号和组别
        #
        index1 <- sample[list.compare[[i]][1]]
        index2 <- sample[list.compare[[i]][2]]
        dirname <- paste(
            sprintf("%02.f", i), '_', index1, '-', index2, sep = ''
        )
    })
    l.r <- list(
        list.compare = list.compare, 
        group = group, 
        group.length = group.length, 
        list.group = list.group, 
        sample = sample, 
        subDir = subDir
    )
    return(l.r)
}
################################################################################
Str2Vector <- function(str, split1 = ' ', split2 = '/') {
    # 用于处理参数列表里的字符串参数
    # 将提供的字符串拆分，返回一维对象，有二级返回列表，没二级返回向量
    #
    lev1 <- unlist(strsplit(str, split = split1))
    if(is.na(split2)) {
        return(lev1)
    } else {
        list.lev2 <- lapply(lev1, function(group) {
            group <- unlist(strsplit(group, split = split2))
            if(all(as.double(group))) {
                return(as.double(group))
            } else {
                return(group)
            }
        })
        return(list.lev2)
    }
}
################################################################################
BuildProjectFramework <- function() {
    # 2.0版新加函数，根据参数信息建立项目框架，主要是创建目录
    # 
    if(grepl('basic', l.a$task)) {
        if(!dir.exists(l.a$outSIMCA)) dir.create(l.a$outSIMCA)
        if(!dir.exists(l.a$outResult)) dir.create(l.a$outResult)
        dir.total <- paste(l.a$outResult, 'TOTAL', sep = '/')
        if(!dir.exists(dir.total)) dir.create(dir.total)
    }
    if(grepl('stat', l.a$task)) {
        lapply(l.r$subDir, function(subDir) {
            dir.sub <- paste(l.a$outResult, subDir, sep = '/')
            if(!dir.exists(dir.sub)) dir.create(dir.sub)
        })
    }
    if (grepl('KEGG', l.a$task)) {
        if(!dir.exists(l.a$outKEGG)) dir.create(l.a$outKEGG)
        dir.kegg <- paste(l.a$outKEGG, '01_DEG-PEAK', sep = '/')
        if(!dir.exists(dir.kegg)) dir.create(dir.kegg)  # 03_KEGG文件夹中的
        dir.kegg <- paste(l.a$outResult, 'KEGG', sep = '/')
        if(!dir.exists(dir.kegg)) dir.create(dir.kegg)  # results文件夹中的
        lapply(l.r$subDir, function(subDir) {
            file.sub <- paste(  # 预创建DEG-PEAK表，给kegg.R做输入
                l.a$outKEGG, '/', '01_DEG-PEAK', '/', 
                subDir, '.txt', sep = ''
            )
            if(!file.exists(file.sub)) file.create(file.sub)
            dir.sub <- paste(dir.kegg, subDir, sep = '/')
            if(!dir.exists(dir.sub)) dir.create(dir.sub)
        })
    }
}
################################################################################
ReadInput <- function() {  # 原始数据输入
    df.ori <- read.csv(
        paste(l.a$input, 'in.csv', sep = '/'), 
        stringsAsFactors = F, check.names = F, na.strings = c('', 'NA')
    )
}
################################################################################
DataReshape <- function(df.in = df.ori) {
    # 进行数据整形
    # 
    # Value: 
    #     df.area：整理好的area信息
    #     
    df.area <- df.in[-1, c(1:6, seq(10, ncol(df.in), by = 4))]
    names(df.area) <- c(
        'Peak','ID','Similarity','R.T.','Count','Mass', 
        names(df.in)[seq(7, ncol(df.in), by = 4)]
    )
    # 变量重命名
    if(!is.na(l.a$is)) {
        # 如果有内标，把内标行放到数据框最下
        # ID号放到数据框最左
        is.row <- which(df.area$ID == l.a$is)
        df.area <- df.area[
            c(1:(is.row - 1), (is.row + 1):nrow(df.area), is.row), 
            c(2, 1, 3:ncol(df.area))
        ]
    }
    if(!is.na(l.a$qc)) {  # 如果有QC，把QC组放到数据框最右
        df.area <- df.area[, c(1:6, (7 + l.a$qc):ncol(df.area), 7:(6 + l.a$qc))]
    }
    df.area[, c(-2, -4)] <- sapply(df.area[, c(-2, -4)], as.numeric)
    # 转换数字类型的变量格式为数字
    df.area[, c(7:ncol(df.area))] <- sapply(
        df.area[, c(7:ncol(df.area))], 
        function(column) {
            column[which(column == 0)] <- NA
            return(column)
        }
    )
    # 将实验组和QC组的0赋值为NA，至于原始数据中0和空值之间有何区别目前未知
    df.area <- as.data.frame(df.area)
    return(df.area)
}
################################################################################
RawFilter <- function(df.in = df.area) {
    # 对实验组数据中的离群点进行筛选，包括四分位距法和rsd法（即CV法）
    # 四分位距法：设超出四分位点1.5倍四分位间距的点为离群点，删除该离群点。此方法
    #   目前仍有疑虑，当实验组数据特别离散时，可能会掩盖某些确实存在的生物学现象
    # rsd法：认为QC组rsd大于0.3的检测不稳定，删除该物质的所有检测数据
    # 注意内标行不参与过滤
    #
    if(!is.na(l.a$is)){  # 内标所在行不参与过滤
        IS.serial <- which(df.in$ID == l.a$is)
        line.IS <- df.in[IS.serial, ]
        df.in <- df.in[-IS.serial, ]
    }
    if(l.a$filter == 'sfw') {  # 四分位距法部分
        if(is.na(l.a$qc)) {
            df.exp <- df.in[, 7:ncol(df.in)]
        } else {
            df.exp <- df.in[, 7:(ncol(df.in) - l.a$qc)]
        }
        # 提取需要过滤的实验组数据
        list.exp <- sapply(1:nrow(df.exp), function(i, df) {
            # 按行循环
            #
            line <- df[i, ]
            quan <- quantile(line, probs = c(0.25,0.75), na.rm = T)
            # 求该物质所有实验组数据的四分位点
            line.max <- quan[[2]] + 1.5 * (quan[[2]] - quan[[1]])
            line.min <- quan[[1]] - 1.5 * (quan[[2]] - quan[[1]])
            line[line > line.max | line < line.min] <- NA
            # 计算四分位距并进行过滤
            return(line)
        }, df = df.exp, simplify = F)
        df.exp <- do.call(rbind, list.exp)
        if(is.na(l.a$qc)) {
            df.in[, 7:ncol(df.in)] <- df.exp
        } else {
            df.in[, 7:(ncol(df.in) - l.a$qc)] <- df.exp
        }
    } else if(l.a$filter == 'rsd') {  # rsd法部分
        stopifnot(!is.na(l,a$qc))  # 检测参数，rsd法qc不能为NA
        df.qc <- df.in[, (ncol(df.in) - l.a$qc + 1):ncol(df.in)]
        # 提取需要过滤的QC组数据
        df.qc$rsd <- apply(df.qc, MARGIN = 1, function(line) {
            # 按行循环
            #
            line.mean <- mean(line, na.rm = T)
            line.sd <- sd(line, na.rm = T)
            rsd <- line.sd / line.mean
            # 计算该物质的QC组rsd
        })
        df.in <- df.in[df.qc$rsd < 0.3, ]  # 卡值取0.3
    }
    if(!is.na(l.a$is)){  # 内标所在行不参与过滤
        df.in <- rbind(df.in, line.IS)  # 内标所在行不参与过滤
    }
    return(df.in)
}
################################################################################
RecodeNA <- function(df.in) {
    # 判断某物质是否值得保留，若保留则填充实验组数据中的NA
    # 保留条件：1、鉴定组数大于总组数的50%
    #           2、任意一个样本的重复实验中：鉴定数大于重复次数的50%
    # 用实验组area最小值的二分之一填充
    #
    if(is.na(l.a$qc)) {
        df.data <- df.in[, 7:ncol(df.in)]
    } else {
        df.data <- df.in[, 7:(ncol(df.in) - l.a$qc)]
    }
    keep <- apply(df.data, MARGIN = 1, function(line) {
        # 判断某物质是否保留
        #
        if(sum(!is.na(line)) >= 0.5 * length(line)) {  # 条件一
            return(1)
        } else {
            temp <- mapply(function(pair, group.size) {
                # 条件二
                # 
                line.sub <- line[pair[1]:pair[2]]
                if(sum(!is.na(line.sub)) >= 0.5 * group.size) {
                    return(1)
                } else {
                    return(0)
                }
            }, l.r$list.group, l.r$group)
            if(sum(temp) >= 1) {
                return(1)
            } else {
                return(0)
            }
        }
    })
    df.in <- df.in[keep == 1, ]
    # 删除不要保留的行，df.in在此处发生变化
    if(is.na(l.a$qc)) {
        df.data <- df.in[, 7:ncol(df.in)]
    } else {
        df.data <- df.in[, 7:(ncol(df.in) - l.a$qc)]
    }
    exp.min <- min(apply(df.data, MARGIN = 1, min, na.rm = T))
    # 求实验组area最小值
    df.in[, 7:ncol(df.in)] <- sapply(df.in[, 7:ncol(df.in)], function(column) {
        # 填充最小值的二分之一
        column[is.na(column)] <- exp.min / 2
        return(column)
    })
    # 填充NA
    return(df.in)
}
################################################################################
Normalize <- function(df.in) {
    # 对实验组和QC组进行进一步归一化，方法有内标法和面积和法
    # 内标法：认为加入的内标在各组数据间相同，利用内标进行归一化
    #     注意如果用内标法，则l.a$is不能为NA
    # 面积和法：认为检测物质总量一定，得出的结果转换为
    #     各个物质相对物质总量的相对值
    # 
    # Args:
    #     df.in：data frame，总数据框
    #     norm：string，标明归一化方法，'neibiao' or 'mianji'
    #     is：numeric，内标的ID号
    #
    if(l.a$norm == 'neibiao') {  # 内标法
        IS.serial <- which(df.in$ID == l.a$is)
        df.in[, 7:ncol(df.in)] <- sapply(
            df.in[, 7:ncol(df.in)], function(column) {
                # 内标矫正
                #
                column <- column / column[IS.serial]
            })
        df.in <- df.in[-IS.serial, ]  # 校正后删除内标行
    } else if(l.a$norm == 'mianji') {  # 面积法
        if(!is.na(l.a$is)) {  # 校正前如果有内标，删除内标行
            IS.serial <- which(df.in$ID == l.a$is)
            df.in <- df.in[-IS.serial, ]
        }
        df.in[, 7:ncol(df.in)] <- sapply(
            df.in[, 7:ncol(df.in)], function(column) {
                # 利用面积矫正
                #
                sum.col <- sum(column)
                column <- column / sum.col
            })
    } else {
        return()  # 杜指导的脚本里就返回空值
    }
    return(df.in)
}
################################################################################
OutputMean <- function(df.in = df.final) {
    # 输出原始数据和MEAN表到客户报告文件夹
    #
    if(!is.na(l.a$qc)) {
        l.r$list.group[[length(l.r$list.group) + 1]] <- 
            c(l.r$list.group[[length(l.r$list.group)]][2] + 1, 
              l.r$list.group[[length(l.r$list.group)]][2] + l.a$qc)
        l.r$sample <- append(l.r$sample, 'QC')
    }
    df.data <- df.in[, 7:ncol(df.in)]
    list.data <- lapply(
        1:length(l.r$list.group), function(i) {
            # 分组计算mean
            #
            start <- l.r$list.group[[i]][1]
            stop  <- l.r$list.group[[i]][2]
            df.sub <- df.data[, start:stop]
            df.sub$temp <- apply(df.sub, MARGIN = 1, mean)
            names(df.sub)[ncol(df.sub)] <- paste('MEAN', l.r$sample[i])
            # 修改mean列命名
            return(df.sub)
        }
    )
    df.out <- do.call(cbind, list.data)
    df.out <- cbind(df.in[, 1:6], df.out)
    # 合并输出列表
    fn <- paste(l.a$outResult, 'MEAN.xlsx', sep = '/')
    write.xlsx2(df.out, fn, row.names = F)
    return(df.out)
}
################################################################################
OutputSIMCA <- function(df.in = df.final) {
    # 输出供SIMCA输入的数据
    # 
    rownames(df.in) <- df.in$ID
    df.out <- as.data.frame(t(df.in[, 7:ncol(df.in)]))
    class.id <- rep(l.r$sample, l.r$group)
    if(!is.na(l.a$qc)) {
        class.id <- append(class.id, rep('QC', l.a$qc))
    }
    df.out$`classID` <- class.id
    df.out <- df.out[c(ncol(df.out), 1:(ncol(df.out) - 1))]
    if(is.na(l.a$qc)) {
        df.out <- df.out[order(df.out$classID), ]
    } else {
        width <- nrow(df.out)
        df.data <- df.out[1:(width - l.a$qc), ]
        df.data <- df.data[order(df.data$classID), ]
        df.out <- rbind(df.data, df.out[(width - l.a$qc + 1):width, ])
    }
    write.csv(df.out, file = paste(l.a$outSIMCA, "SIMCA.csv", sep = '/'))
    return(df.out)
}
################################################################################
OutputDegPeak <- function(df.in = df.final) {
    # 计算并输出每对比较的单变量统计数据
    #
    file.copy(
        paste(l.a$input, 'DEG-PEAK.xlsx', sep = '/'), 
        paste(l.a$outResult, '_DEG-PEAK.xlsx', sep = '/'), 
        overwrite = T
    )
    list.compare.result <- lapply(
        1:length(l.r$list.compare), function(i) {
            # 对每对比较进行一元统计分析
            #
            if(is.na(l.a$qc)) {  # 数据
                df.data <- df.in[, 7:ncol(df.in)]
            } else {
                df.data <- df.in[, 7:(ncol(df.in) - l.a$qc)]
            }
            pair <- l.r$list.compare[[i]]
            a.start <- l.r$list.group[[pair[1]]][1]
            a.stop <- l.r$list.group[[pair[1]]][2]
            b.start <- l.r$list.group[[pair[2]]][1]
            b.stop <- l.r$list.group[[pair[2]]][2]
            # 这里比较绕，pair是一对儿比较（a和b），start和stop分别是对应
            # 实验组数据的起始和终止列
            df.a <- df.data[, a.start:a.stop]
            df.b <- df.data[, b.start:b.stop]
            mean.a <- apply(df.a, MARGIN = 1, mean)
            mean.b <- apply(df.b, MARGIN = 1, mean)
            vip <- ''  # 空出一行给vip值
            p <- sapply(1:nrow(df.data), function(j, df.a, df.b) {
                # 先检验方差齐性，再计算p-value
                #
                line.a <- df.a[j, ]
                line.b <- df.b[j, ]
                var <- var.test(unlist(line.a), unlist(line.b))$p.value > 0.05
                # f检验，验证方差齐性，这么写好溜啊~
                p <- t.test(line.a, line.b, var.equal = var)$p.value  # t检验
                return(p)
            }, df.a = df.a, df.b = df.b)
            q <- fdrtool(p, statistic = 'pvalue', plot = F, verbose = F)$qval
            # {fdrtool}
            fc <- mean.a / mean.b  # fold change
            log.fc <- log2(fc)  # log2 fold change
            df.combine <- data.frame(mean.a, mean.b, vip, p, q, fc, log.fc)
            names(df.combine) <- c(
                paste('MEAN', l.r$sample[l.r$list.compare[[i]][1]]), 
                paste('MEAN', l.r$sample[l.r$list.compare[[i]][2]]), 
                'VIP', 'P-VALUE', 'Q-VALUE', 'FOLD CHANGE', 'LOG_FOLDCHANGE'
            )
            df.out <- cbind(df.in[, 1:6], df.combine)
            df.out <- df.out[order(-df.out$Similarity), ]
            df.out$Similarity <- round(df.out$Similarity)
            write.xlsx2(
                df.out, paste(l.a$outResult, '_DEG-PEAK.xlsx', sep = '/'), 
                sheetName = paste(
                    l.r$sample[l.r$list.compare[[i]][1]], 
                    l.r$sample[l.r$list.compare[[i]][2]], 
                    sep = '-'
                ), row.names = F, append = T
            )
            return(df.out)
        }
    )
    return(list.compare.result)
}
################################################################################
DealWorkspace <- function() {
    if(!dir.exists(l.a$outWS)) {dir.create(l.a$outWS)}
    save(l.a, l.r, file = paste(l.a$outWS, 'listArgs.RData', sep = '/'))
    save.image(file = paste(l.a$outWS, 'biotree.RData', sep = '/'))
}
################################################################################
KEGGPart1 <- function() {
    lapply(
        1:length(l.r$list.compare), function(i) {
            keggYang(i)
        }
    )
}
################################################################################
KEGGPart2 <- function() {
    lapply(
        1:length(l.r$list.compare), function(i) {
            compound(i)
        }
    )
}
################################################################################
keggYang <- function(k) {
    fiehn <- read.csv('./in/newfiehn.csv', stringsAsFactors = F)
    for(i in 1:nrow(fiehn)){
        a <- 0
        for(j in i+1:nrow(fiehn)){
            if(fiehn[j,1]!=""|j>nrow(fiehn)){
                j <- j-1
                break
            }
        }
        if(i!=j){
            a <- fiehn[i,14]
            for(x in (i+1):j){
                a <- paste(a,fiehn[x,14],sep="; ")
            }
            fiehn[i,14] <- a
        }
    }
    newdata <- fiehn[which(fiehn[, 1] != ""), ]
    peak <- read.table(
        paste('../06_KEGG/01_DEG-PEAK/', l.r$subDir[k], '.txt', sep = ''), 
        stringsAsFactors = F, sep = '\t', check.names = F, header = T
    )
    finaldata <- 0
    num <- 0
    pick <- 0
    for(i in 1:nrow(peak)){
        num <- 0
        a <- tolower(peak[i,2])
        for(j in 1:nrow(newdata)){
            line1 <- strsplit(tolower(newdata[j,1]),";",fixed=TRUE)
            pick <- line1[[1]]
            if(!is.na(match(a,pick))){
                num <- c(num,j)
            }
        }
        if(length(num)>1){
            num <- num[-1]
            finaldata <- rbind(
                finaldata, 
                cbind(
                    peak[i, 1:2], newdata[num, 2:14], peak[i, 12], 
                    row.names = NULL
                ), row.names = NULL
            )
        }
    }
    finaldata <- finaldata[-1,]
    finaldata[,8] <- substr(finaldata[,8],1,6)
    keggnum <- finaldata[which(finaldata[,8]!=""),c(8,16)]
    for(i in 1:nrow(keggnum)){
        if(keggnum[i,2]>1){
            keggnum[i,1] <- paste(keggnum[i,1]," red")
        }else if(keggnum[i,2]<1){
            keggnum[i,1] <- paste(keggnum[i,1]," blue")
        }
    }
    output.dir <- '../06_KEGG/02_KEGGcol'
    if(!dir.exists(output.dir)) dir.create(output.dir)
    output.dir <- paste('../06_KEGG/02_KEGGcol', l.r$subDir[k], sep = '/')
    if(!dir.exists(output.dir)) dir.create(output.dir)
    write.csv(
        finaldata[,1:(ncol(finaldata)-1)], 
        paste(output.dir, 'final.csv', sep = '/'), row.names = F
    )
    write.csv(
        keggnum[,1], 
        paste(output.dir, 'keggcol.csv', sep = '/'), row.names = F
    )
}
################################################################################
compound <- function(k){
    raw <- read.table(
        paste('../06_KEGG/03_compoundraw/', l.r$subDir[k], '.txt', sep = ''), 
        header = F, stringsAsFactors = F, sep = '\t', quote = ''
    )
    a <- 0
    b <- 0
    c <- 0
    for (i in 1:nrow(raw)){
        if(substr(raw[i,1],1,3)=='cpd'){
            a[i] <- ''
            b[i] <- ''
            next
        }
        a[i] <- raw[i,1]
        j <- i+1
        c <- ''
        while(j<=nrow(raw) & substr(raw[j,1],1,3)=='cpd'){
            c <- paste(c,' ',raw[j,1],sep='')
            j <- j+1
        }
        b[i] <- substr(c,2,nchar(c))
    }
    c <- cbind(a,b)
    c <- as.matrix(c)
    for(i in 1:nrow(c)){
        if(c[i,1]==''){
            c[i,1] <- NA}}
    d <- na.omit(c)
    d[,1] <- chartr('?',' ',d[,1])
    write.table(
        d, paste('../06_KEGG/04_compound/', l.r$subDir[k], '.txt', sep = ''), 
        sep = '\t', row.names = F
    )
}
################################################################################
# MAIN
l.r <- ArgsVarify()  # 确认参数
# BuildProjectFramework()  # 搭建项目框架
df.ori <- ReadInput()
df.area <- DataReshape()
df.filtered <- RawFilter(df.area)  # 过滤离群点/不稳定点
df.recoded <- RecodeNA(df.filtered)  # 重编码缺失值
df.final <- Normalize(df.recoded)  # 归一化
# df.out.mean <- OutputMean(df.final)  # 输出MEAN表
# df.out.SIMCA <- OutputSIMCA(df.final)  # SIMCA分析前置格式整理部分
list.compare.result <- OutputDegPeak(df.final)  # 单变量统计分析
DealWorkspace()  # 保存工作空间，给别的脚本用
# KEGGPart1()
# KEGGPart2()
