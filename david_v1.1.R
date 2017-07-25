#!/usr/bin/env R
################################################################################
# Version 1.0
# Su Na 2016-08-18
# Encoding in UTF-8
################################################################################
# Preparation
#     1 从BioConductor上安装RDAVIDWebService包。
#     2 在https://david.ncifcrf.gov/webservice/register.htm注册DAVID
#     3 如果在创建david类对象出现和Java有关的报错时，请参考
#       https://support.bioconductor.org/p/70090/
# 
# 心得
#     1 这个包的所有（？）功能都建立在一个DAVIDWebService的Reference上，调用函数
#       格式为object_name$method_name(parameters)。比如如果我需要查看所有DAVID
#       支持的ID类型，我需要先建立一个DAVIDWebService对象（比如下面的david），然
#       后利用david$getIDTypes()查看所有ID类型。
################################################################################
# Update
# 2016-08-22 v1.1
#     添加对.csv格式的输出表格格式支持
#     添加对柱状图作图数据的输出表格支持
################################################################################
library(RDAVIDWebService)
library(ggplot2)
################################################################################
# MAIN
DAVID <- function(
    list, email, id.type, list.name = 'list1', list.type = 'Gene', 
    cat.t = NULL, cat.c = NULL, threshold = 0.1, count = 2, 
    r.t = F, r.c = F, r.t.csv = F, r.c.csv = T, r.t.split = T, 
    plot.t = T, plot.c = T, pathway.top = 10, cluster.top = 10, 
    output.dir = NULL
) {
    # 概述
    #     DAVID()根据输入的参数，链接DAVID Web Service，进行富集分析，依据参数
    #     输出整合的报表和图片。
    # 
    # 参数
    #     list：向量，输入的identifier列表。
    #     email：字符串，注册DAVID账号的邮箱。非常重要，如果没有在DAVID注册，
    #         可使用公司邮箱注册，注册地址为
    #         https://david.ncifcrf.gov/webservice/register.htm
    #     id.type：字符串，DAVID支持的id类型。使用RDAVIDWebService包的
    #         $getALLAnnotationCategoryNames()方法可以查看所有支持的DAVID id类型
    #     list.name：字符串，列表名称。可以随便取，默认值为'list1'
    #     list.type：字符串，列表类型。取值Gene'或者'Background'，默认'Gene'
    #     cat.t：向量，在GO注释富集分析中考察的注释类型。默认为2~4级和FAT级
    #         的所有类型（BP，CC和MF）的GO注释，使用RDAVIDWebService包的
    #         $getALLAnnotationCategoryNames()方法可以查看所有注释类型
    #     cat.c：向量，在有向无环图（direct acyclic graph）中考察的富集注释类型。
    #         默认为ALL级的所有类型（BP，CC和MF）的GO注释，
    #         使用RDAVIDWebService包的
    #         $getALLAnnotationCategoryNames()方法可以查看所有注释类型
    #     threhold：数字，ease score的卡值。默认0.1，取值区间[0, 1]
    #     count：数字，个数卡值。默认2
    #     r.t：逻辑或字符串，report.table的缩写，说明是否写出.tab格式的富集分析
    #         报表和输出报告的位置。
    #         默认F，默认位置'./out/DAVIDReportTable.tab'。
    #     r.c：逻辑或字符串，report.cluster的缩写，说明是否写出.tab格式的
    #         DAVID对term的聚类分析报表和输出报表的位置。
    #         默认F，默认位置'./out/DAVIDReportCluster.tab'。
    #     r.t.csv：逻辑值或字符串，是否生成.csv格式的table结果和报告的文件名。
    #         默认F，默认位置'./out/DAVIDReportTable.csv'。
    #     r.c.csv：逻辑值或字符串，是否生成.csv格式的cluster结果。默认T。
    #         默认T，默认位置'./out/DAVIDclusterTable.csv'。
    #     r.t.split：逻辑，是否按照柱状图的归类，生成对应的.csv格式表格，
    #         默认T，文件名同对应柱状图。
    #     plot.t：逻辑或向量，说明是否生成柱状图，以及柱状图对category的归类。
    #         默认输出柱状图，300dpi，tiff格式。
    #         归类为GO注释的分级，这种分类的公式详见字符串处理函数的pattern
    #     plot.c：
    #     pathway.top：数字，输出柱状图中单个类的柱子个数上限。默认10
    #     cluster.top：数字，输出有向无环图的cluster数上限。默认10
    #     output.dir：字符串，输出报表和/或图片的位置。默认位置'./out'文件夹
    # 
    # 输出
    #     列表，包含上述参数以及报表。
    # 
    # 例子：
    #     l.david <- DAVID(read.table('input.txt')$V1, 
    #     email = 'suna@biotree.com.cn', id.type = 'UNIPROT_ACCESSION')
    # 
    l.david <- SetArguements(  # list.david
        list, email, id.type, list.name, list.type, cat.t, cat.c, threshold, 
        count, r.t, r.c, r.t.csv, r.c.csv, r.t.split, plot.t, plot.c, 
        pathway.top, cluster.top, output.dir
    )
    l.david <- DAVIDStandard(l.david)
    return(l.david)
}
################################################################################
SetArguements <- function(
    list, email, id.type, list.name, list.type, cat.t, cat.c, threshold, count, 
    r.t, r.c, r.t.csv, r.c.csv, r.t.split, plot.t, plot.c, 
    pathway.top, cluster.top, output.dir
) {
    l.david <- list(  # 参数列表list.arguements.david
        list = list, 
        email = email, 
        id.type = id.type, 
        list.name = list.name, 
        list.type = list.type, 
        threshold = threshold, 
        count = as.integer(count), 
        r.t.split = r.t.split, 
        pathway.top = as.integer(pathway.top), 
        cluster.top = as.integer(cluster.top)
    )
    if(is.null(cat.t)) {
        l.david$cat.t <- c(
            'GOTERM_BP_2', 'GOTERM_CC_2', 'GOTERM_MF_2', 
            'GOTERM_BP_3', 'GOTERM_CC_3', 'GOTERM_MF_3', 
            'GOTERM_BP_4', 'GOTERM_CC_4', 'GOTERM_MF_4', 
            'GOTERM_BP_5', 'GOTERM_CC_5', 'GOTERM_MF_5', 
            'GOTERM_BP_FAT', 'GOTERM_CC_FAT', 'GOTERM_MF_FAT'
        )
    } else {
        # 根据自己的需求调整，
        # $getALLAnnotationCategoryNames()方法可以查看所有
        l.david$cat.t <- cat.t
    }
    if(is.null(cat.c)) {
        l.david$cat.c <- c(
            'GOTERM_BP_ALL', 'GOTERM_CC_ALL', 'GOTERM_MF_ALL'
        )
    } else {
        # 根据自己的需求调整，
        # $getALLAnnotationCategoryNames()方法可以查看所有
        l.david$cat.c <- cat.c
    }
    if(is.null(output.dir)) {  # 输出位置
        if(r.t != F | r.c != F | plot.t != F | plot.c != F | 
           r.t.csv != F | r.c.csv != F | r.t.split != F) {
            l.david$output.dir <- './out'
        } else {
            l.david$output.dir <- NULL
        }
    } else {
        l.david$output.dir <- output.dir
    }
    if(!is.null(l.david$output.dir)) {  # 创建输出文件夹
        if(!dir.exists(l.david$output.dir)) dir.create(l.david$output.dir)
    }
    if(r.t == T) {  # 生成富集分析结果的位置，默认为./out/DAVIDReportTable.tab
        l.david$r.t <- paste(
            l.david$output.dir, 'DAVIDReportTable.tab', sep = '/'
        )
    } else if(r.t == F) {
        l.david$r.t <- F
    } else {
        l.david$r.t <- r.t
    }
    if(r.c == T) {  # 生成聚类分析结果的位置，默认为./out/DAVIDReportCluster.tab
        l.david$r.c <- paste(
            l.david$output.dir, 'DAVIDReportCluster.tab', sep = '/'
        )
    } else if(r.c == F) {
        l.david$r.c <- F
    } else {
        l.david$r.c <- r.c
    }
    if(r.t.csv == T) {  
        # 生成富集分析结果的位置，默认为./out/DAVIDReportTable.
        l.david$r.t.csv <- paste(
            l.david$output.dir, 'DAVIDReportTable.csv', sep = '/'
        )
    } else if(r.t.csv == F) {
        l.david$r.t.csv <- F
    } else {
        l.david$r.t.csv <- r.t.csv
    }
    if(r.c.csv == T) {  # 生成聚类分析结果的位置，默认为./DAVIDReportluster.tab
        l.david$r.c.csv <- paste(
            l.david$output.dir, 'DAVIDReportCluster.csv', sep = '/'
        )
    } else if(r.c.csv == F) {
        l.david$r.c.csv <- F
    } else {
        l.david$r.c.csv <- r.c.csv
    }
    if(plot.t == T) {  # 是否作图，以及生成图的公式
        l.david$plot.t <- c(
            'GOTERM_.._2', 'GOTERM_.._3', 'GOTERM_.._4', 'GOTERM_.._5', 
            'GOTERM_.._FAT'
        )
    } else if (plot.t == F) {
        l.david$plot.t <- F
    } else {
        l.david$plot.t <- plot.t
    }
    if(plot.c == T) {  # 是否作图，以及生成图的公式
        l.david$plot.c <- c(
            'BP', 'CC', 'MF'
        )
    } else if (plot.c == F) {
        l.david$plot.c <- F
    } else {
        l.david$plot.c <- plot.c
    }
    return(l.david)
}
################################################################################
DAVIDStandard <- function(l.david) {
    david <- DAVIDWebService$new(  
        # 这句必须提前运行，是运行这个包其它函数的前提
        email = l.david$email, 
        url=paste(
            "https://david.ncifcrf.gov/webservice/services/", 
            "DAVIDWebService.DAVIDWebServiceHttpSoap12Endpoint/", 
            sep = ''
        )
    )
    result <- addList(
        david, l.david$list, idType = l.david$id.type, 
        listName = l.david$list.name, 
        listType = l.david$list.type
    )
    ### 以下为GO注释柱状图和数据部分
    setAnnotationCategories(david, l.david$cat.t)
    if(!is.logical(l.david$r.t) | !is.logical(l.david$r.t.csv)) {
        # 生成报表步骤
        #
        getFunctionalAnnotationChartFile(  # 此处生成一个临时文件
            david, threshold = l.david$threshold, count = l.david$count, 
            fileName = 'temp.tab'
        )
        if(!is.logical(l.david$r.t.csv)) Tab2Csv('temp.tab', l.david$r.t.csv)
        if(!is.logical(l.david$r.t)) {
            file.copy('temp.tab', l.david$r.t, overwrite = T)
        }
        file.remove('temp.tab')  # 删除临时文件
    }
    l.david$report.table <- getFunctionalAnnotationChart(  # 把报告存为数据框
        david, threshold = l.david$threshold, count = l.david$count
    )
    if(!is.logical(l.david$plot.t)) DAVIDBarPlot(l.david)  # 作图函数
    ### 以下为GO注释DAG图和数据部分
    setAnnotationCategories(david, l.david$cat.c)
    if(!is.logical(l.david$r.c) | !is.logical(l.david$r.c.csv)) {
        getClusterReportFile(david, type = 'Term', fileName = 'temp.tab')
        # 同上，此处生成一个临时文件
        if(!is.logical(l.david$r.c.csv)) Tab2Csv('temp.tab', l.david$r.c.csv)
        if(!is.logical(l.david$r.c)) {
            file.copy('temp.tab', l.david$r.c, overwrite = T)
        }
        file.remove('temp.tab')
    }
    l.david$report.cluster <- getClusterReport(david, type = 'Term')
    if(!is.logical(l.david$plot.c)) DAVIDDagPlot(l.david)
    ###
    return(l.david)
}
################################################################################
Tab2Csv <- function(input, output) {
    # 读入.tab格式文件，将.tab格式转换为.csv格式输出
    # 
    temp <- readLines(input)
    temp <- gsub('\t', '\",\"', temp)
    temp <- paste("\"", temp, "\"", sep = '')
    write.table(temp, output, quote = F, sep = ',', 
                row.names = F, col.names = F)
}
################################################################################
DAVIDBarPlot <- function(l.david) {
    guides_merge <- function(gdefs) {
        gdefs <- lapply(gdefs, function(g) {
            g$hash <- paste(g$order, g$hash, sep = "z")
            return(g)
        })
        tapply(
            gdefs, sapply(gdefs, function(g) g$hash), 
            function(gs) Reduce(guide_merge, gs)
        )
    }
    environment(guides_merge) <- environment(ggplot)
    assignInNamespace("guides_merge", guides_merge, pos = "package:ggplot2")
    lapply(l.david$plot.t, function(sub) {
        df.pic <- base::as.data.frame(
            l.david$report.table[
                regexpr(sub, l.david$report.table$Category) != -1, 
            ], stringsAsFactors = F
        )
        if(l.david$r.t.split == T) {
            write.csv(
                df.pic[order(df.pic$Category, -df.pic$Count), ], 
                file = paste(
                    l.david$output.dir, paste(sub, 'csv', sep = '.'), sep = '/'
                ), 
                row.names = F
            )
        }
        df.pic$Category <- as.character(df.pic$Category)
        df.pic$GO <- sub('~.*', '', df.pic$Term)
        df.pic$name <- sub('.*?~', '', df.pic$Term)
        list.pic <- lapply(unique(df.pic$Category), function(cat) {
            df.sub <- df.pic[df.pic$Category == cat, ]
            df.sub <- df.sub[order(-df.sub$Count), ]
            if(nrow(df.sub) > l.david$pathway.top) {
                df.sub <- df.sub[1:l.david$pathway.top, ]
            } 
            return(df.sub)
        })
        df.pic <- do.call(rbind, list.pic)
        df.pic <- df.pic[base::order(df.pic$Category, rev(df.pic$Count)), ]
        df.pic$label <- sapply(df.pic$name, function(name) {
            if(nchar(name) > 40) {
                x <- unlist(strsplit(name, split = ''))
                x <- c(x[1:40], '...')
                name <- paste(x, collapse = '')
            }
            return(name)
        })
        df.pic$name <- factor(df.pic$name, levels = df.pic$name)
        p <- ggplot(df.pic, 
                    aes(name, y = Count, fill = Category, color = Category, 
                        alpha = -log10(PValue))) +
            geom_bar(stat = 'identity') +
            theme_bw() +
            scale_x_discrete(labels = df.pic$label) +
            scale_alpha_continuous(
                expression(paste(-log[10], '(', italic(P), '-value)')), 
                range = c(0.2, 1)
            ) +
            theme(
                axis.text.x = element_text(angle = 60, hjust = 1), 
                panel.border = element_rect(color = 'grey60'), 
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                legend.box = "horizontal", 
                legend.position = 'top'
            ) +
            guides(
                # color = guide_legend('Category', order = 1), 
                # fill = guide_legend('Category', order = 2), 
                alpha = guide_legend(order = 2)
            ) +
            labs(x = 'Pathway name', y = 'Count')
        ggsave(
            filename = paste(
                l.david$output.dir, paste(sub, 'tiff', sep = '.'), sep = '/'
            ), 
            width = l.david$pathway.top * 30, height = 200, units = 'mm', 
            dpi = 300
        )
    })
}
################################################################################
DAVIDDagPlot <- function(l.david) {
    i <- min(length(members(l.david$report.cluster)), l.david$cluster.top)
    lapply(1:i, function(i) {
        subDag <- l.david$report.cluster@cluster[[i]]
        lapply(l.david$plot.c, function(type) {
            if(any(grepl(type, subDag$Members$Category))) {
                davidGODag <- DAVIDGODag(
                    members(l.david$report.cluster)[[i]], 
                    pvalueCutoff = l.david$threshold, type
                )
                if(length(davidGODag@goDag@nodes) != 0) {
                    pdf(file = paste(
                        l.david$output.dir, 
                        paste(
                            sprintf('Cluster_%02d', i), '_', 
                            type, '.pdf', sep = ''
                        ), 
                        sep = '/'
                    ))
                    plotGOTermGraph(
                        g = goDag(davidGODag), r = davidGODag, max.nchar = 40, 
                        node.shape = "ellipse"
                    )
                    dev.off()
                }
            }
        })
    })
}