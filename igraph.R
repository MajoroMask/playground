source('D:/AUsefulScripts/R/detachAllPackages.R')
detachAllPackages()
library(igraph)
g1 <- graph(edges = c(1, 2, 2, 3, 3, 1), n = 3, directed = F)
plot(g1)
# network创建，edges比较写意，奇数个对偶数个
class(g1)
# 特殊的class，数据类型是列表，带着各种attr
g2 <- graph(edges = c(1, 2, 2, 3, 3, 1), n = 10, directed = F)
plot(g2)
# 没edges的部分就是单点了
g4 <- graph(
    c("John", "Jim", "Jim", "Jack", "Jim", "Jack", "John", "John"), 
    isolates = c("Jesse", "Janis", "Jennifer", "Justin")
)
# isolates是edges为字符串型时独有，标明没edge的node，跟数字型里的n类似
plot(
    g4, edge.arrow.size = .5, vertex.color = "gold", vertex.size = 15, 
    vertex.frame.color = "gray", vertex.label.color = "black", 
    vertex.label.cex = 0.8, vertex.label.dist = 2, edge.curved = 0.2
)
# 卧槽，各种图形属性
E(g4)
V(g4)
g4[]
g4[1, ]
# attr的专门函数，E和V分别对应edges和vertex，输出也是带attr的各种东西
V(g4)$gender <- c('M', 'M', 'M', 'M', 'F', 'F', 'M')
vertex_attr(g4)
# 还可以各种修改attr，用于后续分析
E(g4)$type <- "email"
edge_attr(g4)
# 同上
graph_attr(g4)
# 另一种方法是直接利用set_graph_attr()和delete_graph_attr()
plot(
    g4, edge.arrow.size = .5, vertex.label.color = "black", 
    vertex.label.dist = 1.5, 
    vertex.color = c("pink", "skyblue")[1+(V(g4)$gender == "M")]
) 
# 然后就可以用attr做各种羞羞的事情~，如图形和属性映射
g4s <- simplify(
    g4, remove.multiple = T, remove.loops = F, 
    edge.attr.comb = c(weight = "sum", type = "ignore")
)
plot(g4s, vertex.label.dist = 1.5)
# g4中有重复和loop，利用simplify()进行合并，有多种计算方法用以合并
g4s
# 输出比较抽象，描述了network的各种性质
# 以四个字母起始
#     1 D or U, for a directed or undirected graph
#     2 N for a named graph (where nodes have a name attribute)
#     3 W for a weighted graph (where edges have a weight attribute)
#     4 B for a bipartite (two-mode) graph (where nodes have a type attribute)
g <- make_ring(10)
g$layout <- layout_in_circle
plot(g)
tkplot(g)
rglplot(g)









