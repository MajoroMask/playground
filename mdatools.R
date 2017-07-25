library(mdatools)
load("in/df.simca.Rdata")
m <- pca(df.out.SIMCA[, -1], scale = T)  # 创建PCA模型
plotVariance(m)  # fa图
plot(m)
m.mod <- selectCompNum(m, 4)  # 还可以自己设置主成分个数的
plot(m.mod)
plotScores(m, c(1, 3))  # 还能挑主成分的