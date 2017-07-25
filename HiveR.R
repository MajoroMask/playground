library(HiveR)
library(grid)
test2 <- ranHiveData(nx = 2)
test3 <- ranHiveData(nx = 3)
plotHive(test3)
plotHive(
    test3, ch = 5, 
    axLabs = c("axis 1", "axis 2", "axis 3"), 
    axLab.pos = c(10, 15, 15), rot = c(0, 30, -30), 
    axLab.gpar = gpar(col = "orange", fontsize = 14)
)
data(HEC)
plotHive(
    HEC, ch = 0.1, bkgnd = "white", 
    axLabs = c("hair\ncolor", "eye\ncolor"), 
    axLab.pos = c(1, 1), 
    axLab.gpar = gpar(fontsize = 14)
)
grid.text("males", x = 0, y = 2.3, default.units = "native")
grid.text("females", x = 0, y = -2.3, default.units = "native")
grid.text(
    "Pairing of Eye Color with Hair Color", x = 0, y = 4, 
    default.units = "native", gp = gpar(fontsize = 18)
)
# 3d ----
require("rgl")
test4 <- ranHiveData(nx = 4, type = "3D")
plot3dHive(test4)
