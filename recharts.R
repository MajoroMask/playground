library(recharts)
mapData <- head(mapTestData_chs, 5)
eMap(mapData, namevar=~stdName, datavar = ~val1 + val2)
provinceMapData <- mapTestData_chs[6:15,]
eMap(
    provinceMapData, namevar=~stdName, datavar = ~val1+val2, 
    region=unique(provinceMapData$motherName)[1]
)