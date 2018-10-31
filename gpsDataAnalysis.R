#-------------- Code Description ----------------------------------------------#
# Description: 统计各类事件类型，分析特征场景。
#
# Notes:
# ver1.0, 20181022，By MaoYan: GPS data 处理。
#
#------------------------------------------------------------------------------#


library(stringr)


# locLat处理----
tmp.loclat <- df.evntdata$locLat  # 原始locLat

tmp.lat1 <- str_split(string = tmp.loclat, pattern = "°", simplify = TRUE)
tmp.loclatD <- tmp.lat1[,1]  # location latitude, 度

tmp.lat2 <- str_split(string = tmp.lat1[,2], pattern = "'", simplify = TRUE)
tmp.loclatM <- tmp.lat2[,1]  # location latitude, 分

tmp.lat3 <- str_split(string = tmp.lat2[,2], pattern = "\\\"", simplify = TRUE)
tmp.loclatS <- tmp.lat3[,1]  # location latitude, 秒

tmp.loclatDMS <- as.numeric(tmp.loclatD) +
  as.numeric(tmp.loclatM) / 60 +
  as.numeric(tmp.loclatS) / 3600  # location latitude, DMS

df.evntdata$locLat <- tmp.loclatDMS

remove(tmp.loclat)
remove(tmp.lat1)
remove(tmp.lat2)
remove(tmp.lat3)
remove(tmp.loclatD)
remove(tmp.loclatM)
remove(tmp.loclatS)


# locLong处理----
tmp.loclong <- df.evntdata$locLong  # 原始locLong

tmp.long1 <- str_split(string = tmp.loclong, pattern = "°", simplify = TRUE)
tmp.loclongD <- tmp.long1[,1]  # location longitudinal, 度

tmp.long2 <- str_split(string = tmp.long1[,2], pattern = "'", simplify = TRUE)
tmp.loclongM <- tmp.long2[,1]  # location longitudianl, 分

tmp.long3 <- str_split(string = tmp.long2[,2], pattern = "\\\"", simplify = TRUE)
tmp.loclongS <- tmp.long3[,1]  # location longitudianl, 秒

tmp.loclongDMS <- as.numeric(tmp.loclongD) +
  as.numeric(tmp.loclongM) / 60 +
  as.numeric(tmp.loclongS) / 3600  # location longitudinal, DMS

df.evntdata$locLong <- tmp.loclongDMS

remove(tmp.loclong)
remove(tmp.long1)
remove(tmp.long2)
remove(tmp.long3)
remove(tmp.loclongD)
remove(tmp.loclongM)
remove(tmp.loclongS)


# ggmap绘图----
# library(ggmap)  # 访问太慢，连接不上

# REmap绘图----
# library(REmap)  # 不支持经纬度点坐标绘图
# options(remap.ak = "e4ftlR0lN5aeogQUD6soCyS0G2qckOUz")  # baidu API


# leafletCN绘图----
library(leafletCN)
library(leaflet)
library(magrittr)


# 将GPS坐标转换为高德火星坐标，主函数是GPSToGaoDecoords( GPSData)----
transformLon <- function(x, y) {
  ret <- 300.0 + x + 2.0 * y + 0.1 * x * x + 0.1 * x * y + 0.1 * sqrt(abs(x))
  ret <- ret + (20.0 * sin(6.0 * x * pi) +
                  20.0 * sin(2.0 * x * pi)) * 2.0 / 3.0
  ret <- ret + (20.0 * sin(x * pi) +
                  40.0 * sin(x / 3.0 * pi)) * 2.0 / 3.0
  ret <- ret + (150.0 * sin(x / 12.0 * pi) +
                  300.0 * sin(x / 30.0 * pi))* 2.0 / 3.0
  return (ret)
}


transformLat <- function(x, y) {
  ret <- (-100.0) + 2.0 * x + 3.0 * y + 0.2 * y * y +
    0.1 * x * y + 0.2 *sqrt(abs(x))
  ret <- ret + (20.0 * sin(6.0 * x * pi) +
                  20.0 * sin(2.0 * x * pi)) * 2.0 / 3.0
  ret <- ret + (20.0 * sin(y * pi) +
                  40.0 * sin(y / 3.0 * pi)) * 2.0 / 3.0
  ret <- ret + (160.0 * sin(y / 12.0 * pi) +
                  320 * sin(y * pi / 30.0)) * 2.0 / 3.0
  return (ret)
}


GPSToGaoDecoords <- function(GPSData) {
  a <- 6378245.0
  ee <- 0.00669342162296594323
  colnames(GPSData) <- c("long", "lat")
  GPSData$dLat <- transformLat(GPSData$long - 105.0, GPSData$lat - 35.0)
  GPSData$dLong <- transformLon(GPSData$long - 105.0, GPSData$lat - 35.0)
  GPSData$radLat <- GPSData$lat / 180.0 * pi
  GPSData$magic <- sin(GPSData$radLat)
  GPSData$magic <- 1 - ee * GPSData$magic * GPSData$magic
  GPSData$sqrtMagic <- sqrt(GPSData$magic)
  GPSData$dLat <- (GPSData$dLat * 180.0) /
    ((a * (1 - ee)) / (GPSData$magic * GPSData$sqrtMagic) * pi)
  GPSData$dLong <- (GPSData$dLong * 180.0) /
    (a / GPSData$sqrtMagic * cos(GPSData$radLat) * pi)
  GPSData$latitude <- GPSData$lat + GPSData$dLat
  GPSData$longitude <- GPSData$long + GPSData$dLong
  return(subset(GPSData, select = c("longitude","latitude")))
}


# evntData的经纬度校正
df.coord <- data.frame(long = tmp.loclongDMS, lat = tmp.loclatDMS)
df.coord <- GPSToGaoDecoords(df.coord)


# 未校正的经纬度数据绘图，addTiles使用openstreetmap，加载原始gps数据
leaflet() %>%
  addTiles() %>%
  addCircles(lng = df.evntdata$locLong,
             lat = df.evntdata$locLat)


# 校正的经纬度数据绘图，amap使用高德地图，加载转换后的gps数据
leaflet() %>%
  amap() %>%
  addCircles(lng = df.coord$longitude,
             lat = df.coord$latitude)  # 点标记





