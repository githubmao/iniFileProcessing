#-------------- Code Description ----------------------------------------------#
# Description: 导入.ini数据，并将其整理成需要的数据格式。
#
# Notes:
# ver0.1, 20180503, By MaoYan：尝试单个ini数据导。
# ver1.0, 20180510, By MaoYan: 完成第一批529个ini数据的批量导入及预处理。
#
#------------------------------------------------------------------------------#


library(ini)  # 加载ini数据处理package

# 加载数据导入需要的函数
source('E:/R/iniFile/Functions.R', encoding = 'UTF-8')


# Main，文本数据导入及预处理----------------------------------------------------
setwd("E:/R/iniFile/Data/")
get.filename <- list.files(path = "E:/R/iniFile/Data/", pattern = ".ini")
get.dfname <- gsub(pattern = ".ini", replacement = "", x = get.filename)

df.eventdata <- data.frame()  # 整理后的数据框

for (i in 1:length(get.filename)) {
  
  tfile <- read.ini(filepath = get.filename[i],
                    encoding = getOption("encoding"))  # 单个ini导入
  
  tmp.eventdf <- data.frame(eventNum = get.dfname[i])  # 事件编号
  
  # Basic Information数据提取
  tmp.eventdf <- cbind(tmp.eventdf,
                       GetBasicInformation(tfile))
  
  # Move Information数据提取，from Move Information0 to Move Information47
  for (kTimeNum in 0:47) {
    
    tmp.moveinformation <- GetMoveInformation(tfile,
                                              kTag = paste("Move Information",
                                                           kTimeNum, sep = ""),
                                              kTime = paste("Time",
                                                            kTimeNum, sep = ""))
    tmp.eventdf <- cbind(tmp.eventdf, tmp.moveinformation)
  }
  
  # 合并数据
  df.eventdata <- rbind(df.eventdata, tmp.eventdf)
}






