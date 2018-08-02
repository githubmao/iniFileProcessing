#-------------- Code Description ----------------------------------------------#
# Description: 导入.ini数据，并将其整理成需要的数据格式。
#
# Notes:
# ver0.1, 20180503, By MaoYan：尝试单个ini数据导。
# ver1.0, 20180510, By MaoYan: 完成第一批529个ini数据的批量导入及预处理。
# ver2.0, 20180802, By MaoYan: 完成8893个ini数据的批量导入及预处理。
#
#------------------------------------------------------------------------------#


library(ini)
library(rlist)
library(magrittr)

# 加载数据导入需要的函数
source('E:/R/iniFile/Functions.R', encoding = 'UTF-8')


# Main，文本数据导入及预处理----------------------------------------------------
setwd("E:/R/iniFile/Data/")
get.filename <- list.files(path = "E:/R/iniFile/Data/", pattern = ".ini")
get.dfname <- gsub(pattern = ".ini", replacement = "", x = get.filename)

df.event <- data.frame()  # 整理后的数据框

# 目标时刻向量
vector.time <- c("Time -8.00",
                 "Time -7.75", "Time -7.50", "Time -7.25", "Time -7.00",
                 "Time -6.75", "Time -6.50", "Time -6.25", "Time -6.00",
                 "Time -5.75", "Time -5.50", "Time -5.25", "Time -5.00",
                 "Time -4.75", "Time -4.50", "Time -4.25", "Time -4.00",
                 "Time -3.75", "Time -3.50", "Time -3.25", "Time -3.00",
                 "Time -2.75", "Time -2.50", "Time -2.25", "Time -2.00",
                 "Time -1.75", "Time -1.50", "Time -1.25", "Time -1.00",
                 "Time -0.75", "Time -0.50", "Time -0.25",
                 "Time +0.00", "Time +0.25", "Time +0.50", "Time +0.75",
                 "Time +1.00", "Time +1.25", "Time +1.50", "Time +1.75",
                 "Time +2.00", "Time +2.25", "Time +2.50", "Time +2.75",
                 "Time +3.00", "Time +3.25", "Time +3.50", "Time +3.75")

# 标签向量
vector.tag <- c("T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8", "T9", "T10",
                "T11", "T12", "T13", "T14", "T15", "T16", "T17", "T18", "T19",
                "T20", "T21", "T22", "T23", "T24", "T25", "T26", "T27", "T28",
                "T29", "T30", "T31", "T32", "T33", "T34", "T35", "T36", "T37",
                "T38", "T39", "T40", "T41", "T42", "T43", "T44", "T45", "T46",
                "T47", "T48")

# 数据导入
for (kFileIdx in 1:length(get.filename)) {
  
  tmp.file <- read.ini(filepath = get.filename[kFileIdx],
                       encoding = getOption("encoding"))  # 单个ini导入
  
  # Basic Information 提取
  tmpdf.event <- cbind(evntNum = get.dfname[kFileIdx],  # 事件编号
                       GetBasicInformation(tFile = tmp.file))
  
  # Move Information 提取
  tmpdf.event <- cbind(tmpdf.event,
                       GetBatchMoveInfo(tFile = tmp.file,
                                        kVectorTime = vector.time,
                                        kVectorTag = vector.tag))
  
  # 合并数据
  df.event <- rbind(df.event, tmpdf.event)
}





















