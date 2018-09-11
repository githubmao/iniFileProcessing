#-------------- Code Description ----------------------------------------------#
# Description: 统计各类事件类型，分析特征场景。
#
# Notes:
# ver0.1, 20180911，By MaoYan: 。
#
#------------------------------------------------------------------------------#


library(ggplot2)


# 加载数据导入需要的函数
source('E:/R/iniFile/manualCodingDataInput.R', encoding = 'UTF-8')


# 分析各类事件出现的频次----
df.evntdata$evntTyp <- as.factor(df.evntdata$evntTyp)

# 不同事件类型分类
df.singleevnt <- subset(x = df.evntdata,
                        subset = evntTyp == "10" |
                                 evntTyp == "11" |
                                 evntTyp == "12" |
                                 evntTyp == "13" |
                                 evntTyp == "14")  # 单车事件

df.rearendevnt <- subset(x = df.evntdata,
                         subset = evntTyp == "21" |
                                  evntTyp == "22")  # 追尾事件

df.headonevnt <- subset(x = df.evntdata,
                        subset = evntTyp == "31" |
                                 evntTyp == "32")  # 正面碰撞事件

df.sideswipe <- subset(x = df.evntdata,
                       subset = evntTyp == "23" |
                                evntTyp == "33")  # 侧面刮擦

df.crossingevnt <- subset(x = df.evntdata,
                          subset = evntTyp == "41" |
                                   evntTyp == "42" |
                                   evntTyp == "43" |
                                   evntTyp == "44" |
                                   evntTyp == "51")  # 交叉事件

df.cyclistevnt <- subset(x = df.evntdata,
                         subset = evntTyp == "61")  # 与自行车事件

df.pedestrianevnt <- subset(x = df.evntdata,
                            subset = evntTyp == "71")  # 与行人事件

df.otherevnt <- subset(x = df.evntdata,
                       subset = evntTyp == "88")  # 其他事件

df.unknownevnt <- subset(x = df.evntdata,
                         subset = evntTyp == "99")  # 其他事件


# 确定各类事件分类的类型次数，总共40种行车场景
nrow(df.singleevnt)
kSingleEvntTypNum <- ceiling(nrow(df.singleevnt) * 40 / nrow(df.evntdata))
kSingleEvntTypNum

nrow(df.rearendevnt)
kRearEndEvntTypNum <- ceiling(nrow(df.rearendevnt) * 40 / nrow(df.evntdata))
kRearEndEvntTypNum

nrow(df.headonevnt)
kHeadOnEvntTypNum <- ceiling(nrow(df.headonevnt) * 40 / nrow(df.evntdata))
kHeadOnEvntTypNum

nrow(df.sideswipe)
kSideSwipeEvntTypNum <- ceiling(nrow(df.sideswipe) * 40 / nrow(df.evntdata))
kSideSwipeEvntTypNum

nrow(df.crossingevnt)
kCrossingEvntTypNum <- ceiling(nrow(df.crossingevnt) * 40 / nrow(df.evntdata))
kCrossingEvntTypNum

nrow(df.cyclistevnt)
kCyclistEvntTypNum <- ceiling(nrow(df.cyclistevnt) * 40 / nrow(df.evntdata))
kCyclistEvntTypNum

nrow(df.pedestrianevnt)
kPedestrianEvntTypNum <- ceiling(nrow(df.pedestrianevnt) * 40 /
                                   nrow(df.evntdata))
kPedestrianEvntTypNum

nrow(df.otherevnt)
kOtherEvntTypNum <- ceiling(nrow(df.otherevnt) * 40 / nrow(df.evntdata))
kOtherEvntTypNum

nrow(df.unknownevnt)
kUnknownEvntTypNum <- ceiling(nrow(df.unknownevnt) * 40 / nrow(df.evntdata))
kUnknownEvntTypNum


# 单车事件聚类分析，分4类----








