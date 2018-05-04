#-------------- Code Description ----------------------------------------------#
# Description: 导入.ini数据，并将其整理成需要的数据格式。
#
# Notes:
# ver0.1, 20180503, By MaoYan：第一批529个数据。
#
#------------------------------------------------------------------------------#


library(ini)  # 加载ini数据处理package

# 加载数据导入需要的函数
#source('E:/R/iniFile/Functions.R', encoding = 'UTF-8')

tfile <- read.ini(filepath = "E:/R/iniFile/Data/Event196.ini",
                  encoding = getOption("encoding"))


# Basic Information, recordDate----
tmp.recorddate <- tfile$`Basic Information`$Record_Date  # 原始recordDate
tmp.dayoftheweek <- GetDayoftheWeek(strsplit(tmp.recorddate,
                                             split = ",")[[1]][1])  # 星期几
tmp.month <- GetMonth(strsplit(strsplit(tmp.recorddate, split = ",")[[1]][2],
                               split = " ")[[1]][2])  # 月份
tmp.date <- strsplit(strsplit(tmp.recorddate, split = ",")[[1]][2],
                     split = " ")[[1]][3]  # 日
tmp.year <- strsplit(strsplit(tmp.recorddate, split = ",")[[1]][3],
                     split = " ")[[1]][2]  # 年

# Basic Information, eventTime----
tmp.eventtime <- tfile$`Basic Information`$Time_Of_Event  # 原始eventTime
tmp.time <- strsplit(tmp.eventtime, split = " ")[[1]][1]  # 时间，12h
tmp.timehour <- strsplit(strsplit(tmp.eventtime, split = " ")[[1]][1],
                         ":")[[1]][1]  # 时间，12h，时
tmp.timeminute <- strsplit(strsplit(tmp.eventtime, split = " ")[[1]][1],
                           ":")[[1]][2]  # 时间，12h，分
tmp.ampm <- ifelse(strsplit(tmp.eventtime, split = " ")[[1]][2] == "上午",
                   "A.M.", "P.M.")  # 时段
tmp.timezone <- strsplit(tmp.eventtime, split = " ")[[1]][3]  # 时区

# Basic Information, eventTrigger----
tmp.eventtrigger <- tfile$`Basic Information`$Event_Trigger  # 原始eventTrigger

# Basic Information, maxForwardTForce----
tmp.maxforwardtforce <- tfile$`Basic Information`$Max_Forward_T_Force

# Basic Information, maxLateralTForce----
tmp.maxlateraltforce <- tfile$`Basic Information`$Max_Lateral_T_Force

# Basic Information, triggerSpeed----
tmp.triggerspeedKMH <- strsplit(tfile$`Basic Information`$Speed,
                                split = " ")[[1]][1]

# Basic Information, moveDirection----
tmp.moveDirection <- tfile$`Basic Information`$Heading

# Basic Information, locationLatitude----
tmp.locationlatitude <- tfile$`Basic Information`$Loaction_N

# Basic Information, locationLongitude----
tmp.locationlongitude <- tfile$`Basic Information`$Loaction_E










tfile <- read.ini(filepath = "E:/R/Recording.ini", encoding = getOption("encoding"))





checkini <- read.ini(iniFile)

read.ini(filepath = "E:/R/iniFile/")

devtools::install_github("Miachol/configr")

library(configr)
is.ini.file(file = "E:/R/iniFile/Record.ini")

config.ini <- system.file('extdata', 'config.ini', package='configr')
config.ini
ini <- get.config.type(file = "E:/R/iniFile/Record.ini") 

ini.list <- read.config(file = "E:/R/iniFile/Record.ini")
ini.list

config.ini.obj <- eval.config(file = "E:/R/iniFile/Record.ini")

ini.groups <- eval.config.groups(file = "E:/R/iniFile/Record.ini")

ini.config.all <- eval.config.merge(file = "E:/R/iniFile/Record.ini")

ini.config.all

ini.list[[68]][4]$GPS
strsplit(ini.list[[68]][4]$GPS, split = " ")[[1]][1]
as.numeric(strsplit(ini.list[[68]][4]$GPS, split = " ")[[1]][1])
class(as.numeric(strsplit(ini.list[[68]][4]$GPS, split = " ")[[1]][1]))


ini.list[[1]]$Loaction_N
