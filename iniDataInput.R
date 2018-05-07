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

# Basic Information----
# BI, recordDate----
tmp.recorddate <- tfile$`Basic Information`$Record_Date  # 原始recordDate
tmp.dayoftheweek <- GetDayoftheWeek(strsplit(tmp.recorddate,
                                             split = ",")[[1]][1])  # 星期几
tmp.month <- GetMonth(strsplit(strsplit(tmp.recorddate, split = ",")[[1]][2],
                               split = " ")[[1]][2])  # 月份
tmp.date <- strsplit(strsplit(tmp.recorddate, split = ",")[[1]][2],
                     split = " ")[[1]][3]  # 日
tmp.year <- strsplit(strsplit(tmp.recorddate, split = ",")[[1]][3],
                     split = " ")[[1]][2]  # 年

# BI, eventTime----
tmp.eventtime <- tfile$`Basic Information`$Time_Of_Event  # 原始eventTime
tmp.time <- strsplit(tmp.eventtime, split = " ")[[1]][1]  # 时间，12h
tmp.timehour <- strsplit(strsplit(tmp.eventtime, split = " ")[[1]][1],
                         ":")[[1]][1]  # 时间，12h，时
tmp.timeminute <- strsplit(strsplit(tmp.eventtime, split = " ")[[1]][1],
                           ":")[[1]][2]  # 时间，12h，分
tmp.ampm <- ifelse(strsplit(tmp.eventtime, split = " ")[[1]][2] == "上午",
                   "A.M.", "P.M.")  # 时段
tmp.timezone <- strsplit(tmp.eventtime, split = " ")[[1]][3]  # 时区

# BI, eventTrigger----
tmp.eventtrigger <- tfile$`Basic Information`$Event_Trigger  # 原始eventTrigger

# BI, maxForwardTForce----
tmp.maxforwardtforce <- tfile$`Basic Information`$Max_Forward_T_Force

# BI, maxLateralTForce----
tmp.maxlateraltforce <- tfile$`Basic Information`$Max_Lateral_T_Force

# BI, triggerSpeed----
tmp.triggerspeedKMH <- strsplit(tfile$`Basic Information`$Speed,
                                split = " ")[[1]][1]

# BI, moveDirection----
tmp.moveDirection <- tfile$`Basic Information`$Heading

# BI, locationLatitude----
tmp.locationlatitude <- tfile$`Basic Information`$Loaction_N

# BI, locationLongitude----
tmp.locationlongitude <- tfile$`Basic Information`$Loaction_E

# BI, serialNumber----
tmp.serialnumber <- tfile$`Basic Information`$Serial_Number

# BI, firmwareVersion----
tmp.firmwareversion <- tfile$`Basic Information`$Firmware_Version

# BI, forwardThreshold----
tmp.forwardthreshold <- tfile$`Basic Information`$Forward_Threshold

# BI, lateralThreshold----
tmp.lateralthreshold <- tfile$`Basic Information`$Lateral_Threshold

# BI, shockThreshold----
tmp.shockthreshold <- tfile$`Basic Information`$Shock_Threshold


# Move Information 0----
tmp.fwdtime0 <- strsplit(tfile$`Move Information0`$FWD,
                         split = " ")[[1]][2]  # FWD, time -8.00
tmp.lattime0 <- strsplit(tfile$`Move Information0`$LAT,
                         split = " ")[[1]][2]  # LAT, time -8.00
tmp.movetime0 <- strsplit(tfile$`Move Information0`$TIME,
                          split = " ")[[1]][2]  # TIME, time -8.00
tmp.speedKMHtime0 <- strsplit(tfile$`Move Information0`$GPS,
                              split = " ")[[1]][1]  # GPS(Speed), time -8.00


# Move Information 1----
tmp.fwdtime1 <- strsplit(tfile$`Move Information1`$FWD,
                         split = " ")[[1]][2]  # FWD, time -8.00
tmp.lattime1 <- strsplit(tfile$`Move Information1`$LAT,
                         split = " ")[[1]][2]  # LAT, time -8.00
tmp.movetime1 <- strsplit(tfile$`Move Information1`$TIME,
                          split = " ")[[1]][2]  # TIME, time -8.00
tmp.speedKMHtime1 <- strsplit(tfile$`Move Information1`$GPS,
                              split = " ")[[1]][1]  # GPS(Speed), time -8.00


# Move Information 2----
tmp.fwdtime2 <- strsplit(tfile$`Move Information2`$FWD,
                         split = " ")[[1]][2]  # FWD, time -7.75
tmp.lattime2 <- strsplit(tfile$`Move Information2`$LAT,
                         split = " ")[[1]][2]  # LAT, time -7.75
tmp.movetime2 <- strsplit(tfile$`Move Information2`$TIME,
                          split = " ")[[1]][2]  # TIME, time -7.75
tmp.speedKMHtime2 <- strsplit(tfile$`Move Information2`$GPS,
                              split = " ")[[1]][1]  # GPS(Speed), time -7.75


# Move Information 3----
tmp.fwdtime3 <- strsplit(tfile$`Move Information3`$FWD,
                         split = " ")[[1]][2]  # FWD, time -7.50
tmp.lattime3 <- strsplit(tfile$`Move Information3`$LAT,
                         split = " ")[[1]][2]  # LAT, time -7.50
tmp.movetime3 <- strsplit(tfile$`Move Information3`$TIME,
                          split = " ")[[1]][2]  # TIME, time -7.50
tmp.speedKMHtime3 <- strsplit(tfile$`Move Information3`$GPS,
                              split = " ")[[1]][1]  # GPS(Speed), time -7.50


# Move Information 4----
tmp.fwdtime4 <- strsplit(tfile$`Move Information4`$FWD,
                         split = " ")[[1]][2]  # FWD, time -7.25
tmp.lattime4 <- strsplit(tfile$`Move Information4`$LAT,
                         split = " ")[[1]][2]  # LAT, time -7.25
tmp.movetime4 <- strsplit(tfile$`Move Information4`$TIME,
                          split = " ")[[1]][2]  # TIME, time -7.25
tmp.speedKMHtime4 <- strsplit(tfile$`Move Information4`$GPS,
                              split = " ")[[1]][1]  # GPS(Speed), time -7.25


# Move Information 5----
tmp.fwdtime5 <- strsplit(tfile$`Move Information5`$FWD,
                         split = " ")[[1]][2]  # FWD, time -7.00
tmp.lattime5 <- strsplit(tfile$`Move Information5`$LAT,
                         split = " ")[[1]][2]  # LAT, time -7.00
tmp.movetime5 <- strsplit(tfile$`Move Information5`$TIME,
                          split = " ")[[1]][2]  # TIME, time -7.00
tmp.speedKMHtime5 <- strsplit(tfile$`Move Information5`$GPS,
                              split = " ")[[1]][1]  # GPS(Speed), time -7.00


# Move Information 6----
tmp.fwdtime6 <- strsplit(tfile$`Move Information6`$FWD,
                         split = " ")[[1]][2]  # FWD, time -6.75
tmp.lattime6 <- strsplit(tfile$`Move Information6`$LAT,
                         split = " ")[[1]][2]  # LAT, time -6.75
tmp.movetime6 <- strsplit(tfile$`Move Information6`$TIME,
                          split = " ")[[1]][2]  # TIME, time -6.75
tmp.speedKMHtime6 <- strsplit(tfile$`Move Information6`$GPS,
                              split = " ")[[1]][1]  # GPS(Speed), time -6.75


# Move Information 7----
tmp.fwdtime7 <- strsplit(tfile$`Move Information7`$FWD,
                         split = " ")[[1]][2]  # FWD, time -6.50
tmp.lattime7 <- strsplit(tfile$`Move Information7`$LAT,
                         split = " ")[[1]][2]  # LAT, time -6.50
tmp.movetime7 <- strsplit(tfile$`Move Information7`$TIME,
                          split = " ")[[1]][2]  # TIME, time -6.50
tmp.speedKMHtime7 <- strsplit(tfile$`Move Information7`$GPS,
                              split = " ")[[1]][1]  # GPS(Speed), time -6.50


# Move Information 8----
tmp.fwdtime8 <- strsplit(tfile$`Move Information8`$FWD,
                         split = " ")[[1]][2]  # FWD, time -6.25
tmp.lattime8 <- strsplit(tfile$`Move Information8`$LAT,
                         split = " ")[[1]][2]  # LAT, time -6.25
tmp.movetime8 <- strsplit(tfile$`Move Information8`$TIME,
                          split = " ")[[1]][2]  # TIME, time -6.25
tmp.speedKMHtime8 <- strsplit(tfile$`Move Information8`$GPS,
                              split = " ")[[1]][1]  # GPS(Speed), time -6.25


# Move Information 9----
tmp.fwdtime9 <- strsplit(tfile$`Move Information9`$FWD,
                         split = " ")[[1]][2]  # FWD, time -6.00
tmp.lattime9 <- strsplit(tfile$`Move Information9`$LAT,
                         split = " ")[[1]][2]  # LAT, time -6.00
tmp.movetime9 <- strsplit(tfile$`Move Information9`$TIME,
                          split = " ")[[1]][2]  # TIME, time -6.00
tmp.speedKMHtime9 <- strsplit(tfile$`Move Information9`$GPS,
                              split = " ")[[1]][1]  # GPS(Speed), time -6.00


# Move Information 10----
tmp.fwdtime10 <- strsplit(tfile$`Move Information10`$FWD,
                         split = " ")[[1]][2]  # FWD, time -5.75
tmp.lattime10 <- strsplit(tfile$`Move Information10`$LAT,
                         split = " ")[[1]][2]  # LAT, time -5.75
tmp.movetime10 <- strsplit(tfile$`Move Information10`$TIME,
                          split = " ")[[1]][2]  # TIME, time -5.75
tmp.speedKMHtime10 <- strsplit(tfile$`Move Information10`$GPS,
                              split = " ")[[1]][1]  # GPS(Speed), time -5.75


# Move Information 11----
tmp.fwdtime11 <- strsplit(tfile$`Move Information11`$FWD,
                          split = " ")[[1]][2]  # FWD, time -5.50
tmp.lattime11 <- strsplit(tfile$`Move Information11`$LAT,
                          split = " ")[[1]][2]  # LAT, time -5.50
tmp.movetime11 <- strsplit(tfile$`Move Information11`$TIME,
                           split = " ")[[1]][2]  # TIME, time -5.50
tmp.speedKMHtime11 <- strsplit(tfile$`Move Information11`$GPS,
                               split = " ")[[1]][1]  # GPS(Speed), time -5.50


# Move Information 12----
tmp.fwdtime12 <- strsplit(tfile$`Move Information12`$FWD,
                          split = " ")[[1]][2]  # FWD, time -5.25
tmp.lattime12 <- strsplit(tfile$`Move Information12`$LAT,
                          split = " ")[[1]][2]  # LAT, time -5.25
tmp.movetime12 <- strsplit(tfile$`Move Information12`$TIME,
                           split = " ")[[1]][2]  # TIME, time -5.25
tmp.speedKMHtime12 <- strsplit(tfile$`Move Information12`$GPS,
                               split = " ")[[1]][1]  # GPS(Speed), time -5.25


# Move Information 13----
tmp.fwdtime13 <- strsplit(tfile$`Move Information13`$FWD,
                          split = " ")[[1]][2]  # FWD, time -5.00
tmp.lattime13 <- strsplit(tfile$`Move Information13`$LAT,
                          split = " ")[[1]][2]  # LAT, time -5.00
tmp.movetime13 <- strsplit(tfile$`Move Information13`$TIME,
                           split = " ")[[1]][2]  # TIME, time -5.00
tmp.speedKMHtime13 <- strsplit(tfile$`Move Information13`$GPS,
                               split = " ")[[1]][1]  # GPS(Speed), time -5.00


# Move Information 14----
tmp.fwdtime14 <- strsplit(tfile$`Move Information14`$FWD,
                          split = " ")[[1]][2]  # FWD, time -4.75
tmp.lattime14 <- strsplit(tfile$`Move Information14`$LAT,
                          split = " ")[[1]][2]  # LAT, time -4.75
tmp.movetime14 <- strsplit(tfile$`Move Information14`$TIME,
                           split = " ")[[1]][2]  # TIME, time -4.75
tmp.speedKMHtime14 <- strsplit(tfile$`Move Information14`$GPS,
                               split = " ")[[1]][1]  # GPS(Speed), time -4.75


# Move Information 15----
tmp.fwdtime15 <- strsplit(tfile$`Move Information15`$FWD,
                          split = " ")[[1]][2]  # FWD, time -4.50
tmp.lattime15 <- strsplit(tfile$`Move Information15`$LAT,
                          split = " ")[[1]][2]  # LAT, time -4.50
tmp.movetime15 <- strsplit(tfile$`Move Information15`$TIME,
                           split = " ")[[1]][2]  # TIME, time -4.50
tmp.speedKMHtime15 <- strsplit(tfile$`Move Information15`$GPS,
                               split = " ")[[1]][1]  # GPS(Speed), time -4.50


# Move Information 16----
tmp.fwdtime16 <- strsplit(tfile$`Move Information16`$FWD,
                          split = " ")[[1]][2]  # FWD, time -4.25
tmp.lattime16 <- strsplit(tfile$`Move Information16`$LAT,
                          split = " ")[[1]][2]  # LAT, time -4.25
tmp.movetime16 <- strsplit(tfile$`Move Information16`$TIME,
                           split = " ")[[1]][2]  # TIME, time -4.25
tmp.speedKMHtime16 <- strsplit(tfile$`Move Information16`$GPS,
                               split = " ")[[1]][1]  # GPS(Speed), time -4.25


# Move Information 17----
tmp.fwdtime17 <- strsplit(tfile$`Move Information17`$FWD,
                          split = " ")[[1]][2]  # FWD, time -4.00
tmp.lattime17 <- strsplit(tfile$`Move Information17`$LAT,
                          split = " ")[[1]][2]  # LAT, time -4.00
tmp.movetime17 <- strsplit(tfile$`Move Information17`$TIME,
                           split = " ")[[1]][2]  # TIME, time -4.00
tmp.speedKMHtime17 <- strsplit(tfile$`Move Information17`$GPS,
                               split = " ")[[1]][1]  # GPS(Speed), time -4.00


# Move Information 18----
tmp.fwdtime18 <- strsplit(tfile$`Move Information18`$FWD,
                          split = " ")[[1]][2]  # FWD, time -3.75
tmp.lattime18 <- strsplit(tfile$`Move Information18`$LAT,
                          split = " ")[[1]][2]  # LAT, time -3.75
tmp.movetime18 <- strsplit(tfile$`Move Information18`$TIME,
                           split = " ")[[1]][2]  # TIME, time -3.75
tmp.speedKMHtime18 <- strsplit(tfile$`Move Information18`$GPS,
                               split = " ")[[1]][1]  # GPS(Speed), time -3.75


# Move Information 19----
tmp.fwdtime19 <- strsplit(tfile$`Move Information19`$FWD,
                          split = " ")[[1]][2]  # FWD, time -3.50
tmp.lattime19 <- strsplit(tfile$`Move Information19`$LAT,
                          split = " ")[[1]][2]  # LAT, time -3.50
tmp.movetime19 <- strsplit(tfile$`Move Information19`$TIME,
                           split = " ")[[1]][2]  # TIME, time -3.50
tmp.speedKMHtime19 <- strsplit(tfile$`Move Information19`$GPS,
                               split = " ")[[1]][1]  # GPS(Speed), time -3.50


# Move Information 20----
tmp.fwdtime20 <- strsplit(tfile$`Move Information20`$FWD,
                          split = " ")[[1]][2]  # FWD, time -3.25
tmp.lattime20 <- strsplit(tfile$`Move Information20`$LAT,
                          split = " ")[[1]][2]  # LAT, time -3.25
tmp.movetime20 <- strsplit(tfile$`Move Information20`$TIME,
                           split = " ")[[1]][2]  # TIME, time -3.25
tmp.speedKMHtime20 <- strsplit(tfile$`Move Information20`$GPS,
                               split = " ")[[1]][1]  # GPS(Speed), time -3.25


# Move Information 21----
tmp.fwdtime21 <- strsplit(tfile$`Move Information21`$FWD,
                          split = " ")[[1]][2]  # FWD, time -3.00
tmp.lattime21 <- strsplit(tfile$`Move Information21`$LAT,
                          split = " ")[[1]][2]  # LAT, time -3.00
tmp.movetime21 <- strsplit(tfile$`Move Information21`$TIME,
                           split = " ")[[1]][2]  # TIME, time -3.00
tmp.speedKMHtime21 <- strsplit(tfile$`Move Information21`$GPS,
                               split = " ")[[1]][1]  # GPS(Speed), time -3.00


# Move Information 22----
tmp.fwdtime22 <- strsplit(tfile$`Move Information22`$FWD,
                          split = " ")[[1]][2]  # FWD, time -2.75
tmp.lattime22 <- strsplit(tfile$`Move Information22`$LAT,
                          split = " ")[[1]][2]  # LAT, time -2.75
tmp.movetime22 <- strsplit(tfile$`Move Information22`$TIME,
                           split = " ")[[1]][2]  # TIME, time -2.75
tmp.speedKMHtime22 <- strsplit(tfile$`Move Information22`$GPS,
                               split = " ")[[1]][1]  # GPS(Speed), time -2.75


# Move Information 23----
tmp.fwdtime23 <- strsplit(tfile$`Move Information23`$FWD,
                          split = " ")[[1]][2]  # FWD, time -2.50
tmp.lattime23 <- strsplit(tfile$`Move Information23`$LAT,
                          split = " ")[[1]][2]  # LAT, time -2.50
tmp.movetime23 <- strsplit(tfile$`Move Information23`$TIME,
                           split = " ")[[1]][2]  # TIME, time -2.50
tmp.speedKMHtime23 <- strsplit(tfile$`Move Information23`$GPS,
                               split = " ")[[1]][1]  # GPS(Speed), time -2.50


# Move Information 24----
tmp.fwdtime24 <- strsplit(tfile$`Move Information24`$FWD,
                          split = " ")[[1]][2]  # FWD, time -2.25
tmp.lattime24 <- strsplit(tfile$`Move Information24`$LAT,
                          split = " ")[[1]][2]  # LAT, time -2.25
tmp.movetime24 <- strsplit(tfile$`Move Information24`$TIME,
                           split = " ")[[1]][2]  # TIME, time -2.25
tmp.speedKMHtime24 <- strsplit(tfile$`Move Information24`$GPS,
                               split = " ")[[1]][1]  # GPS(Speed), time -2.25


# Move Information 25----
tmp.fwdtime25 <- strsplit(tfile$`Move Information25`$FWD,
                          split = " ")[[1]][2]  # FWD, time -2.00
tmp.lattime25 <- strsplit(tfile$`Move Information25`$LAT,
                          split = " ")[[1]][2]  # LAT, time -2.00
tmp.movetime25 <- strsplit(tfile$`Move Information25`$TIME,
                           split = " ")[[1]][2]  # TIME, time -2.00
tmp.speedKMHtime25 <- strsplit(tfile$`Move Information25`$GPS,
                               split = " ")[[1]][1]  # GPS(Speed), time -2.00


# Move Information 26----
tmp.fwdtime26 <- strsplit(tfile$`Move Information26`$FWD,
                          split = " ")[[1]][2]  # FWD, time -1.75
tmp.lattime26 <- strsplit(tfile$`Move Information26`$LAT,
                          split = " ")[[1]][2]  # LAT, time -1.75
tmp.movetime26 <- strsplit(tfile$`Move Information26`$TIME,
                           split = " ")[[1]][2]  # TIME, time -1.75
tmp.speedKMHtime26 <- strsplit(tfile$`Move Information26`$GPS,
                               split = " ")[[1]][1]  # GPS(Speed), time -1.75


# Move Information 27----
tmp.fwdtime27 <- strsplit(tfile$`Move Information27`$FWD,
                          split = " ")[[1]][2]  # FWD, time -1.50
tmp.lattime27 <- strsplit(tfile$`Move Information27`$LAT,
                          split = " ")[[1]][2]  # LAT, time -1.50
tmp.movetime27 <- strsplit(tfile$`Move Information27`$TIME,
                           split = " ")[[1]][2]  # TIME, time -1.50
tmp.speedKMHtime27 <- strsplit(tfile$`Move Information27`$GPS,
                               split = " ")[[1]][1]  # GPS(Speed), time -1.50


# Move Information 28----
tmp.fwdtime28 <- strsplit(tfile$`Move Information28`$FWD,
                          split = " ")[[1]][2]  # FWD, time -1.25
tmp.lattime28 <- strsplit(tfile$`Move Information28`$LAT,
                          split = " ")[[1]][2]  # LAT, time -1.25
tmp.movetime28 <- strsplit(tfile$`Move Information28`$TIME,
                           split = " ")[[1]][2]  # TIME, time -1.25
tmp.speedKMHtime28 <- strsplit(tfile$`Move Information28`$GPS,
                               split = " ")[[1]][1]  # GPS(Speed), time -1.25


# Move Information 29----
tmp.fwdtime29 <- strsplit(tfile$`Move Information29`$FWD,
                          split = " ")[[1]][2]  # FWD, time -1.00
tmp.lattime29 <- strsplit(tfile$`Move Information29`$LAT,
                          split = " ")[[1]][2]  # LAT, time -1.00
tmp.movetime29 <- strsplit(tfile$`Move Information29`$TIME,
                           split = " ")[[1]][2]  # TIME, time -1.00
tmp.speedKMHtime29 <- strsplit(tfile$`Move Information29`$GPS,
                               split = " ")[[1]][1]  # GPS(Speed), time -1.00


# Move Information 30----
tmp.fwdtime30 <- strsplit(tfile$`Move Information30`$FWD,
                          split = " ")[[1]][2]  # FWD, time -0.75
tmp.lattime30 <- strsplit(tfile$`Move Information30`$LAT,
                          split = " ")[[1]][2]  # LAT, time -0.75
tmp.movetime30 <- strsplit(tfile$`Move Information30`$TIME,
                           split = " ")[[1]][2]  # TIME, time -0.75
tmp.speedKMHtime30 <- strsplit(tfile$`Move Information30`$GPS,
                               split = " ")[[1]][1]  # GPS(Speed), time -0.75


# Move Information 31----
tmp.fwdtime31 <- strsplit(tfile$`Move Information31`$FWD,
                          split = " ")[[1]][2]  # FWD, time -0.50
tmp.lattime31 <- strsplit(tfile$`Move Information31`$LAT,
                          split = " ")[[1]][2]  # LAT, time -0.50
tmp.movetime31 <- strsplit(tfile$`Move Information31`$TIME,
                           split = " ")[[1]][2]  # TIME, time -0.50
tmp.speedKMHtime31 <- strsplit(tfile$`Move Information31`$GPS,
                               split = " ")[[1]][1]  # GPS(Speed), time -0.50


# Move Information 32----
tmp.fwdtime32 <- strsplit(tfile$`Move Information32`$FWD,
                          split = " ")[[1]][2]  # FWD, time -0.25
tmp.lattime32 <- strsplit(tfile$`Move Information32`$LAT,
                          split = " ")[[1]][2]  # LAT, time -0.25
tmp.movetime32 <- strsplit(tfile$`Move Information32`$TIME,
                           split = " ")[[1]][2]  # TIME, time -0.25
tmp.speedKMHtime32 <- strsplit(tfile$`Move Information32`$GPS,
                               split = " ")[[1]][1]  # GPS(Speed), time -0.25


# Move Information 33----
tmp.fwdtime33 <- strsplit(tfile$`Move Information33`$FWD,
                          split = " ")[[1]][2]  # FWD, time +0.00
tmp.lattime33 <- strsplit(tfile$`Move Information33`$LAT,
                          split = " ")[[1]][2]  # LAT, time +0.00
tmp.movetime33 <- strsplit(tfile$`Move Information33`$TIME,
                           split = " ")[[1]][2]  # TIME, time +0.00
tmp.speedKMHtime33 <- strsplit(tfile$`Move Information33`$GPS,
                               split = " ")[[1]][1]  # GPS(Speed), time +0.00


# Move Information 34----
tmp.fwdtime34 <- strsplit(tfile$`Move Information34`$FWD,
                          split = " ")[[1]][2]  # FWD, time +0.25
tmp.lattime34 <- strsplit(tfile$`Move Information34`$LAT,
                          split = " ")[[1]][2]  # LAT, time +0.25
tmp.movetime34 <- strsplit(tfile$`Move Information34`$TIME,
                           split = " ")[[1]][2]  # TIME, time +0.25
tmp.speedKMHtime34 <- strsplit(tfile$`Move Information34`$GPS,
                               split = " ")[[1]][1]  # GPS(Speed), time +0.25


# Move Information 35----
tmp.fwdtime35 <- strsplit(tfile$`Move Information35`$FWD,
                          split = " ")[[1]][2]  # FWD, time +0.50
tmp.lattime35 <- strsplit(tfile$`Move Information35`$LAT,
                          split = " ")[[1]][2]  # LAT, time +0.50
tmp.movetime35 <- strsplit(tfile$`Move Information35`$TIME,
                           split = " ")[[1]][2]  # TIME, time +0.50
tmp.speedKMHtime35 <- strsplit(tfile$`Move Information35`$GPS,
                               split = " ")[[1]][1]  # GPS(Speed), time +0.50


# Move Information 36----
tmp.fwdtime36 <- strsplit(tfile$`Move Information36`$FWD,
                          split = " ")[[1]][2]  # FWD, time +0.75
tmp.lattime36 <- strsplit(tfile$`Move Information36`$LAT,
                          split = " ")[[1]][2]  # LAT, time +0.75
tmp.movetime36 <- strsplit(tfile$`Move Information36`$TIME,
                           split = " ")[[1]][2]  # TIME, time +0.75
tmp.speedKMHtime36 <- strsplit(tfile$`Move Information36`$GPS,
                               split = " ")[[1]][1]  # GPS(Speed), time +0.75


# Move Information 37----
tmp.fwdtime37 <- strsplit(tfile$`Move Information37`$FWD,
                          split = " ")[[1]][2]  # FWD, time +1.00
tmp.lattime37 <- strsplit(tfile$`Move Information37`$LAT,
                          split = " ")[[1]][2]  # LAT, time +1.00
tmp.movetime37 <- strsplit(tfile$`Move Information37`$TIME,
                           split = " ")[[1]][2]  # TIME, time +1.00
tmp.speedKMHtime37 <- strsplit(tfile$`Move Information37`$GPS,
                               split = " ")[[1]][1]  # GPS(Speed), time +1.00


# Move Information 38----
tmp.fwdtime38 <- strsplit(tfile$`Move Information38`$FWD,
                          split = " ")[[1]][2]  # FWD, time +1.25
tmp.lattime38 <- strsplit(tfile$`Move Information38`$LAT,
                          split = " ")[[1]][2]  # LAT, time +1.25
tmp.movetime38 <- strsplit(tfile$`Move Information38`$TIME,
                           split = " ")[[1]][2]  # TIME, time +1.25
tmp.speedKMHtime38 <- strsplit(tfile$`Move Information38`$GPS,
                               split = " ")[[1]][1]  # GPS(Speed), time +1.25


# Move Information 39----
tmp.fwdtime39 <- strsplit(tfile$`Move Information39`$FWD,
                          split = " ")[[1]][2]  # FWD, time +1.50
tmp.lattime39 <- strsplit(tfile$`Move Information39`$LAT,
                          split = " ")[[1]][2]  # LAT, time +1.50
tmp.movetime39 <- strsplit(tfile$`Move Information39`$TIME,
                           split = " ")[[1]][2]  # TIME, time +1.50
tmp.speedKMHtime39 <- strsplit(tfile$`Move Information39`$GPS,
                               split = " ")[[1]][1]  # GPS(Speed), time +1.50


# Move Information 40----
tmp.fwdtime40 <- strsplit(tfile$`Move Information40`$FWD,
                          split = " ")[[1]][2]  # FWD, time +1.75
tmp.lattime40 <- strsplit(tfile$`Move Information40`$LAT,
                          split = " ")[[1]][2]  # LAT, time +1.75
tmp.movetime40 <- strsplit(tfile$`Move Information40`$TIME,
                           split = " ")[[1]][2]  # TIME, time +1.75
tmp.speedKMHtime40 <- strsplit(tfile$`Move Information40`$GPS,
                               split = " ")[[1]][1]  # GPS(Speed), time +1.75


# Move Information 41----
tmp.fwdtime41 <- strsplit(tfile$`Move Information41`$FWD,
                          split = " ")[[1]][2]  # FWD, time +2.00
tmp.lattime41 <- strsplit(tfile$`Move Information41`$LAT,
                          split = " ")[[1]][2]  # LAT, time +2.00
tmp.movetime41 <- strsplit(tfile$`Move Information41`$TIME,
                           split = " ")[[1]][2]  # TIME, time +2.00
tmp.speedKMHtime41 <- strsplit(tfile$`Move Information41`$GPS,
                               split = " ")[[1]][1]  # GPS(Speed), time +2.00


# Move Information 42----
tmp.fwdtime42 <- strsplit(tfile$`Move Information42`$FWD,
                          split = " ")[[1]][2]  # FWD, time +2.25
tmp.lattime42 <- strsplit(tfile$`Move Information42`$LAT,
                          split = " ")[[1]][2]  # LAT, time +2.25
tmp.movetime42 <- strsplit(tfile$`Move Information42`$TIME,
                           split = " ")[[1]][2]  # TIME, time +2.25
tmp.speedKMHtime42 <- strsplit(tfile$`Move Information42`$GPS,
                               split = " ")[[1]][1]  # GPS(Speed), time +2.25


# Move Information 43----
tmp.fwdtime43 <- strsplit(tfile$`Move Information43`$FWD,
                          split = " ")[[1]][2]  # FWD, time +2.50
tmp.lattime43 <- strsplit(tfile$`Move Information43`$LAT,
                          split = " ")[[1]][2]  # LAT, time +2.50
tmp.movetime43 <- strsplit(tfile$`Move Information43`$TIME,
                           split = " ")[[1]][2]  # TIME, time +2.50
tmp.speedKMHtime43 <- strsplit(tfile$`Move Information43`$GPS,
                               split = " ")[[1]][1]  # GPS(Speed), time +2.50


# Move Information 44----
tmp.fwdtime44 <- strsplit(tfile$`Move Information44`$FWD,
                          split = " ")[[1]][2]  # FWD, time +2.75
tmp.lattime44 <- strsplit(tfile$`Move Information44`$LAT,
                          split = " ")[[1]][2]  # LAT, time +2.75
tmp.movetime44 <- strsplit(tfile$`Move Information44`$TIME,
                           split = " ")[[1]][2]  # TIME, time +2.75
tmp.speedKMHtime44 <- strsplit(tfile$`Move Information44`$GPS,
                               split = " ")[[1]][1]  # GPS(Speed), time +2.75


# Move Information 45----
tmp.fwdtime45 <- strsplit(tfile$`Move Information45`$FWD,
                          split = " ")[[1]][2]  # FWD, time +3.00
tmp.lattime45 <- strsplit(tfile$`Move Information45`$LAT,
                          split = " ")[[1]][2]  # LAT, time +3.00
tmp.movetime45 <- strsplit(tfile$`Move Information45`$TIME,
                           split = " ")[[1]][2]  # TIME, time +3.00
tmp.speedKMHtime45 <- strsplit(tfile$`Move Information45`$GPS,
                               split = " ")[[1]][1]  # GPS(Speed), time +3.00


# Move Information 46----
tmp.fwdtime46 <- strsplit(tfile$`Move Information46`$FWD,
                          split = " ")[[1]][2]  # FWD, time +3.25
tmp.lattime46 <- strsplit(tfile$`Move Information46`$LAT,
                          split = " ")[[1]][2]  # LAT, time +3.25
tmp.movetime46 <- strsplit(tfile$`Move Information46`$TIME,
                           split = " ")[[1]][2]  # TIME, time +3.25
tmp.speedKMHtime46 <- strsplit(tfile$`Move Information46`$GPS,
                               split = " ")[[1]][1]  # GPS(Speed), time +3.25


# Move Information 47----
tmp.fwdtime47 <- strsplit(tfile$`Move Information47`$FWD,
                          split = " ")[[1]][2]  # FWD, time +3.50
tmp.lattime47 <- strsplit(tfile$`Move Information47`$LAT,
                          split = " ")[[1]][2]  # LAT, time +3.50
tmp.movetime47 <- strsplit(tfile$`Move Information47`$TIME,
                           split = " ")[[1]][2]  # TIME, time +3.50
tmp.speedKMHtime47 <- strsplit(tfile$`Move Information47`$GPS,
                               split = " ")[[1]][1]  # GPS(Speed), time +3.50























