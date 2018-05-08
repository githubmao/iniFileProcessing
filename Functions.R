#-------------- Code Description ----------------------------------------------#
# Description: 导入.ini数据，并将其整理成需要的数据格式，需要的函数。
#
# Notes:
# 
#
#------------------------------------------------------------------------------#


# 1.计算事件发生的时间，星期几----
GetDayoftheWeek <- function(kInputDay) {
  # Input:
  #      kInputDay: 原始数据星期几。
  # Output:输出数据星期几。
  
  if (kInputDay == "一") {         # 星期一
    return("Mon.")
  } else if (kInputDay == "二") {  # 星期二
    return("Tues.")
  } else if (kInputDay == "三") {  # 星期三
    return("Wed.")
  } else if (kInputDay == "四") {  # 星期四
    return("Thur.")
  } else if (kInputDay == "五") {  # 星期五
    return("Fri.")
  } else if (kInputDay == "六") {  # 星期六
    return("Sat.")
  } else if (kInputDay == "日") {  #星期日
    return("Sun.")
  } else {
    return("DataError")  # 其他情况
  }
}


# 2. 计算事件发生的时间，月份----
GetMonth <- function(kInputMonth) {
  # Input:
  #      kInputMonth：原始数据月份。
  # Output:输出数据月份。
  
  if (kInputMonth == "一月") {  # 一月
    return("Jan.")
  } else if (kInputMonth == "二月") {  # 二月
    return("Feb.")
  } else if (kInputMonth == "三月") {  # 三月
    return("Mar.")
  } else if (kInputMonth == "四月") {  #  四月
    return("Apr.")
  } else if (kInputMonth == "五月") {  # 五月
    return("May.")
  } else if (kInputMonth == "六月") {  # 六月
    return("Jun.")
  } else if (kInputMonth == "七月") {  # 七月
    return("Jul.")
  } else if (kInputMonth == "八月") {  # 八月
    return("Aug.")
  } else if (kInputMonth == "九月") {  # 九月
    return("Sep.")
  } else if (kInputMonth == "十月") {  # 十月
    return("Oct.")
  } else if (kInputMonth == "十一月") {  # 十一月
    return("Nov.")
  } else if (kInputMonth == "十二月") {  # 十二月
    return("Dec.")
  } else {
    return("DataError")  # 其他情况
  }
}


# 3.获取ini文件中的Basic Information----
GetBasicInformation <- function(tFile) {
  # Input: 读取的ini文件。
  # Output: 含Basic Information各项信息的数据框。
  
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
  tmp.eventtrigger <- tfile$`Basic Information`$Event_Trigger
  
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
  
  return(data.frame(dDayoftheWeek = c(tmp.dayoftheweek),
                    dMonth = c(tmp.month),
                    dDate = c(tmp.date),
                    dYear = c(tmp.year),
                    timeHour = c(tmp.timehour),
                    timeMinute = c(tmp.timeminute),
                    timeAMPM = c(tmp.ampm),
                    timeTimeZone = c(tmp.timezone),
                    evetTrigger = c(tmp.eventtrigger),
                    maxForwardTForce = c(tmp.maxforwardtforce),
                    maxLateralTForce = c(tmp.maxlateraltforce),
                    triggerSpeedKMH = c(tmp.triggerspeedKMH),
                    moveDirection = c(tmp.moveDirection),
                    locationLatitude = c(tmp.locationlatitude),
                    locationLongitude = c(tmp.locationlongitude),
                    serialNumber = c(tmp.serialnumber),
                    firmwareVersion = c(tmp.firmwareversion),
                    forwardThreshold = c(tmp.forwardthreshold),
                    lateralThreshold = c(tmp.lateralthreshold),
                    shockThreshold = c(tmp.shockthreshold)))
}


# 4. 获取ini文件中的Move Information----
GetMoveInformation <- function(tfile,
                               kTag = "kTag",
                               kTime = "Time1") {
  # Input: 
  #  tfile: 读取的ini文件。
  #  kTag: 读取的Tag标签。
  #  kTime: 读取的时间标签
  #
  # Output: 含Move Information各项信息的数据框。
  
  if (length(which(names(tfile) == kTag)) != 0) {
    
    kTagNumber <- which(names(tfile) == kTag)
    tmp.fwd <- strsplit(tfile[[kTagNumber]]$FWD, split = " ")[[1]][2]
    tmp.lat <- strsplit(tfile[[kTagNumber]]$LAT, split = " ")[[1]][2]
    tmp.movetime <- strsplit(tfile[[kTagNumber]]$TIME, split = " ")[[1]][2]
    tmp.speed <- strsplit(tfile[[kTagNumber]]$GPS, split = " ")[[1]][1]
    
    kColName <- c(paste("fwd", kTime, sep = ""),
                  paste("lat", kTime, sep = ""),
                  paste("move", kTime, sep = ""),
                  paste("speed", kTime, sep = ""))
    
    tmp.df <- data.frame(c(tmp.fwd),
                         c(tmp.lat),
                         c(tmp.movetime),
                         c(tmp.speed))
    
    names(tmp.df) <- kColName
    return(tmp.df)
    
  } else {
    return("False kTag")
  }
}

df <- data.frame()
df <- rbind(df, aa)

tt <- data.frame(dDayoftheWeek = tmp.dayoftheweek,
                 dMonth = tmp.month,
                 dDate = tmp.date,
                 dYear = tmp.year,
                 timeHour = tmp.timehour,
                 timeMinute = tmp.timeminute,
                 timeAMPM = tmp.ampm,
                 timeTimeZone = tmp.timezone,
                 evetTrigger = tmp.eventtrigger,
                 maxForwardTForce = tmp.maxforwardtforce,
                 maxLateralTForce = tmp.maxlateraltforce,
                 triggerSpeedKMH = tmp.triggerspeedKMH,
                 moveDirection = tmp.moveDirection,
                 locationLatitude = tmp.locationlatitude,
                 locationLongitude = tmp.locationlongitude,
                 serialNumber = tmp.serialnumber,
                 firmwareVersion = tmp.firmwareversion,
                 forwardThreshold = tmp.forwardthreshold,
                 lateralThreshold = tmp.lateralthreshold,
                 shockThreshold = tmp.shockthreshold)



tmp.locationlongitude


data.frame(tDayoftheWeek = tmp.dayoftheweek,
           tMonth = c(tmp.month))
