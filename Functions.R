#-------------- Code Description ----------------------------------------------#
# Description: 导入.ini数据，并将其整理成需要的数据格式，需要的函数。
# Notes: ver1.0, 20180510, By MaoYan;
#
# Notes:
#   GetDayoftheWeek: 获取事件发生的时间，星期几。
#   GetMonth: 获取事件发生的时间，月份。
#   GetBasicInformation: 获取ini数据中Basic Information标签下数据。
#   GetMoveInfo: 获取ini数据中单个Move Information标签下数据。
#   GetBatchMoveInfo：获取ini数据中多个指定Time标签的Move Information标签下数据.
#
#------------------------------------------------------------------------------#


# 1.计算事件发生的时间，星期几----
GetDayoftheWeek <- function(kInputDay) {
  # 输入: kInputDay: 原始数据星期几。
  # 输出: 输出数据星期几。
  
  if (kInputDay == "一") {         # 星期一
    return("Mon")
  } else if (kInputDay == "二") {  # 星期二
    return("Tues")
  } else if (kInputDay == "三") {  # 星期三
    return("Wed")
  } else if (kInputDay == "四") {  # 星期四
    return("Thur")
  } else if (kInputDay == "五") {  # 星期五
    return("Fri")
  } else if (kInputDay == "六") {  # 星期六
    return("Sat")
  } else if (kInputDay == "日") {  #星期日
    return("Sun")
  } else {
    stop("Error 'day of the week'!") # 其他情况
  }
}


# 2. 计算事件发生的时间，月份----
GetMonth <- function(kInputMonth) {
  # 输入: kInputMonth：原始数据月份。
  # 输出: 输出数据月份。
  
  if (kInputMonth == "一月") {  # 一月
    return("Jan")
  } else if (kInputMonth == "二月") {  # 二月
    return("Feb")
  } else if (kInputMonth == "三月") {  # 三月
    return("Mar")
  } else if (kInputMonth == "四月") {  #  四月
    return("Apr")
  } else if (kInputMonth == "五月") {  # 五月
    return("May")
  } else if (kInputMonth == "六月") {  # 六月
    return("Jun")
  } else if (kInputMonth == "七月") {  # 七月
    return("Jul")
  } else if (kInputMonth == "八月") {  # 八月
    return("Aug")
  } else if (kInputMonth == "九月") {  # 九月
    return("Sep")
  } else if (kInputMonth == "十月") {  # 十月
    return("Oct")
  } else if (kInputMonth == "十一月") {  # 十一月
    return("Nov")
  } else if (kInputMonth == "十二月") {  # 十二月
    return("Dec")
  } else {
    stop("Error 'Month'!")  # 其他情况
  }
}


# 3.获取ini文件中的Basic Information----
GetBasicInformation <- function(tFile) {
  # 输入: 读取的ini文件。
  # 输出: 含Basic Information各项信息的数据框。
  
  list.BI <- tFile$`Basic Information`  # BasicInformation
  
  # BI, Record_Date----
  # 1.Record_Date不为NULL
  if (!is.null(list.BI$Record_Date)) {
    
    tmp.recdate <- strsplit(x = list.BI$Record_Date,
                            split = ",")[[1]]
    
    # 1.1 Record_Date不为NULL, Record_Date正常取值
    if (tmp.recdate[1] %in% c("一", "二", "三", "四", "五", "六", "日")) {
      tmp.dayoftheweek <- GetDayoftheWeek(tmp.recdate[1])  # 星期几
      tmp.dmonth <- strsplit(x = tmp.recdate[2], split = " ")[[1]][2] %>%
        GetMonth()  # 月份
      tmp.ddate <- strsplit(x = tmp.recdate[2], split = " ")[[1]][3]  # 日
      tmp.dyear <- strsplit(x = tmp.recdate[3], split = " ")[[1]][2]  # 年
      
    # 1.2 Record_Date不为NULL, Record_Date非正常值
    } else {
      tmp.dayoftheweek <- NA
      tmp.dmonth <- NA
      tmp.ddate <- NA
      tmp.dyear <- NA
    }
    
  # 2.Record_Date为NULL
  } else {
    tmp.dayoftheweek <- NA
    tmp.dmonth <- NA
    tmp.ddate <- NA
    tmp.dyear <- NA
  }
  
  
  # BI, Time_Of_Event----
  # 1. Time_Of_Event不为NULL
  if (!is.null(list.BI$Time_Of_Event)) {
    
    tmp.evnttime <- strsplit(x = list.BI$Time_Of_Event,
                             split = " ")[[1]]
    
    # 1.1 Time_Of_Event不为NULL, Time_Of_Event正常取值
    if (length(tmp.evnttime == 3)) {
      tmp.timehr <- strsplit(x = tmp.evnttime[1], split = ":")[[1]][1]  # 时
      tmp.timemin <- strsplit(x = tmp.evnttime[1], split = ":")[[1]][2]  # 分
      tmp.ampm <- ifelse(tmp.evnttime[2] == "上午", "am", "pm")  # 上午or下午
      tmp.timezone <- tmp.evnttime[3]  # 时区
      
    # 1.2 Time_Of_Event不为NULL，Time_Of_Event非正常值
    } else {
      tmp.timehr <- NA
      tmp.timemin <- NA
      tmp.ampm <- NA
      tmp.timezone <- NA
    }
    
  # 2. Time_Of_Event为NULL
  } else {
    tmp.timehr <- NA
    tmp.timemin <- NA
    tmp.ampm <- NA
    tmp.timezone <- NA
  }
  
  
  # BI, Event_Trigger----
  tmp.evnttrigger <- ifelse(is.null(list.BI$Event_Trigger), NA,
                            list.BI$Event_Trigger)
  
  
  # BI, Max_Forward_T_Force----
  # 1. Max_Forward_T_Force不为NULL
  if (!is.null(list.BI$Max_Forward_T_Force)) {
    
    tmp.maxfwdforceS <- strsplit(x = list.BI$Max_Forward_T_Force,
                                 split = " ")[[1]]
    
    # 1.1 Max_Forward_T_Force不为NULL，Max_Forward_T_Force正常取值
    if (length(tmp.maxfwdforceS) == 1) {
      tmp.maxfwdforce <- list.BI$Max_Forward_T_Force
      
    # 1.2 Max_Forward_T_Force不为NULL，Max_Forward_T_Force非正常值
    } else {
      tmp.maxfwdforce <- NA
    }
    
  # 2. Max_Forward_T_Force为NULL
  } else {
    tmp.maxfwdforce <- NA
  }
  
  
  # BI, Max_Lateral_T_Force----
  # 1. Max_Lateral_T_Force不为NULL
  if (!is.null(list.BI$Max_Lateral_T_Force)) {
    
    tmp.maxlatforceS <- strsplit(x = list.BI$Max_Lateral_T_Force,
                                 split = " ")[[1]]
    
    # 1.1 Max_Lateral_T_Force不为NULL，Max_Lateral_T_Force正常取值
    if (length(tmp.maxlatforceS) == 1) {
      tmp.maxlatforce <- list.BI$Max_Lateral_T_Force
      
    # 1.2 Max_Lateral_T_Force不为NULL，Max_Lateral_T_Force非正常值
    } else {
      tmp.maxlatforce <- NA
    }
    
  # 1. Max_Lateral_T_Force为NULL
  } else {
    tmp.maxlatforce <- NA
  }
  
  
  # BI, Speed----
  # 1. Speed不为NULL
  if (!is.null(list.BI$Speed)) {
    
    tmp.speedGPS <- strsplit(x = list.BI$Speed, split = " ")[[1]]
    
    # 1.1 Speed不为NULL，Speed正常取值
    if (list.BI$Speed != "Data not available") {
      tmp.triggerspeed <- tmp.speedGPS[1]
      
    # 1.2 Speed不为NULL，Speed非正常值
    } else {
      tmp.triggerspeed <- NA
    }
    
  # 2. Speed为NULL
  } else {
    tmp.triggerspeed <- NA
  }
  
  
  # BI, Heading----
  # 1. Heading不为NULL
  if (!is.null(list.BI$Heading)) {
    
    # 1.1 Heading不为NULL，Heading正常取值
    if (list.BI$Heading != "Data not available") {
      tmp.movedir <- list.BI$Heading
      
    # 1.2 Heading不为NULL，Heading非正常值
    } else {
      tmp.movedir <- NA
    }
  # 2. Heading为NULL
  } else {
    tmp.movedir <- NA
  }
  
  
  # BI, Loaction_N----
  # 1. Loaction_N不为NULL
  if (!is.null(list.BI$Loaction_N)) {
    
    # 1.1 Loaction_N不为NULL，Loaction_N正常取值
    if (list.BI$Loaction_N != "Data not available") {
      tmp.loclat <- list.BI$Loaction_N
      
      # 1.2 Loaction_N不为NULL，Loaction_N非正常值
    } else {
      tmp.loclat <- NA
    }
    # 2. Loaction_N为NULL
  } else {
    tmp.loclat <- NA
  }

  
  # BI, Loaction_E----
  # 1. Loaction_E不为NULL
  if (!is.null(list.BI$Loaction_E)) {
    
    # 1.1 Loaction_E不为NULL，Loaction_E正常取值
    if (list.BI$Loaction_E != "Data not available") {
      tmp.loclong <- list.BI$Loaction_E
      
      # 1.2Loaction_E不为NULL，Loaction_E非正常值
    } else {
      tmp.loclong <- NA
    }
    # 2. Loaction_E为NULL
  } else {
    tmp.loclong <- NA
  }
  
  
  # BI, Serial_Number----
  tmp.serialnum <- ifelse(is.null(list.BI$Serial_Number), NA,
                          list.BI$Serial_Number)
  
  
  # BI, Firmware_Version----
  tmp.firmwarever <- ifelse(is.null(list.BI$Firmware_Version), NA,
                            list.BI$Firmware_Version)
  
  
  # BI, Forward_Threshold----
  tmp.fwdthold <- ifelse(is.null(list.BI$Forward_Threshold), NA,
                         list.BI$Forward_Threshold)
  
  
  # BI, Lateral_Threshold----
  tmp.latthold <- ifelse(is.null(list.BI$Lateral_Threshold), NA,
                         list.BI$Lateral_Threshold)
  
  
  # BI, Shock_Threshold----
  tmp.shockthold <- ifelse(is.null(list.BI$Shock_Threshold), NA,
                           list.BI$Shock_Threshold)
  
  
  return(data.frame(dDayoftheWeek = tmp.dayoftheweek,
                    dMonth = tmp.dmonth,
                    dDate = tmp.ddate,
                    dYear = tmp.dyear,
                    timeHr = tmp.timehr,
                    timeMin = tmp.timemin,
                    timeAMPM = tmp.ampm,
                    timeZone = tmp.timezone,
                    evntTrigger = tmp.evnttrigger,
                    maxFwdForce = tmp.maxfwdforce,
                    maxLatForce = tmp.maxlatforce,
                    triggerSpeedKMH = tmp.triggerspeed,
                    moveDir = tmp.movedir,
                    locLat = tmp.loclat,
                    locLong = tmp.loclong,
                    serialNum = tmp.serialnum,
                    firmwareVer = tmp.firmwarever,
                    fwdThold = tmp.fwdthold,
                    latThold = tmp.latthold,
                    shockThold = tmp.shockthold))
}


# 4. 获取单个ini文件中的某一Move Information----
GetMoveInfo <- function(tFile,
                        kTime = NA,
                        kTag = NA){
  # Input: 
  #  tFile: 读取的单个ini文件。
  #  kTime: 目标时刻。
  #  kTag: 返回数据框的时间标签。
  #
  # Output: 含Move Information各项信息的数据框。
  
  if (is.na(kTime)) {
    stop("Please input the 'kTime'.")  # 没有输入kTime
  } else if (is.na(kTag)) {
    stop("Please input the 'kTag'.")  # 没有输入kTag
  } else if (is.character(kTime) & is.character(kTag)) {
    
    kDropListNum <- which(names(tFile) == "Basic Information")  # 识别BI
    tmp.list <- tFile[-kDropListNum]  # 只含MoveInformation的list
    list.moveinfo <- list.filter(tmp.list, TIME == kTime)  # 目标list
    
    # 有满足kTimeTag时间点的 Move Information 列表
    if (length(list.moveinfo) != 0) {
      
      # FWD
      if (!is.null(strsplit(x = list.moveinfo[[1]]$FWD,
                            split = " ")[[1]][2])) {  # FWD不为NULL
        tmp.fwd <- strsplit(x = list.moveinfo[[1]]$FWD, split = " ")[[1]][2]
      } else {   # FWD为NULL
        tmp.fwd <- NA
      }
      
      # LAT
      if (!is.null(strsplit(x = list.moveinfo[[1]]$LAT,
                            split = " ")[[1]][2])) {  # LAT不为NULL
        tmp.lat <- strsplit(x = list.moveinfo[[1]]$LAT, split = " ")[[1]][2]
      } else {  # LAT为NULL
        tmp.lat <- NA
      }
      
      # Time
      tmp.movetime <- kTime
      
      # GPS, Speed
      if (!is.null(strsplit(x = list.moveinfo[[1]]$GPS,
                            split = " ")[[1]][1]) &
          strsplit(x = list.moveinfo[[1]]$GPS,
                   split = " ")[[1]][1] != "--" &
          strsplit(x = list.moveinfo[[1]]$GPS,
                   split = " ")[[1]][1] != "Speed") {  # GPS不为NULL
        tmp.speed <- strsplit(x = list.moveinfo[[1]]$GPS, split = " ")[[1]][1]
      } else {  # GPS为NULL
        tmp.speed <- NA
      }
      
      
    # 没有满足kTimeTag时间点的 Move Information 列表
    } else {
      tmp.fwd <- NA
      tmp.lat <- NA
      tmp.movetime <- kTime
      tmp.speed <- NA
    }
    
    # 返回数据框变量名
    kColName <- c(paste("fwd", kTag, sep = ""),
                  paste("lat", kTag, sep = ""),
                  paste("move", kTag, sep = ""),
                  paste("speedKMH", kTag, sep = ""))
    
    tmp.df <- data.frame(tmp.fwd, tmp.lat, tmp.movetime, tmp.speed)
    names(tmp.df) <- kColName
    
    return(tmp.df)
    
  } else {
    stop("Please check the input 'kTime' and 'kTag'.\
  The 'kTime' should be a character variable.\
  The 'kTag' should be a character variable.")
  }
}


# 5. 批量读取单个ini文件中的所有Move Information----
GetBatchMoveInfo <- function(tFile,
                             kVectorTime = NA,
                             kVectorTag = NA){
  # Input: 
  #  tFile: 读取的单个ini文件。
  #  kVectorTime: 目标时刻，向量。
  #  kVectorTag: 返回数据框的时间标签，向量。
  #
  # Output: 含Move Information各项信息的数据框。
  
  if (is.na(kVectorTime)[1]) {
    stop("Please input the 'kVectorTime'.")  # 没有输入kVectorTime
  } else if (is.na(kVectorTag)[1]) {
    stop("Please input the 'kVectorTag'.")  # 没有输入kVectorTag
  } else if (is.character(kVectorTime) & is.character(kVectorTag) &
             length(kVectorTime) == length(kVectorTag)) {
    
    for (kTimeIdx in 1:length(kVectorTime)) {  # 依次计算各个Move Information
      
      tmpdf.moveinfo <- GetMoveInfo(tFile = tFile,
                                    kTime = kVectorTime[kTimeIdx],
                                    kTag = kVectorTag[kTimeIdx])
      
      ifelse(kTimeIdx == 1,
             df.moveinfo <- tmpdf.moveinfo,
             df.moveinfo <- cbind(df.moveinfo, tmpdf.moveinfo))
    }
    
    return(df.moveinfo)
    
  } else {
    stop("Please check the input 'kVectorTime' and 'kVectorTag'.\
  The 'kVectorTime' should be a character vector.\
  The 'kVectorTag' should be a character vector.\
  The 'kVectorTime' and 'kVectorTag' should be the same length.")
  }
}
















