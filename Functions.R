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








