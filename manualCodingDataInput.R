#-------------- Code Description ----------------------------------------------#
# Description: 导入人工编码的csv数据。
#
# Notes:
# ver0.1, 20180802，By MaoYan: 导入数据。
#
#------------------------------------------------------------------------------#


# 加载数据导入需要的函数
source('E:/R/iniFile/iniDataInput.R', encoding = 'UTF-8')

# 导入数据
setwd("E:/R/iniFile/ManualCodingData")
df.mcodingdata <- read.table(file = "BBS_data_20150831.csv",
                             header = TRUE,
                             sep = ",",
                             stringsAsFactors = FALSE)

# 导入数据重命名
kColName <- c("evntNum",  # 事件编号
              "rdSurf",  # 道路表面情况
              "lightCond",  # 光线情况
              "tfcDens",  # 交通密度（是否拥堵）
              "weatCond",  # 天气状况
              "tfcSep",  # 交通分隔情况
              "tfcCtrl",  # 交通控制情况
              "alignCond",  # 道路平面线形
              "gradeCond",  # 沿道路方向倾斜趋势
              "tfcLane",  # 车道数
              "jctTyp",  # 道路交汇的方式
              "preMnvr",  # 事件发生前车辆状态
              "wshldCond",  # 车辆挡风玻璃是否干净
              "drvrGlasses",  # 驾驶人是否佩戴眼镜
              "drvrSeatbelt",  # 驾驶人是否佩戴安全带
              "handsOnWheel",  # 方向盘握持方式
              "drvrFaceDir",  # 驾驶人面部朝向
              "triggerTyp",  # 触发类型
              "pptFactor",  # 事件发生的触发因素
              "numOfOthObjs",  # 其他涉及本交通事件的参与方数目
              "obj2Typ",  # 参与方2的类型
              "obj2Loc",  # 参与方2的位置
              "obj2Mnvr",  # 参与方2事件发生前的车辆状态（机动车or非机动车）
              "obj2Rxn",  # 参与方2事件发生前的驾驶人操作（机动车or非机动车）
              "obj3Typ",  # 参与方3的类型
              "obj3Loc",  # 参与方3的位置
              "obj3Mnvr",  # 参与方3事件发生前的车辆状态（机动车or非机动车）
              "obj3Rxn",  # 参与方3事件发生前的驾驶人操作（机动车or非机动车）
              "drvrImpaired",  # 驾驶能力是否丧失（疲劳or饮酒等）
              "drvrGndr",  # 驾驶人性别
              "drvrDistract",  # 驾驶人是否分心
              "distractCause",  # 驾驶人分心的原因
              "assOnDistract",  # 驾驶人分心对事件发生的影响
              "othRiskBehavior",  # 驾驶人其他危险行为
              "infraCause",  # 交通设施原因
              "visObstruction",  # 是否有视野障碍
              "obsCause",  # 视野障碍的原因
              "vehFactor",  # 是否有车辆障碍对事件发生产生影响
              "avoidMnvr",  # 驾驶人的避让操作
              "evntSeverity",  # 事件严重程度
              "evntTyp")  # 事件类型

names(df.mcodingdata) <- kColName


# 按照evntNum合并df.event和df.mcodingdata，只保留二者的交集
df.mcodingdata$evntNum <- paste("Event",
                                df.mcodingdata$evntNum,
                                sep = "")  # mcodingdata evntNum变量修改

df.evntdata <- merge(x = df.event,
                     y = df.mcodingdata,
                     by = "evntNum")  # 数据合并，只保留交集
