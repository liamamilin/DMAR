
# 读取数据 --------------------------------------------------------------------
# psm

load("/Users/milin/Library/Containers/com.tencent.xinWeChat/Data/Library/Application\ Support/com.tencent.xinWeChat/2.0b4.0.9/53566e35a70cea7efea3dd23a98b7c78/Message/MessageTemp/91cd85d3f31000ac413bdbff4e17f50f/File/psm-ceac-CEAC图数据.Rdata")

load("/Users/milin/Library/Containers/com.tencent.xinWeChat/Data/Library/Application\ Support/com.tencent.xinWeChat/2.0b4.0.9/53566e35a70cea7efea3dd23a98b7c78/Message/MessageTemp/91cd85d3f31000ac413bdbff4e17f50f/File/psm-psa-散点图数据.Rdata")

load("/Users/milin/Library/Containers/com.tencent.xinWeChat/Data/Library/Application\ Support/com.tencent.xinWeChat/2.0b4.0.9/53566e35a70cea7efea3dd23a98b7c78/Message/MessageTemp/91cd85d3f31000ac413bdbff4e17f50f/File/psm-tornado-龙卷风图数据.Rdata")
 
# dt
load("/Users/milin/Library/Containers/com.tencent.xinWeChat/Data/Library/Application\ Support/com.tencent.xinWeChat/2.0b4.0.9/53566e35a70cea7efea3dd23a98b7c78/Message/MessageTemp/91cd85d3f31000ac413bdbff4e17f50f/File/decision_psa.Rdata")

load("/Users/milin/Library/Containers/com.tencent.xinWeChat/Data/Library/Application\ Support/com.tencent.xinWeChat/2.0b4.0.9/53566e35a70cea7efea3dd23a98b7c78/Message/MessageTemp/91cd85d3f31000ac413bdbff4e17f50f/File/decision_tornado.Rdata")

 

# # 1. 标题 -----------------------------------------------------------------


# "1、支持设置图表标题名称

TitleSetting <- function(titleText = "标题",
                         subtitleText = "副标题",
                         captionText = "说明文字",
                         xlabText = "x轴线标签",
                         ylabText = "y轴线标签"){
  title <- labs(title = titleText,subtitle = subtitleText,
                caption = captionText,x=xlabText,y=ylabText)
  return(title)
  
}


p+TitleSetting()


# 2、支持设置图表标题字体大小、字体颜色、加粗

ploTitleFace <- "bold" #标题是不是粗体/斜体 是否粗体("plain", "italic", "bold", "bold.italic")
ploTitleColor <- "red" # 标题颜色可以是英文单词colors()，可以是rgb,rgb()可以是数字1表示的颜色是colors()[1]，可以是颜色十六进制代码Hex code：#69b3a2
ploTitleSize <- 10 # 标题大小
ploTitleHjust <- 1 #  标题水平位置[0,1] 水平位置
ploTitleVjust <- 0.9 # 标题垂直位置[0,1] 垂直位置
ploTitleAngle <- 0 # 标题角度[0,360] 反转角度

TiitleTheme <- function(ploTitleFace = NULL, 
                        ploTitleColor = NULL,
                        ploTitleSize=NULL,
                        ploTitleHjust=NULL,
                        ploTitleVjust=NULL,
                        ploTitleAngle=NULL){
  titleTheme <- theme(plot.title = element_text(
    face = ploTitleFace, # ("plain", "italic", "bold", "bold.italic")
    colour = ploTitleColor, # 可以是英文单词colors()，可以是rgb,rgb()可以是数字1表示的颜色是colors()[1]，可以是颜色十六进制代码Hex code：#69b3a2
    size = ploTitleSize,
    hjust = ploTitleHjust, #0.5, # [0,1] 水平位置
    vjust = ploTitleVjust, #0.9, # [0,1] 垂直位置
    angle = ploTitleAngle, # [0,360]角度
    
  )) 
  return(titleTheme)
}

p+TitleSetting() +TiitleTheme()

ploSubtitleFace <- "bold" # 是否粗体("plain", "italic", "bold", "bold.italic")
ploSubtitleColor <- "red" # 可以是英文单词colors()，可以是rgb,rgb()可以是数字1表示的颜色是colors()[1]，可以是颜色十六进制代码Hex code：#69b3a2
ploSubtitleSize <- 10
ploSubtitleHjust <- 1 #  [0,1] 水平位置
ploSubtitleVjust <- 0.9 # [0,1] 垂直位置
ploSubtitleAngle <- 0 # [0,360] 反转角度


SubtitleTheme <- function(ploSubtitleFace=NULL, # 副标题
                          ploSubtitleColor=NULL, # 颜色
                          ploSubtitleSize=NULL, # 大小
                          ploSubtitleHjust=NULL, # 水平位置
                          ploSubtitleVjust=NULL, #垂直位置
                          ploSubtitleAngle=0){
  subtitleTheme <- theme(plot.subtitle = element_text(
    face = ploSubtitleFace, # ("plain", "italic", "bold", "bold.italic")
    colour = ploSubtitleColor, # 可以是英文单词colors()，可以是rgb,rgb()可以是数字1表示的颜色是colors()[1]，可以是颜色十六进制代码Hex code：#69b3a2
    size = ploSubtitleSize,
    hjust = ploSubtitleHjust, # [0,1] 水平位置
    vjust = ploSubtitleVjust, # [0,1] 垂直位置
    angle = ploSubtitleAngle, # [0,360]角度
    
  ))
  
  return(subtitleTheme)
} # 角度

p+TitleSetting()+SubtitleTheme()


ploCaptionFace <- "bold" # 是否粗体("plain", "italic", "bold", "bold.italic")
ploCaptionColor <- "red" # 可以是英文单词colors()，可以是rgb,rgb()可以是数字1表示的颜色是colors()[1]，可以是颜色十六进制代码Hex code：#69b3a2
ploCaptionSize <- 10
ploCaptionHjust <- 1 #  [0,1] 水平位置
ploCaptionVjust <- 0.9 # [0,1] 垂直位置
ploCaptionAngle <- 0 # [0,360] 反转角度


CaptionTheme <- function(ploCaptionFace=NULL, # 说明文字
                         ploCaptionColor=NULL, # 说明文字颜色
                         ploCaptionSize=NULL,  # 大小
                         ploCaptionHjust=NULL, # 水平
                         ploCaptionVjust=NULL, # 垂直
                         ploCaptionAngle=NULL){
  captionTheme <- theme(plot.caption = element_text(
    face = ploCaptionFace, # ("plain", "italic", "bold", "bold.italic")
    colour = ploCaptionColor, # 可以是英文单词colors()，可以是rgb,rgb()可以是数字1表示的颜色是colors()[1]，可以是颜色十六进制代码Hex code：#69b3a2
    size = ploCaptionSize,
    hjust = ploCaptionHjust, # [0,1] 水平位置
    vjust = ploCaptionVjust, # [0,1] 垂直位置
    angle = ploCaptionAngle, # [0,360]角度
    
  ))
  
  return(captionTheme)
} # 角度


p+TitleSetting() +CaptionTheme()


# # 2. 标签 -----------------------------------------------------------------


# "1、支持设置图表标签字体大小、字体颜色、加粗； 
# 2、支持设置图表标签的小数位数；-图表标签是字符串，无需设置小数位数
# 3、支持设置图表标签位置：
#       标签内：则标签位于图表内部，如条形图，标签位于条块内；
#       标签外：则标签位于图表外部，如条形图，标签位于条块外 "
textLabelx <- NULL
textLabely <- NULL
textLabel <- "text label"
textLabelColor <- "black"
textLabelSize <- 10
textLabelFontface <- "plain"
textLabelAngle = 0
AddLabel <- function(textLabelx=NULL, # 标签x轴位置
                     textLabely=NULL, # y轴位置
                     textLabel="text label", # 标签文本
                     textLabelColor=NULL, # 颜色
                     textLabelSize=NULL, # 大小
                     textLabelAngle=NULL, # 角度
                     textLabelFontface=NULL # 粗体/斜体
                     ){
  textLabel <- annotate(geom = "text",x = textLabelx,y = textLabely,#位置
                        label=textLabel,
                        color=textLabelColor,# 颜色
                        size = textLabelSize, # 大小
                        angle=textLabelAngle, 
                        fontface=textLabelFontface# 粗体
  )
} 


p+TitleSetting() +AddLabel(textLabelx = 50000,
                           textLabely = 0.5,textLabelSize = 4,textLabelColor = "green")

# # 3 图例 ------------------------------------------------------------------



# "1、支持设置图例字体大小、字体颜色、加粗
# 2、支持设置图例位置：顶部居左
# 顶部居中
# 顶部居右
# 底部居左
# 底部居中
# 底部居右"

# color 图例

guildTitle <- "颜色"
guildTitleFace <- "bold"
guildTitleColor <- "green"
guildTitleSize <- 10
guildTitleHjust <- NULL
guildTitleVjust <- NULL
guildTitleAngle <- 0

guildLabelFace <- "bold"
guildLabelColor <- "green"
guildLabelSize <- 10
guildLabelHjust <- NULL
guildLabelVjust <- NULL
guildLabelAngle <- 0


GuildColorSetting <- function(guildTitle="图例标题", # 图例标题
                              guildTitleFace=NULL, # 粗体/斜体
                              guildTitleColor=NULL, # 颜色
                              guildTitleSize=NULL, # 大小
                              guildTitleHjust=NULL, # 水平位置
                              guildTitleVjust=NULL, # 垂直位置
                              guildTitleAngle=NULL, # 角度
                              guildLabelFace=NULL, # 图例标签粗体/斜体
                              guildLabelColor=NULL, # 颜色
                              guildLabelSize=NULL, # 大小
                              guildLabelHjust=NULL, # 水平位置
                              guildLabelVjust=NULL, # 垂直位置
                              guildLabelAngle=NULL # 角度
                              ){
  guildSetting <-guides(colour= guide_legend(title=guildTitle,
                                              title.theme = element_text(
                                                face = guildTitleFace, # ("plain", "italic", "bold", "bold.italic")
                                                colour = guildTitleColor, # 可以是英文单词colors()，可以是rgb,rgb()可以是数字1表示的颜色是colors()[1]，可以是颜色十六进制代码Hex code：#69b3a2
                                                size = guildTitleSize,
                                                hjust = guildTitleHjust, #0.5, # [0,1] 水平位置
                                                vjust = guildTitleVjust, #0.9, # [0,1] 垂直位置
                                                angle = guildTitleAngle, # [0,360]角度
                                              ),
                                              label.theme = element_text(
                                                face = guildLabelFace, # ("plain", "italic", "bold", "bold.italic")
                                                colour = guildLabelColor, # 可以是英文单词colors()，可以是rgb,rgb()可以是数字1表示的颜色是colors()[1]，可以是颜色十六进制代码Hex code：#69b3a2
                                                size = guildLabelSize,
                                                hjust = guildLabelHjust, #0.5, # [0,1] 水平位置
                                                vjust = guildLabelVjust, #0.9, # [0,1] 垂直位置
                                                angle = guildLabelAngle, # [0,360]角度
                                              ))
  )
  return(guildSetting)
}


p+GuildColorSetting()

# 修改图例中的标签名称与颜色

GuildLabelName <- c("a","b") # 如果图例
GuildLabelcolor <- c("red","yellow")
  
GuildColorLabelName <- function(GuildLabelName=c("a","b"), # 图例标签名称/
                                GuildLabelcolor=c("red","yellow") # 图例标签颜色
                                ){
  guildLabel <- scale_color_manual(labels = GuildLabelName,values = GuildLabelcolor)
  # guildLabel <- lims(colour = c("a","b"))
  return(guildLabel)
}


p+GuildColorLabelName()+GuildColorSetting()


# fill 图例 龙卷风图使用下面两个函数修改图例

guildTitle <- "颜色"
guildTitleFace <- "bold"
guildTitleColor <- "green"
guildTitleSize <- 10
guildTitleHjust <- NULL
guildTitleVjust <- NULL
guildTitleAngle <- 0

guildLabelFace <- "bold"
guildLabelColor <- "green"
guildLabelSize <- 10
guildLabelHjust <- NULL
guildLabelVjust <- NULL
guildLabelAngle <- 0


GuildFillSetting <- function(guildTitle="图例标题",
                              guildTitleFace=NULL,
                              guildTitleColor=NULL,
                              guildTitleSize=NULL,
                              guildTitleHjust=NULL,
                              guildTitleVjust=NULL,
                              guildTitleAngle=NULL,
                              guildLabelFace=NULL,
                              guildLabelColor=NULL,
                              guildLabelSize=NULL,
                              guildLabelHjust=NULL,
                              guildLabelVjust=NULL,
                              guildLabelAngle=NULL){
  guildSetting <-guides(fill= guide_legend(title=guildTitle,
                                             title.theme = element_text(
                                               face = guildTitleFace, # ("plain", "italic", "bold", "bold.italic")
                                               colour = guildTitleColor, # 可以是英文单词colors()，可以是rgb,rgb()可以是数字1表示的颜色是colors()[1]，可以是颜色十六进制代码Hex code：#69b3a2
                                               size = guildTitleSize,
                                               hjust = guildTitleHjust, #0.5, # [0,1] 水平位置
                                               vjust = guildTitleVjust, #0.9, # [0,1] 垂直位置
                                               angle = guildTitleAngle, # [0,360]角度
                                             ),
                                             label.theme = element_text(
                                               face = guildLabelFace, # ("plain", "italic", "bold", "bold.italic")
                                               colour = guildLabelColor, # 可以是英文单词colors()，可以是rgb,rgb()可以是数字1表示的颜色是colors()[1]，可以是颜色十六进制代码Hex code：#69b3a2
                                               size = guildLabelSize,
                                               hjust = guildLabelHjust, #0.5, # [0,1] 水平位置
                                               vjust = guildLabelVjust, #0.9, # [0,1] 垂直位置
                                               angle = guildLabelAngle, # [0,360]角度
                                             ))
  )
  return(guildSetting)
}


p+GuildFillSetting()

# 修改图例中的标签名称与颜色

GuildLabelName <- c("a","b") # 如果图例
GuildLabelcolor <- c("red","yellow")

GuildFillLabelName <- function(GuildLabelName=c("a","b"),
                                GuildLabelcolor=c("red","yellow")){
  guildLabel <- scale_fill_manual(labels = GuildLabelName,values = GuildLabelcolor)
  # guildLabel <- lims(colour = c("a","b"))
  return(guildLabel)
}


# # 4 坐标轴 -----------------------------------------------------------------


# # "1、支持设置坐标轴线的类型、颜色、粗细；
# 2、支持设置坐标轴数值标签刻度的最大值、最小值与间隔刻度
# 3、支持设置坐标轴标签的字体大小、颜色、加粗
# 4、支持设置坐标轴标签水平显示还是倾斜显示
# 5、支持设置坐标轴标题
# 6、支持设置坐标轴标题的字体大小、颜色、加粗
# 7、支持设置坐标轴标题距坐标轴的距离
# 8、X轴和Y轴根据图表所需自适应显示，当图表只有X轴时，则只需要显示X轴模块配置"


xlabAxisTitleTextFace <- "plain"
xlabAxisTitleTextColor <- colors()[100]
xlabAxisTitleTextSize <- 10
xlabAxisTitleTextHjust <- NULL
xlabAxisTitleTextVjust <- NULL
xlabAxisTitleTextAngle <- 0

xlabAxisTextFace <- "italic"
xlabAxisTextColor <- colors()[100]
xlabAxisTextSize <- 10
xlabAxisTextHjust <- NULL
xlabAxisTextVjust <- NULL
xlabAxisTextAngle <- 0

xlabAxisTicksSize <- 1
xlabAxisTicksColor <- "red"
xlabAxisTicksLength <- unit(.5,"cm")

xlabAxisLineSize <- 3
xlabAxisLineColor <- "red"
xlabAxisLineType <- 1

XlabTheme <- function(xlabAxisTitleTextFace=NULL, # x轴标题粗体/斜体等
                      xlabAxisTitleTextColor=NULL, # 颜色
                      xlabAxisTitleTextSize=NULL, # 大小
                      xlabAxisTitleTextHjust=NULL, # 水平位置
                      xlabAxisTitleTextVjust=NULL, # 垂直位置
                      xlabAxisTitleTextAngle=NULL, # 角度
                      xlabAxisTextFace=NULL, # 刻度文本 粗体/斜体等
                      xlabAxisTextColor=NULL, # 颜色
                      xlabAxisTextSize=NULL, # 大小
                      xlabAxisTextHjust=NULL, # 水平位置
                      xlabAxisTextVjust=NULL, # 垂直位置
                      xlabAxisTextAngle=NULL, # 角度
                      xlabAxisTicksSize=NULL, # 刻度大小
                      xlabAxisTicksColor=NULL, # 刻度颜色
                      xlabAxisTicksLength=0.1, #  刻度长度
                      xlabAxisLineSize=NULL, # x坐标轴线大小
                      xlabAxisLineColor=NULL, # 颜色
                      xlabAxisLineType=NULL # 线条类型
                      ){ 
  xlabTheme <- theme(axis.title.x= element_text(
    face = xlabAxisTitleTextFace, # ("plain", "italic", "bold", "bold.italic")
    colour = xlabAxisTitleTextColor, # 可以是英文单词colors()，可以是rgb,rgb()可以是数字1表示的颜色是colors()[1]，可以是颜色十六进制代码Hex code：#69b3a2
    size = xlabAxisTitleTextSize,
    hjust = xlabAxisTitleTextHjust, # [0,1] 水平位置
    vjust = xlabAxisTitleTextVjust, # [0,1] 垂直位置
    angle = xlabAxisTitleTextAngle, # [0,360]角度
  ),axis.text.x = element_text( # 坐标轴刻度文字
    face = xlabAxisTextFace, # ("plain", "italic", "bold", "bold.italic")
    colour = xlabAxisTextColor, # 可以是英文单词colors()，可以是rgb,rgb()可以是数字1表示的颜色是colors()[1]，可以是颜色十六进制代码Hex code：#69b3a2
    size = xlabAxisTextSize,
    hjust = xlabAxisTextHjust, # [0,1] 水平位置
    vjust = xlabAxisTextVjust, # [0,1] 垂直位置
    angle = xlabAxisTextAngle, # [0,360]角度
  ),axis.ticks.x = element_line(size = xlabAxisTicksSize, color=xlabAxisTicksColor), # 沿轴的记号
  axis.ticks.length.x   = unit(xlabAxisTicksLength, "cm"), #  刻度线长度
  axis.line.x  = element_line(size = xlabAxisLineSize, colour = xlabAxisLineColor, linetype=xlabAxisLineType)# 修改轴线
  
  
  )
  
  return(xlabTheme)
}


# 修改y轴线

YlabTheme <- function(ylabAxisTitleTextFace=NULL,
                      ylabAxisTitleTextColor=NULL,
                      ylabAxisTitleTextSize=NULL,
                      ylabAxisTitleTextHjust=NULL,
                      ylabAxisTitleTextVjust=NULL,
                      ylabAxisTitleTextAngle=NULL,
                      ylabAxisTextFace=NULL,
                      ylabAxisTextColor=NULL,
                      ylabAxisTextSize=NULL,
                      ylabAxisTextHjust=NULL,
                      ylabAxisTextVjust=NULL,
                      ylabAxisTextAngle=NULL,
                      ylabAxisTicksSize=NULL,
                      ylabAxisTicksColor=NULL,
                      ylabAxisTicksLength=0.1,
                      ylabAxisLineSize=NULL,
                      ylabAxisLineColor=NULL,
                      ylabAxisLineType=NULL){
  ylabTheme <- theme(axis.title.y= element_text(
    face = ylabAxisTitleTextFace, # ("plain", "italic", "bold", "bold.italic")
    colour = ylabAxisTitleTextColor, # 可以是英文单词colors()，可以是rgb,rgb()可以是数字1表示的颜色是colors()[1]，可以是颜色十六进制代码Hex code：#69b3a2
    size = ylabAxisTitleTextSize,
    hjust = ylabAxisTitleTextHjust, # [0,1] 水平位置
    vjust = ylabAxisTitleTextVjust, # [0,1] 垂直位置
    angle = ylabAxisTitleTextAngle, # [0,360]角度
  ),axis.text.y = element_text( # 坐标轴刻度文字
    face = ylabAxisTextFace, # ("plain", "italic", "bold", "bold.italic")
    colour = ylabAxisTextColor, # 可以是英文单词colors()，可以是rgb,rgb()可以是数字1表示的颜色是colors()[1]，可以是颜色十六进制代码Hex code：#69b3a2
    size = ylabAxisTextSize,
    hjust = ylabAxisTextHjust, # [0,1] 水平位置
    vjust = ylabAxisTextVjust, # [0,1] 垂直位置
    angle = ylabAxisTextAngle, # [0,360]角度
  ),axis.ticks.y = element_line(size = ylabAxisTicksSize, color=ylabAxisTicksColor), # 沿轴的记号
  axis.ticks.length.y   = unit(ylabAxisTicksLength, "cm"), #  刻度线长度
  axis.line.y  = element_line(size = ylabAxisLineSize, colour = ylabAxisLineColor, linetype=ylabAxisLineType)# 修改轴线
  
  
  )
  
  return(ylabTheme)
}



p+GuildColorLabelName()+GuildColorSetting() +
  XlabTheme(xlabAxisTitleTextFace = "italic",xlabAxisTextAngle = 90)+
  YlabTheme(ylabAxisTitleTextFace = "italic",ylabAxisTextAngle = 180)


#  修改坐标轴范围
Xlim <- function(start=NULL,end=NULL){ # 开始位置，结束位置
  xlim(start,end)
}

Ylim <- function(start=NULL,end=NULL){
  ylim(start,end)
}
p+GuildColorLabelName()+GuildColorSetting() +Xlim(0,250000)


# # 5 点设置 -----------------------------------------------------------------


# "1、该模块根据图表中的元素判断是否显示，当图表中存在“点”的元素时，该模块才显示；
# 2、支持设置点的类型：圆点
# 空心圆
# 菱形
# 矩形
# 三角
# 3、支持设置点的大小"






# # 6 线设置 -----------------------------------------------------------------



# "1、该模块根据图表种的元素判断是否显示，当图表中存在“线”的元素时，该模块才显示；
# 2、支持设置线的类型
# 3、支持设置线的粗细
# 4、支持设置为平滑曲线，勾选时图表为平滑曲线，不勾选时图表为折线。"




# 7. 条块设置 ------------------------------------------------------------------

# "1、该模块根据图表种的元素判断是否显示，当图表中存在“条块”的元素时，该模块才显示；
# 2、支持设置条块的宽度"


# # 散点图 -------------------------------------------------------------------


PlotData <- data.frame(x=rnorm(n = 1000,),y=rnorm(1000,0,10))

# lineType 可选值
# "blank"：空白线条 对应1
# "solid"：实线
# "dashed"：虚线
# "dotted"：点线
# "dotdash"：点划线
# "longdash"：长划线
# "twodash"：两点划线

# 基础图形
CEP <- function(PlotData, # 数据集/格式参看PlotData
                center=F, # 坐标轴是否居中
                pointShape=20, # 点的类型0-25
                pointColor="black", # 颜色
                pointSize=1, # 大小
                smooth=T, # 平滑曲线还是折线
                lineColor="black", # 线条颜色
                lineType=1,  # 类型
                lineWidth=1 # 宽度
                ){
  if(smooth==T){
    if(center==F){
      require(ggplot2)
      p <- ggplot(data=PlotData,aes(x=x,y=y))+geom_point(shape=pointShape,color=pointColor,size=pointSize)+geom_smooth(color=lineColor,linetype=lineType,linewidth=lineWidth,se=F)+
        theme_classic() + showtext::showtext.auto()
      return(p)
    }else{
      require(ggh4x)
      require(ggplot2)
      p <- ggplot(data=PlotData,aes(x=x,y=y))+geom_point(shape=pointShape,color=pointColor,size=pointSizee)+geom_smooth(color=lineColor,linetype=lineType,linewidth=lineWidth,se=F)+
        theme_classic() + showtext::showtext.auto()+coord_axes_inside(labels_inside = TRUE)
      return(p)
    }
  }else{
    if(center==F){
      require(ggplot2)
      p <- ggplot(data=PlotData,aes(x=x,y=y))+geom_point(shape=pointShape,color=pointColor,size=pointSize)+geom_line(color=lineColor,linetype=lineType,linewidth=lineWidth)+
        theme_classic() + showtext::showtext.auto()
      return(p)
    }else{
      require(ggh4x)
      require(ggplot2)
      p <- ggplot(data=PlotData,aes(x=x,y=y))+geom_point(shape=pointShape,color=pointColor,size=pointSize)+geom_line()+
        theme_classic() + showtext::showtext.auto()+coord_axes_inside(labels_inside = TRUE)
      return(p)
    }
  }


}

CEP(PlotData,lineType = 5,lineWidth = 0.5,smooth = F) 



# 成本可接受曲线图 --------------------------------------------------------------

# cea_out$mce
CEAC <- function(PlotData, # 数据集
                 lineColor, # 线条颜色
                 linetype, # 线条
                 linewidth # 线条宽度
                 ){
  require(ggplot2)
  p <- ggplot(PlotData,aes(x=k,y=prob,color=as.factor(strategy_id)))+
    geom_line(color=lineColor,linetype=lineType,linewidth=lineWidth)+ theme_classic() + showtext::showtext.auto()
  return(p)
}




CEAC(cea_out$mce)

 

 
 
# # 龙卷风图 ------------------------------------------------------------------
# mce

library(ggplot2)

# 
x <- load("/Users/milin/Library/Containers/com.tencent.xinWeChat/Data/Library/Application\ Support/com.tencent.xinWeChat/2.0b4.0.9/53566e35a70cea7efea3dd23a98b7c78/Message/MessageTemp/91cd85d3f31000ac413bdbff4e17f50f/File/psm-tornado-龙卷风图数据.Rdata")
PlotData <- get(x)

Tornado <- function(PlotData, # 数据
                    barWidth=NULL  # 宽度0-1
                    ){
  p <- ggplot(PlotData, aes(var, value, fill=as.factor(level))) +
    coord_flip() + 
    theme_classic()+
    geom_bar(position="identity", stat="identity",width=barWidth) +
    theme(legend.position="top", axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 10))
  return(p)
}

p <- Tornado(PlotData = PlotData[-c(9,10),],barWidth = 0.9)
 
p 

 



# 调整像素并且保存图片 --------------------------------------------------------------

# ggsave("test2.png", units="in", dpi=300, width=4, height=4, device="png")
