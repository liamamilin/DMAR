#' help function
#' some tips of visualization
#' @examples
#' helpfunOfVis()

helpfunOfVis <- function(){
  cat("face参数的可选包括(plain", "italic", "bold", "bold.italic)\n")
  cat("颜色可以是英文单词colors()，可以是rgb,rgb()可以是数字1表示的颜色是colors()[1]，可以是颜色十六进制代码Hex code：#69b3a2\n")
  cat("线条类型可选参数包括：blank,solid,dashed,dotted,dotdash,longdash,twodash.\n")
  cat("点的类型包括.,o,x,s,d,p,h,+,-,*等等\n")
  cat("参考：https://rpubs.com/liam/plotdetail")
}





#' Title Setting
#'
#' @param titleText title text
#' @param subtitleText subtitle text
#' @param captionText caption text
#' @param xlabText xlab text
#' @param ylabText ylab text
#' @return  A ggolot2 object

TitleSetting <- function(titleText = NULL,
                         subtitleText = NULL,
                         captionText = NULL,
                         xlabText = NULL,
                         ylabText = NULL){
  require(ggplot2)
  showtext::showtext_auto()
  title <- labs(title = titleText,subtitle = subtitleText,
                caption = captionText,x=xlabText,y=ylabText)
  return(title)

}







#' Set Title theme
#'
#' @param ploTitleFace title face
#' @param ploTitleFamily title font family
#' @param ploTitleColor title color
#' @param ploTitleSize tilte size
#' @param ploTitleHjust title horizontal position
#' @param ploTitleVjust title vertical position
#' @param ploTitleAngle title angle
#' @return  A ggolot2 object
#'
TitleTheme <- function(ploTitleFamily=NULL,
                       ploTitleFace = NULL,
                       ploTitleColor = NULL,
                       ploTitleSize=NULL,
                       ploTitleHjust=NULL,
                       ploTitleVjust=NULL,
                       ploTitleAngle=NULL){
  titleTheme <- theme(plot.title = element_text(family = ploTitleFamily,
                                                face = ploTitleFace, # ("plain", "italic", "bold", "bold.italic")
                                                colour = ploTitleColor, # 可以是英文单词colors()，可以是rgb,rgb()可以是数字1表示的颜色是colors()[1]，可以是颜色十六进制代码Hex code：#69b3a2
                                                size = ploTitleSize,
                                                hjust = ploTitleHjust, #0.5, # [0,1] 水平位置
                                                vjust = ploTitleVjust, #0.9, # [0,1] 垂直位置
                                                angle = ploTitleAngle, # [0,360]角度

  ))
  return(titleTheme)
}


# p+TitleSetting(titleText = "标题",subtitleText = "副标题",captionText = "图形解释",xlabText = "x轴线标签",ylabText = "y轴线标签")+
#   TitleTheme(ploTitleFace = "bold.italic",ploTitleColor = "red",ploTitleSize = 20,ploTitleHjust = 0.5,ploTitleVjust = 0.5)+
#   XlabTheme(xlabAxisTitleTextFace = "italic",xlabAxisTitleTextColor = "grey")



#' Set Subtitle theme
#'
#' @param ploSubtitleFace subtitle font face
#' @param ploSubtitleFamily subtitle font family
#' @param ploSubtitleColor subtitle color
#' @param ploSubtitleSize subtitle size
#' @param ploSubtitleHjust subtitle horizontal position
#' @param ploSubtitleVjust subtitle vertical position
#' @param ploSubtitleAngle subtitle angle
#' @return  A ggolot2 object
#'
SubtitleTheme <- function(ploSubtitleFamily=NULL,
                          ploSubtitleFace=NULL, # 副标题
                          ploSubtitleColor=NULL, # 颜色
                          ploSubtitleSize=NULL, # 大小
                          ploSubtitleHjust=NULL, # 水平位置
                          ploSubtitleVjust=NULL, #垂直位置
                          ploSubtitleAngle=0){
  subtitleTheme <- theme(plot.subtitle = element_text(
    family = ploSubtitleFamily,
    face = ploSubtitleFace, # ("plain", "italic", "bold", "bold.italic")
    colour = ploSubtitleColor, # 可以是英文单词colors()，可以是rgb,rgb()可以是数字1表示的颜色是colors()[1]，可以是颜色十六进制代码Hex code：#69b3a2
    size = ploSubtitleSize,
    hjust = ploSubtitleHjust, # [0,1] 水平位置
    vjust = ploSubtitleVjust, # [0,1] 垂直位置
    angle = ploSubtitleAngle, # [0,360]角度

  ))

  return(subtitleTheme)
} # 角度



#' Set Caption theme
#'
#' @param ploCaptionFace caption font face
#' @param ploCaptionFamily caption font family
#' @param ploCaptionColor caption color
#' @param ploCaptionSize caption size
#' @param ploCaptionHjust caption horizontal position
#' @param ploCaptionVjust caption vertical position
#' @param ploCaptionAngle caption angle
#' @return  A ggolot2 object
#'

CaptionTheme <- function(ploCaptionFamily=NULL,
                         ploCaptionFace=NULL, # 说明文字
                         ploCaptionColor=NULL, # 说明文字颜色
                         ploCaptionSize=NULL,  # 大小
                         ploCaptionHjust=NULL, # 水平
                         ploCaptionVjust=NULL, # 垂直
                         ploCaptionAngle=NULL){
  captionTheme <- theme(plot.caption = element_text(
    family = ploCaptionFamily,
    face = ploCaptionFace, # ("plain", "italic", "bold", "bold.italic")
    colour = ploCaptionColor, # 可以是英文单词colors()，可以是rgb,rgb()可以是数字1表示的颜色是colors()[1]，可以是颜色十六进制代码Hex code：#69b3a2
    size = ploCaptionSize,
    hjust = ploCaptionHjust, # [0,1] 水平位置
    vjust = ploCaptionVjust, # [0,1] 垂直位置
    angle = ploCaptionAngle, # [0,360]角度

  ))

  return(captionTheme)
} # 角度





#' Add annotate
#'
#' @param textLabelx annotate x position
#' @param textLabely annotate y position
#' @param textLabel annotate label text
#' @param textLabelColor annotate color
#' @param textLabelSize annotate size
#' @param textLabelAngle annotate angle
#' @param textLabelFontface annotate font face
#' @return  A ggolot2 object
#'

AddAnnotate <- function(textLabelx=NULL, # 标签x轴位置
                     textLabely=NULL, # y轴位置
                     textLabel="text label", # 标签文本
                     textLabelColor=NULL, # 颜色
                     textLabelSize=NULL, # 大小
                     textLabelAngle=NULL, # 角度
                     textLabelFontface=NULL # 粗体/斜体
){
  require(ggplot2)
  showtext::showtext_auto()
  textLabel <- annotate(geom = "text",x = textLabelx,y = textLabely,#位置
                        label=textLabel,
                        color=textLabelColor,# 颜色
                        size = textLabelSize, # 大小
                        angle=textLabelAngle,
                        fontface=textLabelFontface# 粗体
  )
}





# color 图例

#' Color guide
#'
#' @param guideTitle legend title
#' @param guideTitleFace legend font face
#' @param guildTitleFamily legend font family
#' @param guideTitleColor legend color
#' @param guideTitleSize legend size
#' @param guideTitleHjust legend x positon
#' @param guideTitleVjust legend y position
#' @param guideTitleAngle legend angle
#' @param guideLabelFace legend label font face
#' @param guildLabelFamily legend label font family
#' @param guideLabelColor legend lable color
#' @param guideLabelSize legend label size
#' @param guideLabelHjust  legend label x postion
#' @param guideLabelVjust legend label y position
#' @param guideLabelAngle legend label angle
#' @return  A ggolot2 object
#'

GuildColorSetting <- function(guildTitle="图例标题", # 图例标题
                              guildTitleFace=NULL, # 粗体/斜体
                              guildTitleFamily=NULL,
                              guildTitleColor=NULL, # 颜色
                              guildTitleSize=NULL, # 大小
                              guildTitleHjust=NULL, # 水平位置
                              guildTitleVjust=NULL, # 垂直位置
                              guildTitleAngle=NULL, # 角度
                              guildLabelFace=NULL, # 图例标签粗体/斜体
                              guildLabelFamily=NULL,
                              guildLabelColor=NULL, # 颜色
                              guildLabelSize=NULL, # 大小
                              guildLabelHjust=NULL, # 水平位置
                              guildLabelVjust=NULL, # 垂直位置
                              guildLabelAngle=NULL # 角度
){
  guildSetting <-guides(colour=guide_legend(title=guildTitle,
                                            title.theme = element_text(
                                              family = guildTitleFamily,
                                              face = guildTitleFace, # ("plain", "italic", "bold", "bold.italic")
                                              colour = guildTitleColor, # 可以是英文单词colors()，可以是rgb,rgb()可以是数字1表示的颜色是colors()[1]，可以是颜色十六进制代码Hex code：#69b3a2
                                              size = guildTitleSize,
                                              angle = guildTitleAngle, # [0,360]角度
                                            ),title.hjust = guildTitleHjust,title.vjust = guildTitleVjust,
                                            label.vjust = guildTitleVjust,label.hjust = guildLabelHjust,
                                            label.theme = element_text(
                                              family = guildLabelFamily,
                                              face = guildLabelFace, # ("plain", "italic", "bold", "bold.italic")
                                              colour = guildLabelColor, # 可以是英文单词colors()，可以是rgb,rgb()可以是数字1表示的颜色是colors()[1]，可以是颜色十六进制代码Hex code：#69b3a2
                                              size = guildLabelSize,
                                              angle = guildLabelAngle, # [0,360]角度
                                            )),linetype="none"
  )
  return(guildSetting)
}



# 修改图例中的标签名称与颜色


#' Change Color guide name and color
#'
#' @param guideLabelName legend label text
#' @param guideLabelcolor legend label color
#' @return  A ggolot2 object
#'
GuideColorLabelName <- function(guideLabelName=c("a","b"), # 图例标签名称/
                                guideLabelcolor=c("red","yellow") # 图例标签颜色
){
  require(ggplot2)
  showtext::showtext_auto()
  guideLabel <- scale_color_manual(labels = guideLabelName,values = guideLabelcolor)
  # guideLabel <- lims(colour = c("a","b"))
  return(guideLabel)
}




# fill 图例 龙卷风图使用下面两个函数修改图例

#' fill guide
#'
#' @param guideTitle legend title
#' @param guideTitleFace legend font face
#' @param guildTitleFamily legend font family
#' @param guideTitleColor legend color
#' @param guideTitleSize legend size
#' @param guideTitleHjust legend x positon
#' @param guideTitleVjust legend y position
#' @param guideTitleAngle legend angle
#' @param guideLabelFace legend label font face
#' @param guildLabelFamily legend label font family
#' @param guideLabelColor legend lable color
#' @param guideLabelSize legend label size
#' @param guideLabelHjust  legend label x postion
#' @param guideLabelVjust legend label y position
#' @param guideLabelAngle legend label angle
#' @return  A ggolot2 object
#'

GuildFillSetting <- function(guildTitle="图例标题",
                             guildTitleFace=NULL,
                             guildTitleFamily=NULL,
                             guildTitleColor=NULL,
                             guildTitleSize=NULL,
                             guildTitleHjust=NULL,
                             guildTitleVjust=NULL,
                             guildTitleAngle=NULL,
                             guildLabelFace=NULL,
                             guildLabelFamily=NULL,
                             guildLabelColor=NULL,
                             guildLabelSize=NULL,
                             guildLabelHjust=NULL,
                             guildLabelVjust=NULL,
                             guildLabelAngle=NULL){
  guildSetting <-guides(fill= guide_legend(title=guildTitle,
                                           title.theme = element_text(
                                             family = guildTitleFamily,
                                             face = guildTitleFace, # ("plain", "italic", "bold", "bold.italic")
                                             colour = guildTitleColor, # 可以是英文单词colors()，可以是rgb,rgb()可以是数字1表示的颜色是colors()[1]，可以是颜色十六进制代码Hex code：#69b3a2
                                             size = guildTitleSize,
                                             angle = guildTitleAngle, # [0,360]角度
                                           ),title.hjust = guildTitleHjust,title.vjust = guildTitleVjust,
                                           label.hjust = guildLabelHjust,label.vjust = guildLabelVjust,
                                           label.theme = element_text(
                                             family = guildLabelFamily,
                                             face = guildLabelFace, # ("plain", "italic", "bold", "bold.italic")
                                             colour = guildLabelColor, # 可以是英文单词colors()，可以是rgb,rgb()可以是数字1表示的颜色是colors()[1]，可以是颜色十六进制代码Hex code：#69b3a2
                                             size = guildLabelSize,
                                             angle = guildLabelAngle, # [0,360]角度
                                           ))
  ) #+ theme(legend.position = legend.position)
  return(guildSetting)
}




# 修改图例中的标签名称与颜色

#' Change fill guide name and color
#'
#' @param guideLabelName legend label text
#' @param guideLabelcolor legend label color
#' @return  A ggplot2 object
#''

GuideFillLabelName <- function(guideLabelName=c("a","b"),
                               guideLabelcolor=c("red","yellow")){
  require(ggplot2)
  showtext::showtext_auto()
  guideLabel <- scale_fill_manual(labels = guideLabelName,values = guideLabelcolor)
  # guideLabel <- lims(colour = c("a","b"))
  return(guideLabel)
}


#' Change legend position
#'
#' @param legend.position Position of legend. option left, right, top, bottom or two element numeric vector
#' @return A ggplot2 object

legendPosition <- function(legend.position="left"){
  p <- theme(legend.position = legend.position)
}


#' Axis x
#'
#' @param xlabAxisTitleTextFace x axis title text face
#' @param xlabAxisTitleTextColor color
#' @param xlabAxisTitleTextSize size
#' @param xlabAxisTitleTextHjust x postion
#' @param xlabAxisTitleTextVjust y position
#' @param xlabAxisTitleTextAngle angle
#' @param xlabAxisTextFace x axis text face
#' @param xlabAxisTextColor color
#' @param xlabAxisTextSize size
#' @param xlabAxisTextHjust x postion
#' @param xlabAxisTextVjust y postion
#' @param xlabAxisTextAngle angle
#' @param xlabAxisTicksSize ticks size
#' @param xlabAxisTicksColor color
#' @param xlabAxisTicksLength length
#' @param xlabAxisLineSize line size
#' @param xlabAxisLineColor color
#' @param xlabAxisLineType type
#' @return  A ggolot2 object
#'

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
  require(ggplot2)
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
  ),axis.ticks.x = element_line(linewidth = xlabAxisTicksSize, color=xlabAxisTicksColor), # 沿轴的记号
  axis.ticks.length.x   = unit(xlabAxisTicksLength, "cm"), #  刻度线长度
  axis.line.x  = element_line(linewidth = xlabAxisLineSize, colour = xlabAxisLineColor, linetype=xlabAxisLineType)# 修改轴线


  )

  return(xlabTheme)
}


# 修改y轴线

#' Axis y
#'
#' @param ylabAxisTitleTextFace axis title text face
#' @param ylabAxisTitleTextColor color
#' @param ylabAxisTitleTextSize size
#' @param ylabAxisTitleTextHjust x postion
#' @param ylabAxisTitleTextVjust y position
#' @param ylabAxisTitleTextAngle angle
#' @param ylabAxisTextFace axis text face
#' @param ylabAxisTextColor color
#' @param ylabAxisTextSize size
#' @param ylabAxisTextHjust x postion
#' @param ylabAxisTextVjust y position
#' @param ylabAxisTextAngle angle
#' @param ylabAxisTicksSize ticks size
#' @param ylabAxisTicksColor color
#' @param ylabAxisTicksLength length
#' @param ylabAxisLineSize line size
#' @param ylabAxisLineColor color
#' @param ylabAxisLineType type
#' @return  A ggolot2 object
#'

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
  require(ggplot2)
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
  ),axis.ticks.y = element_line(linewidth = ylabAxisTicksSize, color=ylabAxisTicksColor), # 沿轴的记号
  axis.ticks.length.y   = unit(ylabAxisTicksLength, "cm"), #  刻度线长度
  axis.line.y  = element_line(linewidth = ylabAxisLineSize, colour = ylabAxisLineColor, linetype=ylabAxisLineType)# 修改轴线


  )

  return(ylabTheme)
}




#  修改坐标轴范围

#' Axis x range
#'
#' @param start start point of x axis
#' @param end point of x axis
#' @param Breaks A numeric vector of positions
#' @return A ggplot2 object

Xlim <- function(start=NULL,end=NULL,Breaks=waiver()){ # 开始位置，结束位置
  require(ggplot2)
  scale_x_continuous(limits = c(start,end),breaks = Breaks)
}


#' Axis y range
#'
#' @param start start point of y axis
#' @param end end point of y axis
#' @param Breaks A numeric vector of positions
#' @return A ggplot2 object <-

Ylim <- function(start=NULL,end=NULL,Breaks=waiver()){
  require(ggplot2)
  scale_x_continuous(limits = c(start,end),breaks = Breaks)
}



#' Change line type
#'
#' @param value  a vector
#' @return A ggplot2 object

lineType <- function(value= c(1,1)){
  require(ggplot2)
  p <- scale_linetype_manual(values = value)
  return(p)
}


#' Add line
#'
#' @param xstart  x start point
#' @param ystart y start point
#' @param xend x end point
#' @param yend y end point
#' @param color color of line
#' @param linetype line type
#' @param lineWidth linee width
#' @return A ggplot2 object


addLine <- function(xstart,ystart,xend,yend,color="black",lineType=1,lineWidth=1){
  require(ggplot2)
  p <- geom_segment(x=xstart,y=ystart,xend=xend,yend=yend,color=color,linetype=lineType,linewidth=lineWidth)

  return(p)

}
# ggsave("test2.png", units="in", dpi=300, width=4, height=4, device="png")
