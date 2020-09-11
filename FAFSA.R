library(readxl)
library(ggplotly)

VAHS <- read_excel("VA_HS.xlsx", 
                   col_types = c("text", "numeric", "numeric", 
                                 "text", "text", "text", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric"))
View(VAHS)



#Percentage of Complete FAFSA applications/Number of HG Students
FAFSA_CO<-aggregate(x=list(FAFSA=VAHS$COMPLETE20, POP=VAHS$POP12.2019), 
                    by=list(County=VAHS$`Division Name`), 
                    FUN="sum")

CO_Per<-(round((FAFSA_CO$FAFSA/FAFSA_CO$POP)*100, 2))
FAFSA_CO<-data.frame(FAFSA_CO, CO_Per); View(FAFSA_CO)
FAFSA_CO <- read.csv("VDOE-App/FAFSA_CO.csv")

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



library(GGally)


theme_SDAD<-function (base_size=12, base_family="sans", style=c("default","darkunica"), bgcolor=NULL)
{
  if (!is.null(bgcolor)) {
    warning("`bgcolor` is deprecated. Use `style` instead.")
    style<-bgcolor
  }
  style<-match.arg(style)
  bgcolor<-switch(style, default="#FFFFFF", darkunica="#2A2A2B")
  ret<-theme(rect=element_rect(fill=bgcolor, linetype=0, colour = NA),
             text=element_text(size=base_size, family=base_family),
             title=element_text(hjust=0.5),
             axis.title.x=element_text(hjust=0.5),
             axis.title.y=element_text(hjust=0.5),
             panel.grid.major.y=element_line(colour="#D8D8D8"),
             panel.grid.minor.y=element_blank(),
             panel.grid.major.x=element_line(colour="#D8D8D8"),
             panel.grid.minor.x=element_blank(),
             panel.border=element_blank(),
             panel.background=element_blank(),
             legend.position="bottom",
             legend.key=element_rect(fill="#FFFFFF"))
  if (style=="darkunica") {
    ret<-(ret + theme(rect=element_rect(fill=bgcolor),
                      text=element_text(colour="#A0A0A3"),
                      title=element_text(colour="#FFFFFF"),
                      axis.title.x=element_text(colour="#A0A0A3"),
                      axis.title.y=element_text(colour="#A0A0A3"),
                      panel.grid.major.y=element_line(colour="#707073"),
                      legend.title=element_text(colour="#A0A0A3")))
  }
  ret
}




# Plot
ggparcoord(VAHS,
           columns=c(26,24,22,20,18,16), scale="std", 
           groupColumn=1, showPoints=TRUE, 
           title="Parallel Coordinate Plot for Completed FAFSA Submissions",
           alphaLines=0.3) +
  scale_color_manual(values=cbPalette[c(1:8)]) +
  theme_SDAD()+
  theme(
    plot.title = element_text(size=14, hjust=0),
    axis.text.x=element_text(size=12), 
    axis.text.y=element_text(size=12), 
    legend.position="none")

#1280, 629

VAHS1<-VAHS[VAHS$Region=="1",]
DECR1<-ifelse(VAHS1$RELDIF<=-25, "Decrease", "No Decrease")
DECR1<-ordered(DECR1, levels=c("No Decrease", "Decrease"))
VAHS1<-data.frame(cbind(VAHS1, DECR1))

p <- parcoord.val(VAHS1,
           columns=c(26,24,22,20,18,16), scale="globalminmax", 
           groupColumn=28, showPoints=TRUE, shadeBox=NULL,
           title="Region 1 Parallel Coordinate Plot for Completed FAFSA Submissions over Time by High School",
           alphaLines=1, boxplot=TRUE) +
  scale_color_manual(values=cbPalette[c(7,1)]) +
  scale_x_discrete(labels=c("2015-16","2016-17","2017-18","2018-19","2019-20","2020-21")) +
 theme_SDAD() + 
  xlab("") + ylab("Number of Completed FAFSA Applications") +
  theme(
    plot.title = element_text(size=14, hjust=0.5),
    axis.text.x=element_text(size=12), 
    axis.text.y=element_text(size=12), 
    legend.position="none")

#View(VAHS1[order(-VAHS1[,14]), c(5, 14, 27, 11, 10)])
dim(VAHS1)[1]; length(VAHS1$RELDIF[VAHS1$RELDIF<0])


VAHS2<-VAHS[VAHS$Region=="2",]
DECR2<-ifelse(VAHS2$RELDIF<=-25, "Decrease", "No Decrease")
DECR2<-ordered(DECR2, levels=c("No Decrease", "Decrease"))
VAHS2<-data.frame(cbind(VAHS2, DECR2))

parcoord.val(VAHS2,
           columns=c(26,24,22,20,18,16), scale="globalminmax", 
           groupColumn=28, showPoints=TRUE, 
           title="Region 2 Parallel Coordinate Plot for Completed FAFSA Submissions over Time by High School",
           alphaLines=1, boxplot=TRUE) +
  scale_color_manual(values=cbPalette[c(7, 1)]) +
  scale_x_discrete(labels=c("2015-16","2016-17","2017-18","2018-19","2019-20","2020-21")) +
  theme_SDAD() + 
  xlab("") + ylab("Number of Completed FAFSA Applications") +
  theme(
    plot.title = element_text(size=14, hjust=0.5),
    axis.text.x=element_text(size=12), 
    axis.text.y=element_text(size=12), 
    legend.position="none")

#View(VAHS2[order(-VAHS2[,14]), c(5, 14, 27, 11, 10)])
dim(VAHS2)[1]; length(VAHS2$RELDIF[VAHS2$RELDIF<0])


VAHS3<-VAHS[VAHS$Region=="3",]
DECR3<-ifelse(VAHS3$RELDIF<=-25, "Decrease", "No Decrease")
DECR3<-ordered(DECR3, levels=c("No Decrease", "Decrease"))
VAHS3<-data.frame(cbind(VAHS3, DECR3))

parcoord.val(VAHS3,
           columns=c(26,24,22,20,18,16), scale="globalminmax", 
           groupColumn=28, showPoints=TRUE, 
           title="Region 3 Parallel Coordinate Plot for Completed FAFSA Submissions Over Time by High School",
           alphaLines=1, boxplot=TRUE) +
  scale_color_manual(values=cbPalette[c(7, 1)]) +
  scale_x_discrete(labels=c("2015-16","2016-17","2017-18","2018-19","2019-20","2020-21")) +
  theme_SDAD() + 
  xlab("") + ylab("Number of Completed FAFSA Applications") +
  theme(
    plot.title = element_text(size=14, hjust=0.5),
    axis.text.x=element_text(size=12), 
    axis.text.y=element_text(size=12), 
    legend.position="none")

#View(VAHS3[order(-VAHS3[,14]), c(5, 14, 27, 11, 10)])
dim(VAHS3)[1]; length(VAHS3$RELDIF[VAHS3$RELDIF<0])


VAHS4<-VAHS[VAHS$Region=="4",]
DECR4<-ifelse(VAHS4$RELDIF<=-25, "Decrease", "No Decrease")
DECR4<-ordered(DECR4, levels=c("No Decrease", "Decrease"))
VAHS4<-data.frame(cbind(VAHS4, DECR4))

parcoord.val(VAHS4,
           columns=c(26,24,22,20,18,16), scale="globalminmax", 
           groupColumn=28, showPoints=TRUE, 
           title="Region 4 Parallel Coordinate Plot for Completed FAFSA Submissions Over Time by High School",
           alphaLines=1, boxplot=TRUE) +
  scale_color_manual(values=cbPalette[c(7, 1)]) +
  scale_x_discrete(labels=c("2015-16","2016-17","2017-18","2018-19","2019-20","2020-21")) +
  theme_SDAD() + 
  xlab("") + ylab("Number of Completed FAFSA Applications") +
  theme(
    plot.title = element_text(size=14, hjust=0),
    axis.text.x=element_text(size=12), 
    axis.text.y=element_text(size=12), 
    legend.position="none")

#View(VAHS4[order(-VAHS4[,14]), c(5, 14, 27, 11, 10)])
dim(VAHS4)[1]; length(VAHS4$RELDIF[VAHS4$RELDIF<0])


VAHS5<-VAHS[VAHS$Region=="5",]
DECR5<-ifelse(VAHS5$RELDIF<=-25, "Decrease", "No Decrease")
DECR5<-ordered(DECR5, levels=c("No Decrease", "Decrease"))
VAHS5<-data.frame(cbind(VAHS5, DECR5))

parcoord.val(VAHS5,
           columns=c(26,24,22,20,18,16), scale="globalminmax", 
           groupColumn=28, showPoints=TRUE, 
           title="Region 5 Parallel Coordinate Plot for Completed FAFSA Submissions Over Time by High School",
           alphaLines=1, boxplot=TRUE) +
  scale_color_manual(values=cbPalette[c(7, 1)]) +
  scale_x_discrete(labels=c("2015-16","2016-17","2017-18","2018-19","2019-20","2020-21")) +
  theme_SDAD() + 
  xlab("") + ylab("Number of Completed FAFSA Applications") +
  theme(
    plot.title = element_text(size=14, hjust=0.5),
    axis.text.x=element_text(size=12), 
    axis.text.y=element_text(size=12), 
    legend.position="none")

#View(VAHS5[order(-VAHS5[,14]), c(5, 14, 27, 11, 10)])
dim(VAHS5)[1]; length(VAHS5$RELDIF[VAHS5$RELDIF<0])


VAHS6<-VAHS[VAHS$Region=="6",]
DECR6<-ifelse(VAHS6$RELDIF<=-25, "Decrease", "No Decrease")
DECR6<-ordered(DECR6, levels=c("No Decrease", "Decrease"))
VAHS6<-data.frame(cbind(VAHS6, DECR6))

parcoord.val(VAHS6,
           columns=c(26,24,22,20,18,16), scale="globalminmax", 
           groupColumn=28, showPoints=TRUE, 
           title="Region 6 Parallel Coordinate Plot for Completed FAFSA Submissions Over Time by High School",
           alphaLines=1, boxplot=TRUE) +
  scale_color_manual(values=cbPalette[c(7, 1)]) +
  scale_x_discrete(labels=c("2015-16","2016-17","2017-18","2018-19","2019-20","2020-21")) +
  theme_SDAD() + 
  xlab("") + ylab("Number of Completed FAFSA Applications") +
  theme(
    plot.title = element_text(size=14, hjust=0.5),
    axis.text.x=element_text(size=12), 
    axis.text.y=element_text(size=12), 
    legend.position="none")

#View(VAHS6[order(-VAHS6[,14]), c(5, 14, 27, 11, 10)])
dim(VAHS6)[1]; length(VAHS6$RELDIF[VAHS6$RELDIF<0])


VAHS7<-VAHS[VAHS$Region=="7",]
DECR7<-ifelse(VAHS7$RELDIF<=-25, "Decrease", "No Decrease")
DECR7<-ordered(DECR7, levels=c("No Decrease", "Decrease"))
VAHS7<-data.frame(cbind(VAHS7, DECR7))

parcoord.val(VAHS7,
           columns=c(26,24,22,20,18,16), scale="globalminmax", 
           groupColumn=28, showPoints=TRUE, 
           title="Region 7 Parallel Coordinate Plot for Completed FAFSA Submissions Over Time by High School",
           alphaLines=1, boxplot=TRUE) +
  scale_color_manual(values=cbPalette[c(7, 1)]) +
  scale_x_discrete(labels=c("2015-16","2016-17","2017-18","2018-19","2019-20","2020-21")) +
  theme_SDAD() + 
  xlab("") + ylab("Number of Completed FAFSA Applications") +
  theme(
    plot.title = element_text(size=14, hjust=0.5),
    axis.text.x=element_text(size=12), 
    axis.text.y=element_text(size=12), 
    legend.position="none")

#View(VAHS7[order(-VAHS7[,14]), c(5, 14, 27, 11, 10)])
dim(VAHS7)[1]; length(VAHS7$RELDIF[VAHS7$RELDIF<0])


VAHS8<-VAHS[VAHS$Region=="8",]
DECR8<-ifelse(VAHS8$RELDIF<=-25, "Decrease", "No Decrease")
DECR8<-ordered(DECR8, levels=c("No Decrease", "Decrease"))
VAHS8<-data.frame(cbind(VAHS8, DECR8))

parcoord.val(VAHS8,
           columns=c(26,24,22,20,18,16), scale="globalminmax", 
           groupColumn=28, showPoints=TRUE, 
           title="Region 8 Parallel Coordinate Plot for Completed FAFSA Submissions Over Time by High School",
           alphaLines=1, boxplot=TRUE, shadeBox=NULL) +
  scale_color_manual(values=cbPalette[c(7, 1)]) +
  scale_x_discrete(labels=c("2015-16","2016-17","2017-18","2018-19","2019-20","2020-21")) +
  theme_SDAD() + 
  xlab("") + ylab("Number of Completed FAFSA Applications") +
  theme(
        plot.title=element_text(size=14, hjust=0.5),
        axis.text.x=element_text(size=12), 
        axis.text.y=element_text(size=12), 
        legend.position="none")

#View(VAHS8[order(-VAHS8[,14]), c(5, 14, 27, 11, 10)])
dim(VAHS8)[1]; length(VAHS8$RELDIF[VAHS8$RELDIF<0])


 