library(ggplot2)
library(RColorBrewer)

data <- read.csv("apps/speedtime/GroupVariables.csv")
rownames(data)<-data$Group
Time <-(seq(from = 3, to = 1800, by = 1))
PlotData <- as.data.frame(Time)


U    = function (t){data[Group,"k"]*sqrt(g*l)*((sqrt(l/g))^-data[Group,"b"])*t^data[Group,"b"]}
g=9.81
l=0.5
Group <- "Catfish & Sunfish"
PlotData$"Catfish & Sunfish" <- U(PlotData$Time)
Group <- "Eel"
PlotData$"Eel" <- U(PlotData$Time)
Group <- "Herring"
PlotData$"Herring" <- U(PlotData$Time)
Group <- "Salmon & Walleye"
PlotData$"Salmon & Walleye" <- U(PlotData$Time)
Group <- "Sturgeon"
PlotData$"Sturgeon" <- U(PlotData$Time)
Group <- "Pike (derived)"
PlotData$"Pike" <- U(PlotData$Time)

meltPlotData <- melt(PlotData, id=c("Time"))

names(meltPlotData)[names(meltPlotData)=="variable"] <- "Group"

colours = brewer.pal(8,"Set1")


plot <- ggplot(meltPlotData) +
  geom_line(aes(x=Time, y=value, colour = Group), size = 1)+
  ggtitle("Mean swim performance of 500 mm fish")+
  theme_classic()+ 
  theme(axis.line.x = element_line(color="black", size = .5),
        axis.line.y = element_line(color="black", size = .5))+
  theme(legend.position=c(0.8,0.8), legend.text=element_text(size=13), legend.title=element_text(size=15), legend.title.align=0.6)+
  theme(axis.text = element_text(size=13), axis.title = element_text(size=15))+
  theme(plot.margin=unit(c(1,1,0,0),"cm"))+
  scale_x_log10(name = "Swim time (s)", limits=c(3, 1800))+
  scale_y_log10(name = "Swim speed (m/s)", limits=c(.1, 15))+
  annotation_logticks(base = 10, sides = "b")+
  annotation_logticks(base = 10, sides = "l")+
  scale_color_manual(values=brewer.pal(6, "Dark2"))

png(filename="Figures/500mmPlot.png", 
    type="cairo",
    units="in", 
    width=8, 
    height=8, 
    pointsize=12, 
    res=96)
plot
dev.off()



l=0.05
Group <- "Catfish & Sunfish"
PlotData$"Catfish & Sunfish" <- U(PlotData$Time)
Group <- "Eel"
PlotData$"Eel" <- U(PlotData$Time)
Group <- "Herring"
PlotData$"Herring" <- U(PlotData$Time)
Group <- "Salmon & Walleye"
PlotData$"Salmon & Walleye" <- U(PlotData$Time)
Group <- "Sturgeon"
PlotData$"Sturgeon" <- U(PlotData$Time)
Group <- "Pike (derived)"
PlotData$"Pike" <- U(PlotData$Time)

meltPlotData <- melt(PlotData, id=c("Time"))

names(meltPlotData)[names(meltPlotData)=="variable"] <- "Group"

colours = brewer.pal(8,"Set1")


plot <- ggplot(meltPlotData) +
  geom_line(aes(x=Time, y=value, colour = Group), size = 1)+
  ggtitle("Mean swim performance of 50 mm fish")+
  theme_classic()+ 
  theme(axis.line.x = element_line(color="black", size = .5),
        axis.line.y = element_line(color="black", size = .5))+
  theme(legend.position=c(0.8,0.8), legend.text=element_text(size=13), legend.title=element_text(size=15), legend.title.align=0.6)+
  theme(axis.text = element_text(size=13), axis.title = element_text(size=15))+
  theme(plot.margin=unit(c(1,1,0,0),"cm"))+
  scale_x_log10(name = "Swim time (s)", limits=c(3, 1800))+
  scale_y_log10(name = "Swim speed (m/s)", limits=c(.05, 2))+
  annotation_logticks(base = 10, sides = "b")+
  annotation_logticks(base = 10, sides = "l")+
  scale_color_manual(values=brewer.pal(6, "Dark2"))

png(filename="Figures/50mmPlot.png", 
    type="cairo",
    units="in", 
    width=8, 
    height=8, 
    pointsize=12, 
    res=96)
plot
dev.off()
