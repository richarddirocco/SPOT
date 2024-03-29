library(ggplot2)
library(Cairo)

GraphData <- NULL
Flow <- seq(from=.001, to=1, by=0.001)
GraphData <- data.frame(Flow)
GraphData$Salmon_Walleye_Area <- GraphData$Flow / 0.098
GraphData$Catfish_Sunfish_Area <- GraphData$Flow / 0.106
GraphData$Sturgeon_Area <- GraphData$Flow / 0.082
GraphData$Pike_Area <- GraphData$Flow / 0.055
GraphData$Eel_Area <- GraphData$Flow / 0.035
GraphData$Herring_Area <- GraphData$Flow / 0.093
GraphData$Flow_l_per_s <- GraphData$Flow * 1000

colours = brewer.pal(8,"Set1")

#English
ggplot(data=GraphData, aes(Flow_l_per_s)) +
  theme_bw()+ 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7), 
        legend.key = element_blank(), 
        legend.position="top", 
        legend.text=element_text(size=12),
        axis.text = element_text(size=13), 
        axis.title = element_text(size=15),
        plot.margin=unit(c(1,1,1,1),"cm")) +
  scale_x_continuous(name = expression('Intake flow rate (L/s)'), expand = c(0,0) , limits = c(0,250)) +
  scale_y_continuous(name = expression('Effective screen area (m'^2*')'), expand = c(0,0) , limits = c(0,8), breaks = scales::pretty_breaks(n = 4)) +
  geom_line(aes(y=Salmon_Walleye_Area, colour = "Salmon & Walleye      "), size = 1)+
  geom_line(aes(y=Catfish_Sunfish_Area, colour = "Catfish & Sunfish          "), size = 1)+
  geom_line(aes(y=Sturgeon_Area, colour = "Sturgeon      "), size = 1)+
  geom_line(aes(y=Pike_Area, colour = "Pike      "), size = 1)+
  geom_line(aes(y=Eel_Area, colour = "Eel      "), size = 1)+
  geom_line(aes(y=Herring_Area, colour="Herring      "),size=1)+
  
  #Add legend
  scale_colour_manual("", 
                      breaks=c("Catfish & Sunfish          ", 
                               "Eel      ", 
                               "Herring      ", 
                               "Pike      ", 
                               "Salmon & Walleye      ", 
                               "Sturgeon      "),
                      values = c("Catfish & Sunfish          "= colours[1], 
                                 "Eel      "= colours[2],
                                 "Herring      "= colours[3],
                                 "Pike      "= colours[4],
                                 "Salmon & Walleye      "= colours[5],
                                 "Sturgeon      "= colours[7]))

#French
ggplot(data=GraphData, aes(Flow_l_per_s)) +
  theme_bw()+ 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7), 
        legend.key = element_blank(), 
        legend.position="top", 
        legend.text=element_text(size=12),
        axis.text = element_text(size=13), 
        axis.title = element_text(size=15),
        plot.margin=unit(c(1,1,1,1),"cm")) +
  scale_x_continuous(name = expression('Débit d’entrée (L/s)'), expand = c(0,0) , limits = c(0,250)) +
  scale_y_continuous(name = expression('Superficie utile du grillage (m'^2*')'), expand = c(0,0) , limits = c(0,8), breaks = scales::pretty_breaks(n = 4)) +
  geom_line(aes(y=Salmon_Walleye_Area, colour = "Saumon et doré jaune      "), size = 1)+
  geom_line(aes(y=Catfish_Sunfish_Area, colour = "Poisson-chat et malachigan          "), size = 1)+
  geom_line(aes(y=Sturgeon_Area, colour = "Esturgeon      "), size = 1)+
  geom_line(aes(y=Pike_Area, colour = "Brochet      "), size = 1)+
  geom_line(aes(y=Eel_Area, colour = "Anguille      "), size = 1)+
  geom_line(aes(y=Herring_Area, colour="Hareng      "),size=1)+
  
  #Add legend
  scale_colour_manual("", 
                      breaks=c("Poisson-chat et malachigan          ", 
                               "Anguille      ", 
                               "Hareng      ", 
                               "Brochet      ", 
                               "Saumon et doré jaune      ", 
                               "Esturgeon      "),
                      values = c("Poisson-chat et malachigan          "= colours[1], 
                                 "Anguille      "= colours[2],
                                 "Hareng      "= colours[3],
                                 "Brochet      "= colours[4],
                                 "Saumon et doré jaune      "= colours[5],
                                 "Esturgeon      "= colours[7]))
