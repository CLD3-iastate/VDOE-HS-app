# Code from Sarah - make the regions map for the HS tab

library(patchwork)
library(ggplot2)

# # CB Palette
# cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# wheel <- function(col, radius = 1, ...)
#   pie(rep(1, length(col)), col = col, radius = radius, ...)
# wheel(cbPalette)

hs_fafsa <- readRDS("data/hs_fafsa.rds")


hs_fafsa$`AEA Name` <- factor(hs_fafsa$`AEA Name`)
hs_fafsa$`AEA Name` <- factor(hs_fafsa$`AEA Name`, levels=rev(levels(hs_fafsa$`AEA Name`)))


bar <- ggplot(hs_fafsa, aes(x = `AEA Name`)) + #, fill = as.factor(`AEA Name`))) +
  #scale_fill_discrete(cbPalette) +
  scale_y_continuous(limits = c(0, 60), breaks = c(0, 20, 40, 60))+
  #scale_y_discrete(labels= c("Region 8", "Region 7", "Region 6", "Region 5", "Region 4", "Region 3", "Region 2", "Region 1")) + 
  geom_bar(stat = "count", fill = "#CC79A7")+
  labs(y = "Number of High Schools", x = "")+
  theme(panel.grid.major = element_blank(), 
        axis.text.y = element_text(size = 16, color = "black"),
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.y   = element_blank(),
        axis.line.x = element_line(colour = "black")) +
  coord_flip() 

bar
