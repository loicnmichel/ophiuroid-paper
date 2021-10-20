#Load necessary packages
library(dplyr)
library(ggplot2)
library(officer)
library(rvg)
library(devEMF)


#Read in the data
fulldata <- read.csv2("Fig3_data.csv", header = TRUE)

fulldata$d13C <- as.numeric(fulldata$d13C)
fulldata$d15N <- as.numeric(fulldata$d15N)


#Compute mean and SDs for each item in each zone
meanSD <- fulldata %>% group_by(Zone, Item) %>% 
                      summarise(count = length(Item),
                                mC = mean(d13C), 
                                sdC = sd(d13C), 
                                mN = mean(d15N), 
                                sdN = sd(d15N))


#Plot them
(biplot_full <- ggplot(meanSD, aes (x = mC, y = mN, colour = Zone, shape = Item)) +
    geom_point(size = 2) +                                              
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(legend.position="bottom") +
    scale_y_continuous(name = "d15N", breaks = c(5,6,7,8,9,10,11,12,13,14,15),
                       labels = c("5","","7","","9","","11","","13","","15")) +
    scale_x_continuous(name = "d13C", breaks = c(-25,-24,-23,-22,-21,-20,-19,-18,-17),
                       labels = c("","-24","","-22","","-20","","-18","")) +
    scale_colour_manual(name = NULL, values = c("#FF7F00","#E41A1C","#4DAF4A"), 
                        labels = c("BB","CAA","NOW")) +
    scale_shape_manual(name = NULL, values = c(15,16,17,1)) +
    geom_errorbar(data = meanSD, 
                  mapping = aes(x = mC,
                                ymin = mN - sdN, 
                                ymax = mN + sdN), 
                  width = 0) +
    geom_errorbarh(data = meanSD, 
                   mapping = aes(y = mN,
                                 xmin = mC - sdC,
                                 xmax = mC + sdC),
                   height = 0))


#Export the graph to powerpoint for easy tweaking of small details and integration into manuscript document
#Please note that some characters are not supported (hence the lack of delta and per mille in axis titles)
biplot_export <- dml(ggobj = biplot_full)

read_pptx() %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value=biplot_export, location = ph_location(width=6, height=5, type = "body") ) %>%
  print(target = "Fig3_raw.pptx") %>%
  invisible()