#Load necessary packages
library(SIBER)
library(dplyr)
library(ggplot2)
library(officer)
library(rvg)
library(devEMF)
library(cowplot)


#Read in the data
Nichedata_full <- read.csv("Fig5_data.csv", header=T, sep=";")


#Extract the data for each zone
Nichedata_CAA <- Nichedata_full %>% filter(Zone == "Canadian Arctic Archipelago")

Nichedata_NOW <- Nichedata_full %>% filter(Zone == "North Water Polynia")

Nichedata_BB <- Nichedata_full %>% filter(Zone == "Baffin Bay")


#Format data from each zone for use by SIBER
siber_CAA <- createSiberObject(Nichedata_CAA[,3:6])

siber_NOW <- createSiberObject(Nichedata_NOW[,3:6])

siber_BB <- createSiberObject(Nichedata_BB[,3:6])


#Create a function that will extract ellipse values out of the SIBER objects
pullEllipseStd = function(data, x, y) {
  return(as.data.frame(addEllipse(data$ML.mu[[x]][ , , y],
                                  data$ML.cov[[x]][ , , y],
                                  m = NULL,
                                  n = 100,
                                  p.interval = NULL,
                                  ci.mean = FALSE, do.plot = FALSE)))
}


#And use it to get ellipse positions for each species in each zone
Ellipse_CAA_Obid = pullEllipseStd(data=siber_CAA, x=1, y=1)
Ellipse_CAA_Oser = pullEllipseStd(data=siber_CAA, x=1, y=2)
Ellipse_CAA_Obor = pullEllipseStd(data=siber_CAA, x=1, y=3)

Ellipse_NOW_Obid = pullEllipseStd(data=siber_NOW, x=1, y=1)
Ellipse_NOW_Oser = pullEllipseStd(data=siber_NOW, x=1, y=2)
Ellipse_NOW_Obor = pullEllipseStd(data=siber_NOW, x=1, y=3)

Ellipse_BB_Obid = pullEllipseStd(data=siber_BB, x=1, y=1)
Ellipse_BB_Oser = pullEllipseStd(data=siber_BB, x=1, y=2)
Ellipse_BB_Obor = pullEllipseStd(data=siber_BB, x=1, y=3)


#Plot points and ellipses in each region
(Niche_CAA <- ggplot(Nichedata_CAA, aes(x=iso1, y=iso2))+
    geom_point(aes(col=factor(group)), size=2) +
    scale_y_continuous(name = "d15N",limits = c(9,15.2), breaks = c(9,10,11,12,13,14,15),
                       labels = c("9","","11","","13","","15")) +
    scale_x_continuous(name = "d13C", limits = c(-25,-17), breaks = c(-25,-24,-23,-22,-21,-20,-19,-18,-17),
                       labels = c("","-24","","-22","","-20","","-18","")) +
    scale_colour_manual(name = NULL, values = c("#4DAF4A","#E41A1C","#FF7F00"),
                        labels = c("O. bidentata", "O. sericeum", "O. borealis")) +
    geom_polygon(data=Ellipse_CAA_Obid,aes(x=V1, y=V2),colour="#4DAF4A",size=1.25, fill="#4DAF4A", alpha=0.4)+
    geom_polygon(data=Ellipse_CAA_Oser,aes(x=V1, y=V2),colour="#E41A1C",size=1.25, fill="#E41A1C", alpha=0.4)+
    geom_polygon(data=Ellipse_CAA_Obor,aes(x=V1, y=V2),colour="#FF7F00",size=1.25, fill="#FF7F00", alpha=0.4)+
    ggtitle(label = "A) Canadian Arctic Archipelago") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(legend.position="bottom"))

(Niche_NOW <- ggplot(Nichedata_NOW, aes(x=iso1, y=iso2))+
    geom_point(aes(col=factor(group)), size=2) +
    scale_y_continuous(name = "d15N",limits = c(9,15.2), breaks = c(9,10,11,12,13,14,15),
                       labels = c("9","","11","","13","","15")) +
    scale_x_continuous(name = "d13C", limits = c(-25,-17), breaks = c(-25,-24,-23,-22,-21,-20,-19,-18,-17),
                       labels = c("","-24","","-22","","-20","","-18","")) +
    scale_colour_manual(name = NULL, values = c("#4DAF4A","#E41A1C","#FF7F00"),
                        labels = c("O. bidentata", "O. sericeum", "O. borealis")) +
    geom_polygon(data=Ellipse_NOW_Obid,aes(x=V1, y=V2),colour="#4DAF4A",size=1.25, fill="#4DAF4A", alpha=0.4)+
    geom_polygon(data=Ellipse_NOW_Oser,aes(x=V1, y=V2),colour="#E41A1C",size=1.25, fill="#E41A1C", alpha=0.4)+
    geom_polygon(data=Ellipse_NOW_Obor,aes(x=V1, y=V2),colour="#FF7F00",size=1.25, fill="#FF7F00", alpha=0.4)+
    ggtitle(label = "B) North Water Polynya") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(legend.position="bottom"))

(Niche_BB <- ggplot(Nichedata_BB, aes(x=iso1, y=iso2))+
    geom_point(aes(col=factor(group)), size=2) +
    scale_y_continuous(name = "d15N",limits = c(9,15.2), breaks = c(9,10,11,12,13,14,15),
                       labels = c("9","","11","","13","","15")) +
    scale_x_continuous(name = "d13C", limits = c(-25,-17), breaks = c(-25,-24,-23,-22,-21,-20,-19,-18,-17),
                       labels = c("","-24","","-22","","-20","","-18","")) +
    scale_colour_manual(name = NULL, values = c("#4DAF4A","#E41A1C","#FF7F00"),
                        labels = c("O. bidentata", "O. sericeum", "O. borealis")) +
    geom_polygon(data=Ellipse_BB_Obid,aes(x=V1, y=V2),colour="#4DAF4A",size=1.25, fill="#4DAF4A", alpha=0.4)+
    geom_polygon(data=Ellipse_BB_Oser,aes(x=V1, y=V2),colour="#E41A1C",size=1.25, fill="#E41A1C", alpha=0.4)+
    geom_polygon(data=Ellipse_BB_Obor,aes(x=V1, y=V2),colour="#FF7F00",size=1.25, fill="#FF7F00", alpha=0.4)+
    ggtitle(label = "C) Baffin Bay") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(legend.position="bottom"))


#Join that as panels of a larger figure 
(plot_niches <- plot_grid(Niche_CAA, Niche_NOW, Niche_BB, nrow = 3))


#Export the graph to powerpoint for easy tweaking of small details and integration into manuscript document
#Please note that some characters are not supported (hence the lack of delta and per mille in axis titles)
plot_niches_export <- dml(ggobj = plot_niches)

read_pptx() %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value=plot_niches_export, location = ph_location(width=5, height=12, type = "body") ) %>%
  print(target = "Fig5_raw.pptx") %>%
  invisible()


#Compute overlap between different species in each region and express it as a fraction of total niche space
Overlap_CAA_ObidOser <- maxLikOverlap("1.1", "1.2", siber_CAA, p.interval = NULL, n = 100)
Overlap_CAA_ObidOser_rel <- Overlap_CAA_ObidOser[3]/(Overlap_CAA_ObidOser[1]+Overlap_CAA_ObidOser[2]-Overlap_CAA_ObidOser[3])

Overlap_CAA_ObidObor <- maxLikOverlap("1.1", "1.3", siber_CAA, p.interval = NULL, n = 100)
Overlap_CAA_ObidObor_rel <- Overlap_CAA_ObidObor[3]/(Overlap_CAA_ObidObor[1]+Overlap_CAA_ObidObor[2]-Overlap_CAA_ObidObor[3])

Overlap_CAA_OserObor <- maxLikOverlap("1.2", "1.3", siber_CAA, p.interval = NULL, n = 100)
Overlap_CAA_OserObor_rel <- Overlap_CAA_OserObor[3]/(Overlap_CAA_OserObor[1]+Overlap_CAA_OserObor[2]-Overlap_CAA_OserObor[3])

Overlap_NOW_ObidOser <- maxLikOverlap("2.1", "2.2", siber_NOW, p.interval = NULL, n = 100)
Overlap_NOW_ObidOser_rel <- Overlap_NOW_ObidOser[3]/(Overlap_NOW_ObidOser[1]+Overlap_NOW_ObidOser[2]-Overlap_NOW_ObidOser[3])

Overlap_NOW_ObidObor <- maxLikOverlap("2.1", "2.3", siber_NOW, p.interval = NULL, n = 100)
Overlap_NOW_ObidObor_rel <- Overlap_NOW_ObidObor[3]/(Overlap_NOW_ObidObor[1]+Overlap_NOW_ObidObor[2]-Overlap_NOW_ObidObor[3])

Overlap_NOW_OserObor <- maxLikOverlap("2.2", "2.3", siber_NOW, p.interval = NULL, n = 100)
Overlap_NOW_OserObor_rel <- Overlap_NOW_OserObor[3]/(Overlap_NOW_OserObor[1]+Overlap_NOW_OserObor[2]-Overlap_NOW_OserObor[3])

Overlap_BB_ObidOser <- maxLikOverlap("3.1", "3.2", siber_BB, p.interval = NULL, n = 100)
Overlap_BB_ObidOser_rel <- Overlap_BB_ObidOser[3]/(Overlap_BB_ObidOser[1]+Overlap_BB_ObidOser[2]-Overlap_BB_ObidOser[3])

Overlap_BB_ObidObor <- maxLikOverlap("3.1", "3.3", siber_BB, p.interval = NULL, n = 100)
Overlap_BB_ObidObor_rel <- Overlap_BB_ObidObor[3]/(Overlap_BB_ObidObor[1]+Overlap_BB_ObidObor[2]-Overlap_BB_ObidObor[3])

Overlap_BB_OserObor <- maxLikOverlap("3.2", "3.3", siber_BB, p.interval = NULL, n = 100)
Overlap_BB_OserObor_rel <- Overlap_BB_OserObor[3]/(Overlap_BB_OserObor[1]+Overlap_BB_OserObor[2]-Overlap_BB_OserObor[3])