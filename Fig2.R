#Load necessary packages
library(tRophicPosition)
library(dplyr)
library(ggplot2)
library(officer)
library(rvg)
library(magrittr)
library(devEMF)
library(cowplot)


#Read in the data
TPdata_full <- read.csv("Fig2_data.csv", header=T, sep=";")


#Extract the data for each zone
TPdata_BB <- TPdata_full %>% filter(Zone == "Baffin Bay")

TPdata_CAA <- TPdata_full %>% filter(Zone == "Canadian Arctic Archipelago")

TPdata_NOW <- TPdata_full %>% filter(Zone == "North Water Polynia")


#Get McCutchan et al. TEFs
TEFs <- TDF(author = "McCutchan", element = "both", type = "all")


#Format data from each zone for use by tRophicPosition
IsoData_BB <- extractIsotopeData(TPdata_BB,
                                 b1 = "Sediment", 
                                 baselineColumn = "Item",
                                 consumersColumn = "Item",
                                 groupsColumn = "Species",
                                 deltaN = TEFs$deltaN,
                                 deltaC = TEFs$deltaC,
                                 d13C = "d13C", 
                                 d15N = "d15N")

IsoData_CAA <- extractIsotopeData(TPdata_CAA,
                                 b1 = "Sediment", 
                                 baselineColumn = "Item",
                                 consumersColumn = "Item",
                                 groupsColumn = "Species",
                                 deltaN = TEFs$deltaN,
                                 deltaC = TEFs$deltaC,
                                 d13C = "d13C", 
                                 d15N = "d15N")

IsoData_NOW <- extractIsotopeData(TPdata_NOW,
                                 b1 = "Sediment", 
                                 baselineColumn = "Item",
                                 consumersColumn = "Item",
                                 groupsColumn = "Species",
                                 deltaN = TEFs$deltaN,
                                 deltaC = TEFs$deltaC,
                                 d13C = "d13C", 
                                 d15N = "d15N")


#Run single baseline trophic position model for each zone
model_BB <- multiSpeciesTP(IsoData_BB,
                           model = "oneBaseline",
                           lambda = 1,
                           n.adapt = 500000, n.iter = 500000,
                           burnin = 50000, n.chains = 2, print = FALSE)

model_CAA <- multiSpeciesTP(IsoData_CAA,
                            model = "oneBaseline",
                            lambda = 1,
                            n.adapt = 500000, n.iter = 500000,
                            burnin = 50000, n.chains = 2, print = FALSE)

model_NOW <- multiSpeciesTP(IsoData_NOW,
                            model = "oneBaseline",
                            lambda = 1,
                            n.adapt = 500000, n.iter = 500000,
                            burnin = 50000, n.chains = 2, print = FALSE)


#Save models as R objects
save(model_BB, file="TP_model_BB.rdata")
save(model_CAA, file="TP_model_CAA.rdata")
save(model_NOW, file="TP_model_NOW.rdata")


#Create data frames for ggplot visualization
TP_BB_data <- c(model_BB$TPs$O.bidentata.O.bidentata.1b,
                model_BB$TPs$O.sericeum.O.sericeum.1b,
                model_BB$TPs$O.borealis..O.borealis..1b)

TP_CAA_data <- (c(model_CAA$TPs$O.bidentata.O.bidentata.1b,
                  model_CAA$TPs$O.sericeum.O.sericeum.1b,
                  model_CAA$TPs$O.borealis..O.borealis..1b))

TP_NOW_data <- (c(model_NOW$TPs$O.bidentata.O.bidentata.1b,
                  model_NOW$TPs$O.sericeum.O.sericeum.1b,
                  model_NOW$TPs$O.borealis..O.borealis..1b))

Species <- (c(rep("Ophiacantha bidentata", length(model_BB$TPs$O.bidentata.O.bidentata.1b)),
              rep("Ophiocten sericeum", length(model_BB$TPs$O.sericeum.O.sericeum.1b)),
              rep("Ophiopleura borealis", length(model_BB$TPs$O.borealis..O.borealis..1b))))

TP_BB <- data.frame(Species, TP_BB_data)

TP_CAA <- data.frame(Species, TP_CAA_data)

TP_NOW <- data.frame(Species, TP_NOW_data)


#Plot TP for each region
(boxplot_TP_CAA <- ggplot(TP_CAA, aes (y = TP_CAA_data, x = Species, colour = Species, fill = Species)) +
    geom_boxplot(alpha = 0.6, outlier.shape=NA) +
    scale_y_continuous(name = "Trophic Position", limits = c(2,5)) +
    scale_x_discrete(name = NULL) +
    scale_colour_manual(values = c("#4DAF4A","#E41A1C","#FF7F00")) +
    scale_fill_manual(values = c("#4DAF4A","#E41A1C","#FF7F00")) +
    ggtitle(label = "A) Canadian Arctic Archipelago") +
    theme_bw() +
    theme(legend.position="none") +
    theme(panel.grid.major.x = element_blank()))

(boxplot_TP_NOW <- ggplot(TP_NOW, aes (y = TP_NOW_data, x = Species, colour = Species, fill = Species)) +
    geom_boxplot(alpha = 0.6, outlier.shape=NA) +
    scale_y_continuous(name = "Trophic Position", limits = c(2,5)) +
    scale_x_discrete(name = NULL) +
    scale_colour_manual(values = c("#4DAF4A","#E41A1C","#FF7F00")) +
    scale_fill_manual(values = c("#4DAF4A","#E41A1C","#FF7F00")) +
    ggtitle(label = "B) North Water Polynya") +
    theme_bw() +
    theme(legend.position="none") +
    theme(panel.grid.major.x = element_blank()))

(boxplot_TP_BB <- ggplot(TP_BB, aes (y = TP_BB_data, x = Species, colour = Species, fill = Species)) +
    geom_boxplot(alpha = 0.6, outlier.shape=NA) +
    scale_y_continuous(name = "Trophic Position", limits = c(2,5)) +
    scale_x_discrete(name = NULL) +
    scale_colour_manual(values = c("#4DAF4A","#E41A1C","#FF7F00")) +
    scale_fill_manual(values = c("#4DAF4A","#E41A1C","#FF7F00")) +
    ggtitle(label = "C) Baffin Bay") +
    theme_bw() +
    theme(legend.position="none") +
    theme(panel.grid.major.x = element_blank()))


#Join that as panels of a larger figure 
(plot_TP <- plot_grid(boxplot_TP_CAA, boxplot_TP_NOW, boxplot_TP_BB, nrow = 3))


#Export the larger graph to powerpoint for easy tweaking of small details and integration into manuscript document
plot_TP_export <- dml(ggobj = plot_TP)

read_pptx() %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value=plot_TP_export, location = ph_location(width=5, height=9, type = "body") ) %>%
  print(target = "Fig2_raw.pptx") %>%
  invisible()


#Compute probabilities that species have different trophic positions in each zone
pairwiseTP_BB <- pairwiseComparisons(model_BB$TPs, print = TRUE)

pairwiseTP_CAA <- pairwiseComparisons(model_CAA$TPs, print = TRUE)

pairwiseTP_NOW <- pairwiseComparisons(model_NOW$TPs, print = TRUE)
