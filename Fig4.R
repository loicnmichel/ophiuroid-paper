#Load necessary packages
library(SIBER)
library(dplyr)
library(ggplot2)
library(officer)
library(rvg)
library(devEMF)
library(cowplot)


#Read in the data
Nichedata_full <- read.csv("Fig4_data.csv", header=T, sep=";")


#Extract the data for each zone
Nichedata_CAA <- Nichedata_full %>% filter(Zone == "Canadian Arctic Archipelago")

Nichedata_NOW <- Nichedata_full %>% filter(Zone == "North Water Polynia")

Nichedata_BB <- Nichedata_full %>% filter(Zone == "Baffin Bay")


#Format data from each zone for use by SIBER
siber_CAA <- createSiberObject(Nichedata_CAA[,3:6])

siber_NOW <- createSiberObject(Nichedata_NOW[,3:6])

siber_BB <- createSiberObject(Nichedata_BB[,3:6])


#Define parameters and priors for Bayesian estimation of SEA
parms <- list()
parms$n.iter <- 1 * 10^6
parms$n.burnin <- 1 * 10^5
parms$n.thin <- 10
parms$n.chains <- 2

priors <- list()
priors$R <- 1 * diag(2)
priors$k <- 2
priors$tau.mu <- 1.0E-3


#Run the Bayesian model for each zone, then extract posterior estimates of SEA
BayesEllipses_CAA <- siberMVN(siber_CAA, parms, priors)
SEAB_CAA <- siberEllipses(BayesEllipses_CAA)

BayesEllipses_NOW <- siberMVN(siber_NOW, parms, priors)
SEAB_NOW <- siberEllipses(BayesEllipses_NOW)

BayesEllipses_BB <- siberMVN(siber_BB, parms, priors)
SEAB_BB <- siberEllipses(BayesEllipses_BB)


#Save our SEA estimates as R data objects
save(SEAB_CAA, file="SEAB_CAA.rdata")

save(SEAB_NOW, file="SEAB_NOW.rdata")

save(SEAB_BB, file="SEAB_BB.rdata")


#Create data frames for ggplot visualization
SEAB_CAA_data <- (c(SEAB_CAA[,1],
                    SEAB_CAA[,2],
                    SEAB_CAA[,3]))

SEAB_NOW_data <- (c(SEAB_NOW[,1],
                    SEAB_NOW[,2],
                    SEAB_NOW[,3]))

SEAB_BB_data <- (c(SEAB_BB[,1],
                   SEAB_BB[,2],
                   SEAB_BB[,3]))

Species <- (c(rep("Ophiacantha bidentata", length(SEAB_CAA[,1])),
              rep("Ophiocten sericeum", length(SEAB_CAA[,2])),
              rep("Ophiopleura borealis", length(SEAB_CAA[,3]))))

SEA_CAA <- data.frame(Species, SEAB_CAA_data)

SEA_NOW <- data.frame(Species, SEAB_NOW_data)

SEA_BB <- data.frame(Species, SEAB_BB_data)


#Plot SEA for each region
(boxplot_SEA_CAA <- ggplot(SEA_CAA, aes (y = SEAB_CAA_data, x = Species, colour = Species, fill = Species)) +
    geom_boxplot(alpha = 0.6, outlier.shape=NA) +
    scale_y_continuous(name = "Standard Ellipse Area (pm^2)", limits = c(0,6)) +
    scale_x_discrete(name = NULL) +
    scale_colour_manual(values = c("#4DAF4A","#E41A1C","#FF7F00")) +
    scale_fill_manual(values = c("#4DAF4A","#E41A1C","#FF7F00")) +
    ggtitle(label = "A) Canadian Arctic Archipelago") +
    theme_bw() +
    theme(legend.position="none") +
    theme(panel.grid.major.x = element_blank()))

(boxplot_SEA_NOW <- ggplot(SEA_NOW, aes (y = SEAB_NOW_data, x = Species, colour = Species, fill = Species)) +
    geom_boxplot(alpha = 0.6, outlier.shape=NA) +
    scale_y_continuous(name = "Standard Ellipse Area (pm^2)", limits = c(0,6)) +
    scale_x_discrete(name = NULL) +
    scale_colour_manual(values = c("#4DAF4A","#E41A1C","#FF7F00")) +
    scale_fill_manual(values = c("#4DAF4A","#E41A1C","#FF7F00")) +
    ggtitle(label = "B) North Water Polynya") +
    theme_bw() +
    theme(legend.position="none") +
    theme(panel.grid.major.x = element_blank()))

(boxplot_SEA_BB <- ggplot(SEA_BB, aes (y = SEAB_BB_data, x = Species, colour = Species, fill = Species)) +
    geom_boxplot(alpha = 0.6, outlier.shape=NA) +
    scale_y_continuous(name = "Standard Ellipse Area (pm^2)", limits = c(0,6)) +
    scale_x_discrete(name = NULL) +
    scale_colour_manual(values = c("#4DAF4A","#E41A1C","#FF7F00")) +
    scale_fill_manual(values = c("#4DAF4A","#E41A1C","#FF7F00")) +
    ggtitle(label = "C) Baffin Bay") +
    theme_bw() +
    theme(legend.position="none") +
    theme(panel.grid.major.x = element_blank()))


#Join that as panels of a larger figure 
(plot_SEA <- plot_grid(boxplot_SEA_CAA, boxplot_SEA_NOW, boxplot_SEA_BB, nrow = 3))


#Export the larger graph to powerpoint for easy tweaking of small details and integration into manuscript document
#Please note that some characters are not supported (hence the lack of per mille in axis titles)

plot_SEA_export <- dml(ggobj = plot_SEA)

read_pptx() %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value=plot_SEA_export, location = ph_location(width=5, height=9, type = "body") ) %>%
  print(target = "Fig4_raw.pptx") %>%
  invisible()


#Compute probabilities that species have different standard ellipse areas in each zone
Probdiff_CAA_ObidOser <- sum(SEAB_CAA[,1]>SEAB_CAA[,2])/ NROW(SEAB_CAA[,1])
Probdiff_CAA_ObidObor <- sum(SEAB_CAA[,1]>SEAB_CAA[,3])/ NROW(SEAB_CAA[,1])
Probdiff_CAA_OserObor <- sum(SEAB_CAA[,2]>SEAB_CAA[,3])/ NROW(SEAB_CAA[,2])

Probdiff_NOW_ObidOser <- sum(SEAB_NOW[,1]>SEAB_NOW[,2])/ NROW(SEAB_NOW[,1])
Probdiff_NOW_ObidObor <- sum(SEAB_NOW[,1]>SEAB_NOW[,3])/ NROW(SEAB_NOW[,1])
Probdiff_NOW_OserObor <- sum(SEAB_NOW[,2]>SEAB_NOW[,3])/ NROW(SEAB_NOW[,2])

Probdiff_BB_ObidOser <- sum(SEAB_BB[,1]>SEAB_BB[,2])/ NROW(SEAB_BB[,1])
Probdiff_BB_ObidObor <- sum(SEAB_BB[,1]>SEAB_BB[,3])/ NROW(SEAB_BB[,1])
Probdiff_BB_OserObor <- sum(SEAB_BB[,2]>SEAB_BB[,3])/ NROW(SEAB_BB[,2])