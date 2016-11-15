
#########################################################################################################################
#### Load and Install einiger wichtiger Packages  ##################################################################

loadandinstall <- function(mypkg) {if (!is.element(mypkg,
                                                   installed.packages()[,1])){install.packages(mypkg)}; library(mypkg,
                                                                                                                character.only=TRUE) }
print("Loading necessary Libraries")
loadandinstall("rgdal")
loadandinstall("raster")
loadandinstall("rgeos")
loadandinstall("stringr")
loadandinstall("car")


#########################################################################################################################
#### Einrichten des Arbeitspfades  ######################################################################################

Drive <- "G:/"
work_path <- "/R/L_Freytag/"
setwd(str_c(Drive,work_path))

#########################################################################################################################
####  display working progress      ##############################################################

rasterOptions(progress="text")        # Fortschrittsbalken wird angezeigt (einmal zu Beginn anwenden)

print(i)                              # jedes mal in der Schleife eingeben (i = jeweiliger Laufparameter)


####  öffnen eines neuen Fensters zum Ploten (speichern der Plots in mässiger qualität möglich)  #######################

x11()


#########################################################################################################################
####  Ersetzen der Dateinpf?de (?ffnen eines Suchfensters)  #############################################################

indir <- file.choose()   ### file.choose() anstatt des Pfades


#########################################################################################################################
####  "Curser value" & Zoom   ###########################################################################################

click()
zoom()

#########################################################################################################################
####  schlie?en des Plots

dev.off()


#########################################################################################################################
####  Plotten zweier Raster in einem Fenster mit: ,add=T)  ##############################################################

plot(Muster_Raster,add=T)

