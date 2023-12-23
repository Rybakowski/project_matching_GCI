setwd("C:/Users/rados/OneDrive - SGH/Nowy_projekt_BNK/Casual_Panel_analysis_train")
# install packages from CRAN
packages <- c("dplyr", "readstata13", "fixest", "did", "fect", 
				  "panelView", "PanelMatch", "ggplot2", "bacondecomp")
install.packages(setdiff(packages, rownames(installed.packages())))  

# install "paneltools"
if ("paneltools" %in% rownames(installed.packages()) == FALSE) {
	devtools:: install_github("xuyiqing/paneltools")
}
