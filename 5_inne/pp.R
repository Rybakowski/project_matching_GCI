library(PanelMatch)

DisplayTreatment(unit.id = "wbcode2",
					  time.id = "year", legend.position = "none",
					  xlab = "year", ylab = "Country Code",
					  treatment = "dem", data = dem)
data <- PanelMatch::dem

DisplayTreatment(unit.id = "wbcode2",
					  time.id = "year", legend.position = "none",
					  xlab = "year", ylab = "Country Code",
					  treatment = "dem", data = dem,
					  hide.x.tick.label = TRUE, hide.y.tick.label = TRUE,
					  dense.plot = TRUE) # setting dense.plot to TRUE
?PanelMatch
dem

#Call PanelMatch with refinement
PM.results.maha <- PanelMatch(lag = 4, time.id = "year", unit.id = "wbcode2",
										treatment = "dem", refinement.method = "mahalanobis",
										# use Mahalanobis distance
										data = dem, match.missing = TRUE,
										covs.formula = ~ tradewb,
										size.match = 6, qoi = "att" , outcome.var = "y",
										lead = 0:4, forbid.treatment.reversal = FALSE,
										use.diagonal.variance.matrix = TRUE)
msets.maha <- PM.results.maha$att
print(msets.maha)
msets.maha[[2]]


