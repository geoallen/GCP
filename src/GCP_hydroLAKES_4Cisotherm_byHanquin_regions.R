# Song_etal_methane_hydroLAKES_bySize_0C_isotherm.R
# by George Allen, August 2019


library(foreign)


# made a symbolic link in cmd (run as admin in windows): 
# > mklink /D E:\research\2019_10_08_Global_Carbon_Project\git\in\GISout E:\research\2019_10_08_Global_Carbon_Project\GIS\out

################################################################################
# Read in CSV
################################################################################

# set working dir:
wDir = "E:/research/2019_10_08_Global_Carbon_Project/git/GCP"

# read in hydroLakes:
inPath = paste0(wDir, "/in/GISout/hydroLAKES/0C_isotherm_reg/hydroLAKES_0C_isotherm_Hanquin_regions.dbf")
outDir = paste0(wDir, "/out")
outPathDir = paste0(outDir, "/hydroLakes_0C_isotherm_Hanquin_regions")
hL_full = read.dbf(inPath)


iceCorrection <- function(hL){
  ################################################################################
  # Ice-free conditions:
  ################################################################################
  
  # total surface area: 
  sum_lrArea = sum(hL$Lake_area)
  sum_rArea = sum(hL$Lake_area[hL$Lake_type != 1])
  sum_lArea = sum(hL$Lake_area[hL$Lake_type == 1])
  
  # small waterbodies with areas less than 1km2:
  sum_lrArea_sm = sum(hL$Lake_area[hL$Lake_area<1])
  sum_rArea_sm = sum(hL$Lake_area[hL$Lake_type != 1 & hL$Lake_area<1])
  sum_lArea_sm = sum(hL$Lake_area[hL$Lake_type == 1  & hL$Lake_area<1])
  
  # large waterbodies with areas equal or greater than 1km2:
  sum_lrArea_lg = sum(hL$Lake_area[hL$Lake_area>=1])
  sum_rArea_lg = sum(hL$Lake_area[hL$Lake_type != 1 & hL$Lake_area>=1])
  sum_lArea_lg = sum(hL$Lake_area[hL$Lake_type == 1  & hL$Lake_area>=1])
  
  
  ################################################################################
  # Ice corrected:
  ################################################################################
  # ice correction:
  iceCor = 1 - hL$iceOccur
  
  # total surface area: 
  sum_lrArea_ice = sum(hL$Lake_area*iceCor)
  sum_rArea_ice = sum((hL$Lake_area*iceCor)[hL$Lake_type != 1])
  sum_lArea_ice = sum((hL$Lake_area*iceCor)[hL$Lake_type == 1])
  
  # small waterbodies with areas less than 1km2:
  sum_lrArea_sm_ice = sum((hL$Lake_area*iceCor)[hL$Lake_area<1])
  sum_rArea_sm_ice = sum((hL$Lake_area*iceCor)[hL$Lake_type != 1 & hL$Lake_area<1])
  sum_lArea_sm_ice = sum((hL$Lake_area*iceCor)[hL$Lake_type == 1  & hL$Lake_area<1])
  
  # large waterbodies with areas equal or greater than 1km2:
  sum_lrArea_lg_ice = sum((hL$Lake_area*iceCor)[hL$Lake_area>=1])
  sum_rArea_lg_ice = sum((hL$Lake_area*iceCor)[hL$Lake_type != 1 & hL$Lake_area>=1])
  sum_lArea_lg_ice = sum((hL$Lake_area*iceCor)[hL$Lake_type == 1  & hL$Lake_area>=1])
  
  
  ################################################################################
  # Ice-melt corrected:
  ################################################################################
  # For lakes that freeze (iceBoo==1), add 27% of ice corrected area to area:
  
  # ice-melt correction:
  iceBoo = hL$iceOccur > 0
  meltCor =0.27
  r = hL$Lake_type != 1 
  l = hL$Lake_type == 1
  sm = hL$Lake_area<1
  lg = hL$Lake_area>=1
  
  
  # total surface area:
  sum_lrArea_ice_melt = sum_lrArea_ice + sum((hL$Lake_area*iceCor)[iceBoo])*meltCor
  sum_rArea_ice_melt = sum_rArea_ice + sum((hL$Lake_area*iceCor)[r & iceBoo])*meltCor
  sum_lArea_ice_melt = sum_lArea_ice + sum((hL$Lake_area*iceCor)[l & iceBoo])*meltCor
  
  sum_lrArea_ice_melt_perc = sum_lrArea_ice_melt/sum_lrArea_ice - 1
  sum_rArea_ice_melt_perc = sum_rArea_ice_melt/sum_rArea_ice - 1
  sum_lArea_ice_melt_perc = sum_lArea_ice_melt/sum_lArea_ice - 1
  
  sum_lrArea_ice_melt_corFactor = sum_lrArea_ice_melt/sum_lrArea
  sum_rArea_ice_melt_corFactor = sum_rArea_ice_melt/sum_rArea
  sum_lArea_ice_melt_corFactor = sum_lArea_ice_melt/sum_lArea
  
  
  # small waterbodies with areas less than 1km2:
  sum_lrArea_sm_ice_melt = sum_lrArea_sm_ice + sum((hL$Lake_area*iceCor)[sm & iceBoo])*meltCor
  sum_rArea_sm_ice_melt = sum_rArea_sm_ice + sum((hL$Lake_area*iceCor)[sm & r & iceBoo])*meltCor
  sum_lArea_sm_ice_melt = sum_lArea_sm_ice + sum((hL$Lake_area*iceCor)[sm & l & iceBoo])*meltCor
  
  sum_lrArea_sm_ice_melt_perc = sum_lrArea_sm_ice_melt/sum_lrArea_sm_ice - 1
  sum_rArea_sm_ice_melt_perc = sum_rArea_sm_ice_melt/sum_rArea_sm_ice - 1
  sum_lArea_sm_ice_melt_perc = sum_lArea_sm_ice_melt/sum_lArea_sm_ice - 1
  
  sum_lrArea_sm_ice_melt_corFactor = sum_lrArea_sm_ice_melt/sum_lrArea_sm
  sum_rArea_sm_ice_melt_corFactor = sum_rArea_sm_ice_melt/sum_rArea_sm
  sum_lArea_sm_ice_melt_corFactor = sum_lArea_sm_ice_melt/sum_lArea_sm
  
  
  # large waterbodies with areas equal or greater than 1km2:
  sum_lrArea_lg_ice_melt = sum_lrArea_lg_ice + sum((hL$Lake_area*iceCor)[lg & iceBoo])*meltCor
  sum_rArea_lg_ice_melt = sum_rArea_lg_ice + sum((hL$Lake_area*iceCor)[lg & r & iceBoo])*meltCor
  sum_lArea_lg_ice_melt = sum_lArea_lg_ice + sum((hL$Lake_area*iceCor)[lg & l & iceBoo])*meltCor
  
  sum_lrArea_lg_ice_melt_perc = sum_lrArea_lg_ice_melt/sum_lrArea_lg_ice - 1
  sum_rArea_lg_ice_melt_perc = sum_rArea_lg_ice_melt/sum_rArea_lg_ice - 1
  sum_lArea_lg_ice_melt_perc = sum_lArea_lg_ice_melt/sum_lArea_lg_ice - 1
  
  sum_lrArea_lg_ice_melt_corFactor = sum_lrArea_lg_ice_melt/sum_lrArea_lg
  sum_rArea_lg_ice_melt_corFactor = sum_rArea_lg_ice_melt/sum_rArea_lg
  sum_lArea_lg_ice_melt_corFactor = sum_lArea_lg_ice_melt/sum_lArea_lg
  
  ################################################################################
  # Add to table:
  ################################################################################
  
  # make a table:
  parameter = c(
    "sum total",
    "sum lake",
    "sum large lake",
    "sum small lake",
    "sum reservoir",
    "sum large reservoir",
    "sum small reservoir"
  )
  
  IceFree_km2 = c(
    round(sum_lrArea),
    round(sum_lArea),
    round(sum_lArea_lg),
    round(sum_lArea_sm),
    round(sum_rArea),
    round(sum_rArea_lg),
    round(sum_rArea_sm)
  )
  
  iceCorrected_km2 = c(
    round(sum_lrArea_ice),
    round(sum_lArea_ice),
    round(sum_lArea_lg_ice),
    round(sum_lArea_sm_ice),
    round(sum_rArea_ice),
    round(sum_rArea_lg_ice),
    round(sum_rArea_sm_ice)
  )
  
  iceMeltCorrected_km2 = c(
    round(sum_lrArea_ice_melt, 4),
    round(sum_lArea_ice_melt, 4),
    round(sum_lArea_lg_ice_melt, 4),
    round(sum_lArea_sm_ice_melt, 4),
    round(sum_rArea_ice_melt, 4),
    round(sum_rArea_lg_ice_melt, 4),
    round(sum_rArea_sm_ice_melt, 4)
  )
  
  iceMeltCorrection = c(
    round(sum_lrArea_ice_melt_perc, 4),
    round(sum_lArea_ice_melt_perc, 4),
    round(sum_lArea_lg_ice_melt_perc, 4),
    round(sum_lArea_sm_ice_melt_perc, 4),
    round(sum_rArea_ice_melt_perc, 4),
    round(sum_rArea_lg_ice_melt_perc, 4),
    round(sum_rArea_sm_ice_melt_perc, 4)
  )
  
  combinedCorrectionFactor = c(
    round(sum_lrArea_ice_melt_corFactor, 4),
    round(sum_lArea_ice_melt_corFactor, 4),
    round(sum_lArea_lg_ice_melt_corFactor, 4),
    round(sum_lArea_sm_ice_melt_corFactor, 4),
    round(sum_rArea_ice_melt_corFactor, 4),
    round(sum_rArea_lg_ice_melt_corFactor, 4),
    round(sum_rArea_sm_ice_melt_corFactor, 4)
  )
  
  # plot:
  barplot(rbind(IceFree_km2, iceCorrected_km2, iceMeltCorrected_km2), names.arg=parameter, beside=T, 
          main=paste("Hanquin Region:", uniqReg[i]), ylab="Area (km2)", col=c("darkgray", "white",  "light blue"))
  legend("topright", c("no ice correction", "ice corrected", "melt corrected"), fill=c("darkgray", "white", "light blue"))
  mtext("0C Isotherm")
  
  # put into a table:
  oTab = as.data.frame(cbind(parameter, IceFree_km2, iceCorrected_km2, iceMeltCorrected_km2, combinedCorrectionFactor))
  
  return(oTab)
  
}


################################################################################
# For Each Hanquin Region:
################################################################################

names(hL_full)[names(hL_full) == "gridcode"] = "reg"
uniqReg = sort(unique(hL_full$reg))

outPaths = paste0(outPathDir, "/hydroLakes_0C_isotherm_Hanquin_region_", uniqReg, ".csv")

pdfPath = paste0(outDir, "/fig/hydroLakes_0C_isotherm_Hanquin_regions.pdf")
pdf(pdfPath, 15, 10)

for (i in 1:length(uniqReg)){
  regInd = which(hL_full$reg==uniqReg[i])
  hL = hL_full[regInd,]
  
  oTab = iceCorrection(hL)
  
  # add table to one big output table:
  # if (i == 1){
  #   oTab = cbind(tab, hanquin_region = rep(uniqReg[i], nrow(tab)))
  # } else {
  #   oTab = rbind(oTab, cbind(tab, hanquin_region = rep(uniqReg[i], nrow(tab))))
  # }

  # write out
  write.csv(oTab, outPaths[i], row.names=F)
  
}


dev.off()
system(paste('open', pdfPath))




