# Song_etal_methane_hydroLAKES_bySize_0C_isotherm.R
# by George Allen, August 2019


library(foreign)


# made a symbolic link in cmd (run as admin in windows): 
# > mklink /D E:\research\2019_10_08_Global_Carbon_Project\git\in\GISout E:\research\2019_10_08_Global_Carbon_Project\GIS\out

################################################################################
# Read in CSV
################################################################################

# set working dir:
wDir = "E:/research/2019_10_08_Global_Carbon_Project/git"

# read in hydroLakes:
inPath = paste0(wDir, "/in/GISout/hydroLAKES/0C_isotherm_reg/hydroLAKES_0C_isotherm_Hanquin_regions.dbf")
outPath = paste0(wDir, "/out/hydroLakes_0C_isotherm_Hanquin_regions/hydroLakes_0C_isotherm_Hanquin_regions.csv")
hL = read.dbf(inPath)



################################################################################
# Ice-free conditions:
################################################################################

# total surface area: 
sum_lrArea = sum(hL$Lake_area)
sum_rArea = sum(hL$Lake_area[hL$Lake_type != 1])
sum_lArea = sum(hL$Lake_area[hL$Lake_type == 1])

sum_lrArea
sum_rArea/sum_lrArea
sum_lArea/sum_lrArea


# small waterbodies with areas less than 1km2:
sum_lrArea_sm = sum(hL$Lake_area[hL$Lake_area<1])
sum_rArea_sm = sum(hL$Lake_area[hL$Lake_type != 1 & hL$Lake_area<1])
sum_lArea_sm = sum(hL$Lake_area[hL$Lake_type == 1  & hL$Lake_area<1])

sum_lrArea_sm/sum_lrArea
sum_rArea_sm/sum_lrArea
sum_lArea_sm/sum_lrArea


# large waterbodies with areas equal or greater than 1km2:
sum_lrArea_lg = sum(hL$Lake_area[hL$Lake_area>=1])
sum_rArea_lg = sum(hL$Lake_area[hL$Lake_type != 1 & hL$Lake_area>=1])
sum_lArea_lg = sum(hL$Lake_area[hL$Lake_type == 1  & hL$Lake_area>=1])

sum_lrArea_lg/sum_lrArea
sum_rArea_lg/sum_lrArea
sum_lArea_lg/sum_lrArea



################################################################################
# Ice corrected:
################################################################################


# ice correction:
iceCor = 1 - hL$iceOccur

# total surface area: 
sum_lrArea_ice = sum(hL$Lake_area*iceCor)
sum_rArea_ice = sum((hL$Lake_area*iceCor)[hL$Lake_type != 1])
sum_lArea_ice = sum((hL$Lake_area*iceCor)[hL$Lake_type == 1])

sum_lrArea_ice/sum_lrArea
sum_rArea_ice/sum_rArea
sum_lArea_ice/sum_lArea


# small waterbodies with areas less than 1km2:
sum_lrArea_sm_ice = sum((hL$Lake_area*iceCor)[hL$Lake_area<1])
sum_rArea_sm_ice = sum((hL$Lake_area*iceCor)[hL$Lake_type != 1 & hL$Lake_area<1])
sum_lArea_sm_ice = sum((hL$Lake_area*iceCor)[hL$Lake_type == 1  & hL$Lake_area<1])

sum_lrArea_sm_ice/sum_lrArea_sm
sum_rArea_sm_ice/sum_rArea_sm
sum_lArea_sm_ice/sum_lArea_sm


# large waterbodies with areas equal or greater than 1km2:
sum_lrArea_lg_ice = sum((hL$Lake_area*iceCor)[hL$Lake_area>=1])
sum_rArea_lg_ice = sum((hL$Lake_area*iceCor)[hL$Lake_type != 1 & hL$Lake_area>=1])
sum_lArea_lg_ice = sum((hL$Lake_area*iceCor)[hL$Lake_type == 1  & hL$Lake_area>=1])

sum_lrArea_lg_ice/sum_lrArea_lg
sum_rArea_lg_ice/sum_rArea_lg
sum_lArea_lg_ice/sum_lArea_lg


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

iceMeltCorrection_km2 = c(
  round(sum_lrArea_ice_melt, 4),
  round(sum_lArea_ice_melt, 4),
  round(sum_lArea_lg_ice_melt, 4),
  round(sum_lArea_sm_ice_melt, 4),
  round(sum_rArea_ice_melt, 4),
  round(sum_rArea_lg_ice_melt, 4),
  round(sum_rArea_sm_ice_melt, 4)
)




oTab = cbind(parameter, IceFree_km2, iceCorrected_km2, iceMeltCorrection, combinedCorrectionFactor)

# plot:
barplot(rbind(IceFree_km2, iceCorrected_km2, iceMeltCorrection_km2), names.arg=parameter, beside=T, 
        main="0C Isotherm", ylab="Area (km2)", col=c("darkgray", "white",  "light blue"))
legend("topright", c("no ice correction", "ice corrected", "melt corrected"), fill=c("darkgray", "white", "light blue"))


# write out
write.csv(oTab, outPath, row.names=F)

system(paste('open', outPath))


