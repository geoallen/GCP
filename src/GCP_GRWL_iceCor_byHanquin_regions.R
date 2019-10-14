################################################################################
# GCP_GRWL_iceCor_byHanquin_regions.R
################################################################################
# by George Allen, Oct 2019

# made a symbolic link in cmd (run as admin in windows): 
# >mklink /D target source


library(foreign)



################################################################################
# Hard-coded constants:

# set working dir:
wDir = "E:/research/2019_10_08_Global_Carbon_Project/git/GCP"

inDir = paste0(wDir, "/in")
outDir = paste0(wDir, "/out")
GRWLinDir = paste0(inDir, "/GISout/GRWL/4C_isotherm_hanquin_regions/")
fPaths = list.files(GRWLinDir, ".dbf", full.names=T)
fNames = list.files(GRWLinDir, ".dbf", full.names=F)

bigTabPath = paste0(outDir, "/GRWL_allData/GRWL_allData.csv")

latBandsIn = paste0(inDir, "/latBandsTab/latBands.csv")
outPath = paste0(outDir, "/GRWL_4C_isotherm_byHanquin_regions/GRWL_4C_isotherm_byHanquin_regions.csv")

# total RSSA in km2 from Allen & Pavelsky, 2018, Science:
GRWLTotal = 773000


################################################################################
# Functions:
################################################################################

#### for each degree latitude, calculate the RSSA value:
RSSAbyLat <- function(tab, degreeInterval, region){
  latMn = ceiling(min(tab$lat, na.rm=T))
  latMx = floor(max(tab$lat, na.rm=T))
  latSeq = seq(latMn, latMx, degreeInterval)
  
  RSSA = RSSA_ice = rep(NA, length(latSeq)-1)
  
  for (i in 1:length(latSeq)-1){
    
    # find which GRWL river measurements are located within the lat band, 
    # are above an elevation of 0 m, and not a lake or reservoir: 
    latInd = which(tab$lat > latSeq[i] & 
                     tab$lat < latSeq[i+1] & 
                     tab$elev_m>0 & 
                     tab$lakeFlag == 0)
    
    RSSA[i] = sum(tab$width[latInd]*36.2132, na.rm=T)*1e-6 # mean px length in km - this could be done outside for loop
    RSSA_ice[i] = sum(tab$width[latInd]*36.2132 * (1-tab$iceOccur[latInd]), na.rm=T)*1e-6
    
    print(paste("lat:", latSeq[i]))
    
  }
  
  lat = latSeq[-length(latSeq)]
  latTab = as.data.frame(cbind(lat, RSSA, RSSA_ice))
  
  # clip measurements outside of latitude range:
  latMn = ceiling(min(latTab$lat, na.rm=T))
  latMx = floor(max(latTab$lat, na.rm=T))
  latTab = latTab[latTab$lat>latMn & latTab$lat<latMx, ]
  
  # add a region column:
  latTab = data.frame(cbind(region, latTab))
  
  return(latTab)
  
}

#### plot by lat. band:
pdfplotter <- function(latTab, region, yMax){
  # Uncorrected and corrected observed surface area:
  # plot(latTab$lat, latTab$RSSA, type="l", col=rgb(0,0,0,0.8),
  #      xlab="Latitude",
  #       ylab="Sum RSSA per degree lat. (sq km)")
  # lines(latTab$lat, latTab$RSSA_ice, col=rgb(0,0,1,0.5))
  # legend("topleft", c("GRWL", "Ice Corrected"), lty=1, 
  #        col=c(rgb(0,0,0,0.8), rgb(0,0,1,0.5)), cex=0.8)
  
  # Uncorrected and corrected surface area including small streams:
  plot(latTab$lat, latTab$RSSA_smStr, type="l", col=rgb(0,0,0,0.8),
       main=paste("Hanquin Region:", region),
       xlab="Latitude",
       ylab="Sum RSSA per degree lat. (sq km)",
       ylim=c(0,yMax))
  lines(latTab$lat, latTab$RSSA_smStr_ice, col=rgb(0,0,1,0.5))
  legend("topleft", c("GRWL", "Ice Corrected"), lty=1, 
         col=c(rgb(0,0,0,0.8), rgb(0,0,1,0.5)), cex=0.8)
  
  # Smoothed uncorrected and corrected surface area including small streams:
  # sSpl = smooth.spline(latTab$lat, latTab$RSSA_smStr*(1/degreeInterval), spar=0.3)
  # sSpl_ice = smooth.spline(latTab$lat, latTab$RSSA_smStr_ice*(1/degreeInterval), spar=0.3)
  # plot(sSpl$x, sSpl$y, type='l', 
  #      xlab="Latitude",
  #      ylab="Sum RSSA per degree lat. (sq km)",
  #      lwd=2, col=rgb(0,0,0,0.6))
  # lines(sSpl_ice$x, sSpl_ice$y, type='l', lwd=2, col=rgb(0,0,1,0.5))
  # legend("topleft", c("GRWL", "Ice Corrected"), lty=1, lwd=2,
  #        col=c(rgb(0,0,0,0.6), rgb(0,0,1,0.5)), cex=0.8)
}


################################################################################
# For each file, read in and concatenate GRWL data into one big table:
################################################################################

switch=0

if (switch == 1) {
  file.remove(bigTabPath)
  if (!file.exists(bigTabPath)){
    for (h in 1:length(fPaths)){
      
      print(paste0(h, "/", length(fPaths), " reading in ", fNames[h], "..."))
      
      dbf = read.dbf(fPaths[h])
      
      if (nrow(dbf) < 1){ next }
      
      # create output file:
      bind = cbind(lon=dbf$lon, 
                   lat=dbf$lat, 
                   width = dbf$width_m, 
                   elev_m = dbf$elev_m, 
                   lakeFlag = dbf$lakeFlag, 
                   iceOccur = dbf$perFrozen,
                   region = dbf$gridcode
      )
      
      if (h==1){ tab = bind }else{ tab = rbind(tab, bind)}
    }
    
    tab = as.data.frame(tab)
    print("writing out big table...")
    write.csv(tab, bigTabPath, row.names=T)
  
  } else {
    
    print("reading in big table...")
    tab = read.csv(bigTabPath, header=T)
    
  }

}




################################################################################
# For Each Hanquin Region Sum observed RSSA:
################################################################################
tab_full = tab
names(tab_full)[names(tab_full) == "gridcode"] = "reg"
uniqReg = sort(unique(tab_full$reg))

GRWLbyLatOut = paste0(outDir, "/GRWL_4C_isotherm_byHanquin_regions/GRWLbyLat_individualDegree/GRWLbyLatitude_", uniqReg, ".csv")

for (i in 1:length(uniqReg)){
  print(paste("Hanquin Region:", uniqReg[i]))
  
  regInd = which(tab_full$reg==uniqReg[i])
  tab = tab_full[regInd,]
  
  latTab = RSSAbyLat(tab, degreeInterval=1, region=uniqReg[i])

  # write out latTabs:
  write.csv(latTab, GRWLbyLatOut[i], row.names=F)
  
  # add latTab to all regions lat tab:
  if (i==1){ 
    latTab_allReg = latTab 
  } else {
    latTab_allReg = rbind(latTab_allReg, latTab)
  }

}


################################################################################
# Correct RSSA for ice and plot:
################################################################################
# adjust numbers to take into account total surface area as calculated by Allen & Pavelsky, 2018:
sumObRSSA = sum(latTab_allReg$RSSA)
correctionFactor = GRWLTotal / sumObRSSA
RSSA_smStr_allReg = latTab_allReg$RSSA * correctionFactor
RSSA_smStr_ice_allReg = latTab_allReg$RSSA_ice * correctionFactor


# add adjusted numbers to latTab csvs and plot:
pdfPath = paste0(outDir, "/fig/GRWL_4C_isotherm_byHanquin_regions.pdf")
pdf(pdfPath, 7, 5)

for (i in 1:length(uniqReg)){
  print(paste("Hanquin Region:", uniqReg[i]))
  
  latTab = read.csv(GRWLbyLatOut[i], header=T)
  
  # correct for ice coverage:
  latTab$RSSA_smStr = latTab$RSSA * correctionFactor
  latTab$RSSA_smStr_ice = latTab$RSSA_ice * correctionFactor
  
  # write out latTabs:
  write.csv(latTab, GRWLbyLatOut[i], row.names=F)
  
  # plot:
  pdfplotter(latTab, region=uniqReg[i], yMax=max(RSSA_smStr_allReg))
  
}

dev.off()
cmd = paste('open', pdfOut)
system(cmd)

# check sums:
# sum(RSSA_smStr_ice)
# sum(RSSA_smStr)


################################################################################
# Sum RSSA into larger lat bands:
################################################################################
bandTab = read.csv(latBandsIn, header=T)

GRWLbyLatBandsOut = paste0(outDir, "/GRWL_4C_isotherm_byHanquin_regions/GRWLbyLat_latBands/GRWLbyLatitude_", uniqReg, ".csv")
GRWLbyLat_NSbandsOut = paste0(outDir, "/GRWL_4C_isotherm_byHanquin_regions/GRWLbyLat_NSbands/GRWLbyLatitude_", uniqReg, ".csv")


for (i in 1:length(uniqReg)){
  print(paste("Hanquin Region:", uniqReg[i]))
  
  latTab = read.csv(GRWLbyLatOut[i], header=T)
  
  bandTab$RSSA_km2_ice = bandTab$RSSA_km2 = 0
  
  for (j in 1:nrow(bandTab)){
    print(j)
    k = which(latTab$lat > bandTab$latband_lwr[j] & 
                latTab$lat <= bandTab$latband_upr[j])
    if (length(k) > 0){ 
      bandTab$RSSA_km2[j] = round(sum(latTab$RSSA_smStr[k]))
      bandTab$RSSA_km2_ice[j] = round(sum(latTab$RSSA_smStr_ice[k]))
    }
  }
  
  # add together N & S hemispheres:
  N_hem = which(bandTab$latband_lwr >= 0)
  S_hem = which(bandTab$latband_lwr < 0)
  
  matchHem = match(bandTab$latband_upr[N_hem], abs(bandTab$latband_lwr[S_hem]))
  
  NS_hems_RSSA_km2 = apply(cbind(bandTab$RSSA_km2[N_hem], bandTab$RSSA_km2[matchHem]), 1, sum)
  NS_hems_RSSA_km2_ice = apply(cbind(bandTab$RSSA_km2_ice[N_hem], bandTab$RSSA_km2_ice[matchHem]), 1, sum)
  
  NStab = as.data.frame(array(NA, c(length(N_hem), 4)))
  names(NStab) = c("lat_lwr", "lat_upr", "RSSA_km2", "RSSA_km2_ice")
    
  # built output table: 
  NStab$lat_lwr = bandTab$latband_lwr[N_hem]
  NStab$lat_upr = bandTab$latband_upr[N_hem]
  NStab$RSSA_km2 = NS_hems_RSSA_km2
  NStab$RSSA_km2_ice = NS_hems_RSSA_km2_ice
  
  # write out csvs: 
  write.csv(bandTab, GRWLbyLat_NSbandsOut[i], row.names=F)
  write.csv(NStab, GRWLbyLatBandsOut[i], row.names=F)
  
  # check sums:
  # sum(bandTab$RSSA_km2)
  # sum(bandTab$RSSA_km2_ice)
  
}

