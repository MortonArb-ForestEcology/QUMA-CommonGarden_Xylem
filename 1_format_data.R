# Reading in annd exploring xylem data from the Bur Oak Common Garden (Andrew Hipp, Rebekah Mohn, & Colleagues) collected as part of the REU 2026 project (Elizabeth Moreno)

# Relevant Datasets:
# Fiji/ImageJ measurements: URF-REU 2026 - Moreno - Xylem Vessels/Common_garden_Fiji/*.csv
# Specimen Metadata: URF-REU 2026 - Moreno - Xylem Vessels/Bur oak cookies organization (gsheet; ID: 12Pe2RSCtUKxElPR49gRDeQlgdPCYyd5wezVjzJFNmnU )
# Stomatal Density Trees: URF-REU 2026 - Moreno - Xylem Vessels/Wood_Match_StomataTrees.xlsx
# Garden Info: URF-REU 2026 - Moreno - Xylem Vessels/Copy of HerbariumSpecimenLabels_fromThinning-cleaned.xlsx

library(ggplot2)

# Setting the file path for Google Drive:
path.google <- "~/Google Drive/My Drive/URF-REU 2026 - Moreno - Xylem Vessels/"
path.fiji <- file.path(path.google, "Common_garden_Fiji") # Where our input csv files live
path.out <- file.path(path.google, "Analysis") # Where we want our outputs to live

if(!dir.exists(path.out)) dir.create(path.out)

# get a list of out available input files
f.fiji <- dir(path.fiji, ".csv")

# Creating a placeholder for ring info and raw vessel info
df.rings <- data.frame()
df.vessels <- data.frame()

for(fNOW in 1:length(f.fiji)){
  # Opening a file to look at it
  test <- read.csv(file.path(path.fiji, f.fiji[fNOW]))
  head(test)
  
  # Labeling Structure: MOR_[TreeID]_[radius]_[year]_[EW/LW]_area_[vesselID]
  # Creating a dataframe to put everything in
  dfLabs <- data.frame(site=NA, tree=NA, radius=NA, year=NA, portion=NA, type=NA, vessel=NA)
  testLabs1 <- unlist(lapply(strsplit(test$Label, ":"), function(x){x[2]}))
  for(i in 1:length(testLabs1)){
    labNow <- strsplit(testLabs1[i], "_")[[1]]
    
    dfLabs[i, 1:length(labNow)] <- labNow
    
  }
  summary(dfLabs)
  
  # binding the labels onto our data
  test <- cbind(test[,2:ncol(test)], dfLabs)
  test$type[is.na(test$year)] <- "radius"
  test$portion[is.na(test$portion)] <- "radius"
  test$type[is.na(test$type) & !is.na(test$year)] <- "increment"
  test$type[!is.na(test$vessel)] <- "vessel"
  
  summary(test)
  tail(test)
  
  # Pulling out earlywood & latewood info & Creating a summary table
  test.ew <- test[test$portion=="EW" & test$type=="increment", c("site", "tree", "radius", "year","Length")]
  names(test.ew)[names(test.ew)=="Length"] <- "EW"
  test.lw <- test[test$portion=="LW" & test$type=="increment", c("site", "tree", "radius", "year","Length")]
  names(test.lw)[names(test.lw)=="Length"] <- "LW"
  test.rw <- merge(test.ew, test.lw)
  test.rw$RW <- test.rw$EW + test.rw$LW
  test.rw <- merge(test.rw, test[test$portion=="EW" & test$type=="area",c("site", "tree", "radius", "year","Area")])
  names(test.rw)[names(test.rw)=="Area"] <- "EW.area"
  test.rw
  
  # Now lets work with the vessels
  test.vess <- test[test$type=="vessel",c("site", "tree", "radius", "year", "vessel", "Area")]
  
  # Pulling out some stats about the vessels that will be useful for us
  for(i in 1:nrow(test.rw)){
    YR = test.rw$year[i]
    RAD = test.rw$radius[i]
    vessNow <- test.vess[test.vess$year==YR & test.vess$radius==RAD,]
    
    test.rw[i,"vessel.n"] <- nrow(vessNow)
    test.rw[i,"vessel.AreaTot"] <- sum(vessNow$Area)
    test.rw[i,"vessel.AreaMean"] <- mean(vessNow$Area)
    test.rw[i,"vessel.AreaSD"] <- sd(vessNow$Area)
  }
  
  
  # Adding this file to our master list
  # NOTE: This will get progressively slower and isn't a great way to do it, but it'll work for now
  df.rings <- rbind(df.rings, test.rw)
  df.vessels <- rbind(df.vessels, test.vess)
}
df.rings$relVA <- df.rings$vessel.AreaTot/df.rings$EW.area
summary(df.rings)
summary(df.vessels)

write.csv(df.rings, file.path(path.out, "BurOakCG_RingData_combined.csv"), row.names=F)
write.csv(df.vessels, file.path(path.out, "BurOakCG_VesselData_combined.csv"), row.names=F)

# doing a couple quick exploratory graphs
hist(df.rings$vessel.n)
hist(df.rings$vessel.AreaMean)
hist(df.rings$vessel.AreaTot)
hist(df.rings$relVA)

hist(df.vessels$Area)
