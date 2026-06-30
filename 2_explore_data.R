# Reading in annd exploring xylem data from the Bur Oak Common Garden (Andrew Hipp, Rebekah Mohn, & Colleagues) collected as part of the REU 2026 project (Elizabeth Moreno)

# Relevant Datasets:
# Fiji/ImageJ measurements: URF-REU 2026 - Moreno - Xylem Vessels/Common_garden_Fiji/*.csv
# Specimen Metadata: URF-REU 2026 - Moreno - Xylem Vessels/Bur oak cookies organization (gsheet; ID: 12Pe2RSCtUKxElPR49gRDeQlgdPCYyd5wezVjzJFNmnU )
# Stomatal Density Trees: URF-REU 2026 - Moreno - Xylem Vessels/Wood_Match_StomataTrees.xlsx
# Garden Info: URF-REU 2026 - Moreno - Xylem Vessels/Copy of HerbariumSpecimenLabels_fromThinning-cleaned.xlsx

library(ggplot2)
library(nlme)
# Setting the file path for Google Drive:
path.google <- "~/Google Drive/My Drive/URF-REU 2026 - Moreno - Xylem Vessels/"
path.out <- file.path(path.google, "Analysis") # Where we want our outputs to live

# 1. Lets start by exploring the ring data ----

# Read in the tree data
df.trees <- data.frame(readxl::read_xlsx(file.path(path.google, "Copy of HerbariumSpecimenLabels_fromThinning-cleaned.xlsx")))
names(df.trees)[names(df.trees)=="Tree"] <- "treeID"
summary(df.trees)
head(df.trees)


# Read in the ring data
df.rings <- read.csv(file.path(path.out, "BurOakCG_RingData_combined.csv"))
df.rings <- df.rings[!is.na(df.rings$site),]
df.rings$site <- as.factor(df.rings$site)
df.rings$tree <- as.factor(df.rings$tree)
df.rings$radius <- as.factor(df.rings$radius)
df.rings$treeID <- paste0(df.rings$site, df.rings$tree)
summary(df.rings)

df.rings <- merge(df.rings, df.trees[,c("treeID", "Mother.Tree", "Source.State", "height", "diameter", "DBH", "Weight")], all.x=T, all.y=F)
summary(df.rings)
df.rings$Source.State <- factor(df.rings$Source.State, levels=c("Oklahoma", "Illinois", "Minnesota"))

# Number of vessels
plot.n <- ggplot(data=df.rings) +
  facet_wrap(~year) +
  geom_boxplot(aes(x=Source.State, y=vessel.n, fill=Source.State))
plot.n
png(file.path(path.out, "Vessels_n_byState.png"), height=4, width=6, units = "in", res=240)
plot.n
dev.off()

# Mean Area per vessel per year
plot.areaMean <- ggplot(data=df.rings) +
  facet_wrap(~year) +
  geom_boxplot(aes(x=Source.State, y=vessel.AreaMean, fill=Source.State))

png(file.path(path.out, "Vessels_meanArea_byState.png"), height=4, width=6, units = "in", res=240)
plot.areaMean
dev.off()

# Mean proportion of Earlywood as vessels (# NOTE: Not total conductive area yet!)
plot.relVA <- ggplot(data=df.rings) +
  facet_wrap(~year) +
  geom_boxplot(aes(x=Source.State, y=relVA, fill=Source.State))

png(file.path(path.out, "Vessels_relVA_byState.png"), height=4, width=6, units = "in", res=240)
plot.relVA
dev.off()


# Doing some ANOVAS to see if there any difference
lme.size <- lme(vessel.AreaMean ~ Source.State, random=list(year=~1, Mother.Tree=~1, treeID=~1), data=df.rings[df.rings$year %in% 2023:2025,], na.action = na.omit)
summary(lme.size)
anova(lme.size)

lme.va <- lme(relVA ~ Source.State, random=list(year=~1, Mother.Tree=~1, treeID=~1), data=df.rings[df.rings$year %in% 2023:2025,], na.action = na.omit)
summary(lme.va)
anova(lme.va)
