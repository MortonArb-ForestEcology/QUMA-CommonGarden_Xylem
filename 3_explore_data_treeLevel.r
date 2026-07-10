# Creating tree-level stats for vessel characteristics and seeing if they correlate with tree-level stats

# Reading in annd exploring xylem data from the Bur Oak Common Garden (Andrew Hipp, Rebekah Mohn, & Colleagues) collected as part of the REU 2026 project (Elizabeth Moreno)

# Relevant Datasets:
# Fiji/ImageJ measurements: URF-REU 2026 - Moreno - Xylem Vessels/Common_garden_Fiji/*.csv
# Specimen Metadata: URF-REU 2026 - Moreno - Xylem Vessels/Bur oak cookies organization (gsheet; ID: 12Pe2RSCtUKxElPR49gRDeQlgdPCYyd5wezVjzJFNmnU )
# Stomatal Density Trees: URF-REU 2026 - Moreno - Xylem Vessels/Wood_Match_StomataTrees.xlsx
# Garden Info: URF-REU 2026 - Moreno - Xylem Vessels/Copy of HerbariumSpecimenLabels_fromThinning-cleaned.xlsx

library(ggplot2)
library(nlme)
library(emmeans)

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
df.rings$vessel.density <- df.rings$vessel.n/df.rings$EW.area
df.rings$block <- as.factor(ifelse(nchar(as.vector(df.rings$tree))==3, substr(df.rings$tree, 1, 1), 0))
summary(df.rings)
head(df.rings)

# First getting things to the 1 number per year, then aggreagting the years to 1 number per tree
df.ringAgg1 <- aggregate(cbind(vessel.n, vessel.density, vessel.AreaMean, relVA) ~ site + block + tree + treeID + year , data=df.rings[df.rings$year %in% 2023:2025,], FUN=mean, na.rm=T)

df.ringAgg2 <- aggregate(cbind(vessel.n, vessel.density, vessel.AreaMean, relVA) ~ site + block + tree + treeID , data=df.ringAgg1[df.rings$year %in% 2023:2025,], FUN=mean, na.rm=T)


df.trees <- merge(df.ringAgg2, df.trees[,c("treeID", "Mother.Tree", "Source.State", "height", "diameter", "DBH", "Weight")], all.x=T, all.y=F)
summary(df.trees)


# df.rings <- merge(df.rings, df.trees[,c("treeID", "Mother.Tree", "Source.State", "height", "diameter", "DBH", "Weight")], all.x=T, all.y=F)
# summary(df.rings)
df.trees$Source.State <- factor(df.trees$Source.State, levels=c("Oklahoma", "Illinois", "Minnesota"))

ggplot(data=df.trees,aes(x=height, y=vessel.AreaMean, color=Source.State, fill=Source.State)) +
  geom_point() +
  geom_smooth(method="lm", se=T)

ggplot(data=df.trees,aes(x=height, y=vessel.density, color=Source.State, fill=Source.State)) +
  geom_point() +
  geom_smooth(method="lm", se=T)


lme.dens.height <- lme(vessel.density ~ height*Source.State, random=list(block=~1, Mother.Tree=~1, treeID=~1), data=df.trees, na.action = na.omit)
anova(lme.dens.height)
summary(lme.dens.height)

lme.dens.height2 <- lme(vessel.density ~ height+Source.State, random=list(block=~1, Mother.Tree=~1, treeID=~1), data=df.trees, na.action = na.omit)
anova(lme.dens.height2)
summary(lme.dens.height2)
emmeans(lme.dens.height2, pairwise~Source.State, adjust="tukey")


lme.area.height <- lme(vessel.AreaMean ~ height*Source.State, random=list(block=~1, Mother.Tree=~1, treeID=~1), data=df.trees, na.action = na.omit)
anova(lme.area.height)
summary(lme.area.height)

lme.area.height2 <- lme(vessel.AreaMean ~ height+Source.State, random=list(block=~1, Mother.Tree=~1, treeID=~1), data=df.trees, na.action = na.omit)
anova(lme.area.height2)
summary(lme.area.height2)
emmeans(lme.area.height2, pairwise~Source.State, adjust="tukey")
