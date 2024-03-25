# PhD Plus: Data Literacy in R (2024)
# Session 4: Essential Statistics
# Clay Ford


homes <- readRDS(url("https://github.com/clayford/phdplus2024/raw/main/data/albemarle_homes_2024-02-28.rds"))
eq <- readRDS(url("https://github.com/clayford/phdplus2024/raw/main/data/eq.Rds"))
eq <- eq[!is.na(eq$mag),]

# 5-year randomized study testing whether regular intake of aspirin reduces
# mortality from cardiovascular disease.

# Preliminary report: Findings from the Aspirin Component of the Ongoing Physicians' Health Study. NEJM, 318: 262-264 (1988).
mi <- matrix(data = c(10845, 10933, 189, 104), nrow = 2, 
             dimnames = list("group" = c("placebo", "aspirin"),
                             "myocardial infarction" = c("no", "yes")))

mi




# The rats data contains a sample of captured kangaroo rats (Dipodomys
# merriami) that were used to estimate population size and density.

# Mabry, Karen; Hurtado, Gizelle; Mayer, Ghislaine (2022). Data from: Does
# urbanization ameliorate the effect of endoparasite infection in kangaroo rats?
# [Dataset]. Dryad. https://doi.org/10.5061/dryad.8pk0p2nns

rats <- read.csv("https://github.com/uvastatlab/phdplus2023/raw/main/data/parasite.csv")

# a study measured serotonin in captured locusts for 0, 1, and 2 hours to see
# how it affects social behavior.

# Anstey et al. (2009) Serotonin mediates behavioral gregarization underlying swarm formation in desert locusts. Science 323: 627-630.

locusts <- read.csv("/Users/jcf2d/Dropbox/_statistics/ABD/Data/Data/chapter02/chap02f1_2locustSerotonin.csv")
locusts$treatmentTime <- factor(locusts$treatmentTime)


# Do long spikes on a horned lizard (mm) help protect them from being eaten by
# shrikes, a type of bird that skewers its victims on thorns to eat later? The
# lizards data set contains measurements of the length of lizard horns for two
# groups: those that were alive and those that had been killed and skewered.

# Young, et al. (2004). How the horned lizard got its horns. Science 304: 65.

lizards <- read.csv("/Users/jcf2d/Dropbox/_statistics/ABD/Data/Data/chapter12/chap12e3HornedLizards.csv")
names(lizards) <- c("horn_length", "survival")
lizards$survival <- factor(lizards$survival)
lizards <- lizards[complete.cases(lizards),]


# The lions data set contains the age and proportion of black on their noses for
# 32 lions. Can we use the proportion of black to estimate a lion's age?

# Whitman, et al (2004). Sustainable trophy hunting of African lions. Nature
# 428: 175-178.

lions <- read.csv("/Users/jcf2d/Dropbox/_statistics/ABD/Data/Data/chapter17/chap17e1LionNoses.csv")
names(lions) <- c("proportion", "age")


library(datasauRus)
dat1 <- subset(datasaurus_dozen, dataset == "slant_up", select = c(x, y))
dat2 <- subset(datasaurus_dozen, dataset == "dino", select = c(x, y))
attr(dat1, which = "spec") <- NULL
attr(dat2, which = "spec") <- NULL


save(dat1, dat2, homes, lions, lizards, locusts, rats, mi, eq,
     file = "session_4.Rdata")
