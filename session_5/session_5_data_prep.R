# session 5 data prep

homes <- readRDS(url("https://github.com/clayford/phdplus2024/raw/main/data/albemarle_homes_2024-02-28.rds"))
homes$hsdistrict <- NULL
homes$msdistrict <- NULL
homes$lastsaledate1 <- NULL
homes$lastsaleprice <- NULL
homes$usecode <- NULL
homes$month_sold <- NULL
homes$censustract <- NULL
homes$yearremodeled <- NULL
homes$yearbuilt <- NULL
ah <- subset(homes, esdistrict == "Agnor-Hurt")
homes <- subset(homes, esdistrict == "Woodbrook")
homes$esdistrict <- NULL
ah$esdistrict <- NULL

rownames(homes) <- NULL
rownames(ah) <- NULL

set.seed(123)
i <- sample(nrow(ah), 20)
ah <- ah[i,]

# saveRDS(homes, file = "data/woodbrook.rds")
# saveRDS(ah, file = "data/agnorhurt.rds")
save(homes, ah, file = "data/session_5.Rdata")
