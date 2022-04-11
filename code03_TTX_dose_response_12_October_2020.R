# P4C4PwithTTX - Data analysis
#   Protocol 4: TTX Dose Response Curve
#     1st stimulus occurs at 5s (seconds)
#     System delay is 4ms (miliseconds)
#     Pulsewidth is 500.0us (microseconds)
#     Stimulus rate is 0.2 Hz
#     Last stimulus is at 20s
#
# Processing:
#   Contraction Amplitude (N/g)
#     This will then be plotted against the concentration of TTX that was on the muscle at that time.
# Once all amplitudes and concentration (for one individual/muscle) are plotted, a sigmoidal curve must be generated and the 4 values must be extracted:
#   The x-value at 50% y (x0)
#   The rate of change (dx)
#   The width of the curve from 10% to 90%
#   The peak first derivative of that sigmoidal
#
# 8/15/18 - Fixed sMusMassg problem
#
library(reshape2)
# Get Snake Info - Species, Genotype etc.
sinf = read.csv("C:/Users/rdelcarlo/Desktop/data_r_script/SnakeInfo-09.30.2020.csv")

# Get Snake Muscle Masses
smm = read.csv("C:/Users/rdelcarlo/Desktop/data_r_script/SnakeSkeletalMuscleMasses-9.28.2020.csv")
# reset all missing muscle mass values to -1.0smm
smm[is.na(smm)] <- 0.999

# dname = "C:/Bobby/Data-CSV/p4C4PwithTTX" "C:/Users/rdelcarlo/Desktop/Myography Data/Protocol 6 - Lidocaine" "C:/Users/rdelcarlo/Desktop/data_r_script/p4c4pwithTTX/p4c4pwithTTX"
dname = "C:/Users/rdelcarlo/Desktop/data_r_script/p4c4pwithTTX/p4c4pwithTTX"
#dname = "C:/Users/rdelcarlo/Downloads/Converted/Converted"
#dname = "C:/Users/rdelcarlo/Desktop/data_r_script/p4c4pwithTTX/TestingTTX"
setwd(dname)

x = strsplit(dname, "/")
# Write / overwrite Column Headers for output summary file
ofsum = paste("./output/", x[[1]][lengths(x)],  ".csv", sep = "")

hdrs <- c(
  "Seq",
  "File",
  "Snake",
  "Muscle",
  "Mass",
  "Dose",
  "Pulse",
  "BaseF(N/g)",
  "ContrAmpl(N/g)",
  "To100pct(ms)",
  "To10pct(ms)",
  "To50pct(ms)",
  "10to50pct(ms)",
  "FMaxRateOfChg(N/g/s)",
  "FMinRateOfChg(N/g/s)",
  "ToFChgMax(ms)",
  "ToFChgMin(ms)",
  "DiffFChgMaxToMin(ms)"
)
write.table(
  t(hdrs),
  file = ofsum,
  append = FALSE,
  quote = TRUE,
  sep = ",",
  eol = "\n",
  na = "NA",
  dec = ".",
  row.names = FALSE,
  col.names = FALSE
)

# Output Force File
ofF <- "./output/p4C4PTTX-force.csv"
ofFhdr <- c(
  "Species",
  "GenoType",
  "MAMU",
  "COUNTY",
  "LONGITUDE",
  "LATITUDE",
  "SVLmm",
  "MusMassg",
  "BodMassg",
  "GENDER",
  "EXTRACTION",
  "DATE_Experimented",
  "Date_Collected",
  "Days_In_Between",
  "Pulse",
  "Snake",
  "Muscle",
  "Dose(nM)",
  "Rater",
  (1:1500) / 10000
)
write.table(
  t(ofFhdr),
  file = ofF,
  append = FALSE,
  quote = TRUE,
  sep = ",",
  eol = "\n",
  na = "NA",
  dec = ".",
  row.names = FALSE,
  col.names = FALSE
)

# Output Force first derivative File
ofF1d <- "./output/p4c4pTTX-force1d.csv"
ofF1dhdr <- c(
  "Species",
  "GenoType",
  "MAMU",
  "COUNTY",
  "LONGITUDE",
  "LATITUDE",
  "SVLmm",
  "MusMassg",
  "BodMassg",
  "GENDER",
  "EXTRACTION",
  "DATE_Experimented",
  "Date_Collected",
  "Days_In_Between",
  "Pulse",
  "Snake",
  "Muscle",
  "Dose(nM)",
  "Rater",
  (1:29) / 200
)
write.table(
  t(ofF1dhdr),
  file = ofF1d,
  append = FALSE,
  quote = TRUE,
  sep = ",",
  eol = "\n",
  na = "NA",
  dec = ".",
  row.names = FALSE,
  col.names = FALSE
)

# get all data files
files = list.files(path = ".", pattern = "csv")
q <- strsplit(files, "-")
# snakes <- unlist(lapply(q,'[[',1))
# muscles <- unlist(lapply(q,'[[',2))
# osum <- data.frame(files,snakes,muscles)

fTTX <- paste("./output/", "P4C4PwithTTXFiles",  ".csv", sep = "")
write.table(
  t(c(
    "FileName", "Snake", "Dose(nM)", "Muscle", "Rater", "Mass(g)"
  )),
  file = fTTX,
  append = FALSE,
  quote = TRUE,
  sep = ",",
  eol = "\n",
  na = "NA",
  dec = ".",
  row.names = FALSE,
  col.names = FALSE
)

fnum <- 0
for (file in files) {
  fnum <- fnum + 1
  y <- strsplit(file, "-")
  snake <- trimws(y[[1]][1])
  muscle <- trimws(y[[1]][2])
  dose <- trimws(y[[1]][3])
  #dose <- sub("1.dat.csv", "", dose)
  dose <- sub(".dat.csv", "", dose)
  dose <- as.numeric(gsub("\\D", "", dose))
  dose <- (dose - 1) / 10
  # snake <- as.character(osum$snakes[as.factor(file)])
  # muscle <- as.character(osum$muscles[as.factor(file)])
  # get Snake Info
  sSpecies <- as.character(sinf$Species[which(sinf$Snake == snake)])
  sGenotype <-
    as.character(sinf$Genotype[which(sinf$Snake == snake)])
  sMAMU <- as.character(sinf$MAMU[which(sinf$Snake == snake)])
  sCounty <- as.character(sinf$COUNTY[which(sinf$Snake == snake)])
  sLong <- as.character(sinf$LONGITUDE[which(sinf$Snake == snake)])
  sLat <- as.character(sinf$LATITUDE[which(sinf$Snake == snake)])
  sSVLmm <- as.character(sinf$SVLmm[which(sinf$Snake == snake)])
  # sMusMassg <-
  #  as.character(sinf$MusMassg[which(sinf$Snake == snake)])
  sBodMassg <-
    as.character(sinf$BodMassg[which(sinf$Snake == snake)])
  sGender <- as.character(sinf$GENDER[which(sinf$Snake == snake)])
  sExtraction <-
    as.character(sinf$EXTRACTION[which(sinf$Snake == snake)])
  sDtExp <-
    as.character(sinf$DATE.Experimented[which(sinf$Snake == snake)])
  sDtColl <-
    as.character(sinf$Date.Collected[which(sinf$Snake == snake)])
  sDInBet <-
    as.character(sinf$Days.in.Between[which(sinf$Snake == snake)])
  # Get Muscle Mass
  muscMass <- smm[which(smm$SnakeID == snake), muscle] / 1000
  sMusMassg <- muscMass
  # print(snake, muscle, muscMass)
  # Get Rater from Snake Info file for snake/musc combo
  rater <- as.character(sinf[which(sinf$Snake==snake), paste0("Rater..", muscle, ".")])

  write.table(
    t(c(file, snake, dose, muscle, rater, muscMass)),
    file = fTTX,
    append = TRUE,
    quote = TRUE,
    sep = ",",
    eol = "\n",
    na = "NA",
    dec = ".",
    row.names = FALSE,
    col.names = FALSE
  )
  
  ofdata <- paste("./output/", snake, "-", muscle, ".csv", sep = "")
  raw = read.csv(file)
  # Select all rows with stimulus > 1
  stmRows <- which(raw$Stimulus > 1.0)
  if (length(stmRows) < 4) {
    cat("File: ", file, "; Stim Rows: ", length(stmRows), "\n")
    next
  }
  # find last time for each pulse
  categories <- cutree(hclust(dist(stmRows)), k = 4)
  stmRows <- aggregate(stmRows, list(cats = categories), "max")[, 2]
  if (length(stmRows) < 4) {
    cat("File: ", file, "; Stim Rows: ", length(stmRows), "\n")
    next
  }
  
  # extract force-baseline for 1500 pts after end of stimulus;
  i <- 0
  for (sr in stmRows) {
    i <- i + 1
    meanF = mean(raw$Force..g.[sr - 1000:sr])
    rspF <-
      (raw$Force..g.[stmRows[1]:(stmRows[1] + 1499)] - meanF) * (0.00980665 / muscMass)
    oFline <- c(
      sSpecies,
      sGenotype,
      sMAMU,
      sCounty,
      sLong,
      sLat,
      sSVLmm,
      sMusMassg,
      sBodMassg,
      sGender,
      sExtraction,
      sDtExp,
      sDtColl,
      sDInBet,
      i,
      snake,
      muscle,
      dose,
	  rater,
      rspF
    )
    write.table(
      t(oFline),
      file = ofF,
      append = TRUE,
      quote = TRUE,
      sep = ",",
      eol = "\n",
      na = "NA",
      dec = ".",
      row.names = FALSE,
      col.names = FALSE
    )
    #  Low pass 200Hz filter -> pick every 30th entry
    rspF1d <- diff(rspF[1:30 * 50]) * 200
    oF1dline <- c(
      sSpecies,
      sGenotype,
      sMAMU,
      sCounty,
      sLong,
      sLat,
      sSVLmm,
      sMusMassg,
      sBodMassg,
      sGender,
      sExtraction,
      sDtExp,
      sDtColl,
      sDInBet,
      i,
      snake,
      muscle,
      dose,
	  rater,
      rspF1d
    )
    write.table(
      t(oF1dline),
      file = ofF1d,
      append = TRUE,
      quote = TRUE,
      sep = ",",
      eol = "\n",
      na = "NA",
      dec = ".",
      row.names = FALSE,
      col.names = FALSE
    )
    maxF <- max(rspF)
    t100p <- min(which(rspF >= maxF))
    # convert all times to ms from tenth of a ms
    t10p <- min(which(rspF > 0.1 * maxF))
    t50p <-
      t100p + min(which(rspF[t100p:length(rspF)] <= 0.5 * maxF))
    maxF1d <- max(rspF1d)
    minF1d <- min(rspF1d)
    t1dmax <- min(which(rspF1d >= maxF1d)) * 30
    t1dmin <- min(which(rspF1d <= minF1d)) * 30
    sumLine = c(
      fnum,
      file,
      snake,
      muscle,
      muscMass,
      dose,
      i,
      meanF,
      maxF,
      t100p / 10,
      t10p / 10,
      t50p / 10,
      (t50p - t10p) / 10,
      maxF1d,
      minF1d,
      t1dmax / 10,
      t1dmin / 10,
      (t1dmin - t1dmax) / 10
    )

    write.table(
      t(sumLine),
      file = ofsum,
      append = TRUE,
      quote = TRUE,
      sep = ",",
      eol = "\n",
      na = "NA",
      dec = ".",
      row.names = FALSE,
      col.names = FALSE
    )
  }
}

df = read.csv("./output/p4C4PTTX-force.csv")
# soff <- off[order(off$Pulse,off$Snake.Muscle),]
# toff <- t(soff)
# write.csv(toff, "./output/p4C4PTTX-force-rpt.csv")
write.csv(t(df[order(df$Pulse, df$Snake, df$Muscle, df$Dose), ]),
          "./output/p4C4PTTX-force-rpt.csv")

df = read.csv("./output/p4C4PTTX-force1d.csv")
write.csv(t(df[order(df$Pulse, df$Snake, df$Muscle, df$Dose), ]),
          "./output/p4C4PTTX-force1d-rpt.csv")

df = read.csv(ofsum)
agg <-
  aggregate(
    df,
    by = list(
      Dose = df$Dose,
      Muscle = df$Muscle,
      Snake = df$Snake
      ,       File = df$File
    ),
    FUN = mean,
    simplify = TRUE
  )
a1 <- agg[, c(4, 3, 2, 9:10, 12:22)]
write.csv(a1, paste("./output/", x[[1]][lengths(x)],  "-rpt.csv",sep=""))

# Create reports for each parameter by dose
library(reshape2)
a1_cast <- list()
for (var in colnames(a1)[6:ncol(a1)]) {
  a1_cast[[var]] <-
    dcast(a1,
          Dose ~ Snake + Muscle,
          value.var = var,
          fun.aggregate = mean)
}

for (var in names(a1_cast)) {
  write.csv(a1_cast[[var]], paste0("./output/p4C4PwithTTX-", var, ".csv"))
}

# plot(rspF)
# plot(rspF1d,type="l")
# scatter.smooth(x=1:29, y=rspF1d)