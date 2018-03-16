#this script checks for physically impossible or missing values in the reference ET, wind, and daily minimum relative humidity spatial cimis derived estiamte and replaces them with period of record averages for the given, affected 2 km Spatial CIMIS cell; gap-filling procedure is arguably rudimentary, but bottomline is that dataset is at least 99.9% passable even without doing anything.  So this is more about getting uninterrupted, physically feasible climatic time-series that will allow the dual crop coefficent model to run
#PRISM dataset is OK

modelscaffoldDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/model_scaffold/run_model/Mar2018'
list.files(modelscaffoldDir)
precip <- read.csv(file.path(modelscaffoldDir, 'PRISM_precip_data.csv'), stringsAsFactors=FALSE) #this is a daily summary of precip from 10/1/2003-6/25/17 from 'free' daily PRISM 4km resolution for cells of interest in California, created in download_PRISM.R script (from 6/26/17 download)
sum(is.na(precip[,6:ncol(precip)])) #no NAs in PRISM as of 3/12/18
sum(precip[,6:ncol(precip)] < 0) #no negative numbers
sum(precip[,6:ncol(precip)] == 0) #19,769,868 no precip observations
sum(precip[,6:ncol(precip)] > 0) #4,201,190 precip observations
hist(as.numeric(lapply(precip[,6:ncol(precip)], sum))/25.4) #in inches
max(precip[,6:ncol(precip)]) #max precip was 10.2" in one day
lapply(precip[,6:ncol(precip)], summary)
#no changes made; use existing precip file created in 'download_PRISM.R'

#QC check on relative humdity data
RHmin <- read.csv(file.path(modelscaffoldDir, 'SpatialCIMIS.minRHupdate.rounded.csv'), stringsAsFactors = FALSE) #this is a daily summary of minimum relative humidity by 2 km raster cell, estimated from download of spatial CIMIS Tdew and Tmax data, created in spatialCIMIS.R script
dim(RHmin)
sum(is.na(RHmin[,6:ncol(RHmin)])) #26,120 NAs as of 3/12/18; cell_148533 no longer a cell of interest, so all but 76 are from 2/23/18 missing data; this is 0.04% missing
sum(RHmin[,6:ncol(RHmin)] < 0, na.rm = TRUE) #0 negative numbers
sum(RHmin[,6:ncol(RHmin)] == 0, na.rm = TRUE) #0 observations equal to 0
max(RHmin[,6:ncol(RHmin)], na.rm = TRUE) #180.175 max rel humidity; that's a problem
sum(RHmin[,6:ncol(RHmin)] > 100, na.rm = TRUE) #2690 observations greater than 100%
sum(RHmin[,6:ncol(RHmin)] > 95, na.rm = TRUE) #8658 observations greater than 95%
sum(RHmin[,6:ncol(RHmin)] > 90, na.rm = TRUE) #45737 observations greater than 90%
sum(RHmin[,6:ncol(RHmin)] > 60, na.rm = TRUE) #7,083,837 greater than 60%, about 10% of dataset
hist(as.numeric(lapply(RHmin[,6:ncol(RHmin)], mean, na.rm=TRUE))) #centered over 35-40% rel. humidity
max(as.numeric(lapply(RHmin[,6:ncol(RHmin)], mean, na.rm=TRUE))) #max 58.9% across all days for one cell
min(as.numeric(lapply(RHmin[,6:ncol(RHmin)], mean, na.rm=TRUE))) #min 15.0% across all days for one cell
mean(as.numeric(lapply(RHmin[,6:ncol(RHmin)], mean, na.rm=TRUE))) #mean 37.6% across all days for one cell

#conclusion is to gap-fill 2/23/18 first, then rest of NAs, then numbers greater than 100%
check2.23.data <- unlist(RHmin[which(RHmin$month==2 & RHmin$day==23 & RHmin$year!=2018),6:ncol(RHmin)])
class(check2.23.data)
length(check2.23.data)
sum(check2.23.data < 0) #0 are negative, confirming above, so don't worry about
sum(is.na(check2.23.data)) #0 are NA, so don't have to worry about that for 2/23/18 correction
sum(check2.23.data > 100) #5 are greater than 100; these removed before calculating daily averages below
rm(check2.23.data)
test <- apply(RHmin[which(RHmin$month==2 & RHmin$day==23 & RHmin$year!=2018),6:ncol(RHmin)], 2, function(x) {mean(x[x<100])})
summary(test)
head(test)
length(test)
rm(test)
RHmin[RHmin$dates=='02_23_2018', 6:ncol(RHmin)] <- apply(RHmin[which(RHmin$month==2 & RHmin$day==23 & RHmin$year!=2018),6:ncol(RHmin)], 2, function(x) {mean(x[x<100])}) #reduces NA total to 76
sum(is.na(RHmin[,6:ncol(RHmin)])) #now 13,060 which is exactly one other day
RHmin$dates[is.na(RHmin$cell_3091)] #"12_08_2011"
sum(is.na(RHmin[RHmin$dates=="12_08_2011",6:ncol(RHmin)])) #yup all NA on 12/8/2011

#fix 12/8/2011 data using same approach as above
test <- apply(RHmin[which(RHmin$month==12 & RHmin$day==8 & RHmin$year!=2011),6:ncol(RHmin)], 2, function(x) {mean(x[x<100])})
summary(test)
head(test)
length(test)
rm(test)
RHmin[RHmin$dates=='12_08_2011', 6:ncol(RHmin)] <- apply(RHmin[which(RHmin$month==12 & RHmin$day==8 & RHmin$year!=2011),6:ncol(RHmin)], 2, function(x) {mean(x[x<100])})
sum(is.na(RHmin[,6:ncol(RHmin)])) #all NAs now gap-filled

#fix >100 values
pos_presence_RHmin <- lapply(RHmin[,6:ncol(RHmin)], function(x) {sum(x > 100)})
pos_presence_RHmin <- pos_presence_RHmin[pos_presence_RHmin > 0]
pos_presence_RHmin
length(pos_presence_RHmin) #352 cells with at least 1 >100% RHmin daily value
for (i in seq_along(pos_presence_RHmin)) {
  print(pos_presence_RHmin[i])
  print(RHmin$dates[RHmin[names(pos_presence_RHmin)[i]] > 100])
} #they are all different dates
for (i in seq_along(pos_presence_RHmin)) {
  dates <- RHmin$dates[RHmin[names(pos_presence_RHmin)[i]] > 100]
  for (j in seq_along(dates)) {
    day <- RHmin$day[RHmin$dates==dates[j]]
    month <- RHmin$month[RHmin$dates==dates[j]]
    gap.fill.data <- RHmin[which(RHmin$month==month & RHmin$day==day & RHmin$dates != '02_23_2018' & RHmin$dates != '12_08_2011'), names(pos_presence_RHmin)[i]]
    print(c(mean(gap.fill.data), sd(gap.fill.data)))
    RHmin[RHmin$dates==dates[j], names(pos_presence_RHmin)[i]] <- mean(gap.fill.data[gap.fill.data < 100], na.rm = TRUE) #remove any values >100 or NAs (even though NAs should have already been removed) calculating averages to gap fill
  }
}
sum(RHmin[,6:ncol(RHmin)] > 100, na.rm = TRUE) #good now
#inspect the values near the >100 values

write.csv(RHmin, file.path(modelscaffoldDir, 'SpatialCIMIS.RHmin.QCpass.csv'), row.names = FALSE)

#old code for RHmin data checking
# na_indices_RHmin <- lapply(RHmin[,6:ncol(RHmin)], function(x) {which(is.na(x))})
# which(names(RHmin)=='cell_148533')
# head(RHmin$cell_148533)
# head(RHmin[,10018])
# head(RHmin[,10019])
# for (i in 1:length(na_indices_RHmin)) {
#   if (length(na_indices_RHmin[[i]]) == 1) {
#     next
#   }
#   else(print(i))
# }
# names(na_indices_RHmin)[10013] #"cell_148533" there is no data for this cell.  It's located east of Watsonville right on the beach at this lat, long: 36.90404, -121.844
# na_indices_RHmin <- na_indices_RHmin[-10013]
# na_indices_RHmin <- unlist(na_indices_RHmin)
# unique(na_indices_RHmin) #row 2991 is only NA for all other cells
# #change row 2991 to average of all those same DOYs for a given cell
# doy_indices <- which(RHmin$DOY==RHmin$DOY[2991])
# doy_indices <- doy_indices[-9] #get rid of 2991
# doy342_means <- apply(RHmin[doy_indices, 6:ncol(RHmin)], 2, mean)
# RHmin[2991,6:ncol(RHmin)] <- doy342_means


#check wind data
U2 <- read.csv(file.path(modelscaffoldDir, 'SpatialCIMIS.U2update.rounded.csv'), stringsAsFactors = FALSE) #this is a daily summary of wind data from download of spatial CIMIS data, created in spatialCIMIS.R script
dim(U2)
sum(is.na(U2[,6:ncol(U2)])) #13,060 NAs as of 3/12/18; cell_148533 no longer a cell of interest, so these are all from 2/23/18 missing data
sum(U2[,6:ncol(U2)] < 0, na.rm = TRUE) #167 negative numbers
sum(U2[,6:ncol(U2)] == 0, na.rm = TRUE) #1 U2 observation equal to 0
#two different approaches to look at negative numbers
summary(unlist(U2[,6:ncol(U2)])[which(U2[,6:ncol(U2)] < 0)])
summary(U2[,6:ncol(U2)][U2[,6:ncol(U2)] < 0]) #this kind of indexing returns all the NAs also
U2[,6:ncol(U2)][U2[,6:ncol(U2)] < 0] <- 0 #change negatives to 0
#check for effect on NAs
sum(is.na(U2[,6:ncol(U2)])) #13,060 NAs as of 3/12/18
sum(U2[,6:ncol(U2)] == 0, na.rm = TRUE) #now 168 equal to 0 as a result of QC change
sum(U2[,6:ncol(U2)] > 0, na.rm = TRUE) #68,852,152
nrow(U2)*ncol(U2[,6:ncol(U2)]) #68,865,380
68852152+168+13060 #gives total above, positives + 0s + NAs
#gap fill NAs by making 2/23/18 average of all other Feb 23 in dataset by cell
test <- apply(U2[which(U2$month==2 & U2$day==23 & U2$year!=2018),6:ncol(U2)], 2, mean)
head(test)
length(test)
rm(test)
U2[U2$dates=='02_23_2018', 6:ncol(U2)] <- apply(U2[which(U2$month==2 & U2$day==23 & U2$year!=2018),6:ncol(U2)], 2, mean)
sum(is.na(U2[,6:ncol(U2)])) #0 NAs as of 3/12/18 after gap-filling
sum(U2[,6:ncol(U2)] == 0, na.rm = TRUE) #still 168 equal to 0 as a result of QC change
sum(U2[,6:ncol(U2)] > 0, na.rm = TRUE) #68,865,212
max(U2[,6:ncol(U2)]) #8.924 max m/s daily wind speed; this is 19.9 mph
hist(as.numeric(lapply(U2[,6:ncol(U2)], mean))) #centered over 2.0 m/s as expected
max(as.numeric(lapply(U2[,6:ncol(U2)], mean))) #windiest cell is 3.0 m/s across all days
min(as.numeric(lapply(U2[,6:ncol(U2)], mean))) #least windy cell is 1.3 m/as across all days
write.csv(U2, file.path(modelscaffoldDir, 'SpatialCIMIS.U2.QCpass.csv'), row.names = FALSE)

#old function to check for NAs in U2; not used March 2018
# na_indices_U2 <- lapply(U2[,6:ncol(U2)], function(x) {which(is.na(x))})
# for (i in 1:length(na_indices_U2)) {
#   if (length(na_indices_U2[[i]]) == 0) { #could set to 1 also as above
#     next
#   }
#   else(print(i))
# } #i=9524 is all NA; no other NAs present
# names(na_indices_U2)[10013] #"cell_148533" is NA; same for RHmin

#check the ETo data for missing, negative, and zero values, correcting along the way
ETo <- read.csv(file.path(modelscaffoldDir, 'SpatialCIMIS.EToupdate.rounded.csv'), stringsAsFactors = FALSE) #this is a daily summary of reference ET from download of spatial CIMIS data, created in spatialCIMIS.R script
dim(ETo)
sum(is.na(ETo[,6:ncol(ETo)])) #13,136 NAs as of 3/12/18; cell_148533 no longer a cell of interest, so all but 76 are from 2/23/18 missing data; this is 0.02% missing
sum(ETo[,6:ncol(ETo)] < 0, na.rm = TRUE) #1,633 negative numbers; this is 0.002% negative
sum(ETo[,6:ncol(ETo)] == 0, na.rm = TRUE) #11 observations equal to 0
max(ETo[,6:ncol(ETo)], na.rm = TRUE) #16.8 mm/day max
#strategy is to fill in 2/23/18 first; use mean of all previous days, excluding negative numbers
check2.23.data <- unlist(ETo[which(ETo$month==2 & ETo$day==23 & ETo$year!=2018),6:ncol(ETo)])
sum(check2.23.data < 0) #4 are negative, remove these below
sum(is.na(check2.23.data)) #0 are NA, so don't have to worry about that for 2/23/18 correction
rm(check2.23.data)
test <- apply(ETo[which(ETo$month==2 & ETo$day==23 & ETo$year!=2018),6:ncol(ETo)], 2, function(x) {mean(x[x>0])})
summary(test)
head(test)
length(test)
rm(test)
ETo[ETo$dates=='02_23_2018', 6:ncol(ETo)] <- apply(ETo[which(ETo$month==2 & ETo$day==23 & ETo$year!=2018),6:ncol(ETo)], 2, function(x) {mean(x[x>0])}) #reduces NA total to 76
sum(is.na(ETo[,6:ncol(ETo)]))
na_presence_ETo <- lapply(ETo[,6:ncol(ETo)], function(x) {sum(is.na(x))})
na_presence_ETo <- na_presence_ETo[na_presence_ETo > 0]
na_presence_ETo #7 cells have NAs
for (i in seq_along(na_presence_ETo)) {
  print(na_presence_ETo[i])
  print(ETo$dates[is.na(ETo[names(na_presence_ETo)[i]])])
} #they are all different dates
for (i in seq_along(na_presence_ETo)) {
  dates <- ETo$dates[is.na(ETo[names(na_presence_ETo)[i]])]
  for (j in seq_along(dates)) {
    day <- ETo$day[ETo$dates==dates[j]]
    month <- ETo$month[ETo$dates==dates[j]]
    gap.fill.data <- ETo[which(ETo$month==month & ETo$day==day & ETo$dates != '02_23_2018'), names(na_presence_ETo)[i]]
    ETo[ETo$dates==dates[j], names(na_presence_ETo)[i]] <- mean(gap.fill.data[gap.fill.data > 0], na.rm = TRUE) #remove any negative values from averages
  }
}

#now apply a negative value correction procedure
neg_presence_ETo <- lapply(ETo[,6:ncol(ETo)], function(x) {sum(x < 0)})
neg_presence_ETo <- neg_presence_ETo[neg_presence_ETo > 0]
neg_presence_ETo
length(neg_presence_ETo) #504 cells with negative ETo values
for (i in seq_along(neg_presence_ETo)) {
  print(neg_presence_ETo[i])
  #print(ETo$dates[is.na(ETo[names(neg_presence_ETo)[i]])])
} #they are all different dates
for (i in seq_along(neg_presence_ETo)) {
  dates <- ETo$dates[ETo[names(neg_presence_ETo)[i]] < 0]
  for (j in seq_along(dates)) {
    day <- ETo$day[ETo$dates==dates[j]]
    month <- ETo$month[ETo$dates==dates[j]]
    gap.fill.data <- ETo[which(ETo$month==month & ETo$day==day & ETo$dates != '02_23_2018'), names(neg_presence_ETo)[i]]
    ETo[ETo$dates==dates[j], names(neg_presence_ETo)[i]] <- mean(gap.fill.data[gap.fill.data > 0], na.rm = TRUE) #remove any negative values or zeroes before calculating averages to gap fill
  }
}
sum(is.na(ETo[,6:ncol(ETo)])) #good
sum(ETo[,6:ncol(ETo)] < 0, na.rm = TRUE) #good
sum(ETo[,6:ncol(ETo)] == 0, na.rm = TRUE) #still have 11

#now, correct the zeroes
zero_presence_ETo <- lapply(ETo[,6:ncol(ETo)], function(x) {sum(x == 0)})
zero_presence_ETo <- zero_presence_ETo[zero_presence_ETo > 0] #identify cells that have at least 1 count of a zero value for ETo across all days
zero_presence_ETo
length(zero_presence_ETo) #11 cells with ETo values of zero 1 day
for (i in seq_along(zero_presence_ETo)) {
  print(zero_presence_ETo[i])
  print(ETo$dates[ETo[names(zero_presence_ETo)[i]]==0])
} #they are all different dates
for (i in seq_along(zero_presence_ETo)) {
  dates <- ETo$dates[ETo[names(zero_presence_ETo)[i]] == 0]
  for (j in seq_along(dates)) {
    day <- ETo$day[ETo$dates==dates[j]]
    month <- ETo$month[ETo$dates==dates[j]]
    gap.fill.data <- ETo[which(ETo$month==month & ETo$day==day & ETo$dates != '02_23_2018'), names(zero_presence_ETo)[i]]
    ETo[ETo$dates==dates[j], names(zero_presence_ETo)[i]] <- mean(gap.fill.data[gap.fill.data > 0], na.rm = TRUE) #remove any negative values or zeroes before calculating averages to gap fill
  }
}
sum(is.na(ETo[,6:ncol(ETo)])) #still good
sum(ETo[,6:ncol(ETo)] < 0, na.rm = TRUE) #still good
sum(ETo[,6:ncol(ETo)] == 0, na.rm = TRUE) #good now
max(ETo[,6:ncol(ETo)]) #max 16.759 mm/day;
hist(as.numeric(lapply(ETo[,6:ncol(ETo)], mean))) #centered over 3.8-4.0 mm/day
max(as.numeric(lapply(ETo[,6:ncol(ETo)], mean))) #max 6.3 mm/day across all days for one cell
min(as.numeric(lapply(ETo[,6:ncol(ETo)], mean))) #min 2.3 mm/day across all days for one cell
write.csv(ETo, file.path(modelscaffoldDir, 'SpatialCIMIS.ETo.QCpass.csv'), row.names = FALSE)
#match this fname SpatialCIMIS.U2.QCpass.csv

#old NA correction work from 2017
# na_indices_ETo <- lapply(ETo, function(x) {which(is.na(x))})
# j <- c()
# for (i in 1:length(na_indices_ETo)) {
#   if (length(na_indices_ETo[[i]]) == 0) { #could set to 1 also as above
#     next
#   }
#   else if (length(j) == 0) {
#     j <- i
#     next
#   }
#   else{j <- c(j,i)}
# }
# for (i in 1:length(j)) {
#   print(na_indices_ETo[j][i])
# } #10 cells have NA values, one of which is cell_148533
# #for example,
# ETo[2600:2620, j[1]]
# #run loop to replace NAs
# j <- j[-5] #get rid of index referring to column 10018; it's all NA
# #in a loop, j[i] returns the column index
# column_indices <- j
# i <- 1
# k <- 1
# for (i in 1:length(column_indices)) {
#   row_indices <- unlist(na_indices_ETo[column_indices[i]], use.names = F)
#   for (k in 1:length(row_indices)){
#      doy <- ETo$DOY[row_indices[k]]
#      doy_indices <- which(ETo$DOY==doy)
#      gap_fill <- mean(ETo[doy_indices, column_indices[i]], na.rm = TRUE)
#      ETo[row_indices[k], column_indices[i]] <- gap_fill
#   }
# }


#code to check data updates for problems
#precip <- read.csv('PRISM_precip_data.csv') #this is a daily summary of precip from 10/1/2003-6/25/17 from 'free' daily PRISM 4km resolution for cells of interest in California, created in download_PRISM.R script (from 6/26/17 download)
which(is.na(precip)) #no NAs in PRISM as of 6/26/17

setwd(file.path(modelscaffoldDir, 'SpCIMIS'))
RHmin <- read.csv('SpatialCIMIS.minRHupdate.rounded.csv', stringsAsFactors = F) #this is a daily summary of minimum relative humidity, estimated from download of spatial CIMIS Tdew and Tmax data, created in spatialCIMIS.R script
na_indices_RHmin <- lapply(RHmin[,6:ncol(RHmin)], function(x) {which(is.na(x))})
head(na_indices_RHmin)
which(names(RHmin)=='cell_148533')
head(RHmin$cell_148533)
head(RHmin[,10018])
for (i in 1:length(na_indices_RHmin)) {
  if (length(na_indices_RHmin[[i]]) == 1) {
    next
  }
  else(print(i))
}
#all ok except cell_148533

U2 <- read.csv('SpatialCIMIS.U2update.rounded.csv') #this is a daily summary of wind data from download of spatial CIMIS data, created in spatialCIMIS.R script
na_indices_U2 <- lapply(U2[,6:ncol(U2)], function(x) {which(is.na(x))})
for (i in 1:length(na_indices_U2)) {
  if (length(na_indices_U2[[i]]) == 0) { #could set to 1 also as above
    next
  }
  else(print(i))
} #i=9524 is all NA; no other NAs present
names(na_indices_U2)[10013] #"cell_148533" is NA; same for RHmin
#no changes made to 'SpatialCIMIS_U2_rounded.csv'

ETo <- read.csv('SpatialCIMIS.EToupdate.rounded.csv') #this is a daily summary of reference ET from download of spatial CIMIS data, created in spatialCIMIS.R script
na_indices_ETo <- lapply(ETo, function(x) {which(is.na(x))})
j <- c()
for (i in 1:length(na_indices_ETo)) {
  if (length(na_indices_ETo[[i]]) == 0) { #could set to 1 also as above
    next
  }
  else if (length(j) == 0) {
    j <- i
    next
  }
  else{j <- c(j,i)}
}
for (i in 1:length(j)) {
  print(na_indices_ETo[j][i])
} #only cell_148533 has NA
#proceed with all SpCiMIS update files as is (9/13/2017)

#tests for assurance of gap-filling (these worked 6/26/17)
ETo$cell_77141[2600:2620]
ETo$DOY[2601]
i <- which(ETo$DOY==317)
mean(ETo$cell_77141[i], na.rm = TRUE)
#1.324769 should replace NA at ETo$cell_77141[2601]
ETo$cell_105651[3049]
ETo$DOY[3049]
i <- which(ETo$DOY==35)
mean(ETo$cell_105651[i], na.rm = TRUE)
#1.157308 should replace NA at ETo$cell_105651[3049]

#these were the original NAs in spatial CIMIS ETo
#$cell_77141
#[1] 2601 2602 2603 2612 2613 2614 2615

#$cell_105651
#[1] 3037 3038 3039 3040 3041 3042 3043 3044 3045 3046 3047 3048 3049 3050 3051

#$cell_136867
#[1] 2956 2957 2958 2959 2960 2961 2962 2963 2964 2965 2966 2967 2968 2969 2970

#$cell_139918
#[1] 2742 2743 2744 2745 2746 2747 2748 2749 2750 2751 2752 2753 2754 2755 2756

#$cell_189441
#[1] 3660 3661 3662 3664 3665 3666 3667 3668 3669 3671 3672 3673 3674

#$cell_246668
#[1] 3082 3083 3084 3085 3086 3087 3088 3089 3090 3091 3092 3093 3094 3095 3096

#$cell_254393
#[1] 2726 2727 2728 2729 2730 2731 2732 2733 2734 2735 2736 2737 2738 2739 2740

#$cell_264587
#[1] 4975

#$cell_264588
#[1] 4243 4608
