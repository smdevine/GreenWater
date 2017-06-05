mainDir <- 'E:/Simulations'
results <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/HYDRUS_results/find_FC'
workdir <- file.path(mainDir, '30kto35K')
subdirs <- list.dirs(workdir) #5001 directories, skip first
qc_check_df <- as.data.frame(matrix(ncol = 3, nrow=(length(subdirs)-1)))
colnames(qc_check_df) <- c('chkey', 'water balance [%]', 'error.msg')
for (i in 2:length(subdirs)) {
  dir <- subdirs[i]
  dir_name <- basename(dir)
  dir_name <- strsplit(dir_name, "_")[[1]]
  qc_check_df[(i-1), 1] <- dir_name[3]
  fnames <- list.files(dir)
  if ('Error.msg' %in% fnames) {
    qc_check_df[(i-1), 3] <- 'Yes'
    next
  }
  else if (!('Error.msg' %in% fnames)) {
    qc_check_df[(i-1), 3] <- 'No'
  }
  setwd(dir)
  blnc <- read.table(fnames[1], nrows = 1, skip=31)
  if (length(blnc)==3) {
    qc_check_df[(i-1), 2] <- blnc[3]
    next
  }
  else {print(paste('chkey', dir_name[3], 'was missing Balance.out data but had no Error.msg'))}
}
setwd(results)
write.csv(qc_check_df, paste('QC_check_', basename(workdir), '.csv', sep = ''), row.names = FALSE)


fnames_results <- list.files(results)
for (i in 1:length(fnames_results)) {
  results_temp <- read.csv(fnames_results[i])
  if (i==1) {
    results_all <- results_temp
    next
  }
  results_all <- rbind(results_all, results_temp)
}
colnames(results_all)[2] <- 'water balance %'
results_all <- results_all[order(results_all$chkey), ]
write.csv(results_all, 'QC_check_all.csv', row.names = FALSE)
results_all <- read.csv('QC_check_all.csv')
colnames(results_all)[2] <- 'water balance %'
nrow(results_all)
i <- which(is.na(results_all$`water balance %`)) #231 are NA for water balance
j <- which(results_all$error.msg=='Yes') #22 have error messages
k <- which(results_all$`water balance %` > 1) #49 have water balance error > 1
l <- which(results_all$`water balance %` > 5) #45 have water balance error > 5

#now, find the water content when flux is at 0.1 and 0.01 cm/day
HYDRUS_FC <- function(wdir_name) {
  workdir <- file.path(mainDir, wdir_name)
  subdirs <- list.dirs(workdir) #5001 directories, skip first
  find_fc_df <- as.data.frame(matrix(ncol = 5, nrow=(length(subdirs)-1)))
  colnames(find_fc_df) <- c('chkey', 'flux', 'theta_fc', 'time', 'h')
  for (i in 2:length(subdirs)) {
    dir <- subdirs[i]
    dir_name <- basename(dir)
    dir_name <- strsplit(dir_name, "_")[[1]]
    find_fc_df[(i-1), 1] <- dir_name[3]
    fnames <- list.files(dir)
    if ('Error.msg' %in% fnames) {
      next
    }
    setwd(dir)
    blnc <- read.table(fnames[1], nrows = 1, skip=31)
    if (length(blnc)!=3) { #essentially, blnc length <3 is missing data
      next
    }
    if (blnc[3] > 5) { #If water balance is > 5% skip
      next
    }
    t_out <- as.matrix(read.table('T_Level.out', colClasses='numeric', nrow=10000, skip=9))
    flux_at_fc <- abs(t_out[ ,6] + 0.01) #this assumes all fluxes will be negative (in the downwards direction)
    t <- which.min(flux_at_fc)
    find_fc_df[(i-1), 2] <- t_out[t,6] #this is the flux when it's closest to -0.01 cm/day
    find_fc_df[(i-1), 3] <- t_out[t,17] #this is the theta at the same time (i.e. 'field capacity')
    find_fc_df[(i-1), 4] <- t_out[t,1]  #this is the time when this occurs
    find_fc_df[(i-1), 5] <- mean(t_out[t,12], t_out[t,14]) #this is average h of top and bottom
  }
  setwd(results)
  write.csv(find_fc_df, paste('FC_values_', basename(workdir), '.csv', sep = ''), row.names = FALSE)
}
HYDRUS_FC('5Kto10K')
HYDRUS_FC('10Kto15K')
HYDRUS_FC('15Kto20K')
HYDRUS_FC('20Kto25K')
HYDRUS_FC('25Kto30K')
HYDRUS_FC('30Kto35K')

setwd(results)
fnames_results <- list.files(results)
for (i in 1:length(fnames_results)) {
  results_temp <- read.csv(fnames_results[i])
  if (i==1) {
    results_fc <- results_temp
    next
  }
  results_fc <- rbind(results_fc, results_temp)
}
results_fc <- results_fc[order(results_fc$chkey), ]
colnames(results_fc)[2] <- 'flux_cm_day' 
colnames(results_fc)[4] <- 'time_days'
colnames(results_fc)[5] <- 'h_cm'
write.csv(results_fc, 'results_fc_all.csv', row.names = FALSE)
nrow(results_fc)
i <- which(is.na(results_fc$theta_fc)) #272 are NA for theta FC
length(i)

results_fc <- read.csv('results_fc_all.csv')