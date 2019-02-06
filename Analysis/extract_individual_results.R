library(extrafont)
library(extrafontdb)
#font_import() #only needs to be done one time after updating and re-installing R and moving and updating packages
loadfonts(device = 'win') #once per session
dissertationDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/dissertation/v2.results'
modelscaffoldDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/model_scaffold/run_model/Mar2018'
model.scaffold <- read.csv(file.path(modelscaffoldDir, 'model_scaffold_majcomps2018-03-15.csv'), stringsAsFactors = FALSE)
workDir <- 'D:/Allowable_Depletion/results/Mar2018/almond.mature_majcomps/scenario_0.5m30AD'
workDir2 <- 'D:/Allowable_Depletion/results/Mar2018/almond.mature_majcomps/scenario_1.0m50AD'
workDir3 <- 'D:/Allowable_Depletion/results/Mar2018/almond.mature_majcomps/scenario_2.0m50AD'
workDir4 <- 'D:/Allowable_Depletion/results/Mar2018/almond.mature_majcomps/scenario_0.5m50AD'
list.files(workDir)
fnames <- list.files(workDir)[1:273]
fnames_full <- list.files(workDir, full.names = TRUE)[1:273]
model_codes <- unlist(lapply(fnames, function(x) paste0(unlist(strsplit(x, split=''))[23:28], collapse = '')))
model.scaffold.lite <- model.scaffold[model.scaffold$unique_model_code %in% model_codes, ]
dim(model.scaffold.lite) #314 x 37, because some have more than one majcomp
model.scaffold.lite[model.scaffold.lite$compname=='Hanford',]

almonds_results_0.5m_30AD <- read.csv(file.path(list.files(workDir, full.names = TRUE)[274]), stringsAsFactors = FALSE)
almonds_results_0.5m_30AD[almonds_results_0.5m_30AD$unique_model_code==122607, c('Model.Year', 'GW.ET.growing')]
mean(almonds_results_0.5m_30AD$GW.ET.growing[almonds_results_0.5m_30AD$unique_model_code==122607][3:15], na.rm=TRUE) #86.7
almonds_results_0.5m_30AD[almonds_results_0.5m_30AD$unique_model_code==169885, c('Model.Year', 'GW.ET.growing')]
mean(almonds_results_0.5m_30AD$GW.ET.growing[almonds_results_0.5m_30AD$unique_model_code==169885][3:15], na.rm=TRUE) #126.9

almonds_results_1.0m_50AD <- read.csv(file.path(list.files(workDir2, full.names = TRUE)[274]), stringsAsFactors = FALSE)
mean(almonds_results_1.0m_50AD$GW.ET.growing[almonds_results_1.0m_50AD$unique_model_code==125111][3:15], na.rm=TRUE) #122.7
mean(almonds_results_1.0m_50AD$GW.ET.growing[almonds_results_1.0m_50AD$unique_model_code==169885][3:15], na.rm=TRUE) #184.8

almonds_results_2.0m_50AD <- read.csv(file.path(list.files(workDir3, full.names = TRUE)[274]), stringsAsFactors = FALSE)
mean(almonds_results_2.0m_50AD$GW.ET.growing[almonds_results_2.0m_50AD$unique_model_code==125111][3:15], na.rm=TRUE) #140.9
mean(almonds_results_2.0m_50AD$GW.ET.growing[almonds_results_2.0m_50AD$unique_model_code==169885][3:15], na.rm=TRUE) #228.8

index_122607 <- which(model_codes==122607)
Hanford_southernDWR_0.5mAD30 <- read.csv(fnames_full[index_122607], stringsAsFactors = FALSE)
plot(Hanford_southernDWR_0.5mAD30$Dr.end[which(Hanford_southernDWR_0.5mAD30$dates=='2005-10-01'):which(Hanford_southernDWR_0.5mAD30$dates=='2006-12-31')], type='l')


#southern DWR plot
Hanford_southernDWR_1.0mAD50 <- read.csv(list.files(workDir2, full.names = TRUE)[index_122607], stringsAsFactors = FALSE)
Hanford_southernDWR_2.0mAD50 <- read.csv(list.files(workDir3, full.names = TRUE)[index_122607], stringsAsFactors = FALSE)
plot(as.Date(Hanford_southernDWR_0.5mAD30$dates[which(Hanford_southernDWR_2.0mAD50$dates=='2005-10-01'):which(Hanford_southernDWR_2.0mAD50$dates=='2007-02-15')], '%Y-%m-%d'), -1*Hanford_southernDWR_2.0mAD50$Dr.end[which(Hanford_southernDWR_2.0mAD50$dates=='2005-10-01'):which(Hanford_southernDWR_2.0mAD50$dates=='2007-02-15')], type='l', col='green3', xlab='', xaxt='n', ylab='Soil water depletion (mm)', yaxt='n', ylim=c(-max(Hanford_southernDWR_2.0mAD50$Dr.end[which(Hanford_southernDWR_2.0mAD50$dates=='2005-10-01'):which(Hanford_southernDWR_2.0mAD50$dates=='2006-12-31')]), 5))
lines(as.Date(Hanford_southernDWR_0.5mAD30$dates[which(Hanford_southernDWR_2.0mAD50$dates=='2005-10-01'):which(Hanford_southernDWR_2.0mAD50$dates=='2007-02-15')], '%Y-%m-%d'), -1*Hanford_southernDWR_1.0mAD50$Dr.end[which(Hanford_southernDWR_1.0mAD50$dates=='2005-10-01'):which(Hanford_southernDWR_1.0mAD50$dates=='2007-02-15')], col='blue')
lines(as.Date(Hanford_southernDWR_0.5mAD30$dates[which(Hanford_southernDWR_2.0mAD50$dates=='2005-10-01'):which(Hanford_southernDWR_2.0mAD50$dates=='2007-02-15')], '%Y-%m-%d'), -1*Hanford_southernDWR_0.5mAD30$Dr.end[which(Hanford_southernDWR_0.5mAD30$dates=='2005-10-01'):which(Hanford_southernDWR_0.5mAD30$dates=='2007-02-15')], col='brown')
axis(side = 2, at=c(0, -50, -100, -150), labels = c(0, 50, 100, 150))
axis.Date(side = 1, at=seq.Date(from = as.Date('2005/10/01'), to = as.Date('2007/02/15'), by='months'), format = '%m/%d/%y')
axis(side = 4, at = c(-160, -140, -120, -100), labels = c('0', '10', '20', '30', '40'))
mtext("precipitation per day (mm)", side=4, line=2.5, at=-100)
lines(as.Date(Hanford_southernDWR_0.5mAD30$dates[which(Hanford_southernDWR_2.0mAD50$dates=='2005-10-01'):which(Hanford_southernDWR_2.0mAD50$dates=='2007-02-15')], '%Y-%m-%d'),  (2 * Hanford_southernDWR_2.0mAD50$P[which(Hanford_southernDWR_2.0mAD50$dates=='2005-10-01'):which(Hanford_southernDWR_2.0mAD50$dates=='2007-02-15')] - 160), type='s', col='lightblue', cex=0.5)


#northern Hanford plot
index_169885 <- which(model_codes==169885)
Hanford_northernDWR_0.5mAD30 <- read.csv(fnames_full[index_169885], stringsAsFactors = FALSE)
Hanford_northernAD_0.5m_30 <- model.scaffold$z0.5m_cmH2O_modified_comp[model.scaffold$unique_model_code==169885] * 10 * 0.3
Hanford_northernDWR_1.0mAD50 <- read.csv(list.files(workDir2, full.names = TRUE)[index_169885], stringsAsFactors = FALSE)
Hanford_northernAD_1.0m_50 <- model.scaffold$z1.0m_cmH2O_modified_comp[model.scaffold$unique_model_code==169885] * 10 * 0.5
Hanford_northernDWR_2.0mAD50 <- read.csv(list.files(workDir3, full.names = TRUE)[index_169885], stringsAsFactors = FALSE)
Hanford_northernAD_2.0m_50 <- model.scaffold$z2.0m_cmH2O_modified_comp[model.scaffold$unique_model_code==169885] * 10 * 0.5

#get first irr dates in 2006 and 2007
firstirr2006_Hanford_northernDWR_0.5mAD30 <- as.Date(Hanford_northernDWR_0.5mAD30$dates[which(Hanford_northernDWR_0.5mAD30$years==2006 & Hanford_northernDWR_0.5mAD30$Ir>0)][1], '%Y-%m-%d')
firstirr2006_Hanford_northernDWR_1.0mAD50 <- as.Date(Hanford_northernDWR_1.0mAD50$dates[which(Hanford_northernDWR_1.0mAD50$years==2006 & Hanford_northernDWR_1.0mAD50$Ir>0)][1], '%Y-%m-%d')
firstirr2006_Hanford_northernDWR_2.0mAD50 <- as.Date(Hanford_northernDWR_2.0mAD50$dates[which(Hanford_northernDWR_2.0mAD50$years==2006 & Hanford_northernDWR_2.0mAD50$Ir>0)][1], '%Y-%m-%d')

firstirr2007_Hanford_northernDWR_0.5mAD30 <- as.Date(Hanford_northernDWR_0.5mAD30$dates[which(Hanford_northernDWR_0.5mAD30$years==2007 & Hanford_northernDWR_0.5mAD30$Ir>0)][1], '%Y-%m-%d')
firstirr2007_Hanford_northernDWR_1.0mAD50 <- as.Date(Hanford_northernDWR_1.0mAD50$dates[which(Hanford_northernDWR_1.0mAD50$years==2007 & Hanford_northernDWR_1.0mAD50$Ir>0)][1], '%Y-%m-%d')
firstirr2007_Hanford_northernDWR_2.0mAD50 <- as.Date(Hanford_northernDWR_2.0mAD50$dates[which(Hanford_northernDWR_2.0mAD50$years==2007 & Hanford_northernDWR_2.0mAD50$Ir>0)][1], '%Y-%m-%d')

#get last irr dates
lastirr2006_Hanford_northernDWR_0.5mAD30 <- as.Date(tail(Hanford_northernDWR_0.5mAD30$dates[which(Hanford_northernDWR_0.5mAD30$years==2006 & Hanford_northernDWR_0.5mAD30$Ir>0)], 1), '%Y-%m-%d')
lastirr2006_Hanford_northernDWR_1.0mAD50 <- as.Date(tail(Hanford_northernDWR_1.0mAD50$dates[which(Hanford_northernDWR_1.0mAD50$years==2006 & Hanford_northernDWR_1.0mAD50$Ir>0)], 1), '%Y-%m-%d')
lastirr2006_Hanford_northernDWR_2.0mAD50 <- as.Date(tail(Hanford_northernDWR_2.0mAD50$dates[which(Hanford_northernDWR_2.0mAD50$years==2006 & Hanford_northernDWR_2.0mAD50$Ir>0)], 1), '%Y-%m-%d')

#get depletions at last irr
Dr_lastirr2006_Hanford_northernDWR_0.5mAD30 <- Hanford_northernDWR_0.5mAD30$Dr.end[Hanford_northernDWR_0.5mAD30$dates==as.character(lastirr2006_Hanford_northernDWR_0.5mAD30)]
Dr_lastirr2006_Hanford_northernDWR_1.0mAD50 <- Hanford_northernDWR_1.0mAD50$Dr.end[Hanford_northernDWR_1.0mAD50$dates==as.character(lastirr2006_Hanford_northernDWR_1.0mAD50)]
Dr_lastirr2006_Hanford_northernDWR_2.0mAD50 <- Hanford_northernDWR_2.0mAD50$Dr.end[Hanford_northernDWR_2.0mAD50$dates==as.character(lastirr2006_Hanford_northernDWR_2.0mAD50)]

#make Figure 8 for Green Water chapter
tiff(file = file.path(dissertationDir, 'figures', 'Hanford_169885_depletion1.29.19.tif'), family = 'Times New Roman', pointsize = 11, width = 9, height = 4, units = 'in', res=150)
par(mar=c(2.25, 4.5, 3, 4.5))
plot(as.Date(Hanford_northernDWR_0.5mAD30$dates[which(Hanford_northernDWR_2.0mAD50$dates=='2005-10-01'):which(Hanford_northernDWR_2.0mAD50$dates=='2007-03-15')], '%Y-%m-%d'), -1*Hanford_northernDWR_2.0mAD50$Dr.end[which(Hanford_northernDWR_2.0mAD50$dates=='2005-10-01'):which(Hanford_northernDWR_2.0mAD50$dates=='2007-03-15')], type='l', col='green3', xlab='', xaxt='n', xlim=c(as.Date('2005/10/15'), as.Date('2007/03/01')), ylab='Soil water depletion (mm)', yaxt='n', ylim=c(-max(Hanford_northernDWR_2.0mAD50$Dr.end[which(Hanford_northernDWR_2.0mAD50$dates=='2005-10-01'):which(Hanford_northernDWR_2.0mAD50$dates=='2006-12-31')]), 5))
lines(as.Date(Hanford_northernDWR_0.5mAD30$dates[which(Hanford_northernDWR_2.0mAD50$dates=='2005-10-01'):which(Hanford_northernDWR_2.0mAD50$dates=='2007-03-15')], '%Y-%m-%d'), -1*Hanford_northernDWR_1.0mAD50$Dr.end[which(Hanford_northernDWR_1.0mAD50$dates=='2005-10-01'):which(Hanford_northernDWR_1.0mAD50$dates=='2007-03-15')], col='blue')
lines(as.Date(Hanford_northernDWR_0.5mAD30$dates[which(Hanford_northernDWR_2.0mAD50$dates=='2005-10-01'):which(Hanford_northernDWR_2.0mAD50$dates=='2007-03-15')], '%Y-%m-%d'), -1*Hanford_northernDWR_0.5mAD30$Dr.end[which(Hanford_northernDWR_0.5mAD30$dates=='2005-10-01'):which(Hanford_northernDWR_0.5mAD30$dates=='2007-03-15')], col='brown')
abline(h=-Hanford_northernAD_0.5m_30, lty=2, col='brown')
abline(h=-Hanford_northernAD_1.0m_50, lty=2, col='blue')
abline(h=-Hanford_northernAD_2.0m_50, lty=2, col='green3')
abline(v=as.Date('2005/11/11'), lty=2)
abline(v=as.Date('2006/02/15'), lty=2)
abline(v=as.Date('2006/11/11'), lty=2)
abline(v=as.Date('2007/02/15'), lty=2)
axis(side = 2, at=c(0, -50, -100, -150), labels = c(0, 50, 100, 150))
axis.Date(side = 1, at=seq.Date(from = as.Date('2005/10/01'), to = as.Date('2007/03/15'), by='months'), format = '%m/%d/%y')
axis(side = 4, at = c(-160, -140, -120, -100), labels = c('0', '10', '20', '30'))
mtext(expression('precipitation '~day^-1~"(mm)"), side=4, line=2.5, at=-100)
lines(as.Date(Hanford_northernDWR_0.5mAD30$dates[which(Hanford_northernDWR_2.0mAD50$dates=='2005-10-01'):which(Hanford_northernDWR_2.0mAD50$dates=='2007-03-15')], '%Y-%m-%d'),  (2 * Hanford_northernDWR_2.0mAD50$P[which(Hanford_northernDWR_2.0mAD50$dates=='2005-10-01'):which(Hanford_northernDWR_2.0mAD50$dates=='2007-03-15')] - 160), type='s', col='lightblue', cex=0.5)
text(as.Date('2005/11/15'), 2, 'dormancy', adj = c(0,0))
text(as.Date('2006/02/19'), 2, 'bloom', adj = c(0,0))
text(as.Date('2006/11/15'), 2, 'dormancy', adj = c(0,0))
text(as.Date('2007/02/19'), 2, 'bloom', adj = c(0,0))
text(as.Date('2006/02/19'), -90, 'First', adj = c(0,0))
text(as.Date('2006/02/19'), -103, 'Irrigation', adj = c(0,0))
text(as.Date('2006/11/09'), -115, 'Last', adj = c(1,0))
text(as.Date('2006/11/09'), -128, 'Irrigation', adj = c(1,0))
text(as.Date('2007/01/28'), -90, 'First', adj = c(1,0))
text(as.Date('2007/01/28'), -103, 'Irrigation', adj = c(1,0))
arrows(x0=as.Date('2006/03/26'), y0=-92, x1 = firstirr2006_Hanford_northernDWR_0.5mAD30, y1 = -Hanford_northernAD_0.5m_30, length=0.1, lwd=1.3)
arrows(x0=as.Date('2006/03/26'), y0=-92, x1 = firstirr2006_Hanford_northernDWR_1.0mAD50, y1 = -Hanford_northernAD_1.0m_50, length=0.1, lwd=1.3)
arrows(x0=as.Date('2006/03/26'), y0=-92, x1 = firstirr2006_Hanford_northernDWR_2.0mAD50, y1 = -Hanford_northernAD_2.0m_50, length=0.1, lwd=1.3)
arrows(x0=as.Date('2006/10/20'), y0=-115, x1 = lastirr2006_Hanford_northernDWR_0.5mAD30, y1 = -Dr_lastirr2006_Hanford_northernDWR_0.5mAD30, length=0.1, lwd=1.3)
arrows(x0=as.Date('2006/10/20'), y0=-115, x1 = lastirr2006_Hanford_northernDWR_1.0mAD50, y1 = -Dr_lastirr2006_Hanford_northernDWR_1.0mAD50, length=0.1, lwd=1.3)
arrows(x0=as.Date('2006/10/20'), y0=-115, x1 = lastirr2006_Hanford_northernDWR_2.0mAD50, y1 = -Dr_lastirr2006_Hanford_northernDWR_2.0mAD50, length=0.1, lwd=1.3)
arrows(x0=as.Date('2007/02/01'), y0=-92, x1 = firstirr2007_Hanford_northernDWR_0.5mAD30, y1 = -Hanford_northernAD_0.5m_30, length=0.1, lwd=1.3)
arrows(x0=as.Date('2007/02/01'), y0=-92, x1 = firstirr2007_Hanford_northernDWR_1.0mAD50, y1 = -Hanford_northernAD_1.0m_50, length=0.1, lwd=1.3)
arrows(x0=as.Date('2007/02/01'), y0=-92, x1 = firstirr2007_Hanford_northernDWR_2.0mAD50, y1 = -Hanford_northernAD_2.0m_50, length=0.1, lwd=1.3)
legend(x=as.Date('2005/09/28'), y=35, legend = c('Shallow (0.5 m and 30% AD)', 'Moderate (1.0 m and 50% AD)', 'Deep (2.0 m and 50% AD)'), lty=1, col=c('brown', 'blue', 'green3'), lwd=1.4, ncol=3, xpd=TRUE)
dev.off()
#legend('bottomright', legend=(c("< 1200", '1200-1410', '>1410')), lty=1, col=c('blue', 'orange2', 'red3'), title = expression(paste('annual kWh ', m^2)), inset=0.01)


###northern Hanford plot take 2 with 0.5 m x 50% AD replacing shallow scenario
allowable_depletion <- 0.5
index_169885 <- which(model_codes==169885)
Hanford_northernDWR_0.5mAD50 <- read.csv(list.files(workDir4, full.names = TRUE)[index_169885], stringsAsFactors = FALSE)
Hanford_northernAD_0.5m_50 <- model.scaffold$z0.5m_cmH2O_modified_comp[model.scaffold$unique_model_code==169885] * 10 * allowable_depletion
Hanford_northernDWR_1.0mAD50 <- read.csv(list.files(workDir2, full.names = TRUE)[index_169885], stringsAsFactors = FALSE)
Hanford_northernAD_1.0m_50 <- model.scaffold$z1.0m_cmH2O_modified_comp[model.scaffold$unique_model_code==169885] * 10 * allowable_depletion
Hanford_northernDWR_2.0mAD50 <- read.csv(list.files(workDir3, full.names = TRUE)[index_169885], stringsAsFactors = FALSE)
Hanford_northernAD_2.0m_50 <- model.scaffold$z2.0m_cmH2O_modified_comp[model.scaffold$unique_model_code==169885] * 10 * allowable_depletion

#get first irr dates in 2006 and 2007
firstirr2006_Hanford_northernDWR_0.5mAD50 <- as.Date(Hanford_northernDWR_0.5mAD50$dates[which(Hanford_northernDWR_0.5mAD50$years==2006 & Hanford_northernDWR_0.5mAD50$Ir>0)][1], '%Y-%m-%d')
firstirr2006_Hanford_northernDWR_1.0mAD50 <- as.Date(Hanford_northernDWR_1.0mAD50$dates[which(Hanford_northernDWR_1.0mAD50$years==2006 & Hanford_northernDWR_1.0mAD50$Ir>0)][1], '%Y-%m-%d')
firstirr2006_Hanford_northernDWR_2.0mAD50 <- as.Date(Hanford_northernDWR_2.0mAD50$dates[which(Hanford_northernDWR_2.0mAD50$years==2006 & Hanford_northernDWR_2.0mAD50$Ir>0)][1], '%Y-%m-%d')

firstirr2007_Hanford_northernDWR_0.5mAD50 <- as.Date(Hanford_northernDWR_0.5mAD50$dates[which(Hanford_northernDWR_0.5mAD50$years==2007 & Hanford_northernDWR_0.5mAD50$Ir>0)][1], '%Y-%m-%d')
firstirr2007_Hanford_northernDWR_1.0mAD50 <- as.Date(Hanford_northernDWR_1.0mAD50$dates[which(Hanford_northernDWR_1.0mAD50$years==2007 & Hanford_northernDWR_1.0mAD50$Ir>0)][1], '%Y-%m-%d')
firstirr2007_Hanford_northernDWR_2.0mAD50 <- as.Date(Hanford_northernDWR_2.0mAD50$dates[which(Hanford_northernDWR_2.0mAD50$years==2007 & Hanford_northernDWR_2.0mAD50$Ir>0)][1], '%Y-%m-%d')

#get last irr dates
lastirr2006_Hanford_northernDWR_0.5mAD50 <- as.Date(tail(Hanford_northernDWR_0.5mAD50$dates[which(Hanford_northernDWR_0.5mAD50$years==2006 & Hanford_northernDWR_0.5mAD50$Ir>0)], 1), '%Y-%m-%d')
lastirr2006_Hanford_northernDWR_1.0mAD50 <- as.Date(tail(Hanford_northernDWR_1.0mAD50$dates[which(Hanford_northernDWR_1.0mAD50$years==2006 & Hanford_northernDWR_1.0mAD50$Ir>0)], 1), '%Y-%m-%d')
lastirr2006_Hanford_northernDWR_2.0mAD50 <- as.Date(tail(Hanford_northernDWR_2.0mAD50$dates[which(Hanford_northernDWR_2.0mAD50$years==2006 & Hanford_northernDWR_2.0mAD50$Ir>0)], 1), '%Y-%m-%d')

#get depletions at last irr
Dr_lastirr2006_Hanford_northernDWR_0.5mAD50 <- Hanford_northernDWR_0.5mAD50$Dr.end[Hanford_northernDWR_0.5mAD50$dates==as.character(lastirr2006_Hanford_northernDWR_0.5mAD50)]
Dr_lastirr2006_Hanford_northernDWR_1.0mAD50 <- Hanford_northernDWR_1.0mAD50$Dr.end[Hanford_northernDWR_1.0mAD50$dates==as.character(lastirr2006_Hanford_northernDWR_1.0mAD50)]
Dr_lastirr2006_Hanford_northernDWR_2.0mAD50 <- Hanford_northernDWR_2.0mAD50$Dr.end[Hanford_northernDWR_2.0mAD50$dates==as.character(lastirr2006_Hanford_northernDWR_2.0mAD50)]

#make Figure 8 for Green Water chapter
tiff(file = file.path(dissertationDir, 'figures', 'Hanford_169885_depletion1.30.19.tif'), family = 'Times New Roman', pointsize = 11, width = 9, height = 4, units = 'in', res=150)
par(mar=c(2.25, 4.5, 3, 4.5))
plot(as.Date(Hanford_northernDWR_0.5mAD50$dates[which(Hanford_northernDWR_2.0mAD50$dates=='2005-10-01'):which(Hanford_northernDWR_2.0mAD50$dates=='2007-03-15')], '%Y-%m-%d'), -1*Hanford_northernDWR_2.0mAD50$Dr.end[which(Hanford_northernDWR_2.0mAD50$dates=='2005-10-01'):which(Hanford_northernDWR_2.0mAD50$dates=='2007-03-15')], type='l', col='green3', xlab='', xaxt='n', xlim=c(as.Date('2005/10/15'), as.Date('2007/03/01')), ylab='soil water depletion (mm)', yaxt='n', ylim=c(-max(Hanford_northernDWR_2.0mAD50$Dr.end[which(Hanford_northernDWR_2.0mAD50$dates=='2005-10-01'):which(Hanford_northernDWR_2.0mAD50$dates=='2006-12-31')]), 5))
lines(as.Date(Hanford_northernDWR_0.5mAD50$dates[which(Hanford_northernDWR_2.0mAD50$dates=='2005-10-01'):which(Hanford_northernDWR_2.0mAD50$dates=='2007-03-15')], '%Y-%m-%d'), -1*Hanford_northernDWR_1.0mAD50$Dr.end[which(Hanford_northernDWR_1.0mAD50$dates=='2005-10-01'):which(Hanford_northernDWR_1.0mAD50$dates=='2007-03-15')], col='blue')
lines(as.Date(Hanford_northernDWR_0.5mAD50$dates[which(Hanford_northernDWR_2.0mAD50$dates=='2005-10-01'):which(Hanford_northernDWR_2.0mAD50$dates=='2007-03-15')], '%Y-%m-%d'), -1*Hanford_northernDWR_0.5mAD50$Dr.end[which(Hanford_northernDWR_0.5mAD50$dates=='2005-10-01'):which(Hanford_northernDWR_0.5mAD50$dates=='2007-03-15')], col='brown')
abline(h=-Hanford_northernAD_0.5m_50, lty=2, col='brown')
abline(h=-Hanford_northernAD_1.0m_50, lty=2, col='blue')
abline(h=-Hanford_northernAD_2.0m_50, lty=2, col='green3')
abline(v=as.Date('2005/11/11'), lty=2)
abline(v=as.Date('2006/02/15'), lty=2)
abline(v=as.Date('2006/11/11'), lty=2)
abline(v=as.Date('2007/02/15'), lty=2)
axis(side = 2, at=c(0, -50, -100, -150), labels = c(0, 50, 100, 150))
axis.Date(side = 1, at=seq.Date(from = as.Date('2005/10/01'), to = as.Date('2007/03/15'), by='months'), format = '%m/%d/%y')
axis(side = 4, at = c(-160, -140, -120, -100), labels = c('0', '10', '20', '30'))
mtext(expression('precipitation '~day^-1~"(mm)"), side=4, line=2.5, at=-125)
lines(as.Date(Hanford_northernDWR_0.5mAD50$dates[which(Hanford_northernDWR_2.0mAD50$dates=='2005-10-01'):which(Hanford_northernDWR_2.0mAD50$dates=='2007-03-15')], '%Y-%m-%d'),  (2 * Hanford_northernDWR_2.0mAD50$P[which(Hanford_northernDWR_2.0mAD50$dates=='2005-10-01'):which(Hanford_northernDWR_2.0mAD50$dates=='2007-03-15')] - 160), type='s', col='lightblue', cex=0.5)
text(as.Date('2005/11/15'), 2, 'dormancy', adj = c(0,0))
text(as.Date('2006/02/19'), 2, 'bloom', adj = c(0,0))
text(as.Date('2006/11/15'), 2, 'dormancy', adj = c(0,0))
text(as.Date('2007/02/19'), 2, 'bloom', adj = c(0,0))
text(as.Date('2006/02/19'), -90, 'First', adj = c(0,0))
text(as.Date('2006/02/19'), -103, 'Irrigation', adj = c(0,0))
text(as.Date('2006/11/09'), -115, 'Last', adj = c(1,0))
text(as.Date('2006/11/09'), -128, 'Irrigation', adj = c(1,0))
text(as.Date('2007/01/28'), -90, 'First', adj = c(1,0))
text(as.Date('2007/01/28'), -103, 'Irrigation', adj = c(1,0))
arrows(x0=as.Date('2006/03/26'), y0=-92, x1 = firstirr2006_Hanford_northernDWR_0.5mAD50, y1 = -Hanford_northernAD_0.5m_50, length=0.1, lwd=1.3)
arrows(x0=as.Date('2006/03/26'), y0=-92, x1 = firstirr2006_Hanford_northernDWR_1.0mAD50, y1 = -Hanford_northernAD_1.0m_50, length=0.1, lwd=1.3)
arrows(x0=as.Date('2006/03/26'), y0=-92, x1 = firstirr2006_Hanford_northernDWR_2.0mAD50, y1 = -Hanford_northernAD_2.0m_50, length=0.1, lwd=1.3)
arrows(x0=as.Date('2006/10/20'), y0=-115, x1 = lastirr2006_Hanford_northernDWR_0.5mAD50, y1 = -Dr_lastirr2006_Hanford_northernDWR_0.5mAD50, length=0.1, lwd=1.3)
arrows(x0=as.Date('2006/10/20'), y0=-115, x1 = lastirr2006_Hanford_northernDWR_1.0mAD50, y1 = -Dr_lastirr2006_Hanford_northernDWR_1.0mAD50, length=0.1, lwd=1.3)
arrows(x0=as.Date('2006/10/20'), y0=-115, x1 = lastirr2006_Hanford_northernDWR_2.0mAD50, y1 = -Dr_lastirr2006_Hanford_northernDWR_2.0mAD50, length=0.1, lwd=1.3)
arrows(x0=as.Date('2007/02/01'), y0=-92, x1 = firstirr2007_Hanford_northernDWR_0.5mAD50, y1 = -Hanford_northernAD_0.5m_50, length=0.1, lwd=1.3)
arrows(x0=as.Date('2007/02/01'), y0=-92, x1 = firstirr2007_Hanford_northernDWR_1.0mAD50, y1 = -Hanford_northernAD_1.0m_50, length=0.1, lwd=1.3)
arrows(x0=as.Date('2007/02/01'), y0=-92, x1 = firstirr2007_Hanford_northernDWR_2.0mAD50, y1 = -Hanford_northernAD_2.0m_50, length=0.1, lwd=1.3)
legend(x=as.Date('2005/08/28'), y=35, legend = c('0.5 m and 50% allowable depletion', '1.0 m and 50% allowable depletion', '2.0 m and 50% allowable depletion'), lty=1, col=c('brown', 'blue', 'green3'), lwd=1.4, ncol=3, xpd=TRUE)
dev.off()
#legend('bottomright', legend=(c("< 1200", '1200-1410', '>1410')), lty=1, col=c('blue', 'orange2', 'red3'), title = expression(paste('annual kWh ', m^2)), inset=0.01)
