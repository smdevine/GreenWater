options(mc.cores = 20)
source("FAO56_dualcropcoeff.v2.R")

cropscape_legend <- read.csv(file.path(modelscaffoldDir, "cropscape_legend.txt"), stringsAsFactors = FALSE)
alfalfa_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME == "Alfalfa"]
grape_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME == "Grapes"]
almond_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME == "Almonds"]
walnut_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME == "Walnuts"]
pistachio_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME == "Pistachios"]
modelgrid <- expand.grid(root.depths = c("0.5m", "1.0m", "2.0m", "3.0m"), AD.percentage = as.integer(c(30, 50, 80)), cropnames = c("walnut.mature", "almond.mature", "pistachios", "grapes.table", "grapes.wine", "alfalfa.intermountain", "alfalfa.CV", "alfalfa.imperial"), stringsAsFactors = FALSE)
modelgrid$RDI.min <- ifelse(modelgrid$cropnames == "grapes.wine" & modelgrid$AD.percentage == 30, 0.8, ifelse(modelgrid$cropnames == "grapes.wine" & modelgrid$AD.percentage == 50, 0.5, ifelse(modelgrid$cropnames == "grapes.wine" & modelgrid$AD.percentage == 80, 0.2, NA)))
modelgrid$cropcode <- ifelse(modelgrid$cropnames == "almond.mature", almond_code, ifelse(modelgrid$cropnames == "walnut.mature", walnut_code, ifelse(modelgrid$cropnames == "pistachios", pistachio_code, ifelse(modelgrid$cropnames == "grapes.table", grape_code, ifelse(modelgrid$cropnames == "grapes.wine", grape_code, ifelse(modelgrid$cropnames == "alfalfa.intermountain" | modelgrid$cropnames == "alfalfa.CV" | modelgrid$cropnames == "alfalfa.imperial", alfalfa_code, print("Done")))))))
modelgrid$irrtype <- ifelse(modelgrid$cropnames == "almond.mature", "Microspray, orchards", ifelse(modelgrid$cropnames == "walnut.mature", "Microspray, orchards", ifelse(modelgrid$cropnames == "pistachios", "Microspray, orchards", ifelse(modelgrid$cropnames == "grapes.table", "Drip", ifelse(modelgrid$cropnames == "grapes.wine", "Drip", ifelse(modelgrid$cropnames == "alfalfa.intermountain" | modelgrid$cropnames == "alfalfa.CV" | modelgrid$cropnames == "alfalfa.imperial", "Border", print("Done")))))))
modelgrid$alfalfa.zone <- ifelse(modelgrid$cropnames == "alfalfa.intermountain", "Intermountain", ifelse(modelgrid$cropnames == "alfalfa.CV", "Central Valley", ifelse(modelgrid$cropnames == "alfalfa.imperial", "Imperial Valley", NA)))
modelgrid$grape.zone <- ifelse(modelgrid$cropnames == "grapes.wine", "Coast, Foothills, and Mountains", ifelse(modelgrid$cropnames == "grapes.table", "Central Valley", NA))
modelgrid
parallel::mclapply(1:96, function (i = NULL) 
{
    FAO56DualCropCalc(modelgrid$cropnames[i], modelgrid$cropcode[i], modelgrid$AD.percentage[i], modelgrid$root.depths[i], modelgrid$irrtype[i], crop.parameters.df, model.scaffold, U2.df, P.df, ETo.df, RHmin.df, results_file = "new", row_start = 1, RDI.min = modelgrid$RDI.min[i], alfalfa.zone = modelgrid$alfalfa.zone[i], grape.zone = modelgrid$grape.zone[i], stress.assumption = 0.5, dailyWBsave = FALSE)
})
modelgrid <- expand.grid(root.depths = c("0.5m", "1.0m", "2.0m", "3.0m"), cropnames = c("walnut.mature", "almond.mature"), stringsAsFactors = FALSE)
rep.number <- nrow(modelgrid)
modelgrid <- rbind(modelgrid, modelgrid)
modelgrid$AD.percentage <- ifelse(modelgrid$root.depths == "0.5m", 30, 50)
modelgrid$cropcode <- ifelse(modelgrid$cropnames == "almond.mature", almond_code, ifelse(modelgrid$cropnames == "walnut.mature", walnut_code, ifelse(modelgrid$cropnames == "pistachios", pistachio_code, ifelse(modelgrid$cropnames == "grapes.table", grape_code, ifelse(modelgrid$cropnames == "grapes.wine", grape_code, ifelse(modelgrid$cropnames == "alfalfa.intermountain" | modelgrid$cropnames == "alfalfa.CV" | modelgrid$cropnames == "alfalfa.imperial", alfalfa_code, print("Done")))))))
modelgrid$irrtype <- c(rep("Border", rep.number), rep("Drip", rep.number))
modelgrid$scenario.dir <- c(rep("Border.irrigation", rep.number), rep("Drip.irrigation", rep.number))
modelgrid
parallel::mclapply(1:nrow(modelgrid), function (i = NULL) 
{
    FAO56DualCropCalc(modelgrid$cropnames[i], modelgrid$cropcode[i], modelgrid$AD.percentage[i], modelgrid$root.depths[i], modelgrid$irrtype[i], crop.parameters.df, model.scaffold, U2.df, P.df, ETo.df, RHmin.df, results_file = "new", row_start = 1, RDI.min = NA, alfalfa.zone = NA, grape.zone = NA, stress.assumption = 0.5, dailyWBsave = FALSE, modelgrid$scenario.dir[i])
})
modelgrid <- expand.grid(root.depths = c("0.5m", "1.0m", "2.0m", "3.0m"), cropnames = c("walnut.mature", "almond.mature"), stringsAsFactors = FALSE)
rep.number <- nrow(modelgrid)
modelgrid <- rbind(modelgrid, modelgrid, modelgrid, modelgrid)
modelgrid$AD.percentage <- ifelse(modelgrid$root.depths == "0.5m", 30, 50)
modelgrid$cropcode <- ifelse(modelgrid$cropnames == "almond.mature", almond_code, ifelse(modelgrid$cropnames == "walnut.mature", walnut_code, ifelse(modelgrid$cropnames == "pistachios", pistachio_code, ifelse(modelgrid$cropnames == "grapes.table", grape_code, ifelse(modelgrid$cropnames == "grapes.wine", grape_code, ifelse(modelgrid$cropnames == "alfalfa.intermountain" | modelgrid$cropnames == "alfalfa.CV" | modelgrid$cropnames == "alfalfa.imperial", alfalfa_code, print("Done")))))))
modelgrid$bloom.offset <- c(rep(-20, rep.number), rep(-10, rep.number), rep(10, rep.number), rep(20, rep.number))
modelgrid$scenario.dir <- c(rep("Bloom.offset.-20", rep.number), rep("Bloom.offset.-10", rep.number), rep("Bloom.offset.10", rep.number), rep("Bloom.offset.20", rep.number))
modelgrid
parallel::mclapply(7:nrow(modelgrid), function (i = NULL) 
{
    FAO56DualCropCalc(modelgrid$cropnames[i], modelgrid$cropcode[i], modelgrid$AD.percentage[i], modelgrid$root.depths[i], "Microspray, orchards", crop.parameters.df, model.scaffold, U2.df, P.df, ETo.df, RHmin.df, results_file = "new", row_start = 1, RDI.min = NA, alfalfa.zone = NA, grape.zone = NA, stress.assumption = 0.5, dailyWBsave = FALSE, modelgrid$scenario.dir[i], modelgrid$bloom.offset[i])
})
modelgrid <- expand.grid(root.depths = c("0.5m", "1.0m", "2.0m", "3.0m"), cropnames = c("walnut.mature", "almond.mature"), stringsAsFactors = FALSE)
rep.number <- nrow(modelgrid)
modelgrid <- rbind(modelgrid, modelgrid)
modelgrid$AD.percentage <- ifelse(modelgrid$root.depths == "0.5m", 30, 50)
modelgrid$cropcode <- ifelse(modelgrid$cropnames == "almond.mature", almond_code, ifelse(modelgrid$cropnames == "walnut.mature", walnut_code, ifelse(modelgrid$cropnames == "pistachios", pistachio_code, ifelse(modelgrid$cropnames == "grapes.table", grape_code, ifelse(modelgrid$cropnames == "grapes.wine", grape_code, ifelse(modelgrid$cropnames == "alfalfa.intermountain" | modelgrid$cropnames == "alfalfa.CV" | modelgrid$cropnames == "alfalfa.imperial", alfalfa_code, print("Done")))))))
crop.parameters.df_Kcb.dorm0.1 <- crop.parameters.df
crop.parameters.df_Kcb.dorm0.1$Kcb.dorm[!(crop.parameters.df_Kcb.dorm0.1$crop == "alfalfa.CV" | crop.parameters.df_Kcb.dorm0.1$crop == "alfalfa.imperial")] <- 0.1
crop.parameters.df_Kcb.dorm0.2 <- crop.parameters.df
crop.parameters.df_Kcb.dorm0.2$Kcb.dorm[!(crop.parameters.df_Kcb.dorm0.2$crop == "alfalfa.CV" | crop.parameters.df_Kcb.dorm0.2$crop == "alfalfa.imperial")] <- 0.2
crop.parameters.list <- c(rep(list(crop.parameters.df_Kcb.dorm0.1), rep.number), rep(list(crop.parameters.df_Kcb.dorm0.2), rep.number))
modelgrid$scenario.dir <- c(rep("Kcb.dorm_0.1", rep.number), rep("Kcb.dorm_0.2", rep.number))
modelgrid
parallel::mclapply(1:nrow(modelgrid), function (i = NULL) 
{
    FAO56DualCropCalc(modelgrid$cropnames[i], modelgrid$cropcode[i], modelgrid$AD.percentage[i], modelgrid$root.depths[i], "Microspray, orchards", crop.parameters.list[[i]], model.scaffold, U2.df, P.df, ETo.df, RHmin.df, results_file = "new", row_start = 1, RDI.min = NA, alfalfa.zone = NA, grape.zone = NA, stress.assumption = 0.5, dailyWBsave = FALSE, modelgrid$scenario.dir[i], bloom.offset = 0)
})
modelgrid <- expand.grid(root.depths = c("0.5m", "1.0m", "2.0m", "3.0m"), cropnames = c("walnut.mature", "almond.mature"), stringsAsFactors = FALSE)
rep.number <- nrow(modelgrid)
modelgrid <- rbind(modelgrid, modelgrid)
modelgrid$AD.percentage <- ifelse(modelgrid$root.depths == "0.5m", 30, 50)
modelgrid$cropcode <- ifelse(modelgrid$cropnames == "almond.mature", almond_code, ifelse(modelgrid$cropnames == "walnut.mature", walnut_code, ifelse(modelgrid$cropnames == "pistachios", pistachio_code, ifelse(modelgrid$cropnames == "grapes.table", grape_code, ifelse(modelgrid$cropnames == "grapes.wine", grape_code, ifelse(modelgrid$cropnames == "alfalfa.intermountain" | modelgrid$cropnames == "alfalfa.CV" | modelgrid$cropnames == "alfalfa.imperial", alfalfa_code, print("Done")))))))
modelgrid$surface.assumption <- c(rep("10cm", rep.number), rep("15cm", rep.number))
modelgrid$scenario.dir <- c(rep("surface.depth.10cm", rep.number), rep("surface.depth.15cm", rep.number))
modelgrid
parallel::mclapply(1:nrow(modelgrid), function (i = NULL) 
{
    FAO56DualCropCalc(modelgrid$cropnames[i], modelgrid$cropcode[i], modelgrid$AD.percentage[i], modelgrid$root.depths[i], "Microspray, orchards", crop.parameters.df, model.scaffold, U2.df, P.df, ETo.df, RHmin.df, results_file = "new", row_start = 1, RDI.min = NA, alfalfa.zone = NA, grape.zone = NA, stress.assumption = 0.5, dailyWBsave = FALSE, scenario.dir = modelgrid$scenario.dir[i], bloom.offset = 0, surface.assumption = modelgrid$surface.assumption[i])
})
