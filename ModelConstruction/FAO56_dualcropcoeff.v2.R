#TO-DO:
  #1.  Re-run all 30% and 80% allowable depletion scenarios because of correction of KsCalc function. [DONE]
  #2.  Correct Ei and Ep functions such that TEW cannot be exceeded by Dei or Dep, respectively, in the upper layer [DONE].  
  #3.  Re-run all almond and walnut scenarios, since model codes were changed when tomatoes, etc. were added to the mix [DONE]
  #5.  Modify winter Kcb calculation for alfalfa: allow for fall growth in intermountain region before frost termination; allow for higher peak values in Fall then decline to 0.8 before rising again in Feb [DONE]
  #6.  Modify Fc equation for alfalfa, so that it is directly based off of Kcb values [DONE]
  #7.  Allow for winter residual "mulch" in intermountain alfalfa production (for every 10% effective surface coverage results in 5% reduction in TEW)
  #8.  Modify irrigation decision so that irrigations can't occur before 2/7 in Central Valley, even with drought stress but ensure this doesn't adversely affect results calculations that depend on definitions of Jdev [DONE]
  #9. Add alfalfa zone to model scaffold [DONE]
  #10. Add grape zone to model scaffold [DONE]
#modified KeiCalc and KepCalc on 9/11/17 to correct for overestimation of evaporation from sandy soils with very low TEW (i.e. <12 mm)
  #11. Simple tests or monte-Carlo simulation of effect of following effects (1) climatic adjustment of Kcb values (2) bloom and leaf-drop assumptions (3) Kcb values themselves
  #12. Test effect of varying alfalfa height on Kcb calcs
  #13. Modify definition of GW.ET.growing so that it no longer includes a correction for irrigation water end storage, elminating need for post-run correction
  #14.  Remove one of the deep percolation definitions and do one for the growing season period
  #15.  Do separate water balance tracking for irrigation and green water to get a more temporally accruate summation of green water utilization[NOT NECESSARY; OPERATIONAL DEF WORKS]
  #16. Revise grape zone terminology
  
#changed order of DPei and DPep on 9/11/17  
# changed order of Ir and Ks calculation on 8/23/17
# changed Ir decision function on 8/23/17 to accomodate different irrigation decisions for wine grapes
# concept for wine irrigation decisions is to set a min Ks threshold and irrigate to allowable depletion when that threshold is crossed, as opposed to irrigating to field capacity when allowable depletion is crossed
#script to implement the FAO56 dual crop coefficient ET routine
modelscaffoldDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/model_scaffold/run_model/Mar2018' #location of input data; re-worked early March 2018
resultsDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/results/Mar2018'
rounding_digits <- 3
irrigation.parameters <- read.csv(file.path(modelscaffoldDir, 'irrigation_parameters.csv'), stringsAsFactors = FALSE)
crop.parameters.df <- read.csv(file.path(modelscaffoldDir, 'crop_parameters.csv'), stringsAsFactors = FALSE) #necessary parameters by crop to run the FAO56 dual crop coefficient ET model
P.df <- read.csv(file.path(modelscaffoldDir, 'PRISM_precip_data.csv'), stringsAsFactors = FALSE) #this is a daily summary of precip from 10/1/2003-3/8/17 from 'free' daily PRISM 4km resolution for cells of interest in California, created in download_PRISM.R script (from 3/9/18 download); blanks and negative values checked for in 'data_QA_QC.R'
U2.df <- read.csv(file.path(modelscaffoldDir, 'SpatialCIMIS.U2.QCpass.csv'), stringsAsFactors = FALSE) #this is a daily summary of wind data from download of spatial CIMIS data, created in spatialCIMIS.R script.  No missing data except for cell 148533
RHmin.df <- read.csv(file.path(modelscaffoldDir, 'SpatialCIMIS.RHmin.QCpass.csv'), stringsAsFactors = FALSE) #this is a daily summary of minimum relative humidity, estimated from download of spatial CIMIS Tdew and Tmax data, created in spatialCIMIS.R script.  Blanks filled on "12_08_2011" & "02_23_2018" in data_QA_QC.R, along with 2,690 >100% values corrected
ETo.df <- read.csv(file.path(modelscaffoldDir, 'SpatialCIMIS.ETo.QCpass.csv'), stringsAsFactors = FALSE) #this is a daily summary of reference ET from download of spatial CIMIS data, created in spatialCIMIS.R script.  Blanks filled on multiple days (all on "02_23_2018") in data_QA_QC.R, along with 1,633 negative and 11 zero values corrected.
cropscape_legend <- read.csv(file.path(modelscaffoldDir, 'cropscape_legend.txt'), stringsAsFactors = FALSE)
model.scaffold <- read.csv(file.path(modelscaffoldDir, 'model_scaffold_majcomps2018-03-15.csv'), stringsAsFactors = FALSE) #note this has several more columns than previous versions; will have affects downstream
lapply(model.scaffold, class)
model.scaffold <- model.scaffold[order(model.scaffold$unique_model_code), ]

#model.scaffold$grape.zone[which(model.scaffold$grape.zone=='Southern Desert')] <- 'Central California Valley' #this zone will be treated like 'Central California Valley,' that is, table grape, raisin, or low-quality wine grape


#define functions to implement FAO56 dual crop coefficient model
#includes subroutine that separates evaporable water as P vs. Irr sourced
#some results abbreviations
#E=evaporation
#T=transpiration
#ET=evapotranspriation
#Irr.1=first irrigation of year
#Irr.Last=last irrigation of year
#GW.ET.to.Irr1=ET sourced from green water from flowering to first irrigation (should subtract previous year's carryover irrigation storage)
#ET.growing=total ET from flowering (Jdev) to leaf-drop (Jharv)
#ET.WY=total annual ET on water-year basis
#deep.perc is annual deep percolation ()
#GW.capture.net is net change in soil root zone depletion from Jharv (leaf drop) to Jdev (flowering and development)
#end.season.Dr is soil root zone depletion at Jharv (leaf drop)

#temporary arguments for working inside the function when necessary
cropname <- 'almond.mature'
cropcode <- almond_code #grape code
AD.percentage <- 30
root_depth <- '0.5m'
irr.type <- 'Microspray, orchards'
results_file <- 'new'
row_start <- 1
RDI.min <- NA
alfalfa.zone <- NA
grape.zone <- NA
stress.assumption <- 0.5


FAO56DualCropCalc <- function(cropname, cropcode, AD.percentage, root_depth, irr.type, crop.parameters.df, model.scaffold, U2.df, P.df, ETo.df, RHmin.df, results_file, row_start, RDI.min, alfalfa.zone, grape.zone, stress.assumption) {
  alfalfa_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Alfalfa']
  grape_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Grapes']
  almond_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Almonds']
  walnut_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Walnuts']
  pistachio_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Pistachios']
  CropParametersDefine <- function(crop.parameters, cropname) {
    crop.parameters <- crop.parameters[which(crop.parameters$crop==cropname), ]  
    bloom.date <- strptime(paste0(as.character(crop.parameters$bloom.mo), '/', as.character(crop.parameters$bloom.day)), '%m/%d')
    if (cropname=='alfalfa.intermountain' | cropname=='alfalfa.CV' | cropname=='alfalfa.imperial') {
      crop.parameters$Jini <- as.integer(format.Date(bloom.date, '%j')) #this covers the first cutting
      crop.parameters$Jdev <- crop.parameters$Jini + crop.parameters$Lini
      crop.parameters$Jmid <- crop.parameters$Jdev + crop.parameters$Ldev
      crop.parameters$Jlate <- crop.parameters$Jmid + crop.parameters$Lmid
      crop.parameters$Jharv <- crop.parameters$Jlate + crop.parameters$Llate
      return(crop.parameters)
    } else { #these cover the orchard and vine crops
      crop.parameters$Jdev <- as.integer(format.Date(bloom.date, '%j'))
      crop.parameters$Jmid <- crop.parameters$Jdev + crop.parameters$Ldev
      crop.parameters$Jlate <- crop.parameters$Jmid + crop.parameters$Lmid
      crop.parameters$Jharv <- crop.parameters$Jlate + crop.parameters$Llate
      return(crop.parameters)
    }
  }
  KcbDefine <- function(doy, crop) { #find standard value of Kcb by day of year relative to three reference Kcb points defined by crop parameters
    parameters <- crop.parameters[which(crop.parameters$crop==crop), ]
    if (crop=='alfalfa.intermountain' | crop=='alfalfa.CV' | crop=='alfalfa.imperial') {
      cycle.1.length <- sum(parameters[['Lini']], parameters[['Ldev']], parameters[['Lmid']], parameters[['Llate']])
      alfalfa.Kcb.cycle1 <- function(Kcb.winter, final.else) {
        if (crop=='alfalfa.intermountain') {
          ifelse(doy < parameters[['Jdev']], Kcb.winter, ifelse(doy < parameters[['Jmid']], Kcb.winter + (doy - parameters[['Jdev']]) / parameters[['Ldev']] * (parameters[['Kcb.mid']] - Kcb.winter), ifelse(doy < parameters[['Jlate']] , parameters[['Kcb.mid']], ifelse(doy < parameters[['Jharv']], parameters[['Kcb.mid']] + (doy - parameters[['Jlate']])/ parameters[['Llate']] * (parameters[['Kcb.end']] - parameters[['Kcb.mid']]), final.else))))
        }
        else if (crop=='alfalfa.CV') {
          ifelse(doy < parameters[['Jini']], Kcb.winter, ifelse(doy < parameters[['Jdev']] , Kcb.winter, ifelse(doy < parameters[['Jmid']], Kcb.winter + (doy - parameters[['Jdev']]) / parameters[['Ldev']] * (parameters[['Kcb.mid']] - Kcb.winter), ifelse(doy < parameters[['Jlate']] , parameters[['Kcb.mid']], ifelse(doy < parameters[['Jharv']], parameters[['Kcb.mid']] + (doy - parameters[['Jlate']])/ parameters[['Llate']] * (parameters[['Kcb.end']] - parameters[['Kcb.mid']]), final.else)))))
        } else { #then covers Imperial valley production just before and after Jan cutting
            ifelse(doy < parameters[['Jini']] - parameters[['Llate']], parameters[['Kcb.mid']], ifelse(doy < parameters[['Jini']], parameters[['Kcb.mid']] + (doy - parameters[['Jini']] + parameters[['Llate']])/ parameters[['Llate']] * (parameters[['Kcb.end']] - parameters[['Kcb.mid']]), ifelse(doy < parameters[['Jdev']] , parameters[['Kcb.ini']], ifelse(doy < parameters[['Jmid']], parameters[['Kcb.ini']] + (doy - parameters[['Jdev']]) / parameters[['Ldev']] * (parameters[['Kcb.mid']] - parameters[['Kcb.ini']]), ifelse(doy < parameters[['Jlate']], parameters[['Kcb.mid']], ifelse(doy < parameters[['Jharv']], parameters[['Kcb.mid']] + (doy - parameters[['Jlate']])/ parameters[['Llate']] * (parameters[['Kcb.end']] - parameters[['Kcb.mid']]), final.else))))))
        }
      }
      alfalfa.Kcb.cycles <- function(cycle.no, cycle.length, final.else) {
        ifelse(doy < parameters[['Jini']] + cycle.1.length + cycle.length*(cycle.no - 2) + parameters[['Lini.cycle']], parameters[['Kcb.ini']], ifelse(doy < parameters[['Jini']] + cycle.1.length + cycle.length*(cycle.no - 2) + parameters[['Lini.cycle']] + parameters[['Ldev.cycle']], parameters[['Kcb.ini']] + (doy - parameters[['Jini']] - cycle.1.length - cycle.length*(cycle.no - 2) - parameters[['Lini.cycle']]) / parameters[['Ldev.cycle']] * (parameters[['Kcb.mid']] - parameters[['Kcb.ini']]), ifelse(doy < parameters[['Jini']] + cycle.1.length + cycle.length*(cycle.no - 2) + parameters[['Lini.cycle']] + parameters[['Ldev.cycle']] + parameters[['Lmid.cycle']], parameters[['Kcb.mid']], ifelse(doy < parameters[['Jini']] + cycle.1.length + cycle.length*(cycle.no - 2) + parameters[['Lini.cycle']] + parameters[['Ldev.cycle']] + parameters[['Lmid.cycle']] + parameters[['Llate.cycle']], parameters[['Kcb.mid']] + (doy - parameters[['Jini']] - cycle.1.length - cycle.length*(cycle.no - 2) - parameters[['Lini.cycle']] - parameters[['Ldev.cycle']] - parameters[['Lmid.cycle']]) / parameters[['Llate']] * (parameters[['Kcb.end']] - parameters[['Kcb.mid']]), final.else))))
      }
      CV.alfalfa.last <- function(final.else) {
        ifelse(doy < parameters[['Jini']] + cycle.1.length + parameters[['cycle.length']]*(parameters[['cycle.no']] - 2) + parameters[['Lini']], parameters[['Kcb.ini']], ifelse(doy < parameters[['Jini']] + cycle.1.length + parameters[['cycle.length']]*(parameters[['cycle.no']] - 2) + parameters[['Lini']] + parameters[['Ldev']], parameters[['Kcb.ini']] + (doy - parameters[['Jini']] - cycle.1.length - parameters[['cycle.length']]*(parameters[['cycle.no']] - 2) - parameters[['Lini']]) / parameters[['Ldev']] * (parameters[['Kcb.mid']] - parameters[['Kcb.ini']]), ifelse(doy < parameters[['Jini']] + cycle.1.length + parameters[['cycle.length']]*(parameters[['cycle.no']] - 2) + parameters[['Lini']] + parameters[['Ldev']] + parameters[['Lmid']], parameters[['Kcb.mid']], ifelse(doy < parameters[['Jini']] + cycle.1.length + parameters[['cycle.length']]*(parameters[['cycle.no']] - 2) + parameters[['Lini']] + parameters[['Ldev']] + parameters[['Lmid']] + parameters[['Llate']], parameters[['Kcb.mid']] + (doy - parameters[['Jini']] - cycle.1.length - parameters[['cycle.length']]*(parameters[['cycle.no']] - 2) - parameters[['Lini']] - parameters[['Ldev']] - parameters[['Lmid']]) / parameters[['Llate']] * (parameters[['Kcb.end']] - parameters[['Kcb.mid']]), final.else))))
      }
      winter.Kcb.alfalfa <- function(cycle.no=1, final.else=NA) { #could add dormancy reduction factor here
        if (crop=='alfalfa.CV') {
          ifelse(doy < parameters[['Jharv']] + cycle.1.length + (parameters[['cycle.no']] - 2) * parameters[['cycle.length']] + parameters[['Lini']], parameters[['Kcb.ini']], ifelse(doy < parameters[['Jharv']] + cycle.1.length + (parameters[['cycle.no']] - 2) * parameters[['cycle.length']] + parameters[['Lini']] + parameters[['Ldev']], parameters[['Kcb.ini']] + (doy - parameters[['Jharv']] - cycle.1.length - (parameters[['cycle.no']] - 2) * parameters[['cycle.length']] - parameters[['Lini']]) / parameters[['Ldev']] * (parameters[['Kcb.winter']] - parameters[['Kcb.ini']]), ifelse(doy < parameters[['Jharv']] + cycle.1.length + (parameters[['cycle.no']] - 2) * parameters[['cycle.length']] + parameters[['Lini']] + parameters[['Ldev']] + parameters[['Lmid']], parameters[['Kcb.mid']], ifelse(doy < parameters[['Jharv']] + cycle.1.length + (parameters[['cycle.no']] - 2) * parameters[['cycle.length']] + parameters[['Lini']] + parameters[['Ldev']] + parameters[['Lmid']] + parameters[['Llate']] + parameters[['Llate']], parameters[['Kcb.mid']] + (doy - parameters[['Jharv']] - cycle.1.length - parameters[['cycle.length']]*(parameters[['cycle.no']] - 2) - parameters[['Lini']] - parameters[['Ldev']] - parameters[['Lmid']]) / (parameters[['Llate']] + parameters[['Llate']]) * (parameters[['Kcb.winter']] - parameters[['Kcb.mid']]), parameters[['Kcb.winter']]))))
        }
        else if (crop=='alfalfa.intermountain') {
          ifelse(doy < parameters[['Jharv']] + (parameters[['cycle.no']] - 1) * parameters[['cycle.length']] + parameters[['Lini']], parameters[['Kcb.ini']], ifelse(doy < parameters[['Jharv']] + (parameters[['cycle.no']] - 1) * parameters[['cycle.length']] + parameters[['Lini']] + parameters[['Ldev']], parameters[['Kcb.ini']] + (doy - parameters[['Jharv']] - (parameters[['cycle.no']] - 1) * parameters[['cycle.length']] - parameters[['Lini']]) / parameters[['Ldev']] * (parameters[['Kcb.mid']] - parameters[['Kcb.ini']]), ifelse(doy < parameters[['Jharv']] + (parameters[['cycle.no']] - 1) * parameters[['cycle.length']] + parameters[['Lini']] + parameters[['Ldev']] + parameters[['Lmid']], parameters[['Kcb.mid']], ifelse(doy < parameters[['Jharv']] + (parameters[['cycle.no']] - 1) * parameters[['cycle.length']] + parameters[['Lini']] + parameters[['Ldev']] + parameters[['Lmid']] + parameters[['Lfrost.kill']], parameters[['Kcb.mid']] + (doy - parameters[['Jharv']] - (parameters[['cycle.no']] - 1) * parameters[['cycle.length']] - parameters[['Lini']] - parameters[['Ldev']] - parameters[['Lmid']]) / parameters[['Lfrost.kill']] * (parameters[['Kcb.dorm']] - parameters[['Kcb.mid']]), parameters[['Kcb.dorm']]))))
        } else { #this covers Imperial Valley
          ifelse(doy < parameters[['Jharv']] + (parameters[['cycle.no']] - 3) * parameters[['cycle.length']] + (cycle.no - parameters[['cycle.no']] + 1) * cycle.1.length + parameters[['Lini']], parameters[['Kcb.ini']], ifelse(doy < parameters[['Jharv']] + (parameters[['cycle.no']] - 3) * parameters[['cycle.length']] + (cycle.no - parameters[['cycle.no']] + 1) * cycle.1.length + parameters[['Lini']] + parameters[['Ldev']], parameters[['Kcb.ini']] + (doy - parameters[['Jharv']] - (parameters[['cycle.no']] - 3) * parameters[['cycle.length']] - (cycle.no - parameters[['cycle.no']] + 1) * cycle.1.length - parameters[['Lini']]) / parameters[['Ldev']] * (parameters[['Kcb.mid']] - parameters[['Kcb.ini']]), ifelse(doy < parameters[['Jharv']] + (parameters[['cycle.no']] - 3) * parameters[['cycle.length']] + (cycle.no - parameters[['cycle.no']] + 1) * cycle.1.length + parameters[['Lini']] + parameters[['Ldev']] + parameters[['Lmid']], parameters[['Kcb.mid']], ifelse(doy < parameters[['Jharv']] + (parameters[['cycle.no']] - 3) * parameters[['cycle.length']] + (cycle.no - parameters[['cycle.no']] + 1) * cycle.1.length + parameters[['Lini']] + parameters[['Ldev']] + parameters[['Lmid']] + parameters[['Llate']], parameters[['Kcb.mid']] + (doy - parameters[['Jharv']] - (parameters[['cycle.no']] - 3) * parameters[['cycle.length']] - (cycle.no - parameters[['cycle.no']] + 1) * cycle.1.length - parameters[['Lini']] - parameters[['Ldev']] - parameters[['Lmid']]) / parameters[['Llate']] * (parameters[['Kcb.end']] - parameters[['Kcb.mid']]), final.else))))
        }
      }
      if (crop=='alfalfa.intermountain') {
        alfalfa.Kcb.cycle1(parameters[['Kcb.dorm']], alfalfa.Kcb.cycles(2, parameters[['cycle.length']], alfalfa.Kcb.cycles(3, parameters[['cycle.length']], winter.Kcb.alfalfa())))
      }
      else if (crop=='alfalfa.CV') {
        alfalfa.Kcb.cycle1(parameters[['Kcb.winter']], alfalfa.Kcb.cycles(2, parameters[['cycle.length']], alfalfa.Kcb.cycles(3, parameters[['cycle.length']], alfalfa.Kcb.cycles(4, parameters[['cycle.length']], alfalfa.Kcb.cycles(5, parameters[['cycle.length']], alfalfa.Kcb.cycles(6, parameters[['cycle.length']], CV.alfalfa.last(winter.Kcb.alfalfa())))))))
      } else { #crop is alfalfa.imperial
        alfalfa.Kcb.cycle1(Kcb.winter=NA, alfalfa.Kcb.cycles(2, parameters[['cycle.length']], alfalfa.Kcb.cycles(3, parameters[['cycle.length']], alfalfa.Kcb.cycles(4, parameters[['cycle.length']], alfalfa.Kcb.cycles(5, parameters[['cycle.length']], alfalfa.Kcb.cycles(6, parameters[['cycle.length']], alfalfa.Kcb.cycles(7, parameters[['cycle.length']], alfalfa.Kcb.cycles(8, parameters[['cycle.length']], alfalfa.Kcb.cycles(9, parameters[['cycle.length']], winter.Kcb.alfalfa(cycle.no = 10, final.else = winter.Kcb.alfalfa(cycle.no = 11, final.else=NA)))))))))))
      }
    } else {
        ifelse(doy < parameters[['Jdev']], parameters[['Kcb.dorm']], ifelse(doy < parameters[['Jmid']], parameters[['Kcb.ini']] + (doy - parameters[['Jdev']]) / parameters[['Ldev']] * (parameters[['Kcb.mid']] - parameters[['Kcb.ini']]), ifelse(doy < parameters[['Jlate']], parameters[['Kcb.mid']], ifelse(doy < parameters[['Jharv']], parameters[['Kcb.mid']] + (doy - parameters[['Jlate']]) / parameters[['Llate']] * (parameters[['Kcb.end']] - parameters[['Kcb.mid']]), parameters[['Kcb.dorm']]))))
    }
  }
  #adjust standard Kcb values by daily climate according to equation #5 in Allen et al. 2005 and calculate Kc max at the same time
  KcbAdj <- function(Kcb.daily, crop.parameters, crop, U2, RHmin){
    height <- crop.parameters$height[which(crop.parameters$crop==crop)]
    Kcb.climate.adj <- Kcb.daily + (0.04 * (U2 - 2) - 0.004 * (RHmin - 45)) * (height / 3) ^ 0.3
    Kcb.climate.adj[which(Kcb.climate.adj < 0)] <- 0
    Kc.max <- pmax(1.2 + (0.04 * (U2 - 2) - 0.004 * (RHmin - 45)) * (height / 3) ^ 0.3, Kcb.climate.adj + 0.05)
    return(as.data.frame(cbind(Kcb.climate.adj, Kc.max)))
  }
  #calculate fraction of cover ('fc') for specific day of year relative to three points defined in crop parameters
  fcCalc <- function(doy, parameters, crop) {
    parameters <- crop.parameters[which(crop.parameters$crop==crop), ]
    if (crop == 'alfalfa.CV' | crop == 'alfalfa.imperial') {
      Kcb.std/parameters[['Kcb.mid']]
    }
    else if (crop == 'alfalfa.intermountain') {
      ifelse(doy < parameters[['Jdev']], 0, ifelse(doy < parameters[['Jharv']] + (parameters[['cycle.no']] - 1) * parameters[['cycle.length']] + parameters[['Lini']] + parameters[['Ldev']] + parameters[['Lmid']] + parameters[['Lfrost.kill']], Kcb.std/parameters[['Kcb.mid']], 0))
    } else {
        ifelse(doy < parameters[['Jdev']], parameters[['fc.ini']], ifelse(doy < parameters[['Jmid']], parameters[['fc.ini']] + (doy - parameters[['Jdev']]) / parameters[['Ldev']] * (parameters[['fc.mid']] - parameters[['fc.ini']]), ifelse(doy < parameters[['Jlate']], parameters[['fc.mid']], ifelse(doy < parameters[['Jharv']], parameters[['fc.mid']] + (doy-parameters[['Jlate']]) / parameters[['Llate']] * (parameters[['fc.end']] - parameters[['fc.mid']]), parameters[['fc.ini']]))))
    }
  }
  #calculate fraction of soil wetted by both irrigation and precipitation and exposed to rapid drying (fewi) and fraction of soil exposed to rapid drying and is wetted by precipitation only (fewp).  These are dependent upon fraction of cover calculation above
  fwSelect <- function(irr.parameters, irr.type) {
    irr.parameters$fw[which(irr.parameters$irrigation.type==irr.type)]
  }
  fewiCalc <- function(fc, fw) { #see p.147 of FAO-56 Chp.7 re: drip
    fewi.temp <- pmin(1 - fc, fw)
    fewi.temp[which(fewi.temp < 0.001)] <- 0.001 #lower limit on fewi for numeric stability
    fewi.temp[which(fewi.temp > 1)] <- 1 #upper limit on fewi for numeric stability
    return(fewi.temp)
  }
  fewpCalc <- function(fc, fewi) {
    fewp.temp <- 1 - fc - fewi
    fewp.temp[which(fewp.temp < 0.001)] <- 0.001 #lower limit on fewp for numeric stability
    fewp.temp[which(fewp.temp > 1)] <- 1 #upper limit on fewp for numeric stability
    return(fewp.temp)
  }
  # TEWCalc <- function(ThetaFC, ThetaWP, REW, Ze=0.10) { #Ze is depth of upper soil 
  #   #layer where evaporation occurs in meters
  #   result <- 1000 * (ThetaFC - 0.5 * ThetaWP) * Ze
  #   if (result < REW) {
  #     stop(print('Hay una problema con TEW')) #could change to next and print or 
  #     #save model code
  #   } 
  #   return(result)
  # }
  DepInitialCalc <- function(Dep.end, P, i) {
    max(Dep.end[i-1] - P[i], 0) # DON'T RUN THIS IF i=1
  }
  DeiInitialCalc <- function(Dei.end, P, Ir, fw, i) { #DON'T RUN THIS WHEN i=1
    max(Dei.end[i - 1] - P[i] - Ir[i - 1] / fw, 0) #have to use irrigation from 
    #previous day because current day irrigation decision is dependent on this calc
  }
  KrCalc <- function(TEW, REW, De.initial, i) { #can be used to calculate Kri or Krp
    max(0, if(De.initial[i] < REW) { #could initialize this in vector above
      1
    } else if(TEW==REW) {
      0
    } else {
      (TEW - De.initial[i]) / (TEW - REW)
    })
  }
  WCalc <- function(TEW, Dei.initial, Dep.initial, fewp, fewi, i) {
    1 / (1 + (fewp[i] / fewi[i]) * max(TEW - Dep.initial[i], 0.001) / max((TEW - Dei.initial[i]), 0.001))
  }
  KeiCalc <- function(Kri, W, Kcmax, Kcb, fewi, TEW, Dei.end, DPei, P, ETo, i) {
    Kei.est <- min(Kri[i] * W[i] * (Kcmax[i] - Kcb[i]), fewi[i] * Kcmax[i])
    #print(Kei.est)
    TEW.check <- max(Dei.end[i - 1] - P[i] - Ir[i - 1] / fw + Kei.est * ETo[i] / fewi[i] + DPei[i], 0)
    #print(TEW.check)
    if (TEW.check > TEW) {
      max(fewi[i] * (TEW - Dei.end[i - 1] + P[i] + Ir[i - 1] / fw - DPei[i]) / ETo[i], 0) #as revised Kep estimate
    } else {
      max(Kei.est, 0)
    }
  }
  KepCalc <- function(Krp, W, Kcmax, Kcb, fewp, TEW, Dep.end, DPep, P, ETo, i) {
    Kep.est <- min(Krp[i] * (1 - W[i]) * (Kcmax[i] - Kcb[i]), fewp[i] * Kcmax[i])
    #print(Kep.est)
    TEW.check <- max(Dep.end[i - 1] - P[i] + (Kep.est * ETo[i]) / fewp[i] + DPep[i], 0)
    #print(TEW.check)
    if (TEW.check > TEW) {
      max(fewp[i] * (TEW - Dep.end[i - 1] + P[i] - DPep[i]) / ETo[i], 0) #as revised Kep estimate
    } else {
        max(Kep.est, 0)
    }
  }
  EpCalc <- function(ETo, Kep, i) {
    ETo[i] * Kep[i]
  }
  EiCalc <- function(ETo, Kei, i) {
    ETo[i] * Kei[i]
  }
  DPepCalc <- function(P, Dep.end, i) { #DON'T RUN THIS FOR i=1
    max(P[i] - Dep.end[i-1], 0)
  }
  #as implmented below in loop: DPep[i] <- DPepCalc(P, Dep.initial, i)
  DepEndCalc <- function(Dep.end, P, Ep, fewp, DPep, i) { #DON'T RUN THIS FOR i=1
    Dep.end.est <- max(Dep.end[i - 1] - P[i] + Ep[i] / fewp[i] + DPep[i], 0)
  } #ignores transpiration and runoff from upper layer
  DPeiCalc <- function(P, Ir, fw, Dei.end, i) { #DON'T RUN THIS FOR i=1
    max(0, P[i] + Ir[i - 1] / fw - Dei.end[i - 1])
  }
  #as implemented below in loop: DPei[i] <- DPeiCalc(P, Ir, fw, Dei.initial, i)
  DeiEndCalc <- function(Dei.end, P, Ir, fw, fewi, Ei, DPei, i) { #DON'T RUN THIS FOR i=1
    max(Dei.end[i - 1] - P[i] - Ir[i - 1] / fw + Ei[i] / fewi[i] + DPei[i], 0)
  } #again, ignoring tranpiration from upper 10 cm and runoff, eqn. 21)
  KcnsCalc <- function(Kcb, Kei, Kep, i) {
    Kcb[i] + Kei[i] + Kep[i]
  }
  ETcnsCalc <- function(Kc.ns, ETo, i) {
    Kc.ns[i] * ETo[i]
  }
  DrInitialCalc <- function(Dr.end, ETc.ns, P, Ir, i) { #DON'T RUN THIS FOR i=1; this only used for making irrigation decisions as of 4/5, because Ks calc should not be dependent on pre-assuming that no-stress ET will be the case
    Dr.end[i - 1] - P[i] - Ir[i-1] + ETc.ns[i]
  }
  #need to refine days.no.irr based on ETo data for a given location
  DaysNoIrr <- function(P, ETo, Kcb.adjusted, AD, doys.model, years, Jmid, Jharv, buffer.days) { #defined in crop.parameters$L.final.irr.buffer.days = Jharv(cycle 1) + (cycle.no - 1) * cycle.length + Lini + Ldev + Lmid + L.frost.kill * 0.444 = 298, in ref. to alfalfa.intermountain; doy 322 for alfalfa.CV from 292 [last day in harvest.days] + 30 [cycle.length]
    if (cropname=='alfalfa.imperial') {
      return(0)
    }
    else if (cropname == 'alfalfa.CV' | cropname == 'alfalfa.intermountain') {
      last.irr.possible <- harvest.days[length(harvest.days)] + buffer.days
      d.f <- data.frame(cbind(Kcb.adjusted, ETo, P, doys.model, years))
      d.f <- d.f[which(d.f$doys.model >= 200 & d.f$doys.model <= last.irr.possible), ]
      d.f$ETcb <- d.f$Kcb.adjusted * d.f$ETo
      daily.ETcb <- tapply(d.f$ETcb, d.f$doys.model, mean)
      daily.P <- tapply(d.f$P, d.f$doys.model, mean)
      daily.WB <- daily.ETcb - daily.P
      for (i in 1:length(daily.WB)) {
        if (sum(daily.WB) < AD) {
          last.irr <- as.integer(names(daily.WB[1]))
          while(last.irr %in% harvest.days) {
            last.irr <- last.irr + 1
          }
          return(last.irr.possible - last.irr)
        } else {
            daily.WB <- daily.WB[-1]
        }
      }
    }
    else if (cropname=='grapes.wine') {
      return(30)
    } else {
      d.f <- data.frame(cbind(Kcb.adjusted, ETo, P, doys.model, years))
      d.f <- d.f[which(d.f$doys.model >= Jmid & d.f$doys.model <= Jharv), ]
      d.f$ETcb <- d.f$Kcb.adjusted * d.f$ETo
      daily.ETcb <- tapply(d.f$ETcb, d.f$doys.model, mean)
      daily.P <- tapply(d.f$P, d.f$doys.model, mean)
      daily.WB <- daily.ETcb - daily.P
      for (i in 1:length(daily.WB)) {
        if (sum(daily.WB) < AD) {
          return(Jharv - as.integer(names(daily.WB[1])))
        } else {
          daily.WB <- daily.WB[-1]
        }
      }
    }
  }
  GrapeLastIrr <- function() {
    if (cropname == 'grapes.wine') {
      d.f <- data.frame(cbind(Kcb.adjusted, ETo, P, doys.model))
      d.f <- d.f[which(d.f$doys.model >= Jharv - days.no.irr & d.f$doys.model <= Jharv), ]
      d.f$ETcb <- d.f$Kcb.adjusted * d.f$ETo
      daily.ETcb <- tapply(d.f$ETcb, d.f$doys.model, mean)
      daily.P <- tapply(d.f$P, d.f$doys.model, mean)
      last.irr <- Dr.initial[i] + sum(daily.ETcb) - sum(daily.P) - AD
      if (last.irr > Dr.initial[i]) {
        return(Dr.initial[i])
      } else {
        return(max(last.irr, 0))
      }
    }
  }
  HarvestDays <- function(crop) { #assume 4 day buffer on either end of each harvest
    if (crop == 'alfalfa.imperial') {
      parameters <- crop.parameters[which(crop.parameters$crop==crop), ]
      harvest.dates <- c(parameters[['Jini']], seq(from=parameters[['Jharv']], to=parameters[['Jharv']] + parameters[['cycle.length']]*(parameters[['cycle.no']]-3), by=parameters[['cycle.length']]), parameters[['Jharv']] + parameters[['cycle.length']]*(parameters[['cycle.no']]-3) + (parameters[['Jharv']] - parameters[['Jini']]))
      harvest.dates.all <- c(harvest.dates, harvest.dates-1, harvest.dates-2, harvest.dates-3, harvest.dates-4, harvest.dates+1, harvest.dates+2, harvest.dates+3, harvest.dates+4)
      harvest.dates.all[order(harvest.dates.all)]
    }
    else if (crop == 'alfalfa.CV') {
      parameters <- crop.parameters[which(crop.parameters$crop==crop), ]
      harvest.dates <- c(seq(from=parameters[['Jharv']], to=parameters[['Jharv']] + parameters[['cycle.length']]*(parameters[['cycle.no']] - 2), by=parameters[['cycle.length']]), parameters[['Jharv']] + parameters[['cycle.length']]*(parameters[['cycle.no']] - 2) + (parameters[['Jharv']] - parameters[['Jini']]))
      harvest.dates.all <- c(harvest.dates, harvest.dates - 1, harvest.dates - 2, harvest.dates - 3, harvest.dates - 4, harvest.dates + 1, harvest.dates + 2, harvest.dates + 3, harvest.dates + 4)
      harvest.dates.all[order(harvest.dates.all)]
    } else { #this is for intermountain alfalfa
      parameters <- crop.parameters[which(crop.parameters$crop==crop), ]
      harvest.dates <- seq(from=parameters[['Jharv']], to=parameters[['Jharv']] + (parameters[['cycle.no']] - 1) * parameters[['cycle.length']], by=parameters[['cycle.length']])
      harvest.dates.all <- c(harvest.dates, harvest.dates - 1, harvest.dates - 2, harvest.dates - 3, harvest.dates - 4, harvest.dates + 1, harvest.dates + 2, harvest.dates + 3, harvest.dates + 4)
      harvest.dates.all[order(harvest.dates.all)]
    }
  }
  IrCalc <- function(Ks, RDI.min, AD, Dr.initial, doys.model, Jdev, Jharv, days.no.irr, i, buffer.days) { #see notes defining these above in days.no.irr calc
    if (cropname == 'alfalfa.imperial') {
      if (Dr.initial[i] > AD & !(doys.model[i] %in% harvest.days)) {
        return(Dr.initial[i])
      } else {
          return(0)
        }
    } else if (cropname == 'alfalfa.CV' | cropname == 'alfalfa.intermountain') {
        if (doys.model[i] < crop.parameters$Jini[which(crop.parameters$crop==cropname)] | doys.model[i] %in% harvest.days | doys.model[i] > harvest.days[length(harvest.days)] + buffer.days - days.no.irr) {
          return(0)
        } else if(Dr.initial[i] > AD) {
            return(Dr.initial[i])
        } else if (doys.model[i] == harvest.days[length(harvest.days)] + buffer.days - days.no.irr & Dr.initial[i] > 0) { #added "& Dr.initial[i] > 0" as condition on 3/27/18; see note below
            return(Dr.initial[i])
        } else {
            return(0)
        }
    } else if (doys.model[i] < Jdev | doys.model[i] > Jharv - days.no.irr) {
      return(0)
    } else if (cropname == 'grapes.wine' & doys.model[i] < Jharv - days.no.irr & Ks[i] < RDI.min) {
      return(Dr.initial[i] - AD)
    } else if (Dr.initial[i] > AD & cropname != 'grapes.wine') {
      return(Dr.initial[i])
    } else if (doys.model[i] == Jharv - days.no.irr & cropname=='grapes.wine' & Dr.initial[i] > 0) {
      return(GrapeLastIrr())
    } else if (doys.model[i] == Jharv - days.no.irr & Dr.initial[i] > 0) { #added "& Dr.initial[i] > 0" condition on 3/27/18; was leading to an occassional negative irrigation on the last day of irrigation for each model when there was a large rain event the same day resulting in negative Dr.initial that results in deep percolation
      return(Dr.initial[i])
    } else {
      return(0)
    }
  }
  DPrCalc <- function(P, Ir, ETc.ns, Dr.end, i) { #DON'T RUN THIS FOR i=1
    max(P[i] + Ir[i-1] - ETc.ns[i] - Dr.end[i - 1], 0)
  }
  KsCalc <- function(Dr.end, P, Ir, PAW, stress.point=stress.assumption*PAW, i) { #changed Ks to definition to assume soil water status from previous day's ending value plus early morning precip and rain
    Dr.temp <- Dr.end[i - 1] - P[i] - Ir[i - 1] #don't include current days' ET.ns in estimate of soil water status
    if (Dr.temp > stress.point) {
      max((PAW - Dr.temp) / (PAW - stress.point), 0) #in the case where Dr.temp exceeds PAW, result will be 0, not negative
    } else {1}
  }
  KcactCalc <- function(Ks, Kcb, Kei, Kep, i) {#equation 4 in Allen et al. 2005
    Ks[i] * Kcb[i] + Kei[i] + Kep[i]
  }
  ETcactCalc <- function(Kc.act, ETo, i) {#equation 3 in Allen et al. 2005
    Kc.act[i] * ETo[i]
  }
  DrEndCalc <- function(Dr.end, P, Ir, Kc.act, ETo, DPr, i) { #NOT TO BE USED for i=1
    Dr.end[i - 1] - P[i] - Ir[i-1] + Kc.act[i] * ETo[i] + DPr[i]
  }
  #results function to summarize each model's results
  #find time to first and last irrigation
  #Jdev is 1 and Jharv is 365 for alfalfa.CV and alfalfa.imperial; Jdev is Jini for alfalfa.intermountain and Jharv is doy 327 (complete dormancy), even though actual irrigations are shortened to <doy293 for alfalfa.intermountain and <doy322 for alfalfa.CV
  #can use sum(df$Ir) in lieu of length(which(df$Ir)) as long as no NAs present in df$Ir, which there should not be --in this case--sum(df$Ir, na.rm=TRUE)
  IrDateCalc <- function(d.f) { #as.Date('1900-01-01) is a proxy for NA
    first.irr.index <- if (sum(d.f$Ir > 0) > 1 & d.f$doys.model[1] <= Jdev) {
      head(which(d.f$Ir > 0), 1)} else {
        if (sum(d.f$Ir > 0) == 1 & d.f$doys.model[1] <= Jdev) { #in the case that overall model starts after Jdev, which it does in 2003
          which(d.f$Ir > 0)} else {vector()}
      }
    last.irr.index <- if (sum(d.f$Ir > 0) > 1 & d.f$doys.model[nrow(d.f)] >= Jharv - days.no.irr) {
      tail(which(d.f$Ir > 0), 1)} else {
        if (sum(d.f$Ir > 0) == 1 & d.f$doys.model[nrow(d.f)] >= Jharv - days.no.irr) {
          which(d.f$Ir > 0)} else {vector()}
      }
    all.irr.doys <-if (sum(d.f$Ir > 0) > 0) {paste(as.character(d.f$doys.model[d.f$Ir > 0]), collapse = '.')} else {NA}
    irr.count <- sum(d.f$Ir > 0)
    if (length(first.irr.index) == 0 & length(last.irr.index) == 0) {
      data.frame(Irr.1=as.Date('1900-01-01'), Irr.Last=as.Date('1900-01-01'), Irr.All=NA, Irr.Count=irr.count)
    }
    else if (length(first.irr.index) == 0) {
      data.frame(Irr.1=as.Date('1900-01-01'), Irr.Last=d.f$dates[last.irr.index], Irr.All=all.irr.doys, Irr.Count=irr.count, stringsAsFactors = FALSE)
    }
    else if (length(last.irr.index) == 0) {
      data.frame(Irr.1=d.f$dates[first.irr.index], Irr.Last=as.Date('1900-01-01'), Irr.All=all.irr.doys, Irr.Count=irr.count, stringsAsFactors = FALSE)
    } else {
      data.frame(Irr.1=d.f$dates[first.irr.index], Irr.Last=d.f$dates[last.irr.index], Irr.All=all.irr.doys, Irr.Count=irr.count, stringsAsFactors = FALSE)
    }
  }
#do.call(rbind, lapply(split(model.result, model.result$years), IrDateCalc))
  
  #calculate Green Water utilized
  #this asssumes that residual irrigation water storage from previous fall will not contribute to the following year's growing season ET for the purpose of calculating green water utilization.  A correction is no longer applied to the growing season ET for residual irrigation water storage (using the 'irr_end_storage' variable) to attempt to correctly estimate green water utilization within the same year.
  #past definitation for year's with a final irrigation: GW.ET.growing = sum(df$ETc.act[jdev_index:jharv_index] - df$Ir[jdev_index:jharv_index]) + irr_end_storage, where
  #irr_storage <- max(AD - df$Dr.end[jharv_index] - sum(df$P[last.irr.index:jharv_index])
  #RAW is readily available water
  #PAW is plant available water
  #note that GW.ET.to.Irr1 is not relevant for alfalfa.imperial
  #new simplification of function to ignore results for partial years
  WaterBalanceCalc <- function(d.f, stress.point=stress.assumption*PAW) { #concept is to run by year and get growing season results relative to Jdev-Jharv period each year for the respective crop
    #d.f <- model.result[which(model.result$years==2004),]
    leap.year <- if(d.f$years[1]%%4==0 & d.f$years[1]%%100 != 0) {TRUE} else if (d.f$years[1]%%400==0) {TRUE} else {FALSE}
    if (leap.year & nrow(d.f) < 366) {
      return(data.frame(Dr.begin.year = NA, Dr.begin.season = NA, Dr.end.season = NA, P.end.season=NA, Dr.end.year = NA, GW.ET.growing = NA, Irr.app.total = NA, Irr.app.last = NA, ET.growing = NA, E.growing = NA, T.growing = NA, crop.stress.growing = NA, deep.perc.growing = NA, ET.annual = NA, E.annual = NA, T.annual = NA, crop.stress.annual = NA, deep.perc.annual = NA, non.irr.deep.perc = NA, Irr1.to.last.deep.perc = NA, fall.deep.perc = NA, GW.ET.to.Irr1=NA, GW.E.to.Irr1=NA, GW.T.to.Irr1=NA))
    }
    if (!leap.year & nrow(d.f) < 365) {
      return(data.frame(Dr.begin.year = NA, Dr.begin.season = NA, Dr.end.season = NA, P.end.season=NA, Dr.end.year = NA, GW.ET.growing = NA, Irr.app.total = NA, Irr.app.last = NA, ET.growing = NA, E.growing = NA, T.growing = NA, crop.stress.growing = NA, deep.perc.growing = NA, ET.annual = NA, E.annual = NA, T.annual = NA, crop.stress.annual = NA, deep.perc.annual = NA, non.irr.deep.perc = NA, Irr1.to.last.deep.perc = NA, fall.deep.perc = NA, GW.ET.to.Irr1=NA, GW.E.to.Irr1=NA, GW.T.to.Irr1=NA))
    }
    first.irr.index <- if (sum(d.f$Ir > 0) > 1) {
      head(which(d.f$Ir > 0), 1)} else {
        if (sum(d.f$Ir > 0) == 1) {
          which(d.f$Ir > 0)} else {vector()}
      }
    last.irr.index <- if (sum(d.f$Ir >0) > 1) {
      tail(which(d.f$Ir > 0), 1)} else {
        if (length(which(d.f$Ir >0)) == 1) {
          which(d.f$Ir > 0)} else {vector()}
      }
    jharv_index <- if (cropname != 'alfalfa.CV' & cropname != 'alfalfa.imperial') {which(d.f$doys.model==Jharv)} else {nrow(d.f)} #Jharv is redefined as 365 during model initialization but redefined here as nrow(d.f) to accurately deal with leap years
    jdev_index <- which(d.f$doys.model==Jdev) #Jdev is redefined as doy 1 for alfalfa.imperial & alfalfa.CV during model initialization for these crops for the purpose of tabulating results
    final_doy_index <- nrow(d.f)
    if (sum(d.f$Ir > 0) == 0) { #but in the event no irrigations occurred
      data.frame(Dr.begin.year = d.f$Dr.end[1], Dr.begin.season = d.f$Dr.end[jdev_index], Dr.end.season = d.f$Dr.end[jharv_index], P.end.season = NA, Dr.end.year = d.f$Dr.end[final_doy_index], GW.ET.growing = sum(d.f$ETc.act[jdev_index:jharv_index], -d.f$Ir), Irr.app.total = 0, Irr.app.last = 0, ET.growing = sum(d.f$ETc.act[jdev_index:jharv_index]), E.growing = sum(d.f$Ei[jdev_index:jharv_index], d.f$Ep[jdev_index:jharv_index]), T.growing = sum(d.f$ETc.act[jdev_index:jharv_index], -d.f$Ei[jdev_index:jharv_index], -d.f$Ep[jdev_index:jharv_index]), crop.stress.growing=sum(d.f$ETc.ns[jdev_index:jharv_index], -d.f$ETc.act[jdev_index:jharv_index]), deep.perc.growing=sum(d.f$DPr[jdev_index:jharv_index]), ET.annual = sum(d.f$ETc.act), E.annual = sum(d.f$Ei, d.f$Ep), T.annual = sum(d.f$ETc.act, -d.f$Ei, -d.f$Ep), crop.stress.annual = sum(d.f$ETc.ns, -d.f$ETc.act), deep.perc.annual = sum(d.f$DPr), non.irr.deep.perc = NA, Irr1.to.last.deep.perc = NA, fall.deep.perc = NA, GW.ET.to.Irr1=NA, GW.E.to.Irr1=NA, GW.T.to.Irr1=NA)
    } else {
      data.frame(Dr.begin.year = d.f$Dr.end[1], Dr.begin.season = d.f$Dr.end[jdev_index], Dr.end.season = d.f$Dr.end[jharv_index], P.end.season = sum(d.f$P[last.irr.index:jharv_index]), Dr.end.year = d.f$Dr.end[final_doy_index], GW.ET.growing = sum(d.f$ETc.act[jdev_index:jharv_index], -d.f$Ir), Irr.app.total = sum(d.f$Ir), Irr.app.last = d.f$Ir[last.irr.index], ET.growing = sum(d.f$ETc.act[jdev_index:jharv_index]), E.growing = sum(d.f$Ei[jdev_index:jharv_index], d.f$Ep[jdev_index:jharv_index]), T.growing = sum(d.f$ETc.act[jdev_index:jharv_index], -d.f$Ei[jdev_index:jharv_index], -d.f$Ep[jdev_index:jharv_index]), crop.stress.growing=sum(d.f$ETc.ns[jdev_index:jharv_index], -d.f$ETc.act[jdev_index:jharv_index]), deep.perc.growing=sum(d.f$DPr[jdev_index:jharv_index]), ET.annual = sum(d.f$ETc.act), E.annual = sum(d.f$Ei, d.f$Ep), T.annual = sum(d.f$ETc.act, -d.f$Ei, -d.f$Ep), crop.stress.annual = sum(d.f$ETc.ns, -d.f$ETc.act), deep.perc.annual = sum(d.f$DPr), non.irr.deep.perc = sum(d.f$DPr[1:first.irr.index], d.f$DPr[last.irr.index:final_doy_index]), Irr1.to.last.deep.perc = sum(d.f$DPr[first.irr.index:last.irr.index]), fall.deep.perc = sum(d.f$DPr[last.irr.index:final_doy_index]), GW.ET.to.Irr1 = sum(d.f$ETc.act[jdev_index:first.irr.index]), GW.E.to.Irr1 = sum(d.f$Ei[jdev_index:first.irr.index] + d.f$Ep[jdev_index:first.irr.index]), GW.T.to.Irr1 = sum(d.f$Kcb.adjusted[jdev_index:first.irr.index] * d.f$Ks[jdev_index:first.irr.index] * d.f$ETo[jdev_index:first.irr.index])) #from this, can calculate dormant season values for E, ET, and H2O.stress later
    }
  }
  do.call(rbind, lapply(split(model.result, model.result$years), WaterBalanceCalc))
  
  WaterBalanceMonthly <- function(d.f) {
    leap.year <- if(d.f$years[1]%%4==0 & d.f$years[1]%%100 != 0) {TRUE} else if (d.f$years[1]%%400==0) {TRUE} else {FALSE}
    if (leap.year & nrow(d.f) < 366) {
      data.frame(IrrApp.Jan = NA, IrrApp.Feb = NA, IrrApp.Mar = NA, IrrApp.Apr = NA, IrrApp.May = NA, IrrApp.Jun = NA, IrrApp.Jul = NA, IrrApp.Aug = NA, IrrApp.Sep = NA, IrrApp.Oct = NA, IrrApp.Nov = NA, IrrApp.Dec = NA, ET.Jan = NA, ET.Feb = NA, ET.Mar = NA, ET.Apr = NA, ET.May = NA, ET.Jun = NA, ET.Jul = NA, ET.Aug = NA, ET.Sep = NA, ET.Oct = NA, ET.Nov = NA, ET.Dec = NA, E.Jan = NA, E.Feb = NA, E.Mar = NA, E.Apr = NA, E.May = NA, E.Jun = NA, E.Jul = NA, E.Aug = NA, E.Sep = NA, E.Oct = NA, E.Nov = NA, E.Dec = NA, DeepPerc.Jan = NA, DeepPerc.Feb = NA, DeepPerc.Mar = NA, DeepPerc.Apr = NA, DeepPerc.May = NA, DeepPerc.Jun = NA, DeepPerc.Jul = NA, DeepPerc.Aug = NA, DeepPerc.Sep = NA, DeepPerc.Oct = NA, DeepPerc.Nov = NA, DeepPerc.Dec = NA, CropStress.Jan = NA, CropStress.Feb = NA, CropStress.Mar = NA, CropStress.Apr = NA, CropStress.May = NA, CropStress.Jun = NA, CropStress.Jul = NA, CropStress.Aug = NA, CropStress.Sep = NA, CropStress.Oct = NA, CropStress.Nov = NA, CropStress.Dec = NA)
    }
    else if (!leap.year & nrow(d.f) < 365) {
      data.frame(IrrApp.Jan = NA, IrrApp.Feb = NA, IrrApp.Mar = NA, IrrApp.Apr = NA, IrrApp.May = NA, IrrApp.Jun = NA, IrrApp.Jul = NA, IrrApp.Aug = NA, IrrApp.Sep = NA, IrrApp.Oct = NA, IrrApp.Nov = NA, IrrApp.Dec = NA, ET.Jan = NA, ET.Feb = NA, ET.Mar = NA, ET.Apr = NA, ET.May = NA, ET.Jun = NA, ET.Jul = NA, ET.Aug = NA, ET.Sep = NA, ET.Oct = NA, ET.Nov = NA, ET.Dec = NA, E.Jan = NA, E.Feb = NA, E.Mar = NA, E.Apr = NA, E.May = NA, E.Jun = NA, E.Jul = NA, E.Aug = NA, E.Sep = NA, E.Oct = NA, E.Nov = NA, E.Dec = NA, DeepPerc.Jan = NA, DeepPerc.Feb = NA, DeepPerc.Mar = NA, DeepPerc.Apr = NA, DeepPerc.May = NA, DeepPerc.Jun = NA, DeepPerc.Jul = NA, DeepPerc.Aug = NA, DeepPerc.Sep = NA, DeepPerc.Oct = NA, DeepPerc.Nov = NA, DeepPerc.Dec = NA, CropStress.Jan = NA, CropStress.Feb = NA, CropStress.Mar = NA, CropStress.Apr = NA, CropStress.May = NA, CropStress.Jun = NA, CropStress.Jul = NA, CropStress.Aug = NA, CropStress.Sep = NA, CropStress.Oct = NA, CropStress.Nov = NA, CropStress.Dec = NA)
    } else {
        jan_indices <- which(d.f$months==1)
        feb_indices <- which(d.f$months==2)
        mar_indices <- which(d.f$months==3)
        apr_indices <- which(d.f$months==4)
        may_indices <- which(d.f$months==5)
        jun_indices <- which(d.f$months==6)
        jul_indices <- which(d.f$months==7)
        aug_indices <- which(d.f$months==8)
        sep_indices <- which(d.f$months==9)
        oct_indices <- which(d.f$months==10)
        nov_indices <- which(d.f$months==11)
        dec_indices <- which(d.f$months==12)
        data.frame(IrrApp.Jan = sum(d.f$Ir[jan_indices]), IrrApp.Feb = sum(d.f$Ir[feb_indices]), IrrApp.Mar = sum(d.f$Ir[mar_indices]), IrrApp.Apr = sum(d.f$Ir[apr_indices]), IrrApp.May = sum(d.f$Ir[may_indices]), IrrApp.Jun = sum(d.f$Ir[jun_indices]), IrrApp.Jul = sum(d.f$Ir[jul_indices]), IrrApp.Aug = sum(d.f$Ir[aug_indices]), IrrApp.Sep = sum(d.f$Ir[sep_indices]), IrrApp.Oct = sum(d.f$Ir[oct_indices]), IrrApp.Nov = sum(d.f$Ir[nov_indices]), IrrApp.Dec = sum(d.f$Ir[dec_indices]), ET.Jan = sum(d.f$ETc.act[jan_indices]), ET.Feb = sum(d.f$ETc.act[feb_indices]), ET.Mar = sum(d.f$ETc.act[mar_indices]), ET.Apr = sum(d.f$ETc.act[apr_indices]), ET.May = sum(d.f$ETc.act[may_indices]), ET.Jun = sum(d.f$ETc.act[jun_indices]), ET.Jul = sum(d.f$ETc.act[jul_indices]), ET.Aug = sum(d.f$ETc.act[aug_indices]), ET.Sep = sum(d.f$ETc.act[sep_indices]), ET.Oct = sum(d.f$ETc.act[oct_indices]), ET.Nov = sum(d.f$ETc.act[nov_indices]), ET.Dec = sum(d.f$ETc.act[dec_indices]), E.Jan = sum(d.f$Ei[jan_indices], d.f$Ep[jan_indices]), E.Feb = sum(d.f$Ei[feb_indices], d.f$Ep[feb_indices]), E.Mar = sum(d.f$Ei[mar_indices], d.f$Ep[mar_indices]), E.Apr = sum(d.f$Ei[apr_indices], d.f$Ep[apr_indices]), E.May = sum(d.f$Ei[may_indices], d.f$Ep[may_indices]), E.Jun = sum(d.f$Ei[jun_indices], d.f$Ep[jun_indices]), E.Jul = sum(d.f$Ei[jul_indices], d.f$Ep[jul_indices]), E.Aug = sum(d.f$Ei[aug_indices], d.f$Ep[aug_indices]), E.Sep = sum(d.f$Ei[sep_indices], d.f$Ep[sep_indices]), E.Oct = sum(d.f$Ei[oct_indices], d.f$Ep[oct_indices]), E.Nov = sum(d.f$Ei[nov_indices], d.f$Ep[nov_indices]), E.Dec = sum(d.f$Ei[dec_indices], d.f$Ep[dec_indices]), DeepPerc.Jan = sum(d.f$DPr[jan_indices]), DeepPerc.Feb = sum(d.f$DPr[feb_indices]), DeepPerc.Mar = sum(d.f$DPr[mar_indices]), DeepPerc.Apr = sum(d.f$DPr[apr_indices]), DeepPerc.May = sum(d.f$DPr[may_indices]), DeepPerc.Jun = sum(d.f$DPr[jun_indices]), DeepPerc.Jul = sum(d.f$DPr[jul_indices]), DeepPerc.Aug = sum(d.f$DPr[aug_indices]), DeepPerc.Sep = sum(d.f$DPr[sep_indices]), DeepPerc.Oct = sum(d.f$DPr[oct_indices]), DeepPerc.Nov = sum(d.f$DPr[nov_indices]), DeepPerc.Dec = sum(d.f$DPr[dec_indices]), CropStress.Jan = sum(d.f$ETc.ns[jan_indices], -d.f$ETc.act[jan_indices]), CropStress.Feb = sum(d.f$ETc.ns[feb_indices], -d.f$ETc.act[feb_indices]), CropStress.Mar = sum(d.f$ETc.ns[mar_indices], -d.f$ETc.act[mar_indices]), CropStress.Apr = sum(d.f$ETc.ns[apr_indices], -d.f$ETc.act[apr_indices]), CropStress.May =sum(d.f$ETc.ns[may_indices], -d.f$ETc.act[may_indices]), CropStress.Jun = sum(d.f$ETc.ns[jun_indices], -d.f$ETc.act[jun_indices]), CropStress.Jul = sum(d.f$ETc.ns[jul_indices], -d.f$ETc.act[jul_indices]), CropStress.Aug = sum(d.f$ETc.ns[aug_indices], -d.f$ETc.act[aug_indices]), CropStress.Sep = sum(d.f$ETc.ns[sep_indices], -d.f$ETc.act[sep_indices]), CropStress.Oct = sum(d.f$ETc.ns[oct_indices], -d.f$ETc.act[oct_indices]), CropStress.Nov = sum(d.f$ETc.ns[nov_indices], -d.f$ETc.act[nov_indices]), CropStress.Dec = sum(d.f$ETc.ns[dec_indices], -d.f$ETc.act[dec_indices]))
    }
  }
  #do.call(rbind, lapply(split(model.result, model.result$years), WaterBalanceMonthly))
  
  if (length(U2.df$DOY)==length(RHmin.df$DOY) & length(U2.df$DOY)==length(ETo.df$DOY)) {
    doys.model <- U2.df$DOY
    dates <- as.Date(U2.df$dates, format='%m_%d_%Y') #'%b_%d_%Y'
    days <- U2.df$day
    months <- U2.df$month
    years <- U2.df$year
    water.year <- years
    water.year[which(months >= 10)] <- years[which(months >= 10)] + 1
    print('Temporal coverages match in Spatial CIMIS.')
  } else {
    print('There are differing temporal coverages in the Spatial CIMIS data.')
  }
  model.length <- nrow(ETo.df)
  #get precip and spatial CIMIS data to same temporal end point
  last.date <- ETo.df$dates[nrow(ETo.df)]
  P.df <- P.df[1:which(P.df$dates==last.date), ] #trim the P dataset to match spatial CIMIS
  cropname.dir <-  paste0(cropname, '_majcomps')
  scenario.name <- paste0(cropname.dir, '/scenario_', root_depth, as.character(AD.percentage), 'AD') #need to add other variables here if performing sensitivity analysis
  if (cropname=='alfalfa.intermountain' | cropname=='alfalfa.CV' | cropname == 'alfalfa.imperial') {
    paw.var <- paste0('z', root_depth, '_cmH2O_unmodified_comp') #use unmodified AWC SSURGO data for alfalfa only
  } else {
      paw.var <- paste0('z', root_depth, '_cmH2O_modified_comp') #for other crops use modified SSURGO data; see 'SSURGO_allCA_aggregation_awc_r.R' for details
  }
  if (!dir.exists(file.path(resultsDir, cropname.dir))) {
    dir.create(file.path(resultsDir, cropname.dir))
  }
  if (!dir.exists(file.path(resultsDir, scenario.name))) {
    dir.create(file.path(resultsDir, scenario.name))
  }
#initialization assumptions
  first_irrigation <- 0
  TEW.fraction <- 0.5
#limit model.scaffold to cropname and location, if applicable
  if (cropname=='alfalfa.intermountain' | cropname=='alfalfa.CV' | cropname=='alfalfa.imperial') {
    model.scaffold.crop <- model.scaffold[which(model.scaffold$cropcode==cropcode & model.scaffold$alfalfa.zone==alfalfa.zone), ]
  } else if (cropname=='grapes.table' | cropname=='grapes.wine') {
    model.scaffold.crop <- model.scaffold[grepl(grape.zone, model.scaffold$grape.zone), ] #wine.grapes will be run with 'Coast, Foothills, and Mountains'; grapes.table will be run for 'Central Valley'; note that grepl returns FALSE for NAs in x
  } else {
      model.scaffold.crop <- model.scaffold[which(model.scaffold$cropcode==cropcode), ]
  }
#make a results data.frame
  if (results_file == 'new') {
    model.length.yrs <- max(ETo.df$year) - min(ETo.df$year) + 1 #data starts 10/1/2003 & ends 3/8/18
    paw.vector <- model.scaffold.crop[[paw.var]]
    #take out unnecessary paw data from model scaffold
    model.scaffold2 <- model.scaffold.crop[ ,-which(grepl('cmH2O', colnames(model.scaffold.crop)))] 
    model.scaffold2[[paw.var]] <- paw.vector #put back in relevant paw data and use this as basis for QC results template below
    model.scaffold3 <- model.scaffold2[ ,c('mukey', 'cropcode', 'PRISMcellnumber', 'CIMIScellnumber', 'unique_model_code', 'cokey', 'comppct_r', 'TEW', 'REW', 'surface.depth')] #and this for annual results template to be expanded below
    model.scaffold.results <- model.scaffold3[rep(seq.int(1, nrow(model.scaffold3)), model.length.yrs), 1:ncol(model.scaffold3)] #makes a new data.frame with each row repeated model.length.yrs number of times
    model.scaffold.results <- model.scaffold.results[order(model.scaffold.results$unique_model_code, model.scaffold.results$cokey), ] #for some reason it is produced out of order
    model.scaffold.results <- data.frame(model.scaffold.results, Model.Year=rep(seq(from=min(ETo.df$year), to=max(ETo.df$year), by=1), times=nrow(model.scaffold.crop)), Irr.1=as.Date('1900-01-01'), Irr.Last=as.Date('1900-01-01'), Irr.All=NA, Irr.Count=NA, Dr.begin.year = NA, Dr.begin.season = NA, Dr.end.season = NA, P.end.season=NA, Dr.end.year = NA, GW.ET.growing = NA, Irr.app.total = NA, Irr.app.last = NA, ET.growing = NA, E.growing = NA, T.growing = NA, crop.stress.growing = NA, deep.perc.growing = NA, ET.annual = NA, E.annual = NA, T.annual = NA, crop.stress.annual = NA, deep.perc.annual = NA, non.irr.deep.perc = NA, Irr1.to.last.deep.perc = NA, fall.deep.perc = NA, GW.ET.to.Irr1=NA, GW.E.to.Irr1=NA, GW.T.to.Irr1=NA, IrrApp.Jan = NA, IrrApp.Feb = NA, IrrApp.Mar = NA, IrrApp.Apr = NA, IrrApp.May = NA, IrrApp.Jun = NA, IrrApp.Jul = NA, IrrApp.Aug = NA, IrrApp.Sep = NA, IrrApp.Oct = NA, IrrApp.Nov = NA, IrrApp.Dec = NA, ET.Jan = NA, ET.Feb = NA, ET.Mar = NA, ET.Apr = NA, ET.May = NA, ET.Jun = NA, ET.Jul = NA, ET.Aug = NA, ET.Sep = NA, ET.Oct = NA, ET.Nov = NA, ET.Dec = NA, E.Jan = NA, E.Feb = NA, E.Mar = NA, E.Apr = NA, E.May = NA, E.Jun = NA, E.Jul = NA, E.Aug = NA, E.Sep = NA, E.Oct = NA, E.Nov = NA, E.Dec = NA, DeepPerc.Jan = NA, DeepPerc.Feb = NA, DeepPerc.Mar = NA, DeepPerc.Apr = NA, DeepPerc.May = NA, DeepPerc.Jun = NA, DeepPerc.Jul = NA, DeepPerc.Aug = NA, DeepPerc.Sep = NA, DeepPerc.Oct = NA, DeepPerc.Nov = NA, DeepPerc.Dec = NA, CropStress.Jan = NA, CropStress.Feb = NA, CropStress.Mar = NA, CropStress.Apr = NA, CropStress.May = NA, CropStress.Jun = NA, CropStress.Jul = NA, CropStress.Aug = NA, CropStress.Sep = NA, CropStress.Oct = NA, CropStress.Nov = NA, CropStress.Dec = NA)
    QC.results <- data.frame(model.scaffold2, DaysNoIrr=NA, WaterBlncError=NA, WaterBlncInput=NA, NegFlags=NA)
    Dr.results <- data.frame(matrix(data=NA, nrow=nrow(model.scaffold.crop), ncol=2+length(dates)))
    Dr.results$unique_model_code <- model.scaffold.crop$unique_model_code
    Dr.results$cokey <- model.scaffold.crop$cokey
    colnames(Dr.results) <- c('unique_model_code', 'cokey', format.Date(dates, '%b_%d_%Y'))
    rm(model.scaffold2, model.scaffold3)
  } else {  ###THIS NEEDS TO BE EDITED FOR INTERRUPTED RUNS
    #fname <- list.files(pattern = glob2rx('*_FAO56results.csv'))
    #model.scaffold.results <- read.csv(fname, stringsAsFactors = FALSE)
    model.scaffold.results <- read.csv(file.path(resultsDir, scenario.name, results_file), stringsAsFactors = FALSE)
    model.scaffold.results$Irr.1 <- as.Date(model.scaffold.results$Irr.1)
    model.scaffold.results$Irr.Last <- as.Date(model.scaffold.results$Irr.Last)
  }
  crop.parameters <- CropParametersDefine(crop.parameters.df, cropname)
  Kcb.std <- KcbDefine(doys.model, cropname) #this will be substituted with a crop code
  fc <- fcCalc(doys.model, crop.parameters, cropname) #TO-DO: implement alternative fc calculation in accordance with Eq. 11 from Allen et al. 2005: ((Kcb-Kcmin)/(Kcmax-Kcmin))^(1+0.5*h).  However, this produced a strange result in spreadsheet model for almonds, where increasing h decreases fc.
  if (cropname == 'alfalfa.intermountain' | cropname == 'alfalfa.CV' | cropname == 'alfalfa.imperial') {
    harvest.days <- HarvestDays(cropname)
  }
  if (cropname=='alfalfa.intermountain') {
    Jdev <- crop.parameters$Jini[crop.parameters$crop==cropname] #Jdev is intended to represent start of growing season as DOY
  } else if (cropname == 'alfalfa.imperial' | cropname=='alfalfa.CV') {
      Jdev <- 1 #day 1 is start of green alfalfa growth, though supressed by cold
  } else {
      Jdev <- crop.parameters$Jdev[crop.parameters$crop==cropname]
  }
  Jmid <- crop.parameters$Jmid[crop.parameters$crop==cropname]
  Jlate <- crop.parameters$Jlate[crop.parameters$crop==cropname]
  if (cropname=='alfalfa.intermountain') {
    Jharv <- harvest.days[length(harvest.days)] + crop.parameters$Lini.cycle[crop.parameters$crop==cropname] + crop.parameters$Ldev.cycle[crop.parameters$crop==cropname] + crop.parameters$Lmid.cycle[crop.parameters$crop==cropname] + crop.parameters$Lfrost.kill[crop.parameters$crop==cropname] #Jharv is intended to represent end of growing season where alfalfa is dormant in this region, approx. 11/23 that would be best varied as function of TMIN
  } else if (cropname == 'alfalfa.imperial' | cropname=='alfalfa.CV') {
    Jharv <- 365 #this no longer serves a purpose as complete model years are now only used for tabulating results and nrow(df) by year is taken as Jharv for Imperial Valley alfalfa
  } else {
      Jharv <- crop.parameters$Jharv[crop.parameters$crop==cropname]
  }
  buffer.days <- crop.parameters$L.final.irr.buffer.days
  fw <- fwSelect(irrigation.parameters, irr.type)
  fewi <- fewiCalc(fc, fw)
  fewp <- fewpCalc(fc, fewi)
#loop through all rows of model scaffold but only do these operations once for each model.scaffold.crop row
  set.seed(461980)
  rows.to.sample <- sample(1:nrow(model.scaffold.crop), 0.01*nrow(model.scaffold.crop))
  if (nrow(model.scaffold.crop) > 10000) {
    save.times <- seq(from=10000, to=nrow(model.scaffold.crop), by=10000)
  } else {save.times <- 1000}
  for (n in row_start:nrow(model.scaffold.crop)) {
    model.code <- model.scaffold.crop$unique_model_code[n]
    PAW <- model.scaffold.crop[[paw.var]][n]*10
    AD <- (AD.percentage/100)*PAW
    cokey <- model.scaffold.crop$cokey[n]
    REW.parameter <- model.scaffold.crop$REW[n]
    TEW.parameter <- model.scaffold.crop$TEW[n]
    if (is.na(AD) | is.na(REW.parameter) | is.na(TEW.parameter)) {
      next(print(paste('Soils data is missing for cokey ', as.character(cokey)))) #TO-DO: write this result to separate file of NAs
    }
    if (AD==0 | TEW.parameter==0 | REW.parameter==0) {
      print(paste('AD, TEW, or REW is 0 for cokey ', as.character(cokey)))
      next
    }
    if (REW.parameter > TEW.parameter) { #there are several hundered instances where this was the result of the SSURGO aggregation work
      REW.parameter <- TEW.parameter
    }
  #identify climatic and soil parameters
    spCIMIScell <- model.scaffold.crop$CIMIScellnumber[n]
    PRISMcell <- model.scaffold.crop$PRISMcellnumber[n]
    comppctr <- model.scaffold.crop$comppct_r[n]
    mukey <- model.scaffold.crop$mukey[n]
    P <- P.df[ ,paste0('cell_', as.character(PRISMcell))] #was this: which(colnames(P.df)==paste0('cell_', as.character(PRISMcell))), note that comma followed by column name maintains numeric class
    ETo <- ETo.df[ ,paste0('cell_', as.character(spCIMIScell))]
    U2 <- U2.df[ ,paste0('cell_', as.character(spCIMIScell))]
    RHmin <- RHmin.df[ ,paste0('cell_', as.character(spCIMIScell))]
    if (all(is.na(P)) | all(is.na(ETo))) {
      print(paste('Climate data is missing for scenario number', as.character(n))) #TO-DO: write this result to separate file of NAs
      next
    }
    Kcb.df <- KcbAdj(Kcb.std, crop.parameters=crop.parameters, cropname, U2, RHmin) #object 'crop.parameters' not found
    Kcb.adjusted <- Kcb.df$Kcb.climate.adj
    days.no.irr <- DaysNoIrr(P, ETo, Kcb.adjusted, AD, doys.model, years, Jmid, Jharv, buffer.days)
    if (is.null(days.no.irr)) {
      print(paste0('Null value returned for days.no.irr for model.scaffold.crop n = ', as.character(n), ', likely a result of a low PAW: ', as.character(PAW), ' mm.'))
      next
    }
    Kcmax <- Kcb.df$Kc.max
    Dei.initial <- numeric(length = model.length)
    DPei <- numeric(length = model.length)
    Dep.initial <- numeric(length = model.length)
    Kri <- numeric(length = model.length)
    Kei <- numeric(length = model.length)
    Ei <- numeric(length = model.length)
    Dei.end <- numeric(length = model.length)
    Krp <- numeric(length = model.length)
    Kep <- numeric(length = model.length)
    Ep <- numeric(length = model.length)
    DPep <- numeric(length = model.length)
    Dep.end <- numeric(length = model.length)
    W <- numeric(length = model.length)
    Kc.ns <- numeric(length = model.length)
    ETc.ns <- numeric(length = model.length)
    Dr.initial <- numeric(length = model.length)
#assumes depletion is zero for root zone except for half of TEW in upper layer
    Ir <- numeric(length = model.length)
    DPr <- numeric(length = model.length)
    Ks <- numeric(length = model.length)
    Kc.act <- numeric(length = model.length)
    Dr.end <- numeric(length = model.length)
    ETc.act <- numeric(length = model.length)
  #now for i=1, which will have slighly modified execution as it is the model initialization
    Dei.initial[1] <- max(TEW.parameter * TEW.fraction - P[1], 0) #this is an initial estimate of 
  #the water balance for the exposed surface soil where irrigation is not applied 
  #that assumes all daily precip occurs early in the morning so as to estimate Ke 
  #and Kr.  Same done for Dep.initial
    Dep.initial[1] <- max(TEW.parameter * TEW.fraction - P[1], 0)
    Kri[1] <- KrCalc(TEW.parameter, REW.parameter, Dei.initial, 1)
    Krp[1] <- KrCalc(TEW.parameter, REW.parameter, Dep.initial, 1)
    W[1] <- WCalc(TEW.parameter, Dei.initial, Dep.initial, fewp, fewi, 1)
    DPei[1] <- max(P[1] - TEW.parameter * TEW.fraction, 0) #initial estimate assumes irrigation is zero on previous day
    DPep[1] <- DPepCalc(P, Dep.initial, 1)
    Kei[1] <- min(Kri[1] * W[1] * (Kcmax[1] - Kcb.adjusted[1]), fewi[1] * Kcmax[1])
    Kep[1] <- min(Krp[1] * (1 - W[1]) * (Kcmax[1] - Kcb.adjusted[1]), fewp[1] * Kcmax[1])
    Ep[1] <- EpCalc(ETo, Kep, 1)
    Ei[1] <- EiCalc(ETo, Kei, 1)
    Dep.end[1] <- min(TEW.parameter, max(TEW.parameter * TEW.fraction - P[1] + Ep[1] / fewp[1] + DPep[1], 0)) #replaces Dep.end[i-1] with TEW.parameter * TEW.fraction
    Dei.end[1] <- min(TEW.parameter, max(TEW.parameter * TEW.fraction - P[1] + Ei[1] / fewi[1] + DPei[1], 0)) #replaces Dei.end[i-1] with Dei.intial[i]
    Kc.ns[1] <- KcnsCalc(Kcb.adjusted, Kei, Kep, 1)
    ETc.ns[1] <- ETcnsCalc(Kc.ns, ETo, 1)
    Dr.initial[1] <- max(TEW.parameter * TEW.fraction - P[1] + ETc.ns[1], 0) #initial calc
    Ks[1] <-  if (max(TEW.parameter * TEW.fraction - P[1], 0) > stress.assumption*PAW) {
                max((PAW - max(TEW.parameter * TEW.fraction - P[1], 0)) / (PAW - stress.assumption*PAW), 0)
              } else {1}
    Ir[1] <- IrCalc(Ks, RDI.min, AD, Dr.initial, doys.model, Jdev, Jharv, days.no.irr, 1, buffer.days)
    DPr[1] <- max(max(P[1] + Ir[1] - TEW.parameter * TEW.fraction - ETc.ns[1], 0)) #initial calc
    Kc.act[1] <- KcactCalc(Ks, Kcb.adjusted, Kei, Kep, 1)
    ETc.act[1] <- ETcactCalc(Kc.act, ETo, 1)
    Dr.end[1] <- TEW.parameter * TEW.fraction - P[1] + Kc.act[1] * ETo[1] + DPr[1] #initial calc
    for (i in 2:model.length) { #now for days 2...model.length after initialization
      Dei.initial[i] <- DeiInitialCalc(Dei.end, P, Ir, fw, i)
      Dep.initial[i] <- DepInitialCalc(Dep.end, P, i)
      Kri[i] <- KrCalc(TEW.parameter, REW.parameter, Dei.initial, i)
      Krp[i] <- KrCalc(TEW.parameter, REW.parameter, Dep.initial, i)
      W[i] <- WCalc(TEW.parameter, Dei.initial, Dep.initial, fewp, fewi, i)
      DPep[i] <- DPepCalc(P, Dep.end, i) #changed to Dep.end instead of Dep.initial 4/5/18
      DPei[i] <- DPeiCalc(P, Ir, fw, Dei.end, i) #changed to Dei.end instead of Dei.initial 4/5/18
      Kei[i] <- KeiCalc(Kri, W, Kcmax, Kcb.adjusted, fewi, TEW.parameter, Dei.end, DPei, P, ETo, i)
      Kep[i] <- KepCalc(Krp, W, Kcmax, Kcb.adjusted, fewp, TEW.parameter, Dep.end, DPep, P, ETo, i)
      Ep[i] <- EpCalc(ETo, Kep, i)
      Ei[i] <- EiCalc(ETo, Kei, i)
      Dep.end[i] <- DepEndCalc(Dep.end, P, Ep, fewp, DPep, i)
      Dei.end[i] <- DeiEndCalc(Dei.end, P, Ir, fw, fewi, Ei, DPei, i)
      Kc.ns[i] <- KcnsCalc(Kcb.adjusted, Kei, Kep, i)
      ETc.ns[i] <- ETcnsCalc(Kc.ns, ETo, i)
      Dr.initial[i] <- DrInitialCalc(Dr.end, ETc.ns, P, Ir, i)
      Ks[i] <- KsCalc(Dr.end = Dr.initial, P = P, Ir = Ir, PAW = PAW, stress.point = stress.assumption * PAW, i = i) #corrected on 10/18/17
      Ir[i] <- IrCalc(Ks, RDI.min, AD, Dr.initial, doys.model, Jdev, Jharv, days.no.irr, i, buffer.days)
      DPr[i] <- DPrCalc(P, Ir, ETc.ns, Dr.end, i)
      Kc.act[i] <- KcactCalc(Ks, Kcb.adjusted, Kei, Kep, i)
      Dr.end[i] <- DrEndCalc(Dr.end, P, Ir, Kc.act, ETo, DPr, i)
      ETc.act[i] <- ETcactCalc(Kc.act, ETo, i) #could take this out of loop
    }
    model.result <- data.frame(dates, months, days, years, water.year, doys.model, P, ETo, RHmin, U2, Kcb.std, Kcb.adjusted, Kcmax, fc, fw, fewi, fewp, Dei.initial, Dep.initial, Kri, Krp, W, Kei, Kep, Ei, Ep, DPei, DPep, Dei.end, Dep.end, Kc.ns, ETc.ns, Dr.initial, Ir, DPr, Ks, Kc.act, ETc.act, Dr.end)
    annual.summary <- cbind(do.call(rbind, lapply(split(model.result, model.result$years), IrDateCalc)), do.call(rbind, lapply(split(model.result, model.result$years), WaterBalanceCalc)), do.call(rbind, lapply(split(model.result, model.result$years), WaterBalanceMonthly))) #took out merge call since each results function now should result in equal row results
    model.scaffold.results[which(model.scaffold.results$unique_model_code==model.code & model.scaffold.results$cokey == cokey), which(colnames(model.scaffold.results)=='Irr.1'):(which(colnames(model.scaffold.results)=='CropStress.Dec'))] <- annual.summary
    QC.results[which(QC.results$unique_model_code==model.code & QC.results$cokey == cokey), which(colnames(QC.results)=='DaysNoIrr'):(which(colnames(QC.results)=='NegFlags'))] <- cbind(DaysNoIrr=days.no.irr, WaterBlncError=sum(model.result$P) + sum(model.result$Ir[1:(nrow(model.result) - 1)]) - sum(model.result$ETc.act) - sum(model.result$DPr) + (model.result$Dr.end[nrow(model.result)] - model.result$Dr.initial[1]), WaterBlncInput=sum(model.result$P, model.result$Ir[1:(nrow(model.result) - 1)]), NegFlags=sum(subset(model.result, select = -c(Dei.initial, Dep.initial, Dr.initial, Dr.end)) < 0))
    Dr.results[which(Dr.results$unique_model_code == model.code & Dr.results$cokey == cokey), 3:ncol(Dr.results)] <- round(Dr.end, 3)
    print(paste(scenario.name, as.character(n)))
    if (n==1 | n %in% rows.to.sample) {
      model.result <- data.frame(dates, months, days, years, water.year, doys.model, P, ETo, RHmin, U2, lapply(X=list(Kcb.std=Kcb.std, Kcb.adjusted=Kcb.adjusted, Kcmax=Kcmax, fceff=fc, fw=fw, fewi=fewi, fewp=fewp, Dei.initial=Dei.initial, Dep.initial=Dep.initial, Kri=Kri, Krp=Krp, W=W, Kei=Kei, Kep=Kep, Ei=Ei, Ep=Ep, DPei=DPei, DPep=DPep, Dei.end=Dei.end, Dep.end=Dep.end, Kc.ns=Kc.ns, ETc.ns=ETc.ns, Dr.initial=Dr.initial, Ir=Ir, DPr=DPr, Ks=Ks, Kc.act=Kc.act, ETc.act=ETc.act, Dr.end=Dr.end), round, digits=rounding_digits))
      write.csv(model.result, file.path(resultsDir, scenario.name, paste0(cropname, root_depth, 'AD', as.character(AD.percentage), '_', as.character(model.code), '_', as.character(cokey), '_', Sys.Date(), '.csv')), row.names=FALSE)
    }
    if (n %in% save.times) { #was (n==100 | n %in% save.times)
      write.csv(model.scaffold.results, file.path(resultsDir, scenario.name, paste0(cropname, root_depth, 'AD', as.character(AD.percentage), '_FAO56results.csv')), row.names=FALSE)
      write.csv(QC.results, file.path(resultsDir, scenario.name, paste0(cropname, root_depth, 'AD', as.character(AD.percentage), '_model_QC.results.csv')), row.names=FALSE)
    } else {next}
  } #now, outside of model.scaffold.crop loop, all scenarios run, save complete results, QC, and metadata files
  write.csv(model.scaffold.results, file.path(resultsDir, scenario.name, paste0(cropname, root_depth, 'AD', as.character(AD.percentage), '_FAO56results.csv')), row.names=FALSE)
  write.csv(QC.results, file.path(resultsDir, scenario.name, paste0(cropname, root_depth, 'AD', as.character(AD.percentage), '_model_QC.results.csv')), row.names=FALSE)
  write.csv(Dr.results, file.path(resultsDir, scenario.name, paste0(cropname, root_depth, 'AD', as.character(AD.percentage), '_model_Dr.results.csv')), row.names=FALSE)
  metadata <- cbind(data.frame(date.run=Sys.Date(), crop=cropname, alfalfa.zone=alfalfa.zone, grape.zone=grape.zone, cropscape.code=cropcode, AD.percentage=AD.percentage, RDI.min=RDI.min, stress.assumption=stress.assumption, rooting.depth=root_depth, irrigation.type=irr.type, paw.varname = paw.var, model.days=model.length, first.day=dates[1], last.day=dates[length(dates)], n.models=nrow(model.scaffold.crop)), crop.parameters[which(crop.parameters$crop==cropname), 2:ncol(crop.parameters)], irrigation.parameters[which(irrigation.parameters$irrigation.type==irr.type), 'fw'])
  colnames(metadata)[ncol(metadata)] <- 'fw'
  write.csv(metadata, file.path(resultsDir, scenario.name, paste0(cropname, root_depth, 'AD', as.character(AD.percentage), '_model_metadata.csv')), row.names = FALSE)
}
###END OF FUNCTION
#arguments to function: cropname, cropcode, AD.percentage, root_depth, irr.type, crop.parameters.df, model.scaffold, U2.df, P.df, ETo.df, RHmin.df, results_file, row_start, RDI.min, alfalfa.zone, grape.zone, stress.assumption
#temp
lapply(model.result, function(x) sum(x < 0))
write.csv(model.result, file.path(resultsDir, 'TESTS', paste0(cropname, root_depth, 'AD', as.character(AD.percentage), '_', as.character(model.code), '_', as.character(cokey), '_', Sys.Date(), '.csv')), row.names=FALSE)

#incorporate into results
results$wb[i] <- sum(df$P) + sum(df$Ir[1:(nrow(df) - 1)]) - sum(df$ETc.act) - sum(df$DPr) + (df$Dr.end[nrow(df)] - df$Dr.initial[1])
results$rel.error[i] <- 100 * (results$wb[i] / sum(df$P, df$Ir[1:(nrow(df) - 1)]))

#legend for FAO56 abbreviations
#De,j=De,j-1 - P,j - Ij/fw + Ej/fewi + DPei,j (again, ignoring tranpiration from upper 10 cm and runoff, eqn. 21) 
#pseudo-code outline of 'separate prediction of evaporation from soil wetted by precipitation only' following Allen et al. 2005, except ignoring runoff, essentially assuming that runoff will really only occur when soils are near field capacity, so partitioning this as 'deep percolation' is acceptable and is consciously preferred over introduced errors from the curve number approach
#Ke = Kei + Kep (eqn. 14)
#where Kei=evaporation coefficient for the exposed fraction of the soil wetted by both irrigation and by precipitation and Kep=evaporation coefficient for the exposed fraction of the soil wetted by precipitation only
#Kei = Kri*W*(Kcmax-Kcb) <= fewi*Kcmax (eqn. 15)
#Kep = Krp*(1-W)*(Kcmax-Kcb) <= fewp*Kcmax (eqn. 16)
#where fewi=fraction of soil wetted by both irrigation and precipitation and is exposed to rapid drying due to exposure to solar radiation and/or ventilation and is calculated as: min(1-fc, fw) (eqn. 18);
#where fewp=fraction of soil exposed to rapid drying and is wetted by precipitation only and is calculated as: 1-fc-fewi (eqn. 17)
#where Kri and Krp=evaporation reduction coefficients for the fewi and fewp fractions, respectively
#where W=weighting coefficient for partitioning the energy available for evaporation in the fewi and fewp soil fractions, depending on water availability, and is calculated as: 1/(1+(fewp/fewi)*(TEW-Dep)/(TEW-De)) (eqn. 19);
#where De=cumulative depletion depth (mm) from the evaporating layer for the fewi fraction of soil
#where Dep=cumulative depletion depth (mm) from the evaporating layer for the fewp fraction of soil
#finally, the water balance formation for fraction of soil only wetted by precipitation:
#Dep,j=Dep,j-1 - Pj + Ep,j/fewp + DPep,j (ignoring transpiration from upper 10 cm and runoff, eqn. 20)
#where Dep,j-1 and Dep,j are the cumulative depletion depths at the ends of days j-1 and j in the fewp fraction of the surface (mm)
#where Ep,j=evaporation from kewp fraction on day j and is calculated as: Kep*ETo in mm
#where DPep,j=deep percolation from the fewp fraction of the evaporation layer on day j if soil water content exceeds field capacity
#where 0 <= Dep, j <= TEW
#TEW=total evaporable water in upper layer and is calculated as: 1000*(field_capacity-0.5*wilting_point)*Ze
#where Ze=effective depth of wetting and field capacity and wilting point is assumed to be moisture content at 1/3 and 15 bars, respectively, determined from the SSURGO database
#and the water balance formulation for the fraction of soil wetted by both precipitation and irrigation
#De,j=De,j-1 - P,j - Ij/fw + Ej/fewi + DPei,j (again, ignoring tranpiration from upper 10 cm and runoff, eqn. 21)
#where 0 <= De,j <= TEW
#where Kri=(TEW-De,j-1)/(TEW-REW) (eqn. 22)
#where Krp=(TEW-Dep,j-1)/(TEW-REW) (eqn. 23)
#where Dpei,j=Pj + Ij/fw - Dei,j-1 >= 0
#where DPep,j=Pj-Dep,j-1 >= 0
#Kr depends on daily water balance of surface layer (upper 10-15 cm); Few will also depend on daily water balance if separate calculations are done for soil wetted by precipitation vs. by both precip and irrigation (see "Separate Prediction of Evaporation from Soil Wetted by Precipitation Only" in Allen et al. 2005)

