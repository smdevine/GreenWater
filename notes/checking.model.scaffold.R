#model scaffold exploration after reading in FAO56_dualcropcoeff.v2.R
summary(model.scaffold$TEW)
sum(model.scaffold$TEW < 10)
sum(model.scaffold$TEW < 5)
unique(model.scaffold$compname[model.scaffold$TEW < 5])
unique(model.scaffold$cokey[model.scaffold$TEW < 5])
unique(model.scaffold$compname[model.scaffold$TEW < 10])
unique(model.scaffold$TEW[model.scaffold$TEW < 5])
sum(model.scaffold$TEW > model.scaffold$z0.5m_cmH2O_unmodified_comp*10)
sum(model.scaffold$TEW > model.scaffold$z1.0m_cmH2O_unmodified_comp*10)
sum(model.scaffold$TEW > model.scaffold$z0.5m_cmH2O_modified_comp*10)
sum(model.scaffold$TEW > model.scaffold$z1.0m_cmH2O_modified_comp*10)
unique(model.scaffold$compname[model.scaffold$TEW > model.scaffold$z0.5m_cmH2O_unmodified_comp*10])
model.scaffold.crop[model.scaffold.crop$TEW > model.scaffold.crop$z0.5m_cmH2O_modified_comp*10, ]
sum(model.scaffold.crop$unique_model_code==101362)
model.scaffold.crop[model.scaffold.crop$PRISMcellnumber==446907 & model.scaffold.crop$CIMIScellnumber==155748, ]
sum(model.scaffold$cropcode==69 & model.scaffold$PRISMcellnumber==510126 & model.scaffold$CIMIScellnumber==208777)
model.scaffold$grape.zone[model.scaffold$cropcode==69 & model.scaffold$PRISMcellnumber==510126 & model.scaffold$CIMIScellnumber==208777]
