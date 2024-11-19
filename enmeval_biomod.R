library(ggplot2)
library(ggtext)
library(terra)
library(tidyterra)
library(ENMeval)
library(raster)
library(dplyr)
library(dismo)
library(tidyr)
library(biomod2)
library(sf)
library(parallel)
options(cores = detectCores() - 1) 

#install.packages("ecospat")

setwd("D:/Research_Data_Backup/Eleutherodactylus_invasion")
#setwd("/scratch/negishi/jeon96/Eleutherodactylus_invasion")

# Read data
Ecoqui_envs.files <- list.files(path = "Ecoqui_contemp", pattern = ".tif$", full.names=TRUE)
Ecoqui_envs <- stack(Ecoqui_envs.files)
proj_wgs84 <- crs(Ecoqui_envs)

Ecoqui_points <- read.csv("Eleutherodactylus_invasion_arcgis/Ecoqui_rarefied_points.csv", header = T) # there are few weirdly formated rows. manually check and correct them.
Ecoqui_coordinates <- cbind(Ecoqui_points$decimalLon, Ecoqui_points$decimalLat)
colnames(Ecoqui_coordinates ) <- c("Lon", "Lat")
Ecoqui_occs <- as.data.frame(Ecoqui_coordinates)
#Ecoqui_occs <- SpatialPoints(Ecoqui_coordinates)
#projection(Ecoqui_occs) <- proj_wgs84
#Ecoqui_occs$Lon <- as.numeric(Ecoqui_occs$Lon)
#Ecoqui_occs$Lat <- as.numeric(Ecoqui_occs$Lat)
Ecoqui_occs.z <- cbind(Ecoqui_occs, raster::extract(Ecoqui_envs, Ecoqui_occs)) # extract raster values at points

Ecoqui_bias <- raster("Ecoqui_bias/ecoqui.asc")

# Plot first raster in the stack, the mean annual temperature
plot(Ecoqui_bias)

# Add points for all the occurrence points onto the raster
points(Ecoqui_occs, col = "red")

# Randomly sample 10,000 background points from one background extent raster (n=10000 is recommended for MaxEnt by Barbet-Massin et al. 2012 in Methods in Ecology and Evolution)
Ecoqui_bgpoints <- dismo::randomPoints(Ecoqui_bias, n = 10000, prob = T) %>% as.data.frame()
colnames(Ecoqui_bgpoints) <- colnames(Ecoqui_coordinates)
#Ecoqui_bgpoints <- SpatialPoints(Ecoqui_bgpoints)
#projection(Ecoqui_bgpoints) <- proj_wgs84
Ecoqui_bg.z <- cbind(Ecoqui_bgpoints, raster::extract(Ecoqui_envs, Ecoqui_bgpoints)) # extract raster values at points

#plot(Ecoqui_bias, xlim = c(-84,-82), ylim = c(9,11)
plot(Ecoqui_bias, xlim = c(-156.25, -154.8), ylim = c(18.7, 20.5))
points(Ecoqui_bgpoints, pch = 20, cex = 0.2)

# Run ENMeval
e.mx.Ecoqui <- ENMevaluate(occs = Ecoqui_occs.z, bg = Ecoqui_bg.z, 
                           algorithm = 'maxnet', partitions = 'block', 
                           tune.args = list(fc = c("L","LQ","LQH","H"), rm = seq(0.5,5,0.5)), 
                           parallel = TRUE, numCores = 2)
e.mx.Ecoqui
str(e.mx.Ecoqui, max.level=2)

# Results table with summary statistics for cross validation on test data.
eval.results(e.mx.Ecoqui) %>% head()

# Results table with cross validation statistics for each test partition.
eval.results.partitions(e.mx.Ecoqui) %>% head()

# Visualize tuning results
evalplot.stats(e = e.mx.Ecoqui, stats = c("or.mtp", "auc.val"), color = "fc", x.var = "rm", error.bars = FALSE)

# Overall results
res.Ecoqui <- eval.results(e.mx.Ecoqui)

# Select the model with the lowest AICc score
res.Ecoqui[order(res.Ecoqui$delta.AICc),] %>% head()
opt.aicc.Ecoqui <- res.Ecoqui %>% filter(delta.AICc == 0)
opt.aicc.Ecoqui # fc: H, rm: 2

# the sequential criteria
res.Ecoqui[order(res.Ecoqui$or.10p.avg, -res.Ecoqui$auc.val.avg),] %>% head()
opt.seq.Ecoqui <- res.Ecoqui %>% 
  filter(or.10p.avg == min(or.10p.avg)) %>% 
  filter(auc.val.avg == max(auc.val.avg))
opt.seq.Ecoqui # fc: L, rm: 2.5


# Repeat the above steps for Ejohnstonei
Ejohnstonei_envs.files <- list.files(path = "Ejohnstonei_contemp", pattern = ".tif$", full.names=TRUE)
Ejohnstonei_envs <- stack(Ejohnstonei_envs.files)
proj_wgs84 <- crs(Ejohnstonei_envs)

Ejohnstonei_points <- read.csv("Eleutherodactylus_invasion_arcgis/Ejohnstonei_rarefied_points.csv", header = T)
Ejohnstonei_coordinates <- cbind(Ejohnstonei_points$decimalLon, Ejohnstonei_points$decimalLat)
colnames(Ejohnstonei_coordinates ) <- c("Lon", "Lat")
Ejohnstonei_occs <- as.data.frame(Ejohnstonei_coordinates)
Ejohnstonei_occs.z <- cbind(Ejohnstonei_occs, raster::extract(Ejohnstonei_envs, Ejohnstonei_occs))

Ejohnstonei_bias <- raster("Ejohnstonei_bias/Ejohnstonei.asc")
plot(Ejohnstonei_bias)
points(Ejohnstonei_occs, col = "red")

Ejohnstonei_bgpoints <- dismo::randomPoints(Ejohnstonei_bias, n = 10000, prob = T) %>% as.data.frame()
colnames(Ejohnstonei_bgpoints) <- colnames(Ejohnstonei_coordinates)
Ejohnstonei_bg.z <- cbind(Ejohnstonei_bgpoints, raster::extract(Ejohnstonei_envs, Ejohnstonei_bgpoints))

plot(Ejohnstonei_bias, xlim = c(-75,-70), ylim = c(2,7))
points(Ejohnstonei_bgpoints, pch = 20, cex = 0.2)

e.mx.Ejohnstonei <- ENMevaluate(occs = Ejohnstonei_occs.z, bg = Ejohnstonei_bg.z, 
                                algorithm = 'maxnet', partitions = 'block', 
                                tune.args = list(fc = c("L","LQ","LQH","H"), rm = seq(0.5,5,0.5)))
e.mx.Ejohnstonei
str(e.mx.Ejohnstonei, max.level=2)

eval.results(e.mx.Ejohnstonei) %>% head()

eval.results.partitions(e.mx.Ejohnstonei) %>% head()

evalplot.stats(e = e.mx.Ejohnstonei, stats = c("or.mtp", "auc.val"), color = "fc", x.var = "rm", error.bars = FALSE)

res.Ejohnstonei <- eval.results(e.mx.Ejohnstonei)

res.Ejohnstonei[order(res.Ejohnstonei$delta.AICc),] %>% head()
opt.aicc.Ejohnstonei <- res.Ejohnstonei %>% filter(delta.AICc == 0)
opt.aicc.Ejohnstonei # fc: LQ, rm: 1.5

res.Ejohnstonei[order(res.Ejohnstonei$or.10p.avg, -res.Ejohnstonei$auc.val.avg),] %>% head()
opt.seq.Ejohnstonei <- res.Ejohnstonei %>% 
  filter(or.10p.avg == min(or.10p.avg)) %>% 
  filter(auc.val.avg == max(auc.val.avg))
opt.seq.Ejohnstonei # fc: L, rm: 0.5


# Repeat the above steps for Eplanirostris
Eplanirostris_envs.files <- list.files(path = "Eplanirostris_contemp", pattern = ".tif$", full.names=TRUE)
Eplanirostris_envs <- stack(Eplanirostris_envs.files)
proj_wgs84 <- crs(Eplanirostris_envs)

Eplanirostris_points <- read.csv("Eleutherodactylus_invasion_arcgis/Eplanirostris_rarefied_points.csv", header = T)
Eplanirostris_coordinates <- cbind(Eplanirostris_points$decimalLon, Eplanirostris_points$decimalLat)
colnames(Eplanirostris_coordinates ) <- c("Lon", "Lat")
Eplanirostris_occs <- as.data.frame(Eplanirostris_coordinates)
Eplanirostris_occs.z <- cbind(Eplanirostris_occs, raster::extract(Eplanirostris_envs, Eplanirostris_occs))
  
Eplanirostris_bias <- raster("Eplanirostris_bias/eplanirostris.asc")
plot(Eplanirostris_bias)
points(Eplanirostris_occs, col = "red")

Eplanirostris_bgpoints <- dismo::randomPoints(Eplanirostris_bias, n = 10000, prob = T) %>% as.data.frame()
colnames(Eplanirostris_bgpoints) <- colnames(Eplanirostris_coordinates)
Eplanirostris_bg.z <- cbind(Eplanirostris_bgpoints, raster::extract(Eplanirostris_envs, Eplanirostris_bgpoints))
  
plot(Eplanirostris_bias, xlim = c(-97,-92), ylim = c(28,33))
points(Eplanirostris_bgpoints, pch = 20, cex = 0.2)

e.mx.Eplanirostris <- ENMevaluate(occs = Eplanirostris_occs.z, bg = Eplanirostris_bg.z, 
                                  algorithm = 'maxnet', partitions = 'block', 
                                  tune.args = list(fc = c("L","LQ","LQH","H"), rm = seq(0.5,5,0.5)))
e.mx.Eplanirostris
str(e.mx.Eplanirostris, max.level=2)

eval.results(e.mx.Eplanirostris) %>% head()

eval.results.partitions(e.mx.Eplanirostris) %>% head()

evalplot.stats(e = e.mx.Eplanirostris, stats = c("or.mtp", "auc.val"), color = "fc", x.var = "rm", error.bars = FALSE)

res.Eplanirostris <- eval.results(e.mx.Eplanirostris)

res.Eplanirostris[order(res.Eplanirostris$delta.AICc),] %>% head()
opt.aicc.Eplanirostris <- res.Eplanirostris %>% filter(delta.AICc == 0)
opt.aicc.Eplanirostris # fc: H, rm: 0.5

res.Eplanirostris[order(res.Eplanirostris$or.10p.avg, -res.Eplanirostris$auc.val.avg),] %>% head()
opt.seq.Eplanirostris <- res.Eplanirostris %>% 
  filter(or.10p.avg == min(or.10p.avg)) %>% 
  filter(auc.val.avg == max(auc.val.avg))
opt.seq.Eplanirostris # fc: LQ, rm: 5


# Biomod Maxent
Ecoqui_pre <- Ecoqui_occs
Ecoqui_pre$presence <- 1
Ecoqui_abs <- Ecoqui_bgpoints
Ecoqui_abs$presence <- NA
Ecoqui_res <- rbind(Ecoqui_pre, Ecoqui_abs)

# Format the data
Ecoqui_data <- BIOMOD_FormatingData(
  resp.var = Ecoqui_res['presence'],
  resp.xy = Ecoqui_res[, c('Lon', 'Lat')],
  expl.var = Ecoqui_envs,
  resp.name = "E.coqui",
  PA.nb.rep = 1,
  PA.nb.absences = 10000,
  PA.strategy = 'random'
)

plot(Ecoqui_data)
summary(Ecoqui_data)
head(Ecoqui_data@PA.table) 

# Define model options
coqui.MAXENT <- list('_allData_allRun' = list(path_to_maxent.jar = "maxent",
                                              linear = FALSE, quadratic = FALSE, product = FALSE, threshold = FALSE, hinge = TRUE,
                                              betamultiplier= 2, memory_allocated = 2048))
coqui.val <- list(MAXENT.binary.MAXENT.MAXENT = coqui.MAXENT)
Ecoqui_opt <- bm_ModelingOptions(
  data.type = 'binary',
  models = c("MAXENT"),
  strategy = 'user.defined',
  user.val = coqui.val
)

# Run model
Ecoqui_model <- BIOMOD_Modeling(
  bm.format = Ecoqui_data,
  models = c("MAXENT"),
  OPT.user = Ecoqui_opt,
  CV.strategy = 'random',
  CV.nb.rep = 3,
  CV.perc = 0.7,
  var.import = 3,
  metric.eval = c('TSS','ROC'),
  modeling.id = "Ecoqui_sdm"
)

# Get model evaluation scores
Ecoqui_model_scores <- get_evaluations(Ecoqui_model)
dim(Ecoqui_model_scores)
dimnames(Ecoqui_model_scores)
(Ecoqui_model_eval.scor_mean <- aggregate(data = Ecoqui_model_scores, calibration ~ metric.eval, FUN = mean))
  # TSS=0.586, ROC=0.870 (old)
  # TSS=0.435, ROC=0.790 (new)
  
# Plot model evaluation scores
bm_PlotEvalMean(Ecoqui_model)

# Check variable importance
(Ecoqui_model_var_import <- get_variables_importance(Ecoqui_model))

# Make the mean of variable importance by algorithm
(Ecoqui_var.imp_mean <- aggregate(data = Ecoqui_model_var_import, var.imp ~ expl.var, FUN = mean))
  # bio1=0.253, bio2=0.059, bio3=0.129, bio5=0.020, bio14=0.410, bio19=0.051 (old)
  # bio1=0.468, bio2=0.124, bio4=0.001, bop12=0.466, bio15=0.002, bio19=0.011 (new)

# Model response plots
Ecoqui_eval_plot <- 
  bm_PlotResponseCurves(
    bm.out  = Ecoqui_model,
    new.env = get_formal_data(Ecoqui_model,'expl.var'), 
    show.variables= get_formal_data(Ecoqui_model,'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var = 'median',
    )

#Ecoqui_model_chosen <- 
#  get_evaluations(Ecoqui_model) %>% 
#  select(full.name, PA, run, algo, validation, metric.eval) %>% 
#  pivot_wider(names_from = "metric.eval", values_from = validation) %>% 
#  filter(TSS > 0.6, ROC > 0.8) %>% 
#  pull(full.name)

# Run the ensemble model
Ecoqui_ensemble_model <- 
  BIOMOD_EnsembleModeling(
    bm.mod = Ecoqui_model,
    models.chosen = 'all',
    em.by = 'all',
    em.algo = 'EMwmean',
    metric.eval = c('TSS','ROC'),
    metric.select = "TSS",
    var.import = 3,
    nb.cpu = 6
  )


# Assess ensemble models quality ----
(Ecoqui_ensemble_model_scores <- get_evaluations(Ecoqui_ensemble_model))
  # TSS=0.572, ROC=0.867 (old)
  # TSS=0.424, ROC=0.786 (new)

# Check variable importance
(Ecoqui_ensemble_model_var_import <- get_variables_importance(Ecoqui_ensemble_model))
(Ecoqui_ensemble_var.imp_mean <- aggregate(data = Ecoqui_ensemble_model_var_import, var.imp ~ expl.var, FUN = mean))
  # bio1=0.247, bio2=0.057, bio3=0.123, bio5=0.012, bio14=0.406, bio19=0.037 (old)
  # bio1=0.469, bio2=0.118, bio4=0.0001, bio12=0.478, bio15=0.003, bio19=0.012 (new)

# Model response plots
Ecoqui_ensemble_eval_plot <- 
  bm_PlotResponseCurves(
    bm.out  = Ecoqui_ensemble_model,
    new.env = get_formal_data(Ecoqui_ensemble_model,'expl.var'), 
    show.variables= get_formal_data(Ecoqui_ensemble_model,'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var = 'median',
  )

# Present environment projections
Ecoqui_model_proj_present <- 
  BIOMOD_Projection(
    bm.mod = Ecoqui_model,
    models.chosen = "all",
    new.env = Ecoqui_envs,
    proj.name = "Ecoqui_present",
    metric.binary = "all",
    output.format = ".tif",
    do.stack = FALSE
  )

Ecoqui_ensemble_model_proj_present <- 
  BIOMOD_EnsembleForecasting(
    bm.em = Ecoqui_ensemble_model,
    models.chosen = "all",
    bm.proj = Ecoqui_model_proj_present,
    metric.binary = "all",
    output.format = ".tif",
    do.stack = FALSE,
    nb.cpu = 6
  )
plot(Ecoqui_ensemble_model_proj_present)

Ecoqui_ensemble_present.pred <- get_predictions(Ecoqui_ensemble_model_proj_present)
Ecoqui_ensemble_present.pred.bin <- get_predictions(Ecoqui_ensemble_model_proj_present, metric.binary = "TSS") #This is the binary representation
plot(Ecoqui_ensemble_present.pred)
plot(Ecoqui_ensemble_present.pred.bin)
#writeRaster(Ecoqui_ensemble_present.pred, filename = "Ecoqui_ensemble_present.tif", format = "GTiff", overwrite = T)
#writeRaster(Ecoqui_ensemble_present.pred.bin, filename = "Ecoqui_ensemble_present.bin.tif", format = "GTiff", overwrite = T)

for (year in c("2021-2040","2041-2060","2061-2080","2081-2100")){
  for (scen in c("ssp126", "ssp585")) {
    envs_files.name <- paste("Ecoqui_envs",year,scen, sep="_")
    assign(envs_files.name, list.files(path = paste0("Ecoqui_",year,"/",scen), pattern = ".tif$", full.names=TRUE))
    envs_stack.name <- paste0("Ecoqui_",year,"_",scen,".stack")
    assign(envs_stack.name, stack(get(envs_files.name)))
    envs_stack <- get(envs_stack.name)
    #new_env_names <- sub(paste0("wc2\\.1_30s_bioc_HadGEM3\\.GC31\\.LL_",scen,"_",strsplit(year, "-")[[1]][1],".",strsplit(year, "-")[[1]][2],"_"), "bio", names(envs_stack))
    new_env_names <- sub(paste0("wc2\\.1_30s_bioc_HadGEM3\\.GC31\\.LL_",scen,"_",strsplit(year, "-")[[1]][1],".",strsplit(year, "-")[[1]][2],"_"), "Band_", names(envs_stack))
    names(envs_stack) <- new_env_names
    sngl.name <- paste0("Ecoqui_model_proj_",year,"_",scen)
    assign(sngl.name, BIOMOD_Projection(
      bm.mod = Ecoqui_model,
      models.chosen = "all",
      new.env = envs_stack,
      proj.name = paste0("Ecoqui_",year,"_",scen),
      metric.binary = "all",
      output.format = ".tif",
      do.stack = FALSE
    ))
    esmbl.name <- paste0("Ecoqui_ensemble_proj_",year,"_",scen)
    assign(esmbl.name, BIOMOD_EnsembleForecasting(
      bm.em = Ecoqui_ensemble_model,
      models.chosen = "all",
      bm.proj = get(sngl.name),
      metric.binary = "all",
      output.format = ".tif",
      do.stack = FALSE,
      nb.cpu = 6
    ))
    pred_file.name <- paste0(paste("Ecoqui_ensemble",year,scen, sep="_"),".pred")
    assign(pred_file.name, get_predictions(get(esmbl.name)))
    pred.bin_file.name <- paste0(paste("Ecoqui_ensemble",year,scen, sep="_"),".pred.bin")
    assign(pred.bin_file.name, get_predictions(get(esmbl.name), metric.binary = "TSS"))
  }
} 

# Repeat above steps for Ejohnstonei
# Biomod Maxent
Ejohnstonei_pre <- Ejohnstonei_occs
Ejohnstonei_pre$presence <- 1
Ejohnstonei_abs <- Ejohnstonei_bgpoints
Ejohnstonei_abs$presence <- NA
Ejohnstonei_res <- rbind(Ejohnstonei_pre, Ejohnstonei_abs)
Ejohnstonei_res %>% head()
Ejohnstonei_res %>% tail()

# Format the data
Ejohnstonei_data <- BIOMOD_FormatingData(
  resp.var = Ejohnstonei_res['presence'],
  resp.xy = Ejohnstonei_res[, c('Lon', 'Lat')],
  expl.var = Ejohnstonei_envs,
  resp.name = "E.johnstonei",
  PA.nb.rep = 1,
  PA.nb.absences = 10000,
  PA.strategy = 'random'
)

plot(Ejohnstonei_data)
summary(Ejohnstonei_data)
head(Ejohnstonei_data@PA.table) 

# Define model options
johnstonei.MAXENT <- list('_allData_allRun' = list(path_to_maxent.jar = "maxent",
                                                   linear = TRUE, quadratic = TRUE, product = FALSE, threshold = FALSE, hinge = FALSE,
                                                   betamultiplier= 1.5, memory_allocated = 2048))
johnstonei.val <- list(MAXENT.binary.MAXENT.MAXENT = johnstonei.MAXENT)
Ejohnstonei_opt <- bm_ModelingOptions(
  data.type = 'binary',
  models = c("MAXENT"),
  strategy = 'user.defined',
  user.val = johnstonei.val
)
#Ejohnstonei_opt <- BIOMOD_ModelingOptions(
#  MAXENT = list(path_to_maxent.jar = "D:/Research_Data_Backup/maxent/maxent.jar",
#                linear = TRUE, quadratic = TRUE, product = FALSE, threshold = FALSE, hinge = FALSE,
#                betamultiplier= 1.5, memory_allocated = 2048, maximumiterations = 1000)
#) #old-version package command

# Run model
Ejohnstonei_model <- BIOMOD_Modeling(
  bm.format = Ejohnstonei_data,
  models = c("MAXENT"),
  bm.options = Ejohnstonei_opt,
  CV.strategy = 'random',
  CV.nb.rep = 3,
  CV.perc = 0.7,
  var.import = 3,
  metric.eval = c('TSS','ROC'),
  modeling.id = "Ejohnstonei_sdm"
)

# Get model evaluation scores
Ejohnstonei_model_scores <- get_evaluations(Ejohnstonei_model)
dim(Ejohnstonei_model_scores)
dimnames(Ejohnstonei_model_scores)
(Ejohnstonei_model_eval.scor_mean <- aggregate(data = Ejohnstonei_model_scores, calibration ~ metric.eval, FUN = mean))
  # TSS=0.462, ROC=0.770 (old)
  # TSS=0.706, ROC=0.897 (new)

# Plot model evaluation scores
bm_PlotEvalMean(Ejohnstonei_model)

# Check variable importance
(Ejohnstonei_model_var_import <- get_variables_importance(Ejohnstonei_model))

# Make the mean of variable importance by algorithm
(Ejohnstonei_var.imp_mean <- aggregate(data = Ejohnstonei_model_var_import, var.imp ~ expl.var, FUN = mean))
  # bio1=0.481, bio2=0.337, bio3=0.064, bio12=0.514, bio14=0.326, bio18=114 (old)
  # bio1=0.145, bio2=0.121, bio3=0.272, bio12=0.178, bio14=0.549 (new)  

# Model response plots
Ejohnstonei_eval_plot <- 
  bm_PlotResponseCurves(
    bm.out  = Ejohnstonei_model,
    new.env = get_formal_data(Ejohnstonei_model,'expl.var'), 
    show.variables= get_formal_data(Ejohnstonei_model,'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var = 'median',
  )

# Run the ensemble model
Ejohnstonei_ensemble_model <- 
  BIOMOD_EnsembleModeling(
    bm.mod = Ejohnstonei_model,
    models.chosen = 'all',
    em.by = 'all',
    em.algo = 'EMwmean',
    metric.eval = c('TSS','ROC'),
    metric.select = "TSS",
    var.import = 3,
    nb.cpu = 6
  )

# Assess ensemble models quality ----
(Ejohnstonei_ensemble_model_scores <- get_evaluations(Ejohnstonei_ensemble_model))
  # TSS=0.417, ROC=0.773 (old)
  # TSS=0.708, ROC=0.891 (new)

# Check variable importance
(Ejohnstonei_ensemble_model_var_import <- get_variables_importance(Ejohnstonei_ensemble_model))
(Ejohnstonei_ensemble_var.imp_mean <- aggregate(data = Ejohnstonei_ensemble_model_var_import, var.imp ~ expl.var, FUN = mean))
  # bio1=0.524, bio2=0.380, bio3=0.040, bio12=0.536, bio14=0.258, bio18=0.119 (old)
  # bio1=0.101, bio2=0.140, bio3=0.250, bio12=0.123, bio14=0.574 (new)

# Model response plots
Ejohnstonei_ensemble_eval_plot <- 
  bm_PlotResponseCurves(
    bm.out  = Ejohnstonei_ensemble_model,
    new.env = get_formal_data(Ejohnstonei_ensemble_model,'expl.var'), 
    show.variables= get_formal_data(Ejohnstonei_ensemble_model,'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var = 'median',
  )

# Present environment projections
Ejohnstonei_model_proj_present <- 
  BIOMOD_Projection(
    bm.mod = Ejohnstonei_model,
    models.chosen = "all",
    new.env = Ejohnstonei_envs,
    proj.name = "Ejohnstonei_present",
    metric.binary = "all",
    output.format = ".tif",
    do.stack = FALSE
  )

Ejohnstonei_ensemble_model_proj_present <- 
  BIOMOD_EnsembleForecasting(
    bm.em = Ejohnstonei_ensemble_model,
    models.chosen = "all",
    bm.proj = Ejohnstonei_model_proj_present,
    metric.binary = "all",
    output.format = ".tif",
    do.stack = FALSE,
    nb.cpu = 6
  )
plot(Ejohnstonei_ensemble_model_proj_present)

Ejohnstonei_ensemble_present.pred <- get_predictions(Ejohnstonei_ensemble_model_proj_present)
Ejohnstonei_ensemble_present.pred.bin <- get_predictions(Ejohnstonei_ensemble_model_proj_present, metric.binary = "TSS") #This is the binary representation
plot(Ejohnstonei_ensemble_present.pred)
plot(Ejohnstonei_ensemble_present.pred.bin)
#writeRaster(Ejohnstonei_ensemble_present.pred, filename = "Ejohnstonei_ensemble_present.tif", format = "GTiff", overwrite = T)
#writeRaster(Ejohnstonei_ensemble_present.pred.bin, filename = "Ejohnstonei_ensemble_present.bin.tif", format = "GTiff", overwrite = T)

for (year in c("2021-2040","2041-2060","2061-2080","2081-2100")){
  for (scen in c("ssp126", "ssp585")) {
    envs_files.name <- paste("Ejohnstonei_envs",year,scen, sep="_")
    assign(envs_files.name, list.files(path = paste0("Ejohnstonei_",year,"/",scen), pattern = ".tif$", full.names=TRUE))
    envs_stack.name <- paste0("Ejohnstonei_",year,"_",scen,".stack")
    assign(envs_stack.name, stack(get(envs_files.name)))
    envs_stack <- get(envs_stack.name)
    new_env_names <- sub(paste0("wc2\\.1_30s_bioc_HadGEM3\\.GC31\\.LL_",scen,"_",strsplit(year, "-")[[1]][1],".",strsplit(year, "-")[[1]][2],"_"), "Band_", names(envs_stack))
    names(envs_stack) <- new_env_names
    sngl.name <- paste0("Ejohnstonei_model_proj_",year,"_",scen)
    assign(sngl.name, BIOMOD_Projection(
      bm.mod = Ejohnstonei_model,
      models.chosen = "all",
      new.env = envs_stack,
      proj.name = paste0("Ejohnstonei_",year,"_",scen),
      metric.binary = "all",
      output.format = ".tif",
      do.stack = FALSE
    ))
    esmbl.name <- paste0("Ejohnstonei_ensemble_proj_",year,"_",scen)
    assign(esmbl.name, BIOMOD_EnsembleForecasting(
      bm.em = Ejohnstonei_ensemble_model,
      models.chosen = "all",
      bm.proj = get(sngl.name),
      metric.binary = "all",
      output.format = ".tif",
      do.stack = FALSE,
      nb.cpu = 6
    ))
    pred_file.name <- paste0(paste("Ejohnstonei_ensemble",year,scen, sep="_"),".pred")
    assign(pred_file.name, get_predictions(get(esmbl.name)))
    pred.bin_file.name <- paste0(paste("Ejohnstonei_ensemble",year,scen, sep="_"),".pred.bin")
    assign(pred.bin_file.name, get_predictions(get(esmbl.name), metric.binary = "TSS"))
  }
} 

# Repeat above steps for Eplanirostris
# Biomod Maxent
Eplanirostris_pre <- Eplanirostris_occs
Eplanirostris_pre$presence <- 1
Eplanirostris_abs <- Eplanirostris_bgpoints
Eplanirostris_abs$presence <- NA
Eplanirostris_res <- rbind(Eplanirostris_pre, Eplanirostris_abs)
Eplanirostris_res %>% head()
Eplanirostris_res %>% tail()

# Format the data
Eplanirostris_data <- BIOMOD_FormatingData(
  resp.var = Eplanirostris_res['presence'],
  resp.xy = Eplanirostris_res[, c('Lon', 'Lat')],
  expl.var = Eplanirostris_envs,
  resp.name = "E.planirostris",
  PA.nb.rep = 1,
  PA.nb.absences = 10000,
  PA.strategy = 'random'
)

plot(Eplanirostris_data)
summary(Eplanirostris_data)
head(Eplanirostris_data@PA.table) 

# Define model options
Eplanirostris_opt.MAXENT <- list('_allData_allRun' = list(path_to_maxent.jar = "maxent",
                                                         linear = FALSE, quadratic = FALSE, product = FALSE, threshold = FALSE, hinge = TRUE,
                                                         betamultiplier= 0.5, memory_allocated = 2048))
Eplanirostris_opt.val <- list(MAXENT.binary.MAXENT.MAXENT = Eplanirostris_opt.MAXENT)
Eplanirostris_opt <- bm_ModelingOptions(
  data.type = 'binary',
  models = c("MAXENT"),
  strategy = 'user.defined',
  user.val = Eplanirostris_opt.val
)
#Eplanirostris_opt <- BIOMOD_ModelingOptions(
#  MAXENT = list(path_to_maxent.jar = "D:/Research_Data_Backup/maxent/maxent.jar",
#                linear = FALSE, quadratic = FALSE, product = FALSE, threshold = FALSE, hinge = TRUE,
#                betamultiplier= 0.5, memory_allocated = 2048, maximumiterations = 1000)
#) # old-version package command

# Run model
Eplanirostris_model <- BIOMOD_Modeling(
  bm.format = Eplanirostris_data,
  models = c("MAXENT"),
  bm.options = Eplanirostris_opt,
  CV.strategy = 'random',
  CV.nb.rep = 3,
  CV.perc = 0.7,
  var.import = 3,
  metric.eval = c('TSS','ROC'),
  modeling.id = "Eplanirostris_sdm"
)

# Get model evaluation scores
Eplanirostris_model_scores <- get_evaluations(Eplanirostris_model)
dim(Eplanirostris_model_scores)
dimnames(Eplanirostris_model_scores)
(Eplanirostris_model_eval.scor_mean <- aggregate(data = Eplanirostris_model_scores, calibration ~ metric.eval, FUN = mean))
  # TSS=0.442, ROC=0.784 (old)
  # TSS=0.536, ROC=0.845 (new)

# Plot model evaluation scores
bm_PlotEvalMean(Eplanirostris_model)

# Check variable importance
(Eplanirostris_model_var_import <- get_variables_importance(Eplanirostris_model))

# Make the mean of variable importance by algorithm
(Eplanirostris_var.imp_mean <- aggregate(data = Eplanirostris_model_var_import, var.imp ~ expl.var, FUN = mean))
  # bio1=0.381, bio2=0.043, bio3=0.159, bio5=0.137, bio8=0.094, bio12=0.108, bio14=0.107, bio18=118 (old)
  # bio1=0.237, bio2=0.029, bio3=0.063, bio5=0.205, bio8=0.471, bio12=0.150, bio14=0.068, bio15=0.067, bio18=0.087 (new)

# Model response plots
Eplanirostris_eval_plot <- 
  bm_PlotResponseCurves(
    bm.out  = Eplanirostris_model,
    new.env = get_formal_data(Eplanirostris_model,'expl.var'), 
    show.variables= get_formal_data(Eplanirostris_model,'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var = 'median',
  )

# Run the ensemble model
Eplanirostris_ensemble_model <- 
  BIOMOD_EnsembleModeling(
    bm.mod = Eplanirostris_model,
    models.chosen = 'all',
    em.by = 'all',
    em.algo = 'EMwmean',
    metric.eval = c('TSS','ROC'),
    metric.select = "TSS",
    var.import = 3,
    nb.cpu = 6
  )

# Assess ensemble models quality ----
(Eplanirostris_ensemble_model_scores <- get_evaluations(Eplanirostris_ensemble_model))
  # TSS=0.416, ROC=0.776 (old)
  # TSS=0.532, ROC=0.837 (new)

# Check variable importance
(Eplanirostris_ensemble_model_var_import <- get_variables_importance(Eplanirostris_ensemble_model))

# Make the mean of variable importance by algorithm
(Eplanirostris_ensemble_var.imp_mean <- aggregate(data = Eplanirostris_ensemble_model_var_import, var.imp ~ expl.var, FUN = mean))
  # bio1=0.375, bio2=0.035, bio3=0.152, bio5=0.133, bio8=0.096, bio12=0.106, bio14=0.104, bio18=114 (old)
  # bio1=0.238, bio2=0.029, bio3=0.058, bio5=0.201, bio8=0.462, bio12=0.140, bio14=0.061, bio15=0.061, bio18=0.077 (new)

# Model response plots
Eplanirostris_ensemble_eval_plot <- 
  bm_PlotResponseCurves(
    bm.out  = Eplanirostris_ensemble_model,
    new.env = get_formal_data(Eplanirostris_ensemble_model,'expl.var'), 
    show.variables= get_formal_data(Eplanirostris_ensemble_model,'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var = 'median',
  )

# Present environment projections
Eplanirostris_model_proj_present <- 
  BIOMOD_Projection(
    bm.mod = Eplanirostris_model,
    models.chosen = "all",
    new.env = Eplanirostris_envs,
    proj.name = "Eplanirostris_present",
    metric.binary = "all",
    output.format = ".tif",
    do.stack = FALSE
  )

Eplanirostris_ensemble_model_proj_present <- 
  BIOMOD_EnsembleForecasting(
    bm.em = Eplanirostris_ensemble_model,
    models.chosen = "all",
    bm.proj = Eplanirostris_model_proj_present,
    metric.binary = "all",
    output.format = ".tif",
    do.stack = FALSE,
    nb.cpu = 6
  )
plot(Eplanirostris_ensemble_model_proj_present)

Eplanirostris_ensemble_present.pred <- get_predictions(Eplanirostris_ensemble_model_proj_present)
Eplanirostris_ensemble_present.pred.bin <- get_predictions(Eplanirostris_ensemble_model_proj_present, metric.binary = "TSS") #This is the binary representation
plot(Eplanirostris_ensemble_present.pred)
plot(Eplanirostris_ensemble_present.pred.bin)
#writeRaster(Eplanirostris_ensemble_present.pred, filename = "Eplanirostris_ensemble_present.tif", format = "GTiff", overwrite = T)
#writeRaster(Eplanirostris_ensemble_present.pred.bin, filename = "Eplanirostris_ensemble_present.bin.tif", format = "GTiff", overwrite = T)

for (year in c("2021-2040","2041-2060","2061-2080","2081-2100")){
  for (scen in c("ssp126", "ssp585")) {
    envs_files.name <- paste("Eplanirostris_envs",year,scen, sep="_")
    assign(envs_files.name, list.files(path = paste0("Eplanirostris_",year,"/",scen), pattern = ".tif$", full.names=TRUE))
    envs_stack.name <- paste0("Eplanirostris_",year,"_",scen,".stack")
    assign(envs_stack.name, stack(get(envs_files.name)))
    envs_stack <- get(envs_stack.name)
    new_env_names <- sub(paste0("wc2\\.1_30s_bioc_HadGEM3\\.GC31\\.LL_",scen,"_",strsplit(year, "-")[[1]][1],".",strsplit(year, "-")[[1]][2],"_"), "Band_", names(envs_stack))
    names(envs_stack) <- new_env_names
    sngl.name <- paste0("Eplanirostris_model_proj_",year,"_",scen)
    assign(sngl.name, BIOMOD_Projection(
      bm.mod = Eplanirostris_model,
      models.chosen = "all",
      new.env = envs_stack,
      proj.name = paste0("Eplanirostris_",year,"_",scen),
      metric.binary = "all",
      output.format = ".tif",
      do.stack = FALSE
    ))
    esmbl.name <- paste0("Eplanirostris_ensemble_proj_",year,"_",scen)
    assign(esmbl.name, BIOMOD_EnsembleForecasting(
      bm.em = Eplanirostris_ensemble_model,
      models.chosen = "all",
      bm.proj = get(sngl.name),
      metric.binary = "all",
      output.format = ".tif",
      do.stack = FALSE,
      nb.cpu = 6
    ))
    pred_file.name <- paste0(paste("Eplanirostris_ensemble",year,scen, sep="_"),".pred")
    assign(pred_file.name, get_predictions(get(esmbl.name)))
    pred.bin_file.name <- paste0(paste("Eplanirostris_ensemble",year,scen, sep="_"),".pred.bin")
    assign(pred.bin_file.name, get_predictions(get(esmbl.name), metric.binary = "TSS"))
  }
} 

# Plotting E. coqui
combined_land <- st_read("Eleutherodactylus_invasion_arcgis/combined_land.shp")

custom_palette <- colorRampPalette(c("#FFD966", "#00A600"))
zlim <- c(0, 1)

#Hawaii_window <- extent(-162.5, -154.5, 18.5, 22.5)
Hawaii_window <- extent(-156.25, -154.8, 18.7, 20.5)
#CostaCal_window <- extent(-122, -82, 6, 36)
Puerto_window <- extent(-68, -64, 17, 19)

Ecoqui_ensemble_present.pred.bin <- raster("./E.coqui/proj_Ecoqui_present/individual_projections/E.coqui_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
combined_land_rast <- rasterize(combined_land[6], Ecoqui_ensemble_present.pred.bin)
#raster_outline <- rasterToPolygons(combined_land_rast, dissolve = TRUE)

zoom(Ecoqui_ensemble_present.pred.bin, Hawaii_window, legend=FALSE)
plot(combined_land_rast, col = "black", legend=FALSE, asp=1, add=TRUE)
plot(Ecoqui_ensemble_present.pred.bin, add=TRUE, legend=FALSE)

tiff("./E.coqui/Hawaii_present_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(Ecoqui_ensemble_present.pred.bin, ext=Hawaii_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), zlim = zlim, legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Hawaii_window, add = TRUE, border = "black")
dev.off()

#zoom(Ecoqui_ensemble_present.pred.bin, CostaCal_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), zlim = zlim, legend=FALSE)
zoom(Ecoqui_ensemble_present.pred.bin, Puerto_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), zlim = zlim, legend=FALSE)
plot(combined_land_rast, col = "black", legend=FALSE, asp=1, add=TRUE)
plot(Ecoqui_ensemble_present.pred.bin, add=TRUE, legend=FALSE)

tiff("./E.coqui/Puerto_present_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Puerto_window, asp=1)
plot(Ecoqui_ensemble_present.pred.bin, ext=Puerto_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Puerto_window, add = TRUE, border = "black")
dev.off()

Ecoqui_ensemble_2021.2040_ssp126.pred.bin <- raster("./E.coqui/proj_Ecoqui_2021-2040_ssp126/individual_projections/E.coqui_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
tiff("./E.coqui/Hawaii_2021.2040_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(Ecoqui_ensemble_2021.2040_ssp126.pred.bin, ext=Hawaii_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Hawaii_window, add = TRUE, border = "black", lwd = 2)
dev.off()

tiff("./E.coqui/Puerto_2021.2040_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Puerto_window, asp=1)
plot(Ecoqui_ensemble_2021.2040_ssp126.pred.bin, ext=Puerto_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Puerto_window, add = TRUE, border = "black", lwd = 2)
dev.off()

#zoom(Ecoqui_ensemble_2021.2040_ssp126.pred.bin, CostaCal_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), zlim = zlim, legend=FALSE)
#plot(combined_land_rast, col = "#E6E402", legend=FALSE, asp=1, add=TRUE)
#plot(Ecoqui_ensemble_2021.2040_ssp126.pred.bin, add=TRUE, legend=FALSE)

Ecoqui_ensemble_2041.2060_ssp126.pred.bin <- raster("./E.coqui/proj_Ecoqui_2041-2060_ssp126/individual_projections/E.coqui_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
tiff("./E.coqui/Hawaii_2041.2060_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(Ecoqui_ensemble_2041.2060_ssp126.pred.bin, ext=Hawaii_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Hawaii_window, add = TRUE, border = "black", lwd = 2)
dev.off()

tiff("./E.coqui/Puerto_2041.2060_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Puerto_window, asp=1)
plot(Ecoqui_ensemble_2041.2060_ssp126.pred.bin, ext=Puerto_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Puerto_window, add = TRUE, border = "black", lwd = 2)
dev.off()

#zoom(Ecoqui_ensemble_2041.2060_ssp126.pred.bin, CostaCal_window, legend=FALSE)
#plot(combined_land_rast, col = "black", legend=FALSE, asp=1, add=TRUE)
#plot(Ecoqui_ensemble_2041.2060_ssp126.pred.bin, add=TRUE, legend=FALSE)

Ecoqui_ensemble_2061.2080_ssp126.pred.bin <- raster("./E.coqui/proj_Ecoqui_2061-2080_ssp126/individual_projections/E.coqui_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
tiff("./E.coqui/Hawaii_2061.2080_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(Ecoqui_ensemble_2061.2080_ssp126.pred.bin, ext=Hawaii_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Hawaii_window, add = TRUE, border = "black", lwd = 2)
dev.off()

tiff("./E.coqui/Puerto_2061.2080_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Puerto_window, asp=1)
plot(Ecoqui_ensemble_2061.2080_ssp126.pred.bin, ext=Puerto_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Puerto_window, add = TRUE, border = "black", lwd = 2)
dev.off()

#zoom(Ecoqui_ensemble_2061.2080_ssp126.pred.bin, CostaCal_window, legend=FALSE)
#plot(combined_land_rast, col = "#E6E402", legend=FALSE, asp=1, add=TRUE)
#plot(Ecoqui_ensemble_2061.2080_ssp126.pred.bin, add=TRUE, legend=FALSE)

Ecoqui_ensemble_2081.2100_ssp126.pred.bin <- raster("./E.coqui/proj_Ecoqui_2081-2100_ssp126/individual_projections/E.coqui_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
tiff("./E.coqui/Hawaii_2081.2100_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(Ecoqui_ensemble_2081.2100_ssp126.pred.bin, ext=Hawaii_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Hawaii_window, add = TRUE, border = "black", lwd = 2)
dev.off()

tiff("./E.coqui/Puerto_2081.2100_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Puerto_window, asp=1)
plot(Ecoqui_ensemble_2081.2100_ssp126.pred.bin, ext=Puerto_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Puerto_window, add = TRUE, border = "black", lwd = 2)
dev.off()

#zoom(Ecoqui_ensemble_2081.2100_ssp126.pred.bin, CostaCal_window, legend=FALSE)
#plot(combined_land_rast, col = "black", legend=FALSE, asp=1, add=TRUE)
#plot(Ecoqui_ensemble_2081.2100_ssp126.pred.bin, add=TRUE, legend=FALSE)


Ecoqui_ensemble_2021.2040_ssp585.pred.bin <- raster("./E.coqui/proj_Ecoqui_2021-2040_ssp585/individual_projections/E.coqui_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
tiff("./E.coqui/Hawaii_2021.2040_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(Ecoqui_ensemble_2021.2040_ssp585.pred.bin, ext=Hawaii_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Hawaii_window, add = TRUE, border = "black", lwd = 2)
dev.off()

tiff("./E.coqui/Puerto_2021.2040_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Puerto_window, asp=1)
plot(Ecoqui_ensemble_2021.2040_ssp585.pred.bin, ext=Puerto_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Puerto_window, add = TRUE, border = "black", lwd = 2)
dev.off()

#zoom(Ecoqui_ensemble_2021.2040_ssp585.pred.bin, CostaCal_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), zlim = zlim, legend=FALSE)
#plot(combined_land_rast, col = "black", legend=FALSE, asp=1, add=TRUE)
#plot(Ecoqui_ensemble_2021.2040_ssp585.pred.bin, add=TRUE, legend=FALSE)

Ecoqui_ensemble_2041.2060_ssp585.pred.bin <- raster("./E.coqui/proj_Ecoqui_2041-2060_ssp585/individual_projections/E.coqui_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
tiff("./E.coqui/Hawaii_2041.2060_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(Ecoqui_ensemble_2041.2060_ssp585.pred.bin, ext=Hawaii_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Hawaii_window, add = TRUE, border = "black", lwd = 2)
dev.off()

tiff("./E.coqui/Puerto_2041.2060_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Puerto_window, asp=1)
plot(Ecoqui_ensemble_2041.2060_ssp585.pred.bin, ext=Puerto_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Puerto_window, add = TRUE, border = "black", lwd = 2)
dev.off()

#zoom(Ecoqui_ensemble_2041.2060_ssp585.pred.bin, CostaCal_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), zlim = zlim, legend=FALSE)
#plot(combined_land_rast, col = "black", legend=FALSE, asp=1, add=TRUE)
#plot(Ecoqui_ensemble_2041.2060_ssp585.pred.bin, add=TRUE, legend=FALSE)

Ecoqui_ensemble_2061.2080_ssp585.pred.bin <- raster("./E.coqui/proj_Ecoqui_2061-2080_ssp585/individual_projections/E.coqui_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
tiff("./E.coqui/Hawaii_2061.2080_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(Ecoqui_ensemble_2061.2080_ssp585.pred.bin, ext=Hawaii_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Hawaii_window, add = TRUE, border = "black", lwd = 2)
dev.off()

tiff("./E.coqui/Puerto_2061.2080_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Puerto_window, asp=1)
plot(Ecoqui_ensemble_2061.2080_ssp585.pred.bin, ext=Puerto_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Puerto_window, add = TRUE, border = "black", lwd = 2)
dev.off()

#zoom(Ecoqui_ensemble_2061.2080_ssp585.pred.bin, CostaCal_window, legend=FALSE)
#plot(combined_land_rast, col = "#E6E402", legend=FALSE, asp=1, add=TRUE)
#plot(Ecoqui_ensemble_2061.2080_ssp585.pred.bin, add=TRUE, legend=FALSE)

Ecoqui_ensemble_2081.2100_ssp585.pred.bin <- raster("./E.coqui/proj_Ecoqui_2081-2100_ssp585/individual_projections/E.coqui_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
tiff("./E.coqui/Hawaii_2081.2100_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(Ecoqui_ensemble_2081.2100_ssp585.pred.bin, ext=Hawaii_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Hawaii_window, add = TRUE, border = "black", lwd = 2)
dev.off()

tiff("./E.coqui/Puerto_2081.2100_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Puerto_window, asp=1)
plot(Ecoqui_ensemble_2081.2100_ssp585.pred.bin, ext=Puerto_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Puerto_window, add = TRUE, border = "black", lwd = 2)
dev.off()

#zoom(Ecoqui_ensemble_2081.2100_ssp585.pred.bin, CostaCal_window, legend=FALSE)
#plot(combined_land_rast, col = "#E6E402", legend=FALSE, asp=1, add=TRUE)
#plot(Ecoqui_ensemble_2081.2100_ssp585.pred.bin, add=TRUE, legend=FALSE)


# Plotting E. johnstonei
johnstonei_window <- extent(-80, -45, -25, 33)

Ejohnstonei_ensemble_present.pred.bin <- raster("./E.johnstonei/proj_Ejohnstonei_present/individual_projections/E.johnstonei_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
combined_land_rast <- rasterize(combined_land[6], Ejohnstonei_ensemble_present.pred.bin)
#raster_outline <- rasterToPolygons(combined_land_rast, dissolve = TRUE)

tiff("./E.johnstonei/Ejohnstonei_present_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=johnstonei_window, asp=1)
plot(Ejohnstonei_ensemble_present.pred.bin, ext=johnstonei_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=johnstonei_window, add = TRUE, border = "black", lwd = 2)
dev.off()

#zoom(Ejohnstonei_ensemble_present.pred.bin, johnstonei_window, legend=FALSE)
#plot(combined_land_rast, col = "#E6E402", legend=FALSE, asp=1, add=TRUE)
#plot(Ejohnstonei_ensemble_present.pred.bin, add=TRUE, legend=FALSE)

Ejohnstonei_ensemble_2021.2040_ssp126.pred.bin <- raster("./E.johnstonei/proj_Ejohnstonei_2021-2040_ssp126/individual_projections/E.johnstonei_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
tiff("./E.johnstonei/Ejohnstonei_2021.2040_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=johnstonei_window, asp=1)
plot(Ejohnstonei_ensemble_2021.2040_ssp126.pred.bin, ext=johnstonei_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=johnstonei_window, add = TRUE, border = "black", lwd = 2)
dev.off()

Ejohnstonei_ensemble_2041.2060_ssp126.pred.bin <- raster("./E.johnstonei/proj_Ejohnstonei_2041-2060_ssp126/individual_projections/E.johnstonei_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
tiff("./E.johnstonei/Ejohnstonei_2041.2060_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=johnstonei_window, asp=1)
plot(Ejohnstonei_ensemble_2041.2060_ssp126.pred.bin, ext=johnstonei_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=johnstonei_window, add = TRUE, border = "black", lwd = 2)
dev.off()

Ejohnstonei_ensemble_2061.2080_ssp126.pred.bin <- raster("./E.johnstonei/proj_Ejohnstonei_2061-2080_ssp126/individual_projections/E.johnstonei_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
tiff("./E.johnstonei/Ejohnstonei_2061.2080_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=johnstonei_window, asp=1)
plot(Ejohnstonei_ensemble_2061.2080_ssp126.pred.bin, ext=johnstonei_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=johnstonei_window, add = TRUE, border = "black", lwd = 2)
dev.off()

Ejohnstonei_ensemble_2081.2100_ssp126.pred.bin <- raster("./E.johnstonei/proj_Ejohnstonei_2081-2100_ssp126/individual_projections/E.johnstonei_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
tiff("./E.johnstonei/Ejohnstonei_2081.2100_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=johnstonei_window, asp=1)
plot(Ejohnstonei_ensemble_2081.2100_ssp126.pred.bin, ext=johnstonei_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=johnstonei_window, add = TRUE, border = "black", lwd = 2)
dev.off()


Ejohnstonei_ensemble_2021.2040_ssp585.pred.bin <- raster("./E.johnstonei/proj_Ejohnstonei_2021-2040_ssp585/individual_projections/E.johnstonei_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
tiff("./E.johnstonei/Ejohnstonei_2021.2040_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=johnstonei_window, asp=1)
plot(Ejohnstonei_ensemble_2021.2040_ssp585.pred.bin, ext=johnstonei_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=johnstonei_window, add = TRUE, border = "black", lwd = 2)
dev.off()

Ejohnstonei_ensemble_2041.2060_ssp585.pred.bin <- raster("./E.johnstonei/proj_Ejohnstonei_2041-2060_ssp585/individual_projections/E.johnstonei_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
tiff("./E.johnstonei/Ejohnstonei_2041.2060_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=johnstonei_window, asp=1)
plot(Ejohnstonei_ensemble_2041.2060_ssp585.pred.bin, ext=johnstonei_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=johnstonei_window, add = TRUE, border = "black", lwd = 2)
dev.off()

Ejohnstonei_ensemble_2061.2080_ssp585.pred.bin <- raster("./E.johnstonei/proj_Ejohnstonei_2061-2080_ssp585/individual_projections/E.johnstonei_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
tiff("./E.johnstonei/Ejohnstonei_2061.2080_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=johnstonei_window, asp=1)
plot(Ejohnstonei_ensemble_2061.2080_ssp585.pred.bin, ext=johnstonei_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=johnstonei_window, add = TRUE, border = "black", lwd = 2)
dev.off()

Ejohnstonei_ensemble_2081.2100_ssp585.pred.bin <- raster("./E.johnstonei/proj_Ejohnstonei_2081-2100_ssp585/individual_projections/E.johnstonei_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
tiff("./E.johnstonei/Ejohnstonei_2081.2100_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=johnstonei_window, asp=1)
plot(Ejohnstonei_ensemble_2081.2100_ssp585.pred.bin, ext=johnstonei_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=johnstonei_window, add = TRUE, border = "black", lwd = 2)
dev.off()


# Plotting E. planirostris
#Hawaii_window <- extent(-156.25, -154.8, 18.7, 20.5)
Hawaii_window <- extent(-162.5, -154.5, 18.5, 22.5)
US_window <- extent(-100, -70, 5, 45)
Asia_window <- extent(110, 130, 5, 25)

Eplanirostris_ensemble_present.pred.bin <- raster("./E.planirostris/proj_Eplanirostris_present/individual_projections/E.planirostris_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
combined_land_rast <- rasterize(combined_land[6], Eplanirostris_ensemble_present.pred.bin) # took too long -> use ArcGIS Pro
#combined_land_rast <- raster("Eleutherodactylus_invasion_arcgis/Epla_combined_land_rast.tif")
#raster_outline <- rasterToPolygons(combined_land_rast, dissolve = TRUE) # took too long -> use ArcGIS Pro
#raster_outline <- terra::vect("Eleutherodactylus_invasion_arcgis/Epla_combined_land_poly.shp")
#raster_outline_US <- crop(raster_outline, US_window)
#raster_outline_US <- project(raster_outline_US, crs(combined_land_rast))

tiff("./E.planirostris/Hawaii_present_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(Eplanirostris_ensemble_present.pred.bin, ext=Hawaii_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Hawaii_window, add = TRUE, border = "black", lwd = 2, col = NA)
dev.off()

#zoom(Eplanirostris_ensemble_present.pred.bin, US_window, legend=FALSE)
#plot(combined_land_rast, col = "#E6E402", legend=FALSE, asp=1, add=TRUE)
#plot(Eplanirostris_ensemble_present.pred.bin, add=TRUE, legend=FALSE)

tiff("./E.planirostris/US_present_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=US_window, asp=1)
plot(Eplanirostris_ensemble_present.pred.bin, ext=US_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline_US, border = "black", lwd = 0.5, add = TRUE, col = NA)
dev.off()

#zoom(Eplanirostris_ensemble_present.pred.bin, ext=Asia_window, legend=FALSE)
#plot(combined_land_rast, col = "#E6E402", legend=FALSE, asp=1, add=TRUE)
#plot(Eplanirostris_ensemble_present.pred.bin, add=TRUE, legend=FALSE)

tiff("./E.planirostris/Asia_present_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Asia_window, asp=1)
plot(Eplanirostris_ensemble_present.pred.bin, ext=Asia_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Asia_window, add = TRUE, border = "black", lwd = 2, col = NA)
dev.off()

Eplanirostris_ensemble_2021.2040_ssp126.pred.bin <- raster("./E.planirostris/proj_Eplanirostris_2021-2040_ssp126/individual_projections/E.planirostris_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
tiff("./E.planirostris/Hawaii_2021.2040_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(Eplanirostris_ensemble_2021.2040_ssp126.pred.bin, ext=Hawaii_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Hawaii_window, add = TRUE, border = "black", lwd = 2, col = NA)
dev.off()

tiff("./E.planirostris/US_2021.2040_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=US_window, asp=1)
plot(Eplanirostris_ensemble_2021.2040_ssp126.pred.bin, ext=US_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=US_window, add = TRUE, border = "black", lwd = 2, col = NA)
dev.off()

tiff("./E.planirostris/Asia_2021.2040_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Asia_window, asp=1)
plot(Eplanirostris_ensemble_2021.2040_ssp126.pred.bin, ext=Asia_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Asia_window, add = TRUE, border = "black", lwd = 2, col = NA)
dev.off()

Eplanirostris_ensemble_2041.2060_ssp126.pred.bin <- raster("./E.planirostris/proj_Eplanirostris_2041-2060_ssp126/individual_projections/E.planirostris_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
tiff("./E.planirostris/Hawaii_2041.2060_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(Eplanirostris_ensemble_2041.2060_ssp126.pred.bin, ext=Hawaii_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Hawaii_window, add = TRUE, border = "black", lwd = 2, col = NA)
dev.off()

tiff("./E.planirostris/US_2041.2060_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=US_window, asp=1)
plot(Eplanirostris_ensemble_2041.2060_ssp126.pred.bin, ext=US_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=US_window, add = TRUE, border = "black", lwd = 2, col = NA)
dev.off()

tiff("./E.planirostris/Asia_2041.2060_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Asia_window, asp=1)
plot(Eplanirostris_ensemble_2041.2060_ssp126.pred.bin, ext=Asia_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Asia_window, add = TRUE, border = "black", lwd = 2, col = NA)
dev.off()

Eplanirostris_ensemble_2061.2080_ssp126.pred.bin <- raster("./E.planirostris/proj_Eplanirostris_2061-2080_ssp126/individual_projections/E.planirostris_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
tiff("./E.planirostris/Hawaii_2061.2080_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(Eplanirostris_ensemble_2061.2080_ssp126.pred.bin, ext=Hawaii_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Hawaii_window, add = TRUE, border = "black", lwd = 2, col = NA)
dev.off()

tiff("./E.planirostris/US_2061.2080_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=US_window, asp=1)
plot(Eplanirostris_ensemble_2061.2080_ssp126.pred.bin, ext=US_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=US_window, add = TRUE, border = "black", lwd = 2, col = NA)
dev.off()

tiff("./E.planirostris/Asia_2061.2080_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Asia_window, asp=1)
plot(Eplanirostris_ensemble_2061.2080_ssp126.pred.bin, ext=Asia_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Asia_window, add = TRUE, border = "black", lwd = 2, col = NA)
dev.off()

Eplanirostris_ensemble_2081.2100_ssp126.pred.bin <- raster("./E.planirostris/proj_Eplanirostris_2081-2100_ssp126/individual_projections/E.planirostris_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
tiff("./E.planirostris/Hawaii_2081.2100_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(Eplanirostris_ensemble_2081.2100_ssp126.pred.bin, ext=Hawaii_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Hawaii_window, add = TRUE, border = "black", lwd = 2, col = NA)
dev.off()

tiff("./E.planirostris/US_2081.2100_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=US_window, asp=1)
plot(Eplanirostris_ensemble_2081.2100_ssp126.pred.bin, ext=US_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=US_window, add = TRUE, border = "black", lwd = 2, col = NA)
dev.off()

tiff("./E.planirostris/Asia_2081.2100_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Asia_window, asp=1)
plot(Eplanirostris_ensemble_2081.2100_ssp126.pred.bin, ext=Asia_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Asia_window, add = TRUE, border = "black", lwd = 2, col = NA)
dev.off()


Eplanirostris_ensemble_2021.2040_ssp585.pred.bin <- raster("./E.planirostris/proj_Eplanirostris_2021-2040_ssp585/individual_projections/E.planirostris_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
tiff("./E.planirostris/Hawaii_2021.2040_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(Eplanirostris_ensemble_2021.2040_ssp585.pred.bin, ext=Hawaii_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Hawaii_window, add = TRUE, border = "black", lwd = 2, col = NA)
dev.off()

tiff("./E.planirostris/US_2021.2040_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=US_window, asp=1)
plot(Eplanirostris_ensemble_2021.2040_ssp585.pred.bin, ext=US_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=US_window, add = TRUE, border = "black", lwd = 2, col = NA)
dev.off()

tiff("./E.planirostris/Asia_2021.2040_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Asia_window, asp=1)
plot(Eplanirostris_ensemble_2021.2040_ssp585.pred.bin, ext=Asia_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Asia_window, add = TRUE, border = "black", lwd = 2, col = NA)
dev.off()

Eplanirostris_ensemble_2041.2060_ssp585.pred.bin <- raster("./E.planirostris/proj_Eplanirostris_2041-2060_ssp585/individual_projections/E.planirostris_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
tiff("./E.planirostris/Hawaii_2041.2060_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(Eplanirostris_ensemble_2041.2060_ssp585.pred.bin, ext=Hawaii_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Hawaii_window, add = TRUE, border = "black", lwd = 2, col = NA)
dev.off()

tiff("./E.planirostris/US_2041.2060_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=US_window, asp=1)
plot(Eplanirostris_ensemble_2041.2060_ssp585.pred.bin, ext=US_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=US_window, add = TRUE, border = "black", lwd = 2, col = NA)
dev.off()

tiff("./E.planirostris/Asia_2041.2060_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Asia_window, asp=1)
plot(Eplanirostris_ensemble_2041.2060_ssp585.pred.bin, ext=Asia_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Asia_window, add = TRUE, border = "black", lwd = 2, col = NA)
dev.off()

Eplanirostris_ensemble_2061.2080_ssp585.pred.bin <- raster("./E.planirostris/proj_Eplanirostris_2061-2080_ssp585/individual_projections/E.planirostris_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
tiff("./E.planirostris/Hawaii_2061.2080_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(Eplanirostris_ensemble_2061.2080_ssp585.pred.bin, ext=Hawaii_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Hawaii_window, add = TRUE, border = "black", lwd = 2, col = NA)
dev.off()

tiff("./E.planirostris/US_2061.2080_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=US_window, asp=1)
plot(Eplanirostris_ensemble_2061.2080_ssp585.pred.bin, ext=US_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=US_window, add = TRUE, border = "black", lwd = 2, col = NA)
dev.off()

tiff("./E.planirostris/Asia_2061.2080_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Asia_window, asp=1)
plot(Eplanirostris_ensemble_2061.2080_ssp585.pred.bin, ext=Asia_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Asia_window, add = TRUE, border = "black", lwd = 2, col = NA)
dev.off()

Eplanirostris_ensemble_2081.2100_ssp585.pred.bin <- raster("./E.planirostris/proj_Eplanirostris_2081-2100_ssp585/individual_projections/E.planirostris_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
tiff("./E.planirostris/Hawaii_2081.2100_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(Eplanirostris_ensemble_2081.2100_ssp585.pred.bin, ext=Hawaii_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Hawaii_window, add = TRUE, border = "black", lwd = 2, col = NA)
dev.off()

tiff("./E.planirostris/US_2081.2100_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=US_window, asp=1)
plot(Eplanirostris_ensemble_2081.2100_ssp585.pred.bin, ext=US_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=US_window, add = TRUE, border = "black", lwd = 2, col = NA)
dev.off()

tiff("./E.planirostris/Asia_2081.2100_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Asia_window, asp=1)
plot(Eplanirostris_ensemble_2081.2100_ssp585.pred.bin, ext=Asia_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Asia_window, add = TRUE, border = "black", lwd = 2, col = NA)
dev.off()
