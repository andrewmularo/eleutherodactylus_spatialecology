# load geos, gdal, proj, R first
#export LD_LIBRARY_PATH=/apps/spack/negishi/apps/anaconda/2024.02-py311-gcc-8.5.0-lr57z2f/lib:$LD_LIBRARY_PATH
# Run in Negishi
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

#setwd("D:/Research_Data_Backup/Eleutherodactylus_invasion")
setwd("/scratch/negishi/jeon96/Eleutherodactylus_invasion")

# Read data
Ecoqui_envs.files <- list.files(path = "coqui_contemp", pattern = "^CS[0-9]+\\.tif$", full.names=TRUE)
Ecoqui_envs <- stack(Ecoqui_envs.files)
proj_wgs84 <- crs(Ecoqui_envs)

Ecoqui_points <- read.csv("coqui_rev3.csv", header = T) # there are few weirdly formated rows. manually check and correct them.
Ecoqui_coordinates <- cbind(Ecoqui_points$x, Ecoqui_points$y)
colnames(Ecoqui_coordinates ) <- c("Lon", "Lat")
Ecoqui_occs <- as.data.frame(Ecoqui_coordinates)
Ecoqui_occs.z <- cbind(Ecoqui_occs, raster::extract(Ecoqui_envs, Ecoqui_occs)) # extract raster values at points

Ecoqui_bgpoints <- read.csv("coqui_rev3_background.csv", header = T)
Ecoqui_bg_coordinates <- cbind(Ecoqui_bgpoints$x, Ecoqui_bgpoints$y)
colnames(Ecoqui_bg_coordinates ) <- c("Lon", "Lat")
Ecoqui_bgs <- as.data.frame(Ecoqui_bg_coordinates)
Ecoqui_bgs.z <- cbind(Ecoqui_bgs, raster::extract(Ecoqui_envs, Ecoqui_bgs)) # extract raster values at points

plot(Ecoqui_envs[[1]], xlim = c(-156.25, -154.8), ylim = c(18.7, 20.5))
points(Ecoqui_occs, col = "red")
points(Ecoqui_bgs, col = "blue")

# Run ENMeval
e.mx.Ecoqui <- ENMevaluate(occs = Ecoqui_occs.z, bg = Ecoqui_bgs.z, # Removed 26 occurrence points with NA predictor variable values; Removed 1131 background points with NA predictor variable values
                           algorithm = 'maxnet', partitions = 'randomkfold', partition.settings = list(kfolds = 5),
                           tune.args = list(fc = c("L","LQ","LQH","H"), rm = seq(0.5,5,0.5)), 
                           parallel = TRUE, numCores = 120)
e.mx.Ecoqui # occurrence/background points:  475 / 1047
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
opt.aicc.Ecoqui # fc: LQH, rm: 0.5 (auc.train = 0.8972473, cbi.train = 0.977, auc.val.avg = 0.8978535, cbi.val.avg = 0.8956, or.10p.avg = 0.1157895, or.mtp.avg = 0.004210526, AICc = 6220.3)

# Select the model with the highest CBI
res.Ecoqui[order(-res.Ecoqui$cbi.val.avg),] %>% head()
opt.cbi.Ecoqui <- res.Ecoqui %>% filter(cbi.val.avg == max(cbi.val.avg))
opt.cbi.Ecoqui # fc: H, rm: 0.5 (auc.train = 0.8985382, cbi.train = 0.963, auc.val.avg = 0.8984216, cbi.val.avg = 0.8992, or.10p.avg = 0.1115789, or.mtp.avg = 0.004210526, AICc = 6234.08)


# Repeat the above steps for Ejohnstonei
Ejohnstonei_envs.files <- list.files(path = "johnstonei_contemp", pattern = "^CS[0-9]+\\.tif$", full.names=TRUE)
Ejohnstonei_envs <- stack(Ejohnstonei_envs.files)
proj_wgs84 <- crs(Ejohnstonei_envs)

Ejohnstonei_points <- read.csv("johnstonei_rev3.csv", header = T)
Ejohnstonei_coordinates <- cbind(Ejohnstonei_points$x, Ejohnstonei_points$y)
colnames(Ejohnstonei_coordinates ) <- c("Lon", "Lat")
Ejohnstonei_occs <- as.data.frame(Ejohnstonei_coordinates)
Ejohnstonei_occs.z <- cbind(Ejohnstonei_occs, raster::extract(Ejohnstonei_envs, Ejohnstonei_occs))

Ejohnstonei_bgpoints <- read.csv("johnstonei_rev3_background.csv", header = T)
Ejohnstonei_bg_coordinates <- cbind(Ejohnstonei_bgpoints$x, Ejohnstonei_bgpoints$y)
colnames(Ejohnstonei_bg_coordinates ) <- c("Lon", "Lat")
Ejohnstonei_bgs <- as.data.frame(Ejohnstonei_bg_coordinates)
Ejohnstonei_bgs.z <- cbind(Ejohnstonei_bgs, raster::extract(Ejohnstonei_envs, Ejohnstonei_bgs))

plot(Ejohnstonei_envs[[1]], xlim = c(-77,-72), ylim = c(1,8))
points(Ejohnstonei_occs, pch = 20, cex = 0.2, col = "red")
points(Ejohnstonei_bgs, pch = 20, cex = 0.2, col = "blue")

e.mx.Ejohnstonei <- ENMevaluate(occs = Ejohnstonei_occs.z, bg = Ejohnstonei_bgs.z, 
                                algorithm = 'maxnet', partitions = 'randomkfold', partition.settings = list(kfolds = 5), 
                                tune.args = list(fc = c("L","LQ","LQH","H"), rm = seq(0.5,5,0.5)),
                                parallel = TRUE, numCores = 120)
e.mx.Ejohnstonei
str(e.mx.Ejohnstonei, max.level=2)

eval.results(e.mx.Ejohnstonei) %>% head()

eval.results.partitions(e.mx.Ejohnstonei) %>% head()

evalplot.stats(e = e.mx.Ejohnstonei, stats = c("or.mtp", "auc.val"), color = "fc", x.var = "rm", error.bars = FALSE)

res.Ejohnstonei <- eval.results(e.mx.Ejohnstonei)

res.Ejohnstonei[order(res.Ejohnstonei$delta.AICc),] %>% head()
opt.aicc.Ejohnstonei <- res.Ejohnstonei %>% filter(delta.AICc == 0)
opt.aicc.Ejohnstonei # fc: LQH, rm: 0.5 (auc.train = 0.8530479, cbi.train = 0.949, auc.val.avg = 0.8291212, cbi.val.avg = 0.8354, or.10p.avg = 0.1086379, or.mtp.avg = 0.009302326, AICc = 2831.301)

res.Ejohnstonei[order(-res.Ejohnstonei$cbi.val.avg),] %>% head()
opt.cbi.Ejohnstonei <- res.Ejohnstonei %>% filter(cbi.val.avg == max(cbi.val.avg))
opt.cbi.Ejohnstonei # fc: H, rm: 0.5 (auc.train = 0.8423235, cbi.train = 0.907, auc.val.avg = 0.8107695, cbi.val.avg = 0.8758, or.10p.avg = 0.1325581, or.mtp.avg = 0.01406423, AICc = 2843.561)


# Repeat the above steps for Eplanirostris
Eplanirostris_envs.files <- list.files(path = "planirostris_contemp", pattern = "^CS[0-9]+\\.tif$", full.names=TRUE)
Eplanirostris_envs <- stack(Eplanirostris_envs.files)
proj_wgs84 <- crs(Eplanirostris_envs)

Eplanirostris_points <- read.csv("planirostris_rev3.csv", header = T)
Eplanirostris_coordinates <- cbind(Eplanirostris_points$x, Eplanirostris_points$y)
colnames(Eplanirostris_coordinates ) <- c("Lon", "Lat")
Eplanirostris_occs <- as.data.frame(Eplanirostris_coordinates)
Eplanirostris_occs.z <- cbind(Eplanirostris_occs, raster::extract(Eplanirostris_envs, Eplanirostris_occs))

Eplanirostris_bgpoints <- read.csv("planirostris_rev3_background.csv", header = T)
Eplanirostris_bg_coordinates <- cbind(Eplanirostris_bgpoints$x, Eplanirostris_bgpoints$y)
colnames(Eplanirostris_bg_coordinates ) <- c("Lon", "Lat")
Eplanirostris_bgs <- as.data.frame(Eplanirostris_bg_coordinates)
Eplanirostris_bgs.z <- cbind(Eplanirostris_bgs, raster::extract(Eplanirostris_envs, Eplanirostris_bgs)) # extract raster values at points

plot(Eplanirostris_envs[[1]], xlim = c(-97,-92), ylim = c(28,33))
points(Eplanirostris_occs, pch = 20, cex = 0.2, col = "red")
points(Eplanirostris_bgs, pch = 20, cex = 0.2, col = "blue")

e.mx.Eplanirostris <- ENMevaluate(occs = Eplanirostris_occs.z, bg = Eplanirostris_bgs.z, 
                                  algorithm = 'maxnet', partitions = 'randomkfold', partition.settings = list(kfolds = 5), 
                                  tune.args = list(fc = c("L","LQ","LQH","H"), rm = seq(0.5,5,0.5)),
                                  parallel = TRUE, numCores = 120)
e.mx.Eplanirostris
str(e.mx.Eplanirostris, max.level=2)

eval.results(e.mx.Eplanirostris) %>% head()

eval.results.partitions(e.mx.Eplanirostris) %>% head()

evalplot.stats(e = e.mx.Eplanirostris, stats = c("or.mtp", "auc.val"), color = "fc", x.var = "rm", error.bars = FALSE)

res.Eplanirostris <- eval.results(e.mx.Eplanirostris)

res.Eplanirostris[order(res.Eplanirostris$delta.AICc),] %>% head()
opt.aicc.Eplanirostris <- res.Eplanirostris %>% filter(delta.AICc == 0)
opt.aicc.Eplanirostris # fc: H, rm: 0.5 (auc.train = 0.8904125, cbi.train = 0.994, auc.val.avg = 0.8881872, cbi.val.avg = 0.9768, or.10p.avg = 0.1067843, or.mtp.avg = 0.0005063291, AICc = 34710.76)

res.Eplanirostris[order(-res.Eplanirostris$cbi.val.avg),] %>% head()
opt.cbi.Eplanirostrisi <- res.Eplanirostris %>% filter(cbi.val.avg == max(cbi.val.avg))
opt.cbi.Eplanirostrisi # fc: LQH, rm: 0.5 (auc.train = 0.8904152, cbi.train = 0.994, auc.val.avg = 0.8881982, cbi.val.avg = 0.9772, or.10p.avg = 0.1072919, or.mtp.avg = 0.0005063291, AICc = 34712.41)


# Repeat the above steps for Eantillensis
Eantillensis_envs.files <- list.files(path = "antillensis_contemp", pattern = "^CS[0-9]+\\.tif$", full.names=TRUE)
Eantillensis_envs <- stack(Eantillensis_envs.files)
proj_wgs84 <- crs(Eantillensis_envs)

Eantillensis_points <- read.csv("antillensis_rev3.csv", header = T)
Eantillensis_coordinates <- cbind(Eantillensis_points$x, Eantillensis_points$y)
colnames(Eantillensis_coordinates ) <- c("Lon", "Lat")
Eantillensis_occs <- as.data.frame(Eantillensis_coordinates)
Eantillensis_occs.z <- cbind(Eantillensis_occs, raster::extract(Eantillensis_envs, Eantillensis_occs))
  
Eantillensis_bgpoints <- read.csv("antillensis_rev3_background.csv", header = T)
Eantillensis_bg_coordinates <- cbind(Eantillensis_bgpoints$x, Eantillensis_bgpoints$y)
colnames(Eantillensis_bg_coordinates ) <- c("Lon", "Lat")
Eantillensis_bgs <- as.data.frame(Eantillensis_bg_coordinates)
Eantillensis_bgs.z <- cbind(Eantillensis_bgs, raster::extract(Eantillensis_envs, Eantillensis_bgs)) # extract raster values at points

plot(Eantillensis_envs[[1]])
points(Eantillensis_occs, pch = 20, cex = 0.2, col = "red")
points(Eantillensis_bgs, pch = 20, cex = 0.2, col = "blue")

e.mx.Eantillensis <- ENMevaluate(occs = Eantillensis_occs.z, bg = Eantillensis_bgs.z, 
                                  algorithm = 'maxnet', partition = 'randomkfold', partition.settings = list(kfolds = 5), 
                                  tune.args = list(fc = c("L","LQ","LQH","H"), rm = seq(0.5,5,0.5)),
                                  parallel = TRUE, numCores = 120)
e.mx.Eantillensis
str(e.mx.Eantillensis, max.level=2)

eval.results(e.mx.Eantillensis) %>% head()

eval.results.partitions(e.mx.Eantillensis) %>% head()

evalplot.stats(e = e.mx.Eantillensis, stats = c("or.mtp", "auc.val"), color = "fc", x.var = "rm", error.bars = FALSE)

res.Eantillensis <- eval.results(e.mx.Eantillensis)

res.Eantillensis[order(res.Eantillensis$delta.AICc),] %>% head()
opt.aicc.Eantillensis <- res.Eantillensis %>% filter(delta.AICc == 0)
opt.aicc.Eantillensis # fc: LQH, rm: 1.5 (auc.train = 0.6788033, cbi.train = 0.546, auc.val.avg = 0.6478835, cbi.val.avg = 0.3286, or.10p.avg = 0.1395238, or.mtp.avg = 0, AICc = 2071.963)

res.Eantillensis[order(-res.Eantillensis$cbi.val.avg),] %>% head()
opt.cbi.Eantillensis <- res.Eantillensis %>% filter(cbi.val.avg == max(cbi.val.avg))
opt.cbi.Eantillensis # fc: L, rm: 0.5 (auc.train = 0.6653437, cbi.train = 0.691, auc.val.avg = 0.6653297, cbi.val.avg = 0.6868, or.10p.avg = 0.0952381, or.mtp.avg = 0.005555556, AICc = 2088.932)


# Repeat the above steps for Emartinicensis
Emartinicensis_envs.files <- list.files(path = "martinicensis_contemp", pattern = "^CS[0-9]+\\.tif$", full.names=TRUE)
Emartinicensis_envs <- stack(Emartinicensis_envs.files)
proj_wgs84 <- crs(Emartinicensis_envs)

Emartinicensis_points <- read.csv("martinicensis_rev3.csv", header = T)
Emartinicensis_coordinates <- cbind(Emartinicensis_points$x, Emartinicensis_points$y)
colnames(Emartinicensis_coordinates ) <- c("Lon", "Lat")
Emartinicensis_occs <- as.data.frame(Emartinicensis_coordinates)
Emartinicensis_occs.z <- cbind(Emartinicensis_occs, raster::extract(Emartinicensis_envs, Emartinicensis_occs))
  
Emartinicensis_bgpoints <- read.csv("martinicensis_rev3_background.csv", header = T)
Emartinicensis_bg_coordinates <- cbind(Emartinicensis_bgpoints$x, Emartinicensis_bgpoints$y)
colnames(Emartinicensis_bg_coordinates ) <- c("Lon", "Lat")
Emartinicensis_bgs <- as.data.frame(Emartinicensis_bg_coordinates)
Emartinicensis_bgs.z <- cbind(Emartinicensis_bgs, raster::extract(Emartinicensis_envs, Emartinicensis_bgs)) # extract raster values at points
  
plot(Emartinicensis_envs[[1]])
points(Emartinicensis_occs, pch = 20, cex = 0.2, col = "red")
points(Emartinicensis_bgs, pch = 20, cex = 0.2, col = "blue")

e.mx.Emartinicensis <- ENMevaluate(occs = Emartinicensis_occs.z, bg = Emartinicensis_bgs.z, 
                                  algorithm = 'maxnet', partition = 'randomkfold', partition.settings = list(kfolds = 5), 
                                  tune.args = list(fc = c("L","LQ","LQH","H"), rm = seq(0.5,5,0.5)),
                                  parallel = TRUE, numCores = 120)
e.mx.Emartinicensis
str(e.mx.Emartinicensis, max.level=2)

eval.results(e.mx.Emartinicensis) %>% head()

eval.results.partitions(e.mx.Emartinicensis) %>% head()

evalplot.stats(e = e.mx.Emartinicensis, stats = c("or.mtp", "auc.val"), color = "fc", x.var = "rm", error.bars = FALSE)

res.Emartinicensis <- eval.results(e.mx.Emartinicensis)

res.Emartinicensis[order(res.Emartinicensis$delta.AICc),] %>% head()
opt.aicc.Emartinicensis <- res.Emartinicensis %>% filter(delta.AICc == 0)
opt.aicc.Emartinicensis # fc: LQ, rm: 1 (auc.train = 0.9352007, cbi.train = 0.637, auc.val.avg = 0.9303953, cbi.val.avg = 0.3254, or.10p.avg = 0.09454545, or.mtp.avg = 0.05454545, AICc = 439.4114)

res.Emartinicensis[order(-res.Emartinicensis$cbi.val.avg),] %>% head()
opt.cbi.Emartinicensis <- res.Emartinicensis %>% filter(cbi.val.avg == max(cbi.val.avg))
opt.cbi.Emartinicensis # fc: H, rm: 1 (auc.train = 0.958194, cbi.train = 0.723, auc.val.avg = 0.9559684, cbi.val.avg = 0.7452, or.10p.avg = 0.1290909, or.mtp.avg = 0.07272727, AICc = 456.1586)



# Biomod Maxent
Ecoqui_pre <- Ecoqui_occs
Ecoqui_pre$presence <- 1
Ecoqui_abs <- Ecoqui_bgs
Ecoqui_abs$presence <- 0 # use pre-selected pseudo-absence points
Ecoqui_res <- rbind(Ecoqui_pre, Ecoqui_abs)

# Format the data
Ecoqui_data <- BIOMOD_FormatingData(
  resp.var = Ecoqui_res['presence'],
  resp.xy = Ecoqui_res[, c('Lon', 'Lat')],
  expl.var = Ecoqui_envs,
  resp.name = "E.coqui",
  #PA.nb.rep = 1,
  #PA.nb.absences = 10000,
  #PA.nb.absences = nrow(Ecoqui_bgpoints), 
  #PA.strategy = 'random',
  #filter.raster = TRUE
)

summary(Ecoqui_data)
#  dataset run PA Presences True_Absences Pseudo_Absences Undefined
#1 initial  NA NA       475          1047               0         0

e <- extent(Ecoqui_envs)
par(mfrow = c(1, 2))
plot(Ecoqui_envs[[1]], ext = e, main = "Occurrences")
points(Ecoqui_occs, pch = 20, cex = 0.4, col = "red")

plot(Ecoqui_envs[[1]], ext = e, main = "Background")
points(Ecoqui_bgs, pch = 20, cex = 0.4, col = "blue")
par(mfrow = c(1, 1))

# Define model options
coqui.MAXENT <- list('_allData_allRun' = list(path_to_maxent.jar = "maxent",
                                              linear = FALSE, quadratic = FALSE, product = FALSE, threshold = FALSE, hinge = TRUE,
                                              betamultiplier = 0.5, memory_allocated = 2048))
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
  CV.nb.rep = 5,
  CV.perc = 0.8,
  var.import = 5,
  metric.eval = c('BOYCE','TSS','ROC'),
  modeling.id = "Ecoqui_sdm"
)
saveRDS(Ecoqui_model, "Ecoqui_rev3_model.rds")

# Get model evaluation scores
Ecoqui_model_scores <- get_evaluations(Ecoqui_model)
dim(Ecoqui_model_scores)
dimnames(Ecoqui_model_scores)
(Ecoqui_model_eval.scor_mean <- aggregate(data = Ecoqui_model_scores, calibration ~ metric.eval, FUN = mean))
  # AUCroc = 0.8518333, BOYCE = 0.9378333, TSS = 0.6036667

# Plot model evaluation scores
bm_PlotEvalMean(Ecoqui_model)

# Check variable importance
(Ecoqui_model_var_import <- get_variables_importance(Ecoqui_model))

# Make the mean of variable importance by algorithm
(Ecoqui_var.imp_mean <- aggregate(data = Ecoqui_model_var_import, var.imp ~ expl.var, FUN = mean))
  # CS1=0.7650488, CS2=0.4607276, CS3=0.7480420, CS4=0.9524323, CS5=0.4925977

# Model response plots
Ecoqui_eval_plot <- 
  bm_PlotResponseCurves(
    bm.out  = Ecoqui_model,
    new.env = get_formal_data(Ecoqui_model,'expl.var'), 
    show.variables= get_formal_data(Ecoqui_model,'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var = 'median',
    )

# Run the ensemble model
Ecoqui_ensemble_model <- 
  BIOMOD_EnsembleModeling(
    bm.mod = Ecoqui_model,
    models.chosen = 'all',
    em.by = 'all',
    em.algo = 'EMwmean',
    metric.eval = c('BOYCE','TSS','AUCroc'),
    metric.select = c("BOYCE"),
    metric.select.thresh = c(0.5),
    var.import = 5,
    nb.cpu = 120
  )
saveRDS(Ecoqui_ensemble_model, "Ecoqui_rev3_ensemble_model.rds")

# Assess ensemble models quality
(Ecoqui_ensemble_model_scores <- get_evaluations(Ecoqui_ensemble_model))
#                                                    full.name merged.by.PA
#1 E.coqui_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo   mergedData
#2 E.coqui_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo   mergedData
#3 E.coqui_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo   mergedData
#  merged.by.run merged.by.algo filtered.by    algo metric.eval cutoff
#1     mergedRun     mergedAlgo       BOYCE EMwmean       BOYCE    127
#2     mergedRun     mergedAlgo       BOYCE EMwmean         TSS    146
#3     mergedRun     mergedAlgo       BOYCE EMwmean      AUCroc    156
#  sensitivity specificity calibration validation evaluation
#1      89.263      77.841       0.933         NA         NA
#2      87.579      82.235       0.698         NA         NA
#3      86.105      83.668       0.904         NA         NA

# Check variable importance
(Ecoqui_ensemble_model_var_import <- get_variables_importance(Ecoqui_ensemble_model))
(Ecoqui_ensemble_var.imp_mean <- aggregate(data = Ecoqui_ensemble_model_var_import, var.imp ~ expl.var, FUN = mean))
  # CS1=0.8140482, CS2=0.4133568, CS3=0.8232784, CS4=0.9907358, CS5=0.4252144

# Model response plots
tiff("Ecoqui_rev3_ensemble_eval_plot.tiff", width=664, height=664)
Ecoqui_ensemble_eval_plot <- 
  bm_PlotResponseCurves(
    bm.out  = Ecoqui_ensemble_model,
    new.env = get_formal_data(Ecoqui_ensemble_model,'expl.var'), 
    show.variables= get_formal_data(Ecoqui_ensemble_model,'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var = 'median',
  )
dev.off()

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
    nb.cpu = 120
  )
plot(Ecoqui_ensemble_model_proj_present)

Ecoqui_ensemble_present.pred <- get_predictions(Ecoqui_ensemble_model_proj_present)
Ecoqui_ensemble_present.pred.bin <- get_predictions(Ecoqui_ensemble_model_proj_present, metric.binary = "BOYCE") #This is the binary representation
plot(Ecoqui_ensemble_present.pred)
plot(Ecoqui_ensemble_present.pred.bin)
terra::writeRaster(Ecoqui_ensemble_present.pred, filename = "Ecoqui_rev3_ensemble_present.tif", filetype = "GTiff", overwrite = T)
terra::writeRaster(Ecoqui_ensemble_present.pred.bin, filename = "Ecoqui_rev3_ensemble_present.bin.tif", filetype = "GTiff", overwrite = T)

for (year in c("2041-2060","2081-2100")){
  for (model in c("ACCESS-CM2","INM-CM5-0","MPI-ESM1-2-HR","IPSL-CM6A-LR","UKESM1-0-LL")){
    for (scen in c("ssp126", "ssp585")) {
      envs_files.name <- paste("Ecoqui_envs",model,year,scen, sep="_")
      assign(envs_files.name, list.files(path = paste0("coqui_",year,"/",model,"/",scen), pattern = "^CS[0-9]+\\.tif$", full.names=TRUE))
      envs_stack.name <- paste0("Ecoqui_",model,"_",year,"_",scen,".stack")
      assign(envs_stack.name, stack(get(envs_files.name)))
      envs_stack <- get(envs_stack.name)
      cs_num <- as.integer(sub(".*CS([0-9]+)\\.tif$", "\\1", basename(get(envs_files.name))))
      envs_stack <- envs_stack[[order(cs_num)]]
      names(envs_stack) <- paste0("Band_1.", sort(cs_num))
      sngl.name <- paste0("Ecoqui_rev3_model_proj_",model,"_",year,"_",scen)
      assign(sngl.name, BIOMOD_Projection(
        bm.mod = Ecoqui_model,
        models.chosen = "all",
        new.env = envs_stack,
        proj.name = paste0("Ecoqui_",model,"_",year,"_",scen),
        metric.binary = "all",
        output.format = ".tif",
        do.stack = FALSE
      ))
      saveRDS(get(sngl.name), paste0(sngl.name,".rds"))
      esmbl.name <- paste0("Ecoqui_rev3_ensemble_proj_",model,"_",year,"_",scen)
      assign(esmbl.name, BIOMOD_EnsembleForecasting(
        bm.em = Ecoqui_ensemble_model,
        models.chosen = "all",
        bm.proj = get(sngl.name),
        metric.binary = "all",
        output.format = ".tif",
        do.stack = FALSE,
        nb.cpu = 120
      ))
      saveRDS(get(esmbl.name), paste0(esmbl.name,".rds"))
      pred_file.name <- paste0(paste("Ecoqui_rev3_ensemble",model,year,scen, sep="_"),".pred")
      assign(pred_file.name, get_predictions(get(esmbl.name)))
      pred.bin_file.name <- paste0(paste("Ecoqui_rev3_ensemble",model,year,scen, sep="_"),".pred.bin")
      assign(pred.bin_file.name, get_predictions(get(esmbl.name), metric.binary = "BOYCE"))
    }
  }
} 

# GCM ensemble
for (year in c("2041-2060","2081-2100")){
  for (scen in c("ssp126","ssp585")){
    # load models
    preds <- list()
    for (model in c("ACCESS-CM2","INM-CM5-0","MPI-ESM1-2-HR","IPSL-CM6A-LR","UKESM1-0-LL")){
      #esmbl <- readRDS(paste0("Ecoqui_rev3_ensemble_proj_",model,"_",year,"_",scen,".rds"))
      #r <- get_predictions(esmbl)                    
      pred_name <- paste0("Ecoqui_rev3_ensemble_",model,"_",year,"_",scen,".pred")
      r <- get(pred_name)
      boyce_idx <- grep("EMwmeanByBOYCE", names(r))
      preds[[model]] <- r[[boyce_idx]]
    }
    
    # terra stack
    preds_stack <- rast(preds)

    # weighted mean by BOYCE
    weights <- c("ACCESS-CM2" = 0.20, "INM-CM5-0" = 0.20, "MPI-ESM1-2-HR"= 0.20, "IPSL-CM6A-LR" = 0.20, "UKESM1-0-LL"  = 0.20)
    weights <- weights / sum(weights)
    weights_vec <- weights[names(preds_stack)]
    gcm_ensemble <- weighted.mean(preds_stack, w = weights_vec, na.rm = TRUE)
    
    # save
    assign(paste0("Ecoqui_rev3_GCMensemble_",year,"_",scen), gcm_ensemble)
    
    # export
    terra::writeRaster(gcm_ensemble,
                       filename = paste0("Ecoqui_rev3_GCMensemble_",year,"_",scen,".tif"),
                       filetype = "GTiff",
                       overwrite = TRUE)

    # load binary predictions from each GCM
    bins <- list()
    for (model in c("ACCESS-CM2","INM-CM5-0","MPI-ESM1-2-HR","IPSL-CM6A-LR","UKESM1-0-LL")){
      #esmbl <- readRDS(paste0("Ecoqui_rev3_ensemble_proj_",model,"_",year,"_",scen,".rds"))
      #r <- get_predictions(esmbl, metric.binary = "BOYCE")  
      bin_name <- paste0("Ecoqui_rev3_ensemble_",model,"_",year,"_",scen,".pred.bin")
      r <- get(bin_name)
      boyce_idx <- grep("EMwmeanByBOYCE", names(r))
      bins[[model]] <- r[[boyce_idx]]
    }
    
    # terra stack of binary (0/1) predictions
    bins_stack <- rast(bins)

    # agreement: how many GCMs predict suitable (0–5)
    gcm_agreement <- sum(bins_stack, na.rm = TRUE)

    # consensus binary: suitable where majority (>=3 of 5) agree
    gcm_consensus_bin <- gcm_agreement >= 3

    # save objects
    assign(paste0("Ecoqui_rev3_GCMagreement_",year,"_",scen), gcm_agreement)
    assign(paste0("Ecoqui_rev3_GCMconsensus_",year,"_",scen), gcm_consensus_bin)

    # export
    terra::writeRaster(gcm_agreement,
                       filename = paste0("Ecoqui_rev3_GCMagreement_",year,"_",scen,".tif"),
                       filetype = "GTiff", overwrite = TRUE)
    terra::writeRaster(gcm_consensus_bin,
                       filename = paste0("Ecoqui_rev3_GCMconsensus_",year,"_",scen,".tif"),
                       filetype = "GTiff", overwrite = TRUE)
  }
}


# Repeat above steps for Ejohnstonei
# Biomod Maxent
Ejohnstonei_pre <- Ejohnstonei_occs
Ejohnstonei_pre$presence <- 1
Ejohnstonei_abs <- Ejohnstonei_bgs
Ejohnstonei_abs$presence <- 0
Ejohnstonei_res <- rbind(Ejohnstonei_pre, Ejohnstonei_abs)
Ejohnstonei_res %>% head()
Ejohnstonei_res %>% tail()

# Format the data
Ejohnstonei_data <- BIOMOD_FormatingData(
  resp.var = Ejohnstonei_res['presence'],
  resp.xy = Ejohnstonei_res[, c('Lon', 'Lat')],
  expl.var = Ejohnstonei_envs,
  resp.name = "E.johnstonei",
  #PA.nb.rep = 1,
  #PA.nb.absences = 10000,
  #PA.strategy = 'random',
  #filter.raster = TRUE
)

summary(Ejohnstonei_data)
#  dataset run PA Presences True_Absences Pseudo_Absences Undefined
#1 initial  NA NA       211          1305               0         0


# Define model options
johnstonei.MAXENT <- list('_allData_allRun' = list(path_to_maxent.jar = "maxent",
                                                   linear = FALSE, quadratic = FALSE, product = FALSE, threshold = FALSE, hinge = TRUE,
                                                   betamultiplier = 0.5, memory_allocated = 2048))
johnstonei.val <- list(MAXENT.binary.MAXENT.MAXENT = johnstonei.MAXENT)
Ejohnstonei_opt <- bm_ModelingOptions(
  data.type = 'binary',
  models = c("MAXENT"),
  strategy = 'user.defined',
  user.val = johnstonei.val
)

# Run model
Ejohnstonei_model <- BIOMOD_Modeling(
  bm.format = Ejohnstonei_data,
  models = c("MAXENT"),
  OPT.user = Ejohnstonei_opt,
  CV.strategy = 'random',
  CV.nb.rep = 5,
  CV.perc = 0.8,
  var.import = 5,
  metric.eval = c('BOYCE','TSS','ROC'),
  modeling.id = "Ejohnstonei_sdm"
)
saveRDS(Ejohnstonei_model, "Ejohnstonei_rev3_model.rds")

# Get model evaluation scores
Ejohnstonei_model_scores <- get_evaluations(Ejohnstonei_model)
dim(Ejohnstonei_model_scores)
dimnames(Ejohnstonei_model_scores)
(Ejohnstonei_model_eval.scor_mean <- aggregate(data = Ejohnstonei_model_scores, calibration ~ metric.eval, FUN = mean))
  # AUCroc = 0.7208333, BOYCE = 0.5886667, TSS = 0.3636667

# Plot model evaluation scores
bm_PlotEvalMean(Ejohnstonei_model)

# Check variable importance
(Ejohnstonei_model_var_import <- get_variables_importance(Ejohnstonei_model))

# Make the mean of variable importance by algorithm
(Ejohnstonei_var.imp_mean <- aggregate(data = Ejohnstonei_model_var_import, var.imp ~ expl.var, FUN = mean))
  # CS1=0.6344442, CS2=0.5061064, CS3=0.4384222, CS4=0.2632552, CS5=0.5979360

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
    metric.eval = c('BOYCE','TSS','ROC'),
    metric.select = c("BOYCE"),
    metric.select.thresh = c(0.5),
    var.import = 5,
    nb.cpu = 120
  )
saveRDS(Ejohnstonei_ensemble_model, "Ejohnstonei_rev3_ensemble_model.rds")

# Assess ensemble models quality
(Ejohnstonei_ensemble_model_scores <- get_evaluations(Ejohnstonei_ensemble_model))
#                                                    full.name merged.by.PA
#1 E.johnstonei_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo   mergedData
#2 E.johnstonei_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo   mergedData
#3 E.johnstonei_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo   mergedData
#  merged.by.run merged.by.algo filtered.by    algo metric.eval cutoff
#1     mergedRun     mergedAlgo       BOYCE EMwmean       BOYCE  283.0
#2     mergedRun     mergedAlgo       BOYCE EMwmean         TSS  180.0
#3     mergedRun     mergedAlgo       BOYCE EMwmean      AUCroc  174.5
#  sensitivity specificity calibration validation evaluation
#1      67.299      89.195       0.971         NA         NA
#2      82.464      74.943       0.574         NA         NA
#3      83.886      74.253       0.846         NA         NA

# Check variable importance
(Ejohnstonei_ensemble_model_var_import <- get_variables_importance(Ejohnstonei_ensemble_model))
(Ejohnstonei_ensemble_var.imp_mean <- aggregate(data = Ejohnstonei_ensemble_model_var_import, var.imp ~ expl.var, FUN = mean))
  # CS1=1.0000000, CS2=0.3954430, CS3=0.5476606, CS4=0.2591836, CS5=0.5309994

# Model response plots
tiff("Ejohnstonei_rev3_ensemble_eval_plot.tiff", width=664, height=664)
Ejohnstonei_ensemble_eval_plot <- 
  bm_PlotResponseCurves(
    bm.out  = Ejohnstonei_ensemble_model,
    new.env = get_formal_data(Ejohnstonei_ensemble_model,'expl.var'), 
    show.variables= get_formal_data(Ejohnstonei_ensemble_model,'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var = 'median',
  )
dev.off()

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
    nb.cpu = 120
  )
plot(Ejohnstonei_ensemble_model_proj_present)

Ejohnstonei_ensemble_present.pred <- get_predictions(Ejohnstonei_ensemble_model_proj_present)
Ejohnstonei_ensemble_present.pred.bin <- get_predictions(Ejohnstonei_ensemble_model_proj_present, metric.binary = "BOYCE") #This is the binary representation
plot(Ejohnstonei_ensemble_present.pred)
plot(Ejohnstonei_ensemble_present.pred.bin)
terra::writeRaster(Ejohnstonei_ensemble_present.pred, filename = "Ejohnstonei_rev3_ensemble_present.tif", filetype  = "GTiff", overwrite = T)
terra::writeRaster(Ejohnstonei_ensemble_present.pred.bin, filename = "Ejohnstonei_rev3_ensemble_present.bin.tif", filetype = "GTiff", overwrite = T)

for (year in c("2041-2060","2081-2100")){
  for (model in c("ACCESS-CM2","INM-CM5-0","MPI-ESM1-2-HR","IPSL-CM6A-LR","UKESM1-0-LL")){
    for (scen in c("ssp126", "ssp585")) {
      envs_files.name <- paste("Ejohnstonei_envs",model,year,scen, sep="_")
      assign(envs_files.name, list.files(path = paste0("johnstonei_",year,"/",model,"/",scen), pattern = "^CS[0-9]+\\.tif$", full.names=TRUE))
      envs_stack.name <- paste0("Ejohnstonei_",model,"_",year,"_",scen,".stack")
      assign(envs_stack.name, stack(get(envs_files.name)))
      envs_stack <- get(envs_stack.name)
      cs_num <- as.integer(sub(".*CS([0-9]+)\\.tif$", "\\1", basename(get(envs_files.name))))
      envs_stack <- envs_stack[[order(cs_num)]]
      names(envs_stack) <- paste0("Band_1.", sort(cs_num))
      sngl.name <- paste0("Ejohnstonei_rev3_model_proj_",model,"_",year,"_",scen)
      assign(sngl.name, BIOMOD_Projection(
        bm.mod = Ejohnstonei_model,
        models.chosen = "all",
        new.env = envs_stack,
        proj.name = paste0("Ejohnstonei_",model,"_",year,"_",scen),
        metric.binary = "all",
        output.format = ".tif",
        do.stack = FALSE
      ))
      saveRDS(get(sngl.name), paste0(sngl.name,".rds"))
      esmbl.name <- paste0("Ejohnstonei_rev3_ensemble_proj_",model,"_",year,"_",scen)
      assign(esmbl.name, BIOMOD_EnsembleForecasting(
        bm.em = Ejohnstonei_ensemble_model,
        models.chosen = "all",
        bm.proj = get(sngl.name),
        metric.binary = "all",
        output.format = ".tif",
        do.stack = FALSE,
        nb.cpu = 120
      ))
      saveRDS(get(esmbl.name), paste0(esmbl.name,".rds"))
      pred_file.name <- paste0(paste("Ejohnstonei_rev3_ensemble",model,year,scen, sep="_"),".pred")
      assign(pred_file.name, get_predictions(get(esmbl.name)))
      pred.bin_file.name <- paste0(paste("Ejohnstonei_rev3_ensemble",model,year,scen, sep="_"),".pred.bin")
      assign(pred.bin_file.name, get_predictions(get(esmbl.name), metric.binary = "BOYCE"))
    }
  }
} 

# GCM ensemble
for (year in c("2041-2060","2081-2100")){
  for (scen in c("ssp126","ssp585")){
    # load models
    preds <- list()
    for (model in c("ACCESS-CM2","INM-CM5-0","MPI-ESM1-2-HR","IPSL-CM6A-LR","UKESM1-0-LL")){
      pred_name <- paste0("Ejohnstonei_rev3_ensemble_",model,"_",year,"_",scen,".pred")
      r <- get(pred_name)
      boyce_idx <- grep("EMwmeanByBOYCE", names(r))
      preds[[model]] <- r[[boyce_idx]]
    }
    
    # terra stack
    preds_stack <- rast(preds)

    # weighted mean by BOYCE
    weights <- c("ACCESS-CM2" = 0.20, "INM-CM5-0" = 0.20, "MPI-ESM1-2-HR"= 0.20, "IPSL-CM6A-LR" = 0.20, "UKESM1-0-LL"  = 0.20)
    weights <- weights / sum(weights)
    weights_vec <- weights[names(preds_stack)]
    gcm_ensemble <- weighted.mean(preds_stack, w = weights_vec, na.rm = TRUE)
    
    # save
    assign(paste0("Ejohnstonei_rev3_GCMensemble_",year,"_",scen), gcm_ensemble)
    
    # export
    terra::writeRaster(gcm_ensemble,
                       filename = paste0("Ejohnstonei_rev3_GCMensemble_",year,"_",scen,".tif"),
                       filetype = "GTiff",
                       overwrite = TRUE)

    # load binary predictions from each GCM
    bins <- list()
    for (model in c("ACCESS-CM2","INM-CM5-0","MPI-ESM1-2-HR","IPSL-CM6A-LR","UKESM1-0-LL")){
      esmbl <- readRDS(paste0("Ejohnstonei_rev3_ensemble_proj_",model,"_",year,"_",scen,".rds"))
      r <- get_predictions(esmbl, metric.binary = "BOYCE")   # binary 예측 다시 추출
      #bin_name <- paste0("Eplanirostris_rev3_ensemble_",model,"_",year,"_",scen,".pred.bin")
      #r <- get(bin_name)
      boyce_idx <- grep("EMwmeanByBOYCE", names(r))
      bins[[model]] <- r[[boyce_idx]]
    }
    
    # terra stack of binary (0/1) predictions
    bins_stack <- rast(bins)

    # agreement: how many GCMs predict suitable (0–5)
    gcm_agreement <- sum(bins_stack, na.rm = TRUE)

    # consensus binary: suitable where majority (>=3 of 5) agree
    gcm_consensus_bin <- gcm_agreement >= 3

    # save objects
    assign(paste0("Ejohnstonei_rev3_GCMagreement_",year,"_",scen), gcm_agreement)
    assign(paste0("Ejohnstonei_rev3_GCMconsensus_",year,"_",scen), gcm_consensus_bin)

    # export
    terra::writeRaster(gcm_agreement,
                       filename = paste0("Ejohnstonei_rev3_GCMagreement_",year,"_",scen,".tif"),
                       filetype = "GTiff", overwrite = TRUE)
    terra::writeRaster(gcm_consensus_bin,
                       filename = paste0("Ejohnstonei_rev3_GCMconsensus_",year,"_",scen,".tif"),
                       filetype = "GTiff", overwrite = TRUE)
  }
}


# Repeat above steps for Eplanirostris
# Biomod Maxent
Eplanirostris_pre <- Eplanirostris_occs
Eplanirostris_pre$presence <- 1
Eplanirostris_abs <- Eplanirostris_bgs
Eplanirostris_abs$presence <- 0
Eplanirostris_res <- rbind(Eplanirostris_pre, Eplanirostris_abs)
Eplanirostris_res %>% head()
Eplanirostris_res %>% tail()

# Format the data
Eplanirostris_data <- BIOMOD_FormatingData(
  resp.var = Eplanirostris_res['presence'],
  resp.xy = Eplanirostris_res[, c('Lon', 'Lat')],
  expl.var = Eplanirostris_envs,
  resp.name = "E.planirostris",
  #PA.nb.rep = 1,
  #PA.nb.absences = 10000,
  #PA.strategy = 'random',
  #filter.raster = TRUE
)

summary(Eplanirostris_data)
#  dataset run PA Presences True_Absences Pseudo_Absences Undefined
#1 initial  NA NA      1976         17940               0         0

# Define model options
Eplanirostris.MAXENT <- list('_allData_allRun' = list(path_to_maxent.jar = "maxent",
                                                      linear = TRUE, quadratic = TRUE, product = FALSE, threshold = FALSE, hinge = TRUE,
                                                      betamultiplier = 0.5, memory_allocated = 2048))
Eplanirostris.val <- list(MAXENT.binary.MAXENT.MAXENT = Eplanirostris.MAXENT)
Eplanirostris_opt <- bm_ModelingOptions(
  data.type = 'binary',
  models = c("MAXENT"),
  strategy = 'user.defined',
  user.val = Eplanirostris.val
)

# Run model
Eplanirostris_model <- BIOMOD_Modeling(
  bm.format = Eplanirostris_data,
  models = c("MAXENT"),
  OPT.user = Eplanirostris_opt,
  CV.strategy = 'random',
  CV.nb.rep = 5,
  CV.perc = 0.8,
  var.import = 5,
  metric.eval = c('BOYCE','TSS','ROC'),
  modeling.id = "Eplanirostris_sdm"
)
saveRDS(Eplanirostris_model, "Eplanirostris_rev3_model.rds")

# Get model evaluation scores
Eplanirostris_model_scores <- get_evaluations(Eplanirostris_model)
dim(Eplanirostris_model_scores)
dimnames(Eplanirostris_model_scores)
(Eplanirostris_model_eval.scor_mean <- aggregate(data = Eplanirostris_model_scores, calibration ~ metric.eval, FUN = mean))
  # AUCroc = 0.8261667, BOYCE = 0.9116667, TSS = 0.5376667

# Plot model evaluation scores
bm_PlotEvalMean(Eplanirostris_model)

# Check variable importance
(Eplanirostris_model_var_import <- get_variables_importance(Eplanirostris_model))

# Make the mean of variable importance by algorithm
(Eplanirostris_var.imp_mean <- aggregate(data = Eplanirostris_model_var_import, var.imp ~ expl.var, FUN = mean))
  # CS1=0.2951879, CS2=0.2801503, CS3=0.7330199, CS4=0.7749140, CS5=0.0688008, CS6=0.1834205

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
    metric.eval = c('BOYCE','TSS','ROC'),
    metric.select = c("BOYCE"),
    metric.select.thresh = c(0.5),
    var.import = 5,
    nb.cpu = 120
  )
saveRDS(Eplanirostris_ensemble_model, "Eplanirostris_rev3_ensemble_model.rds")

# Assess ensemble models quality ----
(Eplanirostris_ensemble_model_scores <- get_evaluations(Eplanirostris_ensemble_model))
#                                                      full.name merged.by.PA
#1 E.planirostris_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo   mergedData
#2 E.planirostris_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo   mergedData
#3 E.planirostris_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo   mergedData
#  merged.by.run merged.by.algo filtered.by    algo metric.eval cutoff
#1     mergedRun     mergedAlgo       BOYCE EMwmean       BOYCE  227.0
#2     mergedRun     mergedAlgo       BOYCE EMwmean         TSS  279.0
#3     mergedRun     mergedAlgo       BOYCE EMwmean      AUCroc  287.5
#  sensitivity specificity calibration validation evaluation
#1      92.257      71.800       0.994         NA         NA
#2      87.551      76.628       0.642         NA         NA
#3      85.678      77.798       0.892         NA         NA

# Check variable importance
(Eplanirostris_ensemble_model_var_import <- get_variables_importance(Eplanirostris_ensemble_model))

# Make the mean of variable importance by algorithm
(Eplanirostris_ensemble_var.imp_mean <- aggregate(data = Eplanirostris_ensemble_model_var_import, var.imp ~ expl.var, FUN = mean))
  # CS1=0.1620628, CS2=0.1633084, CS3=0.7113580, CS4=0.7691700, CS5=0.0356642, CS6=0.0314618

# Model response plots
tiff("Eplanirostris_rev3_ensemble_eval_plot.tiff", width=664, height=664)
Eplanirostris_ensemble_eval_plot <- 
  bm_PlotResponseCurves(
    bm.out  = Eplanirostris_ensemble_model,
    new.env = get_formal_data(Eplanirostris_ensemble_model,'expl.var'), 
    show.variables= get_formal_data(Eplanirostris_ensemble_model,'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var = 'median',
  )
dev.off()

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
    nb.cpu = 120
  )
plot(Eplanirostris_ensemble_model_proj_present)

Eplanirostris_ensemble_present.pred <- get_predictions(Eplanirostris_ensemble_model_proj_present)
Eplanirostris_ensemble_present.pred.bin <- get_predictions(Eplanirostris_ensemble_model_proj_present, metric.binary = "BOYCE") #This is the binary representation
plot(Eplanirostris_ensemble_present.pred)
plot(Eplanirostris_ensemble_present.pred.bin)
terra::writeRaster(Eplanirostris_ensemble_present.pred, filename = "Eplanirostris_rev3_ensemble_present.tif", filetype = "GTiff", overwrite = T)
terra::writeRaster(Eplanirostris_ensemble_present.pred.bin, filename = "Eplanirostris_rev3_ensemble_present.bin.tif", filetype = "GTiff", overwrite = T)

for (year in c("2041-2060","2081-2100")){
  for (model in c("ACCESS-CM2","INM-CM5-0","MPI-ESM1-2-HR","IPSL-CM6A-LR","UKESM1-0-LL")){
    for (scen in c("ssp126", "ssp585")) {
      envs_files.name <- paste("Eplanirostris_envs",model,year,scen, sep="_")
      assign(envs_files.name, list.files(path = paste0("planirostris_",year,"/",model,"/",scen), pattern = "^CS[0-9]+\\.tif$", full.names=TRUE))
      envs_stack.name <- paste0("Eplanirostris_",model,"_",year,"_",scen,".stack")
      assign(envs_stack.name, stack(get(envs_files.name)))
      envs_stack <- get(envs_stack.name)
      cs_num <- as.integer(sub(".*CS([0-9]+)\\.tif$", "\\1", basename(get(envs_files.name))))
      envs_stack <- envs_stack[[order(cs_num)]]
      names(envs_stack) <- paste0("Band_1.", sort(cs_num))
      sngl.name <- paste0("Eplanirostris_rev3_model_proj_",model,"_",year,"_",scen)
      assign(sngl.name, BIOMOD_Projection(
        bm.mod = Eplanirostris_model,
        models.chosen = "all",
        new.env = envs_stack,
        proj.name = paste0("Eplanirostris_",model,"_",year,"_",scen),
        metric.binary = "all",
        output.format = ".tif",
        do.stack = FALSE
      ))
      saveRDS(get(sngl.name), paste0(sngl.name,".rds"))
      esmbl.name <- paste0("Eplanirostris_rev3_ensemble_proj_",model,"_",year,"_",scen)
      assign(esmbl.name, BIOMOD_EnsembleForecasting(
        bm.em = Eplanirostris_ensemble_model,
        models.chosen = "all",
        bm.proj = get(sngl.name),
        metric.binary = "all",
        output.format = ".tif",
        do.stack = FALSE,
        nb.cpu = 120
      ))
      saveRDS(get(esmbl.name), paste0(esmbl.name,".rds"))
      pred_file.name <- paste0(paste("Eplanirostris_rev3_ensemble",model,year,scen, sep="_"),".pred")
      assign(pred_file.name, get_predictions(get(esmbl.name)))
      pred.bin_file.name <- paste0(paste("Eplanirostris_rev3_ensemble",model,year,scen, sep="_"),".pred.bin")
      assign(pred.bin_file.name, get_predictions(get(esmbl.name), metric.binary = "BOYCE"))
    }
  }
} 

# GCM ensemble
for (year in c("2041-2060","2081-2100")){
  for (scen in c("ssp126","ssp585")){
    # load models
    preds <- list()
    for (model in c("ACCESS-CM2","INM-CM5-0","MPI-ESM1-2-HR","IPSL-CM6A-LR","UKESM1-0-LL")){
      pred_name <- paste0("Eplanirostris_rev3_ensemble_",model,"_",year,"_",scen,".pred")
      r <- get(pred_name)
      boyce_idx <- grep("EMwmeanByBOYCE", names(r))
      preds[[model]] <- r[[boyce_idx]]
    }
    
    # terra stack
    preds_stack <- rast(preds)

    # weighted mean by BOYCE
    weights <- c("ACCESS-CM2" = 0.20, "INM-CM5-0" = 0.20, "MPI-ESM1-2-HR"= 0.20, "IPSL-CM6A-LR" = 0.20, "UKESM1-0-LL"  = 0.20)
    weights <- weights / sum(weights)
    weights_vec <- weights[names(preds_stack)]
    gcm_ensemble <- weighted.mean(preds_stack, w = weights_vec, na.rm = TRUE)
    
    # save
    assign(paste0("Eplanirostris_rev3_GCMensemble_",year,"_",scen), gcm_ensemble)
    
    # export
    terra::writeRaster(gcm_ensemble,
                       filename = paste0("Eplanirostris_rev3_GCMensemble_",year,"_",scen,".tif"),
                       filetype = "GTiff",
                       overwrite = TRUE)

    # load binary predictions from each GCM
    bins <- list()
    for (model in c("ACCESS-CM2","INM-CM5-0","MPI-ESM1-2-HR","IPSL-CM6A-LR","UKESM1-0-LL")){
      bin_name <- paste0("Eplanirostris_rev3_ensemble_",model,"_",year,"_",scen,".pred.bin")
      r <- get(bin_name)
      boyce_idx <- grep("EMwmeanByBOYCE", names(r))
      bins[[model]] <- r[[boyce_idx]]
    }
    
    # terra stack of binary (0/1) predictions
    bins_stack <- rast(bins)

    # agreement: how many GCMs predict suitable (0–5)
    gcm_agreement <- sum(bins_stack, na.rm = TRUE)

    # consensus binary: suitable where majority (>=3 of 5) agree
    gcm_consensus_bin <- gcm_agreement >= 3

    # save objects
    assign(paste0("Eplanirostris_rev3_GCMagreement_",year,"_",scen), gcm_agreement)
    assign(paste0("Eplanirostris_rev3_GCMconsensus_",year,"_",scen), gcm_consensus_bin)

    # export
    terra::writeRaster(gcm_agreement,
                       filename = paste0("Eplanirostris_rev3_GCMagreement_",year,"_",scen,".tif"),
                       filetype = "GTiff", overwrite = TRUE)
    terra::writeRaster(gcm_consensus_bin,
                       filename = paste0("Eplanirostris_rev3_GCMconsensus_",year,"_",scen,".tif"),
                       filetype = "GTiff", overwrite = TRUE)
  }
}


# Repeat above steps for Eantillensis
# Biomod Maxent
Eantillensis_pre <- Eantillensis_occs
Eantillensis_pre$presence <- 1
Eantillensis_abs <- Eantillensis_bgs
Eantillensis_abs$presence <- 0
Eantillensis_res <- rbind(Eantillensis_pre, Eantillensis_abs)
Eantillensis_res %>% head()
Eantillensis_res %>% tail()

# Format the data
Eantillensis_data <- BIOMOD_FormatingData(
  resp.var = Eantillensis_res['presence'],
  resp.xy = Eantillensis_res[, c('Lon', 'Lat')],
  expl.var = Eantillensis_envs,
  resp.name = "E.antillensis",
  #PA.nb.rep = 1,
  #PA.nb.absences = 10000, # due to the small range, all available cells were chosen as background points which was n=351; thus, only one set of 351 background points were used.
  #PA.strategy = 'random',
  #filter.raster = TRUE
)

summary(Eantillensis_data)
#  dataset run PA Presences True_Absences Pseudo_Absences Undefined
#1 initial  NA NA       179           259               0         0

# Define model options
Eantillensis.MAXENT <- list('_allData_allRun' = list(path_to_maxent.jar = "maxent",
                                                     linear = TRUE, quadratic = FALSE, product = FALSE, threshold = FALSE, hinge = FALSE,
                                                     betamultiplier = 0.5, memory_allocated = 2048))
Eantillensis.val <- list(MAXENT.binary.MAXENT.MAXENT = Eantillensis.MAXENT)
Eantillensis_opt <- bm_ModelingOptions(
  data.type = 'binary',
  models = c("MAXENT"),
  strategy = 'user.defined',
  user.val = Eantillensis.val
)

# Run model
Eantillensis_model <- BIOMOD_Modeling(
  bm.format = Eantillensis_data,
  models = c("MAXENT"),
  OPT.user = Eantillensis_opt,
  CV.strategy = 'random',
  CV.nb.rep = 5,
  CV.perc = 0.8,
  var.import = 5,
  metric.eval = c('BOYCE','TSS','ROC'),
  modeling.id = "Eantillensis_sdm"
)
saveRDS(Eantillensis_model, "Eantillensis_rev3_model.rds")

# Get model evaluation scores
Eantillensis_model_scores <- get_evaluations(Eantillensis_model)
dim(Eantillensis_model_scores)
dimnames(Eantillensis_model_scores)
(Eantillensis_model_eval.scor_mean <- aggregate(data = Eantillensis_model_scores, calibration ~ metric.eval, FUN = mean))
  # AUCroc = 0.6746667, BOYCE = 0.6546667, TSS = 0.3235000

# Plot model evaluation scores
bm_PlotEvalMean(Eantillensis_model)

# Check variable importance
(Eantillensis_model_var_import <- get_variables_importance(Eantillensis_model))

# Make the mean of variable importance by algorithm
(Eantillensis_var.imp_mean <- aggregate(data = Eantillensis_model_var_import, var.imp ~ expl.var, FUN = mean))
  # CS1=0.9992640, CS2=0.9144897, CS3=0.7897909, CS4=0.6426599

# Model response plots
Eantillensis_eval_plot <- 
  bm_PlotResponseCurves(
    bm.out  = Eantillensis_model,
    new.env = get_formal_data(Eantillensis_model,'expl.var'), 
    show.variables= get_formal_data(Eantillensis_model,'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var = 'median',
  )

# Run the ensemble model
Eantillensis_ensemble_model <- 
  BIOMOD_EnsembleModeling(
    bm.mod = Eantillensis_model,
    models.chosen = 'all',
    em.by = 'all',
    em.algo = 'EMwmean',
    metric.eval = c('BOYCE','TSS','ROC'),
    metric.select = c("BOYCE"),
    metric.select.thresh = c(0.5), 
    var.import = 5,
    nb.cpu = 120
  )
saveRDS(Eantillensis_ensemble_model, "Eantillensis_rev3_ensemble_model.rds")

# Assess ensemble models qualit
(Eantillensis_ensemble_model_scores <- get_evaluations(Eantillensis_ensemble_model))
#                                                     full.name merged.by.PA
#1 E.antillensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo   mergedData
#2 E.antillensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo   mergedData
#3 E.antillensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo   mergedData
#  merged.by.run merged.by.algo filtered.by    algo metric.eval cutoff
#1     mergedRun     mergedAlgo       BOYCE EMwmean       BOYCE  677.0
#2     mergedRun     mergedAlgo       BOYCE EMwmean         TSS  472.0
#3     mergedRun     mergedAlgo       BOYCE EMwmean      AUCroc  471.5
#  sensitivity specificity calibration validation evaluation
#1       2.793      98.069       0.721         NA         NA
#2      74.860      55.985       0.314         NA         NA
#3      75.419      55.985       0.679         NA         NA

# Check variable importance
(Eantillensis_ensemble_model_var_import <- get_variables_importance(Eantillensis_ensemble_model))

# Make the mean of variable importance by algorithm
(Eantillensis_ensemble_var.imp_mean <- aggregate(data = Eantillensis_ensemble_model_var_import, var.imp ~ expl.var, FUN = mean))
  # CS1=1.0000000, CS2=0.9792328, CS3=0.7253666, CS4=0.8206832

# Model response plots
tiff("Eantillensis_rev3_ensemble_eval_plot.tiff", width=664, height=664)
Eantillensis_ensemble_eval_plot <- 
  bm_PlotResponseCurves(
    bm.out  = Eantillensis_ensemble_model,
    new.env = get_formal_data(Eantillensis_ensemble_model,'expl.var'), 
    show.variables= get_formal_data(Eantillensis_ensemble_model,'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var = 'median',
  )
dev.off()

# Present environment projections
Eantillensis_model_proj_present <- 
  BIOMOD_Projection(
    bm.mod = Eantillensis_model,
    models.chosen = "all",
    new.env = Eantillensis_envs,
    proj.name = "Eantillensis_present",
    metric.binary = "all",
    output.format = ".tif",
    do.stack = FALSE
  )

Eantillensis_ensemble_model_proj_present <- 
  BIOMOD_EnsembleForecasting(
    bm.em = Eantillensis_ensemble_model,
    models.chosen = "all",
    bm.proj = Eantillensis_model_proj_present,
    metric.binary = "all",
    output.format = ".tif",
    do.stack = FALSE,
    nb.cpu = 120
  )
plot(Eantillensis_ensemble_model_proj_present)

Eantillensis_ensemble_present.pred <- get_predictions(Eantillensis_ensemble_model_proj_present)
Eantillensis_ensemble_present.pred.bin <- get_predictions(Eantillensis_ensemble_model_proj_present, metric.binary = "BOYCE") #This is the binary representation
plot(Eantillensis_ensemble_present.pred)
plot(Eantillensis_ensemble_present.pred.bin)
terra::writeRaster(Eantillensis_ensemble_present.pred, filename = "Eantillensis_rev3_ensemble_present.tif", filetype = "GTiff", overwrite = T)
terra::writeRaster(Eantillensis_ensemble_present.pred.bin, filename = "Eantillensis_rev3_ensemble_present.bin.tif", filetype = "GTiff", overwrite = T)

for (year in c("2041-2060","2081-2100")){
  for (model in c("ACCESS-CM2","INM-CM5-0","MPI-ESM1-2-HR","IPSL-CM6A-LR","UKESM1-0-LL")){
    for (scen in c("ssp126", "ssp585")) {
      envs_files.name <- paste("Eantillensis_envs",model,year,scen, sep="_")
      assign(envs_files.name, list.files(path = paste0("antillensis_",year,"/",model,"/",scen), pattern = "^CS[0-9]+\\.tif$", full.names=TRUE))
      envs_stack.name <- paste0("Eantillensis_",model,"_",year,"_",scen,".stack")
      assign(envs_stack.name, stack(get(envs_files.name)))
      envs_stack <- get(envs_stack.name)
      cs_num <- as.integer(sub(".*CS([0-9]+)\\.tif$", "\\1", basename(get(envs_files.name))))
      envs_stack <- envs_stack[[order(cs_num)]]
      names(envs_stack) <- paste0("Band_1.", sort(cs_num))
      sngl.name <- paste0("Eantillensis_rev3_model_proj_",model,"_",year,"_",scen)
      assign(sngl.name, BIOMOD_Projection(
        bm.mod = Eantillensis_model,
        models.chosen = "all",
        new.env = envs_stack,
        proj.name = paste0("Eantillensis_",model,"_",year,"_",scen),
        metric.binary = "all",
        output.format = ".tif",
        do.stack = FALSE
      ))
      saveRDS(get(sngl.name), paste0(sngl.name,".rds"))
      esmbl.name <- paste0("Eantillensis_rev3_ensemble_proj_",model,"_",year,"_",scen)
      assign(esmbl.name, BIOMOD_EnsembleForecasting(
        bm.em = Eantillensis_ensemble_model,
        models.chosen = "all",
        bm.proj = get(sngl.name),
        metric.binary = "all",
        output.format = ".tif",
        do.stack = FALSE,
        nb.cpu = 120
      ))
      saveRDS(get(esmbl.name), paste0(esmbl.name,".rds"))
      pred_file.name <- paste0(paste("Eantillensis_rev3_ensemble",model,year,scen, sep="_"),".pred")
      assign(pred_file.name, get_predictions(get(esmbl.name)))
      pred.bin_file.name <- paste0(paste("Eantillensis_rev3_ensemble",model,year,scen, sep="_"),".pred.bin")
      assign(pred.bin_file.name, get_predictions(get(esmbl.name), metric.binary = "BOYCE"))
    }
  }
} 

# GCM ensemble
for (year in c("2041-2060","2081-2100")){
  for (scen in c("ssp126","ssp585")){
    # load models
    preds <- list()
    for (model in c("ACCESS-CM2","INM-CM5-0","MPI-ESM1-2-HR","IPSL-CM6A-LR","UKESM1-0-LL")){
      pred_name <- paste0("Eantillensis_rev3_ensemble_",model,"_",year,"_",scen,".pred")
      r <- get(pred_name)
      boyce_idx <- grep("EMwmeanByBOYCE", names(r))
      preds[[model]] <- r[[boyce_idx]]
    }
    
    # terra stack
    preds_stack <- rast(preds)

    # weighted mean by BOYCE
    weights <- c("ACCESS-CM2" = 0.20, "INM-CM5-0" = 0.20, "MPI-ESM1-2-HR"= 0.20, "IPSL-CM6A-LR" = 0.20, "UKESM1-0-LL"  = 0.20)
    weights <- weights / sum(weights)
    weights_vec <- weights[names(preds_stack)]
    gcm_ensemble <- weighted.mean(preds_stack, w = weights_vec, na.rm = TRUE)
    
    # save
    assign(paste0("Eantillensis_rev3_GCMensemble_",year,"_",scen), gcm_ensemble)
    
    # export
    terra::writeRaster(gcm_ensemble,
                       filename = paste0("Eantillensis_rev3_GCMensemble_",year,"_",scen,".tif"),
                       filetype = "GTiff",
                       overwrite = TRUE)

    # load binary predictions from each GCM
    bins <- list()
    for (model in c("ACCESS-CM2","INM-CM5-0","MPI-ESM1-2-HR","IPSL-CM6A-LR","UKESM1-0-LL")){
      bin_name <- paste0("Eantillensis_rev3_ensemble_",model,"_",year,"_",scen,".pred.bin")
      r <- get(bin_name)
      boyce_idx <- grep("EMwmeanByBOYCE", names(r))
      bins[[model]] <- r[[boyce_idx]]
    }
    
    # terra stack of binary (0/1) predictions
    bins_stack <- rast(bins)

    # agreement: how many GCMs predict suitable (0–5)
    gcm_agreement <- sum(bins_stack, na.rm = TRUE)

    # consensus binary: suitable where majority (>=3 of 5) agree
    gcm_consensus_bin <- gcm_agreement >= 3

    # save objects
    assign(paste0("Eantillensis_rev3_GCMagreement_",year,"_",scen), gcm_agreement)
    assign(paste0("Eantillensis_rev3_GCMconsensus_",year,"_",scen), gcm_consensus_bin)

    # export
    terra::writeRaster(gcm_agreement,
                       filename = paste0("Eantillensis_rev3_GCMagreement_",year,"_",scen,".tif"),
                       filetype = "GTiff", overwrite = TRUE)
    terra::writeRaster(gcm_consensus_bin,
                       filename = paste0("Eantillensis_rev3_GCMconsensus_",year,"_",scen,".tif"),
                       filetype = "GTiff", overwrite = TRUE)
  }
}


# Repeat above steps for Emartinicensis
# Biomod Maxent
Emartinicensis_pre <- Emartinicensis_occs
Emartinicensis_pre$presence <- 1
Emartinicensis_abs <- Emartinicensis_bgs
Emartinicensis_abs$presence <- 0
Emartinicensis_res <- rbind(Emartinicensis_pre, Emartinicensis_abs)
Emartinicensis_res %>% head()
Emartinicensis_res %>% tail()

# Format the data
Emartinicensis_data <- BIOMOD_FormatingData(
  resp.var = Emartinicensis_res['presence'],
  resp.xy = Emartinicensis_res[, c('Lon', 'Lat')],
  expl.var = Emartinicensis_envs,
  resp.name = "E.martinicensis",
  #PA.nb.rep = 1,
  #PA.nb.absences = 10000, # due to the small range, all available cells were chosen as background points which was n=557; thus, only one set of 557 background points were used.
  #PA.strategy = 'random',
  #filter.raster = TRUE
)

summary(Emartinicensis_data)
#  dataset run PA Presences True_Absences Pseudo_Absences Undefined
#1 initial  NA NA        52            46               0         0

# Define model options
Emartinicensis.MAXENT <- list('_allData_allRun' = list(path_to_maxent.jar = "maxent",
                                                        linear = FALSE, quadratic = FALSE, product = FALSE, threshold = FALSE, hinge = TRUE,
                                                        betamultiplier = 1, memory_allocated = 2048))
Emartinicensis.val <- list(MAXENT.binary.MAXENT.MAXENT = Emartinicensis.MAXENT)
Emartinicensis_opt <- bm_ModelingOptions(
  data.type = 'binary',
  models = c("MAXENT"),
  strategy = 'user.defined',
  user.val = Emartinicensis.val
)

# Run model
Emartinicensis_model <- BIOMOD_Modeling(
  bm.format = Emartinicensis_data,
  models = c("MAXENT"),
  OPT.user = Emartinicensis_opt,
  CV.strategy = 'random',
  CV.nb.rep = 5,
  CV.perc = 0.8,
  var.import = 5,
  metric.eval = c('BOYCE','TSS','ROC'),
  modeling.id = "Emartinicensis_sdm"
)
saveRDS(Emartinicensis_model, "Emartinicensis_rev3_model.rds")

# Get model evaluation scores
Emartinicensis_model_scores <- get_evaluations(Emartinicensis_model)
dim(Emartinicensis_model_scores)
dimnames(Emartinicensis_model_scores)
(Emartinicensis_model_eval.scor_mean <- aggregate(data = Emartinicensis_model_scores, calibration ~ metric.eval, FUN = mean))
  # AUCroc = 0.9100000, BOYCE = 0.7000000, TSS = 0.8068333

# Plot model evaluation scores
bm_PlotEvalMean(Emartinicensis_model)

# Check variable importance
(Emartinicensis_model_var_import <- get_variables_importance(Emartinicensis_model))

# Make the mean of variable importance by algorithm
(Emartinicensis_var.imp_mean <- aggregate(data = Emartinicensis_model_var_import, var.imp ~ expl.var, FUN = mean))
  # CS1=0.30853487, CS2=0.35538563, CS3=0.09229773, CS4=0.84631913

# Model response plots
Emartinicensis_eval_plot <- 
  bm_PlotResponseCurves(
    bm.out  = Emartinicensis_model,
    new.env = get_formal_data(Emartinicensis_model,'expl.var'), 
    show.variables= get_formal_data(Emartinicensis_model,'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var = 'median',
  )

# Run the ensemble model
Emartinicensis_ensemble_model <- 
  BIOMOD_EnsembleModeling(
    bm.mod = Emartinicensis_model,
    models.chosen = 'all',
    em.by = 'all',
    em.algo = 'EMwmean',
    metric.eval = c('BOYCE','TSS','ROC'),
    metric.select = c("BOYCE"), # ROC threshold was relaxed to avoid error
    metric.select.thresh = c(0.5), 
    var.import = 5,
    nb.cpu = 120
  )
saveRDS(Emartinicensis_ensemble_model, "Emartinicensis_rev3_ensemble_model.rds")

# Assess ensemble models quality
(Emartinicensis_ensemble_model_scores <- get_evaluations(Emartinicensis_ensemble_model))
#                                              full.name merged.by.PA
#1 E.martinicensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo   mergedData
#2 E.martinicensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo   mergedData
#3 E.martinicensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo   mergedData
#  merged.by.run merged.by.algo filtered.by    algo metric.eval cutoff
#1     mergedRun     mergedAlgo       BOYCE EMwmean       BOYCE    325
#2     mergedRun     mergedAlgo       BOYCE EMwmean         TSS    317
#3     mergedRun     mergedAlgo       BOYCE EMwmean      AUCroc     99
#  sensitivity specificity calibration validation evaluation
#1      80.769      97.826       0.539         NA         NA
#2      80.769      97.826       0.786         NA         NA
#3      82.692      93.478       0.896         NA         NA

# Check variable importance
(Emartinicensis_ensemble_model_var_import <- get_variables_importance(Emartinicensis_ensemble_model))

# Make the mean of variable importance by algorithm
(Emartinicensis_ensemble_var.imp_mean <- aggregate(data = Emartinicensis_ensemble_model_var_import, var.imp ~ expl.var, FUN = mean))
  # CS1=0.2009522, CS2=0.1736818, CS3=0.0342826, CS4=0.9873454

# Model response plots
tiff("Emartinicensis_rev3_ensemble_eval_plot.tiff", width=664, height=664)
Emartinicensis_ensemble_eval_plot <- 
  bm_PlotResponseCurves(
    bm.out  = Emartinicensis_ensemble_model,
    new.env = get_formal_data(Emartinicensis_ensemble_model,'expl.var'), 
    show.variables= get_formal_data(Emartinicensis_ensemble_model,'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var = 'median',
  )
dev.off()

# Present environment projections
Emartinicensis_model_proj_present <- 
  BIOMOD_Projection(
    bm.mod = Emartinicensis_model,
    models.chosen = "all",
    new.env = Emartinicensis_envs,
    proj.name = "Emartinicensis_present",
    metric.binary = "all",
    output.format = ".tif",
    do.stack = FALSE
  )

Emartinicensis_ensemble_model_proj_present <- 
  BIOMOD_EnsembleForecasting(
    bm.em = Emartinicensis_ensemble_model,
    models.chosen = "all",
    bm.proj = Emartinicensis_model_proj_present,
    metric.binary = "all",
    output.format = ".tif",
    do.stack = FALSE,
    nb.cpu = 120
  )
plot(Emartinicensis_ensemble_model_proj_present)

Emartinicensis_ensemble_present.pred <- get_predictions(Emartinicensis_ensemble_model_proj_present)
Emartinicensis_ensemble_present.pred.bin <- get_predictions(Emartinicensis_ensemble_model_proj_present, metric.binary = "BOYCE") #This is the binary representation
plot(Emartinicensis_ensemble_present.pred)
plot(Emartinicensis_ensemble_present.pred.bin)
terra::writeRaster(Emartinicensis_ensemble_present.pred, filename = "Emartinicensis_rev3_ensemble_present.tif", filetype = "GTiff", overwrite = T)
terra::writeRaster(Emartinicensis_ensemble_present.pred.bin, filename = "Emartinicensis_rev3_ensemble_present.bin.tif", filetype = "GTiff", overwrite = T)

for (year in c("2041-2060","2081-2100")){
  for (model in c("ACCESS-CM2","INM-CM5-0","MPI-ESM1-2-HR","IPSL-CM6A-LR","UKESM1-0-LL")){
    for (scen in c("ssp126", "ssp585")) {
      envs_files.name <- paste("Emartinicensis_envs",model,year,scen, sep="_")
      assign(envs_files.name, list.files(path = paste0("martinicensis_",year,"/",model,"/",scen), pattern = "^CS[0-9]+\\.tif$", full.names=TRUE))
      envs_stack.name <- paste0("Emartinicensis_",model,"_",year,"_",scen,".stack")
      assign(envs_stack.name, stack(get(envs_files.name)))
      envs_stack <- get(envs_stack.name)
      cs_num <- as.integer(sub(".*CS([0-9]+)\\.tif$", "\\1", basename(get(envs_files.name))))
      envs_stack <- envs_stack[[order(cs_num)]]
      names(envs_stack) <- paste0("Band_1.", sort(cs_num))
      sngl.name <- paste0("Emartinicensis_rev3_model_proj_",model,"_",year,"_",scen)
      assign(sngl.name, BIOMOD_Projection(
        bm.mod = Emartinicensis_model,
        models.chosen = "all",
        new.env = envs_stack,
        proj.name = paste0("Emartinicensis_",model,"_",year,"_",scen),
        metric.binary = "all",
        output.format = ".tif",
        do.stack = FALSE
      ))
      saveRDS(get(sngl.name), paste0(sngl.name,".rds"))
      esmbl.name <- paste0("Emartinicensis_rev3_ensemble_proj_",model,"_",year,"_",scen)
      assign(esmbl.name, BIOMOD_EnsembleForecasting(
        bm.em = Emartinicensis_ensemble_model,
        models.chosen = "all",
        bm.proj = get(sngl.name),
        metric.binary = "all",
        output.format = ".tif",
        do.stack = FALSE,
        nb.cpu = 120
      ))
      saveRDS(get(esmbl.name), paste0(esmbl.name,".rds"))
      pred_file.name <- paste0(paste("Emartinicensis_rev3_ensemble",model,year,scen, sep="_"),".pred")
      assign(pred_file.name, get_predictions(get(esmbl.name)))
      pred.bin_file.name <- paste0(paste("Emartinicensis_rev3_ensemble",model,year,scen, sep="_"),".pred.bin")
      assign(pred.bin_file.name, get_predictions(get(esmbl.name), metric.binary = "BOYCE"))
    }
  }
} 

# GCM ensemble
for (year in c("2041-2060","2081-2100")){
  for (scen in c("ssp126","ssp585")){
    # load models
    preds <- list()
    for (model in c("ACCESS-CM2","INM-CM5-0","MPI-ESM1-2-HR","IPSL-CM6A-LR","UKESM1-0-LL")){
      pred_name <- paste0("Emartinicensis_rev3_ensemble_",model,"_",year,"_",scen,".pred")
      r <- get(pred_name)
      boyce_idx <- grep("EMwmeanByBOYCE", names(r))
      preds[[model]] <- r[[boyce_idx]]
    }
    
    # terra stack
    preds_stack <- rast(preds)

    # weighted mean by BOYCE
    weights <- c("ACCESS-CM2" = 0.20, "INM-CM5-0" = 0.20, "MPI-ESM1-2-HR"= 0.20, "IPSL-CM6A-LR" = 0.20, "UKESM1-0-LL"  = 0.20)
    weights <- weights / sum(weights)
    weights_vec <- weights[names(preds_stack)]
    gcm_ensemble <- weighted.mean(preds_stack, w = weights_vec, na.rm = TRUE)
    
    # save
    assign(paste0("Emartinicensis_rev3_GCMensemble_",year,"_",scen), gcm_ensemble)
    
    # export
    terra::writeRaster(gcm_ensemble,
                       filename = paste0("Emartinicensis_rev3_GCMensemble_",year,"_",scen,".tif"),
                       filetype = "GTiff",
                       overwrite = TRUE)
    
    # load binary predictions from each GCM
    bins <- list()
    for (model in c("ACCESS-CM2","INM-CM5-0","MPI-ESM1-2-HR","IPSL-CM6A-LR","UKESM1-0-LL")){
      bin_name <- paste0("Emartinicensis_rev3_ensemble_",model,"_",year,"_",scen,".pred.bin")
      r <- get(bin_name)
      boyce_idx <- grep("EMwmeanByBOYCE", names(r))
      bins[[model]] <- r[[boyce_idx]]
    }

    # terra stack of binary (0/1) predictions
    bins_stack <- rast(bins)

    # agreement: how many GCMs predict suitable (0–5)
    gcm_agreement <- sum(bins_stack, na.rm = TRUE)

    # consensus binary: suitable where majority (>=3 of 5) agree
    gcm_consensus_bin <- gcm_agreement >= 3

    # save objects
    assign(paste0("Emartinicensis_rev3_GCMagreement_",year,"_",scen), gcm_agreement)
    assign(paste0("Emartinicensis_rev3_GCMconsensus_",year,"_",scen), gcm_consensus_bin)

    # export
    terra::writeRaster(gcm_agreement,
                       filename = paste0("Emartinicensis_rev3_GCMagreement_",year,"_",scen,".tif"),
                       filetype = "GTiff", overwrite = TRUE)
    terra::writeRaster(gcm_consensus_bin,
                       filename = paste0("Emartinicensis_rev3_GCMconsensus_",year,"_",scen,".tif"),
                       filetype = "GTiff", overwrite = TRUE)
  }
}



# Plotting E. coqui
combined_land <- st_read("Eleutherodactylus_invasion_arcgis/combined_land.shp")

custom_palette <- colorRampPalette(c("#FFD966", "#00A600"))(5)
breaks  <- seq(0, 1, by = 0.2) 
breaks_bin  <- c(0, 1)                            
zlim     <- c(0, 1)

Hawaii_window <- extent(-156.2, -154.7, 18.7, 20.4)
Puerto_window <- extent(-67.5, -64, 17.5, 19)

Ecoqui_ensemble_present.pred <- raster("./E.coqui/proj_Ecoqui_present/individual_projections/E.coqui_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif")
cl_vect <- vect(combined_land)
template <- rast(cl_vect, resolution = res(Ecoqui_ensemble_present.pred))
combined_land_rast <- rasterize(cl_vect, template)
#combined_land_rast <- rasterize(combined_land[6], Ecoqui_ensemble_present.pred)
Ecoqui_scaled <- Ecoqui_ensemble_present.pred / 1000
Ecoqui_scaled[Ecoqui_scaled == 0] <- NA

Ecoqui_ensemble_present.pred.bin <- raster("./E.coqui/proj_Ecoqui_present/individual_projections/E.coqui_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo_BOYCEbin.tif")
Ecoqui_ensemble_present.pred.bin[Ecoqui_ensemble_present.pred.bin == 0] <- NA

zoom(Ecoqui_ensemble_present.pred, Hawaii_window, legend=FALSE)
plot(combined_land_rast, col = "black", legend=FALSE, asp=1, add=TRUE)
plot(Ecoqui_ensemble_present.pred, add=TRUE, legend=FALSE)

zoom(Ecoqui_ensemble_present.pred, Puerto_window, legend=FALSE)
plot(combined_land_rast, col = "black", legend=FALSE, asp=1, add=TRUE)
plot(Ecoqui_ensemble_present.pred, add=TRUE, legend=FALSE)

tiff("./E.coqui/Hawaii_present_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(Ecoqui_scaled, ext=Hawaii_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
points(Ecoqui_occs$Lon, Ecoqui_occs$Lat, col = adjustcolor("#ff00a2ff", alpha.f = 0.5), pch=1, cex=1.2)
dev.off()

tiff("./E.coqui/Hawaii_present_cont_xpoint.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(Ecoqui_scaled, ext=Hawaii_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.coqui/Puerto_present_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Puerto_window, asp=1)
plot(Ecoqui_scaled, ext=Puerto_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
points(Ecoqui_occs$Lon, Ecoqui_occs$Lat, col = adjustcolor("#ff00a2ff", alpha.f = 0.5), pch=1, cex=0.6)
dev.off()

tiff("./E.coqui/Puerto_present_cont_xpoint.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Puerto_window, asp=1)
plot(Ecoqui_scaled, ext=Puerto_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("legend.tiff", width=166, height=664)
plot.new()
legend("center", legend = c("0", "0–0.2", "0.2–0.4", "0.4–0.6", "0.6–0.8", "0.8–1.0"), fill = c("black", custom_palette), border = "black", bty = "n", title = "Suitability")
dev.off()

tiff("./E.coqui/Hawaii_present_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(Ecoqui_ensemble_present.pred.bin, ext=Hawaii_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
points(Ecoqui_occs$Lon, Ecoqui_occs$Lat, col = adjustcolor("#ff00a2ff", alpha.f = 0.5), pch=1, cex=1.2)
dev.off()

tiff("./E.coqui/Hawaii_present_bin_xpoint.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(Ecoqui_ensemble_present.pred.bin, ext=Hawaii_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.coqui/Puerto_present_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Puerto_window, asp=1)
plot(Ecoqui_ensemble_present.pred.bin, ext=Puerto_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
points(Ecoqui_occs$Lon, Ecoqui_occs$Lat, col = adjustcolor("#ff00a2ff", alpha.f = 0.5), pch=1, cex=1.2)
dev.off()

tiff("./E.coqui/Puerto_present_bin_xpoint.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Puerto_window, asp=1)
plot(Ecoqui_ensemble_present.pred.bin, ext=Puerto_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("legend_bin.tiff", width=166, height=664)
plot.new()
legend("center", legend = c("0", "1.0"), fill = c("black", "#00A600"), border = "black", bty = "n", title = "Suitability")
dev.off()

GCMcont <- rast("Ecoqui_rev3_GCMensemble_2041-2060_ssp126.tif")
GCMcont <- GCMcont / 1000                
GCMcont[GCMcont == 0] <- NA

GCMbin <- rast("Ecoqui_rev3_GCMconsensus_2041-2060_ssp126.tif")
GCMbin[GCMbin == 0] <- NA             

tiff("./E.coqui/Hawaii_2041.2060_ssp126_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(raster(GCMcont), ext=Hawaii_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.coqui/Puerto_2041.2060_ssp126_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Puerto_window, asp=1)
plot(raster(GCMcont), ext=Puerto_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.coqui/Hawaii_2041.2060_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(raster(GCMbin), ext=Hawaii_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.coqui/Puerto_2041.2060_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Puerto_window, asp=1)
plot(raster(GCMbin), ext=Puerto_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

GCMcont <- rast("Ecoqui_rev3_GCMensemble_2081-2100_ssp126.tif")
GCMcont <- GCMcont / 1000                
GCMcont[GCMcont == 0] <- NA

GCMbin <- rast("Ecoqui_rev3_GCMconsensus_2081-2100_ssp126.tif")
GCMbin[GCMbin == 0] <- NA             

tiff("./E.coqui/Hawaii_2081.2100_ssp126_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(raster(GCMcont), ext=Hawaii_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.coqui/Puerto_2081.2100_ssp126_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Puerto_window, asp=1)
plot(raster(GCMcont), ext=Puerto_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.coqui/Hawaii_2081.2100_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(raster(GCMbin), ext=Hawaii_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.coqui/Puerto_2081.2100_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Puerto_window, asp=1)
plot(raster(GCMbin), ext=Puerto_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

GCMcont <- rast("Ecoqui_rev3_GCMensemble_2041-2060_ssp585.tif")
GCMcont <- GCMcont / 1000                
GCMcont[GCMcont == 0] <- NA

GCMbin <- rast("Ecoqui_rev3_GCMconsensus_2041-2060_ssp585.tif")
GCMbin[GCMbin == 0] <- NA             

tiff("./E.coqui/Hawaii_2041.2060_ssp585_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(raster(GCMcont), ext=Hawaii_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.coqui/Puerto_2041.2060_ssp585_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Puerto_window, asp=1)
plot(raster(GCMcont), ext=Puerto_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.coqui/Hawaii_2041.2060_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(raster(GCMbin), ext=Hawaii_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.coqui/Puerto_2041.2060_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Puerto_window, asp=1)
plot(raster(GCMbin), ext=Puerto_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

GCMcont <- rast("Ecoqui_rev3_GCMensemble_2081-2100_ssp585.tif")
GCMcont <- GCMcont / 1000                
GCMcont[GCMcont == 0] <- NA

GCMbin <- rast("Ecoqui_rev3_GCMconsensus_2081-2100_ssp585.tif")
GCMbin[GCMbin == 0] <- NA             

tiff("./E.coqui/Hawaii_2081.2100_ssp585_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(raster(GCMcont), ext=Hawaii_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.coqui/Puerto_2081.2100_ssp585_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Puerto_window, asp=1)
plot(raster(GCMcont), ext=Puerto_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.coqui/Hawaii_2081.2100_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(raster(GCMbin), ext=Hawaii_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.coqui/Puerto_2081.2100_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Puerto_window, asp=1)
plot(raster(GCMbin), ext=Puerto_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()


# Plotting E. johnstonei
johnstonei_window <- extent(-80, -45, -25, 20)

Ejohnstonei_ensemble_present.pred <- raster("./E.johnstonei/proj_Ejohnstonei_present/individual_projections/E.johnstonei_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif")
#combined_land_rast <- rasterize(combined_land[6], Ejohnstonei_ensemble_present.pred)
Ejohnstonei_scaled <- Ejohnstonei_ensemble_present.pred / 1000
Ejohnstonei_scaled[Ejohnstonei_scaled == 0] <- NA

Ejohnstonei_ensemble_present.pred.bin <- raster("./E.johnstonei/proj_Ejohnstonei_present/individual_projections/E.johnstonei_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo_BOYCEbin.tif")
Ejohnstonei_ensemble_present.pred.bin[Ejohnstonei_ensemble_present.pred.bin == 0] <- NA

plot(Ejohnstonei_ensemble_present.pred, legend=FALSE)

zoom(Ejohnstonei_ensemble_present.pred, johnstonei_window, legend=FALSE)
plot(combined_land_rast, col = "black", legend=FALSE, asp=1, add=TRUE)
plot(Ejohnstonei_ensemble_present.pred, add=TRUE, legend=FALSE)

tiff("./E.johnstonei/johnstonei_present_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=johnstonei_window, asp=1)
plot(Ejohnstonei_scaled, ext=johnstonei_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
points(Ejohnstonei_occs$Lon, Ejohnstonei_occs$Lat, col = adjustcolor("#ff00a2ff", alpha.f = 0.5), pch=1, cex=0.6)
dev.off()

tiff("./E.johnstonei/johnstonei_present_cont_xpoint.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=johnstonei_window, asp=1)
plot(Ejohnstonei_scaled, ext=johnstonei_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.johnstonei/johnstonei_present_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=johnstonei_window, asp=1)
plot(Ejohnstonei_ensemble_present.pred.bin, ext=johnstonei_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
points(Ejohnstonei_occs$Lon, Ejohnstonei_occs$Lat, col = adjustcolor("#ff00a2ff", alpha.f = 0.5), pch=1, cex=1.2)
dev.off()

tiff("./E.johnstonei/johnstonei_present_bin_xpoint.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=johnstonei_window, asp=1)
plot(Ejohnstonei_ensemble_present.pred.bin, ext=johnstonei_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

GCMcont <- rast("Ejohnstonei_rev3_GCMensemble_2041-2060_ssp126.tif")
GCMcont <- GCMcont / 1000                
GCMcont[GCMcont == 0] <- NA

GCMbin <- rast("Ejohnstonei_rev3_GCMconsensus_2041-2060_ssp126.tif")
GCMbin[GCMbin == 0] <- NA             

tiff("./E.johnstonei/johnstonei_2041.2060_ssp126_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=johnstonei_window, asp=1)
plot(raster(GCMcont), ext=johnstonei_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.johnstonei/johnstonei_2041.2060_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=johnstonei_window, asp=1)
plot(raster(GCMbin), ext=johnstonei_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

GCMcont <- rast("Ejohnstonei_rev3_GCMensemble_2081-2100_ssp126.tif")
GCMcont <- GCMcont / 1000                
GCMcont[GCMcont == 0] <- NA

GCMbin <- rast("Ejohnstonei_rev3_GCMconsensus_2081-2100_ssp126.tif")
GCMbin[GCMbin == 0] <- NA             

tiff("./E.johnstonei/johnstonei_2081.2100_ssp126_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=johnstonei_window, asp=1)
plot(raster(GCMcont), ext=johnstonei_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.johnstonei/johnstonei_2081.2100_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=johnstonei_window, asp=1)
plot(raster(GCMbin), ext=johnstonei_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

GCMcont <- rast("Ejohnstonei_rev3_GCMensemble_2041-2060_ssp585.tif")
GCMcont <- GCMcont / 1000                
GCMcont[GCMcont == 0] <- NA

GCMbin <- rast("Ejohnstonei_rev3_GCMconsensus_2041-2060_ssp585.tif")
GCMbin[GCMbin == 0] <- NA             

tiff("./E.johnstonei/johnstonei_2041.2060_ssp585_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=johnstonei_window, asp=1)
plot(raster(GCMcont), ext=johnstonei_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.johnstonei/johnstonei_2041.2060_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=johnstonei_window, asp=1)
plot(raster(GCMbin), ext=johnstonei_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

GCMcont <- rast("Ejohnstonei_rev3_GCMensemble_2081-2100_ssp585.tif")
GCMcont <- GCMcont / 1000                
GCMcont[GCMcont == 0] <- NA

GCMbin <- rast("Ejohnstonei_rev3_GCMconsensus_2081-2100_ssp585.tif")
GCMbin[GCMbin == 0] <- NA             

tiff("./E.johnstonei/johnstonei_2081.2100_ssp585_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=johnstonei_window, asp=1)
plot(raster(GCMcont), ext=johnstonei_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.johnstonei/johnstonei_2081.2100_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=johnstonei_window, asp=1)
plot(raster(GCMbin), ext=johnstonei_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()


# Plotting E. planirostris
Hawaii_window <- extent(-160.5, -154.5, 18.5, 22.5)
US_window <- extent(-110, -70, 5, 40)
Asia_window <- extent(100, 130, 0, 30)

Eplanirostris_ensemble_present.pred <- raster("./E.planirostris/proj_Eplanirostris_present/individual_projections/E.planirostris_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif")
#combined_land_rast <- rasterize(combined_land[6], Eplanirostris_ensemble_present.pred)
Eplanirostris_scaled <- Eplanirostris_ensemble_present.pred / 1000
Eplanirostris_scaled[Eplanirostris_scaled == 0] <- NA

Eplanirostris_ensemble_present.pred.bin <- raster("./E.planirostris/proj_Eplanirostris_present/individual_projections/E.planirostris_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo_BOYCEbin.tif")
Eplanirostris_ensemble_present.pred.bin[Eplanirostris_ensemble_present.pred.bin == 0] <- NA

plot(Eplanirostris_ensemble_present.pred, legend=FALSE)

zoom(Eplanirostris_ensemble_present.pred, Hawaii_window, legend=FALSE)
plot(combined_land_rast, col = "black", legend=FALSE, asp=1, add=TRUE)
plot(Eplanirostris_ensemble_present.pred, add=TRUE, legend=FALSE)

zoom(Eplanirostris_ensemble_present.pred, US_window, legend=FALSE)
plot(combined_land_rast, col = "black", legend=FALSE, asp=1, add=TRUE)
plot(Eplanirostris_ensemble_present.pred, add=TRUE, legend=FALSE)

zoom(Eplanirostris_ensemble_present.pred, Asia_window, legend=FALSE)
plot(combined_land_rast, col = "black", legend=FALSE, asp=1, add=TRUE)
plot(Eplanirostris_ensemble_present.pred, add=TRUE, legend=FALSE)

tiff("./E.planirostris/Hawaii_present_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(Eplanirostris_scaled, ext=Hawaii_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
points(Eplanirostris_occs$Lon, Eplanirostris_occs$Lat, col = "#ff00a2ff", pch=1, cex=0.6)
dev.off()

tiff("./E.planirostris/Hawaii_present_cont_xpoint.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(Eplanirostris_scaled, ext=Hawaii_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.planirostris/US_present_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=US_window, asp=1)
plot(Eplanirostris_scaled, ext=US_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
points(Eplanirostris_occs$Lon, Eplanirostris_occs$Lat, col = "#ff00a2ff", pch=1, cex=0.6)
dev.off()

tiff("./E.planirostris/US_present_cont_xpoint.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=US_window, asp=1)
plot(Eplanirostris_scaled, ext=US_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.planirostris/Asia_present_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Asia_window, asp=1)
plot(Eplanirostris_scaled, ext=Asia_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
points(Eplanirostris_occs$Lon, Eplanirostris_occs$Lat, col = "#ff00a2ff", pch=1, cex=0.6)
dev.off()

tiff("./E.planirostris/Asia_present_cont_xpoint.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Asia_window, asp=1)
plot(Eplanirostris_scaled, ext=Asia_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.planirostris/Hawaii_present_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(Eplanirostris_ensemble_present.pred.bin, ext=Hawaii_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
points(Eplanirostris_occs$Lon, Eplanirostris_occs$Lat, col = adjustcolor("#ff00a2ff", alpha.f = 0.5), pch=1, cex=1.2)
dev.off()

tiff("./E.planirostris/Hawaii_present_bin_xpoint.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(Eplanirostris_ensemble_present.pred.bin, ext=Hawaii_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.planirostris/US_present_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=US_window, asp=1)
plot(Eplanirostris_ensemble_present.pred.bin, ext=US_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
points(Eplanirostris_occs$Lon, Eplanirostris_occs$Lat, col = adjustcolor("#ff00a2ff", alpha.f = 0.5), pch=1, cex=1.2)
dev.off()

tiff("./E.planirostris/US_present_bin_xpoint.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=US_window, asp=1)
plot(Eplanirostris_ensemble_present.pred.bin, ext=US_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.planirostris/Asia_present_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Asia_window, asp=1)
plot(Eplanirostris_ensemble_present.pred.bin, ext=Asia_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
points(Eplanirostris_occs$Lon, Eplanirostris_occs$Lat, col = adjustcolor("#ff00a2ff", alpha.f = 0.5), pch=1, cex=1.2)
dev.off()

tiff("./E.planirostris/Asia_present_bin_xpoint.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Asia_window, asp=1)
plot(Eplanirostris_ensemble_present.pred.bin, ext=Asia_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

GCMcont <- rast("Eplanirostris_rev3_GCMensemble_2041-2060_ssp126.tif")
GCMcont <- GCMcont / 1000                
GCMcont[GCMcont == 0] <- NA

GCMbin <- rast("Eplanirostris_rev3_GCMconsensus_2041-2060_ssp126.tif")
GCMbin[GCMbin == 0] <- NA             

tiff("./E.planirostris/Hawaii_2041.2060_ssp126_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(raster(GCMcont), ext=Hawaii_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.planirostris/US_2041.2060_ssp126_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=US_window, asp=1)
plot(raster(GCMcont), ext=US_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.planirostris/Asia_2041.2060_ssp126_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Asia_window, asp=1)
plot(raster(GCMcont), ext=Asia_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.planirostris/Hawaii_2041.2060_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(raster(GCMbin), ext=Hawaii_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.planirostris/US_2041.2060_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=US_window, asp=1)
plot(raster(GCMbin), ext=US_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.planirostris/Asia_2041.2060_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Asia_window, asp=1)
plot(raster(GCMbin), ext=Asia_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

GCMcont <- rast("Eplanirostris_rev3_GCMensemble_2081-2100_ssp126.tif")
GCMcont <- GCMcont / 1000                
GCMcont[GCMcont == 0] <- NA

GCMbin <- rast("Eplanirostris_rev3_GCMconsensus_2081-2100_ssp126.tif")
GCMbin[GCMbin == 0] <- NA             

tiff("./E.planirostris/Hawaii_2081.2100_ssp126_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(raster(GCMcont), ext=Hawaii_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.planirostris/US_2081.2100_ssp126_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=US_window, asp=1)
plot(raster(GCMcont), ext=US_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.planirostris/Asia_2081.2100_ssp126_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Asia_window, asp=1)
plot(raster(GCMcont), ext=Asia_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.planirostris/Hawaii_2081.2100_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(raster(GCMbin), ext=Hawaii_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.planirostris/US_2081.2100_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=US_window, asp=1)
plot(raster(GCMbin), ext=US_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.planirostris/Asia_2081.2100_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Asia_window, asp=1)
plot(raster(GCMbin), ext=Asia_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

GCMcont <- rast("Eplanirostris_rev3_GCMensemble_2041-2060_ssp585.tif")
GCMcont <- GCMcont / 1000                
GCMcont[GCMcont == 0] <- NA

GCMbin <- rast("Eplanirostris_rev3_GCMconsensus_2041-2060_ssp585.tif")
GCMbin[GCMbin == 0] <- NA             

tiff("./E.planirostris/Hawaii_2041.2060_ssp585_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(raster(GCMcont), ext=Hawaii_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.planirostris/US_2041.2060_ssp585_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=US_window, asp=1)
plot(raster(GCMcont), ext=US_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.planirostris/Asia_2041.2060_ssp585_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Asia_window, asp=1)
plot(raster(GCMcont), ext=Asia_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.planirostris/Hawaii_2041.2060_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(raster(GCMbin), ext=Hawaii_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.planirostris/US_2041.2060_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=US_window, asp=1)
plot(raster(GCMbin), ext=US_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.planirostris/Asia_2041.2060_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Asia_window, asp=1)
plot(raster(GCMbin), ext=Asia_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

GCMcont <- rast("Eplanirostris_rev3_GCMensemble_2081-2100_ssp585.tif")
GCMcont <- GCMcont / 1000                
GCMcont[GCMcont == 0] <- NA

GCMbin <- rast("Eplanirostris_rev3_GCMconsensus_2081-2100_ssp585.tif")
GCMbin[GCMbin == 0] <- NA             

tiff("./E.planirostris/Hawaii_2081.2100_ssp585_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(raster(GCMcont), ext=Hawaii_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.planirostris/US_2081.2100_ssp585_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=US_window, asp=1)
plot(raster(GCMcont), ext=US_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.planirostris/Asia_2081.2100_ssp585_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Asia_window, asp=1)
plot(raster(GCMcont), ext=Asia_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.planirostris/Hawaii_2081.2100_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(raster(GCMbin), ext=Hawaii_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.planirostris/US_2081.2100_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=US_window, asp=1)
plot(raster(GCMbin), ext=US_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.planirostris/Asia_2081.2100_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Asia_window, asp=1)
plot(raster(GCMbin), ext=Asia_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()


# Plotting E. antillensis
antillensis_window <- extent(-67.5, -64, 17.5, 19)

Eantillensis_ensemble_present.pred <- raster("./E.antillensis/proj_Eantillensis_present/individual_projections/E.antillensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif")
#combined_land_rast <- rasterize(combined_land[6], Eantillensis_ensemble_present.pred)
Eantillensis_scaled <- Eantillensis_ensemble_present.pred / 1000
Eantillensis_scaled[Eantillensis_scaled == 0] <- NA

Eantillensis_ensemble_present.pred.bin <- raster("./E.antillensis/proj_Eantillensis_present/individual_projections/E.antillensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo_BOYCEbin.tif")
Eantillensis_ensemble_present.pred.bin[Eantillensis_ensemble_present.pred.bin == 0] <- NA

plot(Eantillensis_ensemble_present.pred, legend=FALSE)
plot(combined_land_rast, col = "black", legend=FALSE, asp=1, add=TRUE)
plot(Eantillensis_ensemble_present.pred, add=TRUE, legend=FALSE)

tiff("./E.antillensis/antillensis_present_cont.tiff", width=664, height=664)
plot(combined_land_rast, ext = antillensis_window, col = "black", legend=FALSE, asp=1)
plot(Eantillensis_scaled, ext = antillensis_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
points(Eantillensis_occs$Lon, Eantillensis_occs$Lat, col = adjustcolor("#ff00a2ff", alpha.f = 0.5), pch=1, cex=0.6)
dev.off()

tiff("./E.antillensis/antillensis_present_cont_xpoint.tiff", width=664, height=664)
plot(combined_land_rast, ext = antillensis_window, col = "black", legend=FALSE, asp=1)
plot(Eantillensis_scaled, ext = antillensis_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.antillensis/antillensis_present_bin.tiff", width=664, height=664)
plot(combined_land_rast, ext = antillensis_window, col = "black", legend=FALSE, asp=1)
plot(Eantillensis_ensemble_present.pred.bin, ext = antillensis_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
points(Eantillensis_occs$Lon, Eantillensis_occs$Lat, col = adjustcolor("#ff00a2ff", alpha.f = 0.5), pch=1, cex=1.2)
dev.off()

tiff("./E.antillensis/antillensis_present_bin_xpoint.tiff", width=664, height=664)
plot(combined_land_rast, ext = antillensis_window, col = "black", legend=FALSE, asp=1)
plot(Eantillensis_ensemble_present.pred.bin, ext = antillensis_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

GCMcont <- rast("Eantillensis_rev3_GCMensemble_2041-2060_ssp126.tif")
GCMcont <- GCMcont / 1000                
GCMcont[GCMcont == 0] <- NA

GCMbin <- rast("Eantillensis_rev3_GCMconsensus_2041-2060_ssp126.tif")
GCMbin[GCMbin == 0] <- NA             

tiff("./E.antillensis/antillensis_2041.2060_ssp126_cont.tiff", width=664, height=664)
plot(combined_land_rast, ext = antillensis_window, col = "black", legend=FALSE, asp=1)
plot(raster(GCMcont), ext = antillensis_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.antillensis/antillensis_2041.2060_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, ext = antillensis_window, col = "black", legend=FALSE, asp=1)
plot(raster(GCMbin), ext = antillensis_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

GCMcont <- rast("Eantillensis_rev3_GCMensemble_2081-2100_ssp126.tif")
GCMcont <- GCMcont / 1000                
GCMcont[GCMcont == 0] <- NA

GCMbin <- rast("Eantillensis_rev3_GCMconsensus_2081-2100_ssp126.tif")
GCMbin[GCMbin == 0] <- NA             

tiff("./E.antillensis/antillensis_2081.2100_ssp126_cont.tiff", width=664, height=664)
plot(combined_land_rast, ext = antillensis_window, col = "black", legend=FALSE, asp=1)
plot(raster(GCMcont), ext = antillensis_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.antillensis/antillensis_2081.2100_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, ext = antillensis_window, col = "black", legend=FALSE, asp=1)
plot(raster(GCMbin), ext = antillensis_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

GCMcont <- rast("Eantillensis_rev3_GCMensemble_2041-2060_ssp585.tif")
GCMcont <- GCMcont / 1000                
GCMcont[GCMcont == 0] <- NA

GCMbin <- rast("Eantillensis_rev3_GCMconsensus_2041-2060_ssp585.tif")
GCMbin[GCMbin == 0] <- NA             

tiff("./E.antillensis/antillensis_2041.2060_ssp585_cont.tiff", width=664, height=664)
plot(combined_land_rast, ext = antillensis_window, col = "black", legend=FALSE, asp=1)
plot(raster(GCMcont), ext = antillensis_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.antillensis/antillensis_2041.2060_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, ext = antillensis_window, col = "black", legend=FALSE, asp=1)
plot(raster(GCMbin), ext = antillensis_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

GCMcont <- rast("Eantillensis_rev3_GCMensemble_2081-2100_ssp585.tif")
GCMcont <- GCMcont / 1000                
GCMcont[GCMcont == 0] <- NA

GCMbin <- rast("Eantillensis_rev3_GCMconsensus_2081-2100_ssp585.tif")
GCMbin[GCMbin == 0] <- NA             

tiff("./E.antillensis/antillensis_2081.2100_ssp585_cont.tiff", width=664, height=664)
plot(combined_land_rast, ext = antillensis_window, col = "black", legend=FALSE, asp=1)
plot(raster(GCMcont), ext = antillensis_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.antillensis/antillensis_2081.2100_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, ext = antillensis_window, col = "black", legend=FALSE, asp=1)
plot(raster(GCMbin), ext = antillensis_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()


# Plotting E. martinicensis
Jamaica_window <- extent(-78.75, -76, 17.5, 18.75) 
Antilles_window <- extent(-63, -60, 12, 18) 

Emartinicensis_ensemble_present.pred <- raster("./E.martinicensis/proj_Emartinicensis_present/individual_projections/E.martinicensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif")
#combined_land_rast <- rasterize(combined_land[6], Emartinicensis_ensemble_present.pred)
Emartinicensis_scaled <- Emartinicensis_ensemble_present.pred / 1000
Emartinicensis_scaled[Emartinicensis_scaled == 0] <- NA

Emartinicensis_ensemble_present.pred.bin <- raster("./E.martinicensis/proj_Emartinicensis_present/individual_projections/E.martinicensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo_BOYCEbin.tif")
Emartinicensis_ensemble_present.pred.bin[Emartinicensis_ensemble_present.pred.bin == 0] <- NA

#buffer_1km <- shapefile("martinicensis_rev3_1km_onLand.shp")   # raster::shapefile → Spatial*
#buffer_1km <- spTransform(buffer_1km, crs(Emartinicensis_scaled))
#scaled_1km <- mask(crop(Emartinicensis_scaled, buffer_1km), buffer_1km)
#bin_1km <- mask(crop(Emartinicensis_ensemble_present.pred.bin, buffer_1km), buffer_1km)

zoom(Emartinicensis_ensemble_present.pred, Jamaica_window, legend=FALSE)
plot(combined_land_rast, col = "black", legend=FALSE, asp=1, add=TRUE)
plot(Emartinicensis_ensemble_present.pred, add=TRUE, legend=FALSE)

zoom(Emartinicensis_ensemble_present.pred, Antilles_window, legend=FALSE)
plot(combined_land_rast, col = "black", legend=FALSE, asp=1, add=TRUE)
plot(Emartinicensis_ensemble_present.pred, add=TRUE, legend=FALSE)

tiff("./E.martinicensis/Jamaica_present_cont.tiff", width=664, height=664)
plot(combined_land_rast, ext=Jamaica_window, col = "black", legend=FALSE, asp=1)
plot(Emartinicensis_scaled, ext=Jamaica_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
points(Emartinicensis_occs$Lon, Emartinicensis_occs$Lat, col = adjustcolor("#ff00a2ff", alpha.f = 1), pch=1, cex=1.2)
dev.off()

tiff("./E.martinicensis/Jamaica_present_cont_xpoint.tiff", width=664, height=664)
plot(combined_land_rast, ext=Jamaica_window, col = "black", legend=FALSE, asp=1)
plot(Emartinicensis_scaled, ext=Jamaica_window, ccol = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.martinicensis/Antilles_present_cont.tiff", width=664, height=664)
plot(combined_land_rast, ext=Antilles_window, col = "black", legend=FALSE, asp=1)
plot(Emartinicensis_scaled, ext=Antilles_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
points(Emartinicensis_occs$Lon, Emartinicensis_occs$Lat, col = adjustcolor("#ff00a2ff", alpha.f = 1), pch=1, cex=0.6)
dev.off()

tiff("./E.martinicensis/Antilles_present_cont_xpoint.tiff", width=664, height=664)
plot(combined_land_rast, ext=Antilles_window, col = "black", legend=FALSE, asp=1)
plot(Emartinicensis_scaled, ext=Antilles_window, ccol = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.martinicensis/Jamaica_present_bin.tiff", width=664, height=664)
plot(combined_land_rast, ext=Jamaica_window, col = "black", legend=FALSE, asp=1)
plot(Emartinicensis_ensemble_present.pred.bin, ext=Jamaica_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
points(Emartinicensis_occs$Lon, Emartinicensis_occs$Lat, col = adjustcolor("#ff00a2ff", alpha.f = 0.5), pch=1, cex=1.2)
dev.off()

tiff("./E.martinicensis/Jamaica_present_bin_xpoint.tiff", width=664, height=664)
plot(combined_land_rast, ext=Jamaica_window, col = "black", legend=FALSE, asp=1)
plot(Emartinicensis_ensemble_present.pred.bin, ext=Jamaica_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.martinicensis/Antilles_present_bin.tiff", width=664, height=664)
plot(combined_land_rast, ext=Antilles_window, col = "black", legend=FALSE, asp=1)
plot(Emartinicensis_ensemble_present.pred.bin, ext=Antilles_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
points(Emartinicensis_occs$Lon, Emartinicensis_occs$Lat, col = adjustcolor("#ff00a2ff", alpha.f = 0.5), pch=1, cex=1.2)
dev.off()

tiff("./E.martinicensis/Antilles_present_bin_xpoint.tiff", width=664, height=664)
plot(combined_land_rast, ext=Antilles_window, col = "black", legend=FALSE, asp=1)
plot(Emartinicensis_ensemble_present.pred.bin, ext=Antilles_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

GCMcont <- rast("Emartinicensis_rev3_GCMensemble_2041-2060_ssp126.tif")
GCMcont <- GCMcont / 1000                
GCMcont[GCMcont == 0] <- NA

GCMbin <- rast("Emartinicensis_rev3_GCMconsensus_2041-2060_ssp126.tif")
GCMbin[GCMbin == 0] <- NA             

tiff("./E.martinicensis/Jamaica_2041.2060_ssp126_cont.tiff", width=664, height=664)
plot(combined_land_rast, ext=Jamaica_window, col = "black", legend=FALSE, asp=1)
plot(raster(GCMcont), ext=Jamaica_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.martinicensis/Antilles_2041.2060_ssp126_cont.tiff", width=664, height=664)
plot(combined_land_rast, ext=Antilles_window, col = "black", legend=FALSE, asp=1)
plot(raster(GCMcont), ext=Antilles_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.martinicensis/Jamaica_2041.2060_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, ext=Jamaica_window, col = "black", legend=FALSE, asp=1)
plot(raster(GCMbin), ext=Jamaica_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.martinicensis/Antilles_2041.2060_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, ext=Antilles_window, col = "black", legend=FALSE, asp=1)
plot(raster(GCMbin), ext=Antilles_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

GCMcont <- rast("Emartinicensis_rev3_GCMensemble_2081-2100_ssp126.tif")
GCMcont <- GCMcont / 1000                
GCMcont[GCMcont == 0] <- NA

GCMbin <- rast("Emartinicensis_rev3_GCMconsensus_2081-2100_ssp126.tif")
GCMbin[GCMbin == 0] <- NA             

tiff("./E.martinicensis/Jamaica_2081.2100_ssp126_cont.tiff", width=664, height=664)
plot(combined_land_rast, ext=Jamaica_window, col = "black", legend=FALSE, asp=1)
plot(raster(GCMcont), ext=Jamaica_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.martinicensis/Antilles_2081.2100_ssp126_cont.tiff", width=664, height=664)
plot(combined_land_rast, ext=Antilles_window, col = "black", legend=FALSE, asp=1)
plot(raster(GCMcont), ext=Antilles_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.martinicensis/Jamaica_2081.2100_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, ext=Jamaica_window, col = "black", legend=FALSE, asp=1)
plot(raster(GCMbin), ext=Jamaica_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.martinicensis/Antilles_2081.2100_ssp126_bin.tiff", width=664, height=664)
plot(combined_land_rast, ext=Antilles_window, col = "black", legend=FALSE, asp=1)
plot(raster(GCMbin), ext=Antilles_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

GCMcont <- rast("Emartinicensis_rev3_GCMensemble_2041-2060_ssp585.tif")
GCMcont <- GCMcont / 1000                
GCMcont[GCMcont == 0] <- NA

GCMbin <- rast("Emartinicensis_rev3_GCMconsensus_2041-2060_ssp585.tif")
GCMbin[GCMbin == 0] <- NA             

tiff("./E.martinicensis/Jamaica_2041.2060_ssp585_cont.tiff", width=664, height=664)
plot(combined_land_rast, ext=Jamaica_window, col = "black", legend=FALSE, asp=1)
plot(raster(GCMcont), ext=Jamaica_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.martinicensis/Antilles_2041.2060_ssp585_cont.tiff", width=664, height=664)
plot(combined_land_rast, ext=Antilles_window, col = "black", legend=FALSE, asp=1)
plot(raster(GCMcont), ext=Antilles_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.martinicensis/Jamaica_2041.2060_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, ext=Jamaica_window, col = "black", legend=FALSE, asp=1)
plot(raster(GCMbin), ext=Jamaica_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.martinicensis/Antilles_2041.2060_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, ext=Antilles_window, col = "black", legend=FALSE, asp=1)
plot(raster(GCMbin), ext=Antilles_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

GCMcont <- rast("Emartinicensis_rev3_GCMensemble_2081-2100_ssp585.tif")
GCMcont <- GCMcont / 1000                
GCMcont[GCMcont == 0] <- NA

GCMbin <- rast("Emartinicensis_rev3_GCMconsensus_2081-2100_ssp585.tif")
GCMbin[GCMbin == 0] <- NA             

tiff("./E.martinicensis/Jamaica_2081.2100_ssp585_cont.tiff", width=664, height=664)
plot(combined_land_rast, ext=Jamaica_window, col = "black", legend=FALSE, asp=1)
plot(raster(GCMcont), ext=Jamaica_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.martinicensis/Antilles_2081.2100_ssp585_cont.tiff", width=664, height=664)
plot(combined_land_rast, ext=Antilles_window, col = "black", legend=FALSE, asp=1)
plot(raster(GCMcont), ext=Antilles_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.martinicensis/Jamaica_2081.2100_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, ext=Jamaica_window, col = "black", legend=FALSE, asp=1)
plot(raster(GCMbin), ext=Jamaica_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.martinicensis/Antilles_2081.2100_ssp585_bin.tiff", width=664, height=664)
plot(combined_land_rast, ext=Antilles_window, col = "black", legend=FALSE, asp=1)
plot(raster(GCMbin), ext=Antilles_window, col = "#00A600", breaks = breaks_bin, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()



# ExDet analysis
library(flexsdm)
library(dplyr)
library(terra)

# E. coqui
Hawaii_window <- ext(-156.2, -154.7, 18.7, 20.4)
Puerto_window <- ext(-67.5, -64, 17.5, 19)

# calibration environment = occurences + backgrounds
band_cols <- grep("^Band_1\\.[0-9]+$", names(Ecoqui_bgs.z), value = TRUE)
band_num  <- as.integer(sub("^Band_1\\.([0-9]+)$", "\\1", band_cols))
cs_names  <- paste0("CS", band_num)      # Band_1.1 -> CS1
cs_cols   <- cs_names

calib_pres <- cbind(Ecoqui_occs.z[, c("Lon","Lat")], Ecoqui_occs.z[, band_cols, drop = FALSE])
calib_bg   <- cbind(Ecoqui_bgs.z[,  c("Lon","Lat")], Ecoqui_bgs.z[,  band_cols, drop = FALSE])
names(calib_pres) <- c("x", "y", cs_names)
names(calib_bg)   <- c("x", "y", cs_names)

calib_pres$pr_ab <- 1
calib_bg$pr_ab   <- 0

calib_data <- bind_rows(calib_pres, calib_bg)
calib_data <- calib_data[complete.cases(calib_data), ] # remove NAs
calib_env  <- calib_data[, c(cs_cols, "pr_ab")] 

# present environment
present_rast <- rast(Ecoqui_envs.files)              
cs_n <- as.integer(sub(".*CS([0-9]+)\\.tif$","\\1", basename(Ecoqui_envs.files)))
present_rast <- present_rast[[order(cs_n)]]
names(present_rast) <- paste0("CS", sort(cs_n))

# # check: Band_1.1 -> CS1
chk <- terra::extract(present_rast, Ecoqui_occs.z[, c("Lon","Lat")])
head(data.frame(
  CS1_from_raster = chk$CS1,
  Band_1.1_in_occs = Ecoqui_occs.z$`Band_1.1`
))

# spatial extrapolation (calibration points vs. present projection area)
xp_spatial <- extra_eval(
  training_data = calib_env,      
  pr_ab         = "pr_ab",         
  projection_data = present_rast, 
  metric        = "mahalanobis",  
  univar_comb   = TRUE,           # NT1(i.e., variable(s) of projection data is outside the range of training conditions) + NT2(i.e., variables of projection data are within the range of training conditions but with new combination of variables)
  aggreg_factor = 1
)
# novelty raster (the higher the values, the more deviated from calibration)
writeRaster(xp_spatial$extrapolation, "Ecoqui_spatial_extrapolation_shape.tif", overwrite=TRUE)
writeRaster(xp_spatial$uni_comb,      "Ecoqui_spatial_extrapolation_unicomb.tif", overwrite=TRUE)

# plotting
pdf("Ecoqui_pextra_spatial.pdf", width = 20, height = 10)
print(
  p_extra(training_data = calib_data, x = "x", y = "y", pr_ab = "pr_ab",
          extra_suit_data = xp_spatial, projection_data = present_rast,
          geo_space = TRUE, prop_points = 0.05)
)
dev.off()

cl <- c("#FDE725","#B3DC2B","#6DCC57","#36B677","#1F9D87",
        "#25818E","#30678D","#3D4988","#462777","#440154")
pdf("Ecoqui_spatial_extrapolation.pdf", width = 11, height = 5)
par(mfrow = c(1,2))
plot(xp_spatial$extrapolation, ext = Hawaii_window, main = "Shape metric (Hawaii)", col = cl)
plot(xp_spatial$uni_comb,     ext = Hawaii_window, main = "Univariate & Combinatorial extrapolation (Hawaii)", col = cl)
plot(xp_spatial$extrapolation, ext = Puerto_window, main = "Shape metric (Puerto Rico)", col = cl)
plot(xp_spatial$uni_comb,     ext = Puerto_window, main = "Univariate & Combinatorial extrapolation (Puerto Rico)", col = cl)
par(mfrow = c(1,1))
dev.off()

# uni_comb: 1 = univariate, 2 = combinatorial, NA = non-extrapolation (analog)
uc <- xp_spatial$uni_comb

n_uni  <- global(uc == 1, "sum", na.rm = TRUE)[[1]]   # univariate cell count
n_comb <- global(uc == 2, "sum", na.rm = TRUE)[[1]]   # combinatorial cell count
total_proj <- global(!is.na(xp_spatial$extrapolation), "sum", na.rm = TRUE)[[1]]  # total cell count
n_analog <- total_proj - (n_uni + n_comb) # non-extrapolation cell count

extrap_tab <- data.frame(
  species        = "E.coqui",
  total_cells       = total_proj,
  univariate     = n_uni,
  combinatorial  = n_comb,
  analog            = n_analog,                            # non-extrapolation
  pct_univariate    = round(100 * n_uni    / total_proj, 2),  
  pct_combinatorial = round(100 * n_comb   / total_proj, 2),
  pct_extrapolated  = round(100 * (n_uni + n_comb) / total_proj, 2),
  median_shape = round(global(xp_spatial$extrapolation, fun = median, na.rm=TRUE)[[1]], 1),
  mean_shape   = round(global(xp_spatial$extrapolation, "mean",   na.rm=TRUE)[[1]], 1),
  min_shape    = round(global(xp_spatial$extrapolation, "min",   na.rm=TRUE)[[1]], 1),
  max_shape    = round(global(xp_spatial$extrapolation, "max",    na.rm=TRUE)[[1]], 1)  
)
extrap_tab
write.csv(extrap_tab, "Ecoqui_spatial_extrapolation_summary.csv", row.names = FALSE)

# temporal extrapolation (present projection vs. future projection)
present_df <- as.data.frame(present_rast, na.rm = TRUE) %>% mutate(pr_ab = 0)

temporal_summary <- list()

for (year in c("2041-2060","2081-2100")){
  for (scen in c("ssp126","ssp585")){
    for (model in c("ACCESS-CM2","INM-CM5-0","MPI-ESM1-2-HR","IPSL-CM6A-LR","UKESM1-0-LL")){
      fp <- list.files(paste0("coqui_",year,"/",model,"/",scen),
                       pattern = "^CS[0-9]+\\.tif$", full.names = TRUE)
      n  <- as.integer(sub(".*CS([0-9]+)\\.tif$","\\1", basename(fp)))
      fut_rast <- rast(fp[order(n)])
      names(fut_rast) <- paste0("CS", sort(n))

      xp_temporal <- extra_eval(
        training_data   = present_df,       # reference - present
        pr_ab           = "pr_ab",
        projection_data = fut_rast,          # projection - future
        metric          = "mahalanobis",
        univar_comb     = TRUE,
        aggreg_factor   = 1
      )
      writeRaster(xp_temporal,
                  paste0("Ecoqui_temporal_extrapolation_",model,"_",year,"_",scen,".tif"),
                  overwrite = TRUE)

      pdf(paste0("Ecoqui_temporal_extrapolation_",model,"_",year,"_",scen,".pdf"),
          width = 12, height = 12)
      par(mfrow = c(2,2))
      plot(xp_temporal$extrapolation, ext = Hawaii_window,
           main = paste("Shape metric (Hawaii)", year, scen, model), col = cl)
      plot(xp_temporal$uni_comb, ext = Hawaii_window,
           main = "Univariate & Combinatorial extrapolation (Hawaii)", col = cl)
      plot(xp_temporal$extrapolation, ext = Puerto_window,
           main = paste("Shape metric (Puerto Rico)", year, scen, model), col = cl)
      plot(xp_temporal$uni_comb, ext = Puerto_window,
           main = "Univariate & Combinatorial extrapolation (Puerto Rico)", col = cl)
      par(mfrow = c(1,1))
      dev.off()

      uc <- xp_temporal$uni_comb
      n_uni  <- global(uc == 1, "sum", na.rm = TRUE)[[1]]
      n_comb <- global(uc == 2, "sum", na.rm = TRUE)[[1]]
      total_proj <- global(!is.na(xp_temporal$extrapolation), "sum", na.rm = TRUE)[[1]]
      n_analog <- total_proj - (n_uni + n_comb)

      temporal_summary[[paste(year, scen, model)]] <- data.frame(
        species           = "E.coqui",
        year              = year,
        scenario          = scen,
        gcm               = model,
        total_cells       = total_proj,
        univariate        = n_uni,
        combinatorial     = n_comb,
        analog            = n_analog,
        pct_univariate    = round(100 * n_uni  / total_proj, 2),
        pct_combinatorial = round(100 * n_comb / total_proj, 2),
        pct_extrapolated  = round(100 * (n_uni + n_comb) / total_proj, 2),
        median_shape = round(global(xp_temporal$extrapolation, fun = median, na.rm=TRUE)[[1]], 1),
        mean_shape   = round(global(xp_temporal$extrapolation, "mean",   na.rm=TRUE)[[1]], 1),
        min_shape    = round(global(xp_temporal$extrapolation, "min",   na.rm=TRUE)[[1]], 1),
        max_shape    = round(global(xp_temporal$extrapolation, "max",    na.rm=TRUE)[[1]], 1)
      )
    }
  }
}

temporal_tab <- do.call(rbind, temporal_summary)
rownames(temporal_tab) <- NULL
write.csv(temporal_tab, "Ecoqui_temporal_extrapolation_summary.csv", row.names = FALSE)


# E. johnstonei
johnstonei_window <- ext(-80, -45, -25, 20)

# calibration environment = occurences + backgrounds
band_cols <- grep("^Band_1\\.[0-9]+$", names(Ejohnstonei_bgs.z), value = TRUE)
band_num  <- as.integer(sub("^Band_1\\.([0-9]+)$", "\\1", band_cols))
cs_names  <- paste0("CS", band_num)      # Band_1.1 -> CS1
cs_cols   <- cs_names

calib_pres <- cbind(Ejohnstonei_occs.z[, c("Lon","Lat")], Ejohnstonei_occs.z[, band_cols, drop = FALSE])
calib_bg   <- cbind(Ejohnstonei_bgs.z[,  c("Lon","Lat")], Ejohnstonei_bgs.z[,  band_cols, drop = FALSE])
names(calib_pres) <- c("x", "y", cs_names)
names(calib_bg)   <- c("x", "y", cs_names)

calib_pres$pr_ab <- 1
calib_bg$pr_ab   <- 0

calib_data <- bind_rows(calib_pres, calib_bg)
calib_data <- calib_data[complete.cases(calib_data), ] # remove NAs
calib_env  <- calib_data[, c(cs_cols, "pr_ab")] 

# present environment
present_rast <- rast(Ejohnstonei_envs.files)              
cs_n <- as.integer(sub(".*CS([0-9]+)\\.tif$","\\1", basename(Ejohnstonei_envs.files)))
present_rast <- present_rast[[order(cs_n)]]
names(present_rast) <- paste0("CS", sort(cs_n))

# # check: Band_1.1 -> CS1
chk <- terra::extract(present_rast, Ejohnstonei_occs.z[, c("Lon","Lat")])
head(data.frame(
  CS1_from_raster = chk$CS1,
  Band_1.1_in_occs = Ejohnstonei_occs.z$`Band_1.1`
))

# spatial extrapolation (calibration points vs. present projection area)
xp_spatial <- extra_eval(
  training_data = calib_env,      
  pr_ab         = "pr_ab",         
  projection_data = present_rast, 
  metric        = "mahalanobis",  
  univar_comb   = TRUE,           # NT1(i.e., variable(s) of projection data is outside the range of training conditions) + NT2(i.e., variables of projection data are within the range of training conditions but with new combination of variables)
  aggreg_factor = 1
)
# novelty raster (the higher the values, the more deviated from calibration)
writeRaster(xp_spatial$extrapolation, "Ejohnstonei_spatial_extrapolation_shape.tif", overwrite=TRUE)
writeRaster(xp_spatial$uni_comb,      "Ejohnstonei_spatial_extrapolation_unicomb.tif", overwrite=TRUE)

# plotting
pdf("Ejohnstonei_pextra_spatial.pdf", width = 20, height = 10)
print(
  p_extra(training_data = calib_data, x = "x", y = "y", pr_ab = "pr_ab",
          extra_suit_data = xp_spatial, projection_data = present_rast,
          geo_space = TRUE, prop_points = 0.05)
)
dev.off()

cl <- c("#FDE725","#B3DC2B","#6DCC57","#36B677","#1F9D87",
        "#25818E","#30678D","#3D4988","#462777","#440154")
pdf("Ejohnstonei_spatial_extrapolation.pdf", width = 11, height = 5)
par(mfrow = c(1,2))
plot(xp_spatial$extrapolation, ext = johnstonei_window, main = "Shape metric (johnstonei)", col = cl)
plot(xp_spatial$uni_comb,      ext = johnstonei_window, main = "Univariate & Combinatorial extrapolation (johnstonei)", col = cl)
par(mfrow = c(1,1))
dev.off()

# uni_comb: 1 = univariate, 2 = combinatorial, NA = non-extrapolation (analog)
uc <- xp_spatial$uni_comb

n_uni  <- global(uc == 1, "sum", na.rm = TRUE)[[1]]   # univariate cell count
n_comb <- global(uc == 2, "sum", na.rm = TRUE)[[1]]   # combinatorial cell count
total_proj <- global(!is.na(xp_spatial$extrapolation), "sum", na.rm = TRUE)[[1]]  # total cell count
n_analog <- total_proj - (n_uni + n_comb) # non-extrapolation cell count

extrap_tab <- data.frame(
  species        = "E.johnstonei",
  total_cells       = total_proj,
  univariate     = n_uni,
  combinatorial  = n_comb,
  analog            = n_analog,                            # non-extrapolation
  pct_univariate    = round(100 * n_uni    / total_proj, 2),  
  pct_combinatorial = round(100 * n_comb   / total_proj, 2),
  pct_extrapolated  = round(100 * (n_uni + n_comb) / total_proj, 2),  
  median_shape = round(global(xp_spatial$extrapolation, fun = median, na.rm=TRUE)[[1]], 1),
  mean_shape   = round(global(xp_spatial$extrapolation, "mean",   na.rm=TRUE)[[1]], 1),
  min_shape    = round(global(xp_spatial$extrapolation, "min",   na.rm=TRUE)[[1]], 1),
  max_shape    = round(global(xp_spatial$extrapolation, "max",    na.rm=TRUE)[[1]], 1)
)
extrap_tab
write.csv(extrap_tab, "Ejohnstonei_spatial_extrapolation_summary.csv", row.names = FALSE)

# temporal extrapolation (present projection vs. future projection)
present_df <- as.data.frame(present_rast, na.rm = TRUE) %>% mutate(pr_ab = 0)

temporal_summary <- list()

for (year in c("2041-2060","2081-2100")){
  for (scen in c("ssp126","ssp585")){
    for (model in c("ACCESS-CM2","INM-CM5-0","MPI-ESM1-2-HR","IPSL-CM6A-LR","UKESM1-0-LL")){
      fp <- list.files(paste0("johnstonei_",year,"/",model,"/",scen),
                       pattern = "^CS[0-9]+\\.tif$", full.names = TRUE)
      n  <- as.integer(sub(".*CS([0-9]+)\\.tif$","\\1", basename(fp)))
      fut_rast <- rast(fp[order(n)])
      names(fut_rast) <- paste0("CS", sort(n))

      xp_temporal <- extra_eval(
        training_data   = present_df,       # reference - present
        pr_ab           = "pr_ab",
        projection_data = fut_rast,          # projection - future
        metric          = "mahalanobis",
        univar_comb     = TRUE,
        aggreg_factor   = 1
      )
      writeRaster(xp_temporal,
                  paste0("Ejohnstonei_temporal_extrapolation_",model,"_",year,"_",scen,".tif"),
                  overwrite = TRUE)

      pdf(paste0("Ejohnstonei_temporal_extrapolation_",model,"_",year,"_",scen,".pdf"),
          width = 15, height = 5)
      par(mfrow = c(1,2))
      plot(xp_temporal$extrapolation, ext = johnstonei_window,
           main = paste("Shape metric (johnstonei)", year, scen, model), col = cl)
      plot(xp_temporal$uni_comb, ext = johnstonei_window,
           main = "Univariate & Combinatorial extrapolation (johnstonei)", col = cl)
      par(mfrow = c(1,1))
      dev.off()

      uc <- xp_temporal$uni_comb
      n_uni  <- global(uc == 1, "sum", na.rm = TRUE)[[1]]
      n_comb <- global(uc == 2, "sum", na.rm = TRUE)[[1]]
      total_proj <- global(!is.na(xp_temporal$extrapolation), "sum", na.rm = TRUE)[[1]]
      n_analog <- total_proj - (n_uni + n_comb)

      temporal_summary[[paste(year, scen, model)]] <- data.frame(
        species           = "E.johnstonei",
        year              = year,
        scenario          = scen,
        gcm               = model,
        total_cells       = total_proj,
        univariate        = n_uni,
        combinatorial     = n_comb,
        analog            = n_analog,
        pct_univariate    = round(100 * n_uni  / total_proj, 2),
        pct_combinatorial = round(100 * n_comb / total_proj, 2),
        pct_extrapolated  = round(100 * (n_uni + n_comb) / total_proj, 2),
        median_shape = round(global(xp_temporal$extrapolation, fun = median, na.rm=TRUE)[[1]], 1),
        mean_shape   = round(global(xp_temporal$extrapolation, "mean",   na.rm=TRUE)[[1]], 1),
        min_shape    = round(global(xp_temporal$extrapolation, "min",   na.rm=TRUE)[[1]], 1),
        max_shape    = round(global(xp_temporal$extrapolation, "max",    na.rm=TRUE)[[1]], 1)
      )
    }
  }
}

temporal_tab <- do.call(rbind, temporal_summary)
rownames(temporal_tab) <- NULL
write.csv(temporal_tab, "Ejohnstonei_temporal_extrapolation_summary.csv", row.names = FALSE)


# E. planirostris
Hawaii_window <- ext(-160.5, -154.5, 18.5, 22.5)
US_window <- ext(-110, -70, 5, 40)
Asia_window <- ext(100, 130, 0, 30)

# calibration environment = occurences + backgrounds
band_cols <- grep("^Band_1\\.[0-9]+$", names(Eplanirostris_bgs.z), value = TRUE)
band_num  <- as.integer(sub("^Band_1\\.([0-9]+)$", "\\1", band_cols))
cs_names  <- paste0("CS", band_num)      # Band_1.1 -> CS1
cs_cols   <- cs_names

calib_pres <- cbind(Eplanirostris_occs.z[, c("Lon","Lat")], Eplanirostris_occs.z[, band_cols, drop = FALSE])
calib_bg   <- cbind(Eplanirostris_bgs.z[,  c("Lon","Lat")], Eplanirostris_bgs.z[,  band_cols, drop = FALSE])
names(calib_pres) <- c("x", "y", cs_names)
names(calib_bg)   <- c("x", "y", cs_names)

calib_pres$pr_ab <- 1
calib_bg$pr_ab   <- 0

calib_data <- bind_rows(calib_pres, calib_bg)
calib_data <- calib_data[complete.cases(calib_data), ] # remove NAs
calib_env  <- calib_data[, c(cs_cols, "pr_ab")] 

# present environment
present_rast <- rast(Eplanirostris_envs.files)              
cs_n <- as.integer(sub(".*CS([0-9]+)\\.tif$","\\1", basename(Eplanirostris_envs.files)))
present_rast <- present_rast[[order(cs_n)]]
names(present_rast) <- paste0("CS", sort(cs_n))

# # check: Band_1.1 -> CS1
chk <- terra::extract(present_rast, Eplanirostris_occs.z[, c("Lon","Lat")])
head(data.frame(
  CS1_from_raster = chk$CS1,
  Band_1.1_in_occs = Eplanirostris_occs.z$`Band_1.1`
))

# spatial extrapolation (calibration points vs. present projection area)
xp_spatial <- extra_eval(
  training_data = calib_env,      
  pr_ab         = "pr_ab",         
  projection_data = present_rast, 
  metric        = "mahalanobis",  
  univar_comb   = TRUE,           # NT1(i.e., variable(s) of projection data is outside the range of training conditions) + NT2(i.e., variables of projection data are within the range of training conditions but with new combination of variables)
  aggreg_factor = 1
)
# novelty raster (the higher the values, the more deviated from calibration)
writeRaster(xp_spatial$extrapolation, "Eplanirostris_spatial_extrapolation_shape.tif", overwrite=TRUE)
writeRaster(xp_spatial$uni_comb,      "Eplanirostris_spatial_extrapolation_unicomb.tif", overwrite=TRUE)

# plotting
pdf("Eplanirostris_pextra_spatial.pdf", width = 20, height = 10)
print(
  p_extra(training_data = calib_data, x = "x", y = "y", pr_ab = "pr_ab",
          extra_suit_data = xp_spatial, projection_data = present_rast,
          geo_space = TRUE, prop_points = 0.05)
)
dev.off()

cl <- c("#FDE725","#B3DC2B","#6DCC57","#36B677","#1F9D87",
        "#25818E","#30678D","#3D4988","#462777","#440154")
pdf("Eplanirostris_spatial_extrapolation.pdf", width = 11, height = 15)
par(mfrow = c(3,2))
plot(xp_spatial$extrapolation, ext = Hawaii_window, main = "Shape metric (Hawaii)", col = cl)
plot(xp_spatial$uni_comb,      ext = Hawaii_window, main = "Univariate & Combinatorial extrapolation (Hawaii)", col = cl)
plot(xp_spatial$extrapolation, ext = US_window,     main = "Shape metric (US)", col = cl)
plot(xp_spatial$uni_comb,      ext = US_window,     main = "Univariate & Combinatorial extrapolation (US)", col = cl)
plot(xp_spatial$extrapolation, ext = Asia_window,   main = "Shape metric (Asia)", col = cl)
plot(xp_spatial$uni_comb,      ext = Asia_window,   main = "Univariate & Combinatorial extrapolation (Asia)", col = cl)
par(mfrow = c(1,1))
dev.off()

# uni_comb: 1 = univariate, 2 = combinatorial, NA = non-extrapolation (analog)
uc <- xp_spatial$uni_comb

n_uni  <- global(uc == 1, "sum", na.rm = TRUE)[[1]]   # univariate cell count
n_comb <- global(uc == 2, "sum", na.rm = TRUE)[[1]]   # combinatorial cell count
total_proj <- global(!is.na(xp_spatial$extrapolation), "sum", na.rm = TRUE)[[1]]  # total cell count
n_analog <- total_proj - (n_uni + n_comb) # non-extrapolation cell count

extrap_tab <- data.frame(
  species        = "E.planirostris",
  total_cells       = total_proj,
  univariate     = n_uni,
  combinatorial  = n_comb,
  analog            = n_analog,                            # non-extrapolation
  pct_univariate    = round(100 * n_uni    / total_proj, 2),  
  pct_combinatorial = round(100 * n_comb   / total_proj, 2),
  pct_extrapolated  = round(100 * (n_uni + n_comb) / total_proj, 2),
  median_shape = round(global(xp_spatial$extrapolation, fun = median, na.rm=TRUE)[[1]], 1),
  mean_shape   = round(global(xp_spatial$extrapolation, "mean",   na.rm=TRUE)[[1]], 1),
  min_shape    = round(global(xp_spatial$extrapolation, "min",   na.rm=TRUE)[[1]], 1),
  max_shape    = round(global(xp_spatial$extrapolation, "max",    na.rm=TRUE)[[1]], 1)  
)
extrap_tab
write.csv(extrap_tab, "Eplanirostris_spatial_extrapolation_summary.csv", row.names = FALSE)

# temporal extrapolation (present projection vs. future projection)
present_df <- as.data.frame(present_rast, na.rm = TRUE) %>% mutate(pr_ab = 0)

temporal_summary <- list()

for (year in c("2041-2060","2081-2100")){
  for (scen in c("ssp126","ssp585")){
    for (model in c("ACCESS-CM2","INM-CM5-0","MPI-ESM1-2-HR","IPSL-CM6A-LR","UKESM1-0-LL")){
      fp <- list.files(paste0("planirostris_",year,"/",model,"/",scen),
                       pattern = "^CS[0-9]+\\.tif$", full.names = TRUE)
      n  <- as.integer(sub(".*CS([0-9]+)\\.tif$","\\1", basename(fp)))
      fut_rast <- rast(fp[order(n)])
      names(fut_rast) <- paste0("CS", sort(n))

      xp_temporal <- extra_eval(
        training_data   = present_df,       # reference - present
        pr_ab           = "pr_ab",
        projection_data = fut_rast,          # projection - future
        metric          = "mahalanobis",
        univar_comb     = TRUE,
        aggreg_factor   = 1
      )
      writeRaster(xp_temporal,
                  paste0("Eplanirostris_temporal_extrapolation_",model,"_",year,"_",scen,".tif"),
                  overwrite = TRUE)

      pdf(paste0("Eplanirostris_temporal_extrapolation_",model,"_",year,"_",scen,".pdf"),
          width = 11, height = 10)
      par(mfrow = c(3,2))
      plot(xp_temporal$extrapolation, ext = Hawaii_window,
           main = paste("Shape metric (Hawaii)", year, scen, model), col = cl)
      plot(xp_temporal$uni_comb, ext = Hawaii_window,
           main = "Univariate & Combinatorial extrapolation (Hawaii)", col = cl)
      plot(xp_temporal$extrapolation, ext = US_window,
           main = paste("Shape metric (US)", year, scen, model), col = cl)
      plot(xp_temporal$uni_comb, ext = US_window,
           main = "Univariate & Combinatorial extrapolation (US)", col = cl)
      plot(xp_temporal$extrapolation, ext = Asia_window,
           main = paste("Shape metric (Asia)", year, scen, model), col = cl)
      plot(xp_temporal$uni_comb, ext = Asia_window,
           main = "Univariate & Combinatorial extrapolation (Asia)", col = cl)
      par(mfrow = c(1,1))
      dev.off()

      uc <- xp_temporal$uni_comb
      n_uni  <- global(uc == 1, "sum", na.rm = TRUE)[[1]]
      n_comb <- global(uc == 2, "sum", na.rm = TRUE)[[1]]
      total_proj <- global(!is.na(xp_temporal$extrapolation), "sum", na.rm = TRUE)[[1]]
      n_analog <- total_proj - (n_uni + n_comb)

      temporal_summary[[paste(year, scen, model)]] <- data.frame(
        species           = "E.planirostris",
        year              = year,
        scenario          = scen,
        gcm               = model,
        total_cells       = total_proj,
        univariate        = n_uni,
        combinatorial     = n_comb,
        analog            = n_analog,
        pct_univariate    = round(100 * n_uni  / total_proj, 2),
        pct_combinatorial = round(100 * n_comb / total_proj, 2),
        pct_extrapolated  = round(100 * (n_uni + n_comb) / total_proj, 2),
        median_shape = round(global(xp_temporal$extrapolation, fun = median, na.rm=TRUE)[[1]], 1),
        mean_shape   = round(global(xp_temporal$extrapolation, "mean",   na.rm=TRUE)[[1]], 1),
        min_shape    = round(global(xp_temporal$extrapolation, "min",   na.rm=TRUE)[[1]], 1),
        max_shape    = round(global(xp_temporal$extrapolation, "max",    na.rm=TRUE)[[1]], 1)
      )
    }
  }
}

temporal_tab <- do.call(rbind, temporal_summary)
rownames(temporal_tab) <- NULL
write.csv(temporal_tab, "Eplanirostris_temporal_extrapolation_summary.csv", row.names = FALSE)


# E. antillensis
antillensis_window <- ext(-67.5, -64, 17.5, 19)

# calibration environment = occurences + backgrounds
band_cols <- grep("^Band_1\\.[0-9]+$", names(Eantillensis_bgs.z), value = TRUE)
band_num  <- as.integer(sub("^Band_1\\.([0-9]+)$", "\\1", band_cols))
cs_names  <- paste0("CS", band_num)      # Band_1.1 -> CS1
cs_cols   <- cs_names

calib_pres <- cbind(Eantillensis_occs.z[, c("Lon","Lat")], Eantillensis_occs.z[, band_cols, drop = FALSE])
calib_bg   <- cbind(Eantillensis_bgs.z[,  c("Lon","Lat")], Eantillensis_bgs.z[,  band_cols, drop = FALSE])
names(calib_pres) <- c("x", "y", cs_names)
names(calib_bg)   <- c("x", "y", cs_names)

calib_pres$pr_ab <- 1
calib_bg$pr_ab   <- 0

calib_data <- bind_rows(calib_pres, calib_bg)
calib_data <- calib_data[complete.cases(calib_data), ] # remove NAs
calib_env  <- calib_data[, c(cs_cols, "pr_ab")] 

# present environment
present_rast <- rast(Eantillensis_envs.files)              
cs_n <- as.integer(sub(".*CS([0-9]+)\\.tif$","\\1", basename(Eantillensis_envs.files)))
present_rast <- present_rast[[order(cs_n)]]
names(present_rast) <- paste0("CS", sort(cs_n))

# # check: Band_1.1 -> CS1
chk <- terra::extract(present_rast, Eantillensis_occs.z[, c("Lon","Lat")])
head(data.frame(
  CS1_from_raster = chk$CS1,
  Band_1.1_in_occs = Eantillensis_occs.z$`Band_1.1`
))

# spatial extrapolation (calibration points vs. present projection area)
xp_spatial <- extra_eval(
  training_data = calib_env,      
  pr_ab         = "pr_ab",         
  projection_data = present_rast, 
  metric        = "mahalanobis",  
  univar_comb   = TRUE,           # NT1(i.e., variable(s) of projection data is outside the range of training conditions) + NT2(i.e., variables of projection data are within the range of training conditions but with new combination of variables)
  aggreg_factor = 1
)
# novelty raster (the higher the values, the more deviated from calibration)
writeRaster(xp_spatial$extrapolation, "Eantillensis_spatial_extrapolation_shape.tif", overwrite=TRUE)
writeRaster(xp_spatial$uni_comb,      "Eantillensis_spatial_extrapolation_unicomb.tif", overwrite=TRUE)

# plotting
pdf("Eantillensis_pextra_spatial.pdf", width = 20, height = 10)
print(
  p_extra(training_data = calib_data, x = "x", y = "y", pr_ab = "pr_ab",
          extra_suit_data = xp_spatial, projection_data = present_rast,
          geo_space = TRUE, prop_points = 0.05)
)
dev.off()

cl <- c("#FDE725","#B3DC2B","#6DCC57","#36B677","#1F9D87",
        "#25818E","#30678D","#3D4988","#462777","#440154")
pdf("Eantillensis_spatial_extrapolation.pdf", width = 11, height = 5)
par(mfrow = c(1,2))
plot(xp_spatial$extrapolation, ext = antillensis_window, main = "Shape metric (antillensis)", col = cl)
plot(xp_spatial$uni_comb,      ext = antillensis_window, main = "Univariate & Combinatorial extrapolation (antillensis)", col = cl)
par(mfrow = c(1,1))
dev.off()

# uni_comb: 1 = univariate, 2 = combinatorial, NA = non-extrapolation (analog)
uc <- xp_spatial$uni_comb

n_uni  <- global(uc == 1, "sum", na.rm = TRUE)[[1]]   # univariate cell count
n_comb <- global(uc == 2, "sum", na.rm = TRUE)[[1]]   # combinatorial cell count
total_proj <- global(!is.na(xp_spatial$extrapolation), "sum", na.rm = TRUE)[[1]]  # total cell count
n_analog <- total_proj - (n_uni + n_comb) # non-extrapolation cell count

extrap_tab <- data.frame(
  species        = "E.antillensis",
  total_cells       = total_proj,
  univariate     = n_uni,
  combinatorial  = n_comb,
  analog            = n_analog,                            # non-extrapolation
  pct_univariate    = round(100 * n_uni    / total_proj, 2),  
  pct_combinatorial = round(100 * n_comb   / total_proj, 2),
  pct_extrapolated  = round(100 * (n_uni + n_comb) / total_proj, 2),  
  median_shape = round(global(xp_spatial$extrapolation, fun = median, na.rm=TRUE)[[1]], 1),
  mean_shape   = round(global(xp_spatial$extrapolation, "mean",   na.rm=TRUE)[[1]], 1),
  min_shape    = round(global(xp_spatial$extrapolation, "min",   na.rm=TRUE)[[1]], 1),
  max_shape    = round(global(xp_spatial$extrapolation, "max",    na.rm=TRUE)[[1]], 1)  
)
extrap_tab
write.csv(extrap_tab, "Eantillensis_spatial_extrapolation_summary.csv", row.names = FALSE)

# temporal extrapolation (present projection vs. future projection)
present_df <- as.data.frame(present_rast, na.rm = TRUE) %>% mutate(pr_ab = 0)

temporal_summary <- list()

for (year in c("2041-2060","2081-2100")){
  for (scen in c("ssp126","ssp585")){
    for (model in c("ACCESS-CM2","INM-CM5-0","MPI-ESM1-2-HR","IPSL-CM6A-LR","UKESM1-0-LL")){
      fp <- list.files(paste0("antillensis_",year,"/",model,"/",scen),
                       pattern = "^CS[0-9]+\\.tif$", full.names = TRUE)
      n  <- as.integer(sub(".*CS([0-9]+)\\.tif$","\\1", basename(fp)))
      fut_rast <- rast(fp[order(n)])
      names(fut_rast) <- paste0("CS", sort(n))

      xp_temporal <- extra_eval(
        training_data   = present_df,       # reference - present
        pr_ab           = "pr_ab",
        projection_data = fut_rast,          # projection - future
        metric          = "mahalanobis",
        univar_comb     = TRUE,
        aggreg_factor   = 1
      )
      writeRaster(xp_temporal,
                  paste0("Eantillensis_temporal_extrapolation_",model,"_",year,"_",scen,".tif"),
                  overwrite = TRUE)

      pdf(paste0("Eantillensis_temporal_extrapolation_",model,"_",year,"_",scen,".pdf"),
          width = 15, height = 5)
      par(mfrow = c(1,2))
      plot(xp_temporal$extrapolation, ext = antillensis_window, 
           main = paste("Shape metric (antillensis)", year, scen, model), col = cl)
      plot(xp_temporal$uni_comb, ext = antillensis_window, 
           main = "Univariate & Combinatorial extrapolation (antillensis)", col = cl)
      par(mfrow = c(1,1))
      dev.off()

      uc <- xp_temporal$uni_comb
      n_uni  <- global(uc == 1, "sum", na.rm = TRUE)[[1]]
      n_comb <- global(uc == 2, "sum", na.rm = TRUE)[[1]]
      total_proj <- global(!is.na(xp_temporal$extrapolation), "sum", na.rm = TRUE)[[1]]
      n_analog <- total_proj - (n_uni + n_comb)

      temporal_summary[[paste(year, scen, model)]] <- data.frame(
        species           = "E.antillensis",
        year              = year,
        scenario          = scen,
        gcm               = model,
        total_cells       = total_proj,
        univariate        = n_uni,
        combinatorial     = n_comb,
        analog            = n_analog,
        pct_univariate    = round(100 * n_uni  / total_proj, 2),
        pct_combinatorial = round(100 * n_comb / total_proj, 2),
        pct_extrapolated  = round(100 * (n_uni + n_comb) / total_proj, 2),
        median_shape = round(global(xp_temporal$extrapolation, fun = median, na.rm=TRUE)[[1]], 1),
        mean_shape   = round(global(xp_temporal$extrapolation, "mean",   na.rm=TRUE)[[1]], 1),
        min_shape    = round(global(xp_temporal$extrapolation, "min",   na.rm=TRUE)[[1]], 1),
        max_shape    = round(global(xp_temporal$extrapolation, "max",    na.rm=TRUE)[[1]], 1)
      )
    }
  }
}

temporal_tab <- do.call(rbind, temporal_summary)
rownames(temporal_tab) <- NULL
write.csv(temporal_tab, "Eantillensis_temporal_extrapolation_summary.csv", row.names = FALSE)


# E. martinicensis
Jamaica_window <- ext(-78.75, -76, 17.5, 18.75) 
Antilles_window <- ext(-63, -60, 12, 18) 

# calibration environment = occurences + backgrounds
band_cols <- grep("^Band_1\\.[0-9]+$", names(Emartinicensis_bgs.z), value = TRUE)
band_num  <- as.integer(sub("^Band_1\\.([0-9]+)$", "\\1", band_cols))
cs_names  <- paste0("CS", band_num)      # Band_1.1 -> CS1
cs_cols   <- cs_names

calib_pres <- cbind(Emartinicensis_occs.z[, c("Lon","Lat")], Emartinicensis_occs.z[, band_cols, drop = FALSE])
calib_bg   <- cbind(Emartinicensis_bgs.z[,  c("Lon","Lat")], Emartinicensis_bgs.z[,  band_cols, drop = FALSE])
names(calib_pres) <- c("x", "y", cs_names)
names(calib_bg)   <- c("x", "y", cs_names)

calib_pres$pr_ab <- 1
calib_bg$pr_ab   <- 0

calib_data <- bind_rows(calib_pres, calib_bg)
calib_data <- calib_data[complete.cases(calib_data), ] # remove NAs
calib_env  <- calib_data[, c(cs_cols, "pr_ab")] 

# present environment
present_rast <- rast(Emartinicensis_envs.files)              
cs_n <- as.integer(sub(".*CS([0-9]+)\\.tif$","\\1", basename(Emartinicensis_envs.files)))
present_rast <- present_rast[[order(cs_n)]]
names(present_rast) <- paste0("CS", sort(cs_n))

# # check: Band_1.1 -> CS1
chk <- terra::extract(present_rast, Emartinicensis_occs.z[, c("Lon","Lat")])
head(data.frame(
  CS1_from_raster = chk$CS1,
  Band_1.1_in_occs = Emartinicensis_occs.z$`Band_1.1`
))

# spatial extrapolation (calibration points vs. present projection area)
xp_spatial <- extra_eval(
  training_data = calib_env,      
  pr_ab         = "pr_ab",         
  projection_data = present_rast, 
  metric        = "mahalanobis",  
  univar_comb   = TRUE,           # NT1(i.e., variable(s) of projection data is outside the range of training conditions) + NT2(i.e., variables of projection data are within the range of training conditions but with new combination of variables)
  aggreg_factor = 1
)
# novelty raster (the higher the values, the more deviated from calibration)
writeRaster(xp_spatial$extrapolation, "Emartinicensis_spatial_extrapolation_shape.tif", overwrite=TRUE)
writeRaster(xp_spatial$uni_comb,      "Emartinicensis_spatial_extrapolation_unicomb.tif", overwrite=TRUE)

# plotting
pdf("Emartinicensis_pextra_spatial.pdf", width = 20, height = 10)
print(
  p_extra(training_data = calib_data, x = "x", y = "y", pr_ab = "pr_ab",
          extra_suit_data = xp_spatial, projection_data = present_rast,
          geo_space = TRUE, prop_points = 0.05)
)
dev.off()

cl <- c("#FDE725","#B3DC2B","#6DCC57","#36B677","#1F9D87",
        "#25818E","#30678D","#3D4988","#462777","#440154")
pdf("Emartinicensis_spatial_extrapolation.pdf", width = 11, height = 10)
par(mfrow = c(2,2))
plot(xp_spatial$extrapolation, ext = Jamaica_window,        main = "Shape metric (Jamaica)", col = cl)
plot(xp_spatial$uni_comb,      ext = Jamaica_window,        main = "Univariate & Combinatorial extrapolation (Jamaica)", col = cl)
plot(xp_spatial$extrapolation, ext = Antilles_window, main = "Shape metric (Lesser Antilles)", col = cl)
plot(xp_spatial$uni_comb,      ext = Antilles_window, main = "Univariate & Combinatorial extrapolation (Lesser Antilles)", col = cl)
par(mfrow = c(1,1))
dev.off()

# uni_comb: 1 = univariate, 2 = combinatorial, NA = non-extrapolation (analog)
uc <- xp_spatial$uni_comb

n_uni  <- global(uc == 1, "sum", na.rm = TRUE)[[1]]   # univariate cell count
n_comb <- global(uc == 2, "sum", na.rm = TRUE)[[1]]   # combinatorial cell count
total_proj <- global(!is.na(xp_spatial$extrapolation), "sum", na.rm = TRUE)[[1]]  # total cell count
n_analog <- total_proj - (n_uni + n_comb) # non-extrapolation cell count

extrap_tab <- data.frame(
  species        = "E.martinicensis",
  total_cells       = total_proj,
  univariate     = n_uni,
  combinatorial  = n_comb,
  analog            = n_analog,                            # non-extrapolation
  pct_univariate    = round(100 * n_uni    / total_proj, 2),  
  pct_combinatorial = round(100 * n_comb   / total_proj, 2),
  pct_extrapolated  = round(100 * (n_uni + n_comb) / total_proj, 2),
  median_shape = round(global(xp_spatial$extrapolation, fun = median, na.rm=TRUE)[[1]], 1),
  mean_shape   = round(global(xp_spatial$extrapolation, "mean",   na.rm=TRUE)[[1]], 1),
  min_shape    = round(global(xp_spatial$extrapolation, "min",   na.rm=TRUE)[[1]], 1),
  max_shape    = round(global(xp_spatial$extrapolation, "max",    na.rm=TRUE)[[1]], 1)    
)
extrap_tab
write.csv(extrap_tab, "Emartinicensis_spatial_extrapolation_summary.csv", row.names = FALSE)

# temporal extrapolation (present projection vs. future projection)
present_df <- as.data.frame(present_rast, na.rm = TRUE) %>% mutate(pr_ab = 0)

temporal_summary <- list()

for (year in c("2041-2060","2081-2100")){
  for (scen in c("ssp126","ssp585")){
    for (model in c("ACCESS-CM2","INM-CM5-0","MPI-ESM1-2-HR","IPSL-CM6A-LR","UKESM1-0-LL")){
      fp <- list.files(paste0("martinicensis_",year,"/",model,"/",scen),
                       pattern = "^CS[0-9]+\\.tif$", full.names = TRUE)
      n  <- as.integer(sub(".*CS([0-9]+)\\.tif$","\\1", basename(fp)))
      fut_rast <- rast(fp[order(n)])
      names(fut_rast) <- paste0("CS", sort(n))

      xp_temporal <- extra_eval(
        training_data   = present_df,       # reference - present
        pr_ab           = "pr_ab",
        projection_data = fut_rast,          # projection - future
        metric          = "mahalanobis",
        univar_comb     = TRUE,
        aggreg_factor   = 1
      )
      writeRaster(xp_temporal,
                  paste0("Emartinicensis_temporal_extrapolation_",model,"_",year,"_",scen,".tif"),
                  overwrite = TRUE)

      pdf(paste0("Emartinicensis_temporal_extrapolation_",model,"_",year,"_",scen,".pdf"),
          width = 12, height = 10)
      par(mfrow = c(2,2))
      plot(xp_temporal$extrapolation, ext = Jamaica_window,
           main = paste("Shape metric (Jamaica)", year, scen, model), col = cl)
      plot(xp_temporal$uni_comb, ext = Jamaica_window,
           main = "Univariate & Combinatorial extrapolation (Jamaica)", col = cl)
      plot(xp_temporal$extrapolation, ext = Antilles_window,
           main = paste("Shape metric (Lesser Antilles)", year, scen, model), col = cl)
      plot(xp_temporal$uni_comb, ext = Antilles_window,
           main = "Univariate & Combinatorial extrapolation (Lesser Antilles)", col = cl)
      par(mfrow = c(1,1))
      dev.off()

      uc <- xp_temporal$uni_comb
      n_uni  <- global(uc == 1, "sum", na.rm = TRUE)[[1]]
      n_comb <- global(uc == 2, "sum", na.rm = TRUE)[[1]]
      total_proj <- global(!is.na(xp_temporal$extrapolation), "sum", na.rm = TRUE)[[1]]
      n_analog <- total_proj - (n_uni + n_comb)

      temporal_summary[[paste(year, scen, model)]] <- data.frame(
        species           = "E.martinicensis",
        year              = year,
        scenario          = scen,
        gcm               = model,
        total_cells       = total_proj,
        univariate        = n_uni,
        combinatorial     = n_comb,
        analog            = n_analog,
        pct_univariate    = round(100 * n_uni  / total_proj, 2),
        pct_combinatorial = round(100 * n_comb / total_proj, 2),
        pct_extrapolated  = round(100 * (n_uni + n_comb) / total_proj, 2),
        median_shape = round(global(xp_temporal$extrapolation, fun = median, na.rm=TRUE)[[1]], 1),
        mean_shape   = round(global(xp_temporal$extrapolation, "mean",   na.rm=TRUE)[[1]], 1),
        min_shape    = round(global(xp_temporal$extrapolation, "min",   na.rm=TRUE)[[1]], 1),
        max_shape    = round(global(xp_temporal$extrapolation, "max",    na.rm=TRUE)[[1]], 1)
      )
    }
  }
}

temporal_tab <- do.call(rbind, temporal_summary)
rownames(temporal_tab) <- NULL
write.csv(temporal_tab, "Emartinicensis_temporal_extrapolation_summary.csv", row.names = FALSE)



# Quantifiaction of suitable areas
library(terra)

calc_area <- function(bin_path, buffer_path) {
  bin <- terra::rast(bin_path)
  buf <- terra::vect(buffer_path)
  buf <- terra::project(buf, crs(bin))
  bin_m <- terra::mask(crop(bin, buf), buf, touches = TRUE)   
  area_r <- terra::cellSize(bin_m, unit = "km")
  global(bin_m * area_r, "sum", na.rm = TRUE)[[1]]
}

species_list <- list(
  list(prefix = "Ecoqui",         shp = "coqui",         label = "E.coqui"),
  list(prefix = "Ejohnstonei",    shp = "johnstonei",    label = "E.johnstonei"),
  list(prefix = "Eplanirostris",  shp = "planirostris",  label = "E.planirostris"),
  list(prefix = "Eantillensis",   shp = "antillensis",   label = "E.antillensis"),
  list(prefix = "Emartinicensis", shp = "martinicensis", label = "E.martinicensis")
)

all_area <- list()

for (sp in species_list) {
  # present (1km buffer)
  a_present <- calc_area(
    paste0(sp$prefix, "_rev3_ensemble_present.bin.tif"),
    paste0(sp$shp, "_rev3_1km_onLand.shp")
  )

  # future: year × scenario; 40 or 80km buffer
  a_40_126 <- calc_area(paste0(sp$prefix,"_rev3_GCMconsensus_2041-2060_ssp126.tif"), paste0(sp$shp,"_rev3_40km_onLand.shp"))
  a_80_126 <- calc_area(paste0(sp$prefix,"_rev3_GCMconsensus_2081-2100_ssp126.tif"), paste0(sp$shp,"_rev3_80km_onLand.shp"))
  a_40_585 <- calc_area(paste0(sp$prefix,"_rev3_GCMconsensus_2041-2060_ssp585.tif"), paste0(sp$shp,"_rev3_40km_onLand.shp"))
  a_80_585 <- calc_area(paste0(sp$prefix,"_rev3_GCMconsensus_2081-2100_ssp585.tif"), paste0(sp$shp,"_rev3_80km_onLand.shp"))

  areas <- c(a_present, a_40_126, a_80_126, a_40_585, a_80_585)

  a_area_tab <- data.frame(
  species    = sp$label,
  period     = c("present", "2041-2060", "2081-2100", "2041-2060", "2081-2100"),
  scenario   = c(NA, "ssp126", "ssp126", "ssp585", "ssp585"),
  buffer     = c("1km", "40km", "80km", "40km", "80km"),
  area_km2   = round(areas, 1),
  abs_change = round(areas - a_present, 1),
  pct_change = round(100 * (areas - a_present) / a_present, 1)
  )
  a_area_tab
  write.csv(a_area_tab, paste0(sp$prefix, "_area_change.csv"), row.names = FALSE)

  all_area[[sp$label]] <- a_area_tab
}

area_tab <- do.call(rbind, all_area)
rownames(area_tab) <- NULL
area_tab
write.csv(area_tab, "all_species_area_change.csv", row.names = FALSE)