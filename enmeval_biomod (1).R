# load geos, gdal, proj first
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
#Ecoqui_envs.files <- list.files(path = "Ecoqui_contemp", pattern = ".tif$", full.names=TRUE)
Ecoqui_envs.files <- list.files(path = "coqui_contemp", pattern = ".tif$", full.names=TRUE)
Ecoqui_envs <- stack(Ecoqui_envs.files)
proj_wgs84 <- crs(Ecoqui_envs)

#Ecoqui_points <- read.csv("Eleutherodactylus_invasion_arcgis/Ecoqui_rarefied_points.csv", header = T) # there are few weirdly formated rows. manually check and correct them.
Ecoqui_points <- read.csv("Eleutherodactylus_invasion_arcgis/coqui-rev_rarefied_points.csv", header = T) # there are few weirdly formated rows. manually check and correct them.
Ecoqui_coordinates <- cbind(Ecoqui_points$decimalLon, Ecoqui_points$decimalLat)
colnames(Ecoqui_coordinates ) <- c("Lon", "Lat")
Ecoqui_occs <- as.data.frame(Ecoqui_coordinates)
#Ecoqui_occs <- SpatialPoints(Ecoqui_coordinates)
#projection(Ecoqui_occs) <- proj_wgs84
#Ecoqui_occs$Lon <- as.numeric(Ecoqui_occs$Lon)
#Ecoqui_occs$Lat <- as.numeric(Ecoqui_occs$Lat)
Ecoqui_occs.z <- cbind(Ecoqui_occs, raster::extract(Ecoqui_envs, Ecoqui_occs)) # extract raster values at points

#Ecoqui_bias <- raster("Ecoqui_bias/ecoqui.asc")
Ecoqui_bias <- raster("Eleutherodactylus_invasion_arcgis/coqui_rev_bias/coqui.asc")

# Plot first raster in the stack, the mean annual temperature
plot(Ecoqui_bias)

# Add points for all the occurrence points onto the raster
points(Ecoqui_occs, col = "red")

# Randomly sample 10,000 background points from one background extent raster (n=10000 is recommended for MaxEnt by Barbet-Massin et al. 2012 in Methods in Ecology and Evolution; n=50000 by 2)
#Ecoqui_bgpoints <- dismo::randomPoints(Ecoqui_bias, n = 10000, prob = T) %>% as.data.frame()
Ecoqui_bgpoints <- spatSample(x = rast(Ecoqui_bias), size = 50000, method = "weights", replace = TRUE, na.rm = TRUE, xy = TRUE) |> as.data.frame()
colnames(Ecoqui_bgpoints) <- colnames(Ecoqui_coordinates)
Ecoqui_bgpoints <- Ecoqui_bgpoints[ , -3]
#mask <- Ecoqui_bias
#vals <- values(mask)
#cand_cells <- which(!is.na(vals) & vals > 0)
#pres_cells <- cellFromXY(mask, Ecoqui_coordinates[, c("Lon","Lat")])
#pres_cells <- unique(na.omit(pres_cells))
#avail_cells <- setdiff(cand_cells, pres_cells)
#n_bg <- min(10000L, length(avail_cells)) # =3848
#w <- vals[avail_cells] # weights from bias raster for the candidate cells
#stopifnot(all(is.finite(w) & w > 0)) # sanity check; adjust if needed
#set.seed(1)
#sel_cells <- sample(avail_cells, size = n_bg, replace = FALSE, prob = w)
#Ecoqui_bgpoints <- as.data.frame(xyFromCell(mask, sel_cells))
#Ecoqui_bgpoints <- SpatialPoints(Ecoqui_bgpoints)
#projection(Ecoqui_bgpoints) <- proj_wgs84
#Ecoqui_bgpoints <- dismo::randomPoints(Ecoqui_bias, n = 3848, prob = T) %>% as.data.frame() # use all the non-NA cells (sum(!is.na(values(Ecoqui_bias))) = 3733)
#colnames(Ecoqui_bgpoints) <- colnames(Ecoqui_coordinates)

Ecoqui_bg.z <- cbind(Ecoqui_bgpoints, raster::extract(Ecoqui_envs, Ecoqui_bgpoints)) # extract raster values at points

#plot(Ecoqui_bias, xlim = c(-84,-82), ylim = c(9,11)
plot(Ecoqui_bias, xlim = c(-156.25, -154.8), ylim = c(18.7, 20.5))
points(Ecoqui_bgpoints, pch = 20, cex = 0.2)

# Run ENMeval
e.mx.Ecoqui <- ENMevaluate(occs = Ecoqui_occs.z, bg = Ecoqui_bg.z, 
                           algorithm = 'maxnet', partitions = 'randomkfold', partition.settings = list(kfolds = 5),
                           tune.args = list(fc = c("L","LQ","LQH","H"), rm = seq(0.5,5,0.5)), 
                           parallel = TRUE, numCores = 120)
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
opt.aicc.Ecoqui # fc: H, rm: 2 -> fc: LQH, rm: 0.5

# the sequential criteria
res.Ecoqui[order(res.Ecoqui$or.10p.avg, -res.Ecoqui$auc.val.avg),] %>% head()
opt.seq.Ecoqui <- res.Ecoqui %>% 
  filter(or.10p.avg == min(or.10p.avg)) %>% 
  filter(auc.val.avg == max(auc.val.avg))
opt.seq.Ecoqui # fc: L, rm: 2.5 -> fc: LQ, rm: 0.5


# Repeat the above steps for Ejohnstonei
#Ejohnstonei_envs.files <- list.files(path = "Ejohnstonei_contemp", pattern = ".tif$", full.names=TRUE)
Ejohnstonei_envs.files <- list.files(path = "johnstonei_contemp", pattern = ".tif$", full.names=TRUE)
Ejohnstonei_envs <- stack(Ejohnstonei_envs.files)
proj_wgs84 <- crs(Ejohnstonei_envs)

#Ejohnstonei_points <- read.csv("Eleutherodactylus_invasion_arcgis/Ejohnstonei_rarefied_points.csv", header = T)
Ejohnstonei_points <- read.csv("Eleutherodactylus_invasion_arcgis/johnstonei-rev_rarefied_points.csv", header = T)
Ejohnstonei_coordinates <- cbind(Ejohnstonei_points$decimalLon, Ejohnstonei_points$decimalLat)
colnames(Ejohnstonei_coordinates ) <- c("Lon", "Lat")
Ejohnstonei_occs <- as.data.frame(Ejohnstonei_coordinates)
Ejohnstonei_occs.z <- cbind(Ejohnstonei_occs, raster::extract(Ejohnstonei_envs, Ejohnstonei_occs))

#Ejohnstonei_bias <- raster("Ejohnstonei_bias/Ejohnstonei.asc")
Ejohnstonei_bias <- raster("Eleutherodactylus_invasion_arcgis/johnstonei_rev_bias/johnstonei.asc")
plot(Ejohnstonei_bias)
points(Ejohnstonei_occs, col = "red")

Ejohnstonei_bgpoints <- spatSample(x = rast(Ejohnstonei_bias), size = 50000, method = "weights", replace = TRUE, na.rm = TRUE, xy = TRUE) |> as.data.frame()
colnames(Ejohnstonei_bgpoints) <- colnames(Ejohnstonei_coordinates)
Ejohnstonei_bgpoints <- Ejohnstonei_bgpoints[ , -3]

#Ejohnstonei_bgpoints <- dismo::randomPoints(Ejohnstonei_bias, n = 10000, prob = T) %>% as.data.frame()
#colnames(Ejohnstonei_bgpoints) <- colnames(Ejohnstonei_coordinates)
Ejohnstonei_bg.z <- cbind(Ejohnstonei_bgpoints, raster::extract(Ejohnstonei_envs, Ejohnstonei_bgpoints))

plot(Ejohnstonei_bias, xlim = c(-75,-70), ylim = c(2,7))
points(Ejohnstonei_bgpoints, pch = 20, cex = 0.2)

e.mx.Ejohnstonei <- ENMevaluate(occs = Ejohnstonei_occs.z, bg = Ejohnstonei_bg.z, 
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
opt.aicc.Ejohnstonei # fc: LQ, rm: 1.5 -> fc: H, rm: 0.5

res.Ejohnstonei[order(res.Ejohnstonei$or.10p.avg, -res.Ejohnstonei$auc.val.avg),] %>% head()
opt.seq.Ejohnstonei <- res.Ejohnstonei %>% 
  filter(or.10p.avg == min(or.10p.avg)) %>% 
  filter(auc.val.avg == max(auc.val.avg))
opt.seq.Ejohnstonei # fc: L, rm: 0.5 -> fc: LQ, rm: 0.5


# Repeat the above steps for Eplanirostris
#Eplanirostris_envs.files <- list.files(path = "Eplanirostris_contemp", pattern = ".tif$", full.names=TRUE)
Eplanirostris_envs.files <- list.files(path = "planirostris_contemp", pattern = ".tif$", full.names=TRUE)
Eplanirostris_envs <- stack(Eplanirostris_envs.files)
proj_wgs84 <- crs(Eplanirostris_envs)

#Eplanirostris_points <- read.csv("Eleutherodactylus_invasion_arcgis/Eplanirostris2_rarefied_points.csv", header = T)
Eplanirostris_points <- read.csv("Eleutherodactylus_invasion_arcgis/planirostris-rev_rarefied_points.csv", header = T)
Eplanirostris_coordinates <- cbind(Eplanirostris_points$decimalLon, Eplanirostris_points$decimalLat)
colnames(Eplanirostris_coordinates ) <- c("Lon", "Lat")
Eplanirostris_occs <- as.data.frame(Eplanirostris_coordinates)
Eplanirostris_occs.z <- cbind(Eplanirostris_occs, raster::extract(Eplanirostris_envs, Eplanirostris_occs))
  
#Eplanirostris_bias <- raster("Eplanirostris_bias/eplanirostris2.asc")
Eplanirostris_bias <- raster("Eleutherodactylus_invasion_arcgis/planirostris_rev_bias/planirostris.asc")
plot(Eplanirostris_bias)
points(Eplanirostris_occs, col = "red")

Eplanirostris_bgpoints <- spatSample(x = rast(Eplanirostris_bias), size = 50000, method = "weights", replace = TRUE, na.rm = TRUE, xy = TRUE) |> as.data.frame()
colnames(Eplanirostris_bgpoints) <- colnames(Eplanirostris_coordinates)
Eplanirostris_bgpoints <- Eplanirostris_bgpoints[ , -3]

#Eplanirostris_bgpoints <- dismo::randomPoints(Eplanirostris_bias, n = 10000, prob = T) %>% as.data.frame()
#colnames(Eplanirostris_bgpoints) <- colnames(Eplanirostris_coordinates)
Eplanirostris_bg.z <- cbind(Eplanirostris_bgpoints, raster::extract(Eplanirostris_envs, Eplanirostris_bgpoints))
  
plot(Eplanirostris_bias, xlim = c(-97,-92), ylim = c(28,33))
points(Eplanirostris_bgpoints, pch = 20, cex = 0.2)

e.mx.Eplanirostris <- ENMevaluate(occs = Eplanirostris_occs.z, bg = Eplanirostris_bg.z, 
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
opt.aicc.Eplanirostris # fc: H, rm: 0.5 -> fc: LQH, rm: 0.5

res.Eplanirostris[order(res.Eplanirostris$or.10p.avg, -res.Eplanirostris$auc.val.avg),] %>% head()
opt.seq.Eplanirostris <- res.Eplanirostris %>% 
  filter(or.10p.avg == min(or.10p.avg)) %>% 
  filter(auc.val.avg == max(auc.val.avg))
opt.seq.Eplanirostris # fc: LQ, rm: 5 -> fc: LQ, rm: 3

# Repeat the above steps for Eantillensis
Eantillensis_envs.files <- list.files(path = "antillensis_contemp", pattern = ".tif$", full.names=TRUE)
Eantillensis_envs <- stack(Eantillensis_envs.files)
proj_wgs84 <- crs(Eantillensis_envs)

Eantillensis_points <- read.csv("Eleutherodactylus_invasion_arcgis/antillensis-rev_rarefied_points.csv", header = T)
Eantillensis_coordinates <- cbind(Eantillensis_points$decimalLon, Eantillensis_points$decimalLat)
colnames(Eantillensis_coordinates ) <- c("Lon", "Lat")
Eantillensis_occs <- as.data.frame(Eantillensis_coordinates)
Eantillensis_occs.z <- cbind(Eantillensis_occs, raster::extract(Eantillensis_envs, Eantillensis_occs))
  
Eantillensis_bias <- raster("Eleutherodactylus_invasion_arcgis/antillensis_rev_bias/antillensis.asc")
plot(Eantillensis_bias)
points(Eantillensis_occs, col = "red")

Eantillensis_bgpoints <- spatSample(x = rast(Eantillensis_bias), size = 50000, method = "weights", replace = TRUE, na.rm = TRUE, xy = TRUE) |> as.data.frame()
colnames(Eantillensis_bgpoints) <- colnames(Eantillensis_coordinates)
Eantillensis_bgpoints <- Eantillensis_bgpoints[ , -3]

Eantillensis_bg.z <- cbind(Eantillensis_bgpoints, raster::extract(Eantillensis_envs, Eantillensis_bgpoints))
  
plot(Eantillensis_bias)
points(Eantillensis_bgpoints, pch = 20, cex = 0.2)

e.mx.Eantillensis <- ENMevaluate(occs = Eantillensis_occs.z, bg = Eantillensis_bg.z, 
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
opt.aicc.Eantillensis # fc: LQH, rm: 3

res.Eantillensis[order(res.Eantillensis$or.10p.avg, -res.Eantillensis$auc.val.avg),] %>% head()
opt.seq.Eantillensis <- res.Eantillensis %>% 
  filter(or.10p.avg == min(or.10p.avg)) %>% 
  filter(auc.val.avg == max(auc.val.avg))
opt.seq.Eantillensis # fc: LQH, rm: 4.5

# Repeat the above steps for Emartinicensis
Emartinicensis_envs.files <- list.files(path = "martinicensis_contemp", pattern = ".tif$", full.names=TRUE)
Emartinicensis_envs <- stack(Emartinicensis_envs.files)
proj_wgs84 <- crs(Emartinicensis_envs)

Emartinicensis_points <- read.csv("Eleutherodactylus_invasion_arcgis/martinicensis-rev_rarefied_points.csv", header = T)
Emartinicensis_coordinates <- cbind(Emartinicensis_points$decimalLon, Emartinicensis_points$decimalLat)
colnames(Emartinicensis_coordinates ) <- c("Lon", "Lat")
Emartinicensis_occs <- as.data.frame(Emartinicensis_coordinates)
Emartinicensis_occs.z <- cbind(Emartinicensis_occs, raster::extract(Emartinicensis_envs, Emartinicensis_occs))
  
Emartinicensis_bias <- raster("Eleutherodactylus_invasion_arcgis/martinicensis_rev_bias/martinicensis.asc")
plot(Emartinicensis_bias)
points(Emartinicensis_occs, col = "red")

Emartinicensis_bgpoints <- spatSample(x = rast(Emartinicensis_bias), size = 50000, method = "weights", replace = TRUE, na.rm = TRUE, xy = TRUE) |> as.data.frame()
colnames(Emartinicensis_bgpoints) <- colnames(Emartinicensis_coordinates)
Emartinicensis_bgpoints <- Emartinicensis_bgpoints[ , -3]

Emartinicensis_bg.z <- cbind(Emartinicensis_bgpoints, raster::extract(Emartinicensis_envs, Emartinicensis_bgpoints))
  
plot(Emartinicensis_bias)
points(Emartinicensis_bgpoints, pch = 20, cex = 0.2)

e.mx.Emartinicensis <- ENMevaluate(occs = Emartinicensis_occs.z, bg = Emartinicensis_bg.z, 
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
opt.aicc.Emartinicensis # fc: L, rm: 0.5

res.Emartinicensis[order(res.Emartinicensis$or.10p.avg, -res.Emartinicensis$auc.val.avg),] %>% head()
opt.seq.Emartinicensis <- res.Emartinicensis %>% 
  filter(or.10p.avg == min(or.10p.avg)) %>% 
  filter(auc.val.avg == max(auc.val.avg))
opt.seq.Emartinicensis # fc: H, rm: 5



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
  PA.nb.rep = 50,
  PA.nb.absences = 1000,
  #PA.nb.absences = nrow(Ecoqui_bgpoints), 
  PA.strategy = 'random',
  filter.raster = TRUE
)

assignInNamespace("isFALSE", base::isFALSE, ns = "xfun")
plot(Ecoqui_data)
summary(Ecoqui_data)
head(Ecoqui_data@PA.table) 

# Define model options
coqui.MAXENT <- list('_allData_allRun' = list(path_to_maxent.jar = "maxent",
                                              linear = TRUE, quadratic = TRUE, product = FALSE, threshold = FALSE, hinge = TRUE,
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
  var.import = 3,
  metric.eval = c('BOYCE','TSS','ROC'),
  modeling.id = "Ecoqui_sdm"
)
saveRDS(Ecoqui_model, "Ecoqui_rev_model.rds")

# Get model evaluation scores
Ecoqui_model_scores <- get_evaluations(Ecoqui_model)
dim(Ecoqui_model_scores)
dimnames(Ecoqui_model_scores)
(Ecoqui_model_eval.scor_mean <- aggregate(data = Ecoqui_model_scores, calibration ~ metric.eval, FUN = mean))
  # TSS=0.586, ROC=0.870 (old)
  # TSS=0.435, ROC=0.790 (new)
  # TSS=0.672, ROC=0.909, BOYCE=0.933 (rev)
  
# Plot model evaluation scores
bm_PlotEvalMean(Ecoqui_model)

# Check variable importance
(Ecoqui_model_var_import <- get_variables_importance(Ecoqui_model))

# Make the mean of variable importance by algorithm
(Ecoqui_var.imp_mean <- aggregate(data = Ecoqui_model_var_import, var.imp ~ expl.var, FUN = mean))
  # bio1=0.253, bio2=0.059, bio3=0.129, bio5=0.020, bio14=0.410, bio19=0.051 (old)
  # bio1=0.468, bio2=0.124, bio4=0.001, bop12=0.466, bio15=0.002, bio19=0.011 (new)
  # PC1=0.576, PC2=0.701 (rev)

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
    metric.eval = c('BOYCE','TSS','ROC'),
    metric.select = "BOYCE",
    var.import = 3,
    nb.cpu = 120
  )
saveRDS(Ecoqui_ensemble_model, "Ecoqui_rev_ensemble_model.rds")

# Assess ensemble models quality ----
(Ecoqui_ensemble_model_scores <- get_evaluations(Ecoqui_ensemble_model))
  # TSS=0.572, ROC=0.867 (old)
  # TSS=0.424, ROC=0.786 (new)
  # TSS=0.650, ROC=0.880, BOYCE=0.798 (rev)

# Check variable importance
(Ecoqui_ensemble_model_var_import <- get_variables_importance(Ecoqui_ensemble_model))
(Ecoqui_ensemble_var.imp_mean <- aggregate(data = Ecoqui_ensemble_model_var_import, var.imp ~ expl.var, FUN = mean))
  # bio1=0.247, bio2=0.057, bio3=0.123, bio5=0.012, bio14=0.406, bio19=0.037 (old)
  # bio1=0.469, bio2=0.118, bio4=0.0001, bio12=0.478, bio15=0.003, bio19=0.012 (new)
  # PC1=0.622, PC2=0.760 (rev)

# Model response plots
tiff("Ecoqui_rev_ensemble_eval_plot.tiff", width=664, height=664)
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
#terra::writeRaster(Ecoqui_ensemble_present.pred, filename = "Ecoqui_rev_ensemble_present.tif", filetype = "GTiff", overwrite = T)
#terra::writeRaster(Ecoqui_ensemble_present.pred.bin, filename = "Ecoqui_rev_ensemble_present.bin.tif", filetype = "GTiff", overwrite = T)

#for (year in c("2021-2040","2041-2060","2061-2080","2081-2100")){
for (year in c("2041-2060","2081-2100")){
  for (model in c("ACCESS-CM2","INM-CM5-0","MPI-ESM1-2-HR","IPSL-CM6A-LR","UKESM1-0-LL")){
    for (scen in c("ssp126", "ssp585")) {
      #envs_files.name <- paste("Ecoqui_envs",year,scen, sep="_")
      envs_files.name <- paste("Ecoqui_envs",model,year,scen, sep="_")
      #assign(envs_files.name, list.files(path = paste0("coqui_",year,"/",scen), pattern = ".tif$", full.names=TRUE))
      assign(envs_files.name, list.files(path = paste0("coqui_",year,"/",model,"/",scen), pattern = ".tif$", full.names=TRUE))
      #envs_stack.name <- paste0("Ecoqui_",year,"_",scen,".stack")
      envs_stack.name <- paste0("Ecoqui_",model,"_",year,"_",scen,".stack")
      assign(envs_stack.name, stack(get(envs_files.name)))
      envs_stack <- get(envs_stack.name)
      #new_env_names <- sub(paste0("wc2\\.1_30s_bioc_HadGEM3\\.GC31\\.LL_",scen,"_",strsplit(year, "-")[[1]][1],".",strsplit(year, "-")[[1]][2],"_"), "bio", names(envs_stack))
      new_env_names <- sub(paste0("wc2\\.1_2\\.5m_bioc_.*_1\\."), "Band_1.", names(envs_stack))
      names(envs_stack) <- new_env_names
      #sngl.name <- paste0("Ecoqui_model_proj_",year,"_",scen)
      sngl.name <- paste0("Ecoqui_rev_model_proj_",model,"_",year,"_",scen)
      assign(sngl.name, BIOMOD_Projection(
        bm.mod = Ecoqui_model,
        models.chosen = "all",
        new.env = envs_stack,
        #proj.name = paste0("Ecoqui_",year,"_",scen),
        proj.name = paste0("Ecoqui_",model,"_",year,"_",scen),
        metric.binary = "all",
        output.format = ".tif",
        do.stack = FALSE
      ))
      saveRDS(get(sngl.name), paste0(sngl.name,".rds"))
      #esmbl.name <- paste0("Ecoqui_ensemble_proj_",year,"_",scen)
      esmbl.name <- paste0("Ecoqui_rev_ensemble_proj_",model,"_",year,"_",scen)
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
      #pred_file.name <- paste0(paste("Ecoqui_ensemble",year,scen, sep="_"),".pred")
      pred_file.name <- paste0(paste("Ecoqui_rev_ensemble",model,year,scen, sep="_"),".pred")
      assign(pred_file.name, get_predictions(get(esmbl.name)))
      #pred.bin_file.name <- paste0(paste("Ecoqui_ensemble",year,scen, sep="_"),".pred.bin")
      pred.bin_file.name <- paste0(paste("Ecoqui_rev_ensemble",model,year,scen, sep="_"),".pred.bin")
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
      pred_name <- paste0("Ecoqui_rev_ensemble_",model,"_",year,"_",scen,".pred")
      preds[[model]] <- get(pred_name)
    }
    
    # terra stack
    preds_stack <- rast(preds)
    
    # weighted mean by BOYCE
    weights <- c("ACCESS-CM2" = 0.20, "INM-CM5-0" = 0.20, "MPI-ESM1-2-HR"= 0.20, "IPSL-CM6A-LR" = 0.20, "UKESM1-0-LL"  = 0.20)
    weights <- weights / sum(weights)
    weights_vec <- weights[names(preds_stack)]
    gcm_ensemble <- weighted.mean(preds_stack, w = weights_vec, na.rm = TRUE)
    
    # save
    assign(paste0("Ecoqui_rev_GCMensemble_",year,"_",scen), gcm_ensemble)
    
    # export
    terra::writeRaster(gcm_ensemble,
                       filename = paste0("Ecoqui_rev_GCMensemble_",year,"_",scen,".tif"),
                       filetype = "GTiff",
                       overwrite = TRUE)
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
  PA.nb.rep = 50,
  PA.nb.absences = 1000,
  PA.strategy = 'random',
  filter.raster = TRUE
)

plot(Ejohnstonei_data)
summary(Ejohnstonei_data)
head(Ejohnstonei_data@PA.table) 

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
  CV.nb.rep = 5,
  CV.perc = 0.8,
  var.import = 3,
  metric.eval = c('BOYCE','TSS','ROC'),
  modeling.id = "Ejohnstonei_sdm"
)
saveRDS(Ejohnstonei_model, "Ejohnstonei_rev_model.rds")

# Get model evaluation scores
Ejohnstonei_model_scores <- get_evaluations(Ejohnstonei_model)
dim(Ejohnstonei_model_scores)
dimnames(Ejohnstonei_model_scores)
(Ejohnstonei_model_eval.scor_mean <- aggregate(data = Ejohnstonei_model_scores, calibration ~ metric.eval, FUN = mean))
  # TSS=0.462, ROC=0.770 (old)
  # TSS=0.706, ROC=0.897 (new)
  # TSS=0.400, ROC=0.744, BOYCE=0.815 (rev)

# Plot model evaluation scores
bm_PlotEvalMean(Ejohnstonei_model)

# Check variable importance
(Ejohnstonei_model_var_import <- get_variables_importance(Ejohnstonei_model))

# Make the mean of variable importance by algorithm
(Ejohnstonei_var.imp_mean <- aggregate(data = Ejohnstonei_model_var_import, var.imp ~ expl.var, FUN = mean))
  # bio1=0.481, bio2=0.337, bio3=0.064, bio12=0.514, bio14=0.326, bio18=114 (old)
  # bio1=0.145, bio2=0.121, bio3=0.272, bio12=0.178, bio14=0.549 (new)  
  # PC1=0.741, PC2=0.264 (rev)

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
    metric.select = "BOYCE",
    var.import = 3,
    nb.cpu = 120
  )
saveRDS(Ejohnstonei_ensemble_model, "Ejohnstonei_rev_ensemble_model.rds")

# Assess ensemble models quality ----
(Ejohnstonei_ensemble_model_scores <- get_evaluations(Ejohnstonei_ensemble_model))
  # TSS=0.417, ROC=0.773 (old)
  # TSS=0.708, ROC=0.891 (new)
  # TSS=0.258, ROC=0.562, BOYCE=0.561 (rev)

# Check variable importance
(Ejohnstonei_ensemble_model_var_import <- get_variables_importance(Ejohnstonei_ensemble_model))
(Ejohnstonei_ensemble_var.imp_mean <- aggregate(data = Ejohnstonei_ensemble_model_var_import, var.imp ~ expl.var, FUN = mean))
  # bio1=0.524, bio2=0.380, bio3=0.040, bio12=0.536, bio14=0.258, bio18=0.119 (old)
  # bio1=0.101, bio2=0.140, bio3=0.250, bio12=0.123, bio14=0.574 (new)
  # PC1=0.828, PC2=0.163 (rev)

# Model response plots
tiff("Ejohnstonei_rev_ensemble_eval_plot.tiff", width=664, height=664)
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
#terra::writeRaster(Ejohnstonei_ensemble_present.pred, filename = "Ejohnstonei_rev_ensemble_present.tif", filetype  = "GTiff", overwrite = T)
#terra::writeRaster(Ejohnstonei_ensemble_present.pred.bin, filename = "Ejohnstonei_rev_ensemble_present.bin.tif", filetype = "GTiff", overwrite = T)

for (year in c("2041-2060","2081-2100")){
  for (model in c("ACCESS-CM2","INM-CM5-0","MPI-ESM1-2-HR","IPSL-CM6A-LR","UKESM1-0-LL")){
    for (scen in c("ssp126", "ssp585")) {
      envs_files.name <- paste("Ejohnstonei_envs",model,year,scen, sep="_")
      assign(envs_files.name, list.files(path = paste0("johnstonei_",year,"/",model,"/",scen), pattern = ".tif$", full.names=TRUE))
      envs_stack.name <- paste0("Ejohnstonei_",model,"_",year,"_",scen,".stack")
      assign(envs_stack.name, stack(get(envs_files.name)))
      envs_stack <- get(envs_stack.name)
      new_env_names <- sub(paste0("wc2\\.1_2\\.5m_bioc_.*_1\\."), "Band_1.", names(envs_stack))
      names(envs_stack) <- new_env_names
      sngl.name <- paste0("Ejohnstonei_rev_model_proj_",model,"_",year,"_",scen)
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
      esmbl.name <- paste0("Ejohnstonei_rev_ensemble_proj_",model,"_",year,"_",scen)
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
      pred_file.name <- paste0(paste("Ejohnstonei_rev_ensemble",model,year,scen, sep="_"),".pred")
      assign(pred_file.name, get_predictions(get(esmbl.name)))
      pred.bin_file.name <- paste0(paste("Ejohnstonei_rev_ensemble",model,year,scen, sep="_"),".pred.bin")
      assign(pred.bin_file.name, get_predictions(get(esmbl.name), metric.binary = "BOYCE"))
    }
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
  resp.name = "E.planirostris2",
  PA.nb.rep = 50,
  PA.nb.absences = 1000,
  PA.strategy = 'random',
  filter.raster = TRUE
)

plot(Eplanirostris_data)
summary(Eplanirostris_data)
head(Eplanirostris_data@PA.table) 

# Define model options
Eplanirostris_opt.MAXENT <- list('_allData_allRun' = list(path_to_maxent.jar = "maxent",
                                                         linear = TRUE, quadratic = TRUE, product = FALSE, threshold = FALSE, hinge = TRUE,
                                                         betamultiplier = 0.5, memory_allocated = 2048))
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
  CV.nb.rep = 5,
  CV.perc = 0.8,
  var.import = 3,
  metric.eval = c('BOYCE','TSS','ROC'),
  modeling.id = "Eplanirostris_sdm"
)
saveRDS(Eplanirostris_model, "Eplanirostris_rev_model.rds")

# Get model evaluation scores
Eplanirostris_model_scores <- get_evaluations(Eplanirostris_model)
dim(Eplanirostris_model_scores)
dimnames(Eplanirostris_model_scores)
(Eplanirostris_model_eval.scor_mean <- aggregate(data = Eplanirostris_model_scores, calibration ~ metric.eval, FUN = mean))
  # TSS=0.442, ROC=0.784 (old)
  # TSS=0.536, ROC=0.845 (new)
  # TSS=0.533, ROC=0.845 (new2)
  # TSS=0.519, ROC=0.838, BOYCE=0.984 (rev)

# Plot model evaluation scores
bm_PlotEvalMean(Eplanirostris_model)

# Check variable importance
(Eplanirostris_model_var_import <- get_variables_importance(Eplanirostris_model))

# Make the mean of variable importance by algorithm
(Eplanirostris_var.imp_mean <- aggregate(data = Eplanirostris_model_var_import, var.imp ~ expl.var, FUN = mean))
  # bio1=0.381, bio2=0.043, bio3=0.159, bio5=0.137, bio8=0.094, bio12=0.108, bio14=0.107, bio18=118 (old)
  # bio1=0.237, bio2=0.029, bio3=0.063, bio5=0.205, bio8=0.471, bio12=0.150, bio14=0.068, bio15=0.067, bio18=0.087 (new)
  # bio1=0.191, bio2=0.050, bio3=0.060, bio5=0.165, bio8=0.510, bio12=0.134, bio14=0.092, bio15=0.040, bio18=0.073 (new2)
  # PC1=0.488, PC2=0.546 (rev)

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
    metric.select = "BOYCE",
    var.import = 3,
    nb.cpu = 120
  )
saveRDS(Eplanirostris_ensemble_model, "Eplanirostris_rev_ensemble_model.rds")

# Assess ensemble models quality ----
(Eplanirostris_ensemble_model_scores <- get_evaluations(Eplanirostris_ensemble_model))
  # TSS=0.416, ROC=0.776 (old)
  # TSS=0.532, ROC=0.837 (new)
  # TSS=0.521, ROC=0.839 (new2)
  # TSS=0.428, ROC=0.761, BOYCE=0.709 (rev)

# Check variable importance
(Eplanirostris_ensemble_model_var_import <- get_variables_importance(Eplanirostris_ensemble_model))

# Make the mean of variable importance by algorithm
(Eplanirostris_ensemble_var.imp_mean <- aggregate(data = Eplanirostris_ensemble_model_var_import, var.imp ~ expl.var, FUN = mean))
  # bio1=0.375, bio2=0.035, bio3=0.152, bio5=0.133, bio8=0.096, bio12=0.106, bio14=0.104, bio18=114 (old)
  # bio1=0.238, bio2=0.029, bio3=0.058, bio5=0.201, bio8=0.462, bio12=0.140, bio14=0.061, bio15=0.061, bio18=0.077 (new)
  # bio1=0.205, bio2=0.052, bio3=0.055, bio5=0.169, bio8=0.518, bio12=0.132, bio14=0.095, bio15=0.030, bio18=0.063 (new2)
  # PC1=0.570, PC2=0.584 (rev)

# Model response plots
tiff("Eplanirostris_rev_ensemble_eval_plot.tiff", width=664, height=664)
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
Eplanirostris_ensemble_present.pred.bin <- get_predictions(Eplanirostris_ensemble_model_proj_present, metric.binary = "TSS") #This is the binary representation
plot(Eplanirostris_ensemble_present.pred)
plot(Eplanirostris_ensemble_present.pred.bin)
#terra::writeRaster(Eplanirostris_ensemble_present.pred, filename = "Eplanirostris_ensemble_present.tif", filetype = "GTiff", overwrite = T)
#terra::writeRaster(Eplanirostris_ensemble_present.pred.bin, filename = "Eplanirostris_ensemble_present.bin.tif", filetyype = "GTiff", overwrite = T)

for (year in c("2041-2060","2081-2100")){
  for (model in c("ACCESS-CM2","INM-CM5-0","MPI-ESM1-2-HR","IPSL-CM6A-LR","UKESM1-0-LL")){
    for (scen in c("ssp126", "ssp585")) {
      envs_files.name <- paste("Eplanirostris_envs",model,year,scen, sep="_")
      assign(envs_files.name, list.files(path = paste0("planirostris_",year,"/",model,"/",scen), pattern = ".tif$", full.names=TRUE))
      envs_stack.name <- paste0("Eplanirostris_",model,"_",year,"_",scen,".stack")
      assign(envs_stack.name, stack(get(envs_files.name)))
      envs_stack <- get(envs_stack.name)
      new_env_names <- sub(paste0("wc2\\.1_2\\.5m_bioc_.*_1\\."), "Band_1.", names(envs_stack))
      names(envs_stack) <- new_env_names
      sngl.name <- paste0("Eplanirostris_rev_model_proj_",model,"_",year,"_",scen)
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
      esmbl.name <- paste0("Eplanirostris_rev_ensemble_proj_",model,"_",year,"_",scen)
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
      pred_file.name <- paste0(paste("Eplanirostris_rev_ensemble",model,year,scen, sep="_"),".pred")
      assign(pred_file.name, get_predictions(get(esmbl.name)))
      pred.bin_file.name <- paste0(paste("Eplanirostris_rev_ensemble",model,year,scen, sep="_"),".pred.bin")
      assign(pred.bin_file.name, get_predictions(get(esmbl.name), metric.binary = "BOYCE"))
    }
  }
} 

# Repeat above steps for Eantillensis
# Biomod Maxent
Eantillensis_pre <- Eantillensis_occs
Eantillensis_pre$presence <- 1
Eantillensis_abs <- Eantillensis_bgpoints
Eantillensis_abs$presence <- NA
Eantillensis_res <- rbind(Eantillensis_pre, Eantillensis_abs)
Eantillensis_res %>% head()
Eantillensis_res %>% tail()

# Format the data
Eantillensis_data <- BIOMOD_FormatingData(
  resp.var = Eantillensis_res['presence'],
  resp.xy = Eantillensis_res[, c('Lon', 'Lat')],
  expl.var = Eantillensis_envs,
  resp.name = "E.antillensis",
  PA.nb.rep = 50,
  PA.nb.absences = 1000, # due to the small range, all available cells were chosen as background points which was n=351; thus, only one set of 351 background points were used.
  PA.strategy = 'random',
  filter.raster = TRUE
)

plot(Eantillensis_data)
summary(Eantillensis_data)
head(Eantillensis_data@PA.table) 

# Define model options
Eantillensis_opt.MAXENT <- list('_allData_allRun' = list(path_to_maxent.jar = "maxent",
                                                        linear = TRUE, quadratic = TRUE, product = FALSE, threshold = FALSE, hinge = TRUE,
                                                        betamultiplier = 3, memory_allocated = 2048))
Eantillensis_opt.val <- list(MAXENT.binary.MAXENT.MAXENT = Eantillensis_opt.MAXENT)
Eantillensis_opt <- bm_ModelingOptions(
  data.type = 'binary',
  models = c("MAXENT"),
  strategy = 'user.defined',
  user.val = Eantillensis_opt.val
)

# Run model
Eantillensis_model <- BIOMOD_Modeling(
  bm.format = Eantillensis_data,
  models = c("MAXENT"),
  bm.options = Eantillensis_opt,
  CV.strategy = 'random',
  CV.nb.rep = 5,
  CV.perc = 0.8,
  var.import = 3,
  metric.eval = c('BOYCE','TSS','ROC'),
  modeling.id = "Eantillensis_sdm"
)
saveRDS(Eantillensis_model, "Eantillensis_rev_model.rds")

# Get model evaluation scores
Eantillensis_model_scores <- get_evaluations(Eantillensis_model)
dim(Eantillensis_model_scores)
dimnames(Eantillensis_model_scores)
(Eantillensis_model_eval.scor_mean <- aggregate(data = Eantillensis_model_scores, calibration ~ metric.eval, FUN = mean))
  # TSS=0.229, ROC=0.657, BOYCE=0.794 (rev)

# Plot model evaluation scores
bm_PlotEvalMean(Eantillensis_model)

# Check variable importance
(Eantillensis_model_var_import <- get_variables_importance(Eantillensis_model))

# Make the mean of variable importance by algorithm
(Eantillensis_var.imp_mean <- aggregate(data = Eantillensis_model_var_import, var.imp ~ expl.var, FUN = mean))
  # PC1=0.819, PC2=0.381 (rev)

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
    metric.select = "BOYCE",
    var.import = 3,
    nb.cpu = 120
  )
saveRDS(Eantillensis_ensemble_model, "Eantillensis_rev_ensemble_model.rds")

# Assess ensemble models quality ----
(Eantillensis_ensemble_model_scores <- get_evaluations(Eantillensis_ensemble_model))
  # TSS=0.199, ROC=0.648, BOYCE=0.955 (rev)

# Check variable importance
(Eantillensis_ensemble_model_var_import <- get_variables_importance(Eantillensis_ensemble_model))

# Make the mean of variable importance by algorithm
(Eantillensis_ensemble_var.imp_mean <- aggregate(data = Eantillensis_ensemble_model_var_import, var.imp ~ expl.var, FUN = mean))
  # PC1=0.862, PC2=0.360 (rev)

# Model response plots
tiff("Eantillensis_rev_ensemble_eval_plot.tiff", width=664, height=664)
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
Eantillensis_ensemble_present.pred.bin <- get_predictions(Eantillensis_ensemble_model_proj_present, metric.binary = "TSS") #This is the binary representation
plot(Eantillensis_ensemble_present.pred)
plot(Eantillensis_ensemble_present.pred.bin)
#terra::writeRaster(Eantillensis_ensemble_present.pred, filename = "Eantillensis_ensemble_present.tif", filetype = "GTiff", overwrite = T)
#terra::writeRaster(Eantillensis_ensemble_present.pred.bin, filename = "Eantillensis_ensemble_present.bin.tif", filetyype = "GTiff", overwrite = T)

for (year in c("2041-2060","2081-2100")){
  for (model in c("ACCESS-CM2","INM-CM5-0","MPI-ESM1-2-HR","IPSL-CM6A-LR","UKESM1-0-LL")){
    for (scen in c("ssp126", "ssp585")) {
      envs_files.name <- paste("Eantillensis_envs",model,year,scen, sep="_")
      assign(envs_files.name, list.files(path = paste0("antillensis_",year,"/",model,"/",scen), pattern = ".tif$", full.names=TRUE))
      envs_stack.name <- paste0("Eantillensis_",model,"_",year,"_",scen,".stack")
      assign(envs_stack.name, stack(get(envs_files.name)))
      envs_stack <- get(envs_stack.name)
      new_env_names <- sub(paste0("wc2\\.1_2\\.5m_bioc_.*_1\\."), "Band_1.", names(envs_stack))
      names(envs_stack) <- new_env_names
      sngl.name <- paste0("Eantillensis_rev_model_proj_",model,"_",year,"_",scen)
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
      esmbl.name <- paste0("Eantillensis_rev_ensemble_proj_",model,"_",year,"_",scen)
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
      pred_file.name <- paste0(paste("Eantillensis_rev_ensemble",model,year,scen, sep="_"),".pred")
      assign(pred_file.name, get_predictions(get(esmbl.name)))
      pred.bin_file.name <- paste0(paste("Eantillensis_rev_ensemble",model,year,scen, sep="_"),".pred.bin")
      assign(pred.bin_file.name, get_predictions(get(esmbl.name), metric.binary = "BOYCE"))
    }
  }
} 

# Repeat above steps for Emartinicensis
# Biomod Maxent
Emartinicensis_pre <- Emartinicensis_occs
Emartinicensis_pre$presence <- 1
Emartinicensis_abs <- Emartinicensis_bgpoints
Emartinicensis_abs$presence <- NA
Emartinicensis_res <- rbind(Emartinicensis_pre, Emartinicensis_abs)
Emartinicensis_res %>% head()
Emartinicensis_res %>% tail()

# Format the data
Emartinicensis_data <- BIOMOD_FormatingData(
  resp.var = Emartinicensis_res['presence'],
  resp.xy = Emartinicensis_res[, c('Lon', 'Lat')],
  expl.var = Emartinicensis_envs,
  resp.name = "E.martinicensis",
  PA.nb.rep = 50,
  PA.nb.absences = 1000, # due to the small range, all available cells were chosen as background points which was n=557; thus, only one set of 557 background points were used.
  PA.strategy = 'random',
  filter.raster = TRUE
)

plot(Emartinicensis_data)
summary(Emartinicensis_data)
head(Emartinicensis_data@PA.table) 

# Define model options
Emartinicensis_opt.MAXENT <- list('_allData_allRun' = list(path_to_maxent.jar = "maxent",
                                                        linear = TRUE, quadratic = FALSE, product = FALSE, threshold = FALSE, hinge = FALSE,
                                                        betamultiplier = 0.5, memory_allocated = 2048))
Emartinicensis_opt.val <- list(MAXENT.binary.MAXENT.MAXENT = Emartinicensis_opt.MAXENT)
Emartinicensis_opt <- bm_ModelingOptions(
  data.type = 'binary',
  models = c("MAXENT"),
  strategy = 'user.defined',
  user.val = Emartinicensis_opt.val
)

# Run model
Emartinicensis_model <- BIOMOD_Modeling(
  bm.format = Emartinicensis_data,
  models = c("MAXENT"),
  bm.options = Emartinicensis_opt,
  CV.strategy = 'random',
  CV.nb.rep = 5,
  CV.perc = 0.8,
  var.import = 3,
  metric.eval = c('BOYCE','TSS','ROC'),
  modeling.id = "Emartinicensis_sdm"
)
saveRDS(Emartinicensis_model, "Emartinicensis_rev_model.rds")

# Get model evaluation scores
Emartinicensis_model_scores <- get_evaluations(Emartinicensis_model)
dim(Emartinicensis_model_scores)
dimnames(Emartinicensis_model_scores)
(Emartinicensis_model_eval.scor_mean <- aggregate(data = Emartinicensis_model_scores, calibration ~ metric.eval, FUN = mean))
  # TSS=0.453, ROC=0.765, BOYCE=0.805 (rev)

# Plot model evaluation scores
bm_PlotEvalMean(Emartinicensis_model)

# Check variable importance
(Emartinicensis_model_var_import <- get_variables_importance(Emartinicensis_model))

# Make the mean of variable importance by algorithm
(Emartinicensis_var.imp_mean <- aggregate(data = Emartinicensis_model_var_import, var.imp ~ expl.var, FUN = mean))
  # PC1=1.000, PC2=0.368 (rev)

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
    metric.select = "BOYCE",
    var.import = 3,
    nb.cpu = 120
  )
saveRDS(Emartinicensis_ensemble_model, "Emartinicensis_rev_ensemble_model.rds")

# Assess ensemble models quality ----
(Emartinicensis_ensemble_model_scores <- get_evaluations(Emartinicensis_ensemble_model))
  # TSS=0.471, ROC=0.769, BOYCE=0.838 (rev)

# Check variable importance
(Emartinicensis_ensemble_model_var_import <- get_variables_importance(Emartinicensis_ensemble_model))

# Make the mean of variable importance by algorithm
(Emartinicensis_ensemble_var.imp_mean <- aggregate(data = Emartinicensis_ensemble_model_var_import, var.imp ~ expl.var, FUN = mean))
  # PC1=1.000, PC2=0.352 (rev)

# Model response plots
tiff("Emartinicensis_rev_ensemble_eval_plot.tiff", width=664, height=664)
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
Emartinicensis_ensemble_present.pred.bin <- get_predictions(Emartinicensis_ensemble_model_proj_present, metric.binary = "TSS") #This is the binary representation
plot(Emartinicensis_ensemble_present.pred)
plot(Emartinicensis_ensemble_present.pred.bin)
#terra::writeRaster(Emartinicensis_ensemble_present.pred, filename = "Emartinicensis_ensemble_present.tif", filetype = "GTiff", overwrite = T)
#terra::writeRaster(Emartinicensis_ensemble_present.pred.bin, filename = "Emartinicensis_ensemble_present.bin.tif", filetyype = "GTiff", overwrite = T)

for (year in c("2041-2060","2081-2100")){
  for (model in c("ACCESS-CM2","INM-CM5-0","MPI-ESM1-2-HR","IPSL-CM6A-LR","UKESM1-0-LL")){
    for (scen in c("ssp126", "ssp585")) {
      envs_files.name <- paste("Emartinicensis_envs",model,year,scen, sep="_")
      assign(envs_files.name, list.files(path = paste0("martinicensis_",year,"/",model,"/",scen), pattern = ".tif$", full.names=TRUE))
      envs_stack.name <- paste0("Emartinicensis_",model,"_",year,"_",scen,".stack")
      assign(envs_stack.name, stack(get(envs_files.name)))
      envs_stack <- get(envs_stack.name)
      new_env_names <- sub(paste0("wc2\\.1_2\\.5m_bioc_.*_1\\."), "Band_1.", names(envs_stack))
      names(envs_stack) <- new_env_names
      sngl.name <- paste0("Emartinicensis_rev_model_proj_",model,"_",year,"_",scen)
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
      esmbl.name <- paste0("Emartinicensis_rev_ensemble_proj_",model,"_",year,"_",scen)
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
      pred_file.name <- paste0(paste("Emartinicensis_rev_ensemble",model,year,scen, sep="_"),".pred")
      assign(pred_file.name, get_predictions(get(esmbl.name)))
      pred.bin_file.name <- paste0(paste("Emartinicensis_rev_ensemble",model,year,scen, sep="_"),".pred.bin")
      assign(pred.bin_file.name, get_predictions(get(esmbl.name), metric.binary = "BOYCE"))
    }
  }
} 


# Plotting E. coqui
combined_land <- st_read("Eleutherodactylus_invasion_arcgis/combined_land.shp")

#custom_palette <- colorRampPalette(c("#FFD966", "#00A600"))
custom_palette <- colorRampPalette(c("#FFD966", "#00A600"))(5)
breaks  <- seq(0, 1, by = 0.2)                           
zlim     <- c(0, 1)

#Hawaii_window <- extent(-162.5, -154.5, 18.5, 22.5)
Hawaii_window <- extent(-156.25, -154.8, 18.7, 20.5)
#CostaCal_window <- extent(-122, -82, 6, 36)
Puerto_window <- extent(-68, -64, 17, 19)

#Ecoqui_ensemble_present.pred.bin <- raster("./E.coqui/proj_Ecoqui_present/individual_projections/E.coqui_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
Ecoqui_ensemble_present.pred <- raster("./E.coqui/proj_Ecoqui_present/individual_projections/E.coqui_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif")
combined_land_rast <- rasterize(combined_land[6], Ecoqui_ensemble_present.pred)
#raster_outline <- rasterToPolygons(combined_land_rast, dissolve = TRUE)
Ecoqui_scaled <- Ecoqui_ensemble_present.pred / 1000
Ecoqui_scaled[Ecoqui_scaled == 0] <- NA

zoom(Ecoqui_ensemble_present.pred, Hawaii_window, legend=FALSE)
plot(combined_land_rast, col = "black", legend=FALSE, asp=1, add=TRUE)
plot(Ecoqui_ensemble_present.pred, add=TRUE, legend=FALSE)

tiff("./E.coqui/Hawaii_present_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
#plot(Ecoqui_ensemble_present.pred, ext=Hawaii_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), zlim = zlim, legend=FALSE, add=TRUE)
plot(Ecoqui_scaled, ext=Hawaii_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
points(Ecoqui_occs$Lon, Ecoqui_occs$Lat, col = adjustcolor("#ff00a2ff", alpha.f = 0.5), pch=1, cex=1.2)
dev.off()

tiff("./E.coqui/Hawaii_present_cont_xpoint.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
#plot(Ecoqui_ensemble_present.pred, ext=Hawaii_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), zlim = zlim, legend=FALSE, add=TRUE)
plot(Ecoqui_scaled, ext=Hawaii_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("legend.tiff", width=166, height=664)
plot.new()
legend("center", legend = c("0", "0–0.2", "0.2–0.4", "0.4–0.6", "0.6–0.8", "0.8–1.0"), fill = c("black", custom_palette), border = "black", bty = "n", title = "Suitability")
#plot(raster_outline, ext=Hawaii_window, add = TRUE, border = "black")
dev.off()

#zoom(Ecoqui_ensemble_present.pred.bin, CostaCal_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), zlim = zlim, legend=FALSE)
zoom(Ecoqui_ensemble_present.pred, Puerto_window, legend=FALSE)
plot(combined_land_rast, col = "black", legend=FALSE, asp=1, add=TRUE)
plot(Ecoqui_ensemble_present.pred, add=TRUE, legend=FALSE)

tiff("./E.coqui/Puerto_present_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Puerto_window, asp=1)
#plot(Ecoqui_ensemble_present.pred.bin, ext=Puerto_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
plot(Ecoqui_scaled, ext=Puerto_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Puerto_window, add = TRUE, border = "black")
points(Ecoqui_occs$Lon, Ecoqui_occs$Lat, col = adjustcolor("#ff00a2ff", alpha.f = 0.5), pch=1, cex=0.6)
dev.off()

tiff("./E.coqui/Puerto_present_cont_xpoint.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Puerto_window, asp=1)
#plot(Ecoqui_ensemble_present.pred.bin, ext=Puerto_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
plot(Ecoqui_scaled, ext=Puerto_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Puerto_window, add = TRUE, border = "black")
dev.off()

#Ecoqui_ensemble_2021.2040_ssp126.pred.bin <- raster("./E.coqui/proj_Ecoqui_2021-2040_ssp126/individual_projections/E.coqui_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
#tiff("./E.coqui/Hawaii_2021.2040_ssp126_bin.tiff", width=664, height=664)
#plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
#plot(Ecoqui_ensemble_2021.2040_ssp126.pred.bin, ext=Hawaii_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Hawaii_window, add = TRUE, border = "black", lwd = 2)
#dev.off()

#tiff("./E.coqui/Puerto_2021.2040_ssp126_bin.tiff", width=664, height=664)
#plot(combined_land_rast, col = "black", legend=FALSE, ext=Puerto_window, asp=1)
#plot(Ecoqui_ensemble_2021.2040_ssp126.pred.bin, ext=Puerto_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Puerto_window, add = TRUE, border = "black", lwd = 2)
#dev.off()

#zoom(Ecoqui_ensemble_2021.2040_ssp126.pred.bin, CostaCal_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), zlim = zlim, legend=FALSE)
#plot(combined_land_rast, col = "#E6E402", legend=FALSE, asp=1, add=TRUE)
#plot(Ecoqui_ensemble_2021.2040_ssp126.pred.bin, add=TRUE, legend=FALSE)

#Ecoqui_ensemble_2041.2060_ssp126.pred.bin <- raster("./E.coqui/proj_Ecoqui_2041-2060_ssp126/individual_projections/E.coqui_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
r <- rast(c(
  "./E.coqui/proj_Ecoqui_ACCESS-CM2_2041-2060_ssp126/individual_projections/E.coqui_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.coqui/proj_Ecoqui_INM-CM5-0_2041-2060_ssp126/individual_projections/E.coqui_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.coqui/proj_Ecoqui_MPI-ESM1-2-HR_2041-2060_ssp126/individual_projections/E.coqui_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.coqui/proj_Ecoqui_IPSL-CM6A-LR_2041-2060_ssp126/individual_projections/E.coqui_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.coqui/proj_Ecoqui_UKESM1-0-LL_2041-2060_ssp126/individual_projections/E.coqui_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif"
  ))
m <- mean(r, na.rm=TRUE) # GCM ensemble
s <- m / 1000
s[s == 0] <- NA

tiff("./E.coqui/Hawaii_2041.2060_ssp126_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
#plot(Ecoqui_ensemble_2041.2060_ssp126.pred.bin, ext=Hawaii_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Hawaii_window, add = TRUE, border = "black", lwd = 2)
plot(raster(s), ext=Hawaii_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.coqui/Puerto_2041.2060_ssp126_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Puerto_window, asp=1)
#plot(Ecoqui_ensemble_2041.2060_ssp126.pred.bin, ext=Puerto_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Puerto_window, add = TRUE, border = "black", lwd = 2)
plot(raster(s), ext=Puerto_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

#zoom(Ecoqui_ensemble_2041.2060_ssp126.pred.bin, CostaCal_window, legend=FALSE)
#plot(combined_land_rast, col = "black", legend=FALSE, asp=1, add=TRUE)
#plot(Ecoqui_ensemble_2041.2060_ssp126.pred.bin, add=TRUE, legend=FALSE)

#Ecoqui_ensemble_2061.2080_ssp126.pred.bin <- raster("./E.coqui/proj_Ecoqui_2061-2080_ssp126/individual_projections/E.coqui_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
#tiff("./E.coqui/Hawaii_2061.2080_ssp126_bin.tiff", width=664, height=664)
#plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
#plot(Ecoqui_ensemble_2061.2080_ssp126.pred.bin, ext=Hawaii_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Hawaii_window, add = TRUE, border = "black", lwd = 2)
#dev.off()

#tiff("./E.coqui/Puerto_2061.2080_ssp126_bin.tiff", width=664, height=664)
#plot(combined_land_rast, col = "black", legend=FALSE, ext=Puerto_window, asp=1)
#plot(Ecoqui_ensemble_2061.2080_ssp126.pred.bin, ext=Puerto_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Puerto_window, add = TRUE, border = "black", lwd = 2)
#dev.off()

#zoom(Ecoqui_ensemble_2061.2080_ssp126.pred.bin, CostaCal_window, legend=FALSE)
#plot(combined_land_rast, col = "#E6E402", legend=FALSE, asp=1, add=TRUE)
#plot(Ecoqui_ensemble_2061.2080_ssp126.pred.bin, add=TRUE, legend=FALSE)

#Ecoqui_ensemble_2081.2100_ssp126.pred.bin <- raster("./E.coqui/proj_Ecoqui_2081-2100_ssp126/individual_projections/E.coqui_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
r <- rast(c(
  "./E.coqui/proj_Ecoqui_ACCESS-CM2_2081-2100_ssp126/individual_projections/E.coqui_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.coqui/proj_Ecoqui_INM-CM5-0_2081-2100_ssp126/individual_projections/E.coqui_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.coqui/proj_Ecoqui_MPI-ESM1-2-HR_2081-2100_ssp126/individual_projections/E.coqui_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.coqui/proj_Ecoqui_IPSL-CM6A-LR_2081-2100_ssp126/individual_projections/E.coqui_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.coqui/proj_Ecoqui_UKESM1-0-LL_2081-2100_ssp126/individual_projections/E.coqui_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif"
  ))
m <- mean(r, na.rm=TRUE) # GCM ensemble
s <- m / 1000
s[s == 0] <- NA

tiff("./E.coqui/Hawaii_2081.2100_ssp126_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(raster(s), ext=Hawaii_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.coqui/Puerto_2081.2100_ssp126_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Puerto_window, asp=1)
plot(raster(s), ext=Puerto_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

#zoom(Ecoqui_ensemble_2081.2100_ssp126.pred.bin, CostaCal_window, legend=FALSE)
#plot(combined_land_rast, col = "black", legend=FALSE, asp=1, add=TRUE)
#plot(Ecoqui_ensemble_2081.2100_ssp126.pred.bin, add=TRUE, legend=FALSE)

#Ecoqui_ensemble_2021.2040_ssp585.pred.bin <- raster("./E.coqui/proj_Ecoqui_2021-2040_ssp585/individual_projections/E.coqui_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
#tiff("./E.coqui/Hawaii_2021.2040_ssp585_bin.tiff", width=664, height=664)
#plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
#plot(Ecoqui_ensemble_2021.2040_ssp585.pred.bin, ext=Hawaii_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Hawaii_window, add = TRUE, border = "black", lwd = 2)
#dev.off()

#tiff("./E.coqui/Puerto_2021.2040_ssp585_bin.tiff", width=664, height=664)
#plot(combined_land_rast, col = "black", legend=FALSE, ext=Puerto_window, asp=1)
#plot(Ecoqui_ensemble_2021.2040_ssp585.pred.bin, ext=Puerto_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Puerto_window, add = TRUE, border = "black", lwd = 2)
#dev.off()

#zoom(Ecoqui_ensemble_2021.2040_ssp585.pred.bin, CostaCal_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), zlim = zlim, legend=FALSE)
#plot(combined_land_rast, col = "black", legend=FALSE, asp=1, add=TRUE)
#plot(Ecoqui_ensemble_2021.2040_ssp585.pred.bin, add=TRUE, legend=FALSE)

#Ecoqui_ensemble_2041.2060_ssp585.pred.bin <- raster("./E.coqui/proj_Ecoqui_2041-2060_ssp585/individual_projections/E.coqui_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
r <- rast(c(
  "./E.coqui/proj_Ecoqui_ACCESS-CM2_2041-2060_ssp585/individual_projections/E.coqui_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.coqui/proj_Ecoqui_INM-CM5-0_2041-2060_ssp585/individual_projections/E.coqui_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.coqui/proj_Ecoqui_MPI-ESM1-2-HR_2041-2060_ssp585/individual_projections/E.coqui_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.coqui/proj_Ecoqui_IPSL-CM6A-LR_2041-2060_ssp585/individual_projections/E.coqui_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.coqui/proj_Ecoqui_UKESM1-0-LL_2041-2060_ssp585/individual_projections/E.coqui_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif"
  ))
m <- mean(r, na.rm=TRUE) # GCM ensemble
s <- m / 1000
s[s == 0] <- NA

tiff("./E.coqui/Hawaii_2041-2060_ssp585_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(raster(s), ext=Hawaii_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.coqui/Puerto_2041-2060_ssp585_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Puerto_window, asp=1)
plot(raster(s), ext=Puerto_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

#zoom(Ecoqui_ensemble_2041.2060_ssp585.pred.bin, CostaCal_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), zlim = zlim, legend=FALSE)
#plot(combined_land_rast, col = "black", legend=FALSE, asp=1, add=TRUE)
#plot(Ecoqui_ensemble_2041.2060_ssp585.pred.bin, add=TRUE, legend=FALSE)

#Ecoqui_ensemble_2061.2080_ssp585.pred.bin <- raster("./E.coqui/proj_Ecoqui_2061-2080_ssp585/individual_projections/E.coqui_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
#tiff("./E.coqui/Hawaii_2061.2080_ssp585_bin.tiff", width=664, height=664)
#plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
#plot(Ecoqui_ensemble_2061.2080_ssp585.pred.bin, ext=Hawaii_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Hawaii_window, add = TRUE, border = "black", lwd = 2)
#dev.off()

#tiff("./E.coqui/Puerto_2061.2080_ssp585_bin.tiff", width=664, height=664)
#plot(combined_land_rast, col = "black", legend=FALSE, ext=Puerto_window, asp=1)
#plot(Ecoqui_ensemble_2061.2080_ssp585.pred.bin, ext=Puerto_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Puerto_window, add = TRUE, border = "black", lwd = 2)
#dev.off()

#zoom(Ecoqui_ensemble_2061.2080_ssp585.pred.bin, CostaCal_window, legend=FALSE)
#plot(combined_land_rast, col = "#E6E402", legend=FALSE, asp=1, add=TRUE)
#plot(Ecoqui_ensemble_2061.2080_ssp585.pred.bin, add=TRUE, legend=FALSE)

#Ecoqui_ensemble_2081.2100_ssp585.pred.bin <- raster("./E.coqui/proj_Ecoqui_2081-2100_ssp585/individual_projections/E.coqui_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
r <- rast(c(
  "./E.coqui/proj_Ecoqui_ACCESS-CM2_2081-2100_ssp585/individual_projections/E.coqui_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.coqui/proj_Ecoqui_INM-CM5-0_2081-2100_ssp585/individual_projections/E.coqui_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.coqui/proj_Ecoqui_MPI-ESM1-2-HR_2081-2100_ssp585/individual_projections/E.coqui_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.coqui/proj_Ecoqui_IPSL-CM6A-LR_2081-2100_ssp585/individual_projections/E.coqui_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.coqui/proj_Ecoqui_UKESM1-0-LL_2081-2100_ssp585/individual_projections/E.coqui_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif"
  ))
m <- mean(r, na.rm=TRUE) # GCM ensemble
s <- m / 1000
s[s == 0] <- NA

tiff("./E.coqui/Hawaii_2081-2100_ssp585_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(raster(s), ext=Hawaii_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.coqui/Puerto_2081-2100_ssp585_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Puerto_window, asp=1)
plot(raster(s), ext=Puerto_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

#zoom(Ecoqui_ensemble_2081.2100_ssp585.pred.bin, CostaCal_window, legend=FALSE)
#plot(combined_land_rast, col = "#E6E402", legend=FALSE, asp=1, add=TRUE)
#plot(Ecoqui_ensemble_2081.2100_ssp585.pred.bin, add=TRUE, legend=FALSE)


# Plotting E. johnstonei
johnstonei_window <- extent(-80, -45, -25, 33)

Ejohnstonei_ensemble_present.pred <- raster("./E.johnstonei/proj_Ejohnstonei_present/individual_projections/E.johnstonei_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif")
combined_land_rast <- rasterize(combined_land[6], Ejohnstonei_ensemble_present.pred)
#raster_outline <- rasterToPolygons(combined_land_rast, dissolve = TRUE)
Ejohnstonei_scaled <- Ejohnstonei_ensemble_present.pred / 1000
Ejohnstonei_scaled[Ejohnstonei_scaled == 0] <- NA

tiff("./E.johnstonei/johnstonei_present_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=johnstonei_window, asp=1)
#plot(Ejohnstonei_ensemble_present.pred.bin, ext=johnstonei_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
plot(Ejohnstonei_scaled, ext=johnstonei_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
points(Ejohnstonei_occs$Lon, Ejohnstonei_occs$Lat, col = adjustcolor("#ff00a2ff", alpha.f = 0.5), pch=1, cex=0.6)
#plot(raster_outline, ext=johnstonei_window, add = TRUE, border = "black", lwd = 2)
dev.off()

tiff("./E.johnstonei/johnstonei_present_cont_xpoint.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=johnstonei_window, asp=1)
#plot(Ejohnstonei_ensemble_present.pred.bin, ext=johnstonei_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
plot(Ejohnstonei_scaled, ext=johnstonei_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=johnstonei_window, add = TRUE, border = "black", lwd = 2)
dev.off()

#zoom(Ejohnstonei_ensemble_present.pred.bin, johnstonei_window, legend=FALSE)
#plot(combined_land_rast, col = "#E6E402", legend=FALSE, asp=1, add=TRUE)
#plot(Ejohnstonei_ensemble_present.pred.bin, add=TRUE, legend=FALSE)

#Ejohnstonei_ensemble_2021.2040_ssp126.pred.bin <- raster("./E.johnstonei/proj_Ejohnstonei_2021-2040_ssp126/individual_projections/E.johnstonei_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
#tiff("./E.johnstonei/Ejohnstonei_2021.2040_ssp126_bin.tiff", width=664, height=664)
#plot(combined_land_rast, col = "black", legend=FALSE, ext=johnstonei_window, asp=1)
#plot(Ejohnstonei_ensemble_2021.2040_ssp126.pred.bin, ext=johnstonei_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=johnstonei_window, add = TRUE, border = "black", lwd = 2)
#dev.off()

#Ejohnstonei_ensemble_2041.2060_ssp126.pred.bin <- raster("./E.johnstonei/proj_Ejohnstonei_2041-2060_ssp126/individual_projections/E.johnstonei_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
#tiff("./E.johnstonei/Ejohnstonei_2041.2060_ssp126_bin.tiff", width=664, height=664)
#plot(combined_land_rast, col = "black", legend=FALSE, ext=johnstonei_window, asp=1)
#plot(Ejohnstonei_ensemble_2041.2060_ssp126.pred.bin, ext=johnstonei_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=johnstonei_window, add = TRUE, border = "black", lwd = 2)
#dev.off()

r <- rast(c(
  "./E.johnstonei/proj_Ejohnstonei_ACCESS-CM2_2041-2060_ssp126/individual_projections/E.johnstonei_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.johnstonei/proj_Ejohnstonei_INM-CM5-0_2041-2060_ssp126/individual_projections/E.johnstonei_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.johnstonei/proj_Ejohnstonei_MPI-ESM1-2-HR_2041-2060_ssp126/individual_projections/E.johnstonei_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.johnstonei/proj_Ejohnstonei_IPSL-CM6A-LR_2041-2060_ssp126/individual_projections/E.johnstonei_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.johnstonei/proj_Ejohnstonei_UKESM1-0-LL_2041-2060_ssp126/individual_projections/E.johnstonei_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif"
  ))
m <- mean(r, na.rm=TRUE) # GCM ensemble
s <- m / 1000
s[s == 0] <- NA

tiff("./E.johnstonei/johnstonei_2041-2060_ssp126_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=johnstonei_window, asp=1)
plot(raster(s), ext=johnstonei_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

#Ejohnstonei_ensemble_2061.2080_ssp126.pred.bin <- raster("./E.johnstonei/proj_Ejohnstonei_2061-2080_ssp126/individual_projections/E.johnstonei_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
#tiff("./E.johnstonei/Ejohnstonei_2061.2080_ssp126_bin.tiff", width=664, height=664)
#plot(combined_land_rast, col = "black", legend=FALSE, ext=johnstonei_window, asp=1)
#plot(Ejohnstonei_ensemble_2061.2080_ssp126.pred.bin, ext=johnstonei_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=johnstonei_window, add = TRUE, border = "black", lwd = 2)
#dev.off()

#Ejohnstonei_ensemble_2081.2100_ssp126.pred.bin <- raster("./E.johnstonei/proj_Ejohnstonei_2081-2100_ssp126/individual_projections/E.johnstonei_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
#tiff("./E.johnstonei/Ejohnstonei_2081.2100_ssp126_bin.tiff", width=664, height=664)
#plot(combined_land_rast, col = "black", legend=FALSE, ext=johnstonei_window, asp=1)
#plot(Ejohnstonei_ensemble_2081.2100_ssp126.pred.bin, ext=johnstonei_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=johnstonei_window, add = TRUE, border = "black", lwd = 2)
#dev.off()

r <- rast(c(
  "./E.johnstonei/proj_Ejohnstonei_ACCESS-CM2_2081-2100_ssp126/individual_projections/E.johnstonei_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.johnstonei/proj_Ejohnstonei_INM-CM5-0_2081-2100_ssp126/individual_projections/E.johnstonei_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.johnstonei/proj_Ejohnstonei_MPI-ESM1-2-HR_2081-2100_ssp126/individual_projections/E.johnstonei_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.johnstonei/proj_Ejohnstonei_IPSL-CM6A-LR_2081-2100_ssp126/individual_projections/E.johnstonei_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.johnstonei/proj_Ejohnstonei_UKESM1-0-LL_2081-2100_ssp126/individual_projections/E.johnstonei_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif"
  ))
m <- mean(r, na.rm=TRUE) # GCM ensemble
s <- m / 1000
s[s == 0] <- NA

tiff("./E.johnstonei/johnstonei_2081-2100_ssp126_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=johnstonei_window, asp=1)
plot(raster(s), ext=johnstonei_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

#Ejohnstonei_ensemble_2021.2040_ssp585.pred.bin <- raster("./E.johnstonei/proj_Ejohnstonei_2021-2040_ssp585/individual_projections/E.johnstonei_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
#tiff("./E.johnstonei/Ejohnstonei_2021.2040_ssp585_bin.tiff", width=664, height=664)
#plot(combined_land_rast, col = "black", legend=FALSE, ext=johnstonei_window, asp=1)
#plot(Ejohnstonei_ensemble_2021.2040_ssp585.pred.bin, ext=johnstonei_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=johnstonei_window, add = TRUE, border = "black", lwd = 2)
#dev.off()

#Ejohnstonei_ensemble_2041.2060_ssp585.pred.bin <- raster("./E.johnstonei/proj_Ejohnstonei_2041-2060_ssp585/individual_projections/E.johnstonei_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
#tiff("./E.johnstonei/Ejohnstonei_2041.2060_ssp585_bin.tiff", width=664, height=664)
#plot(combined_land_rast, col = "black", legend=FALSE, ext=johnstonei_window, asp=1)
#plot(Ejohnstonei_ensemble_2041.2060_ssp585.pred.bin, ext=johnstonei_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=johnstonei_window, add = TRUE, border = "black", lwd = 2)
#dev.off()

r <- rast(c(
  "./E.johnstonei/proj_Ejohnstonei_ACCESS-CM2_2041-2060_ssp585/individual_projections/E.johnstonei_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.johnstonei/proj_Ejohnstonei_INM-CM5-0_2041-2060_ssp585/individual_projections/E.johnstonei_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.johnstonei/proj_Ejohnstonei_MPI-ESM1-2-HR_2041-2060_ssp585/individual_projections/E.johnstonei_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.johnstonei/proj_Ejohnstonei_IPSL-CM6A-LR_2041-2060_ssp585/individual_projections/E.johnstonei_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.johnstonei/proj_Ejohnstonei_UKESM1-0-LL_2041-2060_ssp585/individual_projections/E.johnstonei_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif"
  ))
m <- mean(r, na.rm=TRUE) # GCM ensemble
s <- m / 1000
s[s == 0] <- NA

tiff("./E.johnstonei/johnstonei_2041-2060_ssp585_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=johnstonei_window, asp=1)
plot(raster(s), ext=johnstonei_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

#Ejohnstonei_ensemble_2061.2080_ssp585.pred.bin <- raster("./E.johnstonei/proj_Ejohnstonei_2061-2080_ssp585/individual_projections/E.johnstonei_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
#tiff("./E.johnstonei/Ejohnstonei_2061.2080_ssp585_bin.tiff", width=664, height=664)
#plot(combined_land_rast, col = "black", legend=FALSE, ext=johnstonei_window, asp=1)
#plot(Ejohnstonei_ensemble_2061.2080_ssp585.pred.bin, ext=johnstonei_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=johnstonei_window, add = TRUE, border = "black", lwd = 2)
#dev.off()

#Ejohnstonei_ensemble_2081.2100_ssp585.pred.bin <- raster("./E.johnstonei/proj_Ejohnstonei_2081-2100_ssp585/individual_projections/E.johnstonei_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
#tiff("./E.johnstonei/Ejohnstonei_2081.2100_ssp585_bin.tiff", width=664, height=664)
#plot(combined_land_rast, col = "black", legend=FALSE, ext=johnstonei_window, asp=1)
#plot(Ejohnstonei_ensemble_2081.2100_ssp585.pred.bin, ext=johnstonei_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=johnstonei_window, add = TRUE, border = "black", lwd = 2)
#dev.off()

r <- rast(c(
  "./E.johnstonei/proj_Ejohnstonei_ACCESS-CM2_2081-2100_ssp585/individual_projections/E.johnstonei_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.johnstonei/proj_Ejohnstonei_INM-CM5-0_2081-2100_ssp585/individual_projections/E.johnstonei_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.johnstonei/proj_Ejohnstonei_MPI-ESM1-2-HR_2081-2100_ssp585/individual_projections/E.johnstonei_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.johnstonei/proj_Ejohnstonei_IPSL-CM6A-LR_2081-2100_ssp585/individual_projections/E.johnstonei_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.johnstonei/proj_Ejohnstonei_UKESM1-0-LL_2081-2100_ssp585/individual_projections/E.johnstonei_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif"
  ))
m <- mean(r, na.rm=TRUE) # GCM ensemble
s <- m / 1000
s[s == 0] <- NA

tiff("./E.johnstonei/johnstonei_2081-2100_ssp585_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=johnstonei_window, asp=1)
plot(raster(s), ext=johnstonei_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

# Plotting E. planirostris
#Hawaii_window <- extent(-156.25, -154.8, 18.7, 20.5)
Hawaii_window <- extent(-162.5, -154.5, 18.5, 22.5)
US_window <- extent(-105, -65, 5, 55)
#Asia_window <- extent(110, 130, 5, 25)
Asia_window <- extent(100, 130, 0, 25)

#Eplanirostris_ensemble_present.pred.bin <- raster("./E.planirostris2/proj_Eplanirostris_present/individual_projections/E.planirostris2_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
Eplanirostris_ensemble_present.pred <- raster("./E.planirostris2/proj_Eplanirostris_present/individual_projections/E.planirostris2_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo_BOYCEbin.tif")
combined_land_rast <- rasterize(combined_land[6], Eplanirostris_ensemble_present.pred) # took too long -> use ArcGIS Pro or cluster
#combined_land_rast <- raster("Eleutherodactylus_invasion_arcgis/Epla_combined_land_rast.tif")
#raster_outline <- rasterToPolygons(combined_land_rast, dissolve = TRUE) # took too long -> use ArcGIS Pro
#raster_outline <- terra::vect("Eleutherodactylus_invasion_arcgis/Epla_combined_land_poly.shp")
#raster_outline_US <- crop(raster_outline, US_window)
#raster_outline_US <- project(raster_outline_US, crs(combined_land_rast))
Eplanirostris_scaled <- Eplanirostris_ensemble_present.pred / 1000
Eplanirostris_scaled[Eplanirostris_scaled == 0] <- NA

tiff("./E.planirostris2/Hawaii_present_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
#plot(Eplanirostris_ensemble_present.pred.bin, ext=Hawaii_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
plot(Eplanirostris_scaled, ext=Hawaii_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
points(Eplanirostris_occs$Lon, Eplanirostris_occs$Lat, col = "#ff00a2ff", pch=1, cex=0.6)
#plot(raster_outline, ext=Hawaii_window, add = TRUE, border = "black", lwd = 2, col = NA)
dev.off()

tiff("./E.planirostris2/Hawaii_present_cont_xpoint.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
#plot(Eplanirostris_ensemble_present.pred.bin, ext=Hawaii_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
plot(Eplanirostris_scaled, ext=Hawaii_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Hawaii_window, add = TRUE, border = "black", lwd = 2, col = NA)
dev.off()

zoom(Eplanirostris_ensemble_present.pred, US_window, legend=FALSE)
plot(combined_land_rast, col = "#E6E402", legend=FALSE, asp=1, add=TRUE)
plot(Eplanirostris_ensemble_present.pred, add=TRUE, legend=FALSE)
points(Eplanirostris_occs$Lon, Eplanirostris_occs$Lat, col = "#ff00a2ff", pch=1, cex=0.6)

tiff("./E.planirostris2/US_present_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=US_window, asp=1)
#plot(Eplanirostris_ensemble_present.pred.bin, ext=US_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(Eplanirostris_scaled, ext=US_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
plot(Eplanirostris_scaled, ext=US_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
points(Eplanirostris_occs$Lon, Eplanirostris_occs$Lat, col = "#ff00a2ff", pch=1, cex=0.6)
#plot(raster_outline_US, border = "black", lwd = 0.5, add = TRUE, col = NA)
dev.off()

tiff("./E.planirostris2/US_present_cont_xpoint.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=US_window, asp=1)
#plot(Eplanirostris_ensemble_present.pred.bin, ext=US_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(Eplanirostris_scaled, ext=US_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
plot(Eplanirostris_scaled, ext=US_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
#plot(raster_outline_US, border = "black", lwd = 0.5, add = TRUE, col = NA)
dev.off()

#zoom(Eplanirostris_ensemble_present.pred.bin, ext=Asia_window, legend=FALSE)
#plot(combined_land_rast, col = "#E6E402", legend=FALSE, asp=1, add=TRUE)
#plot(Eplanirostris_ensemble_present.pred.bin, add=TRUE, legend=FALSE)

tiff("./E.planirostris2/Asia_present_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Asia_window, asp=1)
#plot(Eplanirostris_ensemble_present.pred.bin, ext=Asia_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
plot(Eplanirostris_scaled, ext=Asia_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
points(Eplanirostris_occs$Lon, Eplanirostris_occs$Lat, col = "#ff00a2ff", pch=1, cex=0.6)
#plot(raster_outline, ext=Asia_window, add = TRUE, border = "black", lwd = 2, col = NA)
dev.off()

tiff("./E.planirostris2/Asia_present_cont_xpoint.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Asia_window, asp=1)
#plot(Eplanirostris_ensemble_present.pred.bin, ext=Asia_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
plot(Eplanirostris_scaled, ext=Asia_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Asia_window, add = TRUE, border = "black", lwd = 2, col = NA)
dev.off()

#Eplanirostris_ensemble_2021.2040_ssp126.pred.bin <- raster("./E.planirostris2/proj_Eplanirostris_2021-2040_ssp126/individual_projections/E.planirostris2_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
#tiff("./E.planirostris2/Hawaii_2021.2040_ssp126_bin.tiff", width=664, height=664)
#plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
#plot(Eplanirostris_ensemble_2021.2040_ssp126.pred.bin, ext=Hawaii_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Hawaii_window, add = TRUE, border = "black", lwd = 2, col = NA)
#dev.off()

#tiff("./E.planirostris2/US_2021.2040_ssp126_bin.tiff", width=664, height=664)
#plot(combined_land_rast, col = "black", legend=FALSE, ext=US_window, asp=1)
#plot(Eplanirostris_ensemble_2021.2040_ssp126.pred.bin, ext=US_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=US_window, add = TRUE, border = "black", lwd = 2, col = NA)
#dev.off()

#tiff("./E.planirostris2/Asia_2021.2040_ssp126_bin.tiff", width=664, height=664)
#plot(combined_land_rast, col = "black", legend=FALSE, ext=Asia_window, asp=1)
#plot(Eplanirostris_ensemble_2021.2040_ssp126.pred.bin, ext=Asia_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Asia_window, add = TRUE, border = "black", lwd = 2, col = NA)
#dev.off()

#Eplanirostris_ensemble_2041.2060_ssp126.pred.bin <- raster("./E.planirostris2/proj_Eplanirostris_2041-2060_ssp126/individual_projections/E.planirostris2_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
#tiff("./E.planirostris2/Hawaii_2041.2060_ssp126_bin.tiff", width=664, height=664)
#plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
#plot(Eplanirostris_ensemble_2041.2060_ssp126.pred.bin, ext=Hawaii_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Hawaii_window, add = TRUE, border = "black", lwd = 2, col = NA)
#dev.off()

r <- rast(c(
  "./E.planirostris2/proj_Eplanirostris_ACCESS-CM2_2041-2060_ssp126/individual_projections/E.planirostris2_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.planirostris2/proj_Eplanirostris_INM-CM5-0_2041-2060_ssp126/individual_projections/E.planirostris2_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.planirostris2/proj_Eplanirostris_MPI-ESM1-2-HR_2041-2060_ssp126/individual_projections/E.planirostris2_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.planirostris2/proj_Eplanirostris_IPSL-CM6A-LR_2041-2060_ssp126/individual_projections/E.planirostris2_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.planirostris2/proj_Eplanirostris_UKESM1-0-LL_2041-2060_ssp126/individual_projections/E.planirostris2_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif"
  ))
m <- mean(r, na.rm=TRUE) # GCM ensemble
s <- m / 1000
s[s == 0] <- NA

tiff("./E.planirostris2/Hawaii_2041-2060_ssp126_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(raster(s), ext=Hawaii_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

#tiff("./E.planirostris2/US_2041.2060_ssp126_bin.tiff", width=664, height=664)
#plot(combined_land_rast, col = "black", legend=FALSE, ext=US_window, asp=1)
#plot(Eplanirostris_ensemble_2041.2060_ssp126.pred.bin, ext=US_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=US_window, add = TRUE, border = "black", lwd = 2, col = NA)
#dev.off()

tiff("./E.planirostris2/US_2041-2060_ssp126_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=US_window, asp=1)
plot(raster(s), ext=US_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

#tiff("./E.planirostris2/Asia_2041.2060_ssp126_bin.tiff", width=664, height=664)
#plot(combined_land_rast, col = "black", legend=FALSE, ext=Asia_window, asp=1)
#plot(Eplanirostris_ensemble_2041.2060_ssp126.pred.bin, ext=Asia_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Asia_window, add = TRUE, border = "black", lwd = 2, col = NA)
#dev.off()

tiff("./E.planirostris2/Asia_2041-2060_ssp126_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Asia_window, asp=1)
plot(raster(s), ext=Asia_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

#Eplanirostris_ensemble_2061.2080_ssp126.pred.bin <- raster("./E.planirostris2/proj_Eplanirostris_2061-2080_ssp126/individual_projections/E.planirostris2_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
#tiff("./E.planirostris2/Hawaii_2061.2080_ssp126_bin.tiff", width=664, height=664)
#plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
#plot(Eplanirostris_ensemble_2061.2080_ssp126.pred.bin, ext=Hawaii_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Hawaii_window, add = TRUE, border = "black", lwd = 2, col = NA)
#dev.off()

#tiff("./E.planirostris2/US_2061.2080_ssp126_bin.tiff", width=664, height=664)
#plot(combined_land_rast, col = "black", legend=FALSE, ext=US_window, asp=1)
#plot(Eplanirostris_ensemble_2061.2080_ssp126.pred.bin, ext=US_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=US_window, add = TRUE, border = "black", lwd = 2, col = NA)
#dev.off()

#tiff("./E.planirostris2/Asia_2061.2080_ssp126_bin.tiff", width=664, height=664)
#plot(combined_land_rast, col = "black", legend=FALSE, ext=Asia_window, asp=1)
#plot(Eplanirostris_ensemble_2061.2080_ssp126.pred.bin, ext=Asia_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Asia_window, add = TRUE, border = "black", lwd = 2, col = NA)
#dev.off()

#Eplanirostris_ensemble_2081.2100_ssp126.pred.bin <- raster("./E.planirostris2/proj_Eplanirostris_2081-2100_ssp126/individual_projections/E.planirostris2_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
#tiff("./E.planirostris2/Hawaii_2081.2100_ssp126_bin.tiff", width=664, height=664)
#plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
#plot(Eplanirostris_ensemble_2081.2100_ssp126.pred.bin, ext=Hawaii_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Hawaii_window, add = TRUE, border = "black", lwd = 2, col = NA)
#dev.off()

r <- rast(c(
  "./E.planirostris2/proj_Eplanirostris_ACCESS-CM2_2081-2100_ssp126/individual_projections/E.planirostris2_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.planirostris2/proj_Eplanirostris_INM-CM5-0_2081-2100_ssp126/individual_projections/E.planirostris2_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.planirostris2/proj_Eplanirostris_MPI-ESM1-2-HR_2081-2100_ssp126/individual_projections/E.planirostris2_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.planirostris2/proj_Eplanirostris_IPSL-CM6A-LR_2081-2100_ssp126/individual_projections/E.planirostris2_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.planirostris2/proj_Eplanirostris_UKESM1-0-LL_2081-2100_ssp126/individual_projections/E.planirostris2_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif"
  ))
m <- mean(r, na.rm=TRUE) # GCM ensemble
s <- m / 1000
s[s == 0] <- NA

tiff("./E.planirostris2/Hawaii_2081-2100_ssp126_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(raster(s), ext=Hawaii_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

#tiff("./E.planirostris2/US_2081.2100_ssp126_bin.tiff", width=664, height=664)
#plot(combined_land_rast, col = "black", legend=FALSE, ext=US_window, asp=1)
#plot(Eplanirostris_ensemble_2081.2100_ssp126.pred.bin, ext=US_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=US_window, add = TRUE, border = "black", lwd = 2, col = NA)
#dev.off()

tiff("./E.planirostris2/US_2081-2100_ssp126_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=US_window, asp=1)
plot(raster(s), ext=US_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

#tiff("./E.planirostris2/Asia_2081.2100_ssp126_bin.tiff", width=664, height=664)
#plot(combined_land_rast, col = "black", legend=FALSE, ext=Asia_window, asp=1)
#plot(Eplanirostris_ensemble_2081.2100_ssp126.pred.bin, ext=Asia_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Asia_window, add = TRUE, border = "black", lwd = 2, col = NA)
#dev.off()

tiff("./E.planirostris2/Asia_2081-2100_ssp126_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Asia_window, asp=1)
plot(raster(s), ext=Asia_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

#Eplanirostris_ensemble_2021.2040_ssp585.pred.bin <- raster("./E.planirostris2/proj_Eplanirostris_2021-2040_ssp585/individual_projections/E.planirostris2_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
#tiff("./E.planirostris2/Hawaii_2021.2040_ssp585_bin.tiff", width=664, height=664)
#plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
#plot(Eplanirostris_ensemble_2021.2040_ssp585.pred.bin, ext=Hawaii_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Hawaii_window, add = TRUE, border = "black", lwd = 2, col = NA)
#dev.off()

#tiff("./E.planirostris2/US_2021.2040_ssp585_bin.tiff", width=664, height=664)
#plot(combined_land_rast, col = "black", legend=FALSE, ext=US_window, asp=1)
#plot(Eplanirostris_ensemble_2021.2040_ssp585.pred.bin, ext=US_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=US_window, add = TRUE, border = "black", lwd = 2, col = NA)
#dev.off()

#tiff("./E.planirostris2/Asia_2021.2040_ssp585_bin.tiff", width=664, height=664)
#plot(combined_land_rast, col = "black", legend=FALSE, ext=Asia_window, asp=1)
#plot(Eplanirostris_ensemble_2021.2040_ssp585.pred.bin, ext=Asia_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Asia_window, add = TRUE, border = "black", lwd = 2, col = NA)
#dev.off()

#Eplanirostris_ensemble_2041.2060_ssp585.pred.bin <- raster("./E.planirostris2/proj_Eplanirostris_2041-2060_ssp585/individual_projections/E.planirostris2_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
#tiff("./E.planirostris2/Hawaii_2041.2060_ssp585_bin.tiff", width=664, height=664)
#plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
#plot(Eplanirostris_ensemble_2041.2060_ssp585.pred.bin, ext=Hawaii_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Hawaii_window, add = TRUE, border = "black", lwd = 2, col = NA)
#dev.off()

r <- rast(c(
  "./E.planirostris2/proj_Eplanirostris_ACCESS-CM2_2041-2060_ssp585/individual_projections/E.planirostris2_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.planirostris2/proj_Eplanirostris_INM-CM5-0_2041-2060_ssp585/individual_projections/E.planirostris2_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.planirostris2/proj_Eplanirostris_MPI-ESM1-2-HR_2041-2060_ssp585/individual_projections/E.planirostris2_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.planirostris2/proj_Eplanirostris_IPSL-CM6A-LR_2041-2060_ssp585/individual_projections/E.planirostris2_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.planirostris2/proj_Eplanirostris_UKESM1-0-LL_2041-2060_ssp585/individual_projections/E.planirostris2_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif"
  ))
m <- mean(r, na.rm=TRUE) # GCM ensemble
s <- m / 1000
s[s == 0] <- NA

tiff("./E.planirostris2/Hawaii_2041-2060_ssp585_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(raster(s), ext=Hawaii_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

#tiff("./E.planirostris2/US_2041.2060_ssp585_bin.tiff", width=664, height=664)
#plot(combined_land_rast, col = "black", legend=FALSE, ext=US_window, asp=1)
#plot(Eplanirostris_ensemble_2041.2060_ssp585.pred.bin, ext=US_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=US_window, add = TRUE, border = "black", lwd = 2, col = NA)
#dev.off()

tiff("./E.planirostris2/US_2041-2060_ssp585_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=US_window, asp=1)
plot(raster(s), ext=US_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

#tiff("./E.planirostris2/Asia_2041.2060_ssp585_bin.tiff", width=664, height=664)
#plot(combined_land_rast, col = "black", legend=FALSE, ext=Asia_window, asp=1)
#plot(Eplanirostris_ensemble_2041.2060_ssp585.pred.bin, ext=Asia_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Asia_window, add = TRUE, border = "black", lwd = 2, col = NA)
#dev.off()

tiff("./E.planirostris2/Asia_2041-2060_ssp585_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Asia_window, asp=1)
plot(raster(s), ext=Asia_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

#Eplanirostris_ensemble_2061.2080_ssp585.pred.bin <- raster("./E.planirostris2/proj_Eplanirostris_2061-2080_ssp585/individual_projections/E.planirostris2_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
#tiff("./E.planirostris2/Hawaii_2061.2080_ssp585_bin.tiff", width=664, height=664)
#plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
#plot(Eplanirostris_ensemble_2061.2080_ssp585.pred.bin, ext=Hawaii_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Hawaii_window, add = TRUE, border = "black", lwd = 2, col = NA)
#dev.off()

#tiff("./E.planirostris2/US_2061.2080_ssp585_bin.tiff", width=664, height=664)
#plot(combined_land_rast, col = "black", legend=FALSE, ext=US_window, asp=1)
#plot(Eplanirostris_ensemble_2061.2080_ssp585.pred.bin, ext=US_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=US_window, add = TRUE, border = "black", lwd = 2, col = NA)
#dev.off()

#tiff("./E.planirostris2/Asia_2061.2080_ssp585_bin.tiff", width=664, height=664)
#plot(combined_land_rast, col = "black", legend=FALSE, ext=Asia_window, asp=1)
#plot(Eplanirostris_ensemble_2061.2080_ssp585.pred.bin, ext=Asia_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Asia_window, add = TRUE, border = "black", lwd = 2, col = NA)
#dev.off()

#Eplanirostris_ensemble_2081.2100_ssp585.pred.bin <- raster("./E.planirostris2/proj_Eplanirostris_2081-2100_ssp585/individual_projections/E.planirostris2_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.tif")
#tiff("./E.planirostris2/Hawaii_2081.2100_ssp585_bin.tiff", width=664, height=664)
#plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
#plot(Eplanirostris_ensemble_2081.2100_ssp585.pred.bin, ext=Hawaii_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Hawaii_window, add = TRUE, border = "black", lwd = 2, col = NA)
#dev.off()

r <- rast(c(
  "./E.planirostris2/proj_Eplanirostris_ACCESS-CM2_2081-2100_ssp585/individual_projections/E.planirostris2_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.planirostris2/proj_Eplanirostris_INM-CM5-0_2081-2100_ssp585/individual_projections/E.planirostris2_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.planirostris2/proj_Eplanirostris_MPI-ESM1-2-HR_2081-2100_ssp585/individual_projections/E.planirostris2_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.planirostris2/proj_Eplanirostris_IPSL-CM6A-LR_2081-2100_ssp585/individual_projections/E.planirostris2_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.planirostris2/proj_Eplanirostris_UKESM1-0-LL_2081-2100_ssp585/individual_projections/E.planirostris2_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif"
  ))
m <- mean(r, na.rm=TRUE) # GCM ensemble
s <- m / 1000
s[s == 0] <- NA

tiff("./E.planirostris2/Hawaii_2081-2100_ssp585_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Hawaii_window, asp=1)
plot(raster(s), ext=Hawaii_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

#tiff("./E.planirostris2/US_2081.2100_ssp585_bin.tiff", width=664, height=664)
#plot(combined_land_rast, col = "black", legend=FALSE, ext=US_window, asp=1)
#plot(Eplanirostris_ensemble_2081.2100_ssp585.pred.bin, ext=US_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=US_window, add = TRUE, border = "black", lwd = 2, col = NA)
#dev.off()

tiff("./E.planirostris2/US_2081-2100_ssp585_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=US_window, asp=1)
plot(raster(s), ext=US_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

#tiff("./E.planirostris2/Asia_2081.2100_ssp585_bin.tiff", width=664, height=664)
##plot(combined_land_rast, col = "black", legend=FALSE, ext=Asia_window, asp=1)
#plot(Eplanirostris_ensemble_2081.2100_ssp585.pred.bin, ext=Asia_window, col = custom_palette(100), breaks = seq(zlim[1], zlim[2], length.out = 100), legend=FALSE, add=TRUE)
#plot(raster_outline, ext=Asia_window, add = TRUE, border = "black", lwd = 2, col = NA)
#dev.off()

tiff("./E.planirostris2/Asia_2081-2100_ssp585_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, ext=Asia_window, asp=1)
plot(raster(s), ext=Asia_window, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

# Plotting E. antillensis
#antillensis_window <- extent(-80, -45, -25, 33) # no need a window; already small enough

Eantillensis_ensemble_present.pred <- raster("./E.antillensis/proj_Eantillensis_present/individual_projections/E.antillensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif")
combined_land_rast <- rasterize(combined_land[6], Eantillensis_ensemble_present.pred)
Eantillensis_scaled <- Eantillensis_ensemble_present.pred / 1000
Eantillensis_scaled[Eantillensis_scaled == 0] <- NA

tiff("./E.antillensis/antillensis_present_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, asp=1)
plot(Eantillensis_scaled, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
points(Eantillensis_occs$Lon, Eantillensis_occs$Lat, col = adjustcolor("#ff00a2ff", alpha.f = 0.5), pch=1, cex=0.6)
dev.off()

tiff("./E.antillensis/antillensis_present_cont_xpoint.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, asp=1)
plot(Eantillensis_scaled, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

r <- rast(c(
  "./E.antillensis/proj_Eantillensis_ACCESS-CM2_2041-2060_ssp126/individual_projections/E.antillensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.antillensis/proj_Eantillensis_INM-CM5-0_2041-2060_ssp126/individual_projections/E.antillensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.antillensis/proj_Eantillensis_MPI-ESM1-2-HR_2041-2060_ssp126/individual_projections/E.antillensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.antillensis/proj_Eantillensis_IPSL-CM6A-LR_2041-2060_ssp126/individual_projections/E.antillensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.antillensis/proj_Eantillensis_UKESM1-0-LL_2041-2060_ssp126/individual_projections/E.antillensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif"
  ))
m <- mean(r, na.rm=TRUE) # GCM ensemble
s <- m / 1000
s[s == 0] <- NA

tiff("./E.antillensis/antillensis_2041-2060_ssp126_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, asp=1)
plot(raster(s), col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

r <- rast(c(
  "./E.antillensis/proj_Eantillensis_ACCESS-CM2_2081-2100_ssp126/individual_projections/E.antillensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.antillensis/proj_Eantillensis_INM-CM5-0_2081-2100_ssp126/individual_projections/E.antillensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.antillensis/proj_Eantillensis_MPI-ESM1-2-HR_2081-2100_ssp126/individual_projections/E.antillensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.antillensis/proj_Eantillensis_IPSL-CM6A-LR_2081-2100_ssp126/individual_projections/E.antillensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.antillensis/proj_Eantillensis_UKESM1-0-LL_2081-2100_ssp126/individual_projections/E.antillensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif"
  ))
m <- mean(r, na.rm=TRUE) # GCM ensemble
s <- m / 1000
s[s == 0] <- NA

tiff("./E.antillensis/antillensis_2081-2100_ssp126_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, asp=1)
plot(raster(s), col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

r <- rast(c(
  "./E.antillensis/proj_Eantillensis_ACCESS-CM2_2041-2060_ssp585/individual_projections/E.antillensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.antillensis/proj_Eantillensis_INM-CM5-0_2041-2060_ssp585/individual_projections/E.antillensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.antillensis/proj_Eantillensis_MPI-ESM1-2-HR_2041-2060_ssp585/individual_projections/E.antillensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.antillensis/proj_Eantillensis_IPSL-CM6A-LR_2041-2060_ssp585/individual_projections/E.antillensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.antillensis/proj_Eantillensis_UKESM1-0-LL_2041-2060_ssp585/individual_projections/E.antillensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif"
  ))
m <- mean(r, na.rm=TRUE) # GCM ensemble
s <- m / 1000
s[s == 0] <- NA

tiff("./E.antillensis/antillensis_2041-2060_ssp585_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, asp=1)
plot(raster(s), col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

r <- rast(c(
  "./E.antillensis/proj_Eantillensis_ACCESS-CM2_2081-2100_ssp585/individual_projections/E.antillensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.antillensis/proj_Eantillensis_INM-CM5-0_2081-2100_ssp585/individual_projections/E.antillensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.antillensis/proj_Eantillensis_MPI-ESM1-2-HR_2081-2100_ssp585/individual_projections/E.antillensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.antillensis/proj_Eantillensis_IPSL-CM6A-LR_2081-2100_ssp585/individual_projections/E.antillensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.antillensis/proj_Eantillensis_UKESM1-0-LL_2081-2100_ssp585/individual_projections/E.antillensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif"
  ))
m <- mean(r, na.rm=TRUE) # GCM ensemble
s <- m / 1000
s[s == 0] <- NA

tiff("./E.antillensis/antillensis_2081-2100_ssp585_cont.tiff", width=664, height=664)
plot(combined_land_rast, col = "black", legend=FALSE, asp=1)
plot(raster(s), col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

# Plotting E. martinicensis
#Jamaica_window <- extent(-80, -75, 17, 19)
#Antilles_window <- extent(-64, -60, 12, 18) 

Emartinicensis_ensemble_present.pred <- raster("./E.martinicensis/proj_Emartinicensis_present/individual_projections/E.martinicensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif")
combined_land_rast <- rasterize(combined_land[6], Emartinicensis_ensemble_present.pred)
Emartinicensis_scaled <- Emartinicensis_ensemble_present.pred / 1000
Emartinicensis_scaled[Emartinicensis_scaled == 0] <- NA

tiff("./E.martinicensis/Jamaica_present_cont.tiff", width=664, height=664)
plot(NA, xlim = c(-78, -76), ylim = c(17, 19), xlab = "", ylab = "", asp = 1) # forcing Jamaica window
plot(combined_land_rast,  col = "black", legend=FALSE, add=TRUE)
plot(Emartinicensis_scaled, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
points(Emartinicensis_occs$Lon, Emartinicensis_occs$Lat, col = adjustcolor("#ff00a2ff", alpha.f = 1), pch=1, cex=1.2)
dev.off()

tiff("./E.martinicensis/Jamaica_present_cont_xpoint.tiff", width=664, height=664)
plot(NA, xlim = c(-78, -76), ylim = c(17, 19), xlab = "", ylab = "", asp = 1) # forcing Jamaica window
plot(combined_land_rast,  col = "black", legend=FALSE, add=TRUE)
plot(Emartinicensis_scaled, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.martinicensis/Antilles_present_cont.tiff", width=664, height=664)
plot(NA, xlim = c(-63, -60), ylim = c(12, 18), xlab = "", ylab = "", asp = 1) # forcing Lesser Antilles window
plot(combined_land_rast,  col = "black", legend=FALSE, add=TRUE)
plot(Emartinicensis_scaled, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
points(Emartinicensis_occs$Lon, Emartinicensis_occs$Lat, col = adjustcolor("#ff00a2ff", alpha.f = 1), pch=1, cex=0.6)
dev.off()

tiff("./E.martinicensis/Antilles_present_cont_xpoint.tiff", width=664, height=664)
plot(NA, xlim = c(-63, -60), ylim = c(12, 18), xlab = "", ylab = "", asp = 1) # forcing Lesser Antilles window
plot(combined_land_rast,  col = "black", legend=FALSE, add=TRUE)
plot(Emartinicensis_scaled, col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

r <- rast(c(
  "./E.martinicensis/proj_Emartinicensis_ACCESS-CM2_2041-2060_ssp126/individual_projections/E.martinicensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.martinicensis/proj_Emartinicensis_INM-CM5-0_2041-2060_ssp126/individual_projections/E.martinicensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.martinicensis/proj_Emartinicensis_MPI-ESM1-2-HR_2041-2060_ssp126/individual_projections/E.martinicensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.martinicensis/proj_Emartinicensis_IPSL-CM6A-LR_2041-2060_ssp126/individual_projections/E.martinicensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.martinicensis/proj_Emartinicensis_UKESM1-0-LL_2041-2060_ssp126/individual_projections/E.martinicensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif"
  ))
m <- mean(r, na.rm=TRUE) # GCM ensemble
s <- m / 1000
s[s == 0] <- NA

tiff("./E.martinicensis/Jamaica_2041-2060_ssp126_cont.tiff", width=664, height=664)
plot(NA, xlim = c(-78, -76), ylim = c(17, 19), xlab = "", ylab = "", asp = 1) # forcing Jamaica window
plot(combined_land_rast,  col = "black", legend=FALSE, add=TRUE)
plot(raster(s), col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.martinicensis/Antilles_2041-2060_ssp126_cont.tiff", width=664, height=664)
plot(NA, xlim = c(-63, -60), ylim = c(12, 18), xlab = "", ylab = "", asp = 1) # forcing Lesser Antilles window
plot(combined_land_rast,  col = "black", legend=FALSE, add=TRUE)
plot(raster(s), col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

r <- rast(c(
  "./E.martinicensis/proj_Emartinicensis_ACCESS-CM2_2081-2100_ssp126/individual_projections/E.martinicensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.martinicensis/proj_Emartinicensis_INM-CM5-0_2081-2100_ssp126/individual_projections/E.martinicensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.martinicensis/proj_Emartinicensis_MPI-ESM1-2-HR_2081-2100_ssp126/individual_projections/E.martinicensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.martinicensis/proj_Emartinicensis_IPSL-CM6A-LR_2081-2100_ssp126/individual_projections/E.martinicensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.martinicensis/proj_Emartinicensis_UKESM1-0-LL_2081-2100_ssp126/individual_projections/E.martinicensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif"
  ))
m <- mean(r, na.rm=TRUE) # GCM ensemble
s <- m / 1000
s[s == 0] <- NA

tiff("./E.martinicensis/Jamaica_2081-2100_ssp126_cont.tiff", width=664, height=664)
plot(NA, xlim = c(-78, -76), ylim = c(17, 19), xlab = "", ylab = "", asp = 1) # forcing Jamaica window
plot(combined_land_rast,  col = "black", legend=FALSE, add=TRUE)
plot(raster(s), col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.martinicensis/Antilles_2081-2100_ssp126_cont.tiff", width=664, height=664)
plot(NA, xlim = c(-63, -60), ylim = c(12, 18), xlab = "", ylab = "", asp = 1) # forcing Lesser Antilles window
plot(combined_land_rast,  col = "black", legend=FALSE, add=TRUE)
plot(raster(s), col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

r <- rast(c(
  "./E.martinicensis/proj_Emartinicensis_ACCESS-CM2_2041-2060_ssp585/individual_projections/E.martinicensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.martinicensis/proj_Emartinicensis_INM-CM5-0_2041-2060_ssp585/individual_projections/E.martinicensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.martinicensis/proj_Emartinicensis_MPI-ESM1-2-HR_2041-2060_ssp585/individual_projections/E.martinicensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.martinicensis/proj_Emartinicensis_IPSL-CM6A-LR_2041-2060_ssp585/individual_projections/E.martinicensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.martinicensis/proj_Emartinicensis_UKESM1-0-LL_2041-2060_ssp585/individual_projections/E.martinicensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif"
  ))
m <- mean(r, na.rm=TRUE) # GCM ensemble
s <- m / 1000
s[s == 0] <- NA

tiff("./E.martinicensis/Jamaica_2041-2060_ssp585_cont.tiff", width=664, height=664)
plot(NA, xlim = c(-78, -76), ylim = c(17, 19), xlab = "", ylab = "", asp = 1) # forcing Jamaica window
plot(combined_land_rast,  col = "black", legend=FALSE, add=TRUE)
plot(raster(s), col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.martinicensis/Antilles_2041-2060_ssp585_cont.tiff", width=664, height=664)
plot(NA, xlim = c(-63, -60), ylim = c(12, 18), xlab = "", ylab = "", asp = 1) # forcing Lesser Antilles window
plot(combined_land_rast,  col = "black", legend=FALSE, add=TRUE)
plot(raster(s), col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

r <- rast(c(
  "./E.martinicensis/proj_Emartinicensis_ACCESS-CM2_2081-2100_ssp585/individual_projections/E.martinicensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.martinicensis/proj_Emartinicensis_INM-CM5-0_2081-2100_ssp585/individual_projections/E.martinicensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.martinicensis/proj_Emartinicensis_MPI-ESM1-2-HR_2081-2100_ssp585/individual_projections/E.martinicensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.martinicensis/proj_Emartinicensis_IPSL-CM6A-LR_2081-2100_ssp585/individual_projections/E.martinicensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif",
  "./E.martinicensis/proj_Emartinicensis_UKESM1-0-LL_2081-2100_ssp585/individual_projections/E.martinicensis_EMwmeanByBOYCE_mergedData_mergedRun_mergedAlgo.tif"
  ))
m <- mean(r, na.rm=TRUE) # GCM ensemble
s <- m / 1000
s[s == 0] <- NA

tiff("./E.martinicensis/Jamaica_2081-2100_ssp585_cont.tiff", width=664, height=664)
plot(NA, xlim = c(-78, -76), ylim = c(17, 19), xlab = "", ylab = "", asp = 1) # forcing Jamaica window
plot(combined_land_rast,  col = "black", legend=FALSE, add=TRUE)
plot(raster(s), col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()

tiff("./E.martinicensis/Antilles_2081-2100_ssp585_cont.tiff", width=664, height=664)
plot(NA, xlim = c(-63, -60), ylim = c(12, 18), xlab = "", ylab = "", asp = 1) # forcing Lesser Antilles window
plot(combined_land_rast,  col = "black", legend=FALSE, add=TRUE)
plot(raster(s), col = custom_palette, breaks = breaks, zlim = c(0,1), legend=FALSE, add=TRUE)
dev.off()