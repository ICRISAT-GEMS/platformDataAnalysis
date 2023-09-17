############################################
# Overall LeasyScan pipeline - toy example #
############################################

library(dplyr)
library(LoadCellDataProcessing)
library(statgenHTP)
library(platformDataAnalysis)
library(ggplot2)
library(lubridate)

# data preparation ----

# PE data
setwd('D:/Mes Donnees/WD/BCNAM/data/LeasyScan/Exp51_Kenin_Keni/Exp51 BCNAM Keninkeni Mungbean June 2022_20230426_data')
data <- read.csv(file = 'Exp51 BCNAM Keninkeni Mungbean June 2022_20230426_planteye.csv')

setwd("D:/Mes Donnees/WD/BCNAM/data/LeasyScan/Exp51_Kenin_Keni/Exp51 BCNAM Keninkeni and Mungbean BCFG June2022-1_20230426_data")
data2 <- read.csv(file = 'Exp51 BCNAM Keninkeni and Mungbean BCFG June2022-1_20230426_planteye.csv')

setwd("D:/Mes Donnees/WD/BCNAM/data/LeasyScan/Exp51_Kenin_Keni/Exp51 BCNAM Keninkeni DE Trench June 2022_20230426_data")
data3 <- read.csv(file = 'Exp51 BCNAM Keninkeni DE Trench June 2022_20230426_planteye.csv')

setwd("D:/Mes Donnees/WD/BCNAM/data/LeasyScan/Exp51_Kenin_Keni/Exp51 BCNAM Keninkeni DE trench June 2022-1_20230426_data")
data4 <- read.csv(file = 'Exp51 BCNAM Keninkeni DE trench June 2022-1_20230426_planteye.csv')

pe_data <- rbind(data, data2, data3, data4)
rm(data, data2, data3, data4)

# LC data
setwd('D:/Mes Donnees/WD/BCNAM/data/LeasyScan/Exp51_Kenin_Keni/Exp51 BCNAM Keninkeni Mungbean June 2022_20230426_data')
lc_data <- read.csv(file = 'Exp51 BCNAM Keninkeni Mungbean June 2022_20230426_DroughtSpotter.csv')

# exp design
setwd("D:/Mes Donnees/WD/Experimental_design/design/KeninKeni_LC")

d_design <- read.csv(file = "BCNAM_KeninKeni_design_info.csv")
d_design <- d_design %>% select(row.psx, col.psx, rep, block, geno_id, cross)

# select a small subset

# d_des_s <- d_design %>% filter(row.psx %in% 3:6, col.psx %in% 1:12)
d_des_s <- d_design %>% filter(row.psx %in% 1:3, col.psx %in% 1:12)
d_exp <- d_des_s

# save data
setwd("D:/Mes Donnees/WD/R/packages/data/LS_pipeline_toyexample")
save(d_exp, file = "exp_des.RData")

# convert the row and columns in reference system: col.psx -> row
# row.psx -> col + 2 (the indice depend from where we start here A1 and A2 are skipped)

d_exp$rowNum <- d_exp$col.psx
d_exp$colNum <- d_exp$row.psx + 2
d_exp$plotId <- paste0(paste0("c", d_exp$colNum), paste0("r", d_exp$rowNum))

# add the sector id to the experimental design
setwd("D:/Mes Donnees/WD/ICRISAT/Phenotyping/LeasyScan")
load(file = "unit_row_col_block.RData")

d_unit <- d_unit_row_col_block[, c(2:4, 8)]
d_exp <- left_join(x = d_exp, y =  d_unit, by = "plotId")

# subset pe, lc, weather, and sensor data
pe_data_s <- pe_data[pe_data$unit %in% d_exp$new_unit, ]

# check genotypes correspond to the experimental design
substr(unique(pe_data_s$genotype), 11, nchar(unique(pe_data_s$genotype)))

pe_data <- pe_data_s

lc_data_s <- lc_data[lc_data$unit %in% d_exp$new_unit, ]

unique(lc_data_s$unit)

lc_data <- lc_data_s

# weather data
setwd('D:/Mes Donnees/WD/BCNAM/data/LeasyScan/Exp51_Kenin_Keni/Exp51 BCNAM Keninkeni Mungbean June 2022_20230426_data')
wth_data <- read.csv(file = 'Exp51 BCNAM Keninkeni Mungbean June 2022_20230426_Climate Datalogger.csv')

setwd("D:/Mes Donnees/WD/BCNAM/data/LeasyScan/Exp51_Kenin_Keni/Exp51 BCNAM Keninkeni and Mungbean BCFG June2022-1_20230426_data")
wth_data2 <- read.csv(file = 'Exp51 BCNAM Keninkeni and Mungbean BCFG June2022-1_20230426_Climate Datalogger.csv')

setwd("D:/Mes Donnees/WD/BCNAM/data/LeasyScan/Exp51_Kenin_Keni/Exp51 BCNAM Keninkeni DE Trench June 2022_20230426_data")
wth_data3 <- read.csv(file = 'Exp51 BCNAM Keninkeni DE Trench June 2022_20230426_Climate Datalogger.csv')

setwd("D:/Mes Donnees/WD/BCNAM/data/LeasyScan/Exp51_Kenin_Keni/Exp51 BCNAM Keninkeni DE trench June 2022-1_20230426_data")
wth_data4 <- read.csv(file = 'Exp51 BCNAM Keninkeni DE trench June 2022-1_20230426_Climate Datalogger.csv')

wth_data <- rbind(wth_data, wth_data2, wth_data3, wth_data4)
rm(wth_data2, wth_data3, wth_data4)

# sensor unit map
setwd('D:/Mes Donnees/WD/BCNAM/data/LeasyScan/Exp51_Kenin_Keni/Exp51 BCNAM Keninkeni Mungbean June 2022_20230426_data')
sensor_data <- read.csv(file = 'Exp51 BCNAM Keninkeni Mungbean June 2022_20230426_sensorUnitMap.csv')

setwd("D:/Mes Donnees/WD/BCNAM/data/LeasyScan/Exp51_Kenin_Keni/Exp51 BCNAM Keninkeni and Mungbean BCFG June2022-1_20230426_data")
sensor_data2 <- read.csv(file = 'Exp51 BCNAM Keninkeni and Mungbean BCFG June2022-1_20230426_sensorUnitMap.csv')

setwd("D:/Mes Donnees/WD/BCNAM/data/LeasyScan/Exp51_Kenin_Keni/Exp51 BCNAM Keninkeni DE Trench June 2022_20230426_data")
sensor_data3 <- read.csv(file = 'Exp51 BCNAM Keninkeni DE Trench June 2022_20230426_sensorUnitMap.csv')

setwd("D:/Mes Donnees/WD/BCNAM/data/LeasyScan/Exp51_Kenin_Keni/Exp51 BCNAM Keninkeni DE trench June 2022-1_20230426_data")
sensor_data4 <- read.csv(file = 'Exp51 BCNAM Keninkeni DE trench June 2022-1_20230426_sensorUnitMap.csv')

sensor_data <- rbind(sensor_data, sensor_data2, sensor_data3, sensor_data4)
rm(sensor_data2, sensor_data3, sensor_data4)

# select a subset of the sensor data through unit present in the design
sensor_data <- sensor_data[sensor_data$unit %in% d_exp$new_unit, ]

# subset the weather data given the unique sensor
wth_data <- wth_data[wth_data$sensor %in% unique(sensor_data$sensor), ]

# save data
setwd("D:/Mes Donnees/WD/R/packages/data/LS_pipeline_toyexample")
save(pe_data, file = "pe_data.RData")
save(lc_data, file = "lc_data.RData")
save(sensor_data, file = "sensor_data.RData")
save(wth_data, file = "weather_data.RData")

# Initial filtering through experimental design ----

# load data
setwd("D:/Mes Donnees/WD/R/packages/data/LS_pipeline_toyexample")
load(file = "exp_des.RData")
load(file = "pe_data.RData")
load(file = "lc_data.RData")
load(file = "sensor_data.RData")
load(file = "weather_data.RData")

# starting point: experimental design: list of genotype, positions (row, col), ...
head(d_exp)

# need to add unit (sector) identifiers if not present in the design sheet

# convert the row and columns in reference system: col.psx -> row
# row.psx -> col + 2 (the indice depend from where we start here A1 and A2 are skipped)

d_exp$rowNum <- d_exp$col.psx
d_exp$colNum <- d_exp$row.psx + 2
d_exp$plotId <- paste0(paste0("c", d_exp$colNum), paste0("r", d_exp$rowNum))

# reference data with conversion sector id given row and column position
load(file = "unit_row_col_block.RData")

d_unit <- d_unit_row_col_block[, c(2:4, 8)]
d_exp <- left_join(x = d_exp, y =  d_unit, by = "plotId")

# filter pe_data using exp_des unit information
pe_data <- pe_data[pe_data$unit %in% d_exp$new_unit, ]

# format the genotype data (match further with other data)
genotype <- strsplit(x = pe_data$genotype, split = '_')
geno_n_pieces <- unlist(lapply(X = genotype, FUN = length))
table(geno_n_pieces)

geno_vec <- unlist(lapply(X = genotype, FUN = `[[`, 3))
unique(geno_vec)
table(geno_vec)

# geno_vec[geno_vec == ""] <- NA
pe_data$genotype <- geno_vec

# filter lc_data using exp_des unit information
lc_data <- lc_data[lc_data$unit %in% d_exp$new_unit, ]

# add a genotype column using exp design sector genotype look_up
geno_lk <- d_exp$geno_id
names(geno_lk) <- d_exp$new_unit
lc_data$genotype <- lc_data$g_alias <- geno_lk[lc_data$unit]

# select a subset of the sensor data through unit present in the design
sensor_data <- sensor_data[sensor_data$unit %in% d_exp$new_unit, ]

# subset the weather data given the unique sensor
wth_data <- wth_data[wth_data$sensor %in% unique(sensor_data$sensor), ]

# save the data
setwd("D:/Mes Donnees/WD/R/packages/data/LS_pipeline_toyexample")
save(d_exp, file = "exp_des_filtered.RData")
save(pe_data, file = "pe_data_filtered.RData")
save(lc_data, file = "lc_data_filtered.RData")
save(sensor_data, file = "sensor_data_filtered.RData")
save(wth_data, file = "weather_data_filtered.RData")

############################ LC pipeline #######################################
# LC pipeline: Data processing ----

setwd("D:/Mes Donnees/WD/R/packages/data/LS_pipeline_toyexample")
load(file = "pe_data_filtered.RData")
load(file = "lc_data_filtered.RData")
load(file = "sensor_data_filtered.RData")
load(file = "weather_data_filtered.RData")

# PE_data
pe_data <- pe_data %>% select(unit, genotype, timestamp, Leaf.area..mm..)
pe_data$replicate <- NA
colnames(pe_data) <- c('sector', 'genotype', 'timestamp', 'leaf_area', 'replicate')
col_names <- c("sector", "genotype", "replicate", "timestamp", "leaf_area")
pe_data <- pe_data[, col_names]

# LC data
lc_data <- lc_data[lc_data$variable == "Weight g", ]
lc_data$treatment <- NA
lc_data <- lc_data %>% rename(`Mass..g` = value, sector = unit)
lc_col_name <- c("sector", "genotype", "g_alias", "treatment", "timestamp", "Mass..g")
lc_data <- lc_data[, lc_col_name]

# sensor data
sensor_data <- sensor_data[, 1:2]
colnames(sensor_data) <- c("sector", "sensor")

# weather data

# modify few climatic variable name to be exactly as in the example
wth_data$variable[wth_data$variable == "Relative Humidity (%)"] <- "Relative humidity (%)"
wth_data$variable[wth_data$variable == "Solar Radiation (W/(s*m2))"] <- "Solar radiation (W/(s*m²))"
wth_data$variable[wth_data$variable == "Wind Direction (°)"] <- "Wind direction (°)"

unique(wth_data$variable)

# save the data
setwd("D:/Mes Donnees/WD/R/packages/data/LS_pipeline_toyexample")
save(pe_data, file = "pe_data_processed.RData")
save(lc_data, file = "lc_data_processed.RData")
save(sensor_data, file = "sensor_data_processed.RData")
save(wth_data, file = "weather_data_processed.RData")

# LC pipeline: TR feature data extraction (TR_data_proc) ----

# load data
setwd("D:/Mes Donnees/WD/R/packages/data/LS_pipeline_toyexample")
load(file = "pe_data_processed.RData")
load(file = "lc_data_processed.RData")
load(file = "sensor_data_processed.RData")
load(file = "weather_data_processed.RData")


t1 <- Sys.time()

# Need to modify the function to remove negative values of the smooth
# Profile before calculating the 15 features.

TR_res <- TR_data_proc(lc_data = lc_data, pe_data = pe_data,
                       wth_data = wth_data, sensor_data = sensor_data,
                       lastDate = NULL, skew_test = FALSE,
                       LAI_correction = FALSE)

t2 <- Sys.time()

t_diff <- t2 - t1
print(t_diff)

# LC pipeline: check the results shape, possible selection of days ----

p <- plot_whole_TR_time_series(results = TR_res, sector_sel = c(2, 4, 8))
p

# loop over all sectors to make a pdf with all plots
n_sect <- nrow(TR_res$TR_smth$TR_smth)

pdf(file = "./plot/TR_time_series_full.pdf", width = 12, height = 8)

for(i in 1:n_sect){

  p <- tryCatch(plot_whole_TR_time_series(results = TR_res, sector_sel = i,
                                          main = paste("sector", i)),
                error = function(e) NULL)

  if(!is.null(p)){ print(p) }

}

dev.off()

# leaf area index coverage
# p <- plot_LAI_coverage(results = TR_res)
# p

# check Tr feature daily TS
trait_id <- "TRmax"
title_i <- paste(trait_id, "- KK subset")

p <- plot_TR_time_series(results = TR_res, trait = trait_id, n_sector = NULL,
                         color = FALSE, main = title_i)
p

# select days
start_day = 1
end_day = 8
TR_feature <- TR_res$TR_smth$Max_TR_smth
ts_date <- colnames(TR_feature)[6:ncol(TR_feature)]
LC_day_sel <- ts_date[start_day:end_day]

# the days can be used later to subset
TR_res <- TR_res_subset(TR_res = TR_res, start_day = 1, end_day = 8)


# LC pipeline: TR results processing TP object ----

setwd("D:/Mes Donnees/WD/R/packages/data/LS_pipeline_toyexample")
load(file = "exp_des_filtered.RData")

d_TP <- TR_res_to_pre_TP(TR_res)

# extract the TR ~ VPD regression coefficient
TR_VPD_reg_coeff <- TR_VPD_reg(TR_res = TR_res, do_plot = FALSE)
TR_VPD_lk <- TR_VPD_reg_coeff$Tr_VPD_slope
names(TR_VPD_lk) <- TR_VPD_reg_coeff$unit
d_TP$TR_VPD <- TR_VPD_lk[d_TP$unit]

# add time number
d_TP$timestamp <- strptime(d_TP$timestamp, "%Y-%m-%d")
d_TP <- add_timeNumber(d_TP)

# timeNumber <- yday(d_TP$timestamp)
# timeNumber <- timeNumber - min(timeNumber, na.rm = TRUE) + 1
# d_TP <- data.frame(timeNumber, d_TP)
d_TP <- d_TP %>% arrange(unit, timestamp)

# add extra columns from the experimental design
d_TP <- add_exp_des_col(data = d_TP, d_exp_des = d_exp,
                        data_unit = "unit",
                        d_exp_unit = "new_unit",
                        col_add = c("rowNum", "colNum", "block", "cross"))

# PlotId is the combination of row and col information
d_TP$plotId <- paste0(paste0("c", d_TP$colNum), paste0("r", d_TP$rowNum))

# arrange the columns in a certain order
d_TP_meta <- d_TP %>% select(timeNumber, timestamp, block, rowNum, colNum, plotId, cross,
                             genotype) %>% rename(timePoint = timestamp)

d_TP_trait <- d_TP %>% select(contains("_smth"), TR_VPD)

# modify the names of the traits
tr_nm <- c("max_TR", "slope_6pt_bfr_maxTR", "slope_07_maxTR",
           "slope_00_07", "slope_19h_23h45", "curve_maxTR",
           "total_auc", "auc_10h_15h", "sd_10h_15h",
           "prop_auc_10h_15h", "auc_7h_19h", "sd_7h_19h",
           "prop_auc_7h_19h", "auc_night", "TR_VPD_slope")

colnames(d_TP_trait) <- tr_nm

data_LC <- data.frame(d_TP_meta, d_TP_trait)

# try to reduce the number of genotypes
# data_LC$genotype[data_LC$cross == "BC10"] <- "BC10"

# remove one sector
# data_LC <- data_LC[-c(1, 9, 16), ]

TP_LC <- createTimePoints(dat = data_LC,
                       experimentName = "Exp_51_Kenin_Keni_LC",
                       genotype = "genotype",
                       timePoint = "timePoint",
                       plotId = "plotId",
                       rowNum = "rowNum", colNum = "colNum")

# LC pipeline: diagnostic plot ----

plot(TP_LC, traits = "max_TR",
     plotType = "layout",
     timePoints = 1)

# LC pipeline: TR results spatial adjustment ----

LC_adjusted <- spatial_adjustment(TP = TP_LC,
                                     traits = c("max_TR", "total_auc", "TR_VPD_slope"),
                                     timePoints = NULL,
                                     extraFixedFactors = "block",
                                     geno.decomp = NULL,
                                     what = "fixed",
                                     useCheck = FALSE,
                                     useRepId = FALSE,
                                     engine = "SpATS",
                                     spatial = FALSE,
                                     quiet = TRUE)

plot_LC_adj <- LC_adjusted$plot_res
geno_LC_adj <- LC_adjusted$geno_res
comp_mon <- LC_adjusted$comp_monitor

# check adjusted TS

plot_trend(data = plot_LC_adj, trait = "max_TR_corr")
plot_trend(data = plot_LC_adj, trait = "total_auc_corr")

plot_trend(data = geno_LC_adj, trait = "max_TR_pred", genotype = TRUE)
plot_trend(data = geno_LC_adj, trait = "total_auc_pred", genotype = TRUE)

# save the data
setwd("D:/Mes Donnees/WD/R/packages/data/LS_pipeline_toyexample")
save(plot_LC_adj, file = "LC_plot_adjusted.RData")
save(geno_LC_adj, file = "LC_geno_adjusted.RData")


############################ PE pipeline #######################################
# PE pipeline: Data processing ----

setwd("D:/Mes Donnees/WD/R/packages/data/LS_pipeline_toyexample")
load(file = "pe_data_filtered.RData")
load(file = "exp_des_filtered.RData")

colnames(pe_data) <- mdf_raw_pe_colnames(colnames = colnames(pe_data))

ref_trait_nm <- c("Digital_biomass", "Height", "Leaf_angle", "Leaf_area",
                  "Leaf_area_index", "Leaf_area_projected", "Leaf_inclination",
                  "Light_penetration_depth")

pe_data <- pe_data %>% select(unit, genotype, g_alias, treatment, timestamp,
                        Digital_biomass, Height, Leaf_angle, Leaf_area, Leaf_area_index,
                        Leaf_area_projected, Leaf_inclination, Light_penetration_depth)

colnames(pe_data)[6:13] <- ref_trait_nm

# add time number
pe_data$timestamp <- as.POSIXlt(pe_data$timestamp, format = "%Y-%m-%d %H:%M:%S")
pe_data$timestamp <- strptime(pe_data$timestamp, "%Y-%m-%d")
pe_data <- add_timeNumber(pe_data)

# add experimental design information

# add extra columns from the experimental design
pe_data <- add_exp_des_col(data = pe_data, d_exp_des = d_exp,
                        data_unit = "unit",
                        d_exp_unit = "new_unit",
                        col_add = c("rowNum", "colNum", "block", "cross"))

# PlotId is the combination of row and col information
pe_data$plotId <- paste0(paste0("c", pe_data$colNum), paste0("r", pe_data$rowNum))

# arrange the columns in a certain order
pe_data <- pe_data %>% select(timeNumber, timestamp, block, rowNum, colNum, plotId, cross,
                             genotype, Digital_biomass, Height, Leaf_angle, Leaf_area, Leaf_area_index,
                             Leaf_area_projected, Leaf_inclination, Light_penetration_depth) %>% rename(timePoint = timestamp)

# PE pipeline: Median calculation ----
pe_data <- median_computation(pe_data)

# select time window and plot trend ----

plot_trend(data = pe_data, trait = "Height")
plot_trend(data = pe_data, trait = "Leaf_area")
plot_trend(data = pe_data, trait = "Digital_biomass")

# select specific timepoints (days)
prop_non_miss <- timepoint_prop_non_missing(pe_data)
d_sel_days <- data.frame(tp = names(prop_non_miss),
                         prop = prop_non_miss,
                         sel = prop_non_miss > 0.3)

sel_days <- d_sel_days$tp[d_sel_days$sel]
d_end_rm <- 4
day_rm <- (length(sel_days) - (d_end_rm - 1)):length(sel_days)
sel_days <- sel_days[-day_rm]
d_sel_days <- d_sel_days[-day_rm, ]

pe_data <- pe_data[pe_data$timePoint %in% sel_days, ]

plot_trend(data = pe_data, trait = "Height")
plot_trend(data = pe_data, trait = "Leaf_area")
plot_trend(data = pe_data, trait = "Digital_biomass")

# detect the outliers using boxplot

pe_data <- outlier_boxplot_detect(pe_data)

plot_trend(data = pe_data, trait = "Height")
plot_trend(data = pe_data, trait = "Leaf_area")
plot_trend(data = pe_data, trait = "Digital_biomass")

# PE pipeline: Creation of TP object ----

TP_PE <- createTimePoints(dat = pe_data,
                          experimentName = "Exp_51_Kenin_Keni_PE",
                          genotype = "genotype",
                          timePoint = "timePoint",
                          plotId = "plotId",
                          rowNum = "rowNum", colNum = "colNum")

# PE pipeline: Spatial adjustment ----

TP = TP_PE
traits = c("Height", "Leaf_area", "Digital_biomass")
what = "fixed"
useCheck = FALSE
useRepId = FALSE
engine = "SpATS"
spatial = FALSE
quiet = FALSE

PE_adjusted <- spatial_adjustment(TP = TP_PE,
                                  traits = c("Height", "Leaf_area", "Digital_biomass"),
                                  timePoints = NULL,
                                  extraFixedFactors = "block",
                                  geno.decomp = NULL,
                                  what = "random",
                                  useCheck = FALSE,
                                  useRepId = FALSE,
                                  engine = "SpATS",
                                  spatial = FALSE,
                                  quiet = TRUE)

plot_PE_adj <- PE_adjusted$plot_res
geno_PE_adj <- PE_adjusted$geno_res
comp_mon <- PE_adjusted$comp_monitor

plot_trend(data = plot_PE_adj, trait = "Height_corr")
plot_trend(data = plot_PE_adj, trait = "Digital_biomass_corr")

plot_trend(data = geno_PE_adj, trait = "Leaf_area_pred", genotype = TRUE)
plot_trend(data = geno_PE_adj, trait = "Height_pred", genotype = TRUE)

# save the data
setwd("D:/Mes Donnees/WD/R/packages/data/LS_pipeline_toyexample")
save(plot_PE_adj, file = "PE_plot_adjusted.RData")
save(geno_PE_adj, file = "PE_geno_adjusted.RData")

##### SAVE H2 res.

# use here the function spatial_adjustment when it is done. Take the time to
# make it smooth.
############################ Weather data comp #################################

############################ merge LC and PE ###################################
# LC + PE results merging ----

# load the adjusted LC data
setwd("D:/Mes Donnees/WD/R/packages/data/LS_pipeline_toyexample")
load(file = "LC_plot_adjusted.RData")
load(file = "LC_geno_adjusted.RData")

load(file = "PE_plot_adjusted.RData")
load(file = "PE_geno_adjusted.RData")

# merge

plot_LC_adj <- plot_LC_adj[, -1]
plot_PE_adj <- plot_PE_adj[, -1]

# plot_data <- full_join(x = plot_LC_adj, y = plot_PE_adj, by = c("timePoint", "plotId"))
plot_data <- full_join(x = plot_LC_adj, y = plot_PE_adj,
                        by = intersect(colnames(plot_LC_adj), colnames(plot_PE_adj)))

geno_LC_adj <- geno_LC_adj[, -1]
geno_PE_adj <- geno_PE_adj[, -1]

# geno_data <- full_join(x = geno_LC_adj, y = geno_PE_adj, by = c("timePoint", "genotype"))
geno_data <- full_join(x = geno_LC_adj, y = geno_PE_adj,
                       by = intersect(colnames(geno_LC_adj), colnames(geno_PE_adj)))

# addition of weather data ----

# get the weather data time series information
setwd("D:/Mes Donnees/WD/R/packages/data/LS_pipeline_toyexample")
load(file = "sensor_data_processed.RData")
load(file = "weather_data_processed.RData")

wth_data <- wth_data_proc(wth_data = wth_data, sensor_data = sensor_data)

# reduce the values over days
wth_data_red <- wth_data %>% group_by(dmy) %>% summarise(T_min = min(Temp, na.rm = TRUE),
                                                     T_max = max(Temp, na.rm = TRUE),
                                                     T_av = mean(Temp, na.rm = TRUE),
                                                     RH_av = mean(RH, na.rm = TRUE),
                                                     VPD_av = mean(VPD, na.rm = TRUE),
                                                     SR_av = mean(SR, na.rm = TRUE),
                                                     WS_av = mean(WS, na.rm = TRUE))
colnames(wth_data_red)[1] <- "timePoint"
wth_data_red$timePoint <- as.character(wth_data_red$timePoint)

# merge the weather data with the plot or geno data.
plot_data$timePoint <- as.character(plot_data$timePoint)
plot_data <- left_join(x = plot_data, y = wth_data_red, by = "timePoint")

wth_data_red$timePoint %in% plot_data$timePoint

geno_data <- left_join(x = geno_data, y = wth_data_red, by = "timePoint")
