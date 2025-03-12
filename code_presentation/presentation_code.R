# Pour savoir ce que fait une fonction, on peut écrire ?fonction. 
# Par exemple, pour la fonction ci-dessous, rm() : 
?rm

rm(list=ls()) # Réinitialise l'environnement de travail

###########################################*
#### PARTIE 0 : IMPORTATION DES DONNÉES ####
###########################################*

source("C:/these/presentations/RDT-13-03/code_presentation/helper_functions.R")

data <- import_data(158348)
head(data)
str(data)

###########################################*
#### PARTIE 1 : GRAPHIQUES DESCRIPTIFS ####
###########################################*

#================================================================================#
# Histogrammes de hs #
#================================================================================#

ggplot(data, aes(x = hs)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  geom_text(stat = "bin", bins = 20, aes(label = ..count..), vjust = -0.4,
            hjust=0.4) +
  labs(
    title = "Histogramme de Hauteur significative des vagues (m)",
    x = "Hauteur significative des vagues (m)",
    y = "Fréquence"
  ) +
  scale_x_continuous(breaks = seq(floor(min(data$hs)), ceiling(max(data$hs)), by = 1)) +
  theme_bw()

ggplot(data[data$hs>4,], aes(x = hs)) +
  geom_histogram(bins = 10, fill = "steelblue", color = "white") +
  geom_text(stat = "bin", bins = 10, aes(label = ..count..), vjust = -0.4,
            hjust=0.4) +
  labs(
    title = "Histogramme de Hauteur significative des vagues (m)",
    x = "Hauteur significative des vagues (m)",
    y = "Fréquence"
  ) +
  scale_x_continuous(breaks = seq(floor(min(data$hs)), ceiling(max(data$hs)), by = 1)) +
  theme_bw()

#================================================================================#
# Plot de densité (Hs,Tp) #
#================================================================================#

ggplot(data, aes(x = hs, y = tp)) +
  geom_bin2d(bins = 70) + 
  scale_fill_gradient(name = "Observations", low = "black", high = "blue") +
  labs(
    title = "Distribution jointe de (Hs,Tp)",
    x = "Hauteur significative des vagues (m)",
    y = "Période au pic (s)"
  ) +
  theme_bw()

#================================================================================#
# Diagramme en barres des provenances des vagues #
#================================================================================#

data$dpCardinale <- factor(
  data$dpCardinale, 
  levels = c("N", "NE", "E", "SE", "S", "SO", "O", "NO")
)

ggplot(data, aes(x = dpCardinale)) +
  geom_bar(fill = "orchid", color = "black") +
  geom_text(
    stat = "count", 
    aes(label = ..count..), 
    vjust = -0.3, 
    color = "black"
  ) +
  labs(
    title = "Diagramme de la provenance des vagues",
    x = "Provenance des vagues",
    y = "Fréquence"
  ) +
  theme_bw()


#================================================================================#
# Histogramme des vitesses de courant #
#================================================================================#

ggplot(data, aes(x = cur_speed)) +
  geom_histogram(
    bins = 20,                
    fill = "seagreen",       
    color = "white"         
  ) +
  labs(
    title = "Histogramme de la vitesse du courant (m/s)",
    x = "Vitesse du courant (m/s)",
    y = "Fréquence"
  ) +
  theme_bw()

##########################################################*
#### PARTIE 2 : MODELISATION QUANTILE SANS COVARIABLE ####
##########################################################*

#================================================================================#
# Régression quantile #
#================================================================================#

quantiles <- c(0.5, 0.9, 0.95, 0.99)

models <- lapply(quantiles, function(tau) {
  rq(hs ~ ns(time, df = 12), data = data, tau = tau) 
}) 

for (i in seq_along(quantiles)) {
  pred_col <- paste0("pred_", quantiles[i])
  data[[pred_col]] <- predict(models[[i]], newdata = data)
}

ggplot(data, aes(x = time,y=hs)) +
  geom_line(color="black") +
  geom_line(aes(y = pred_0.5, color = "Quantile 0.5"), size = 1) +
  geom_line(aes(y = pred_0.9, color = "Quantile 0.9"), size = 1) +
  geom_line(aes(y = pred_0.95, color = "Quantile 0.95"), size = 1) +
  geom_line(aes(y = pred_0.99, color = "Quantile 0.99"), size = 1) +
  scale_color_manual(name = "Légende",
                     values = c(
                                "Quantile 0.5" = "green",
                                "Quantile 0.9" = "yellow",
                                "Quantile 0.95" = "orange",
                                "Quantile 0.99" = "red")) +
  labs(
    title = "Régression quantile sur la série temporelle des Hs",
    x = "Temps",
    y = "Hauteur significative des vagues (m)"
  ) +
  theme_bw()

######################################################*
#### PARTIE 3 : MODELISATION DES VALEURS EXTREMES ####
######################################################*


#================================================================================#
# Série temporelle de hs avec données de 2021-2024 #
#================================================================================#

threshold <- quantile(data$hs, 0.96, na.rm = TRUE)

declustered_exceedances <- data %>%
  mutate(
    declustered_hs = decluster(
      x = hs,
      data = .,
      threshold = threshold,
      r = 72
    ),
    excess = declustered_hs - threshold
  ) %>%
  filter(excess > 0)

top2 <- data %>%
  arrange(desc(hs)) %>%
  slice(c(1,4)) # Les 2ème et 3ème plus hauts Hs font partie de Ciaran.

ggplot(data, aes(x = time, y = hs)) +
  geom_point(color = "blue", alpha = 0.3) +
  geom_line(aes(y = threshold), color = "black", linetype = "dashed") +
  geom_point(
    data = data %>% filter(hs > threshold),
    color = "red"
  ) +
  geom_point(
    data = declustered_exceedances,
    color = "black", size = 2
  ) +
  ylim(0,8)+
  geom_point(
    data = top2[1, ], 
    shape = 21, fill = NA, color = "green", size = 5, stroke = 1
  ) +
  geom_text(
    data = top2[1, ],
    aes(label = "Pic tempête Ciaran"),
    vjust = -1
  ) +
  geom_point(
    data = top2[2, ], 
    shape = 21, fill = NA, color = "green", size = 5, stroke = 1
  ) +
  geom_text(
    data = top2[2, ],
    aes(label = "Pic tempête Johanna"),
    vjust = -1
  ) +
  theme_bw() +
  labs(
    title   = "Dépassements de seuil et maxima de tempêtes (1994–2024)",
    caption = "Source : RESOURCECODE",
    x       = "Temps",
    y       = "Hauteur significative des vagues (m)"
  )

#================================================================================#
# Zoom sur les deux tempêtes majeures #
#================================================================================#

index_storm1 <- which(data$time == top2$time[1])
index_storm2 <- which(data$time == top2$time[2])

padding <- 100

range_storm1 <- seq(index_storm1 - padding, index_storm1 + padding)
range_storm2 <- seq(index_storm2 - padding, index_storm2 + padding)

range_storm1 <- range_storm1[range_storm1 >= 1 & range_storm1 <= nrow(data)]
range_storm2 <- range_storm2[range_storm2 >= 1 & range_storm2 <= nrow(data)]

storm1_data <- data[range_storm1, ]
storm2_data <- data[range_storm2, ]

declust_storm1 <- declustered_exceedances %>%
  filter(time >= min(storm1_data$time) & time <= max(storm1_data$time))

declust_storm2 <- declustered_exceedances %>%
  filter(time >= min(storm2_data$time) & time <= max(storm2_data$time))

#========================#
# Graphique zoomé pour la première tempête
#========================#

p1 <- ggplot(storm1_data, aes(x = time, y = hs)) +
  geom_point(color = "blue", alpha = 0.3) +
  geom_line(aes(y = threshold), color = "black", linetype = "dashed") +
  geom_point(
    data = storm1_data %>% filter(hs > threshold),
    color = "red"
  ) +
  geom_point(
    data = declust_storm1,
    color = "black", size = 2
  ) +
  geom_point(
    data = top2[1, ],          
    shape = 21, fill = NA,
    color = "green", size = 5, stroke = 1
  ) +
  geom_line()+
  geom_text(
    data = top2[1, ],
    aes(label = "Pic tempête Ciaran"), 
    vjust = -1
  ) +
  ylim(0,8)+
  theme_bw() +
  labs(
    title   = "Zoom sur la tempête Ciaran",
    caption = "Source : RESOURCECODE",
    x       = "Dates (2023)",
    y       = "Hauteur significative des vagues (m)"
  )

#========================#
# Graphique zoomé pour la deuxième tempête
#========================#

p2 <- ggplot(storm2_data, aes(x = time, y = hs)) +
  geom_point(color = "blue", alpha = 0.3) +
  geom_line(aes(y = threshold), color = "black", linetype = "dashed") +
  geom_point(
    data = storm2_data %>% filter(hs > threshold),
    color = "red"
  ) +
  geom_point(
    data = declust_storm2,
    color = "black", size = 2
  ) +
  geom_point(
    data = top2[2, ],          
    shape = 21, fill = NA,
    color = "green", size = 5, stroke = 1
  ) +
  geom_line()+
  geom_text(
    data = top2[2, ],
    aes(label = "Pic tempête Johanna"), 
    vjust = -1
  ) +
  theme_bw() +
  ylim(0,8)+
  labs(
    title   = "Zoom sur la tempête Johanna",
    caption = "Source : RESOURCECODE",
    x       = "Dates (2008)",
    y       = "Hauteur significative des vagues (m)"
  )

p2+p1

#================================================================================#
# Choix du seuil u #
#================================================================================#

mrlplot(data$hs)
abline(v=3,col="red",lty=2)
title("Dépassement de seuil moyen des Hs en fonction du seuil")

threshrange.plot(data$hs)

# à partir de u=3, la variance devient importante. 

# on choisit u = quantile 96 de Hs = 3.05
quantile(data$hs,.96)


#================================================================================#
# Valeurs extrêmes - dépassements de seuil 1994-2020 #
#================================================================================#

resultExtreme <- return_values_plot(
  data = data[1:236687,],
  var_name = "hs",
  threshold_quantile = 0.96,
  r = 72, # on considère qu'il y au moins 72h entre deux tempêtes différentes.
  subtitle = "Modélisation avec les données de 1994 à 2020",
  showStorms=T
)

resultExtreme$fit_info

sigma <- round(resultExtreme$fit_info$scale,2)
xi <- round(resultExtreme$fit_info$shape,2)

d1 <- resultExtreme$density_plot+theme_bw()+labs(title = bquote("Densité de la GPD ajustée (1994-2020), " ~ 
                                                                sigma == .(sigma) ~ " et " ~ xi == .(xi)))
                                           
qq1 <- resultExtreme$qq_plot+theme_bw()+labs(title="QQ-plot de la GPD ajustée (1994-2020)")

rl1 <- resultExtreme$plot+theme_bw()

#================================================================================#
# Valeurs extrêmes - dépassements de seuil 1994-2024 #
#================================================================================#

resultExtreme <- return_values_plot(
  data = data,
  var_name = "hs",
  threshold_quantile = 0.96,
  r = 72,
  subtitle = "Modélisation avec les données de 1994 à 2024",
  showStorms = T
)

resultExtreme$fit_info

sigma <- round(resultExtreme$fit_info$scale,2)
xi <- round(resultExtreme$fit_info$shape,2)

d2 <- resultExtreme$density_plot+theme_bw()+labs(title = bquote("Densité de la GPD ajustée (1994-2024), " ~ 
                                                            sigma == .(sigma) ~ " et " ~ xi == .(xi)))
qq2 <- resultExtreme$qq_plot+theme_bw()+labs(title="QQ-plot de la GPD ajustée (1994-2024)")

rl2 <- resultExtreme$plot+theme_bw()

d1+d2
qq1+qq2
rl1+rl2

###################################################*
#### PARTIE 4 : MODELISATIONS EXTREMES AVEC COVARIABLES ####
###################################################*

#================================================================================#
# Ajustement d'une GP avec covariables #
#================================================================================#
data$cur_speed <- round(data$cur_speed, 2)

thresh <- quantile(data$hs, 0.96, na.rm=TRUE)

data$declusteredData <- decluster(x=data$hs, threshold=thresh, r=72)
data$excess <- data$declusteredData - thresh

is.na(data$excess[data$excess <= 0]) <- TRUE

data$exceed <- as.integer(!is.na(data$excess))

excess_df <- subset(data, exceed == 1)

gpd_mod <- evgam(
  list(
    excess ~ s(cur_speed, bs="cs", k=10) + s(tp, bs="cs", k=10) +
      ti(cur_speed,tp,bs="cs",k=10),  # scale
    ~ 1 # shape = constant
  ),
  data   = excess_df,
  family = "gpd"
)

summary(gpd_mod)
plot(gpd_mod,scheme=2,se=T,hcolors=viridis(100))

gpd_mod <- evgam(
  list(
    excess ~ s(cur_speed, bs="cs", k=10) + s(dp, bs="cc", k=10) +
      ti(cur_speed,dp,bs="cc",k=10),  # scale
    ~ 1 # shape = constant
  ),
  data   = excess_df,
  family = "gpd"
)

summary(gpd_mod)
plot(gpd_mod,scheme=2,se=T,hcolors=viridis(100))

############################################################*
#### PARTIE 5 : INFORMATIONS APPORTEES PAR LES SPECTRES ####
############################################################*

#================================================================================#
# Séries temporelles de Hs de tempête #
#================================================================================#

analyser_tempetes(data)

#================================================================================#
# Séries temporelles de spectres fréquentiels (Ciaran & Johanna) #
#================================================================================#

spectreCiaran <- readRDS("C:/these/presentations/RDT-13-03/code_presentation/spectres/spectreNov2023.RDS")
spectreJohanna <- readRDS("C:/these/presentations/RDT-13-03/code_presentation/spectres/spectreMars2008.RDS")
spectre98 <- readRDS("C:/these/presentations/RDT-13-03/code_presentation/spectres/spectreJan1998.RDS")

vec2008 <- seq(from = as.POSIXct("2008-03-01 00:00:00", tz = "UTC"),
               to   = as.POSIXct("2008-03-31 23:00:00", tz = "UTC"),
               by   = "hour")

vec2023 <- seq(from = as.POSIXct("2023-11-01 00:00:00", tz = "UTC"),
               to   = as.POSIXct("2023-11-30 23:00:00", tz = "UTC"),
               by   = "hour")

vec1998 <- seq(from = as.POSIXct("1998-01-01 00:00:00", tz = "UTC"),
               to   = as.POSIXct("1998-01-30 23:00:00", tz = "UTC"),
               by   = "hour")

nObs <- 100

max_Ciaran_date <- data[which.max(data$hs),"time"]
time_max_vec <- which(vec2023==max_Ciaran_date)
vec2023max <- vec2023[1:(time_max_vec+(nObs/2))]

max_Johanna_date <- data[which.max(data[year(data$time)!=2023,]$hs),"time"]
time_max_vec <- which(vec2008==max_Johanna_date)
vec2008max <- vec2008[(time_max_vec-(nObs/2)):(time_max_vec+(nObs/2))]

data_98 <- data[year(data$time)==1998,]
max_98_date <- data_98[which.max(data[year(data$time)==1998,]$hs),"time"]
time_max_vec <- which(vec1998==max_98_date)
vec1998max <- vec1998[(time_max_vec-(nObs/2)):(time_max_vec+(nObs/2))]

selected_Johanna <- extract_near_max_energy(spectreJohanna)
selected_Ciaran <- extract_near_max_energy(spectreCiaran)
selected_98 <- extract_near_max_energy(spectre98)


res98 <- plotSpectraTimeSeries(selected_98,timeValues = vec2008max, 
                               freqValues = round(rscd_freq,2),
                               titleSpectre = "Tempête de janvier 1998")


resJohanna <- plotSpectraTimeSeries(selected_Johanna,timeValues = vec2008max, 
                                    freqValues = round(rscd_freq,2),
                                    titleSpectre = "Tempête Johanna (2008)")


resCiaran <- plotSpectraTimeSeries(selected_Ciaran,timeValues = vec2023max,
                                   freqValues = round(rscd_freq,2),
                                   titleSpectre = "Tempête Ciaran (2023)")


res98$specter / resJohanna$specter / resCiaran$specter

#================================================================================#
# Spectres 1D Ciaran : comparaison JONSWAP et WW3 #
#================================================================================#

specOriginal <- readRDS("C:/these/presentations/RDT-13-03/code_presentation/spectres/1d_spec.rds")
spec <- specOriginal[[1]][,-1]

dates <- seq(from = as.POSIXct("1994-01-01 01:00:00", tz = "UTC"),
             to = as.POSIXct("2024-12-31 23:00:00", tz = "UTC"),
             by = "hour")

which(spec == max(spec),arr.ind=T) # max d'énergie dans le spectre à la 11è fréquence et 261537e obs.
cols <- 261532:261542
cols_hs <- data[261532:261542,"hs"]
cols_tp <- data[261532:261542,"tp"]

spec_subset <- spec[, cols]
df <- data.frame(Frequency = rscd_freq, spec_subset)

df_long <- melt(df, id.vars = "Frequency", variable.name = "Observation", value.name = "SpectralEnergy")

results <- vector("list", length = 10)
for (i in seq_along(cols_hs)) {
  results[[i]] <- jonswap(hs = cols_hs[i], tp = cols_tp[i])
}

jonswap_df <- do.call(rbind, lapply(seq_along(results), function(i) {
  data.frame(
    Frequency        = results[[i]]$freq,
    SpectralEnergy   = results[[i]]$spec,
    Observation      = i
  )
}))


df_long$Observation <- sort(rep(seq(1,11),36))

df_long$Source <- "wavewatch III"
jonswap_df$Source <- "jonswap"

hs_unique <- round(cols_hs[1:length(cols)],2)
mylabels <- paste0("Observation ", 1:length(cols_hs), " : Hs = ", hs_unique)

df_long$Observation <- factor(
  as.numeric(as.character(df_long$Observation)),
  levels = 1:length(cols_hs),
  labels = mylabels
)

jonswap_df$Observation <- factor(
  as.numeric(as.character(jonswap_df$Observation)),
  levels = 1:length(cols_hs),
  labels = mylabels
)

ggplot() +
  geom_line(data = df_long, 
            aes(x = Frequency, y = SpectralEnergy, color = Source),
            size = 0.8) +
  geom_line(data = jonswap_df, 
            aes(x = Frequency, y = SpectralEnergy, color = Source),
            size = 0.8) +
  facet_wrap(~ Observation, ncol = 3) +
  scale_color_manual(name = "Model", 
                     values = c("wavewatch III" = "blue", "jonswap" = "red")) +
  labs(x = "Frequency", y = "Spectral Energy Density", 
       title = "Spectres fréquentiels WW3 et estimation JONSWAP au cours de Ciaran ") +
  theme_bw()


###################################################*
#### PERSPECTIVES : CONTOURS ####
###################################################*

#================================================================================#
# Deux modélisations différentes des lois marginales # 
#================================================================================#

dataTable <- as.data.table(data)

ht_estimation <- fit_ht(dataTable, npy=8760, margin_thresh_count = 1000, dep_thresh_count = 1000)
wln_estimation <- fit_wln(dataTable, npy=8760, weibull_method = "lmom") # for isodensity

#================================================================================#
# Contours IFORM #
#================================================================================#

#H&T modeling
iform_estimation <- estimate_iform(ht_estimation, output_rp = c(1,2,5,20,50,100),n_point=100)
p1 <- plot_ec(iform_estimation, raw_data=dataTable)+theme_bw()+labs(title="Contours IFORM avec modélisation conditionnelle de Heffernan&Tawn")
p1 <- p1+theme(legend.position = "none")

# WLN modeling
iform_estimation2 <- estimate_iform(wln_estimation, output_rp = c(1,2,5,20,50,100),n_point=100)
p2 <- plot_ec(iform_estimation2, raw_data=dataTable)+theme_bw()+labs(title="Contours IFORM avec modélisation conditionnelle Weibull-lognormale")

p1 + p2 

#================================================================================#
# Contours d'isodensité #
#================================================================================#

iso_estimation <- estimate_iso(wln_estimation, output_rp = c(1,2,5,20,50,100),n_point = 100)
p3 <- plot_ec(iso_estimation, raw_data=dataTable)+theme_bw()+labs(title="Contours d'isodensité avec modélisation conditionnelle Weibull-lognormale")
p3

