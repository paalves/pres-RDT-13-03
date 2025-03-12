if (!requireNamespace("resourcecodedata", quietly = TRUE)) {
  install.packages(
    "resourcecodedata",
    repos = "https://resourcecode-project.github.io/drat/",
    type  = "source"
  )
}

library(resourcecodedata)

required_packages <- c("dplyr", "ggplot2", "tidyr", "rlang","viridis","evd",
                       "resourcecode","splines", "reshape2", "ecsades",
                       "quantreg","extRemes", "gridExtra", "evgam",
                       "patchwork")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

rm(pkg) ; rm(required_packages)


#================================================================================#
# IMPORTATION DE DONNEES #
#================================================================================#

import_data <- function(node, end=as.POSIXct("2020-12-31 22:00:00",tz="UTC")) {
  
  library(resourcecodedata)
  
  df_raw <- resourcecode::get_parameters(
    parameters = c("hs", "tp", "dp",   
      "uwnd", "vwnd",   
      "ucur", "vcur"),  
    node = node,
    end=end
  )
  
  chemin_dossier <- "C:/these/presentations/RDT-13-03/code_presentation/data-sea-states-2021_2024"
  fichiers <- list.files(path = chemin_dossier, pattern = "\\.RDS$", full.names = TRUE)
  
  data_list <- lapply(fichiers, readRDS)
  
  noms_fichiers <- basename(fichiers)
  names(data_list) <- gsub("\\.RDS$", "", noms_fichiers)
  
  str(data_list)
  
  data_list <- lapply(data_list, function(x) { #on enlève le mois de déc 2020 déjà présent dans les données
    x$data <- x$data[-(1:744)]
    x
  })
  
  cleaned_list <- lapply(data_list, function(x) x[!(names(x) %in% c("parameter", "node_id"))])
  cleaned_df_list <- lapply(cleaned_list, function(x) as.data.frame(x))
  final_df <- do.call(cbind, cleaned_df_list)
  colnames(final_df) <- names(cleaned_df_list)
  final_df$time <- seq(as.POSIXct("2021-01-01 00:00:00",tz="UTC"),
                       as.POSIXct("2024-12-31 23:00:00",tz="UTC"),
                       by = "hour")
  colnames(final_df) <- gsub("^data_", "", colnames(final_df))
  
  final_df$tp <- 1/final_df$fp
  df_raw$fp <- 1/df_raw$tp
  
  final_df <- final_df[, names(df_raw)]
  
  final_data <- rbind(df_raw, final_df)
  
  df_raw <- final_data
  
  df <- df_raw %>%
    mutate(
      wind_speed = sqrt(uwnd^2 + vwnd^2),
      wind_dir   = (atan2(vwnd, uwnd) * 180/pi - 90) %% 360,
      
      cur_speed  = sqrt(ucur^2 + vcur^2),
      cur_dir    = (atan2(vcur, ucur) * 180/pi - 90) %% 360,
      
      dpCardinale = case_when(
        dp >= 337.5 | dp < 22.5   ~ "N",
        dp >= 22.5  & dp < 67.5   ~ "NE",
        dp >= 67.5  & dp < 112.5  ~ "E",
        dp >= 112.5 & dp < 157.5  ~ "SE",
        dp >= 157.5 & dp < 202.5  ~ "S",
        dp >= 202.5 & dp < 247.5  ~ "SO",
        dp >= 247.5 & dp < 292.5  ~ "O",
        dp >= 292.5 & dp < 337.5  ~ "NO"
    )
  )

  return(df)
}

#================================================================================#
# Ajustement du modèle de dépassements de seuil et calcul des valeurs de retour #
#================================================================================#
get_return_values <- function(data, var_name = "hs", threshold_quantile = 0.96, r = 72) {
  
  # 1) Calculer le seuil basé sur le quantile
  threshold <- quantile(data[[var_name]], threshold_quantile, na.rm = TRUE)
  
  # 2) Déclusteriser : decluster() retourne un vecteur, on stocke ça à part
  decl_data <- decluster(data[[var_name]], threshold, r = r)
  
  # 3) Vérifier le nombre de dépassements
  nb_exceed <- sum(data[[var_name]] > threshold, na.rm = TRUE)
  
  if (nb_exceed < 10) {
    # Choisir le seuil n'a peut-être laissé que trop peu de points
    stop("Trop peu de dépassements (moins de 10). Le modèle POT ne peut pas être ajusté correctement.")
  }
  
  # 4) Ajuster le modèle POT (Peak Over Thresholds)
  pot_fit <- tryCatch(
    {
      fevd(
        x         = decl_data,
        threshold = threshold, 
        type      = "GP",      # Generalized Pareto
        method    = "MLE",     # Maximum Likelihood
        time.units = "hours",  
        span      = 31
      )
    },
    error = function(e) {
      stop("Le fit avec fevd() a échoué : ", e$message)
    }
  )
  
  # 5) Générer les valeurs de retour (return_periods en années)
  return_periods <- c(2, 5, 10, 20, 50, 100, 200, 500)
  
  # Ici, return.level() renvoie en général un tableau (lower, est, upper)
  r_levels <- tryCatch(
    {
      return.level(pot_fit, return.period = return_periods, do.ci = TRUE)
    },
    error = function(e) {
      stop("Impossible de calculer les return.levels (IC) : ", e$message)
    }
  )
  
  # Vérifier que return.level() a bien renvoyé 3 colonnes :
  if (!is.matrix(r_levels) || ncol(r_levels) < 3) {
    stop("La sortie de return.level() ne comporte pas 3 colonnes (lower, estimate, upper).")
  }
  
  # 6) Construire le data.frame résultat
  result_df <- data.frame(
    return_period = return_periods,
    lower_ci      = as.numeric(r_levels[, 1]),
    return_level  = as.numeric(r_levels[, 2]),
    upper_ci      = as.numeric(r_levels[, 3])
  )
  
  # 7) Ajouter des attributs sur l'ajustement
  attr(result_df, "threshold")       = threshold
  attr(result_df, "num_exceedances") = nb_exceed
  
  # Vérifier que pot_fit$results$par a bien 2 paramètres (shape, scale)
  if (length(pot_fit$results$par) >= 2) {
    attr(result_df, "shape") = pot_fit$results$par[["shape"]]
    attr(result_df, "scale") = pot_fit$results$par[["scale"]]
  } else {
    warning("Le modèle fevd() n'a pas pu estimer xi (shape) et sigma (scale) correctement (pas assez de données ou problème de convergence).")
    attr(result_df, "shape") = NA
    attr(result_df, "scale") = NA
  }
  
  return(result_df)
}

#--------------------------------------------------------------------#
# Fonction d'affichage
#--------------------------------------------------------------------#

return_values_plot <- function(data, var_name = "hs", threshold_quantile = 0.96, r = 72, subtitle = "", showStorms = FALSE) {
  clean_data <- data[!is.na(data[[var_name]]), ]
  
  result <- tryCatch({
    get_return_values(clean_data, var_name, threshold_quantile, r)
  }, error = function(e) {
    warning("Erreur lors du calcul des valeurs de retour: ", conditionMessage(e))
    return(NULL)
  })
  
  if (is.null(result)) {
    return(NULL)
  }
  
  result$return_level_rounded <- round(result$return_level, 2)
  
  # Graphique
  plot <- ggplot(result, aes(x = return_period, y = return_level)) +
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), fill = "#619CFF", alpha = 0.2) +
    geom_line(color = "#619CFF", linewidth = 1) +
    geom_point(color = "black", size = 2.5) +
    geom_text(aes(label = return_level_rounded), vjust = -0.8, size = 3, color = "black", fontface = "bold") +
    scale_x_log10(
      breaks = c(2, 5, 10, 20, 50, 100, 200, 500),
      labels = c("2", "5", "10", "20", "50", "100", "200", "500")
    ) +
    labs(
      title    = paste0("Valeurs de retour de Hs (seuil: quantile ", threshold_quantile * 100, "%)"),
      subtitle = subtitle,
      x        = "Période de retour (années)",
      y        = "Hauteur significative Hs (m)",
      caption  = "Source : Resourcecode"
    ) +
    theme_minimal()
  
  # Ajouter les points rouges si showStorms est TRUE
  if (showStorms) {
    storms <- data.frame(
      hs = c(7.59, 7.12),
      name = c("Ciaran (2023)", "Johanna (2008)")
    )
    
    storms$return_period <- approx(result$return_level, result$return_period, xout = storms$hs)$y
    
    plot <- plot +
      geom_point(data = storms, aes(x = return_period, y = hs), color = "red", size = 3) +
      geom_text(
        data = storms, 
        aes(x = return_period, y = hs, label = paste0(name, " \n ", round(return_period), " ans")),
        vjust = 1.5, hjust = 0.5, color = "red", size = 3
      )
  }
  
  # Tableau récapitulatif
  return_table <- tidyr::pivot_wider(
    result %>% dplyr::select(return_period, return_level_rounded),
    names_from  = return_period,
    values_from = return_level_rounded,
    names_prefix = "T"
  )
  
  # Infos du fit
  fit_info <- data.frame(
    threshold       = attr(result, "threshold"),
    num_exceedances = attr(result, "num_exceedances"),
    shape           = attr(result, "shape"),
    scale           = attr(result, "scale")
  )
  

  # Recréer le même ajustement POT pour obtenir les diagnostic plots
  threshold <- attr(result, "threshold")
  shape <- attr(result, "shape")
  scale <- attr(result, "scale")
  
  # 1) Recréer le vecteur déclustered pour les QQ-plot et la densité
  decl_data <- decluster(clean_data[[var_name]], threshold, r = r)
  exceedances <- decl_data[decl_data > threshold] - threshold  # Excès au-dessus du seuil

  # 2) Ajuster à nouveau le modèle POT pour avoir l'objet complet
  pot_fit <- tryCatch(
    {
      fevd(
        x         = decl_data,
        threshold = threshold, 
        type      = "GP",
        method    = "MLE",
        time.units = "hours",
        span      = 31
      )
    },
    error = function(e) {
      warning("Impossible de recréer le modèle pour les diagnostics: ", e$message)
      return(NULL)
    }
  )
  
  shape <- pot_fit$results$par[["shape"]]
  scale <- pot_fit$results$par[["scale"]]

  # 3) QQ-plot
  if (!is.null(pot_fit)) {
    # Créer le QQ-plot en utilisant le modèle
    qq_plot <- ggplot() +
      # Utiliser plot.fevd mais récupérer les données
      stat_qq_line(aes(sample = exceedances), distribution = evd::qgpd,
                   dparams = list(shape = shape, scale = scale), 
                   color = "#619CFF", linewidth = 1) +
      stat_qq(aes(sample = exceedances), distribution = evd::qgpd,
              dparams = list(shape = shape, scale = scale), 
              shape = 16, size = 2) +
      labs(
        title = "QQ-plot du modèle GPD",
        x = "Quantiles théoriques",
        y = "Quantiles empiriques"
      ) +
      theme_minimal()
    
    # 4) Densité de la GPD
    # Générer des points pour tracer la densité théorique
    max_exceedance <- max(exceedances, na.rm = TRUE)
    x_vals <- seq(0.0001, max_exceedance * 1.2, length.out = 100)
    
    # Calculer la densité de la GPD pour les valeurs de x
    gpd_density <- dgpd(x_vals, shape = shape, scale = scale)
    density_data <- data.frame(x = x_vals, y = gpd_density)
    
    # Créer un histogramme des excédences avec la densité GPD superposée
    density_plot <- ggplot() +
      geom_histogram(aes(x = exceedances, y = after_stat(density)), 
                     bins = min(30, length(exceedances) %/% 5),
                     fill = "#619CFF", alpha = 0.5) +
      geom_line(data = density_data, aes(x = x, y = y), 
                color = "red", linewidth = 1) +
      labs(
        title = "Densité de la distribution GPD ajustée",
        x = paste0("Excès au-dessus du seuil (", round(threshold, 2), " m)"),
        y = "Densité"
      ) +
      theme_minimal()
  } else {
    qq_plot <- NULL
    density_plot <- NULL
  }
  
  return(list(
    plot         = plot,
    results      = result,
    fit_info     = fit_info,
    return_table = return_table,
    qq_plot      = qq_plot,           # Nouveau : QQ-plot
    density_plot = density_plot       # Nouveau : Graphique de densité
  ))
}
