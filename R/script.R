#' Compter le nombre d'adjoints au Maire
#'
#' Cette fonction compte le nombre d'adjoints au Maire dans une commune donnée.
#'
#' @param data Un data frame contenant les informations sur les élus de la commune.
#'
#' @return Un nombre entier représentant le nombre d'adjoints au Maire.
#' @export
compter_nombre_d_adjoints <- function(data) {
  nombre_adjoints <- sum(grepl("adjoint au Maire", data$`Libellé de la fonction`, ignore.case = TRUE), na.rm = TRUE)
  return(nombre_adjoints)
}


#' Trouver l'élu le plus âgé
#'
#' Cette fonction trouve l'élu le plus âgé dans une commune donnée.
#'
#' @param data Un data frame contenant les informations sur les élus de la commune.
#'
#' @return Une liste contenant le nom, le prénom et l'âge de l'élu le plus âgé.
#' @export
trouver_l_elu_le_plus_age <- function(data) {
  # Convertir la colonne Date de naissance en format Date
  data$`Date de naissance` <- as.Date(data$`Date de naissance`, format = "%d/%m/%Y")

  # Trouver l'élu le plus âgé
  elu_le_plus_age <- data[which.min(data$`Date de naissance`), ]

  # Calculer l'âge de l'élu le plus âgé
  age <- as.numeric(difftime(Sys.Date(), elu_le_plus_age$`Date de naissance`, units = "weeks")) %/% 52

  # Retourner un objet contenant le nom, le prénom et l'âge de l'élu le plus âgé
  return(list(nom = elu_le_plus_age$`Nom de l'élu`, prenom = elu_le_plus_age$`Prénom de l'élu`, age = age))
}


#' Calculer la distribution des âges
#'
#' Cette fonction calcule la distribution des âges des élus dans une commune donnée.
#'
#' @param data Un data frame contenant les informations sur les élus de la commune.
#'
#' @return Un vecteur contenant les quantiles de la distribution des âges.
#' @export
calcul_distribution_age <- function(data) {
  # Vérifiez que la colonne "Date de naissance" est au bon format
  data$`Date de naissance` <- as.Date(data$`Date de naissance`, format="%d/%m/%Y")

  # Calculer l'âge de chaque élu
  data$age <- sapply(data$`Date de naissance`, calculer_age)

  # Calculer les quantiles
  quantiles <- quantile(data$age, probs = c(0, 0.25, 0.5, 0.75, 1))

  # Retourner les quantiles
  return(quantiles)
}


#' Plotter les codes professionnels
#'
#' Cette fonction génère un bar chart pour les codes professionnels des élus dans une commune donnée.
#'
#' @param data Un data frame contenant les informations sur les élus de la commune.
#' @param title Le titre du graphique.
#'
#' @return Un objet ggplot représentant le bar chart.
#' @import ggplot2
#' @export
plot_code_professions <- function(data, title) {
  # Compter le nombre d'élus par code professionnel
  code_count <- table(data$`Code de la catégorie socio-professionnelle`)

  # Convertir en data.frame pour ggplot2
  df_code_count <- as.data.frame(code_count)
  colnames(df_code_count) <- c("Code professionnel", "Nombre d'élus")

  # Création d'un bar chart horizontal
  ggplot(df_code_count, aes(x = reorder(`Code professionnel`, `Nombre d'élus`), y = `Nombre d'élus`)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = title, x = "Code professionnel", y = "Nombre d'élus") +
    theme_minimal()
}


#' Résumer une commune
#'
#' Cette fonction résume les informations d'une commune donnée.
#'
#' @param x Un data frame de type commune contenant les informations sur les élus de la commune.
#'
#' @return Aucun. La fonction imprime les informations dans la console.
#' @export
summary.commune <- function(x) {
  # Vérifiez que l'objet est de type commune
  if (!inherits(x, "commune")) {
    stop("L'objet doit être de type 'commune'")
  }

  # Vérifiez que les colonnes nécessaires existent
  if (!all(c("Libellé de la commune", "Nom de l'élu", "Prénom de l'élu", "Date de naissance") %in% colnames(x))) {
    stop("Le data.frame doit contenir les colonnes 'Libellé de la commune', 'Nom de l'élu', 'Prénom de l'élu', et 'Date de naissance'")
  }

  # Nom de la commune
  nom_commune <- unique(x$`Libellé de la commune`)

  # Nombre d'élu.e.s de la commune
  nombre_elus <- nrow(x)

  #Calcul des âges
  x$age <- round(as.numeric(difftime(Sys.Date(), as.Date(x$`Date de naissance`, format="%d/%m/%Y"), units = "days")) / 365)

  # Distribution des âges des élu.e.s
  distribution_ages <- summary(x$age)

  # Nom et âge de l'élu.e le ou la plus âgé.e
  elu_plus_age <- x[which.max(x$age), ]
  nom_elu_plus_age <- paste(elu_plus_age$`Nom de l'élu`, elu_plus_age$`Prénom de l'élu`)
  age_elu_plus_age <- elu_plus_age$age

  # Impression des informations dans la console
  cat("Nom de la commune:", nom_commune, "\n")
  cat("Nombre d'élu.e.s:", nombre_elus, "\n")
  cat("Distribution des âges des élu.e.s:\n")
  print(distribution_ages)
  cat("Élu.e le ou la plus âgé.e: ", nom_elu_plus_age, ", Âge: ", age_elu_plus_age, "\n")
}


#' Résumer un département
#'
#' Cette fonction résume les informations d'un département donné.
#'
#' @param x Un data frame de type departement contenant les informations sur les élus du département.
#'
#' @return Aucun. La fonction imprime les informations dans la console.
#' @export
summary.departement <- function(x) {
  # Vérifiez que l'objet est de type departement
  if (!inherits(x, "departement")) {
    stop("L'objet doit être de type 'departement'")
  }

  # Vérifiez que les colonnes nécessaires existent
  if (!all(c("Libellé du département", "Libellé de la commune", "Nom de l'élu", "Prénom de l'élu", "Date de naissance") %in% colnames(x))) {
    stop("Le data.frame doit contenir les colonnes 'Libellé du département', 'Libellé de la commune', 'Nom de l'élu', 'Prénom de l'élu', et 'Date de naissance'")
  }

  # Nom du département
  nom_departement <- unique(x$`Libellé du département`)

  # Nombre de communes dans le département
  nombre_communes <- length(unique(x$`Libellé de la commune`))

  # Nombre d'élu.e.s dans le département
  nombre_elus <- nrow(x)

  # Calcul des âges des élu.e.s et arrondir à l'entier le plus proche
  x$age <- round(as.numeric(difftime(Sys.Date(), as.Date(x$`Date de naissance`, format="%d/%m/%Y"), units = "weeks")) / 52.25)

  # Distribution des âges des élu.e.s du département
  distribution_ages <- summary(x$age)

  # Nom et âge de l'élu.e le ou la plus âgé.e et sa commune
  elu_plus_age <- x[which.max(x$age), ]
  nom_elu_plus_age <- paste(elu_plus_age$`Nom de l'élu`, elu_plus_age$`Prénom de l'élu`)
  age_elu_plus_age <- elu_plus_age$age
  commune_elu_plus_age <- elu_plus_age$`Libellé de la commune`

  # Nom et âge de l'élu.e le ou la plus jeune et sa commune
  elu_plus_jeune <- x[which.min(x$age), ]
  nom_elu_plus_jeune <- paste(elu_plus_jeune$`Nom de l'élu`, elu_plus_jeune$`Prénom de l'élu`)
  age_elu_plus_jeune <- elu_plus_jeune$age
  commune_elu_plus_jeune <- elu_plus_jeune$`Libellé de la commune`

  # Nom de la commune à la moyenne d’âge la plus faible
  moyenne_age_par_commune <- aggregate(x$age, by=list(x$`Libellé de la commune`), mean)
  commune_moyenne_age_plus_faible <- moyenne_age_par_commune[which.min(moyenne_age_par_commune$x), 1]
  ages_commune_plus_faible <- x[x$`Libellé de la commune` == commune_moyenne_age_plus_faible, ]$age
  distribution_ages_commune_plus_faible <- summary(ages_commune_plus_faible)

  # Nom de la commune à la moyenne d’âge la plus élevée
  commune_moyenne_age_plus_elevee <- moyenne_age_par_commune[which.max(moyenne_age_par_commune$x), 1]
  ages_commune_plus_elevee <- x[x$`Libellé de la commune` == commune_moyenne_age_plus_elevee, ]$age
  distribution_ages_commune_plus_elevee <- summary(ages_commune_plus_elevee)

  # Impression des informations dans la console
  cat("Nom du département:", nom_departement, "\n")
  cat("Nombre de communes dans le département:", nombre_communes, "\n")
  cat("Nombre d'élu.e.s dans le département:", nombre_elus, "\n")
  cat("Distribution des âges des élu.e.s du département:\n")
  print(distribution_ages)
  cat("Élu.e le ou la plus âgé.e:", nom_elu_plus_age, ", Âge:", age_elu_plus_age, ", Commune:", commune_elu_plus_age, "\n")
  cat("Élu.e le ou la plus jeune:", nom_elu_plus_jeune, ", Âge:", age_elu_plus_jeune, ", Commune:", commune_elu_plus_jeune, "\n")
  cat("Commune à la moyenne d’âge la plus faible:", commune_moyenne_age_plus_faible, "\n")
  cat("Distribution des âges des élu.e.s pour cette commune:\n")
  print(distribution_ages_commune_plus_faible)
  cat("Commune à la moyenne d’âge la plus élevée:", commune_moyenne_age_plus_elevee, "\n")
  cat("Distribution des âges des élu.e.s pour cette commune:\n")
  print(distribution_ages_commune_plus_elevee)
}


#' Plotter les codes socio-professionnels pour une commune
#'
#' Cette fonction génère un bar chart pour les codes socio-professionnels des élus dans une commune donnée.
#'
#' @param x Un data frame contenant les informations sur les élus de la commune.
#'
#' @return Un objet ggplot représentant le bar chart.
#' @import ggplot2
#' @export
plot.commune <- function(x) {
  # Vérifiez que l'objet est de type commune
  if (!inherits(x, "commune")) {
    stop("L'objet doit être de type 'commune'")
  }

  # Vérifiez que les colonnes nécessaires existent
  if (!all(c("Libellé de la commune", "Libellé du département", "Code de la catégorie socio-professionnelle") %in% colnames(x))) {
    stop("Le data.frame doit contenir les colonnes 'Libellé de la commune', 'Libellé du département', et 'Code de la catégorie socio-professionnelle'")
  }

  # Nom de la commune et du département
  nom_commune <- unique(x$`Libellé de la commune`)
  nom_departement <- unique(x$`Libellé du département`)

  # Nombre d'élu.e.s de la commune
  nombre_elus <- nrow(x)

  # Titre du graphique
  titre_graphique <- paste(nom_commune, "-", nom_departement)

  # Compter le nombre d'élus par code socio-professionnel
  code_count <- table(x$`Code de la catégorie socio-professionnelle`)

  # Convertir en data.frame pour ggplot2
  df_code_count <- as.data.frame(code_count)
  colnames(df_code_count) <- c("Code socio-professionnel", "Nombre d'élus")

  # Création d'un bar chart horizontal avec ggplot2
  ggplot(df_code_count, aes(x = reorder(`Code socio-professionnel`, `Nombre d'élus`), y = `Nombre d'élus`)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(
      title = titre_graphique,
      x = paste("Libellés des codes professionnels pour les élus (", nombre_elus, " élus)", sep = ""),
      y = "Nombre d'élus"
    ) +
    theme_minimal()
}


#' Visualiser les catégories socio-professionnelles par département
#'
#' Cette fonction crée un diagramme à barres horizontal qui visualise la distribution des 10 principales catégories socio-professionnelles dans un département donné.
#'
#' @param x Un data.frame de classe 'departement' contenant les colonnes suivantes : 'Libellé du département', 'Libellé de la commune' et 'Code de la catégorie socio-professionnelle'.
#'
#' @return Un objet ggplot2 représentant un diagramme à barres horizontal.
#' @export
#'
#' @examples
#' # Supposons que 'dept_data' soit un data.frame de classe 'departement'
#' plot.departement(dept_data)
#'
plot.departement <- function(x) {
  # Vérifiez que l'objet est de type departement
  if (!inherits(x, "departement")) {
    stop("L'objet doit être de type 'departement'")
  }

  # Vérifiez que les colonnes nécessaires existent
  if (!all(c("Libellé du département", "Libellé de la commune", "Code de la catégorie socio-professionnelle") %in% colnames(x))) {
    stop("Le data.frame doit contenir les colonnes 'Libellé du département', 'Libellé de la commune', et 'Code de la catégorie socio-professionnelle'")
  }

  # Nom du département et nombre de communes
  nom_departement <- unique(x$`Libellé du département`)
  nombre_communes <- length(unique(x$`Libellé de la commune`))

  # Compter le nombre d'élus pour chaque code socio-professionnel
  code_count <- table(x$`Code de la catégorie socio-professionnelle`)
  top10_code_count <- sort(code_count, decreasing = TRUE)[1:10]

  # Convertir en data.frame pour ggplot2
  df_top10_code_count <- as.data.frame(top10_code_count)
  colnames(df_top10_code_count) <- c("Code socio-professionnel", "Nombre d'élus")

  # Titre du graphique
  titre_graphique <- paste(nom_departement, "-", nombre_communes, "communes")

  # Création d'un bar chart horizontal avec ggplot2
  ggplot(df_top10_code_count, aes(x = reorder(`Code socio-professionnel`, `Nombre d'élus`), y = `Nombre d'élus`)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(
      title = titre_graphique,
      x = paste("Libellés des 10 codes professionnels les plus représentés pour le département ", nom_departement),
      y = "Nombre d'élus"
    ) +
    theme_minimal()
}


