#' Compter le nombre d'adjoints au Maire
#'
#' Cette fonction compte le nombre d'adjoints au Maire dans une commune donnée.
#'
#' @param data Un data frame contenant les informations sur les élus de la commune.
#'
#' @return Un nombre entier représentant le nombre d'adjoints au Maire.
#' @export
#' @dependencies
#' - Aucun package externe requis.
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
#' @dependencies
#' - **base** : pour manipuler les dates (fonction `as.Date()` et `difftime()`).
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
#' @dependencies
#' - **base** : pour manipuler les dates (fonction `as.Date()` et `sapply()`).
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
#' @dependencies
#' - **ggplot2** : pour la création de graphiques (fonction `ggplot()`).
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
#' @dependencies
#' - **base** : pour manipuler les dates et calculer les âges (fonction `difftime()`).
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
#' @dependencies
#' - **base** : pour manipuler les dates et calculer les âges (fonction `difftime()`).
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
  nom_elu_plus_jeune <- paste(elu_plus_jeune$`Nom de l'élu`, elu_plus_jeune$`Prénom`)
}



library(readr)
elus_sample <- read_delim("elus_sample.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

library(usethis)

use_data(elus_sample)

#' @title Jeu de données des élus
#'
#' @description
#' Le jeu de données `elus_sample` contient des informations sur les élus dans différentes collectivités, incluant des détails sur leur fonction, leur catégorie socio-professionnelle, leur sexe, et d'autres caractéristiques démographiques et administratives.
#'
#' @format Un data frame avec 9752 lignes et 16 colonnes. Chaque ligne représente un élu et chaque colonne contient une caractéristique de l'élu.
#' \describe{
#'   \item{Code du département}{Le code du département auquel l'élu appartient.}
#'   \item{Libellé du département}{Le nom du département auquel l'élu appartient.}
#'   \item{Code de la collectivité à statut particulier}{Le code de la collectivité à statut particulier si applicable.}
#'   \item{Libellé de la collectivité à statut particulier}{Le nom de la collectivité à statut particulier si applicable.}
#'   \item{Code de la commune}{Le code de la commune de l'élu.}
#'   \item{Libellé de la commune}{Le nom de la commune de l'élu.}
#'   \item{Nom de l'élu}{Le nom de l'élu.}
#'   \item{Prénom de l'élu}{Le prénom de l'élu.}
#'   \item{Code sexe}{Le code correspondant au sexe de l'élu.}
#'   \item{Date de naissance}{La date de naissance de l'élu au format JJ/MM/AAAA.}
#'   \item{Code de la catégorie socio-professionnelle}{Le code de la catégorie socio-professionnelle de l'élu.}
#'   \item{Libellé de la catégorie socio-professionnelle}{Le nom de la catégorie socio-professionnelle de l'élu.}
#'   \item{Date de début du mandat}{La date à laquelle l'élu a commencé son mandat.}
#'   \item{Libellé de la fonction}{La fonction exercée par l'élu (par exemple, maire, adjoint au Maire).}
#'   \item{Date de début de la fonction}{La date à laquelle l'élu a commencé sa fonction.}
#'   \item{Code nationalité}{Le code de la nationalité de l'élu.}
#' }
#'
#' @source Source des données : [indique la source si disponible, par exemple, une base de données publique, un fichier CSV, etc.]
#'
#' @usage data("elus_sample")
#'
#' @examples
#' data("elus_sample")
#' head(elus_sample)
#' summary(elus_sample)
"elus_sample"

#Tests unitaires pour les fonctions

library(testthat)

#Tests pour compter_nombre_d_adjoints()

test_that("compter_nombre_d_adjoints fonctionne correctement", {
  # Test 1 : Vérifier si la fonction renvoie un nombre entier
  data <- data.frame(
    `Libellé de la fonction` = c("Adjoint au Maire", "Maire", "Adjoint au Maire", "Conseiller")
  )
  expect_equal(compter_nombre_d_adjoints(data), 2)

  # Test 2 : Vérifier si la fonction gère les NA dans les données
  data_na <- data.frame(
    `Libellé de la fonction` = c("Adjoint au Maire", NA, "Adjoint au Maire", "Conseiller")
  )
  expect_equal(compter_nombre_d_adjoints(data_na), 2)
})

