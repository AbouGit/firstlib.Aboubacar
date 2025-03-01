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
plot_code_professions <- function(data, title = "Distribution des codes socio-professionnels des élus") {
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

  # Calcul des âges
  x$age <- round(as.numeric(difftime(Sys.Date(), as.Date(x$`Date de naissance`, format="%d/%m/%Y"), units = "days")) / 365)

  # Distribution des âges des élu.e.s
  distribution_ages <- summary(x$age)

  # Nom et âge de l'élu.e le ou la plus âgé.e
  elu_plus_age <- x[which.max(x$age), ]
  nom_elu_plus_age <- paste(elu_plus_age$`Nom de l'élu`, elu_plus_age$`Prénom de l'élu`)
  age_elu_plus_age <- elu_plus_age$age

  # Vérifier que les informations sont extraites correctement
  if (is.null(nom_elu_plus_age) | is.null(age_elu_plus_age)) {
    stop("Impossible de déterminer l'élu le plus âgé.")
  }

  # Retourner les informations
  return(list(
    nom_commune = nom_commune,
    nombre_elus = nombre_elus,
    distribution_ages = distribution_ages,
    elu_plus_age = list(nom = nom_elu_plus_age, age = age_elu_plus_age)
  ))
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
  # Vérifiez que l'objet est de type département
  if (!inherits(x, "departement")) {
    stop("L'objet doit être de type 'departement'")
  }

  # Vérifiez que les colonnes nécessaires existent
  if (!all(c("Libellé du département", "Nom de l'élu", "Prénom de l'élu", "Date de naissance") %in% colnames(x))) {
    stop("Le data.frame doit contenir les colonnes 'Libellé du département', 'Nom de l'élu', 'Prénom de l'élu', et 'Date de naissance'")
  }

  # Nom du département
  nom_departement <- unique(x$`Libellé du département`)

  # Nombre d'élu.e.s du département
  nombre_elus <- nrow(x)

  # Calcul des âges
  x$age <- round(as.numeric(difftime(Sys.Date(), as.Date(x$`Date de naissance`, format="%d/%m/%Y"), units = "days")) / 365)

  # Distribution des âges des élu.e.s
  distribution_ages <- summary(x$age)

  # Nom et âge de l'élu.e le ou la plus âgé.e
  elu_plus_age <- x[which.max(x$age), ]
  nom_elu_plus_age <- paste(elu_plus_age$`Nom de l'élu`, elu_plus_age$`Prénom de l'élu`)
  age_elu_plus_age <- elu_plus_age$age

  # Vérifier que les informations sont extraites correctement
  if (is.null(nom_elu_plus_age) | is.null(age_elu_plus_age)) {
    stop("Impossible de déterminer l'élu le plus âgé.")
  }

  # Retourner les informations
  return(list(
    nom_departement = nom_departement,
    nombre_elus = nombre_elus,
    distribution_ages = distribution_ages,
    elu_plus_age = list(nom = nom_elu_plus_age, age = age_elu_plus_age)
  ))
}



#' Plotter les codes socio-professionnels des élus dans une commune
#'
#' Cette fonction génère un graphique à barres horizontales représentant le nombre d'élus
#' par code socio-professionnel dans une commune donnée. Elle est conçue pour être utilisée
#' avec des objets de type `commune`.
#'
#' @param x Un data frame de type `commune` contenant les informations sur les élus d'une commune.
#' Ce data frame doit contenir les colonnes suivantes :
#' - `Libellé de la commune` : Nom de la commune
#' - `Libellé du département` : Nom du département
#' - `Code de la catégorie socio-professionnelle` : Code identifiant la catégorie socio-professionnelle de chaque élu
#'
#' @return Un graphique de type `ggplot` (bar chart horizontal) affichant le nombre d'élus
#' par code socio-professionnel dans la commune.
#' Le graphique inclut un titre, les axes x et y, et est stylisé avec le thème minimal de ggplot2.
#'
#' @import ggplot2
#' @export
#'
#' @examples
#' data_commune <- data.frame(
#'   `Libellé de la commune` = rep("Commune A", 10),
#'   `Libellé du département` = rep("Département 1", 10),
#'   `Code de la catégorie socio-professionnelle` = c("Prof1", "Prof2", "Prof2", "Prof1", "Prof3", "Prof1", "Prof2", "Prof1", "Prof3", "Prof2")
#' )
#' plot.commune(data_commune)
plot.commune <- function(x) {
  # Vérifiez que l'objet est de type 'commune'
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

  # Création du graphique
  plot <- ggplot(x, aes(x = `Libellé de la commune`)) +
    geom_bar(stat = "count") +
    labs(title = titre_graphique, x = "Commune", y = "Nombre d'élus") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

  # Retourner le graphique
  return(plot)
}


#' Plotter les codes socio-professionnels des élus dans un département
#'
#' Cette fonction génère un graphique à barres horizontales représentant les 10 codes socio-professionnels
#' les plus représentés parmi les élus d'un département donné. Elle est conçue pour être utilisée avec des objets
#' de type `departement`.
#'
#' @param x Un data frame de type `departement` contenant les informations sur les élus d'un département.
#' Ce data frame doit contenir les colonnes suivantes :
#' - `Libellé du département` : Nom du département
#' - `Libellé de la commune` : Nom de la commune
#' - `Code de la catégorie socio-professionnelle` : Code identifiant la catégorie socio-professionnelle de chaque élu
#'
#' @return Un graphique de type `ggplot` (bar chart horizontal) affichant les 10 codes socio-professionnels
#' les plus représentés parmi les élus dans le département.
#' Le graphique inclut un titre, les axes x et y, et est stylisé avec le thème minimal de ggplot2.
#'
#' @import ggplot2
#' @export
#'
#' @examples
#' data_departement <- data.frame(
#'   `Libellé du département` = rep("Département 1", 20),
#'   `Libellé de la commune` = rep("Commune A", 20),
#'   `Code de la catégorie socio-professionnelle` = c("Prof1", "Prof2", "Prof2", "Prof1", "Prof3", "Prof1", "Prof2", "Prof1", "Prof3", "Prof2",
#'                                                  "Prof1", "Prof3", "Prof1", "Prof1", "Prof2", "Prof1", "Prof3", "Prof2", "Prof1", "Prof3")
#' )
#' plot.departement(data_departement)
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




library(readr)
elus_sample <- read_delim("C:/M1_ECAP/R avancé et Github/elus_sample.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

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

library(testthat)

library(testthat)

# Tests de la fonction compter_nombre_d_adjoints()
test_that("compter_nombre_d_adjoints renvoie un nombre entier", {
  result <- compter_nombre_d_adjoints(elus_sample)
  expect_type(result, "integer") # Vérifie que le résultat est bien un entier
})

test_that("compter_nombre_d_adjoints compte correctement les adjoints", {
  # Créer un échantillon de test avec des adjoints
  elus_test <- data.frame(
    `Libellé de la fonction` = c("Adjoint au Maire", "Maire", "Adjoint au Maire", "Conseiller")
  )

  result <- compter_nombre_d_adjoints(elus_test)
  expect_equal(result, 2) # Vérifie qu'on trouve bien 2 adjoints
})


# Tests de la fonction trouver_l_elu_le_plus_age()

test_that("trouver_l_elu_le_plus_age renvoie un nom et prénom", {
  result <- trouver_l_elu_le_plus_age(elus_sample)

  # Vérifier que le nom et prénom sont présents
  expect_true("nom" %in% names(result))
  expect_true("prenom" %in% names(result))
})

test_that("trouver_l_elu_le_plus_age renvoie un âge positif", {
  result <- trouver_l_elu_le_plus_age(elus_sample)

  # Vérifier que l'âge est un entier positif
  expect_true(result$age >= 0)
})


# Tests de la fonction calcul_distribution_age()

calculer_age <- function(date_naissance) {
  today <- Sys.Date()
  age <- as.numeric(difftime(today, as.Date(date_naissance, format="%d/%m/%Y"), units="weeks")) %/% 52
  return(age)
}

test_that("calcul_distribution_age renvoie des quantiles", {
  result <- calcul_distribution_age(elus_sample)

  # Vérifier que le résultat contient bien les quantiles (25%, 50%, 75%)
  expect_true("25%" %in% names(result))
  expect_true("50%" %in% names(result))
  expect_true("75%" %in% names(result))
})

test_that("calcul_distribution_age renvoie des quantiles numériques", {
  result <- calcul_distribution_age(elus_sample)

  # Vérifier que les quantiles sont des valeurs numériques
  expect_true(is.numeric(result[["25%"]]))
  expect_true(is.numeric(result[["50%"]]))
  expect_true(is.numeric(result[["75%"]]))
})


# Tests pour la fonction plot_code_professions()
library(ggplot2)
test_that("plot_code_professions génère un graphique", {
  result <- plot_code_professions(elus_sample)

  # Vérifier que le résultat est de type ggplot
  expect_s3_class(result, "gg")
})

test_that("plot_code_professions contient un titre", {
  result <- plot_code_professions(elus_sample)

  # Vérifier que le graphique a un titre
  expect_true("title" %in% names(result$labels))
})


# Tests pour summary.commune()
class(elus_sample) <- c("commune", "data.frame")

test_that("summary.commune renvoie l'élu le plus âgé correctement", {
  # Appliquer la fonction à un sous-ensemble de données
  result <- summary.commune(elus_sample)

  # Vérifier que la clé "elu_plus_age" existe dans le résultat
  expect_true("elu_plus_age" %in% names(result))

  # Vérifier que la sous-clé "nom" dans "elu_plus_age" n'est pas NULL
  expect_true(!is.null(result$elu_plus_age$nom))
})

test_that("summary.commune renvoie la bonne distribution des âges", {
  # Appliquer la fonction à un sous-ensemble de données
  result <- summary.commune(elus_sample)

  # Vérifier que la distribution des âges contient des informations
  expect_true("distribution_ages" %in% names(result))
  expect_true(length(result$distribution_ages) > 0)  # Vérifie qu'il y a bien une distribution
})

test_that("summary.commune calcule le nombre d'élus", {
  # Appliquer la fonction à un sous-ensemble de données
  result <- summary.commune(elus_sample)

  # Vérifier que le nombre d'élus est correct
  expect_true("nombre_elus" %in% names(result))
  expect_true(result$nombre_elus > 0)
})


# Tests fonction summary.departement()
 class(elus_sample) <- c("departement", "data.frame")

 test_that("summary.departement renvoie l'élu le plus âgé correctement", {
   # Appliquer la fonction à un sous-ensemble de données
   result <- summary.departement(elus_sample)

   # Vérifier que la clé "elu_plus_age" existe dans le résultat
   expect_true("elu_plus_age" %in% names(result))

   # Vérifier que la sous-clé "nom" dans "elu_plus_age" n'est pas NULL
   expect_true(!is.null(result$elu_plus_age$nom))
 })

 test_that("summary.departement renvoie la bonne distribution des âges", {
   # Appliquer la fonction à un sous-ensemble de données
   result <- summary.departement(elus_sample)

   # Vérifier que la distribution des âges contient des informations
   expect_true("distribution_ages" %in% names(result))
   expect_true(length(result$distribution_ages) > 0)  # Vérifie qu'il y a bien une distribution
 })

 test_that("summary.departement calcule le nombre d'élus", {
   # Appliquer la fonction à un sous-ensemble de données
   result <- summary.departement(elus_sample)

   # Vérifier que le nombre d'élus est correct
   expect_true("nombre_elus" %in% names(result))
   expect_true(result$nombre_elus > 0)
 })


# Tests fonction plot.commune()
 class(elus_sample) <- c("commune", "data.frame")

 test_that("plot.commune génère un graphique", {
   # Appliquer la fonction à un sous-ensemble de données
   result <- plot.commune(elus_sample)

   # Vérifier que la sortie est un objet ggplot
   expect_true(inherits(result, "gg"))  # Un objet ggplot devrait être retourné

   # Afficher le titre généré pour comprendre sa structure
   print(result$labels$title)

   # Générer le titre attendu en fonction des données de 'elus_sample'
   expected_title_commune <- unique(elus_sample$`Libellé de la commune`)
   expected_title_departement <- unique(elus_sample$`Libellé du département`)

   # Vérifier que le titre contient la commune et le département
   expect_true(any(grepl(expected_title_commune[1], result$labels$title)))  # Utilisation du premier élément pour la commune
   expect_true(any(grepl(expected_title_departement[1], result$labels$title)))  # Utilisation du premier élément pour le département
 })






 test_that("plot.commune vérifie la présence des colonnes requises", {
   # Appliquer la fonction avec un jeu de données sans la colonne "Libellé de la commune"
   data_incorrect <- elus_sample[, !names(elus_sample) %in% "Libellé de la commune"]

   # Vérifier que la fonction renvoie une erreur si la colonne "Libellé de la commune" est absente
   expect_error(plot.commune(data_incorrect), "Le data.frame doit contenir les colonnes 'Libellé de la commune', 'Libellé du département', et 'Code de la catégorie socio-professionnelle'")
 })


# Tests fonction plot.departement()
 class(elus_sample) <- c("departement", "data.frame")

 test_that("plot.departement génère un graphique", {
   # Appliquer la fonction à un sous-ensemble de données
   result <- plot.departement(elus_sample)

   # Vérifier que la sortie est un objet ggplot
   expect_true(inherits(result, "gg"))  # Un objet ggplot devrait être retourné
 })


 test_that("plot.departement génère un graphique avec le titre correct", {
   # Appliquer la fonction à un sous-ensemble de données
   result <- plot.departement(elus_sample)

   # Générer le titre attendu en fonction des données de 'elus_sample'
   expected_title_departement <- unique(elus_sample$`Libellé du département`)[1]  # Utilisation du premier élément pour le département

   # Récupérer le titre du graphique généré
   actual_title <- result$labels$title

   # Extraire la partie du titre avant les tirets (ce qui correspond au département)
   actual_title_departement <- strsplit(actual_title, " - ")[[1]][1]

   # Vérification que le titre commence bien par le nom du département
   expect_true(actual_title_departement == expected_title_departement,
               info = paste("Expected title to start with department:", expected_title_departement,
                            "but got:", actual_title_departement))
 })







