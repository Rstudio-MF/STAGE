#VERSION 1
##### CHARGEMENT DES PACKAGES ####
library(qs)
library(tidyverse)
library(dplyr)
library(naniar)
library(stringr)
library(lubridate)
library(openxlsx)
library(ggplot2)

#### CHARGEMENT DE DONNÉES ####
urg <- qread("/Users/Manon/Desktop/STAGE/DATA/sa2014.qs", use_alt_rep=FALSE, strict=FALSE, nthreads=1)
summary(urg)
urg <- as.matrix(urg)
urg <- as.data.frame(urg)

#### I) DESCRIPTION / ÉTUDE DE VALIDITÉ ####
#Recoder le niveau de tri : 
#Création colonne CIMU
urg <- urg %>%
  mutate(
    CIMU = grav_res)

#Réajustement du bon niveau de tri (35 = 4, 4 = 5)
urg <- urg %>%
  mutate(CIMU = ifelse(grepl("4", grav_lib), "4", CIMU))

urg <- urg %>%
  mutate(CIMU = ifelse(grepl("5", grav_lib), "5", CIMU))

urg$CIMU <- as.numeric(urg$CIMU)
#On ordonne la CIMU
urg$CIMU <- factor(urg$CIMU, levels = c(
  "1", "2", "3", "4", "5"), ordered = TRUE)

#A noter qu'il y a 737 niveau de tri manquants
missing_values <- which(is.na(urg$CIMU))

#Satistiques descriptives intéressantes
#Création colonne age + médiane de l'âge
urg$age <- as.numeric(difftime(urg$datej, urg$datenaiss, units = "weeks")) / 52.25
hist(urg$age)

#Proportion homme/femme
urg <- urg %>%
  mutate(
    sex = sexe)
urg <- urg %>%
  mutate(sex = ifelse(grepl("F", sexe), "F", sex))

urg <- urg %>%
  mutate(sex = ifelse(grepl("M", sexe), "M", sex))

urg$sex <- as.factor(urg$sex)
summary(urg$sex)
plot(urg$sex)
#I pour indéterminé ?
which(urg$sex == "I")
prop.table(table(urg$sex))

#Répartition du niveau de triage
summary(urg$CIMU)
prop.table(table(urg$CIMU))
#On a majoritairement des tris 3 et 4
plot(urg$CIMU)

#GESTION DES DONNÉES DE TEMPS
#Heure d'inscription : harrived
urg <- unite(urg, "harrived", c("datej", "heure"), sep = " " )
urg$harrived2 <- as.POSIXct(urg$harrived, format = "%Y-%m-%d %H:%M")
urg$harrived2 <- ymd_hms(urg$harrived)
#Heure de prise en charge à l'IAO : hpeciao_res
urg$hpeciao <- as.POSIXct(urg$hpeciao_res, format = "%Y-%m-%d %H:%M")
#Heure de sortie : hexit
urg <- unite(urg, "hexit", c("datesort", "heuresort"), sep = " ")
urg$hexit2 <- as.POSIXct(urg$hexit, format = "%Y-%m-%d %H:%M")

#Délai inscription et IAO
urg$delay1 <- difftime(urg$hpeciao, urg$harrived2, units = "mins")
urg$delay1 <- as.numeric(urg$delay1)
delay1_period <- as.period(urg$delay1)

##Délai IAO et médecin (interne/sénior)
# afficher les lignes où la condition est vraie
urg %>% filter(str_detect(compte_rendu, "Interne") & str_detect(compte_rendu, "IAO")) %>% View()

#Ce qui fonctionne
# Motif de recherche pour extraire les dates
pattern <- "(Senior :|Interne :)\\s*(.*?)\\s*(\\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2})"

# Extraire tous les matches du motif de recherche dans chaque cellule
matches <- str_extract_all(urg$compte_rendu, pattern)

# Extraire les deux dates de chaque vecteur de matches
dates <- lapply(matches, function(x) {
  if (length(x) >= 2) {
    c(str_extract(x[1], "\\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2}"), 
      str_extract(x[2], "\\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2}"))
  } else {
    c(NA, NA)
  }
})

# Créer un data.frame avec les dates extraites
df <- data.frame(date_senior = sapply(dates, "[[", 1),
                 date_interne = sapply(dates, "[[", 2))

#Convertir en YYYY-MM-DD HH:MM
df <- as.data.frame(lapply(df, as.POSIXct, format="%d/%m/%Y %H:%M"))
urg <- cbind(urg, df)

urg$delay2int <- difftime(urg$date_interne, urg$hpeciao, units = "mins")
urg$delay2sen <- difftime(urg$date_senior, urg$hpeciao, units = "mins")

#Durée IAO/sortie
urg$delay3 <- difftime(urg$hexit2, urg$hpeciao, units = "mins")
urg$delay3 <- as.numeric(urg$delay3)
#Durée médecin/sortie
urg$delay4 <- difftime(urg$hexit2, urg$date_senior)
#Durée totale (arrivée/sortie)
urg$delay5 <- difftime(urg$hexit2, urg$harrived2, units = "mins")
urg$delay5 <- as.numeric(urg$delay5)
urg$delay5 <- as.period(urg$delay5)

#Procédures techniques
  #nursing procedures : injection, KT, perfusion, ECG, O2 ou aérosols, pansement complexe)
  #procédure médicale : suture, plâtre/attelle, réduction luxation, aspiration/drainage, packing,
#                      extraction corps étranger, réanimation
  #examens complémentaires : RX, tests labo, avis spécialisé, endoscopie

#A partir des colonnes existantes
actesb <- urg %>%
  distinct(actes_b001_lib)

as.data.frame(table(urg$actes_b001_lib))

table(urg$CIMU, urg$actes_b001_lib)

actesd <- urg %>%
  distinct(actes_d001_lib)

actesh <- urg %>%
  distinct(actes_h001_lib)

actesj <- urg %>%
  distinct(actes_j001_lib)

actesl <- urg %>%
  distinct(actes_l001_lib)

actesm <- urg %>%
  distinct(actes_m001_lib)

actese <- urg %>%
  distinct(actes_e001_lib)

actesr <- urg %>%
  distinct(actes_r001_lib)

actesz <- urg %>%
  distinct(actes_z001_lib)

#On va recréer des catégories de variables
#On fait l'état des lieux des différents libellés
actes_xls <- createWorkbook()

#actes b
addWorksheet(actes_xls, "actes_bl")
# Écrire le dataframe dans la feuille de calcul
writeData(actes_xls, sheet = "actes_bl", x = as.data.frame(table(urg$actes_b001_lib)), colNames = TRUE)

#actes d
addWorksheet(actes_xls, "actes_dl")
writeData(actes_xls, sheet = "actes_dl", x = as.data.frame(table(urg$actes_d001_lib)), colNames = TRUE)

#actes h
addWorksheet(actes_xls, "actes_hl")
writeData(actes_xls, sheet = "actes_hl", x = as.data.frame(table(urg$actes_h001_lib)), colNames = TRUE)

#actes j
addWorksheet(actes_xls, "actes_jl")
writeData(actes_xls, sheet = "actes_jl", x = as.data.frame(table(urg$actes_j001_lib)), colNames = TRUE)

#actes l
addWorksheet(actes_xls, "actes_ll")
writeData(actes_xls, sheet = "actes_ll", x = as.data.frame(table(urg$actes_l001_lib)), colNames = TRUE)

#actes m
addWorksheet(actes_xls, "actes_ml")
writeData(actes_xls, sheet = "actes_ml", x = as.data.frame(table(urg$actes_m001_lib)), colNames = TRUE)

#actes e
addWorksheet(actes_xls, "actes_el")
writeData(actes_xls, sheet = "actes_el", x = as.data.frame(table(urg$actes_e001_lib)), colNames = TRUE)

#actes r
addWorksheet(actes_xls, "actes_rl")
writeData(actes_xls, sheet = "actes_rl", x = as.data.frame(table(urg$actes_r001_lib)), colNames = TRUE)

#actes z
addWorksheet(actes_xls, "actes_zl")
writeData(actes_xls, sheet = "actes_zl", x = as.data.frame(table(urg$actes_z001_lib)), colNames = TRUE)

# Enregistrer le fichier Excel
saveWorkbook(actes_xls, "detail.xlsx", overwrite = TRUE)


#Motif de consultations
motif_consult <- urg %>%
  distinct(motif1_lib)
prop.table(table(motif_consult))
addWorksheet(actes_xls, "motif_consult1")
writeData(actes_xls, sheet = "motif_consult1", x = as.data.frame(table(urg$motif1_lib)), colNames = TRUE)
addWorksheet(actes_xls, "motif_consult2")
writeData(actes_xls, sheet = "motif_consult2", x = as.data.frame(table(urg$motif2_lib)), colNames = TRUE)

#On a quand même 44220 motifs 1 manquants sur 57082 patients


##Devenir : 
#On regarde la colonne secteur
distinct_vals <- urg %>% 
  distinct(secteur)

#On a RX-CS, OBS, UHCD, URG, ATT

#Pour la colonne decisiomedlib_1
distinct_decisions <- urg %>% 
  distinct(decisionmed1_lib)

#Création colonne devenir : sortie/hospitalisation en attente d'avoir les codes
urg <- urg %>%
  mutate(
    devenir = decisionmed1_lib)
urg <- urg %>%
  mutate(devenir = ifelse(grepl("Non-Admission", decisionmed1_lib), "Sortie", devenir))

urg <- urg %>%
  mutate(devenir = ifelse(!grepl("Non-Admission", decisionmed1_lib), "Hospitalisation", devenir))
prop.table(table(urg$devenir))

tab <- table(urg$CIMU, urg$devenir)
prop_tab <- prop.table(tab)
prop_tab




ggplot(urg, aes(x = CIMU, y = devenir, fill = devenir)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Hospitalisation", "Sortie")) +
  labs(title = "Répartition admission/hospitalisation en fonction de la CIMU",
       x = "CIMU", y = "Proportion")

#Décision d'orientation
#Extraction des termes recherchés
terms <- str_extract_all(urg$compte_rendu, "(?<=Decision d'orientation : ).*(?=Transfusion)")

#Fonction gsub() pour supprimer les dates de chaque terme extrait
terms <- lapply(terms, function(x) gsub("\\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2}", "", x))

#Nouvelle colonne
urg$orientation <- sapply(terms, paste, collapse = ", ")


#Ressources utilisées
#Entre Biologie et ECG
terms_bio <- str_extract(urg$compte_rendu, "(?<=Biologie).*(?=ECG)")

bio <- str_extract(texte, "(?<=Biologie ).*(?= ECG)")
str_extract(texte, "(?<=Biologie: ).*(?= ECG:)")

urg$bio <- sapply(terms_bio, paste, collapse = ", ")

#Entre ECG et Radiologie
terms_ECG <- str_extract(urg$compte_rendu, "(?<=ECG : ).*(?=Radiologie)")

#Entre Imagerie spécialisee et Autres
terms_imagerie <- str_extract(urg$compte_rendu, "(?<=Imagerie spécialisée : ).*(?=Autres")
#Entre Autres et .
terms_autres <- str_extract(urg$compte_rendu, "(?<=Autres : ).*(?=Medecin senior ayant valide le dossier ")


# Split the text into lines
lines <- strsplit(urg$compte_rendu, "\n")[[1]]

# Initialize empty vectors for each column
biologie <- character()
ecg <- character()
radiologie <- character()
imagerie <- character()
autres <- character()

# Loop through each line and store the terms in the corresponding column
section <- ""
for (line in lines) {
  if (grepl("Biologie", line)) {
    section <- "biologie"
  } else if (grepl("ECG", line)) {
    section <- "ecg"
  } else if (grepl("Radiologie", line)) {
    section <- "radiologie"
  } else if (grepl("Imagerie specialisee", line)) {
    section <- "imagerie"
  } else if (grepl("Autres", line)) {
    section <- "autres"
  } else {
    if (section == "biologie") {
      biologie <- c(biologie, line)
    } else if (section == "ecg") {
      ecg <- c(ecg, line)
    } else if (section == "radiologie") {
      radiologie <- c(radiologie, line)
    } else if (section == "imagerie") {
      imagerie <- c(imagerie, line)
    } else if (section == "autres") {
      autres <- c(autres, line)
    }
  }
}

# Combine the columns into a single data frame
df <- data.frame(biologie, ecg, radiologie, imagerie, autres)

# Remove the section names from each column
df$biologie <- gsub("Biologie : ", "", df$biologie)
df$ecg <- gsub("ECG : ", "", df$ecg)
df$radiologie <- gsub("Radiologie : ", "", df$radiologie)
df$imagerie <- gsub("Imagerie specialisee : ", "", df$imagerie)
df$autres <- gsub("Autres : ", "", df$autres)

# Remove any leading or trailing whitespace from each column
df$biologie <- trimws(df$biologie)
df$ecg <- trimws(df$ecg)
df$radiologie <- trimws(df$radiologie)
df$imagerie <- trimws(df$imagerie)
df$autres <- trimws(df$autres)

pattern1 <- "Biologie :\\s*(.*?)\\s*ECG"
result <- regmatches(urg$compte_rendu, regexec(pattern, urg$compte_rendu))

test <- str_extract_all(urg$compte_rendu,"(?<=Biologie :).+(?=ECG)")



####
prescriptions <- str_extract(urg$compte_rendu, "(?<=Liste des prescriptions\\s).*")
prescriptions <- str_extract(urg$compte_rendu, "(?<=Liste des prescriptions demandees :)(.|\\n)*?(?=ECG :)")
bio <- str_extract(prescriptions, "(?<=Biologie\\.\\s).*?(?=\\ECG)")



#Attribuer manuellement selon les informations le niveau de tri avec les différents libellés
#de la CIMU


#Regarder si niveau de tri attribué correspond à la complexité du patient (niveau de ressources)
#et si cela correspond à leur sévérité selon leur devenir/orientation (décès, réa/USC, 
#hospitalisation conventionnelle, RAD)





#Faire un test manuellement sur 100 patients tirés au hasard






#### Essai lecture fichier ####
# Définir la proportion de la première partie (50%)
prop <- 0.3

# Diviser le dataframe en deux parties
set.seed(123)
indices <- sample(1:nrow(urg), size = floor(prop * nrow(urg)), replace = FALSE)
partie_1 <- urg[indices, ]
partie_2 <- urg[-indices, ]

donnees_lecture <- createWorkbook()

#actes b
addWorksheet(donnees_lecture, "lecture")
# Écrire le dataframe dans la feuille de calcul
writeData(donnees_lecture, sheet = "lecture", x = as.data.frame(partie_1), colNames = TRUE)
saveWorkbook(donnees_lecture, "donnees_lecture.xlsx", overwrite = TRUE)





##### Brouillon ####
# Définir le motif pour extraire les dates
motif <- "(Senior :|Interne :)\\s*(.*?)\\s*(\\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2})"

# Extraire les dates en deux colonnes
urg <- urg %>%
  mutate(date_senior = str_extract_all(compte_rendu, motif_date) %>%
           sapply(function(x) x[1, 3]) %>%
           as.character(),
         date_interne = str_extract_all(compte_rendu, motif_date) %>%
           sapply(function(x) x[2, 3]) %>%
           as.character())

# Afficher le dataframe avec les dates extraites
df




# Définir le motif pour extraire les informations
motif <- "Senior : (.*?) (\\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2}).*?(.*?) (\\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2}).*?IAO"

# Extraire les informations en quatre colonnes
urg <- urg %>%
  mutate(nom1 = ifelse(str_detect(compte_rendu, motif), 
                       str_extract(compte_rendu, motif) %>%
                         str_extract("(?<=Senior : ).*?(?= \\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2})"),
                       NA),
         date1 = ifelse(str_detect(compte_rendu, motif), 
                        str_extract(compte_rendu, motif) %>%
                          str_extract("\\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2}"),
                        NA),
         nom2 = ifelse(str_detect(compte_rendu, motif), 
                       str_extract(compte_rendu, motif) %>%
                         str_extract("(?<=Interne : ).*?(?= \\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2})|(?<=MANAMANI Jafar \\(PH\\) ).*?(?= \\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2})"),
                       NA),
         date2 = ifelse(str_detect(compte_rendu, motif), 
                        str_extract(compte_rendu, motif) %>%
                          str_extract("(?<=Interne : \\S{6} ).*?(?= \\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2})|(?<=MANAMANI Jafar \\(PH\\) ).*?(?= \\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2})"),
                        NA))


# Extraire les deux formats de dates pour chaque ligne de texte
dates <- lapply(urg$compte_rendu, function(x) {
  date_senior <- str_extract(x, "(?<=Senior : ).*(?= Interne :)")
  date_interne <- str_extract_all(x, "(?<=Interne : ).*(?= IAO)")
  
  # Supprimer les caractères blancs
  date_senior <- str_trim(date_senior)
  date_interne <- str_trim(unlist(date_interne))
  
  # Extraire les dates dans chaque chaîne
  date_senior <- str_extract(date_senior, "\\d{2}/\\d{2}/\\d{4}\\s\\d{2}:\\d{2}")
  date_interne <- str_extract_all(date_interne, "\\d{2}/\\d{2}/\\d{4}\\s\\d{2}:\\d{2}")
  
  # Ajouter des NA s'il n'y a pas de date disponible
  if (length(date_senior) == 0) date_senior <- NA
  if (length(date_interne) == 0) date_interne <- list(NA)
  
  # Retourner un data frame avec les deux colonnes
  data.frame(date_senior, date_interne = unlist(date_interne))
})

# Combiner les résultats en un seul data frame
resultats <- bind_rows(dates)

#urg<- urg %>% mutate(hmed = if_else(str_detect(compte_rendu, "Interne") & str_detect(compte_rendu, "\\IAO"), 
#str_extract(compte_rendu, "(?<=Interne ).*(?=\\IAO)"), 
#NA_character_))

#Si on veut créer de nouvelles colonnes avec séparation
d %>% separate(nom, c("genre", "prenom", "nom"), sep = " ")
urg %>% extract(compte_rendu, "test", "\\d{2}/\\d{2}/\\d{4}\\s\\d{2}:\\d{2}", remove = FALSE)

motif_date_heure <- "(\\d{2}/\\d{2}/\\d{4}) (\\d{2}:\\d{2})"
date_heure <- str_extract(urg$compte_rendu, motif_date_heure)
date_heure <- as.data.frame(date_heure)
