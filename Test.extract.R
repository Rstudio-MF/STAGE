#Test avec + petites données
library(readxl)
urg.test <- read_excel("donnees_lecture.xlsx")

# Extraction du texte après "Liste des prescriptions demandees :"
texte_prescriptions <- str_extract(urg.test$compte_rendu, "(?<=Liste des prescriptions demandées :\\s).*")

#BIOLOGIE :
bio <- str_extract(texte_prescriptions, "Biologie :\\s.*?(?=ECG :|$)") #CELUI-CI
urg.test$bio <- bio
# supprimer "Biologie :\t"
urg.test$bio <- str_replace(urg.test$bio, "Biologie :\\s+", "")

#ECG :
# Création d'une nouvelle colonne "ECG" avec la valeur "non" par défaut
urg.test$ECG <- "non"
# Extraction du texte entre ECG : et Radiologie : ou la fin du texte s'il n'y a pas de Radiologie :
texte_ecg <- str_extract(texte_prescriptions, "(?<=ECG :\\s).*?(?=Radiologie :|$)")
# Si texte_ecg n'est pas nul, alors ECG prend la valeur "oui"
urg.test$ECG[!is.na(texte_ecg)] <- "oui"

#Sinon avec la colonne actes_e001_lib
summary(as.factor(urg.test$actes_e001_lib))
urg.test <- urg.test %>% 
  mutate(ECG1 = case_when(
    grepl("ECG", actes_e001_lib) ~ "ECG",
    TRUE ~ NA_character_
  ))

urg.test <- urg.test %>% 
  mutate(EEG = case_when(
    grepl("EEG", actes_e001_lib) ~ "EEG",
    TRUE ~ NA_character_
  ))



#RADIO : pas satisfaisant
radio <- str_extract(texte_prescriptions, "(?<=Radiologie\\s).*?(?=Imagerie|$)")
radio <- urg.test %>% 
  mutate(radio = case_when(str_detect(texte_prescriptions, ":\t$") ~ NA_character_,
                                      str_detect(texte_prescriptions, ":\t\\w") ~ str_replace(texte_prescriptions, ":\t", ""),
                                      TRUE ~ texte_prescriptions))

nb_na <- sum(is.na(radio))

# Compter le nombre de ":t" non suivis de caractères
nb_0 <- sum(grepl("^\\s*:\\t$", radio))
# Compter le nombre de ":t" suivis de caractères
nb_radio <- sum(str_count(radio, ":\t.*"))

# Afficher les résultats
cat("Nombre de NA : ", nb_na, "\n")
cat("Nombre de ':t' non suivis de caractères : ", nb_0, "\n")
cat("Nombre de ':t' suivis de caractères : ", nb_radio, "\n")
#Donc il y a 15 113 personnes qui n'ont pas eu de radios



#resultat <- str_extract(urg.test$compte_rendu, "(?s)(?<=Radiologie)(.*?)(?=Imagerie)")
#resultat1 <- str_extract(urg.test$compte_rendu, "(?s)(?<=Liste des prescriptions demandees :.*Radiologie)(.*?)(?=Imagerie)")
# Extraire le sous-ensemble de texte entre "Liste des prescriptions demandees :" et "Radiologie"
#texte_radiologie <- str_extract(urg.test$compte_rendu, "(?<=Liste des prescriptions demandees :).*?Imagerie.*")

# Extraire la partie de texte entre "Radiologie" et "Imagerie"
#resultat2 <- str_extract(texte_radiologie, "(?s)(?<=Radiologie).*?(?=Imagerie)")

#Sinon plus simple avec uniquement les catégories
# Compter le nombre de cases vides ou NA
nb_na <- sum(is.na(urg.test$actes_r001_lib) | urg.test$actes_r001_lib == "")

# Compter le nombre de cases non-vides
nb_nonvides <- length(urg.test$actes_r001_lib) - nb_na

# Afficher les résultats
cat("Nombre de cases vides ou NA :", nb_na, "\n")
cat("Nombre de cases non-vides :", nb_nonvides)

#Je n'obtiens pas les mêmes résultats...

urg.test$radio <- radio #à ne pas oublier

#IMAGERIE SPÉCIALISÉE : ne marche pas
#imagerie <- str_extract(texte_prescriptions, "(?<=Imagerie specialis[ée] :\\s).*?(?=Autres|$)")
imagerie <- str_extract(urg.test$compte_rendu, "(?<=Imagerie specialis[ée]\\s).*?(?=Autres|$)")
imagerie <- str_extract(texte_prescriptions, "(?i)(?<=Imagerie spécialis[éee]).*?(?=Autres|)")
imagerie <- str_extract(texte_prescriptions, "(?<=)Imagerie spécialis[ée]* :\\s+\\K.+?(?=\\s+Autres|$)")

imagerie <- str_extract(texte_prescriptions, "(?<=Imagerie spécialis[ée]*\\s*:\\s*(.*)\\s*Autres")
urg.test$imagerie <- imagerie

imagerie <- str_extract(texte_prescriptions, "(?<=Imagerie specialisée\\s)(.*?)(?=Autres :)")
imagerie <- str_extract(texte_prescriptions, "(?<=Imagerie specialisee :)[^:]*?(?=Autres :|$)")

#On fait avec la colonne : actes_z001_res
nb_na <- sum(is.na(urg.test$actes_z001_lib) | urg.test$actes_z001_lib == "")
# Compter le nombre de cases non-vides
nb_nonvides <- length(urg.test$actes_z001_lib) - nb_na

# Afficher les résultats
cat("Nombre de cases vides ou NA :", nb_na, "\n")
cat("Nombre de cases non-vides :", nb_nonvides)


#AUTRES :
autre <- str_extract(texte_prescriptions, "Autres :\\s.*?(?=Medecin senior ayant valide le dossier|$)")

#### MOTIF DE TRIAGE IAO #### CODE
motif_consult <- urg.test %>%
  distinct(motif1_lib)


#### MOTIF DE CONSULTATION ####
consult <- str_extract(urg.test$compte_rendu, "(?<=MOTIF DE LA CONSULTATION RECUEILLI PAR L'IAO :\\s).*?(?=RISQUE INFECTIEUX :|$)")
consult <- str_extract(urg.test$compte_rendu, "(?s)MOTIF DE LA CONSULTATION RECUEILLI PAR L'IAO :\\s*(.*?)\\s*RISQUE INFECTIEUX")

#### PARAMÈTRES VITAUX ####
#GCS : glasgow_res
#FC : pouls1_res
#TAS et TAD
#FR
#T°
#SpO2
#HGT

#### TRAITEMENTS EN-COURS ####
#### ANTÉCÉDENTS ####
atcdt <- str_extract(urg.test$compte_rendu, "(?<=ANTÉCÉDENTS\\s).*?(?=Allergie|$)")

#### ALLERGIE ####

#### HISTOIRE DE LA MALADIE ####


#### SORTIE ####
summary(as.factor(urg$sortie))
#On a les correspondances des codes
#Codes des réanimation : 251, 255 ? (surv continue ?), 355, 362, 423, -> ensuite on a des établissements
#mais pas forcément les services...


