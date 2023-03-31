#Test avec + petites données
library(readxl)
urg.test <- read_excel("donnees_lecture.xlsx")

#Suppression de tous les sauts de lignes et tabulations
urg.test$compte_rendu <- gsub("[\n\t]", "", urg.test$compte_rendu)

# Extraction du texte après "Liste des prescriptions demandees :"
texte_prescriptions <- str_extract(urg.test$compte_rendu, "(?<=Liste des prescriptions demandées :\\s).*")

#BIOLOGIE :
bio <- str_extract(texte_prescriptions, "Biologie\\s.*?(?=ECG :|$)") #CELUI-CI
urg.test$bio <- bio

#ECG :
# Création d'une nouvelle colonne "ECG" avec la valeur "non" par défaut
#urg.test$ECG <- "non"
#urg.test$EEG <- "non"
# Extraction du texte entre ECG : et Radiologie : ou la fin du texte s'il n'y a pas de Radiologie :
#texte_ecg <- str_extract(texte_prescriptions, "(?<=ECG :\\s).*?(?=Radiologie :|$)")

# Si texte_ecg n'est pas nul, alors ECG prend la valeur "oui"
#urg.test$ECG[!is.na(texte_ecg)] <- "oui"

#Sinon avec la colonne actes_e001_lib
summary(as.factor(urg.test$actes_e001_lib))
urg.test <- urg.test %>% 
  mutate(ECG = case_when(
    grepl("ECG", actes_e001_lib) ~ "ECG",
    TRUE ~ NA_character_
  ))

urg.test <- urg.test %>% 
  mutate(EEG = case_when(
    grepl("EEG", actes_e001_lib) ~ "EEG",
    TRUE ~ NA_character_
  ))


#RADIO :
#radio <- str_extract(texte_prescriptions, "(?<=Radiologie\\s).*?(?=Imagerie|$)")
#radio <- str_extract(texte_prescriptions, "Radiologie\\s.*?(?=Imagerie specialisee :|$)")
radio <- str_extract(texte_prescriptions, "Radiologie(?:(?!Imagerie sp[ée]cialis[ée]\\s)[\\s\\S])*?(?=Imagerie sp[ée]cialis[ée]|$)")
# Compter le nombre de fois que "Radiologie :" apparaît seul
count1 <- str_count(radio, "^Radiologie\\s+:$")

# Compter le nombre de fois que "Radiologie :" est suivi de plusieurs caractères
count2 <- str_count(radio, "^Radiologie\\s+:\\S")

sum(count1) #12412 qui n'ont pas eu de radio
sum(count2) #4712 qui ont eu au moins une radio

#Sinon plus simple avec uniquement les catégories
# Compter le nombre de cases vides ou NA
nb_na <- sum(is.na(urg.test$actes_r001_lib) | urg.test$actes_r001_lib == "")

# Compter le nombre de cases non-vides
nb_nonvides <- length(urg.test$actes_r001_lib) - nb_na

# Afficher les résultats
cat("Nombre de cases vides ou NA :", nb_na, "\n")
cat("Nombre de cases non-vides :", nb_nonvides)

#Je n'obtiens pas exactement les mêmes résultats...

urg.test$radio <- radio #à ne pas oublier

#IMAGERIE SPÉCIALISÉE
imagerie <- str_extract(texte_prescriptions, "Imagerie sp[eé]cialis[ée]e(?:(?!Autres\\s)[\\s\\S])*?(?=Autres|$)")
#imagerie <- str_extract(texte_prescriptions, "Imagerie\\s+sp[eé]cialis[ée]|[Ii]magerie\\s+specialis[ée](?:(?!Autres\\s)[\\s\\S])*?(?=Autres|$)")

count3 <- str_count(imagerie, "^Imagerie sp[eé]cialis[ée]e\\s+:$")

# Compter le nombre de fois que "Imagerie spécialisée :" est suivi de plusieurs caractères
count4 <- str_count(imagerie, "^Imagerie sp[eé]cialis[ée]e\\s+:\\S")

sum(count3)
sum(count4)

#On fait avec la colonne : actes_z001_res
nb_na <- sum(is.na(urg.test$actes_z001_lib) | urg.test$actes_z001_lib == "")
# Compter le nombre de cases non-vides
nb_nonvides <- length(urg.test$actes_z001_lib) - nb_na

# Afficher les résultats
cat("Nombre de cases vides ou NA :", nb_na, "\n")
cat("Nombre de cases non-vides :", nb_nonvides)

#Pareil, je n'obtiens pas exactement les mêmes chiffres

#AUTRES :
#autre <- str_extract(texte_prescriptions, "Autres :\\s.*?(?=M[ée]decin s[ée]nior ayant valid[ée] le dossier|$)")
autre <- str_extract(texte_prescriptions, "Autres(?:(?!IM[ée]decin s[ée]nior ayant valid[ée] le dossier\\s)[\\s\\S])*?(?=M[ée]decin s[ée]nior ayant valid[ée] le dossier|$)")
distinct(autre)

#### MOTIF DE TRIAGE IAO #### CODE ou libellé
motif_consult <- urg.test %>%
  distinct(motif1_lib)

motif_consult.code <- urg.test %>%
  distinct(motif1_res)


#### MOTIF DE CONSULTATION en texte libre ####
#consult <- str_extract(urg.test$compte_rendu, "(?<=MOTIF DE LA CONSULTATION RECUEILLI PAR L'IAO :\\s).*?(?=RISQUE INFECTIEUX :|$)")
#consult <- str_extract(urg.test$compte_rendu, paste0("(?s)MOTIF DE LA CONSULTATION RECUEILLI PAR L'IAO :\\s*(.*?)\\s*RISQUE INFECTIEUX"))
#consult <- str_extract_all(urg.test$compte_rendu,"(?<=MOTIF DE LA CONSULTATION RECUEILLI PAR L'IAO).+(?=RISQUE INFECTIEUX)")

consult <- str_extract(urg.test$compte_rendu, "MOTIF DE LA CONSULTATION RECUEILLI PAR L'IAO(?:(?!RISQUE INFECTIEUX\\s)[\\s\\S])*?(?=RISQUE INFECTIEUX|$)")
urg.test$consult <- consult

#### PARAMÈTRES VITAUX ####
#cf script Tri CIMU

#### TRAITEMENTS EN-COURS ####
ttt <- str_extract(urg.test$compte_rendu, "TRAITEMENTS EN-COURS(?:(?!HISTOIRE DE LA MALADIE\\s)[\\s\\S])*?(?=HISTOIRE DE LA MALADIE|$)")

#### ANTÉCÉDENTS ####
#atcdt <- str_extract(urg.test$compte_rendu, "(?<=ANTÉCÉDENTS\\s).*?(?=Allergie|$)")
atcdt <- str_extract(urg.test$compte_rendu, "ANT[ÉE]C[ÉE]DENTS(?:(?!Allergies\\s)[\\s\\S])*?(?=Allergies|$)")

#### ALLERGIE ####
allergie <- str_extract(urg.test$compte_rendu, "Allergies(?:(?!TRAITEMENTS\\s)[\\s\\S])*?(?=TRAITEMENTS|$)")


#### HISTOIRE DE LA MALADIE ####
hdm <- str_extract(urg.test$compte_rendu, "HISTOIRE DE LA MALADIE(?:(?!EXAMEN CLINIQUE INITIAL\\s)[\\s\\S])*?(?=EXAMEN CLINIQUE INITIAL|$)")
urg.test$hdm <- hdm

#### CONCLUSION ####
conclusion <- str_extract(urg.test$compte_rendu, "CONCLUSION(?:(?!D[ée]cision\\s)[\\s\\S])*?(?=D[ée]cision|$)")

#### SORTIE ####
summary(as.factor(urg$sortie))
#On a les correspondances des codes
#Codes des réanimation : 251, 255 ? (surv continue ?), 355, 362, 423, -> ensuite on a des établissements
#mais pas forcément les services...


library(nnet)
model <- multinom(urg.test$CIMU ~ urg.test$sex + urg.test$age + urg.test$glasgow_res 
                  + urg.test$pouls1_res, data = urg.test)



