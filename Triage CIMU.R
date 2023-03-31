#CIMU

#### QUESTIONS 
#Que fait-on motif1_res et/ou motif2_res : exemple TC sur malaise ??
#Douleur sternale pendant le malaise

#### CARDIOLOGIE ####
urg.test$FC180 <- urg.test$pouls1_res >= 180 #Tri 1
table(urg.test$FC180, urg.test$CIMU)
urg.test$FC120.140 <- urg.test$pouls1_res >=120 | urg.test$pouls1_res <= 140 #Tri 3
urg.test$FC141.180 <- urg.test$pouls1_res >=141 | urg.test$pouls1_res <= 180 #Tri 2
urg.test$FC50 <- urg.test$pouls1_res >=40 | urg.test$pouls1_res <=50 #Tri 3
urg.test$FC40 <- urg.test$pouls1_res <40 #Tri 2

urg.test$TA75 <- urg.test$tagmax1_res < 75 | urg.test$tamax1_res <75 #Tri1
table(urg.test$TA75, urg.test$CIMU)
urg.test$hypoTA <- urg.test$tagmax1_res >= 75 | urg.test$tamax1_res >=75 & 
  urg.test$tagmax1_res <= 90 | urg.test$tamax1_res <=90 #Tri2

urg.test$hyperTA <- urg.test$tagmax1_res >= 200 | urg.test$tamax1_res >=200 #Tri 3
urg.test$hyperTA.sympt #réfléchir à comment faire car TA > 220 + symptôme
urg.test$HTA <- urg.test$tagmax1_res >= 140 | urg.test$tamax1_res >=140 & 
  urg.test$tagmax1_res <= 200 | urg.test$tamax1_res <=200 #Tri5 (mais pas bien précisé juste HTA <200mmHg)
prop.table(table(urg.test$TA, urg.test$CIMU))
hyperTA <- subset(urg.test, motif1_res == "I10.0")
hyperTA.motif <- 
  
  #Arrêt cardio-respiratoire Tri 1
  ACR <- subset(urg.test, motif1_res == "I46.9")
arrêt <- urg.test[grep("(\\bACR\\b|\\barret[ -]?cardio[ -]?respiratoire\\b)", urg.test$consult, ignore.case = TRUE), ]

#Douleur thoracique ou irradiation (ECG+) Tri 2/1
#Douleur thoracique antérieure (ECG-) Tri 3
douleurtho <- subset(urg.test, motif1_res == "R07.4")

douleurtho.motif <- urg.test[grep("(\\bdouleur[ ]*thoracique\\b|\\bdouleur[ ]*tho\\b|\\bdlr[ ]*tho\\b)", urg.test$consult, ignore.case = TRUE), ]
#indice 13 : douleur thoracique déclenchée par le SMUR, tropo H4-

#Ischémie aiguë de membre Tri 3/2
ischemie.membre <- subset(urg.test, motif1_res == "I74.3")
#A REVOIR +++
ischemembre.motif <- urg.test[grep("isc[héèêë]mie.*?(membre|MI\\S*\\b|MS\\S*\\b)\\b", urg.test$consult, ignore.case = TRUE), ]

#Phlébite (adressée par MT) Tri 3
phlebite <- subset(urg.test, motif1_res == "I80.2")
phlebite.motif <- urg.test[grep("phl[ée]bite", urg.test$consult, ignore.case = TRUE), ]

#Malaise ou perte de connaissance Tri 3
malaise <- subset(urg.test, motif1_res == "R53.+1" | motif1_res == "R55")
malaise.motif <- urg.test[grep("malaise", urg.test$consult, ignore.case = TRUE), ]

#Oedème membre(s) inférieur(s) Tri 4/3 
#Palpitations Tri 4
palpitations <- subset(urg.test, motif1_res == "R00.2")
palpitations.motif <- urg.test[grep("palpitations", urg.test$consult, ignore.case = TRUE), ]


#### DERMATOLOGIE ####
#Affection cutanée fébrile, érysipèle (suspicion) Tri 4/3
#Erythème étendu Tri 4/3
#Affection vénérienne Tri 5/4
#Affection cutanée ou muqueuse limitée Tri 5

####ENDOCRINO - MÉTABOLISME ####
#Glycémie > 13.7 et cétonémie ≥ 0.5 Tri 3
#Il faut sélectionner ceux dont la glycémie est supérieure et au sein du compte-rendu, extraire la cétonémie
urg.test$hyperglyc <- urg.test$hemog1_res > 13.7

#Glycémie > 13.7 et cétonémie ≥ 1.5 Tri 2
#Glycémie > 13.7 et cétonurie ≥ 1 croix Tri 3
#Glycémie > 13.7 et cétonurie ≥ 2 croix Tri 2
#Glycémie > 20mmol/L chez patient avec antécédent de DID et DNID Tri 4
#Hypoglycémie et trouble de la vigilance Tri 4/3

#Anomalie métabolique (voir sénior) Tri 3/2 --> comment faire ?
#AEG et comorbidités Tri 4/3 -> prendre motif AEG et regarder si antécédents
#AEG sans comorbidités Tri 5

#### GASTRO-ENTÉROLOGIE ###
#Hématémèse / méléna / rectorragies Tri 3/2
#Occlusion intestinale / appendicite Tri 3
#Ictère Tri 3
#Ascite Tri 3
#Douleur abdominale Tri 4/3
#Corps étranger ingéré ou rectal Tri 4/3
#Constipation ou diarrhée Tri 5/4
#Nausées ou vomissements Tri 5/4
#Dysphagie ou hoquet Tri 5/4
#Proctologie (thrombose, fissure...) Tri 5/4

#### GYNÉCOLOGIE ####
#Hémorragie gynécologique Tri 3/2
#Contraction/accouchement Tri 3/2
#Douleur pelvienne Tri 4/3

#### HÉMATOLOGIE ####
#Leucopénie : <1000 PNN et fièvre Tri 2
#Hb ≤ 8g/dL Tri 3/2
#Plq ≤ 50000

#### MALADIES INFECTIEUSES ####
#Méningite (adressée par MT) Tri 2
#Hypothermie 32°C-35.5°C Tri 3
urg.test$hypothermie <- urg.test$temp1_res >=32 | urg.test$temp1_res <= 35.5
#Hypothermie < 32°C Tri 2
urg.test$hypothermiegrave <- urg.test$temp1_res < 32
table(urg.test$hypothermiegrave, urg.test$CIMU)

#Fièvre ≥40°C Tri 3
urg.test$fievre40 <- urg.test$temp1_res >= 40
#Fièvre >39°C si comorbidité ou voyage Tri 3
urg.test$fievre39 <- urg.test$temp1_res > 39
#ensuite chercher si comorbidités / voyage dans histoire de la maladie

#Risque de contamination VIH Tri 5/4
#Adénopathie(s) Tri 5/4
#Chercher adénopathie dans le motif de consultation

#### PNEUMOLOGIE ####
#Cyanose, signes de lutte, FR >40/min Tri 1

#Apnée ou FR ≤8/min Tri 1
#Hypoxémie avec SpO2 85% Tri 1
urg.test$hypoxemie <- urg.test$sat1_res < 85
#Hypoxémie avec SpO2 91-94% Tri 3
urg.test$hyppox91.94 <- urg.test$sat1_res >=91 | urg.test$sat1_res <= 94
#Hypoxémie avec SpO2 85-90% Tri 2
urg.test$hyppox85.90 <- urg.test$sat1_res >=85 | urg.test$sat1_res <= 90

#Dyspnée et PF > 50% Tri 2
#Dyspnée et PF ≥ 50% Tri 3/4
#Chercher dyspnée dans le motif et PF dans histoire de la maladie

#Dyspnée avec FR < 32/min Tri 3 
urg.test$dyspnee32 <- urg.test$fr1_res < 32
#Dyspnée avec 32<FR<40/min Tri 2
urg.test$dyspnee32.40 <- urg.test$fr1_res >=32 | urg.test$fr1_res <= 40

#Hémoptysie crachats Tri 3
#Hémoptysie caillots Tri 2
#Pneumopathie ou embolie (suspicion) Tri 3
#Douleur latéro-thoracique Tri 4/3
#Toux ± crachats 5/4

#### OPHTALMOLOGIE ####
#Trouble ou perte de la vision Tri 3/2
#Brûlure oculaire, corps étranger Tri 4/3
#Inflammation de l'oeil ou annexes Tri 5/4

#### ORL-STOMATOLOGIE ####
#Trouble ou perte de l'audition Tri 3/2
#Epistaxis ou gingivorragie Tri 5/4
#Vertige Tri 5/4
#Angine, odynophagie, tuméfaction ORL Tri 5/4
#Douleur dentaire Tri 5/4

#### RHUMATOLOGIE ####
#Sciatalgie et autres névralgies Tri 4/3
#Arthrite ou gonflement articulaire Tri 4/3
#Douleurs musculaires ou articulaire Tri 5/4
#Cervicalgie, dorsalgie ou lombalgie Tri 5/4
#Pathologie podologique chronique Tri 5

#### NEUROLOGIE ####
#Coma profond, GCS <8 Tri 1
urg.test$GCS8 <- urg.test$glasgow_res <8
#9< GCS < 12
urg.test$GCS9.12 <- urg.test$glasgow_res >= 9 | urg.test$glasgow_res <= 12

#Déficit sensitif ou moteur Tri 3/2
#Convulsions récentes / en cours Tri 3/2
#Confusion mentale, troubles de la vigilance avec GCS ≥ 12 Tri 3
#Céphalée brutale inhabituelle Tri 3
#Céphalée ≥24h ou sur terrain migraineux Tri 4
#Agitation violente ou aggressive Tri 4
#Trouble psychiatrique calme Tri 4
#Angoisse, tétanie Tri 5/4

#### TOXICOLOGIE ####
#Intoxication -> sénior ??
#Ébriété, ivresse (suspicion) Tri 5/4

#### TRAUMATOLOGIE ####
#Multi-traumatisme violent Tri 2/1
#Trauma violent face, cou, rachis, thorax, abdo Tri 3/2
#Trauma crânien et PC brève / confusion post TC Tri 3/2
#Trauma du membre sans / avec impotence Tri 5/4
#Trauma crânien sans PC Tri 5
#Plaie délabrante / amputation Tri 2/1
#Plaie cou, thorax, abdomen superficielle / profonde Tri 3/2
#Plaie superficielle / profonde Tri 4/3
#Plaie : érosion cutanée ou abrasion Tri 5
#Brûlure >10% visage ou main / profonde Tri 3/2
#Brûlure superficielle peu étendue / 5-10% Tri 5/4
#Collection abcédée ou hématique Tri 4
#Corps étranger sous cutané Tri 5/4
#Electrisation 5/4

#### URO-NÉPHROLOGIE ###
#Douleur aiguë des bourses Tri 3/2
#Rétention aiguë d'urine Tri 3
#Anurie Tri 3
#Douleur fosse lombaire ou flanc Tri 4/3
#Hématurie macroscopique Tri 4/3
#Traumatisme OGE, priapisme Tri 4/3
#Dysurie, brûlure mictionnelle, écoulement Tri 5/4

#### DIVERS ####
#Transfert SAMU Tri 2/1
#Problème technique de sonde, cathéter, plâtre Tri 4
#Autre problème (ex. renouvellement d'ordonnance) Tri 5
#Reconsultation pour réévaluation Tri 4/3

















