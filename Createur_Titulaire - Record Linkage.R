start.time <- Sys.time()

#charger librairies
library(RecordLinkage)
library(plyr)
library(dplyr)
library(readxl)
library(xlsx)
library(stringr)
library(data.table)
library(bigmemory)
options(bigmemory.typecast.warning=FALSE)


#Fonction de suppression de caractères spéciaux
caractere<-function(x)
{
  x<-gsub('à','a',x,ignore.case = TRUE)
  x<-gsub('â','a',x,ignore.case = TRUE)
  x<-gsub('ä','a',x,ignore.case = TRUE)
  x<-gsub('é','e',x,ignore.case = TRUE)
  x<-gsub('è','e',x,ignore.case = TRUE)
  x<-gsub('ê','e',x,ignore.case = TRUE)
  x<-gsub('ë','e',x,ignore.case = TRUE)
  x<-gsub('î','i',x,ignore.case = TRUE)
  x<-gsub('ï','i',x,ignore.case = TRUE)
  x<-gsub('ô','o',x,ignore.case = TRUE)
  x<-gsub('ö','o',x,ignore.case = TRUE)
  x<-gsub('ù','u',x,ignore.case = TRUE)
  x<-gsub('û','u',x,ignore.case = TRUE)
  x<-gsub('ü','u',x,ignore.case = TRUE)
  x<-gsub('ç','c',x,ignore.case = TRUE)
  x<-gsub('\\^','',x,ignore.case = TRUE)
  x<-gsub('/','',x)
  x<-gsub('-',' ',x)
  x<-gsub("[[:digit:]]","",x) #supprime les chiffres
  x<-gsub("^\\s+|\\s+$", "", x) #supprime les espaces en début ou fin de mot  
}

formattelephone<-function(x)
{
  x<-gsub(' ','',x)
  x<-gsub('O','0',x)
  x<-gsub('\\+','',x)
  x<-gsub("^33",'0',x)
  x<-gsub('\\-','',x)
  x<-str_pad(x, width=10, side="left", pad="0")
  x<-sapply(x, function(x) {ifelse(grepl("00000000" ,x), NA, x)})
  x<-sapply(x, function(x) {ifelse(grepl("12345"    ,x), NA, x)})
  x<-sapply(x, function(x) {ifelse(grepl("010101"   ,x), NA, x)})
  x<-sapply(x, function(x) {ifelse(grepl("999999"   ,x), NA, x)})
  x<-sapply(x, function(x) {ifelse(grepl("111111"   ,x), NA, x)})
}


#############################################
##############Importer données###############
#############################################
regions<-read_excel("reg2016.xls",sheet="Feuille1")
dpt<-read_excel("depts2016.xls",sheet="Feuille1")
createur<-as.data.table(read_excel("jecreemonentreprise.xls",sheet = "Dossiers"))
titulaires<-fread("Tit_Extract.csv")

#############################################
##############Préparer données###############
#############################################

##############régions##################
regions<-regions[,c("REGION,C,2","NCC,C,70")]
colnames(regions)<-c("numReg","nomReg")
dpt<-dpt[,c("DEP,C,3","NCC,C,70","REGION,C,2")]
colnames(dpt)<-c("numDpt","nomDpt","numReg")
dpt<-join(dpt,regions,type="left",by="numReg")
dpt$nomReg<-gsub("(^|[[:space:]]|\\-)([[:alpha:]])", "\\1\\U\\2", tolower(dpt$nomReg), perl=TRUE)
dpt$nomReg <- gsub("\\-", "\n", dpt$nomReg)

#############createur###############
colnames(createur)<-c("Date","Activité","Email_Crea","Nom_Crea","Prenom_Crea","CodePostal_Crea","TelephoneF_Crea","Siret","Année création","Mois création","Pas créé","Contact","Réponses","Exporté le")
createur$TelephoneF_Crea<-as.integer(createur$TelephoneF_Crea)
createur$TelephoneF_Crea<-formattelephone(createur$TelephoneF_Crea)
createur<-cbind(createur$Nom_Crea,createur$Prenom_Crea,createur$Email_Crea,createur$TelephoneF_Crea,createur$TelephoneF_Crea,createur$CodePostal_Crea,createur$Nom_Concat,createur$numReg)
createur<-data.frame(createur,stringsAsFactors=FALSE)
colnames(createur)<-c("Nom_Crea","Prenom_Crea","Email_Crea","TelephoneF_Crea","TelephoneP_Crea","CodePostal_Crea")
createur$Nom_Crea<-caractere(createur$Nom_Crea)
createur$Nom_Crea<-toupper(createur$Nom_Crea)
createur$Prenom_Crea<-caractere(createur$Prenom_Crea)
createur$Prenom_Crea<-toupper(createur$Prenom_Crea)
createur$CodePostal_Crea<-as.vector(createur$CodePostal_Crea)
createur$CodePostal_Crea<-str_pad(createur$CodePostal_Crea, width=5, side="left", pad="0")
createur$CodePostal_Crea[createur$CodePostal_Crea=="00000"] <- NA
createur$CodePostal_Crea<-gsub('O','0',createur$CodePostal_Crea)
createur$CodePostal_Crea<-gsub("[[:alpha:]]",NA,createur$CodePostal_Crea)
createur<-createur[!grepl('TEST',createur$Nom_Crea),] #supprimer nom "test"
createur<-createur[!grepl('TEST',createur$Prenom_Crea),] #supprimer prénnom "test"
createur<-subset(createur, nchar(as.character(Prenom_Crea)) > 1) #supprimer prénoms d'une seule lettre
createur<-subset(createur, nchar(as.character(Nom_Crea)) > 1) #supprimer noms d'une seule lettre
createur$Email_Crea<-tolower(createur$Email_Crea)
createur$Email_Crea<-gsub(' ','',createur$Email_Crea)
createur<-unique(createur)
createur$numDpt<-ifelse(grepl("^97",createur$CodePostal_Crea), substr(createur$CodePostal_Crea,1,3), substr(createur$CodePostal_Crea,1,2))
createur<-join(createur,dpt,by="numDpt")
regcrea<-table(createur$nomReg)
createur$Region_Crea<-factor(createur$numReg)
createur<-within(createur,rm("numDpt","nomDpt","nomReg","numReg"))


#############titulaires###############
colnames(titulaires)<-c("Nom_Part","Prenom_Part","Email_Part","TelephoneF_Part","TelephoneP_Part","CodePostal_Part")
titulaires$Prenom_Part<-gsub("[[:digit:]]",NA,titulaires$Prenom_Part) #supprimer les chiffres dans les prénoms
titulaires<-subset(titulaires, nchar(as.character(Prenom_Part)) > 1) #supprimer prénoms d'une seule lettre
titulaires<-subset(titulaires, nchar(as.character(Nom_Part)) > 1) #supprimer noms d'une seule lettre
titulaires<-unique(titulaires) #ne pas garder de doublons
titulaires<-titulaires[!is.na(Nom_Part)]
titulaires<-titulaires[titulaires$Nom_Part!=' ']
titulaires<-titulaires[!is.na(Prenom_Part)]
titulaires<-titulaires[titulaires$Prenom_Part!=' ']
titulaires$Nom_Part<-caractere(titulaires$Nom_Part)
titulaires$Nom_Part<-toupper(titulaires$Nom_Part)
titulaires$Prenom_Part<-caractere(titulaires$Prenom_Part)
titulaires$Prenom_Part<-toupper(titulaires$Prenom_Part)
titulaires$Email_Part[titulaires$Email_Part==""]<-NA
titulaires$Email_Part<-tolower(titulaires$Email_Part)
titulaires$Email_Part<-gsub(' ','',titulaires$Email_Part)
titulaires$Email_Part[grepl("pasdemail",titulaires$Email_Part)]<-NA
titulaires$Email_Part[grepl("adresse",titulaires$Email_Part)]<-NA
titulaires$Email_Part[grepl("pasde@mail.fr",titulaires$Email_Part)]<-NA
titulaires$Email_Part[grepl("xxxx",titulaires$Email_Part)]<-NA
titulaires$CodePostal_Part<-as.integer(titulaires$CodePostal_Part)
titulaires$CodePostal_Part<-str_pad(titulaires$CodePostal_Part, width=5, side="left", pad="0")
titulaires$CodePostal_Part[titulaires$CodePostal_Part=="00000"] <- NA
titulaires$TelephoneP_Part<-as.integer(titulaires$TelephoneP_Part)
titulaires$TelephoneF_Part<-as.integer(titulaires$TelephoneF_Part)
titulaires$TelephoneF_Part<-formattelephone(titulaires$TelephoneF_Part)
titulaires$TelephoneP_Part<-formattelephone(titulaires$TelephoneP_Part)
titulaires<-titulaires[!is.na(titulaires$TelephoneF_Part) | !is.na(titulaires$TelephoneP_Part) | !is.na(titulaires$Email_Part),] #ne garder que les lignes qui ont au moins un numéro de téléphone ou une adresse mail
titulaires$numDpt<-ifelse(grepl("^97",titulaires$CodePostal_Part), substr(titulaires$CodePostal_Part,1,3), substr(titulaires$CodePostal_Part,1,2))
titulaires<-join(titulaires,dpt,by="numDpt")
regtit<-table(titulaires$nomReg)
titulaires<-within(titulaires,rm("numDpt","nomDpt","nomReg"))
titulaires$numReg<-factor(titulaires$numReg)


#Distribution par région
regcrea<-regcrea/sum(regcrea)*100
regtit<-regtit/sum(regtit)*100
distriregion<-rbind(regtit,regcrea)
rownames(distriregion)<-c("Titulaires","Créateurs")
par(mar=c(6.5, 4, 2, 0))
barplot(distriregion,beside=TRUE,main="Distribution des titulaires \net créateurs par région",xlab="",ylab = "Pourcentage d'individus",las=2,cex.names=0.8,col=c("darkslategrey","darkseagreen"),ylim=c(0,20),legend.text=TRUE)


#créer les tables qui recevront les résultats
comparaisons_possibles<-as.numeric(nrow(titulaires))*as.numeric(nrow(createur))
links<-NULL
possible_links<-NULL
poids<-big.matrix(nrow=comparaisons_possibles/10,ncol=1,type="short")

#############################################
################Comparaisons#################
#############################################

titulaires <- titulaires[sample(nrow(titulaires)),]#on rend l'ordre de titulaire aléatoire pour éviter les biais dans le débitage
taille<-nrow(titulaires)/100 #débitage du fichier en 100 parts égales pour pouvoir effectuer les calculs avec la mémoire disponible

titulaires0    <- data.frame(titulaires[ 1:taille,]	)					


#supprimer les tables qui ne servent plus par la suite
rm(dpt,regions,regcrea,regtit,distriregion,caractere,formattelephone)
gc()

import_et_preparation<-Sys.time()

#Ensuite, on cherche les valeurs similaires (sur la première tranche)
createur_titulaire<-compare.linkage (createur, titulaires0, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7) #On crée un objet de comparaison
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)  #On calcule les poids pour chaque paire
summary(createur_titulaire) #L'histogramme aide au choix des seuils

createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16) #classification en "link", "non-link" ou "possible-link"
links<-getPairs(createur_titulaire,single.rows = TRUE, show="links") #on extrait les "link"
possible_links<-getPairs(createur_titulaire,single.rows = TRUE, show="possible") #on extrait les "possible link"
poids[1:length(createur_titulaire$Wdata)]<-createur_titulaire$Wdata
paires<-as.numeric(nrow(createur_titulaire$pairs))
rm(titulaires0)
gc()

# Puis on continue avec les tranches suivantes
titulaires1    <- data.frame(titulaires[(taille    +1):(taille*  2),])						
createur_titulaire<-compare.linkage (createur, titulaires1	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires1,createur_titulaire))

titulaires2	   <- titulaires[(taille* 	2 	+1):(taille*	3	),]
createur_titulaire<-compare.linkage (createur, titulaires2	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires2,createur_titulaire))

titulaires3	   <- titulaires[(taille* 	3	  +1):(taille*	4	),]
createur_titulaire<-compare.linkage (createur, titulaires3	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires3,createur_titulaire))

titulaires4	   <- titulaires[(taille* 	4	  +1):(taille*	5	),]
createur_titulaire<-compare.linkage (createur, titulaires4	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires4,createur_titulaire))

titulaires5	   <- titulaires[(taille* 	5	  +1):(taille*	6	),]
createur_titulaire<-compare.linkage (createur, titulaires5	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires5,createur_titulaire))

titulaires6	   <- titulaires[(taille* 	6	  +1):(taille*	7	),]
createur_titulaire<-compare.linkage (createur, titulaires6	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires6,createur_titulaire))

titulaires7	   <- titulaires[(taille* 	7	  +1):(taille*	8	),]
createur_titulaire<-compare.linkage (createur, titulaires7	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires7,createur_titulaire))

titulaires8	   <- titulaires[(taille* 	8	  +1):(taille*	9	),]
createur_titulaire<-compare.linkage (createur, titulaires8	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires8,createur_titulaire))

titulaires9	   <- titulaires[(taille* 	9	  +1):(taille*	10),]
createur_titulaire<-compare.linkage (createur, titulaires9	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires9,createur_titulaire))

titulaires10	 <- titulaires[(taille* 	10	+1):(taille*	11),]
createur_titulaire<-compare.linkage (createur, titulaires10	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires10,createur_titulaire))

titulaires11	 <- titulaires[(taille* 	11	+1):(taille*	12),]
createur_titulaire<-compare.linkage (createur, titulaires11	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires11,createur_titulaire))

titulaires12	 <- titulaires[(taille* 	12	+1):(taille*	13),]
createur_titulaire<-compare.linkage (createur, titulaires12	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires12,createur_titulaire))

titulaires13	 <- titulaires[(taille* 	13	+1):(taille*	14),]
createur_titulaire<-compare.linkage (createur, titulaires13	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires13,createur_titulaire))

titulaires14	 <- titulaires[(taille* 	14	+1):(taille*	15),]
createur_titulaire<-compare.linkage (createur, titulaires14	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires14,createur_titulaire))

titulaires15	 <- titulaires[(taille* 	15	+1):(taille*	16),]
createur_titulaire<-compare.linkage (createur, titulaires15	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires15,createur_titulaire))

titulaires16	 <- titulaires[(taille* 	16	+1):(taille*	17),]
createur_titulaire<-compare.linkage (createur, titulaires16	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires16,createur_titulaire))

titulaires17	 <- titulaires[(taille* 	17	+1):(taille*	18),]
createur_titulaire<-compare.linkage (createur, titulaires17	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires17,createur_titulaire))

titulaires18	 <- titulaires[(taille* 	18	+1):(taille*	19),]
createur_titulaire<-compare.linkage (createur, titulaires18	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires18,createur_titulaire))

titulaires19	 <- titulaires[(taille* 	19	+1):(taille*	20),]
createur_titulaire<-compare.linkage (createur, titulaires19	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires19,createur_titulaire))

titulaires20	 <- titulaires[(taille* 	20	+1):(taille*	21),]
createur_titulaire<-compare.linkage (createur, titulaires20	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires20,createur_titulaire))

titulaires21	 <- titulaires[(taille* 	21	+1):(taille*	22),]
createur_titulaire<-compare.linkage (createur, titulaires21	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires21,createur_titulaire))

titulaires22	 <- titulaires[(taille* 	22	+1):(taille*	23),]
createur_titulaire<-compare.linkage (createur, titulaires22	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires22,createur_titulaire))

titulaires23	 <- titulaires[(taille* 	23	+1):(taille*	24),]
createur_titulaire<-compare.linkage (createur, titulaires23	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires23,createur_titulaire))

titulaires24	 <- titulaires[(taille* 	24	+1):(taille*	25),]
createur_titulaire<-compare.linkage (createur, titulaires24	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires24,createur_titulaire))

titulaires25	 <- titulaires[(taille* 	25	+1):(taille*	26),]
createur_titulaire<-compare.linkage (createur, titulaires25	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires25,createur_titulaire))

titulaires26	 <- titulaires[(taille* 	26	+1):(taille*	27),]
createur_titulaire<-compare.linkage (createur, titulaires26	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires26,createur_titulaire))

titulaires27	 <- titulaires[(taille* 	27	+1):(taille*	28),]
createur_titulaire<-compare.linkage (createur, titulaires27	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires27,createur_titulaire))

titulaires28	 <- titulaires[(taille* 	28	+1):(taille*	29),]
createur_titulaire<-compare.linkage (createur, titulaires28	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires28,createur_titulaire))

titulaires29	 <- titulaires[(taille* 	29	+1):(taille*	30),]
createur_titulaire<-compare.linkage (createur, titulaires29	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires29,createur_titulaire))

titulaires30	 <- titulaires[(taille* 	30	+1):(taille*	31),]
createur_titulaire<-compare.linkage (createur, titulaires30	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires30,createur_titulaire))

titulaires31	 <- titulaires[(taille* 	31	+1):(taille*	32),]
createur_titulaire<-compare.linkage (createur, titulaires31	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires31,createur_titulaire))

titulaires32	 <- titulaires[(taille* 	32	+1):(taille*	33),]
createur_titulaire<-compare.linkage (createur, titulaires32	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires32,createur_titulaire))

titulaires33	 <- titulaires[(taille* 	33	+1):(taille*	34),]
createur_titulaire<-compare.linkage (createur, titulaires33	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires33,createur_titulaire))

titulaires34	 <- titulaires[(taille* 	34	+1):(taille*	35),]
createur_titulaire<-compare.linkage (createur, titulaires34	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires34,createur_titulaire))

titulaires35	 <- titulaires[(taille* 	35	+1):(taille*	36),]
createur_titulaire<-compare.linkage (createur, titulaires35	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires35,createur_titulaire))

titulaires36	 <- titulaires[(taille* 	36	+1):(taille*	37),]
createur_titulaire<-compare.linkage (createur, titulaires36	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires36,createur_titulaire))

titulaires37	 <- titulaires[(taille* 	37	+1):(taille*	38),]
createur_titulaire<-compare.linkage (createur, titulaires37	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires37,createur_titulaire))

titulaires38	 <- titulaires[(taille* 	38	+1):(taille*	39),]
createur_titulaire<-compare.linkage (createur, titulaires38	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires38,createur_titulaire))

titulaires39	 <- titulaires[(taille* 	39	+1):(taille*	40),]
createur_titulaire<-compare.linkage (createur, titulaires39	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires39,createur_titulaire))

titulaires40	 <- titulaires[(taille* 	40	+1):(taille*	41),]
createur_titulaire<-compare.linkage (createur, titulaires40	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires40,createur_titulaire))

titulaires41	 <- titulaires[(taille* 	41	+1):(taille*	42),]
createur_titulaire<-compare.linkage (createur, titulaires41	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires41,createur_titulaire))

titulaires42	 <- titulaires[(taille* 	42	+1):(taille*	43),]
createur_titulaire<-compare.linkage (createur, titulaires42	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires42,createur_titulaire))

titulaires43	 <- titulaires[(taille* 	43	+1):(taille*	44),]
createur_titulaire<-compare.linkage (createur, titulaires43	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires43,createur_titulaire))

titulaires44	 <- titulaires[(taille* 	44	+1):(taille*	45),]
createur_titulaire<-compare.linkage (createur, titulaires44	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires44,createur_titulaire))

titulaires45	 <- titulaires[(taille* 	45	+1):(taille*	46),]
createur_titulaire<-compare.linkage (createur, titulaires45	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires45,createur_titulaire))

titulaires46	 <- titulaires[(taille* 	46	+1):(taille*	47),]
createur_titulaire<-compare.linkage (createur, titulaires46	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires46,createur_titulaire))

titulaires47	 <- titulaires[(taille* 	47	+1):(taille*	48),]
createur_titulaire<-compare.linkage (createur, titulaires47	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires47,createur_titulaire))

titulaires48	 <- titulaires[(taille* 	48	+1):(taille*	49),]
createur_titulaire<-compare.linkage (createur, titulaires48	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires48,createur_titulaire))

titulaires49	 <- titulaires[(taille* 	49	+1):(taille*	50),]
createur_titulaire<-compare.linkage (createur, titulaires49	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires49,createur_titulaire))

titulaires50	 <- titulaires[(taille* 	50	+1):(taille*	51),]
createur_titulaire<-compare.linkage (createur, titulaires50	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires50,createur_titulaire))

titulaires51	 <- titulaires[(taille* 	51	+1):(taille*	52),]
createur_titulaire<-compare.linkage (createur, titulaires51	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires51,createur_titulaire))

titulaires52	 <- titulaires[(taille* 	52	+1):(taille*	53),]
createur_titulaire<-compare.linkage (createur, titulaires52	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires52,createur_titulaire))

titulaires53	 <- titulaires[(taille* 	53	+1):(taille*	54),]
createur_titulaire<-compare.linkage (createur, titulaires53	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires53,createur_titulaire))

titulaires54	 <- titulaires[(taille* 	54	+1):(taille*	55),]
createur_titulaire<-compare.linkage (createur, titulaires54	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires54,createur_titulaire))

titulaires55	 <- titulaires[(taille* 	55	+1):(taille*	56),]
createur_titulaire<-compare.linkage (createur, titulaires55	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires55,createur_titulaire))

titulaires56	 <- titulaires[(taille* 	56	+1):(taille*	57),]
createur_titulaire<-compare.linkage (createur, titulaires56	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires56,createur_titulaire))

titulaires57	 <- titulaires[(taille* 	57	+1):(taille*	58),]
createur_titulaire<-compare.linkage (createur, titulaires57	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires57,createur_titulaire))

titulaires58	 <- titulaires[(taille* 	58	+1):(taille*	59),]
createur_titulaire<-compare.linkage (createur, titulaires58	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires58,createur_titulaire))

titulaires59	 <- titulaires[(taille* 	59	+1):(taille*	60),]
createur_titulaire<-compare.linkage (createur, titulaires59	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires59,createur_titulaire))

titulaires60	 <- titulaires[(taille* 	60	+1):(taille*	61),]
createur_titulaire<-compare.linkage (createur, titulaires60	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires60,createur_titulaire))

titulaires61	 <- titulaires[(taille* 	61	+1):(taille*	62),]
createur_titulaire<-compare.linkage (createur, titulaires61	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires61,createur_titulaire))

titulaires62	 <- titulaires[(taille* 	62	+1):(taille*	63),]
createur_titulaire<-compare.linkage (createur, titulaires62	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires62,createur_titulaire))

titulaires63	 <- titulaires[(taille* 	63	+1):(taille*	64),]
createur_titulaire<-compare.linkage (createur, titulaires63	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires63,createur_titulaire))

titulaires64	 <- titulaires[(taille* 	64	+1):(taille*	65),]
createur_titulaire<-compare.linkage (createur, titulaires64	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires64,createur_titulaire))

titulaires65	 <- titulaires[(taille* 	65	+1):(taille*	66),]
createur_titulaire<-compare.linkage (createur, titulaires65	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires65,createur_titulaire))

titulaires66	 <- titulaires[(taille* 	66	+1):(taille*	67),]
createur_titulaire<-compare.linkage (createur, titulaires66	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires66,createur_titulaire))

titulaires67	 <- titulaires[(taille* 	67	+1):(taille*	68),]
createur_titulaire<-compare.linkage (createur, titulaires67	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires67,createur_titulaire))

titulaires68	 <- titulaires[(taille* 	68	+1):(taille*	69),]
createur_titulaire<-compare.linkage (createur, titulaires68	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires68,createur_titulaire))

titulaires69	 <- titulaires[(taille* 	69	+1):(taille*	70),]
createur_titulaire<-compare.linkage (createur, titulaires69	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires69,createur_titulaire))

titulaires70	 <- titulaires[(taille* 	70	+1):(taille*	71),]
createur_titulaire<-compare.linkage (createur, titulaires70	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires70,createur_titulaire))

titulaires71	 <- titulaires[(taille* 	71	+1):(taille*	72),]
createur_titulaire<-compare.linkage (createur, titulaires71	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires71,createur_titulaire))

titulaires72	 <- titulaires[(taille* 	72	+1):(taille*	73),]
createur_titulaire<-compare.linkage (createur, titulaires72	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires72,createur_titulaire))

titulaires73	 <- titulaires[(taille* 	73	+1):(taille*	74),]
createur_titulaire<-compare.linkage (createur, titulaires73	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires73,createur_titulaire))

titulaires74	 <- titulaires[(taille* 	74	+1):(taille*	75),]
createur_titulaire<-compare.linkage (createur, titulaires74	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires74,createur_titulaire))

titulaires75	 <- titulaires[(taille* 	75	+1):(taille*	76),]
createur_titulaire<-compare.linkage (createur, titulaires75	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires75,createur_titulaire))

titulaires76	 <- titulaires[(taille* 	76	+1):(taille*	77),]
createur_titulaire<-compare.linkage (createur, titulaires76	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires76,createur_titulaire))

titulaires77	 <- titulaires[(taille* 	77	+1):(taille*	78),]
createur_titulaire<-compare.linkage (createur, titulaires77	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires77,createur_titulaire))

titulaires78	 <- titulaires[(taille* 	78	+1):(taille*	79),]
createur_titulaire<-compare.linkage (createur, titulaires78	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires78,createur_titulaire))

titulaires79	 <- titulaires[(taille* 	79	+1):(taille*	80),]
createur_titulaire<-compare.linkage (createur, titulaires79	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires79,createur_titulaire))

titulaires80	 <- titulaires[(taille* 	80	+1):(taille*	81),]
createur_titulaire<-compare.linkage (createur, titulaires80	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires80,createur_titulaire))

titulaires81	 <- titulaires[(taille* 	81	+1):(taille*	82),]
createur_titulaire<-compare.linkage (createur, titulaires81	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires81,createur_titulaire))

titulaires82	 <- titulaires[(taille* 	82	+1):(taille*	83),]
createur_titulaire<-compare.linkage (createur, titulaires82	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires82,createur_titulaire))

titulaires83	 <- titulaires[(taille* 	83	+1):(taille*	84),]
createur_titulaire<-compare.linkage (createur, titulaires83	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires83,createur_titulaire))

titulaires84	 <- titulaires[(taille* 	84	+1):(taille*	85),]
createur_titulaire<-compare.linkage (createur, titulaires84	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires84,createur_titulaire))

titulaires85	 <- titulaires[(taille* 	85	+1):(taille*	86),]
createur_titulaire<-compare.linkage (createur, titulaires85	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires85,createur_titulaire))

titulaires86	 <- titulaires[(taille* 	86	+1):(taille*	87),]
createur_titulaire<-compare.linkage (createur, titulaires86	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires86,createur_titulaire))

titulaires87	 <- titulaires[(taille* 	87	+1):(taille*	88),]
createur_titulaire<-compare.linkage (createur, titulaires87	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires87,createur_titulaire))

titulaires88	 <- titulaires[(taille* 	88	+1):(taille*	89),]
createur_titulaire<-compare.linkage (createur, titulaires88	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires88,createur_titulaire))

titulaires89	 <- titulaires[(taille* 	89	+1):(taille*	90),]
createur_titulaire<-compare.linkage (createur, titulaires89	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires89,createur_titulaire))

titulaires90	 <- titulaires[(taille* 	90	+1):(taille*	91),]
createur_titulaire<-compare.linkage (createur, titulaires90	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires90,createur_titulaire))

titulaires91	 <- titulaires[(taille* 	91	+1):(taille*	92),]
createur_titulaire<-compare.linkage (createur, titulaires91	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires91,createur_titulaire))

titulaires92	 <- titulaires[(taille* 	92	+1):(taille*	93),]
createur_titulaire<-compare.linkage (createur, titulaires92	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires92,createur_titulaire))

titulaires93	 <- titulaires[(taille* 	93	+1):(taille*	94),]
createur_titulaire<-compare.linkage (createur, titulaires93	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires93,createur_titulaire))

titulaires94	 <- titulaires[(taille* 	94	+1):(taille*	95),]
createur_titulaire<-compare.linkage (createur, titulaires94	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires94,createur_titulaire))

titulaires95	 <- titulaires[(taille* 	95	+1):(taille*	96),]
createur_titulaire<-compare.linkage (createur, titulaires95	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires95,createur_titulaire))

titulaires96	 <- titulaires[(taille* 	96	+1):(taille*	97),]
createur_titulaire<-compare.linkage (createur, titulaires96	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires96,createur_titulaire))

titulaires97	 <- titulaires[(taille* 	97	+1):(taille*	98),]
createur_titulaire<-compare.linkage (createur, titulaires97	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires97,createur_titulaire))

titulaires98	 <- titulaires[(taille* 	98	+1):(taille*	99),]
createur_titulaire<-compare.linkage (createur, titulaires98	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires98,createur_titulaire))

titulaires99	 <- titulaires[(taille* 	99	+1):(taille*	100),]
createur_titulaire<-compare.linkage (createur, titulaires99	, blockfld = 7,strcmp = c(1,2,3,4,5,6),strcmpfun = jarowinkler, exclude=7)
createur_titulaire<-fsWeights(createur_titulaire,m=c(0.9,0.8,0.7,0.7,0.5,0.6),u=createur_titulaire$frequencies)
createur_titulaire<-fsClassify(createur_titulaire,threshold.upper=20,threshold.lower=16)
links<-rbind(links,getPairs(createur_titulaire,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(createur_titulaire,single.rows = TRUE, show="possible"))		
poids[(paires+1):(paires+length(createur_titulaire$Wdata))]<-createur_titulaire$Wdata
paires<-as.numeric(paires+nrow(createur_titulaire$pairs))
gc(rm(titulaires99,createur_titulaire))

#############################################
#################Résultats###################
#############################################

poids<-sub.big.matrix(poids,firstRow = 1,lastRow = paires)
poids<-poids[mwhich(x=poids, cols = 1, vals =0,comps='ge' ,op="AND")]
#Observation de la distribution des poids de comparaison (pour amélioration future)
library(Hmisc)
jpeg(filename="Distribution des poids de comparaison Créateur_Titulaire.jpeg")
par(mar=c(3, 3, 1, 1),mgp=c(1.5,0.5,0))
hist(poids,main = "Distribution des poids de comparaison",col.main="dodgerblue4", font.lab=2,col.lab="dodgerblue4",ylab="Effectifs", xlab = "Poids", labels=TRUE, col="#9BBB59", ylim=c(0,4000), xlim=c(0,65))
Hmisc::minor.tick(nx=5,ny=0)
dev.off()


#Une fois fini, on observe et enregistre les liens et les possibles liens (les non liens ne sont pas nécessaires)
links<-arrange(links, desc(Weight), desc(Nom_Crea.1), desc(Prenom_Crea.1))
View(links)
possible_links<-arrange(possible_links, desc(Weight), desc(Nom_Crea.1), desc(Prenom_Crea.1))
View(possible_links)
write.xlsx2(links,"liens_créateurs.xlsx",sheetName="liens sûrs",showNA=FALSE)
write.xlsx2(possible_links,"liens_créateurs.xlsx",sheetName="possibles liens",showNA=FALSE,append=TRUE)
write.xlsx2(poids[,],"liens_créateurs.xlsx",sheetName="poids",showNA=FALSE,append=TRUE)


#Calcul du temps d'exécution de l'algorithme
end.time <- Sys.time()
time.taken <- end.time - start.time
comparaison.time<-end.time - import_et_preparation
prep<-import_et_preparation - start.time


#Message de fin, résumé des résultats
bloquees<-comparaisons_possibles-paires
cat("Temps d'exécution total: ",time.taken,units(time.taken),"\ndont",prep,units(prep),"pour l'import et la préparation de données,\net",comparaison.time,units(comparaison.time),"pour les",paires,"comparaisons effectuées.",
    "\n\nSur",comparaisons_possibles,"comparaisons possibles,",bloquees,"ont été bloquées (soit",bloquees/comparaisons_possibles*100,"%) en raison de région différente,\net ",paires,"ont été effectuées (soit,",paires/comparaisons_possibles*100,"%).",
    "\n\nSur ces",paires,"paires comparées,",paires-(nrow(possible_links)+nrow(links)),"ont été qualifiées comme non-liens (soit",(paires-(nrow(possible_links)+nrow(links)))/paires*100,"%),\n",nrow(possible_links),"comme possibles liens (soit",nrow(possible_links)/paires*100,"%), et",nrow(links),"comme liens (soit",nrow(links)/paires*100,"%).")
