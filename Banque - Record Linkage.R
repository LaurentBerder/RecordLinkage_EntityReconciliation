start.time<-Sys.time()

#charger librairies
library(RecordLinkage)
library(data.table)
library(plyr)
library(dplyr)
library(xlsx)
library(stringr)
library(gsubfn)
library(bigmemory)
options(bigmemory.typecast.warning=FALSE)


#############################################
##############Importer données###############
#############################################
entreprise<-fread(input="ENT_GEN2.csv")
colnames(entreprise)<-c("Muna","Siret","Raison_Sociale","Adresse","CodePostal","Commune","APE","NAF","NACE","CategorieJuridique","DateCreation","IBANpays","IBANcode","IBANcompte")
lourmel<-fread(input="ENT_lourmel.csv")
colnames(lourmel)<-c("Muna","Siret","Raison_Sociale","Adresse","CodePostal","Commune","APE","NAF","NACE","CategorieJuridique","DateCreation","IBANpays","IBANcode","IBANcompte")

BRD<-fread("C:/Users/PB00414/Desktop/Recherche/Données externes/Banque/BRD CAMT054.csv")
colnames(BRD)<-c("MessageIdentification","MCreationDateTime","IdentifiantCompte","ACreationDateTime","IBAN","ReferenceVirement","BIC","NbEntries","Somme","Montant","Devise","CreditDebitCode","Statut","BookingDate","DateComptabilisation","DomainCode","ExternalBankTransactionFamilyCode","ExternalBankTransactionSubfamilyCode","ProprietaryCode","AccountServicerReference","EndToEndIdentification","MontantTransaction","DeviseCompte","Nom","IBANEmetteur","Nom2","noname1","noname2","IdentifiantBanque","NomBanque","Motif")
SGN<-fread("C:/Users/PB00414/Desktop/Recherche/Données externes/Banque/SGN CAMT054.csv")
colnames(SGN)<-c("MessageIdentification","MCreationDateTime","IdentifiantCompte","ACreationDateTime","IBAN","DeviseCompte","BIC","NbEntries","Somme","NbEntries3","Somme4","Montant","Devise","CreditDebitCode","Statut","ExternalBankTransactionDomain1Code","ExternalBankTransactionFamilyCode","ExternalBankTransactionSubfamilyCode","Code","BankTransactionCode","ReferenceCompte","EndToEndIdentification","Montant2","Devise2","Montant3","Devise3","Nom","Pays","Adresse","IBANEmetteur","ReferenceTransaction","Nom13","Motif","AdditionalEntryInformation")
BNP<-fread("C:/Users/PB00414/Desktop/Recherche/Données externes/Banque/BNP CAMT054.csv")
colnames(BNP)<-c("MessageIdentification","MCreationDateTime","IdentifiantCompte","ACreationDateTime","IBAN","DeviseCompte","BIC","NbEntries","Somme","NbEntries3","Somme4","Montant","Devise","CreditDebitCode","Statut","Date","AccountServicerReference","ExternalBankTransactionDomain1Code","ExternalBankTransactionFamilyCode","ExternalBankTransactionSubfamilyCode","EndToEndIdentification","Nom","Pays","Adresse","Motif")
CIC<-fread("C:/Users/PB00414/Desktop/Recherche/Données externes/Banque/CIC CAMT054.csv")
colnames(CIC)<-c("MessageIdentification","MCreationDateTime","PageNb","LastPgInd","IdentifiantCompte","ACreationDateTime","IBAN","DeviseCompte","BIC","NbEntries","Somme","NbEntries3","Somme4","NtryRef","Montant","Devise","CreditDebitCode","Statut","Date","Date5","ExternalBankTransactionDomain1Code","ExternalBankTransactionFamilyCode","ExternalBankTransactionSubfamilyCode","noname1","ReferenceCompte","EndToEndIdentification","Montant8","Devise9","Nom","Adresse","Rmtld","Motif")


#############################################
##############Préparer données###############
#############################################

############Entreprises#################
entreprise$Muna<-paste(entreprise$Muna,".",ifelse(97-(entreprise$Muna%%97)<10,paste(0,97-(entreprise$Muna%%97),sep=''),97-(entreprise$Muna%%97)),sep='')
entreprise<-filter(entreprise,grepl("\\-",Siret)&grepl("\\.",Siret))
entreprise$Siret<-gsub("\\.|\\-","",entreprise$Siret)
entreprise$Siren<-substring(entreprise$Siret,1,9)
entreprise<-select(entreprise,c(Muna,Siret,Siren,Raison_Sociale,Adresse,CodePostal,IBANpays,IBANcode,IBANcompte))
entreprise<-filter(entreprise,!duplicated(Siret))
entreprise$CodePostal[nchar(entreprise$CodePostal)==4]<-paste("0",entreprise$CodePostal[nchar(entreprise$CodePostal)==4],sep='')
entreprise$numDpt<-ifelse(grepl("^97",entreprise$CodePostal), substr(entreprise$CodePostal,1,3), substr(entreprise$CodePostal,1,2))
entreprise$IBANclef<-substr(entreprise$IBANcompte,22,23)
entreprise$IBANcompte<-substr(entreprise$IBANcompte,1,21)
entreprise$IBAN<-paste(entreprise$IBANpays,entreprise$IBANcode,entreprise$IBANcompte,entreprise$IBANclef)
entreprise$IBAN<-gsub('\\s|NA','',entreprise$IBAN)
entreprise[entreprise==""]<-NA

lourmel$Muna<-paste(lourmel$Muna,".",ifelse(97-(lourmel$Muna%%97)<10,paste(0,97-(lourmel$Muna%%97),sep=''),97-(lourmel$Muna%%97)),sep='')
lourmel<-filter(lourmel,grepl("\\-",Siret)&grepl("\\.",Siret))
lourmel$Siret<-gsub("\\.|\\-","",lourmel$Siret)
lourmel$Siren<-substring(lourmel$Siret,1,9)
lourmel<-select(lourmel,c(Muna,Siret,Siren,Raison_Sociale,Adresse,CodePostal,IBANpays,IBANcode,IBANcompte))
lourmel<-filter(lourmel,!duplicated(Siret))
lourmel$CodePostal[nchar(lourmel$CodePostal)==4]<-paste("0",lourmel$CodePostal[nchar(lourmel$CodePostal)==4],sep='')
lourmel$numDpt<-ifelse(grepl("^97",lourmel$CodePostal), substr(lourmel$CodePostal,1,3), substr(lourmel$CodePostal,1,2))
lourmel$IBANclef<-substr(lourmel$IBANcompte,22,23)
lourmel$IBANcompte<-substr(lourmel$IBANcompte,1,21)
lourmel$IBAN<-paste(lourmel$IBANpays,lourmel$IBANcode,lourmel$IBANcompte,lourmel$IBANclef)
lourmel$IBAN<-gsub('\\s|NA','',lourmel$IBAN)
lourmel[lourmel==""]<-NA

##############Banques##################
BRD$Banque<-"BRD"
BRD[BRD==""]<-NA
BRD$Adresse<-NA #le champ Adresse n'existe pas dans ce fichier, et manque pour plusieurs fonctions suivantes
BRD<-filter(BRD,!duplicated(AccountServicerReference)&!is.na(MontantTransaction)) #supprimer lignes dupliquées en parsant l'XML
for(i in 1:nrow(BRD)) {ifelse(grepl("\\d\\d\\d\\d\\d\\d\\d\\d\\.\\d-\\d\\d\\d\\d\\.\\d",BRD$EndToEndIdentification[i]), BRD$Siret[i]<-strapplyc(BRD$EndToEndIdentification[i], "\\d\\d\\d\\d\\d\\d\\d\\d\\.\\d-\\d\\d\\d\\d.\\d"), BRD$Siret[i]<-NA)}
for(i in 1:nrow(BRD)) {if(grepl("S\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d",BRD$EndToEndIdentification[i])) {BRD$Siret[i]<-gsub("S","",strapplyc(BRD$EndToEndIdentification[i], "S\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d", simplify = TRUE))}}
for(i in 1:nrow(BRD)) {if(is.na(BRD$Siret[i]) & grepl("siret|Ref|adh",BRD$EndToEndIdentification[i],ignore.case=TRUE)) BRD$Siret[i]<-substr(gsub("[::A-Z::]|/|\\.|-|\\s","",gsub(".*siret|.*reference|.*ref|.*adh","", BRD$EndToEndIdentification[i],ignore.case=TRUE)),1,14)}
for(i in 1:nrow(BRD)) {if(is.na(BRD$Siret[i]) & grepl("\\d\\d\\d\\d\\d\\d\\d\\d\\s\\d\\s\\d\\d\\d\\d\\s\\d",BRD$EndToEndIdentification[i])) BRD$Siret[i]<-gsub("\\s","",strapplyc(BRD$EndToEndIdentification[i], "\\d\\d\\d\\d\\d\\d\\d\\d\\s\\d\\s\\d\\d\\d\\d\\s\\d"))}
for(i in 1:nrow(BRD)) {if(is.na(BRD$Siret[i]) & grepl("\\d\\d\\d\\d\\d\\d\\d\\d\\.\\d\\.\\d\\d\\d\\d\\.\\d",BRD$EndToEndIdentification[i])) BRD$Siret[i]<-gsub("\\.","",strapplyc(BRD$EndToEndIdentification[i], "\\d\\d\\d\\d\\d\\d\\d\\d\\.\\d\\.\\d\\d\\d\\d\\.\\d"))}
for(i in 1:nrow(BRD)) {if(grepl("\\d\\d\\d\\d\\d\\d\\d\\d\\.\\d-\\d\\d\\d\\d\\.\\d",BRD$Motif[i])) BRD$Siret[i]<-strapplyc(BRD$Motif[i], "\\d\\d\\d\\d\\d\\d\\d\\d\\.\\d-\\d\\d\\d\\d.\\d")}
for(i in 1:nrow(BRD)) {if(grepl("S\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d",BRD$Motif[i])) {BRD$Siret[i]<-gsub("S","",strapplyc(BRD$Motif[i], "S\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d", simplify = TRUE))}}
for(i in 1:nrow(BRD)) {if(is.na(BRD$Siret[i]) & grepl("siret|Ref|adh",BRD$Motif[i],ignore.case=TRUE)) BRD$Siret[i]<-substr(gsub("[::A-Z::]|/|\\.|-|\\s","",gsub(".*siret|.*reference|.*ref|.*adh","", BRD$Motif[i],ignore.case=TRUE)),1,14)}
for(i in 1:nrow(BRD)) {if(is.na(BRD$Siret[i]) & grepl("\\d\\d\\d\\d\\d\\d\\d\\d\\s\\d\\s\\d\\d\\d\\d\\s\\d",BRD$Motif[i])) BRD$Siret[i]<-gsub("\\s","",strapplyc(BRD$Motif[i], "\\d\\d\\d\\d\\d\\d\\d\\d\\s\\d\\s\\d\\d\\d\\d\\s\\d"))}
for(i in 1:nrow(BRD)) {if(is.na(BRD$Siret[i]) & grepl("\\d\\d\\d\\d\\d\\d\\d\\d\\.\\d\\.\\d\\d\\d\\d\\.\\d",BRD$Motif[i])) BRD$Siret[i]<-gsub("\\.","",strapplyc(BRD$Motif[i], "\\d\\d\\d\\d\\d\\d\\d\\d\\.\\d\\.\\d\\d\\d\\d\\.\\d"))}
BRD$Siret<-gsub("-|\\.","",BRD$Siret)
for(i in 1:nrow(BRD)) {if(nchar(BRD$Siret[i])<=13) BRD$Siret[i]<-NA}
for(i in 1:nrow(BRD)) {ifelse(!is.na(BRD$Siret[i]), BRD$Siren[i]<-substring(BRD$Siret[i],1,9), BRD$Siren[i]<-NA)}
for(i in 1:nrow(BRD)) {ifelse(grepl("\\d\\d\\d\\d\\d\\d\\d\\d\\.\\d\\d",BRD$Motif[i]), BRD$Muna[i]<-strapplyc(BRD$Motif[i], "\\d\\d\\d\\d\\d\\d\\d\\d\\.\\d\\d", simplify = TRUE), BRD$Muna[i]<-NA)}
for(i in 1:nrow(BRD)) {ifelse(grepl("\\d\\d\\d\\d\\d", BRD$Adresse[i]), BRD$CodePostal[i]<-strapplyc(BRD$Adresse[i], "\\d\\d\\d\\d\\d"), NA)}
for(i in 1:nrow(BRD)) {BRD$CodePostal[i]<-toString(BRD$CodePostal[[i]][length(BRD$CodePostal[[i]])])}
for(i in 1:nrow(BRD)) {if(grepl("1/",BRD$Nom[i])) BRD$Nom[i]<-gsub("2/.*","", BRD$Nom[i]); BRD$Nom[i]<-gsub("1/","", BRD$Nom[i])}
BRD$CodePostal<-as.character(BRD$CodePostal)
BRD$CodePostal[BRD$CodePostal=="NULL"]<-NA

SGN$Banque<-"SGN"
SGN[SGN==""]<-NA
SGN<-filter(SGN,is.na(Pays)) #supprimer lignes dupliquées en parsant l'XML
SGN<-filter(SGN,!duplicated(Montant))
for(i in 1:nrow(SGN)) {ifelse(grepl("\\d\\d\\d\\d\\d\\d\\d\\d\\.\\d-\\d\\d\\d\\d\\.\\d",SGN$EndToEndIdentification[i]), SGN$Siret[i]<-strapplyc(SGN$EndToEndIdentification[i], "\\d\\d\\d\\d\\d\\d\\d\\d\\.\\d-\\d\\d\\d\\d.\\d"), SGN$Siret[i]<-NA)}
for(i in 1:nrow(SGN)) {if(grepl("S\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d",SGN$EndToEndIdentification[i])) {SGN$Siret[i]<-gsub("S","",strapplyc(SGN$EndToEndIdentification[i], "S\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d", simplify = TRUE))}}
for(i in 1:nrow(SGN)) {if(is.na(SGN$Siret[i]) & grepl("siret|Ref|adh",SGN$EndToEndIdentification[i],ignore.case=TRUE)) SGN$Siret[i]<-substr(gsub("[::A-Z::]|/|\\.|-|\\s","",gsub(".*siret|.*reference|.*ref|.*adh","", SGN$EndToEndIdentification[i],ignore.case=TRUE)),1,14)}
for(i in 1:nrow(SGN)) {if(is.na(SGN$Siret[i]) & grepl("\\d\\d\\d\\d\\d\\d\\d\\d\\s\\d\\s\\d\\d\\d\\d\\s\\d",SGN$EndToEndIdentification[i])) SGN$Siret[i]<-gsub("\\s","",strapplyc(SGN$EndToEndIdentification[i], "\\d\\d\\d\\d\\d\\d\\d\\d\\s\\d\\s\\d\\d\\d\\d\\s\\d"))}
for(i in 1:nrow(SGN)) {if(is.na(SGN$Siret[i]) & grepl("\\d\\d\\d\\d\\d\\d\\d\\d\\.\\d\\.\\d\\d\\d\\d\\.\\d",SGN$EndToEndIdentification[i])) SGN$Siret[i]<-gsub("\\.","",strapplyc(SGN$EndToEndIdentification[i], "\\d\\d\\d\\d\\d\\d\\d\\d\\.\\d\\.\\d\\d\\d\\d\\.\\d"))}
for(i in 1:nrow(SGN)) {if(grepl("\\d\\d\\d\\d\\d\\d\\d\\d\\.\\d-\\d\\d\\d\\d\\.\\d",SGN$Motif[i])) SGN$Siret[i]<-strapplyc(SGN$Motif[i], "\\d\\d\\d\\d\\d\\d\\d\\d\\.\\d-\\d\\d\\d\\d.\\d")}
for(i in 1:nrow(SGN)) {if(grepl("S\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d",SGN$Motif[i])) {SGN$Siret[i]<-gsub("S","",strapplyc(SGN$Motif[i], "S\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d", simplify = TRUE))}}
for(i in 1:nrow(SGN)) {if(is.na(SGN$Siret[i]) & grepl("siret|Ref|adh",SGN$Motif[i],ignore.case=TRUE)) SGN$Siret[i]<-substr(gsub("[::A-Z::]|/|\\.|-|\\s","",gsub(".*siret|.*reference|.*ref|.*adh","", SGN$Motif[i],ignore.case=TRUE)),1,14)}
for(i in 1:nrow(SGN)) {if(is.na(SGN$Siret[i]) & grepl("\\d\\d\\d\\d\\d\\d\\d\\d\\s\\d\\s\\d\\d\\d\\d\\s\\d",SGN$Motif[i])) SGN$Siret[i]<-gsub("\\s","",strapplyc(SGN$Motif[i], "\\d\\d\\d\\d\\d\\d\\d\\d\\s\\d\\s\\d\\d\\d\\d\\s\\d"))}
for(i in 1:nrow(SGN)) {if(is.na(SGN$Siret[i]) & grepl("\\d\\d\\d\\d\\d\\d\\d\\d\\.\\d\\.\\d\\d\\d\\d\\.\\d",SGN$Motif[i])) SGN$Siret[i]<-gsub("\\.","",strapplyc(SGN$Motif[i], "\\d\\d\\d\\d\\d\\d\\d\\d\\.\\d\\.\\d\\d\\d\\d\\.\\d"))}
SGN$Siret<-gsub("-|\\.","",SGN$Siret)
for(i in 1:nrow(SGN)) {if(nchar(SGN$Siret[i])<=13) SGN$Siret[i]<-NA}
for(i in 1:nrow(SGN)) {ifelse(!is.na(SGN$Siret[i]), SGN$Siren[i]<-substring(SGN$Siret[i],1,9), SGN$Siren[i]<-NA)}
for(i in 1:nrow(SGN)) {ifelse(grepl("\\d\\d\\d\\d\\d\\d\\d\\d\\.\\d\\d",SGN$Motif[i]), SGN$Muna[i]<-strapplyc(SGN$Motif[i], "\\d\\d\\d\\d\\d\\d\\d\\d\\.\\d\\d", simplify = TRUE), SGN$Muna[i]<-NA)}
for(i in 1:nrow(SGN)) {ifelse(grepl("\\d\\d\\d\\d\\d", SGN$Adresse[i]), SGN$CodePostal[i]<-strapplyc(SGN$Adresse[i], "\\d\\d\\d\\d\\d"), SGN$CodePostal[i]<-NA)}
for(i in 1:nrow(SGN)) {SGN$CodePostal[i]<-as.character(SGN$CodePostal[i])}
for(i in 1:nrow(SGN)) {if(grepl("1/",SGN$Nom[i])) SGN$Nom[i]<-gsub("2/.*","", SGN$Nom[i]); SGN$Nom[i]<-gsub("1/","", SGN$Nom[i])}
SGN$CodePostal<-as.character(SGN$CodePostal)
SGN$CodePostal[SGN$CodePostal=="NULL"]<-NA


BNP$Banque<-"BNP"
BNP[BNP==""]<-NA
BNP$IBANEmetteur<-NA #le champ IBANEmetteur n'existe pas dans ce fichier, et manque pour plusieurs fonctions suivantes
BNP<-filter(BNP,!duplicated(EndToEndIdentification)) #supprimer lignes dupliquées en parsant l'XML
for(i in 1:nrow(BNP)) {ifelse(grepl("\\d\\d\\d\\d\\d\\d\\d\\d\\.\\d-\\d\\d\\d\\d\\.\\d",BNP$EndToEndIdentification[i]), BNP$Siret[i]<-strapplyc(BNP$EndToEndIdentification[i], "\\d\\d\\d\\d\\d\\d\\d\\d\\.\\d-\\d\\d\\d\\d.\\d"), BNP$Siret[i]<-NA)}
for(i in 1:nrow(BNP)) {if(grepl("S\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d",BNP$EndToEndIdentification[i])) {BNP$Siret[i]<-gsub("S","",strapplyc(BNP$EndToEndIdentification[i], "S\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d", simplify = TRUE))}}
for(i in 1:nrow(BNP)) {if(is.na(BNP$Siret[i]) & grepl("siret|Ref|adh",BNP$EndToEndIdentification[i],ignore.case=TRUE)) BNP$Siret[i]<-substr(gsub("[::A-Z::]|/|\\.|-|\\s","",gsub(".*siret|.*reference|.*ref|.*adh","", BNP$EndToEndIdentification[i],ignore.case=TRUE)),1,14)}
for(i in 1:nrow(BNP)) {if(is.na(BNP$Siret[i]) & grepl("\\d\\d\\d\\d\\d\\d\\d\\d\\s\\d\\s\\d\\d\\d\\d\\s\\d",BNP$EndToEndIdentification[i])) BNP$Siret[i]<-gsub("\\s","",strapplyc(BNP$EndToEndIdentification[i], "\\d\\d\\d\\d\\d\\d\\d\\d\\s\\d\\s\\d\\d\\d\\d\\s\\d"))}
for(i in 1:nrow(BNP)) {if(is.na(BNP$Siret[i]) & grepl("\\d\\d\\d\\d\\d\\d\\d\\d\\.\\d\\.\\d\\d\\d\\d\\.\\d",BNP$EndToEndIdentification[i])) BNP$Siret[i]<-gsub("\\.","",strapplyc(BNP$EndToEndIdentification[i], "\\d\\d\\d\\d\\d\\d\\d\\d\\.\\d\\.\\d\\d\\d\\d\\.\\d"))}
for(i in 1:nrow(BNP)) {if(grepl("\\d\\d\\d\\d\\d\\d\\d\\d\\.\\d-\\d\\d\\d\\d\\.\\d",BNP$Motif[i])) BNP$Siret[i]<-strapplyc(BNP$Motif[i], "\\d\\d\\d\\d\\d\\d\\d\\d\\.\\d-\\d\\d\\d\\d.\\d")}
for(i in 1:nrow(BNP)) {if(grepl("S\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d",BNP$Motif[i])) {BNP$Siret[i]<-gsub("S","",strapplyc(BNP$Motif[i], "S\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d", simplify = TRUE))}}
for(i in 1:nrow(BNP)) {if(is.na(BNP$Siret[i]) & grepl("siret|Ref|adh",BNP$Motif[i],ignore.case=TRUE)) BNP$Siret[i]<-substr(gsub("[::A-Z::]|/|\\.|-|\\s","",gsub(".*siret|.*reference|.*ref|.*adh","", BNP$Motif[i],ignore.case=TRUE)),1,14)}
for(i in 1:nrow(BNP)) {if(is.na(BNP$Siret[i]) & grepl("\\d\\d\\d\\d\\d\\d\\d\\d\\s\\d\\s\\d\\d\\d\\d\\s\\d",BNP$Motif[i])) BNP$Siret[i]<-gsub("\\s","",strapplyc(BNP$Motif[i], "\\d\\d\\d\\d\\d\\d\\d\\d\\s\\d\\s\\d\\d\\d\\d\\s\\d"))}
for(i in 1:nrow(BNP)) {if(is.na(BNP$Siret[i]) & grepl("\\d\\d\\d\\d\\d\\d\\d\\d\\.\\d\\.\\d\\d\\d\\d\\.\\d",BNP$Motif[i])) BNP$Siret[i]<-gsub("\\.","",strapplyc(BNP$Motif[i], "\\d\\d\\d\\d\\d\\d\\d\\d\\.\\d\\.\\d\\d\\d\\d\\.\\d"))}
BNP$Siret<-gsub("-|\\.","",BNP$Siret)
for(i in 1:nrow(BNP)) {if(nchar(BNP$Siret[i])<=13) BNP$Siret[i]<-NA}
for(i in 1:nrow(BNP)) {ifelse(!is.na(BNP$Siret[i]), BNP$Siren[i]<-substring(BNP$Siret[i],1,9), BNP$Siren[i]<-NA)}
for(i in 1:nrow(BNP)) {ifelse(grepl("\\d\\d\\d\\d\\d\\d\\d\\d\\.\\d\\d",BNP$Motif[i]), BNP$Muna[i]<-strapplyc(BNP$Motif[i], "\\d\\d\\d\\d\\d\\d\\d\\d\\.\\d\\d", simplify = TRUE), BNP$Muna[i]<-NA)}
for(i in 1:nrow(BNP)) {ifelse(grepl("\\d\\d\\d\\d\\d", BNP$Adresse[i]), BNP$CodePostal[i]<-strapplyc(BNP$Adresse[i], "\\d\\d\\d\\d\\d"), BNP$CodePostal[i]<-NA)}
for(i in 1:nrow(BNP)) {BNP$CodePostal[i]<-toString(BNP$CodePostal[[i]][length(BNP$CodePostal[[i]])])}
for(i in 1:nrow(BNP)) {if(grepl("1/",BNP$Nom[i])) BNP$Nom[i]<-gsub("2/.*","", BNP$Nom[i]); BNP$Nom[i]<-gsub("1/","", BNP$Nom[i])}
BNP$CodePostal<-as.character(BNP$CodePostal)
BNP$CodePostal[BNP$CodePostal=="NULL"]<-NA

CIC$Banque<-"CIC"
CIC[CIC==""]<-NA
CIC$IBANEmetteur<-NA #le champ IBANEmetteur n'existe pas dans ce fichier, et manque pour plusieurs fonctions suivantes
for(i in 1:nrow(CIC)) {ifelse(grepl("\\d\\d\\d\\d\\d\\d\\d\\d\\.\\d-\\d\\d\\d\\d\\.\\d",CIC$EndToEndIdentification[i]), CIC$Siret[i]<-strapplyc(CIC$EndToEndIdentification[i], "\\d\\d\\d\\d\\d\\d\\d\\d\\.\\d-\\d\\d\\d\\d.\\d"), CIC$Siret[i]<-NA)}
for(i in 1:nrow(CIC)) {if(grepl("S\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d",CIC$EndToEndIdentification[i])) {CIC$Siret[i]<-gsub("S","",strapplyc(CIC$EndToEndIdentification[i], "S\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d", simplify = TRUE))}}
for(i in 1:nrow(CIC)) {if(is.na(CIC$Siret[i]) & grepl("siret|Ref|adh",CIC$EndToEndIdentification[i],ignore.case=TRUE)) CIC$Siret[i]<-substr(gsub("[::A-Z::]|/|\\.|-|\\s","",gsub(".*siret|.*reference|.*ref|.*adh","", CIC$EndToEndIdentification[i],ignore.case=TRUE)),1,14)}
for(i in 1:nrow(CIC)) {if(is.na(CIC$Siret[i]) & grepl("\\d\\d\\d\\d\\d\\d\\d\\d\\s\\d\\s\\d\\d\\d\\d\\s\\d",CIC$EndToEndIdentification[i])) CIC$Siret[i]<-gsub("\\s","",strapplyc(CIC$EndToEndIdentification[i], "\\d\\d\\d\\d\\d\\d\\d\\d\\s\\d\\s\\d\\d\\d\\d\\s\\d"))}
for(i in 1:nrow(CIC)) {if(is.na(CIC$Siret[i]) & grepl("\\d\\d\\d\\d\\d\\d\\d\\d\\.\\d\\.\\d\\d\\d\\d\\.\\d",CIC$EndToEndIdentification[i])) CIC$Siret[i]<-gsub("\\.","",strapplyc(CIC$EndToEndIdentification[i], "\\d\\d\\d\\d\\d\\d\\d\\d\\.\\d\\.\\d\\d\\d\\d\\.\\d"))}
for(i in 1:nrow(CIC)) {if(grepl("\\d\\d\\d\\d\\d\\d\\d\\d\\.\\d-\\d\\d\\d\\d\\.\\d",CIC$Motif[i])) CIC$Siret[i]<-strapplyc(CIC$Motif[i], "\\d\\d\\d\\d\\d\\d\\d\\d\\.\\d-\\d\\d\\d\\d.\\d")}
for(i in 1:nrow(CIC)) {if(grepl("S\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d",CIC$Motif[i])) {CIC$Siret[i]<-gsub("S","",strapplyc(CIC$Motif[i], "S\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d", simplify = TRUE))}}
for(i in 1:nrow(CIC)) {if(is.na(CIC$Siret[i]) & grepl("siret|Ref|adh",CIC$Motif[i],ignore.case=TRUE)) CIC$Siret[i]<-substr(gsub("[::A-Z::]|/|\\.|-|\\s","",gsub(".*siret|.*reference|.*ref|.*adh","", CIC$Motif[i],ignore.case=TRUE)),1,14)}
for(i in 1:nrow(CIC)) {if(is.na(CIC$Siret[i]) & grepl("\\d\\d\\d\\d\\d\\d\\d\\d\\s\\d\\s\\d\\d\\d\\d\\s\\d",CIC$Motif[i])) CIC$Siret[i]<-gsub("\\s","",strapplyc(CIC$Motif[i], "\\d\\d\\d\\d\\d\\d\\d\\d\\s\\d\\s\\d\\d\\d\\d\\s\\d"))}
for(i in 1:nrow(CIC)) {if(is.na(CIC$Siret[i]) & grepl("\\d\\d\\d\\d\\d\\d\\d\\d\\.\\d\\.\\d\\d\\d\\d\\.\\d",CIC$Motif[i])) CIC$Siret[i]<-gsub("\\.","",strapplyc(CIC$Motif[i], "\\d\\d\\d\\d\\d\\d\\d\\d\\.\\d\\.\\d\\d\\d\\d\\.\\d"))}
CIC$Siret<-gsub("-|\\.","",CIC$Siret)
for(i in 1:nrow(CIC)) {if(nchar(CIC$Siret[i])<=13) CIC$Siret[i]<-NA}
for(i in 1:nrow(CIC)) {ifelse(!is.na(CIC$Siret[i]), CIC$Siren[i]<-substring(CIC$Siret[i],1,9), CIC$Siren[i]<-NA)}
for(i in 1:nrow(CIC)) {ifelse(grepl("\\d\\d\\d\\d\\d\\d\\d\\d\\.\\d\\d",CIC$Motif[i]), CIC$Muna[i]<-strapplyc(CIC$Motif[i], "\\d\\d\\d\\d\\d\\d\\d\\d\\.\\d\\d", simplify = TRUE), CIC$Muna[i]<-NA)}
for(i in 1:nrow(CIC)) {ifelse(grepl("\\d\\d\\d\\d\\d", CIC$Adresse[i]), CIC$CodePostal[i]<-strapplyc(CIC$Adresse[i], "\\d\\d\\d\\d\\d"), CIC$CodePostal[i]<-NA)}
for(i in 1:nrow(CIC)) {if(grepl("1/",CIC$Nom[i])) CIC$Adresse[i]<-gsub(".*2/","", CIC$Nom[i])}
for(i in 1:nrow(CIC)) {if(grepl("1/",CIC$Nom[i])) CIC$Nom[i]<-gsub("2/.*","", CIC$Nom[i]); CIC$Nom[i]<-gsub("1/","", CIC$Nom[i])}
CIC$CodePostal<-as.character(CIC$CodePostal)
CIC$CodePostal[CIC$CodePostal=="NULL"]<-NA


import_et_preparation<-Sys.time()

####################################################
####################Match exact#####################
####################################################
#Première étape, matcher par IBAN si possible: on ne peut travailler que sur SGN et BRD
SGNentreprise<-merge(x= SGN, y=entreprise, by.x='IBANEmetteur', by.y='IBANcompte')
SGNentreprise$numDpt.x<-ifelse(grepl("^97",SGNentreprise$CodePostal.x), substr(SGNentreprise$CodePostal.x,1,3), substr(SGNentreprise$CodePostal.x,1,2))
SGN<-filter(SGN,!(IBANEmetteur %in% SGNentreprise$IBANEmetteur)) #supprimer les résultats positifs de la liste pour les raccrochements qui suivent


BRDlourmel<-merge(x=BRD, y=lourmel, by.x='IBANEmetteur', by.y='IBAN')
BRDlourmel$numDpt.x<-ifelse(grepl("^97",BRDlourmel$CodePostal.x), substr(BRDlourmel$CodePostal.x,1,3), substr(BRDlourmel$CodePostal.x,1,2))
BRD<-filter(BRD,!(IBANEmetteur %in% BRDlourmel$IBANEmetteur)) #supprimer les résultats positifs de la liste pour les raccrochements qui suivent


#Deuxième étape, matcher par Muna, Siret et/ou Siren si possible
SGN1<-select(SGN,c(Nom,Adresse,CodePostal,Motif,Siret,Siren,Muna,IBANEmetteur,Banque))
BNP1<-select(BNP,c(Nom,Adresse,CodePostal,Motif,Siret,Siren,Muna,IBANEmetteur,Banque))
CIC1<-select(CIC,c(Nom,Adresse,CodePostal,Motif,Siret,Siren,Muna,IBANEmetteur,Banque))
BRD1<-select(BRD,c(Nom,Adresse,CodePostal,Motif,Siret,Siren,Muna,IBANEmetteur,Banque))
banque<-rbind(SGN1,BNP1,CIC1,BRD1)
rm(SGN1,BNP1,CIC1,BRD1)
banque<-unique(banque)
banque$numDpt<-ifelse(grepl("^97",banque$CodePostal), substr(banque$CodePostal,1,3), substr(banque$CodePostal,1,2))


#matchs
banquentreprise<-merge(x=banque, y=entreprise, by.x='Muna', by.y='Muna',na.omit=TRUE)
banque<-filter(banque,!(Motif %in% banquentreprise$Motif)) #supprimer les résultats positifs de la liste pour les raccrochements qui suivent
banquentreprise<-rbind(banquentreprise,merge(x=banque, y=entreprise, by.x='Siret', by.y='Siret',na.omit=TRUE),fill=TRUE)
banque<-filter(banque,!(Motif %in% banquentreprise$Motif)) #supprimer les résultats positifs de la liste pour les raccrochements qui suivent
banquentreprise<-rbind(banquentreprise,merge(x=banque, y=entreprise, by.x='Siren', by.y='Siren',na.omit=TRUE),fill=TRUE)
banque<-filter(banque,!(Motif %in% banquentreprise$Motif)) #supprimer les résultats positifs de la liste pour les raccrochements qui suivent

banquelourmel<-merge(x=banque, y=lourmel, by.x='Muna', by.y='Muna',na.omit=TRUE)
banque<-filter(banque,!(Motif %in% banquelourmel$Motif))
banquelourmel<-rbind(banquelourmel,merge(x=banque, y=lourmel, by.x='Siret', by.y='Siret',na.omit=TRUE),fill=TRUE)
banque<-filter(banque,!(Motif %in% banquelourmel$Motif))
banquelourmel<-rbind(banquelourmel,merge(x=banque, y=lourmel, by.x='Siren', by.y='Siren',na.omit=TRUE),fill=TRUE)
banque<-filter(banque,!(Motif %in% banquelourmel$Motif))


#On combine tous les résultats positifs ensemble
SGNentreprise<-select(SGNentreprise,c(Nom,Adresse.x,numDpt.x, IBANEmetteur,Muna.x,Siret.x,Siren.x,Banque,Raison_Sociale,Adresse.y,numDpt,IBAN.y,Muna.y,Siret.y,Siren.y))
colnames(SGNentreprise)<-c("Nom_banque","Adresse_banque","Dpt_banque","IBAN_banque","Muna_banque","Siret_banque","Siren_banque","Banque","Raison_Sociale","Adresse","Dpt","IBAN","Muna","Siret","Siren")
BRDlourmel<-select(BRDlourmel,c(Nom,Adresse.x,numDpt.x, IBANEmetteur,Muna.x,Siret.x,Siren.x,Banque,Raison_Sociale,Adresse.y,numDpt,IBAN,Muna.y),Siret.y,Siren.y)
colnames(BRDlourmel)<-c("Nom_banque","Adresse_banque","Dpt_banque","IBAN_banque","Muna_banque","Siret_banque","Siren_banque","Banque","Raison_Sociale","Adresse","Dpt","IBAN","Muna","Siret","Siren")
match_IBAN<-unique(rbind(SGNentreprise,BRDlourmel))

banquelourmel<-cbind(select(banquelourmel,c(Nom,Adresse.x,numDpt.x, IBANEmetteur,Muna,Siret_banque=Siret,Siren.x,Banque,Raison_Sociale,Adresse.y,numDpt.y,IBAN,Muna.y)),banquelourmel$Siret,banquelourmel$Siren)
colnames(banquelourmel)<-c("Nom_banque","Adresse_banque","Dpt_banque","IBAN_banque","Muna_banque","Siret_banque","Siren_banque","Banque","Raison_Sociale","Adresse","Dpt","IBAN","Muna","Siret","Siren")
banquentreprise<-cbind(select(banquentreprise,c(Nom,Adresse.x,numDpt.x,IBANEmetteur,Muna.x,Siret_banque=Siret,Siren.x,Banque,Raison_Sociale,Adresse.y,numDpt.y,IBAN,Muna.y)),banquentreprise$Siret,banquentreprise$Siren)
colnames(banquentreprise)<-c("Nom_banque","Adresse_banque","Dpt_banque","IBAN_banque","Muna_banque","Siret_banque","Siren_banque","Banque","Raison_Sociale","Adresse","Dpt","IBAN","Muna","Siret","Siren")
match_MunaSiretSiren<-unique(rbind(banquelourmel,banquentreprise))
gc(rm(SGNentreprise,BRDlourmel,banquelourmel,banquentreprise))

match_exact.time<-Sys.time()


########################################
#############Record Linkage#############
########################################
comparaisons_possibles<-as.numeric(nrow(banque)*(nrow(entreprise)+nrow(lourmel)))
poids<-big.matrix(nrow=comparaisons_possibles,ncol=1,type="short")

#Troisième étape, record linkage par raison sociale (et adresse si disponible)
reclinklourm<-compare.linkage(select(BRD,c(Nom,Adresse,numDpt,Siret,Siren,Muna,Banque)),select(lourmel,c(Raison_Sociale,Adresse,numDpt,Siret,Siren,Muna,IBANclef)), strcmp=1, strcmpfun=jarowinkler, exclude=2:7)
reclinklourm<-fsWeights(reclinklourm,m=.9,u=reclinklourm$frequencies)
summary(reclinklourm)
reclinklourm<-fsClassify(reclinklourm,threshold.upper=12,threshold.lower=9) #classification en "link", "non-link" ou "possible-link"
linksentr<-getPairs(reclinklourm,single.rows = TRUE, show="links") #on ne montre plus que les "link"
possible_linksentr<-getPairs(reclinklourm,single.rows = TRUE, show="possible")

gc(rm(reclinklourm))


banque<-select(banque,c(Nom,Adresse,numDpt,Siret,Siren,Muna,Banque))
entr0<-select(entreprise,c(Raison_Sociale,Adresse,numDpt,Siret,Siren,Muna,IBANclef))
entr1<-entr0[1:(nrow(entr0)*.05),] #Il faut découper la table entreprise pour raison de mémoire

reclinkentr<-compare.linkage(banque,entr1, strcmp=1:2, strcmpfun=jarowinkler, exclude=3:7)
reclinkentr<-fsWeights(reclinkentr,m=c(.9,.6),u=reclinkentr$frequencies)
summary(reclinkentr)
reclinkentr<-fsClassify(reclinkentr,threshold.upper=12,threshold.lower=9) #classification en "link", "non-link" ou "possible-link"
linksentr<-rbind(linksentr,getPairs(reclinkentr,single.rows = TRUE, show="links")) #on ne montre plus que les "link"
possible_linksentr<-rbind(possible_linksentr,getPairs(reclinkentr,single.rows = TRUE, show="possible"))
poids[1:length(reclinkentr$Wdata)]<-reclinkentr$Wdata
paires<-as.numeric(nrow(reclinkentr$pairs))
gc(rm(entr1,reclinkentr))

entr2<-entr0[((nrow(entr0)*.05)):(nrow(entr0)*.1),]
reclinkentr<-compare.linkage(banque,entr2, strcmp=1:2, strcmpfun=jarowinkler, exclude=3:7)
reclinkentr<-fsWeights(reclinkentr,m=c(.9,.6),u=reclinkentr$frequencies)
reclinkentr<-fsClassify(reclinkentr,threshold.upper=12,threshold.lower=9) #classification en "link", "non-link" ou "possible-link"
linksentr<-rbind(linksentr,getPairs(reclinkentr,single.rows = TRUE, show="links")) #on ne montre plus que les "link"
possible_linksentr<-rbind(possible_linksentr,getPairs(reclinkentr,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclinkentr$Wdata))]<-reclinkentr$Wdata
paires<-as.numeric(paires+nrow(reclinkentr$pairs))
gc(rm(entr2,reclinkentr))

entr3<-entr0[((nrow(entr0)*.1)):(nrow(entr0)*.15),]
reclinkentr<-compare.linkage(banque,entr3, strcmp=1:2, strcmpfun=jarowinkler, exclude=3:7)
reclinkentr<-fsWeights(reclinkentr,m=c(.9,.6),u=reclinkentr$frequencies)
reclinkentr<-fsClassify(reclinkentr,threshold.upper=12,threshold.lower=9) #classification en "link", "non-link" ou "possible-link"
linksentr<-rbind(linksentr,getPairs(reclinkentr,single.rows = TRUE, show="links")) #on ne montre plus que les "link"
possible_linksentr<-rbind(possible_linksentr,getPairs(reclinkentr,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclinkentr$Wdata))]<-reclinkentr$Wdata
paires<-as.numeric(paires+nrow(reclinkentr$pairs))
gc(rm(entr3,reclinkentr))     

entr4<-entr0[((nrow(entr0)*.15)):(nrow(entr0)*.2),]
reclinkentr<-compare.linkage(banque,entr4, strcmp=1:2, strcmpfun=jarowinkler, exclude=3:7)
reclinkentr<-fsWeights(reclinkentr,m=c(.9,.6),u=reclinkentr$frequencies)
reclinkentr<-fsClassify(reclinkentr,threshold.upper=12,threshold.lower=9) #classification en "link", "non-link" ou "possible-link"
linksentr<-rbind(linksentr,getPairs(reclinkentr,single.rows = TRUE, show="links")) #on ne montre plus que les "link"
possible_linksentr<-rbind(possible_linksentr,getPairs(reclinkentr,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclinkentr$Wdata))]<-reclinkentr$Wdata
paires<-as.numeric(paires+nrow(reclinkentr$pairs))
gc(rm(entr4,reclinkentr))     

entr5<-entr0[((nrow(entr0)*.2)):(nrow(entr0)*.25),]
reclinkentr<-compare.linkage(banque,entr5, strcmp=1:2, strcmpfun=jarowinkler, exclude=3:7)
reclinkentr<-fsWeights(reclinkentr,m=c(.9,.6),u=reclinkentr$frequencies)
reclinkentr<-fsClassify(reclinkentr,threshold.upper=12,threshold.lower=9) #classification en "link", "non-link" ou "possible-link"
linksentr<-rbind(linksentr,getPairs(reclinkentr,single.rows = TRUE, show="links")) #on ne montre plus que les "link"
possible_linksentr<-rbind(possible_linksentr,getPairs(reclinkentr,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclinkentr$Wdata))]<-reclinkentr$Wdata
paires<-as.numeric(paires+nrow(reclinkentr$pairs))
gc(rm(entr5,reclinkentr))     #.........

entr6<-entr0[((nrow(entr0)*.25)):(nrow(entr0)*.3),]
reclinkentr<-compare.linkage(banque,entr6, strcmp=1:2, strcmpfun=jarowinkler, exclude=3:7)
reclinkentr<-fsWeights(reclinkentr,m=c(.9,.6),u=reclinkentr$frequencies)
reclinkentr<-fsClassify(reclinkentr,threshold.upper=12,threshold.lower=9) #classification en "link", "non-link" ou "possible-link"
linksentr<-rbind(linksentr,getPairs(reclinkentr,single.rows = TRUE, show="links")) #on ne montre plus que les "link"
possible_linksentr<-rbind(possible_linksentr,getPairs(reclinkentr,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclinkentr$Wdata))]<-reclinkentr$Wdata
paires<-as.numeric(paires+nrow(reclinkentr$pairs))
gc(rm(entr6,reclinkentr))     

entr7<-entr0[((nrow(entr0)*.3)):(nrow(entr0)*.35),]
reclinkentr<-compare.linkage(banque,entr7, strcmp=1:2, strcmpfun=jarowinkler, exclude=3:7)
reclinkentr<-fsWeights(reclinkentr,m=c(.9,.6),u=reclinkentr$frequencies)
reclinkentr<-fsClassify(reclinkentr,threshold.upper=12,threshold.lower=9) #classification en "link", "non-link" ou "possible-link"
linksentr<-rbind(linksentr,getPairs(reclinkentr,single.rows = TRUE, show="links")) #on ne montre plus que les "link"
possible_linksentr<-rbind(possible_linksentr,getPairs(reclinkentr,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclinkentr$Wdata))]<-reclinkentr$Wdata
paires<-as.numeric(paires+nrow(reclinkentr$pairs))
gc(rm(entr7,reclinkentr))     

entr8<-entr0[((nrow(entr0)*.35)):(nrow(entr0)*.4),]
reclinkentr<-compare.linkage(banque,entr8, strcmp=1:2, strcmpfun=jarowinkler, exclude=3:7)
reclinkentr<-fsWeights(reclinkentr,m=c(.9,.6),u=reclinkentr$frequencies)
reclinkentr<-fsClassify(reclinkentr,threshold.upper=12,threshold.lower=9) #classification en "link", "non-link" ou "possible-link"
linksentr<-rbind(linksentr,getPairs(reclinkentr,single.rows = TRUE, show="links")) #on ne montre plus que les "link"
possible_linksentr<-rbind(possible_linksentr,getPairs(reclinkentr,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclinkentr$Wdata))]<-reclinkentr$Wdata
paires<-as.numeric(paires+nrow(reclinkentr$pairs))
gc(rm(entr8,reclinkentr))     

entr9<-entr0[((nrow(entr0)*.4)):(nrow(entr0)*.45),]
reclinkentr<-compare.linkage(banque,entr9, strcmp=1:2, strcmpfun=jarowinkler, exclude=3:7)
reclinkentr<-fsWeights(reclinkentr,m=c(.9,.6),u=reclinkentr$frequencies)
reclinkentr<-fsClassify(reclinkentr,threshold.upper=12,threshold.lower=9) #classification en "link", "non-link" ou "possible-link"
linksentr<-rbind(linksentr,getPairs(reclinkentr,single.rows = TRUE, show="links")) #on ne montre plus que les "link"
possible_linksentr<-rbind(possible_linksentr,getPairs(reclinkentr,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclinkentr$Wdata))]<-reclinkentr$Wdata
paires<-as.numeric(paires+nrow(reclinkentr$pairs))
gc(rm(entr9,reclinkentr))     

entr10<-entr0[((nrow(entr0)*.45)):(nrow(entr0)*.5),]
reclinkentr<-compare.linkage(banque,entr10, strcmp=1:2, strcmpfun=jarowinkler, exclude=3:7)
reclinkentr<-fsWeights(reclinkentr,m=c(.9,.6),u=reclinkentr$frequencies)
reclinkentr<-fsClassify(reclinkentr,threshold.upper=12,threshold.lower=9) #classification en "link", "non-link" ou "possible-link"
linksentr<-rbind(linksentr,getPairs(reclinkentr,single.rows = TRUE, show="links")) #on ne montre plus que les "link"
possible_linksentr<-rbind(possible_linksentr,getPairs(reclinkentr,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclinkentr$Wdata))]<-reclinkentr$Wdata
paires<-as.numeric(paires+nrow(reclinkentr$pairs))
gc(rm(entr10,reclinkentr))     

entr11<-entr0[((nrow(entr0)*.5)):(nrow(entr0)*.55),]
reclinkentr<-compare.linkage(banque,entr11, strcmp=1:2, strcmpfun=jarowinkler, exclude=3:7)
reclinkentr<-fsWeights(reclinkentr,m=c(.9,.6),u=reclinkentr$frequencies)
reclinkentr<-fsClassify(reclinkentr,threshold.upper=12,threshold.lower=9) #classification en "link", "non-link" ou "possible-link"
linksentr<-rbind(linksentr,getPairs(reclinkentr,single.rows = TRUE, show="links")) #on ne montre plus que les "link"
possible_linksentr<-rbind(possible_linksentr,getPairs(reclinkentr,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclinkentr$Wdata))]<-reclinkentr$Wdata
paires<-as.numeric(paires+nrow(reclinkentr$pairs))
gc(rm(entr11,reclinkentr))     

entr12<-entr0[((nrow(entr0)*.55)):(nrow(entr0)*.6),]
reclinkentr<-compare.linkage(banque,entr12, strcmp=1:2, strcmpfun=jarowinkler, exclude=3:7)
reclinkentr<-fsWeights(reclinkentr,m=c(.9,.6),u=reclinkentr$frequencies)
reclinkentr<-fsClassify(reclinkentr,threshold.upper=12,threshold.lower=9) #classification en "link", "non-link" ou "possible-link"
linksentr<-rbind(linksentr,getPairs(reclinkentr,single.rows = TRUE, show="links")) #on ne montre plus que les "link"
possible_linksentr<-rbind(possible_linksentr,getPairs(reclinkentr,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclinkentr$Wdata))]<-reclinkentr$Wdata
paires<-as.numeric(paires+nrow(reclinkentr$pairs))
gc(rm(entr12,reclinkentr))     

entr13<-entr0[((nrow(entr0)*.6)):(nrow(entr0)*.65),]
reclinkentr<-compare.linkage(banque,entr13, strcmp=1:2, strcmpfun=jarowinkler, exclude=3:7)
reclinkentr<-fsWeights(reclinkentr,m=c(.9,.6),u=reclinkentr$frequencies)
reclinkentr<-fsClassify(reclinkentr,threshold.upper=12,threshold.lower=9) #classification en "link", "non-link" ou "possible-link"
linksentr<-rbind(linksentr,getPairs(reclinkentr,single.rows = TRUE, show="links")) #on ne montre plus que les "link"
possible_linksentr<-rbind(possible_linksentr,getPairs(reclinkentr,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclinkentr$Wdata))]<-reclinkentr$Wdata
paires<-as.numeric(paires+nrow(reclinkentr$pairs))
gc(rm(entr13,reclinkentr))     

entr14<-entr0[((nrow(entr0)*.65)):(nrow(entr0)*.7),]
reclinkentr<-compare.linkage(banque,entr14, strcmp=1:2, strcmpfun=jarowinkler, exclude=3:7)
reclinkentr<-fsWeights(reclinkentr,m=c(.9,.6),u=reclinkentr$frequencies)
reclinkentr<-fsClassify(reclinkentr,threshold.upper=12,threshold.lower=9) #classification en "link", "non-link" ou "possible-link"
linksentr<-rbind(linksentr,getPairs(reclinkentr,single.rows = TRUE, show="links")) #on ne montre plus que les "link"
possible_linksentr<-rbind(possible_linksentr,getPairs(reclinkentr,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclinkentr$Wdata))]<-reclinkentr$Wdata
paires<-as.numeric(paires+nrow(reclinkentr$pairs))
gc(rm(entr14,reclinkentr))     

entr15<-entr0[((nrow(entr0)*.7)):(nrow(entr0)*.75),]
reclinkentr<-compare.linkage(banque,entr15, strcmp=1:2, strcmpfun=jarowinkler, exclude=3:7)
reclinkentr<-fsWeights(reclinkentr,m=c(.9,.6),u=reclinkentr$frequencies)
reclinkentr<-fsClassify(reclinkentr,threshold.upper=12,threshold.lower=9) #classification en "link", "non-link" ou "possible-link"
linksentr<-rbind(linksentr,getPairs(reclinkentr,single.rows = TRUE, show="links")) #on ne montre plus que les "link"
possible_linksentr<-rbind(possible_linksentr,getPairs(reclinkentr,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclinkentr$Wdata))]<-reclinkentr$Wdata
paires<-as.numeric(paires+nrow(reclinkentr$pairs))
gc(rm(entr15,reclinkentr))     

entr16<-entr0[((nrow(entr0)*.75)):(nrow(entr0)*.8),]
reclinkentr<-compare.linkage(banque,entr16, strcmp=1:2, strcmpfun=jarowinkler, exclude=3:7)
reclinkentr<-fsWeights(reclinkentr,m=c(.9,.6),u=reclinkentr$frequencies)
reclinkentr<-fsClassify(reclinkentr,threshold.upper=12,threshold.lower=9) #classification en "link", "non-link" ou "possible-link"
linksentr<-rbind(linksentr,getPairs(reclinkentr,single.rows = TRUE, show="links")) #on ne montre plus que les "link"
possible_linksentr<-rbind(possible_linksentr,getPairs(reclinkentr,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclinkentr$Wdata))]<-reclinkentr$Wdata
paires<-as.numeric(paires+nrow(reclinkentr$pairs))
gc(rm(entr16,reclinkentr))     

entr17<-entr0[((nrow(entr0)*.8)):(nrow(entr0)*.85),]
reclinkentr<-compare.linkage(banque,entr17, strcmp=1:2, strcmpfun=jarowinkler, exclude=3:7)
reclinkentr<-fsWeights(reclinkentr,m=c(.9,.6),u=reclinkentr$frequencies)
reclinkentr<-fsClassify(reclinkentr,threshold.upper=12,threshold.lower=9) #classification en "link", "non-link" ou "possible-link"
linksentr<-rbind(linksentr,getPairs(reclinkentr,single.rows = TRUE, show="links")) #on ne montre plus que les "link"
possible_linksentr<-rbind(possible_linksentr,getPairs(reclinkentr,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclinkentr$Wdata))]<-reclinkentr$Wdata
paires<-as.numeric(paires+nrow(reclinkentr$pairs))
gc(rm(entr17,reclinkentr))     

entr18<-entr0[((nrow(entr0)*.85)):(nrow(entr0)*.9),]
reclinkentr<-compare.linkage(banque,entr18, strcmp=1:2, strcmpfun=jarowinkler, exclude=3:7)
reclinkentr<-fsWeights(reclinkentr,m=c(.9,.6),u=reclinkentr$frequencies)
reclinkentr<-fsClassify(reclinkentr,threshold.upper=12,threshold.lower=9) #classification en "link", "non-link" ou "possible-link"
linksentr<-rbind(linksentr,getPairs(reclinkentr,single.rows = TRUE, show="links")) #on ne montre plus que les "link"
possible_linksentr<-rbind(possible_linksentr,getPairs(reclinkentr,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclinkentr$Wdata))]<-reclinkentr$Wdata
paires<-as.numeric(paires+nrow(reclinkentr$pairs))
gc(rm(entr18,reclinkentr))     

entr19<-entr0[((nrow(entr0)*.9)):(nrow(entr0)*.95),]
reclinkentr<-compare.linkage(banque,entr19, strcmp=1:2, strcmpfun=jarowinkler, exclude=3:7)
reclinkentr<-fsWeights(reclinkentr,m=c(.9,.6),u=reclinkentr$frequencies)
reclinkentr<-fsClassify(reclinkentr,threshold.upper=12,threshold.lower=9) #classification en "link", "non-link" ou "possible-link"
linksentr<-rbind(linksentr,getPairs(reclinkentr,single.rows = TRUE, show="links")) #on ne montre plus que les "link"
possible_linksentr<-rbind(possible_linksentr,getPairs(reclinkentr,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclinkentr$Wdata))]<-reclinkentr$Wdata
paires<-as.numeric(paires+nrow(reclinkentr$pairs))
gc(rm(entr19,reclinkentr))     

entr20<-entr0[((nrow(entr0)*.95)):nrow(entr0),]
reclinkentr<-compare.linkage(banque,entr20, strcmp=1:2, strcmpfun=jarowinkler, exclude=3:7)
reclinkentr<-fsWeights(reclinkentr,m=c(.9,.6),u=reclinkentr$frequencies)
reclinkentr<-fsClassify(reclinkentr,threshold.upper=12,threshold.lower=9) #classification en "link", "non-link" ou "possible-link"
linksentr<-rbind(linksentr,getPairs(reclinkentr,single.rows = TRUE, show="links")) #on ne montre plus que les "link"
possible_linksentr<-rbind(possible_linksentr,getPairs(reclinkentr,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclinkentr$Wdata))]<-reclinkentr$Wdata
paires<-as.numeric(paires+nrow(reclinkentr$pairs))

record_linkage<-Sys.time()


poids<-poids[1:paires,]
#Observation de la distribution des poids de comparaison (pour amélioration future)
jpeg(filename="Distribution des poids de comparaison Banque-Entreprise.jpeg")
par(mar=c(3, 3, 1, 1),mgp=c(1.5,0.5,0))
hist(poids[poids>0],main = "Distribution des poids de comparaison",col.main="dodgerblue4", font.lab=2,col.lab="dodgerblue4",ylab="Effectifs", xlab = "Poids", labels=TRUE, col="#9BBB59", breaks=30, xlim=c(min(poids[poids>0]),max(poids)+5))
dev.off()


#Une fois fini, on observe et enregistre les liens et les possibles liens (les non liens ne sont pas nécessaires)
linksentr<-arrange(linksentr,desc(id1),desc(Weight))
View(linksentr)
possible_linksentr<-arrange(possible_linksentr, desc(id1), desc(Weight))
View(possible_linksentr)
match_MunaSiretSiren<-arrange(match_MunaSiretSiren, desc(Muna), desc(Siret), desc(Siren))
write.xlsx2(match_IBAN,"liens_banques.xlsx",sheetName="match IBAN",showNA=FALSE)
write.xlsx2(match_MunaSiretSiren,"liens_banques.xlsx",sheetName="match Muna-Siret-Siren",showNA=FALSE,append=TRUE)
write.xlsx2(select(linksentr,-IBANclef),"liens_banques.xlsx",sheetName="RaisonSociale: liens sûrs",showNA=FALSE,append=TRUE)
write.xlsx2(select(possible_linksentr,-IBANclef),"liens_banques.xlsx",sheetName="RaisonSociale: possibles liens",showNA=FALSE,append=TRUE)


######################################################
##############Message de fin##########################
######################################################
end.time<-Sys.time()
time.taken <- end.time - start.time
exact.comp.time<-match_exact.time - import_et_preparation
comparaison.time<-end.time - import_et_preparation
prep<-import_et_preparation - start.time

bloquees<-comparaisons_possibles-paires
nombre_entreprises_liens<-length(unique(linksentr$id1))
nombre_entreprises_possible<-length(unique(possible_linksentr$id1))

cat("Temps d'exécution total: ",time.taken,units(time.taken),"\ndont",prep,units(prep),"pour l'import et la préparation de données,\net",comparaison.time,units(comparaison.time),"dont",exact.comp.time,units(exact.comp.time),"pour le raccrochement par IBAN, Siret ou Muna.\n\n",
    nrow(match_IBAN),"rattachements ont été effectués par IBAN, et",nrow(match_MunaSiretSiren),"par Muna, Siret ou Siren.",
    "\n\nSur",paires,"comparaisons possibles,",paires-(nrow(possible_linksentr)+nrow(linksentr)),"ont été qualifiées comme non-liens (soit",(paires-(nrow(possible_linksentr)+nrow(linksentr)))/paires*100,"%),\n",nrow(possible_linksentr),"(représentant",nombre_entreprises_possible,"entreprises) comme possibles liens (soit",nrow(possible_linksentr)/paires*100,"%), et\n",nrow(linksentr),"(représentant",nombre_entreprises_liens,"entreprises) comme liens (soit",nrow(linksentr)/paires*100,"%).")

