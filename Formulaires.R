
# Démarrage ---------------------------------------------------------------
#charger librairies
library(RecordLinkage)
library(plyr)
library(dplyr)
library(readxl)
library(xlsx)
library(openxlsx)
library(stringr)
library(data.table)
library(Hmisc)
library(bigmemory)
options(bigmemory.typecast.warning=FALSE)


#se situer sur le dossier ou sont les données
setwd("C:/Users/PB00414/Desktop/Recherche/Données externes")

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

#import des données
full_tit<-fread("Full_Titulaires.csv") %>% as.data.table
regions<-read_excel("reg2016.xls",sheet="Feuille1")
dpt<-read_excel("depts2016.xls",sheet="Feuille1")

#préparation
regions<-regions[,c("REGION,C,2","NCC,C,70")]
colnames(regions)<-c("numReg","nomReg")
dpt<-dpt[,c("DEP,C,3","NCC,C,70","REGION,C,2")]
colnames(dpt)<-c("numDpt","nomDpt","numReg")
dpt<-join(dpt,regions,type="left",by="numReg")
dpt$nomReg<-gsub("(^|[[:space:]]|\\-)([[:alpha:]])", "\\1\\U\\2", tolower(dpt$nomReg), perl=TRUE)
dpt$nomReg <- gsub("\\-", "\n", dpt$nomReg)
dpt<-rbind(dpt,c("Etranger","Etranger","Etranger","Etranger"))

full_tit<-filter(full_tit,PRE_PERS_NOMUS!="XXXXXX")
full_tit<-filter(full_tit,PRE_PERS_NOMUS!="TEST")
full_tit<-filter(full_tit,PRE_PERS_PREUS!="TEST")
full_tit<-filter(full_tit,(nchar(full_tit$PRE_PERS_PREUS)>1))
full_tit[full_tit==""]<-NA
full_tit$PRE_DOSS_TELFIXE<-formattelephone(full_tit$PRE_DOSS_TELFIXE)
full_tit$PRE_DOSS_TELPORT<-formattelephone(full_tit$PRE_DOSS_TELPORT)
full_tit$ANNEE_NAISS<-substr(full_tit$PRE_PERS_DTNAI,6,10)
full_tit$PRE_DOSS_MAIL<-tolower(full_tit$PRE_DOSS_MAIL)
full_tit$PRE_DOSS_MAIL<-gsub(' ','',full_tit$PRE_DOSS_MAIL)
full_tit$PRE_DOSS_MAIL[grepl("pasdemail",full_tit$PRE_DOSS_MAIL)]<-NA
full_tit$PRE_DOSS_MAIL[grepl("adresse",full_tit$PRE_DOSS_MAIL)]<-NA
full_tit$PRE_DOSS_MAIL[grepl("pasde@",full_tit$PRE_DOSS_MAIL)]<-NA
full_tit$PRE_DOSS_MAIL[grepl("xxxx",full_tit$PRE_DOSS_MAIL)]<-NA
full_tit$PRE_DOSS_CP[full_tit$PRE_DOSS_CPAYS %nin% c(NA,"100","138","200","300","400","428","607","700","701","705")]<-"Etranger"
full_tit$PRE_DOSS_CP<-str_pad(full_tit$PRE_DOSS_CP, width=5, side="left", pad="0")
full_tit<-select(full_tit,-c(PRE_DOSS_CPAYS,PRE_PERS_TYPE))
full_tit$numDpt<-ifelse(full_tit$PRE_DOSS_CP=="Etranger", "Etranger", ifelse(grepl("^97",full_tit$PRE_DOSS_CP), substr(full_tit$PRE_DOSS_CP,1,3), substr(full_tit$PRE_DOSS_CP,1,2)))
full_tit<-join(full_tit,dpt,by="numDpt")

#séparation
temporaires<-filter(full_tit,PRE_PERS_MUNAPA>99000000) %>% as.data.table
titulaires<-filter(full_tit,PRE_PERS_MUNAPA<99000000) %>% as.data.table
gc(rm(formattelephone,dpt,regions),verbose=FALSE)


# Répartition géographique-------------------------------------------------

data.frame(t(table(titulaires$nomReg)),rownames = "Var2") %>% select(-Var2)

regtit<-t(table(titulaires$nomReg)) %>% as.data.frame %>% select(-Var1)
rownames(regtit)<-regtit[,1]
regtit<-select(regtit,-Var2)
regtemp<-t(table(temporaires$nomReg)) %>% as.data.frame %>% select(-Var1)
rownames(regtemp)<-regtemp[,1]
regtemp<-select(regtemp,-Var2)
regtemp<-regtemp/sum(regtemp)*100
regtit$Freq<-regtit$Freq/sum(regtit$Freq)*100
distriregion<-cbind(regtit,regtemp)
rownames(distriregion)<-c("Titulaires","Temporaires")
par(mar=c(6.5, 4, 2, 0))
barplot(distriregion,beside=TRUE,main="Distribution des titulaires \net temporaires par région",xlab="",ylab = "Pourcentage d'individus",las=2,cex.names=0.8,col=c("#A651D4", "#72C286"),ylim=c(0,20),legend.text=TRUE)

agetit<-table(titulaires$ANNEE_NAISS)
agetit<-as.data.frame(agetit/sum(agetit)*100)
agetemp<-table(temporaires$ANNEE_NAISS)
agetemp<-as.data.frame(agetemp/sum(agetemp)*100)
distriage<-as.data.table(t(merge(agetemp,agetit,by="Var1",all=TRUE)))
distriage[is.na(distriage)]<-0
distriage<-as.matrix(rbind(as.integer(distriage[1,]),as.numeric(distriage[2,]),as.numeric(distriage[3,])))
colnames(distriage)<-distriage[1,]
distriage<-distriage[-1,]
rownames(distriage)<-c("Titulaires","Temporaires")

par(mar=c(6.5, 4, 2, 0))
barplot(distriage,beside=TRUE,main="Distribution des titulaires \net temporaires par année de naissance",xlab="Année",ylab = "Pourcentage d'individus",las=2,cex.names=0.8,col=c("#A651D4", "#72C286"),ylim=c(0,4),legend.text=TRUE)

gc(rm(agetit,agetemp,regtit,regtemp,distriage,distriregion),verbose=FALSE)

temporaires<-within(temporaires,rm("numDpt","nomDpt","nomReg"))
titulaires<-within(titulaires,rm("numDpt","nomDpt","nomReg"))


# Match -------------------------------------------------------------------
#match exact
match<-inner_join(temporaires,titulaires,by="PRE_PERS_SS",suffix=c(".temp",".tit"))
#portable<-as.data.table(inner_join(filter(temporaires,!is.na(PRE_DOSS_TELPORT)),filter(titulaires,!is.na(PRE_DOSS_TELPORT)),by="PRE_DOSS_TELPORT",suffix=c(".temp",".tit")))
#email<-as.data.table(inner_join(filter(temporaires,!is.na(PRE_DOSS_MAIL)),filter(titulaires,!is.na(PRE_DOSS_MAIL)),by="PRE_DOSS_MAIL",suffix=c(".temp",".tit")))
match$PRE_PERS_SS.tit<-match$PRE_PERS_SS
match$PRE_PERS_SS.temp<-match$PRE_PERS_SS
match<-select(match,-PRE_PERS_SS)
match$Methode<-"SS"

#SS<-select(SS,c(PRE_PERS_MUNAPA.temp,PRE_PERS_NOMUS.temp,PRE_PERS_NOM.temp,PRE_PERS_PREUS.temp,PRE_PERS_DTNAI.temp,ANNEE_NAISS.temp,PRE_PERS_SS.temp,PRE_DOSS_TELFIXE.temp,PRE_DOSS_TELPORT.temp,PRE_DOSS_MAIL.temp,PRE_DOSS_RUE.temp,PRE_DOSS_CRUE.temp,PRE_DOSS_CP.temp,PRE_DOSS_VILLE.temp,numReg.temp,PRE_PERS_MUNAPA.tit,PRE_PERS_NOMUS.tit,PRE_PERS_NOM.tit,PRE_PERS_PREUS.tit,PRE_PERS_DTNAI.tit,ANNEE_NAISS.tit,PRE_PERS_SS.tit,PRE_DOSS_TELFIXE.tit,PRE_DOSS_TELPORT.tit,PRE_DOSS_MAIL.tit,PRE_DOSS_RUE.tit,PRE_DOSS_CRUE.tit,PRE_DOSS_CP.tit,PRE_DOSS_VILLE.tit,numReg.tit,Methode))

#portable$PRE_DOSS_TELPORT.tit<-portable$PRE_DOSS_TELPORT
#portable$PRE_DOSS_TELPORT.temp<-portable$PRE_DOSS_TELPORT
#portable<-select(portable,-PRE_DOSS_TELPORT)
#portable$Methode<-"Portable"
#email$PRE_DOSS_MAIL.tit<-email$PRE_DOSS_MAIL
#email$PRE_DOSS_MAIL.temp<-email$PRE_DOSS_MAIL
#email<-select(email,-PRE_DOSS_MAIL)
#email$Methode<-"email"
#match<-rbind(SS,portable,email) %>% as.data.table()
match<-distinct(match,PRE_PERS_MUNAPA.temp,PRE_PERS_MUNAPA.tit,.keep_all = TRUE)
match<-select(match,c(PRE_PERS_MUNAPA.temp,PRE_PERS_NOMUS.temp,PRE_PERS_NOM.temp,PRE_PERS_PREUS.temp,PRE_PERS_DTNAI.temp,ANNEE_NAISS.temp,PRE_PERS_SS.temp,PRE_DOSS_TELFIXE.temp,PRE_DOSS_TELPORT.temp,PRE_DOSS_MAIL.temp,PRE_DOSS_RUE.temp,PRE_DOSS_CRUE.temp,PRE_DOSS_CP.temp,PRE_DOSS_VILLE.temp,numReg.temp,PRE_PERS_MUNAPA.tit,PRE_PERS_NOMUS.tit,PRE_PERS_NOM.tit,PRE_PERS_PREUS.tit,PRE_PERS_DTNAI.tit,ANNEE_NAISS.tit,PRE_PERS_SS.tit,PRE_DOSS_TELFIXE.tit,PRE_DOSS_TELPORT.tit,PRE_DOSS_MAIL.tit,PRE_DOSS_RUE.tit,PRE_DOSS_CRUE.tit,PRE_DOSS_CP.tit,PRE_DOSS_VILLE.tit,numReg.tit,Methode))
#gc(rm(email,portable,SS),verbose=FALSE)

#supprimer les résultats positifs de la liste pour les raccrochements qui suivent
temporaires<-filter(temporaires,!(PRE_PERS_MUNAPA %in% match$PRE_PERS_MUNAPA.temp))
titulaires<-filter(titulaires,!(PRE_PERS_MUNAPA %in% match$PRE_PERS_MUNAPA.tit))

#record linkage
taille<-nrow(titulaires)/200
poids<-big.matrix(nrow=(taille*nrow(temporaires)+500),ncol=1,type="short")

titulaires0<-titulaires[1:taille,]
reclink<-compare.linkage(temporaires,titulaires0,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-getPairs(reclink,single.rows = TRUE, show="links")
possible_links<-getPairs(reclink,single.rows = TRUE, show="possible")
poids[1:length(reclink$Wdata)]<-reclink$Wdata
paires<-as.numeric(nrow(reclink$pairs))
gc(rm(titulaires0,reclink),verbose = FALSE)


titulaires1<-titulaires[(1*taille+1):(2*taille),]
reclink<-compare.linkage(temporaires,titulaires1,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires1,reclink),verbose = FALSE)


titulaires2<-titulaires[(2*taille+1):(3*taille),]
reclink<-compare.linkage(temporaires,titulaires2,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires2,reclink),verbose = FALSE)


titulaires3<-titulaires[(3*taille+1):(4*taille),]
reclink<-compare.linkage(temporaires,titulaires3,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires3,reclink),verbose = FALSE)


titulaires4<-titulaires[(4*taille+1):(5*taille),]
reclink<-compare.linkage(temporaires,titulaires4,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires4,reclink),verbose = FALSE)


titulaires5<-titulaires[(5*taille+1):(6*taille),]
reclink<-compare.linkage(temporaires,titulaires5,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires5,reclink),verbose = FALSE)


titulaires6<-titulaires[(6*taille+1):(7*taille),]
reclink<-compare.linkage(temporaires,titulaires6,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires6,reclink),verbose = FALSE)


titulaires7<-titulaires[(7*taille+1):(8*taille),]
reclink<-compare.linkage(temporaires,titulaires7,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires7,reclink),verbose = FALSE)


titulaires8<-titulaires[(8*taille+1):(9*taille),]
reclink<-compare.linkage(temporaires,titulaires8,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires8,reclink),verbose = FALSE)


titulaires9<-titulaires[(9*taille+1):(10*taille),]
reclink<-compare.linkage(temporaires,titulaires9,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires9,reclink),verbose = FALSE)


titulaires10<-titulaires[(10*taille+1):(11*taille),]
reclink<-compare.linkage(temporaires,titulaires10,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires10,reclink),verbose = FALSE)


titulaires11<-titulaires[(11*taille+1):(12*taille),]
reclink<-compare.linkage(temporaires,titulaires11,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires11,reclink),verbose = FALSE)


titulaires12<-titulaires[(12*taille+1):(13*taille),]
reclink<-compare.linkage(temporaires,titulaires12,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires12,reclink),verbose = FALSE)


titulaires13<-titulaires[(13*taille+1):(14*taille),]
reclink<-compare.linkage(temporaires,titulaires13,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires13,reclink),verbose = FALSE)


titulaires14<-titulaires[(14*taille+1):(15*taille),]
reclink<-compare.linkage(temporaires,titulaires14,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires14,reclink),verbose = FALSE)


titulaires15<-titulaires[(15*taille+1):(16*taille),]
reclink<-compare.linkage(temporaires,titulaires15,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires15,reclink),verbose = FALSE)


titulaires16<-titulaires[(16*taille+1):(17*taille),]
reclink<-compare.linkage(temporaires,titulaires16,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires16,reclink),verbose = FALSE)


titulaires17<-titulaires[(17*taille+1):(18*taille),]
reclink<-compare.linkage(temporaires,titulaires17,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires17,reclink),verbose = FALSE)


titulaires18<-titulaires[(18*taille+1):(19*taille),]
reclink<-compare.linkage(temporaires,titulaires18,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires18,reclink),verbose = FALSE)


titulaires19<-titulaires[(19*taille+1):(20*taille),]
reclink<-compare.linkage(temporaires,titulaires19,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires19,reclink),verbose = FALSE)


titulaires20<-titulaires[(20*taille+1):(21*taille),]
reclink<-compare.linkage(temporaires,titulaires20,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires20,reclink),verbose = FALSE)


titulaires21<-titulaires[(21*taille+1):(22*taille),]
reclink<-compare.linkage(temporaires,titulaires21,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires21,reclink),verbose = FALSE)


titulaires22<-titulaires[(22*taille+1):(23*taille),]
reclink<-compare.linkage(temporaires,titulaires22,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires22,reclink),verbose = FALSE)


titulaires23<-titulaires[(23*taille+1):(24*taille),]
reclink<-compare.linkage(temporaires,titulaires23,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires23,reclink),verbose = FALSE)


titulaires24<-titulaires[(24*taille+1):(25*taille),]
reclink<-compare.linkage(temporaires,titulaires24,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires24,reclink),verbose = FALSE)


titulaires25<-titulaires[(25*taille+1):(26*taille),]
reclink<-compare.linkage(temporaires,titulaires25,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires25,reclink),verbose = FALSE)


titulaires26<-titulaires[(26*taille+1):(27*taille),]
reclink<-compare.linkage(temporaires,titulaires26,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires26,reclink),verbose = FALSE)


titulaires27<-titulaires[(27*taille+1):(28*taille),]
reclink<-compare.linkage(temporaires,titulaires27,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires27,reclink),verbose = FALSE)


titulaires28<-titulaires[(28*taille+1):(29*taille),]
reclink<-compare.linkage(temporaires,titulaires28,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires28,reclink),verbose = FALSE)


titulaires29<-titulaires[(29*taille+1):(30*taille),]
reclink<-compare.linkage(temporaires,titulaires29,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires29,reclink),verbose = FALSE)


titulaires30<-titulaires[(30*taille+1):(31*taille),]
reclink<-compare.linkage(temporaires,titulaires30,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires30,reclink),verbose = FALSE)


titulaires31<-titulaires[(31*taille+1):(32*taille),]
reclink<-compare.linkage(temporaires,titulaires31,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires31,reclink),verbose = FALSE)


titulaires32<-titulaires[(32*taille+1):(33*taille),]
reclink<-compare.linkage(temporaires,titulaires32,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires32,reclink),verbose = FALSE)


titulaires33<-titulaires[(33*taille+1):(34*taille),]
reclink<-compare.linkage(temporaires,titulaires33,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires33,reclink),verbose = FALSE)


titulaires34<-titulaires[(34*taille+1):(35*taille),]
reclink<-compare.linkage(temporaires,titulaires34,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires34,reclink),verbose = FALSE)


titulaires35<-titulaires[(35*taille+1):(36*taille),]
reclink<-compare.linkage(temporaires,titulaires35,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires35,reclink),verbose = FALSE)


titulaires36<-titulaires[(36*taille+1):(37*taille),]
reclink<-compare.linkage(temporaires,titulaires36,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires36,reclink),verbose = FALSE)


titulaires37<-titulaires[(37*taille+1):(38*taille),]
reclink<-compare.linkage(temporaires,titulaires37,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires37,reclink),verbose = FALSE)


titulaires38<-titulaires[(38*taille+1):(39*taille),]
reclink<-compare.linkage(temporaires,titulaires38,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires38,reclink),verbose = FALSE)


titulaires39<-titulaires[(39*taille+1):(40*taille),]
reclink<-compare.linkage(temporaires,titulaires39,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires39,reclink),verbose = FALSE)


titulaires40<-titulaires[(40*taille+1):(41*taille),]
reclink<-compare.linkage(temporaires,titulaires40,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires40,reclink),verbose = FALSE)


titulaires41<-titulaires[(41*taille+1):(42*taille),]
reclink<-compare.linkage(temporaires,titulaires41,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires41,reclink),verbose = FALSE)


titulaires42<-titulaires[(42*taille+1):(43*taille),]
reclink<-compare.linkage(temporaires,titulaires42,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires42,reclink),verbose = FALSE)


titulaires43<-titulaires[(43*taille+1):(44*taille),]
reclink<-compare.linkage(temporaires,titulaires43,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires43,reclink),verbose = FALSE)


titulaires44<-titulaires[(44*taille+1):(45*taille),]
reclink<-compare.linkage(temporaires,titulaires44,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires44,reclink),verbose = FALSE)


titulaires45<-titulaires[(45*taille+1):(46*taille),]
reclink<-compare.linkage(temporaires,titulaires45,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires45,reclink),verbose = FALSE)


titulaires46<-titulaires[(46*taille+1):(47*taille),]
reclink<-compare.linkage(temporaires,titulaires46,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires46,reclink),verbose = FALSE)


titulaires47<-titulaires[(47*taille+1):(48*taille),]
reclink<-compare.linkage(temporaires,titulaires47,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires47,reclink),verbose = FALSE)


titulaires48<-titulaires[(48*taille+1):(49*taille),]
reclink<-compare.linkage(temporaires,titulaires48,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires48,reclink),verbose = FALSE)


titulaires49<-titulaires[(49*taille+1):(50*taille),]
reclink<-compare.linkage(temporaires,titulaires49,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires49,reclink),verbose = FALSE)


titulaires50<-titulaires[(50*taille+1):(51*taille),]
reclink<-compare.linkage(temporaires,titulaires50,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires50,reclink),verbose = FALSE)


titulaires51<-titulaires[(51*taille+1):(52*taille),]
reclink<-compare.linkage(temporaires,titulaires51,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires51,reclink),verbose = FALSE)


titulaires52<-titulaires[(52*taille+1):(53*taille),]
reclink<-compare.linkage(temporaires,titulaires52,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires52,reclink),verbose = FALSE)


titulaires53<-titulaires[(53*taille+1):(54*taille),]
reclink<-compare.linkage(temporaires,titulaires53,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires53,reclink),verbose = FALSE)


titulaires54<-titulaires[(54*taille+1):(55*taille),]
reclink<-compare.linkage(temporaires,titulaires54,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires54,reclink),verbose = FALSE)


titulaires55<-titulaires[(55*taille+1):(56*taille),]
reclink<-compare.linkage(temporaires,titulaires55,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires55,reclink),verbose = FALSE)


titulaires56<-titulaires[(56*taille+1):(57*taille),]
reclink<-compare.linkage(temporaires,titulaires56,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires56,reclink),verbose = FALSE)


titulaires57<-titulaires[(57*taille+1):(58*taille),]
reclink<-compare.linkage(temporaires,titulaires57,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires57,reclink),verbose = FALSE)


titulaires58<-titulaires[(58*taille+1):(59*taille),]
reclink<-compare.linkage(temporaires,titulaires58,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires58,reclink),verbose = FALSE)


titulaires59<-titulaires[(59*taille+1):(60*taille),]
reclink<-compare.linkage(temporaires,titulaires59,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires59,reclink),verbose = FALSE)


titulaires60<-titulaires[(60*taille+1):(61*taille),]
reclink<-compare.linkage(temporaires,titulaires60,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires60,reclink),verbose = FALSE)


titulaires61<-titulaires[(61*taille+1):(62*taille),]
reclink<-compare.linkage(temporaires,titulaires61,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires61,reclink),verbose = FALSE)


titulaires62<-titulaires[(62*taille+1):(63*taille),]
reclink<-compare.linkage(temporaires,titulaires62,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires62,reclink),verbose = FALSE)


titulaires63<-titulaires[(63*taille+1):(64*taille),]
reclink<-compare.linkage(temporaires,titulaires63,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires63,reclink),verbose = FALSE)


titulaires64<-titulaires[(64*taille+1):(65*taille),]
reclink<-compare.linkage(temporaires,titulaires64,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires64,reclink),verbose = FALSE)


titulaires65<-titulaires[(65*taille+1):(66*taille),]
reclink<-compare.linkage(temporaires,titulaires65,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires65,reclink),verbose = FALSE)


titulaires66<-titulaires[(66*taille+1):(67*taille),]
reclink<-compare.linkage(temporaires,titulaires66,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires66,reclink),verbose = FALSE)


titulaires67<-titulaires[(67*taille+1):(68*taille),]
reclink<-compare.linkage(temporaires,titulaires67,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires67,reclink),verbose = FALSE)


titulaires68<-titulaires[(68*taille+1):(69*taille),]
reclink<-compare.linkage(temporaires,titulaires68,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires68,reclink),verbose = FALSE)


titulaires69<-titulaires[(69*taille+1):(70*taille),]
reclink<-compare.linkage(temporaires,titulaires69,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires69,reclink),verbose = FALSE)


titulaires70<-titulaires[(70*taille+1):(71*taille),]
reclink<-compare.linkage(temporaires,titulaires70,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires70,reclink),verbose = FALSE)


titulaires71<-titulaires[(71*taille+1):(72*taille),]
reclink<-compare.linkage(temporaires,titulaires71,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires71,reclink),verbose = FALSE)


titulaires72<-titulaires[(72*taille+1):(73*taille),]
reclink<-compare.linkage(temporaires,titulaires72,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires72,reclink),verbose = FALSE)


titulaires73<-titulaires[(73*taille+1):(74*taille),]
reclink<-compare.linkage(temporaires,titulaires73,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires73,reclink),verbose = FALSE)


titulaires74<-titulaires[(74*taille+1):(75*taille),]
reclink<-compare.linkage(temporaires,titulaires74,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires74,reclink),verbose = FALSE)


titulaires75<-titulaires[(75*taille+1):(76*taille),]
reclink<-compare.linkage(temporaires,titulaires75,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires75,reclink),verbose = FALSE)


titulaires76<-titulaires[(76*taille+1):(77*taille),]
reclink<-compare.linkage(temporaires,titulaires76,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires76,reclink),verbose = FALSE)


titulaires77<-titulaires[(77*taille+1):(78*taille),]
reclink<-compare.linkage(temporaires,titulaires77,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires77,reclink),verbose = FALSE)


titulaires78<-titulaires[(78*taille+1):(79*taille),]
reclink<-compare.linkage(temporaires,titulaires78,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires78,reclink),verbose = FALSE)


titulaires79<-titulaires[(79*taille+1):(80*taille),]
reclink<-compare.linkage(temporaires,titulaires79,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires79,reclink),verbose = FALSE)


titulaires80<-titulaires[(80*taille+1):(81*taille),]
reclink<-compare.linkage(temporaires,titulaires80,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires80,reclink),verbose = FALSE)


titulaires81<-titulaires[(81*taille+1):(82*taille),]
reclink<-compare.linkage(temporaires,titulaires81,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires81,reclink),verbose = FALSE)


titulaires82<-titulaires[(82*taille+1):(83*taille),]
reclink<-compare.linkage(temporaires,titulaires82,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires82,reclink),verbose = FALSE)


titulaires83<-titulaires[(83*taille+1):(84*taille),]
reclink<-compare.linkage(temporaires,titulaires83,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires83,reclink),verbose = FALSE)


titulaires84<-titulaires[(84*taille+1):(85*taille),]
reclink<-compare.linkage(temporaires,titulaires84,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires84,reclink),verbose = FALSE)


titulaires85<-titulaires[(85*taille+1):(86*taille),]
reclink<-compare.linkage(temporaires,titulaires85,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires85,reclink),verbose = FALSE)


titulaires86<-titulaires[(86*taille+1):(87*taille),]
reclink<-compare.linkage(temporaires,titulaires86,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires86,reclink),verbose = FALSE)


titulaires87<-titulaires[(87*taille+1):(88*taille),]
reclink<-compare.linkage(temporaires,titulaires87,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires87,reclink),verbose = FALSE)


titulaires88<-titulaires[(88*taille+1):(89*taille),]
reclink<-compare.linkage(temporaires,titulaires88,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires88,reclink),verbose = FALSE)


titulaires89<-titulaires[(89*taille+1):(90*taille),]
reclink<-compare.linkage(temporaires,titulaires89,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires89,reclink),verbose = FALSE)


titulaires90<-titulaires[(90*taille+1):(91*taille),]
reclink<-compare.linkage(temporaires,titulaires90,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires90,reclink),verbose = FALSE)


titulaires91<-titulaires[(91*taille+1):(92*taille),]
reclink<-compare.linkage(temporaires,titulaires91,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires91,reclink),verbose = FALSE)


titulaires92<-titulaires[(92*taille+1):(93*taille),]
reclink<-compare.linkage(temporaires,titulaires92,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires92,reclink),verbose = FALSE)


titulaires93<-titulaires[(93*taille+1):(94*taille),]
reclink<-compare.linkage(temporaires,titulaires93,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires93,reclink),verbose = FALSE)


titulaires94<-titulaires[(94*taille+1):(95*taille),]
reclink<-compare.linkage(temporaires,titulaires94,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires94,reclink),verbose = FALSE)


titulaires95<-titulaires[(95*taille+1):(96*taille),]
reclink<-compare.linkage(temporaires,titulaires95,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires95,reclink),verbose = FALSE)


titulaires96<-titulaires[(96*taille+1):(97*taille),]
reclink<-compare.linkage(temporaires,titulaires96,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires96,reclink),verbose = FALSE)


titulaires97<-titulaires[(97*taille+1):(98*taille),]
reclink<-compare.linkage(temporaires,titulaires97,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires97,reclink),verbose = FALSE)


titulaires98<-titulaires[(98*taille+1):(99*taille),]
reclink<-compare.linkage(temporaires,titulaires98,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires98,reclink),verbose = FALSE)


titulaires99<-titulaires[(99*taille+1):(100*taille),]
reclink<-compare.linkage(temporaires,titulaires99,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires99,reclink),verbose = FALSE)


titulaires100<-titulaires[(100*taille+1):(101*taille),]
reclink<-compare.linkage(temporaires,titulaires100,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires100,reclink),verbose = FALSE)


titulaires101<-titulaires[(101*taille+1):(102*taille),]
reclink<-compare.linkage(temporaires,titulaires101,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires101,reclink),verbose = FALSE)


titulaires102<-titulaires[(102*taille+1):(103*taille),]
reclink<-compare.linkage(temporaires,titulaires102,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires102,reclink),verbose = FALSE)


titulaires103<-titulaires[(103*taille+1):(104*taille),]
reclink<-compare.linkage(temporaires,titulaires103,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires103,reclink),verbose = FALSE)


titulaires104<-titulaires[(104*taille+1):(105*taille),]
reclink<-compare.linkage(temporaires,titulaires104,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires104,reclink),verbose = FALSE)


titulaires105<-titulaires[(105*taille+1):(106*taille),]
reclink<-compare.linkage(temporaires,titulaires105,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires105,reclink),verbose = FALSE)


titulaires106<-titulaires[(106*taille+1):(107*taille),]
reclink<-compare.linkage(temporaires,titulaires106,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires106,reclink),verbose = FALSE)


titulaires107<-titulaires[(107*taille+1):(108*taille),]
reclink<-compare.linkage(temporaires,titulaires107,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires107,reclink),verbose = FALSE)


titulaires108<-titulaires[(108*taille+1):(109*taille),]
reclink<-compare.linkage(temporaires,titulaires108,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires108,reclink),verbose = FALSE)


titulaires109<-titulaires[(109*taille+1):(110*taille),]
reclink<-compare.linkage(temporaires,titulaires109,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires109,reclink),verbose = FALSE)


titulaires110<-titulaires[(110*taille+1):(111*taille),]
reclink<-compare.linkage(temporaires,titulaires110,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires110,reclink),verbose = FALSE)


titulaires111<-titulaires[(111*taille+1):(112*taille),]
reclink<-compare.linkage(temporaires,titulaires111,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires111,reclink),verbose = FALSE)


titulaires112<-titulaires[(112*taille+1):(113*taille),]
reclink<-compare.linkage(temporaires,titulaires112,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires112,reclink),verbose = FALSE)


titulaires113<-titulaires[(113*taille+1):(114*taille),]
reclink<-compare.linkage(temporaires,titulaires113,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires113,reclink),verbose = FALSE)


titulaires114<-titulaires[(114*taille+1):(115*taille),]
reclink<-compare.linkage(temporaires,titulaires114,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires114,reclink),verbose = FALSE)


titulaires115<-titulaires[(115*taille+1):(116*taille),]
reclink<-compare.linkage(temporaires,titulaires115,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires115,reclink),verbose = FALSE)


titulaires116<-titulaires[(116*taille+1):(117*taille),]
reclink<-compare.linkage(temporaires,titulaires116,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires116,reclink),verbose = FALSE)


titulaires117<-titulaires[(117*taille+1):(118*taille),]
reclink<-compare.linkage(temporaires,titulaires117,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires117,reclink),verbose = FALSE)


titulaires118<-titulaires[(118*taille+1):(119*taille),]
reclink<-compare.linkage(temporaires,titulaires118,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires118,reclink),verbose = FALSE)


titulaires119<-titulaires[(119*taille+1):(120*taille),]
reclink<-compare.linkage(temporaires,titulaires119,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires119,reclink),verbose = FALSE)


titulaires120<-titulaires[(120*taille+1):(121*taille),]
reclink<-compare.linkage(temporaires,titulaires120,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires120,reclink),verbose = FALSE)


titulaires121<-titulaires[(121*taille+1):(122*taille),]
reclink<-compare.linkage(temporaires,titulaires121,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires121,reclink),verbose = FALSE)


titulaires122<-titulaires[(122*taille+1):(123*taille),]
reclink<-compare.linkage(temporaires,titulaires122,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires122,reclink),verbose = FALSE)


titulaires123<-titulaires[(123*taille+1):(124*taille),]
reclink<-compare.linkage(temporaires,titulaires123,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires123,reclink),verbose = FALSE)


titulaires124<-titulaires[(124*taille+1):(125*taille),]
reclink<-compare.linkage(temporaires,titulaires124,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires124,reclink),verbose = FALSE)


titulaires125<-titulaires[(125*taille+1):(126*taille),]
reclink<-compare.linkage(temporaires,titulaires125,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires125,reclink),verbose = FALSE)


titulaires126<-titulaires[(126*taille+1):(127*taille),]
reclink<-compare.linkage(temporaires,titulaires126,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires126,reclink),verbose = FALSE)


titulaires127<-titulaires[(127*taille+1):(128*taille),]
reclink<-compare.linkage(temporaires,titulaires127,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires127,reclink),verbose = FALSE)


titulaires128<-titulaires[(128*taille+1):(129*taille),]
reclink<-compare.linkage(temporaires,titulaires128,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires128,reclink),verbose = FALSE)


titulaires129<-titulaires[(129*taille+1):(130*taille),]
reclink<-compare.linkage(temporaires,titulaires129,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires129,reclink),verbose = FALSE)


titulaires130<-titulaires[(130*taille+1):(131*taille),]
reclink<-compare.linkage(temporaires,titulaires130,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires130,reclink),verbose = FALSE)


titulaires131<-titulaires[(131*taille+1):(132*taille),]
reclink<-compare.linkage(temporaires,titulaires131,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires131,reclink),verbose = FALSE)


titulaires132<-titulaires[(132*taille+1):(133*taille),]
reclink<-compare.linkage(temporaires,titulaires132,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires132,reclink),verbose = FALSE)


titulaires133<-titulaires[(133*taille+1):(134*taille),]
reclink<-compare.linkage(temporaires,titulaires133,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires133,reclink),verbose = FALSE)


titulaires134<-titulaires[(134*taille+1):(135*taille),]
reclink<-compare.linkage(temporaires,titulaires134,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires134,reclink),verbose = FALSE)


titulaires135<-titulaires[(135*taille+1):(136*taille),]
reclink<-compare.linkage(temporaires,titulaires135,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires135,reclink),verbose = FALSE)


titulaires136<-titulaires[(136*taille+1):(137*taille),]
reclink<-compare.linkage(temporaires,titulaires136,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires136,reclink),verbose = FALSE)


titulaires137<-titulaires[(137*taille+1):(138*taille),]
reclink<-compare.linkage(temporaires,titulaires137,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires137,reclink),verbose = FALSE)


titulaires138<-titulaires[(138*taille+1):(139*taille),]
reclink<-compare.linkage(temporaires,titulaires138,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires138,reclink),verbose = FALSE)


titulaires139<-titulaires[(139*taille+1):(140*taille),]
reclink<-compare.linkage(temporaires,titulaires139,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires139,reclink),verbose = FALSE)


titulaires140<-titulaires[(140*taille+1):(141*taille),]
reclink<-compare.linkage(temporaires,titulaires140,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires140,reclink),verbose = FALSE)


titulaires141<-titulaires[(141*taille+1):(142*taille),]
reclink<-compare.linkage(temporaires,titulaires141,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires141,reclink),verbose = FALSE)


titulaires142<-titulaires[(142*taille+1):(143*taille),]
reclink<-compare.linkage(temporaires,titulaires142,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires142,reclink),verbose = FALSE)


titulaires143<-titulaires[(143*taille+1):(144*taille),]
reclink<-compare.linkage(temporaires,titulaires143,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires143,reclink),verbose = FALSE)


titulaires144<-titulaires[(144*taille+1):(145*taille),]
reclink<-compare.linkage(temporaires,titulaires144,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires144,reclink),verbose = FALSE)


titulaires145<-titulaires[(145*taille+1):(146*taille),]
reclink<-compare.linkage(temporaires,titulaires145,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires145,reclink),verbose = FALSE)


titulaires146<-titulaires[(146*taille+1):(147*taille),]
reclink<-compare.linkage(temporaires,titulaires146,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires146,reclink),verbose = FALSE)


titulaires147<-titulaires[(147*taille+1):(148*taille),]
reclink<-compare.linkage(temporaires,titulaires147,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires147,reclink),verbose = FALSE)


titulaires148<-titulaires[(148*taille+1):(149*taille),]
reclink<-compare.linkage(temporaires,titulaires148,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires148,reclink),verbose = FALSE)


titulaires149<-titulaires[(149*taille+1):(150*taille),]
reclink<-compare.linkage(temporaires,titulaires149,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires149,reclink),verbose = FALSE)


titulaires150<-titulaires[(150*taille+1):(151*taille),]
reclink<-compare.linkage(temporaires,titulaires150,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires150,reclink),verbose = FALSE)


titulaires151<-titulaires[(151*taille+1):(152*taille),]
reclink<-compare.linkage(temporaires,titulaires151,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires151,reclink),verbose = FALSE)


titulaires152<-titulaires[(152*taille+1):(153*taille),]
reclink<-compare.linkage(temporaires,titulaires152,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires152,reclink),verbose = FALSE)


titulaires153<-titulaires[(153*taille+1):(154*taille),]
reclink<-compare.linkage(temporaires,titulaires153,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires153,reclink),verbose = FALSE)


titulaires154<-titulaires[(154*taille+1):(155*taille),]
reclink<-compare.linkage(temporaires,titulaires154,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires154,reclink),verbose = FALSE)


titulaires155<-titulaires[(155*taille+1):(156*taille),]
reclink<-compare.linkage(temporaires,titulaires155,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires155,reclink),verbose = FALSE)


titulaires156<-titulaires[(156*taille+1):(157*taille),]
reclink<-compare.linkage(temporaires,titulaires156,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires156,reclink),verbose = FALSE)


titulaires157<-titulaires[(157*taille+1):(158*taille),]
reclink<-compare.linkage(temporaires,titulaires157,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires157,reclink),verbose = FALSE)


titulaires158<-titulaires[(158*taille+1):(159*taille),]
reclink<-compare.linkage(temporaires,titulaires158,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires158,reclink),verbose = FALSE)


titulaires159<-titulaires[(159*taille+1):(160*taille),]
reclink<-compare.linkage(temporaires,titulaires159,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires159,reclink),verbose = FALSE)


titulaires160<-titulaires[(160*taille+1):(161*taille),]
reclink<-compare.linkage(temporaires,titulaires160,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires160,reclink),verbose = FALSE)


titulaires161<-titulaires[(161*taille+1):(162*taille),]
reclink<-compare.linkage(temporaires,titulaires161,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires161,reclink),verbose = FALSE)


titulaires162<-titulaires[(162*taille+1):(163*taille),]
reclink<-compare.linkage(temporaires,titulaires162,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires162,reclink),verbose = FALSE)


titulaires163<-titulaires[(163*taille+1):(164*taille),]
reclink<-compare.linkage(temporaires,titulaires163,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires163,reclink),verbose = FALSE)


titulaires164<-titulaires[(164*taille+1):(165*taille),]
reclink<-compare.linkage(temporaires,titulaires164,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires164,reclink),verbose = FALSE)


titulaires165<-titulaires[(165*taille+1):(166*taille),]
reclink<-compare.linkage(temporaires,titulaires165,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires165,reclink),verbose = FALSE)


titulaires166<-titulaires[(166*taille+1):(167*taille),]
reclink<-compare.linkage(temporaires,titulaires166,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires166,reclink),verbose = FALSE)


titulaires167<-titulaires[(167*taille+1):(168*taille),]
reclink<-compare.linkage(temporaires,titulaires167,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires167,reclink),verbose = FALSE)


titulaires168<-titulaires[(168*taille+1):(169*taille),]
reclink<-compare.linkage(temporaires,titulaires168,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires168,reclink),verbose = FALSE)


titulaires169<-titulaires[(169*taille+1):(170*taille),]
reclink<-compare.linkage(temporaires,titulaires169,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires169,reclink),verbose = FALSE)


titulaires170<-titulaires[(170*taille+1):(171*taille),]
reclink<-compare.linkage(temporaires,titulaires170,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires170,reclink),verbose = FALSE)


titulaires171<-titulaires[(171*taille+1):(172*taille),]
reclink<-compare.linkage(temporaires,titulaires171,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires171,reclink),verbose = FALSE)


titulaires172<-titulaires[(172*taille+1):(173*taille),]
reclink<-compare.linkage(temporaires,titulaires172,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires172,reclink),verbose = FALSE)


titulaires173<-titulaires[(173*taille+1):(174*taille),]
reclink<-compare.linkage(temporaires,titulaires173,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires173,reclink),verbose = FALSE)


titulaires174<-titulaires[(174*taille+1):(175*taille),]
reclink<-compare.linkage(temporaires,titulaires174,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires174,reclink),verbose = FALSE)


titulaires175<-titulaires[(175*taille+1):(176*taille),]
reclink<-compare.linkage(temporaires,titulaires175,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires175,reclink),verbose = FALSE)


titulaires176<-titulaires[(176*taille+1):(177*taille),]
reclink<-compare.linkage(temporaires,titulaires176,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires176,reclink),verbose = FALSE)


titulaires177<-titulaires[(177*taille+1):(178*taille),]
reclink<-compare.linkage(temporaires,titulaires177,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires177,reclink),verbose = FALSE)


titulaires178<-titulaires[(178*taille+1):(179*taille),]
reclink<-compare.linkage(temporaires,titulaires178,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires178,reclink),verbose = FALSE)


titulaires179<-titulaires[(179*taille+1):(180*taille),]
reclink<-compare.linkage(temporaires,titulaires179,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires179,reclink),verbose = FALSE)


titulaires180<-titulaires[(180*taille+1):(181*taille),]
reclink<-compare.linkage(temporaires,titulaires180,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires180,reclink),verbose = FALSE)


titulaires181<-titulaires[(181*taille+1):(182*taille),]
reclink<-compare.linkage(temporaires,titulaires181,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires181,reclink),verbose = FALSE)


titulaires182<-titulaires[(182*taille+1):(183*taille),]
reclink<-compare.linkage(temporaires,titulaires182,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires182,reclink),verbose = FALSE)


titulaires183<-titulaires[(183*taille+1):(184*taille),]
reclink<-compare.linkage(temporaires,titulaires183,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires183,reclink),verbose = FALSE)


titulaires184<-titulaires[(184*taille+1):(185*taille),]
reclink<-compare.linkage(temporaires,titulaires184,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires184,reclink),verbose = FALSE)


titulaires185<-titulaires[(185*taille+1):(186*taille),]
reclink<-compare.linkage(temporaires,titulaires185,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires185,reclink),verbose = FALSE)


titulaires186<-titulaires[(186*taille+1):(187*taille),]
reclink<-compare.linkage(temporaires,titulaires186,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires186,reclink),verbose = FALSE)


titulaires187<-titulaires[(187*taille+1):(188*taille),]
reclink<-compare.linkage(temporaires,titulaires187,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires187,reclink),verbose = FALSE)


titulaires188<-titulaires[(188*taille+1):(189*taille),]
reclink<-compare.linkage(temporaires,titulaires188,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires188,reclink),verbose = FALSE)


titulaires189<-titulaires[(189*taille+1):(190*taille),]
reclink<-compare.linkage(temporaires,titulaires189,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires189,reclink),verbose = FALSE)


titulaires190<-titulaires[(190*taille+1):(191*taille),]
reclink<-compare.linkage(temporaires,titulaires190,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires190,reclink),verbose = FALSE)


titulaires191<-titulaires[(191*taille+1):(192*taille),]
reclink<-compare.linkage(temporaires,titulaires191,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires191,reclink),verbose = FALSE)


titulaires192<-titulaires[(192*taille+1):(193*taille),]
reclink<-compare.linkage(temporaires,titulaires192,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires192,reclink),verbose = FALSE)


titulaires193<-titulaires[(193*taille+1):(194*taille),]
reclink<-compare.linkage(temporaires,titulaires193,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires193,reclink),verbose = FALSE)


titulaires194<-titulaires[(194*taille+1):(195*taille),]
reclink<-compare.linkage(temporaires,titulaires194,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires194,reclink),verbose = FALSE)


titulaires195<-titulaires[(195*taille+1):(196*taille),]
reclink<-compare.linkage(temporaires,titulaires195,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires195,reclink),verbose = FALSE)


titulaires196<-titulaires[(196*taille+1):(197*taille),]
reclink<-compare.linkage(temporaires,titulaires196,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires196,reclink),verbose = FALSE)


titulaires197<-titulaires[(197*taille+1):(198*taille),]
reclink<-compare.linkage(temporaires,titulaires197,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires197,reclink),verbose = FALSE)


titulaires198<-titulaires[(198*taille+1):(199*taille),]
reclink<-compare.linkage(temporaires,titulaires198,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires198,reclink),verbose = FALSE)


titulaires199<-titulaires[(199*taille+1):(200*taille),]
reclink<-compare.linkage(temporaires,titulaires199,strcmpfun = jarowinkler,strcmp = c(2,3,4,5,7,8,9,10,12),blockfld = c(14,15), exclude = c(1,6,11,13,14,15))
reclink<-fsWeights(reclink,m=c(0.9,0.8,0.9,0.8,0.5,0.5,0.4,0.4,0.3),u=reclink$frequencies)
reclink<-fsClassify(reclink,threshold.upper=35,threshold.lower=20)
links<-rbind(links,getPairs(reclink,single.rows = TRUE, show="links"))
possible_links<-rbind(possible_links,getPairs(reclink,single.rows = TRUE, show="possible"))
poids[(paires+1):(paires+length(reclink$Wdata))]<-reclink$Wdata
paires<-as.numeric(paires+nrow(reclink$pairs))
gc(rm(titulaires199,reclink),verbose = FALSE)


# Résultats ---------------------------------------------------------------
#observation des poids
poids<-sub.big.matrix(poids,firstRow = 1,lastRow = paires) #la matrice poids était trop grande
poidsplus<-poids[mwhich(x=poids, cols = 1, vals =0,comps='ge' ,op="AND")] #ne garder que les valeurs supérieures à 0
hist(poidsplus,ylab="Effectifs", xlab = "Poids", ylim=c(0,3000), labels=TRUE, main="Distribution des poids de comparaison\nFormulaires", col="#F59F1E", col.main="#F59F1E")
Hmisc::minor.tick(nx=5,ny=0)

#Mise en commun des noms de colonnes
colnames(links)<-c("ID.temp","Muna.temp","NomUsuel.temp","NomFamille.temp","Prenom.temp","DateNaissance.temp","SS.temp","TelFixe.temp","TelPort.temp","Mail.temp","Rue.temp","CRue.temp","CodePostal.temp","Ville.temp","AnneeNaiss.temp","Region.temp","Id.tit","Muna.tit","NomUsuel.tit","NomFamille.tit","Prenom.tit","DateNaissance.tit","SS.tit","TelFixe.tit","TelPort.tit","Mail.tit","Rue.tit","CRue.tit","CodePostal.tit","Ville.tit","AnneeNaiss.tit","Region.tit","Poids")
colnames(possible_links)<-c("ID.temp","Muna.temp","NomUsuel.temp","NomFamille.temp","Prenom.temp","DateNaissance.temp","SS.temp","TelFixe.temp","TelPort.temp","Mail.temp","Rue.temp","CRue.temp","CodePostal.temp","Ville.temp","AnneeNaiss.temp","Region.temp","Id.tit","Muna.tit","NomUsuel.tit","NomFamille.tit","Prenom.tit","DateNaissance.tit","SS.tit","TelFixe.tit","TelPort.tit","Mail.tit","Rue.tit","CRue.tit","CodePostal.tit","Ville.tit","AnneeNaiss.tit","Region.tit","Poids")
colnames(match)<-c("Muna.temp","NomUsuel.temp","NomFamille.temp","Prenom.temp","DateNaissance.temp","AnneeNaiss.temp","SS.temp","TelFixe.temp","TelPort.temp","Mail.temp","Rue.temp","CRue.temp","CodePostal.temp","Ville.temp","Region.temp","Muna.tit","NomUsuel.tit","NomFamille.tit","Prenom.tit","DateNaissance.tit","AnneeNaiss.tit","SS.tit","TelFixe.tit","TelPort.tit","Mail.tit","Rue.tit","CRue.tit","CodePostal.tit","Ville.tit","Region.tit","Methode")
#Ordonner les résultats
match<-arrange(match, desc(Methode), NomFamille.temp, Prenom.temp) %>%
  select(-c(AnneeNaiss.temp,AnneeNaiss.tit,Region.temp,Region.tit))
links<-arrange(links, desc(Poids), NomFamille.temp, Prenom.temp) %>% 
  select(-c(AnneeNaiss.temp,AnneeNaiss.tit,Region.temp,Region.tit))
possible_links<-arrange(possible_links, desc(Poids), NomFamille.temp, Prenom.temp) %>% 
  select(-c(AnneeNaiss.temp,AnneeNaiss.tit,Region.temp,Region.tit))

#Ecriture des résultats sur un document excel
xl<-createWorkbook() #crée un objet pour excel
addWorksheet(xl,"Exact",tabColour = "green",header = c("Rattachement Formulaires","Liens par valeurs exactes","")) #crée un onglet dans cet objet excel
writeDataTable(xl,"Exact",match,headerStyle = createStyle(textRotation = 45)) #assigne une table à l'onglet
addWorksheet(xl,"Liens Sûrs",tabColour = "orange",header = c("Rattachement Formulaires","Record Linkage","Liens"))
writeDataTable(xl,"Liens Sûrs",links, colNames=TRUE, headerStyle = createStyle(textRotation = 45))
addWorksheet(xl,"Liens Possibles",tabColour = "red",header = c("Rattachement Formulaires","Record Linkage","Liens possibles"))
writeDataTable(xl,"Liens Possibles",possible_links,headerStyle = createStyle(textRotation = 45))
Sys.setenv(R_ZIPCMD= "C:/RBuildTools/3.3/bin/zip")   
saveWorkbook(xl, "liens_Formulaires.xlsx", overwrite = TRUE) #enregistre l'objet excel dans un fichier
