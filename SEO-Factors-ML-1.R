#
#
#########################################################################################################
#Premiere partie - détermination de facteur SEO à partir des données Yooda et quelques enrichissements
#########################################################################################################
#### Chargement des bibliothèques utiles ##########################################
#Installer une fois 
#install.packages("httr") #à éxecuter une fois - déjà installé chez moi  
#install.packages("jsonlite") #une fois
#install.packages("urltools") #une fois
#install.packages("dplyr") #une fois
#install.packages("openssl")#une fois
#install.packages("stringr") #une fois
#install.packages("stringi") #une fois
#install.packages("pROC") #une fois
#install.packages("caret") #une fois
#install.packages("naivebayes") #une fois
#install.packages("randomforest") #une fois
#install.packages("ranger") #une fois
#install.packages("vtreat") #une fois
#install.packages("magrittr") #une fois
#install.packages("xgboost") #une fois
#Charger les bibliothèques
library(httr) #package utile pour récupérer des données sur le Web. #pour GET, content ...
library(jsonlite) #pcakage pour travailler avec les données au format JSON
library(urltools)   #pour url_parse
library(dplyr) #pour mutate
library(openssl)   #pour downloadd_ssl_cert
library(stringr) #pour str_sub
library(stringi) #pour stri_count_coll
library(pROC) #pour ROC et AUC
library(caret) #pour varImp dans glm
library(naivebayes) #métode naive bayes
library(randomForest) #méthode Random Forest 1 avec randomForest
library(ranger) #méthode Random Forest 2 avec ranger
library(vtreat) #pour retraitement préalable pour XGBoost
library(magrittr) #pour le "pipe" %>%
library(xgboost) #pour XGBoost
###################################################################################

#
#
##### Votre Clé d'API
MyAPIKey <- "xxxxxxxxxxxxxxxxxxxxxxxxxxx"  #ICI indiquer votre clé d'API fournie par Yooda.


#Verification de mes crédits 
MyApiYoodaURLCredits <- paste("https://api.yooda.com/subscriptions/credits?apikey=", MyAPIKey, sep ="")
MyApiYoodaURLCredits
responseCredits <- GET(url = MyApiYoodaURLCredits) #Attention le test de crédits mange 1 crédit :-(
httr::content(responseCredits)$content   #Affiche les crédits restants   

#
#
MyDomains <- read.csv2(file="domains.csv", stringsAsFactors=FALSE)
#str(MyDomains) #verif
#nrow(MyDomains) #verif
#MyDomains[1,1] #verif



###########################################################################################
######  Recuperation des pages/mots-clés/position dans Yooda  - max 5000 pages par Site.
###########################################################################################
#Sauvegarde dans un sous répertoire Yooda au fur et à mesure si plantage. Création du répertoire
DIR<-getwd() #Répertoire courant
foldername<-"Yooda"
path<-paste(DIR,"/", foldername ,sep = "")
if (!file.exists(path))  dir.create(path, recursive = TRUE, mode = "0777") 


YoodaErrors <- c(400, 401, 402, 403, 404, 429, 500)  #pour repérer les erreurs Yooda



for (i in 1:nrow(MyDomains)) {
###########################################################################################
######  Recuperation du domaine / sous domaine
###########################################################################################
MyDomain <- MyDomains[i,1]  #Domaine étudié 
print(MyDomain)

###############################################
#Récupération de l'id du site 
MyApiYoodaURLId <- paste("https://api.yooda.com/insight/domains/", MyDomain, "?apikey=", MyAPIKey, sep ="")
MyApiYoodaURLId  #Vérification de l'url
responseURL <- GET(url = MyApiYoodaURLId)  #Appel API Yooda.
if (!(responseURL$status_code %in% YoodaErrors)) {
  #str(responseURL) #verif
  MyDomainId <- httr::content(responseURL)$content$id  # Recupération de l'Id de domaine ou sous domaine
  MyDomainId  #Verification
  #Récupération des mots clés:  #GET /insight/domains/{domain_id}/keywords 
  MyApiYoodaURLKeywords  <- paste("https://api.yooda.com/insight/domains/", MyDomainId, "/keywords?apikey=",  MyAPIKey, sep ="")
  MyApiYoodaURLKeywords #Vérification de l'URL de l'API
  responseKeywords <- GET(url = MyApiYoodaURLKeywords)  #Appel API Yooda
  #httr::content(responseKeywords) #Verif .
  http_type(responseKeywords) #Verif type reçu
  #Récupération des données à partir du format JSON de la réponse de l'API
  DataKeywords  <- fromJSON(httr::content(responseKeywords, as = "text"), flatten=TRUE)$content$items_list
  #on complète par le nom de domaine (sera utile quand on regroupera tous les fichiers et pour tester le ssl)
  DataKeywords$domain <- MyDomain
  #str(DataKeywords) #Vérifions que l'on a bien un Data Frame
  ##### On sauvegarde au fur et à mesure au cas ou il y ait un plantage
  MyFileName <- paste("Domain-", MyDomain, ".csv", sep="") #on ajoute un préfixe pour faciliter la lecture par la suite
  filepath<-paste(path,"/",MyFileName, sep = "")
  write.csv2(DataKeywords, file = filepath)  #Ecriture du fichier .csv avec séparateur ";"  
  } #/pas d'erreur YOODA

} #/for  fin de la récupération des données dans l'API De YOODA 

#
#
###############################################################################
###  Je regroupe tous les fichiers dans un seul data.frame et un seul fichier
###############################################################################
#lecture des fichiers de positionnements par domaine
domainFiles <- paste0(path,"/",list.files(path = path, pattern = "Domain-.*\\.csv$"))
AllDomainFiles <- lapply(domainFiles,function(i){
read.csv(i, check.names=FALSE, header=TRUE, sep=";", quote="\"")
})

#class(AllDomainFiles) #C'est une liste 
#str(AllDomainFiles) #verif
AllDataKeywords <- do.call(rbind, AllDomainFiles) #transformation en data.frame
rm(AllDomainFiles) #pour faire de la place mémoire
str(AllDataKeywords) #verif
names(AllDataKeywords)[1] <- "obs_domain_id"  #il n'y avait pas de nom à l'id : attention il s'agit de l'id d'une observation pour un domaine (peut servir par la suite)


#
#
######## Variables à expliquer 
levels(as.factor(AllDataKeywords$position)) #combien de niveau de position ? ici 13
#Variable à tester 
#est-ce Position 1 ??
AllDataKeywords <- mutate(AllDataKeywords, istop1pos = ifelse(position==1, TRUE, FALSE)  )
#est-ce dans le Top 3 des positions ?
AllDataKeywords <- mutate(AllDataKeywords, istop3pos = ifelse(position<=3, TRUE, FALSE)  )

#
#
######## création  de Variables explicatives facteurs SEO ???  ici en fonction de l'URL et du domaine
#Ajout Keyword in Domain et Keyword in URL
AllDataKeywords <- mutate(AllDataKeywords, kwindomain = stringi::stri_count_coll(domain, keyword.keyword))
AllDataKeywords <- mutate(AllDataKeywords, kwinurl = stringi::stri_count_coll(url, keyword.keyword)-kwindomain)

#Ajout ishttps
AllDataKeywords$ishttps <- 0
AllDataKeywords$ishttps[which(grepl("https",AllDataKeywords$url))] <- 1
#str(AllDataKeywords) #Verif

#Test certificat https/ssl à validation étendue (EV)
#on va lire uniquement une fois par domaine :
#création d'une dataframe avec les noms de domaines uniques
indexHttps <- data.frame(domain = unique(AllDataKeywords$domain) , isSSLEV = 0 )
#####################################################################
# Detection d'Extended Validation dans SSL
# based on http://giantdorks.org/alain/shell-script-to-check-ssl-certificate-info-like-expiration-date-and-subject/
# et ovh https://github.com/ovh/summit2016-RankingPredict/blob/master/step9_getInfoSSL.R
detectEV <- function(domain) {
  try({  
    chain <- download_ssl_cert(domain, 443)
    issuer <- as.list(chain[[1]])$issuer
    #print(issuer)
    if (grepl("EV", issuer) || grepl("Extended Validation", issuer) )
      return(TRUE)
  })
 return(FALSE)
}

for (i in 1:nrow(indexHttps)) {
  #host <- parse_url(indexHttps[i, "domain"])$path
  #print(host)
  indexHttps[i, "isSSLEV"] <- detectEV(as.character(indexHttps[i, "domain"]))
}

#on récupere isSSLEV dans AllDatakeywords par un merge.
AllDataKeywords <- merge(AllDataKeywords, indexHttps, by = "domain")

#Longueur de l'URL
AllDataKeywords <- mutate(AllDataKeywords, urlnchar = nchar(as.character(url)))
#nombre de slash :  peut correspondre à un "niveau" de page dans le site
AllDataKeywords <- mutate(AllDataKeywords, urlslashcount = str_count(as.character(url), "/"))

str(AllDataKeywords) #verif
#on sauvegarde le fichier de positions pour utilisations ultérieures.
write.csv2(AllDataKeywords, file = "AllDataKeywords.csv", row.names = FALSE)  #ecriture avec sep ";" sans numéro de ligne.

#
#
#############################################################################
### Machine Learning sur les données intéressantes
#############################################################################
############################################################################################
####### Pour ceux qui démarrent  d'ici on va récupérer les données du ficheir AllDataKeywords.csv
AllDataKeywords <- read.csv2(file = "AllDataKeywords.csv") 

#############################################################################
### Creation du fichier de données à tester, de train et de test 
#############################################################################
str(AllDataKeywords)  #verif des donnnées disponibles.

#Selection des variables.
#on garde les infos en rapport avec l'url
Urlcoltokeep <- c("istop3pos", "kwindomain", "kwinurl", "ishttps", "isSSLEV", "urlnchar", "urlslashcount")
UrlDataKeywords <- AllDataKeywords[, Urlcoltokeep]  #Sélection des variables
#str(UrlDataKeywords) #verif

#Decoupage en train et test 
## 70% of the sample size
smp_size <- floor(0.70 * nrow(UrlDataKeywords))
## set the seed to make your partition reproductible
set.seed(12345)
train_ind <- sample(seq_len(nrow(UrlDataKeywords)), size = smp_size)
train <- UrlDataKeywords[train_ind, ]
test <- UrlDataKeywords[-train_ind, ]
str(train) #verif
str(test) #verif

#
#
#######################################################################################
# Logistic Regression Model 
########################################################################################
#Entrainement du Modèle
glmmod <- glm(I(istop3pos==TRUE)~., data=train, family="binomial")
#Predictions 
pred.glmmod <- predict(object = glmmod, newdata=test, type="response" )
pred.glmmod.logi <- ifelse(pred.glmmod <0.5, 0,1)  #transformation en 0 ou 1 pour matrice de confusion.
(Confusion <- table(pred.glmmod.logi , as.numeric(test$istop3pos))) #matrice de confusion
# En pourcentages:
(ConfusionPerCent <- round(prop.table(Confusion), 4))
#soit un taux de bien classé de 0.5667
ConfusionPerCent[1,1]+ConfusionPerCent[2,2]
#ou bien 
mean(pred.glmmod.logi == as.numeric(test$istop3pos))  #0.5666968 mouais pas terrible
#ROC AUC 
ROC <- roc(test$istop3pos, pred.glmmod) #
AUC <- auc(ROC) #ici Area under the curve: 0.5876
# Plot the ROC curve 
#courbe ROC : Receiver Operating Characteristic (caractéristique de fonctionnement du récepteur)
#teste la qualité du modèle.
plot(ROC, col = "blue")
text(0.5,0.5,paste("AUC = ",format(AUC, digits=5, scientific=FALSE)))
text(0.6, 1, "ROC-AUC Modèle Régression Logistique", col="red")
#importance des variables avec logistic regression model
varImp(glmmod) 

#
#
#######################################################################################
# Naive Bayes Model
########################################################################################
#Modèle naive Bayes
nbmod <- naive_bayes(istop3pos~., data = train)
#Predictions
pred.nbmod <- predict(object = nbmod, newdata = test)
#Matrice de confusion
table(pred.nbmod  , test$istop3pos)
mean(pred.nbmod  == test$istop3pos) #0.5494257 presque équivalent à glm 
#ROC et AUC
ROC <- roc(test$istop3pos, as.numeric(pred.nbmod))
AUC <- auc(ROC) #ici auc  Area under the curve: 0.5604  pas terrible < glm
# Plot the ROC curve
plot(ROC, col = "blue")
text(0.5,0.5,paste("AUC = ",format(AUC, digits=5, scientific=FALSE))) 
text(0.6, 1, "ROC-AUC Modèle Naïve Bayes", col="red", cex=0.7)

#
#
#######################################################################################
# RanDom Forest avec la library "randomForest"
########################################################################################
gc() #vider la mémoire (ça mange beaucoup)
#Modèle randomForest
rfmod <- randomForest(as.factor(istop3pos)~., data=train, importance=TRUE)
class(rfmod)
#Predictions
pred.rfmod <- predict(object = rfmod, newdata = test, type="response")
#Matrice de confusion
table(pred.rfmod , test$istop3pos)
mean(pred.rfmod == test$istop3pos) #0.5813223 mieux  que glm 
#ROC et AUC
ROC <- roc(test$istop3pos, as.numeric(pred.rfmod))
AUC <- auc(ROC) #ici auc AArea under the curve: 0.5843 mieux  que glm
# Plot the ROC curve
plot(ROC, col = "blue")
text(0.5,0.5,paste("AUC = ",format(AUC, digits=5, scientific=FALSE))) 
text(0.6, 1, "ROC-AUC Modèle Random Forest randomForest", col="red", cex=0.7)
#Importance des variables
rfmod$importance
varImpPlot(rfmod)

#
#
#######################################################################################
# Random Forest avec la library "ranger"
########################################################################################
gc() #vider la mémoire (ça mange beaucoup)
#Entrainement du Modèle
rangermod <- ranger(as.factor(istop3pos)~., data=train, importance="impurity")
#Prédictions 
pred.rangermod <- predict(object = rangermod, data = test, type="response")$predictions
#Matrice de confusion
table(pred.rangermod  , test$istop3pos)
mean(pred.rangermod  == test$istop3pos) #0.5830645 ~ randomForest !!!!!
#ROC et AUC
ROC <- roc(test$istop3pos, as.numeric(pred.rangermod ))
AUC <- auc(ROC) #ici auc Area under the curve: 0.5867 ~ randomForest
# Plot the ROC curve
plot(ROC, col = "blue")
text(0.5,0.5,paste("AUC = ",format(AUC, digits=5, scientific=FALSE)))
text(0.6, 1, "ROC-AUC Modèle Random Forest ranger", col="red", cex=0.7)
#Importance des variables
rangermod$variable.importance


#
#
#######################################################################################
# XGBoost sur istop3pos
########################################################################################
# Variable à expliquer 
(outcome <- "istop3pos")
# Variables explicatives
(vars <- c("kwindomain", "kwinurl", "ishttps", "isSSLEV", "urlnchar", "urlslashcount"))

#Traitements préalables des données pour être utilisées par XGBoost
# Création d'un "plan de traitement" à partir de train (données d'entrainement)
# "one hot encoding"
treatplan <- designTreatmentsZ(train, vars, verbose = FALSE)
#str(treatplan)
# On récupère les variables  "clean" et "lev" du scoreFrame : treatplan$scoreframe
(newvars <- treatplan %>%
    use_series(scoreFrame) %>%        
    filter(code %in% c("clean","lev")) %>%  # get the rows you care about
    use_series(varName))           # get the varName column

# Preparation des données d'entrainement  à partir du plan de traitement créé précédemment
train.treat <- prepare(treatmentplan = treatplan, dframe = train ,  varRestriction = newvars)
# Preparation des données de test  à partir du plan de traitement créé précédemment
test.treat <- prepare(treatmentplan = treatplan, dframe = test,  varRestriction = newvars)

# on commence par faire tourner  xgb.cv   pour déterminer le nombre d'arbres optimal.
cv <- xgb.cv(data = as.matrix(train.treat), 
             label = train$istop3pos,
             nrounds = 100,
             nfold = 5,
             objective = "binary:logistic",
             eta = 0.3,
             max_depth = 6,
             early_stopping_rounds = 10,
             verbose = 1    # silent
)
#str(cv)
#pour regression linéaire objective = "reg:linear",
#pour binaire objective = "binary:logistic", 
# Get the evaluation log 
elog <- cv$evaluation_log
str(elog)
# Determination du nombre d'arbres qui minimise l'erreur sur le jeu de train et le jeu de test
(Twotreesvalue <- elog %>% 
    summarize(ntrees.train = which.min(train_error_mean),   # find the index of min(train_rmse_mean)
              ntrees.test  = which.min(test_error_mean)) )   # find the index of min(test_rmse_mean)

#on prend le plus petit des 2 
ntrees = min(Twotreesvalue$ntrees.train, Twotreesvalue$ntrees.test)
# The number of trees to use, as determined by xgb.cv
ntrees

# Run xgboost
xgbmod <- xgboost(data = as.matrix(train.treat), # training data as matrix
                           label = train$istop3pos,  # column of outcomes
                           nrounds = ntrees,       # number of trees to build
                           objective = "binary:logistic", # objective
                           eta = 0.3,
                           depth = 6,
                           verbose = 1  # affichage ou non
)

#Predictions
pred.xgbmod <-predict(xgbmod, as.matrix(test.treat))

#Matrice de Confusion
table(round(pred.xgbmod) , test$istop3pos)
mean(round(pred.xgbmod) == test$istop3pos) ###0.609369 mieux que randomForest, ranger et glm

#ROC et AUC
ROC <- roc(test$istop3pos, pred.xgbmod)
AUC <- auc(ROC) #ici auc Area under the curve: 0.661  mieux mieux que randomForest, ranger et glm !

# Plot the ROC curve
plot(ROC, col = "blue")
text(0.5,0.5,paste("AUC = ",format(AUC, digits=5, scientific=FALSE))) 
text(0.6, 1, "ROC-AUC Modèle XGBoost", col="red", cex=0.7)

#importance  #c'est le gain qui nous intéresse !
(importance <- xgb.importance(feature_names = colnames(x = train.treat), model = xgbmod))

