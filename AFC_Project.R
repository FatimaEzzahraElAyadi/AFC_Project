#installation des packages
install.packages("ade4")
install.packages("ca")
install.packages("MASS")
install.packages("ExPosition")
install.packages("FactoMineR")
install.packages("factoextra")
#chargement des packages
library("ca")
library("MASS")
library("ExPosition")

#installation des packages
install.packages("FactoMineR")
install.packages("factoextra")
#chargement des packages
library("FactoMineR")
library("factoextra")


library("corrplot")
#Se mettre sur le répertoire de travail
setwd("E:/FatistaBureau/INSEA/P2/ADD/data")
#Créer le tableau de contingence
read.csv("Touristes.csv", sep=";", dec="," , header=TRUE, check.names = FALSE, fileEncoding = "latin1")

#Charger les données individuelles sous format csv
Touristes=read.table("Touristes.csv", sep=";", dec="," , row.names=1 , 
                 header=TRUE, check.names = FALSE, fileEncoding = "latin1")
help("read.table")

#Vérifier la nature des variables
summary(Touristes)

#évaluer s'il existe une dépendance entre es lignes et les colonnes
chisq <- chisq.test (Touristes)
chisq

#Convertir les données en tant que table avec as.table
dt=as.table(as.matrix(Touristes))
#Visualiser le tableau de contingence en diagramme à bulle 
install.packages("gplots")
library("gplots")
balloonplot(t (dt), main = "housetasks", xlab = "", ylab = "",
            label = FALSE, show.margins = FALSE)

help("ca")

#Exécuter l'AFC 
AFC=CA(Touristes, graph=TRUE)
AFC

ca(Touristes, graph=TRUE)
CA(Touristes, graph=TRUE)
#Visualiser les résultats avec fviz_ca_biplot
# Biplot asymétrique où les profils des colonnes sont représentés dans l'espace des lignes ou vice versa grâce aux relations de transitions.
# Biplot asymétrique permet d'interpréter la distance entre les points colonnes et les points lignes.
# map = "rowprincipal" (ou "colprincipal") : Les lignes sont en coordonnées principales et les colonnes en coordonnées standard (ou vice versa). 
# arrow : Pour afficher les flèches des lignes et des colonnes
fviz_ca_biplot(AFC,arrow = c(TRUE, TRUE), map = "rowprincipal", rapel=TRUE)
help("fviz_ca_biplot")
#Afficher le résultat de l'AFC avec summary
summary(AFC)
#Valeurs propres et variances expliquées avec  get_eigenvalue 
eig.val=get_eigenvalue(AFC)

#Valeurs propres
get_eigenvalue(AFC)

help("get_eigenvalue")

#Tracer le graphique des valeurs propres avec fviz_screeplot 
fviz_screeplot(AFC, addlabels=TRUE, ylim=c(0,80))

###################################

#Tracer le graphique avec moyenne
fviz_screeplot (AFC, addlabels=TRUE, ylim=c(0,80)) + 
  geom_hline (yintercept = 12.5, linetype = 2, color = "red")

#Déterminer la valeur propre moyenne 
VPM=max(1/(nrow(Touristes)-1), 1/(ncol(Touristes)-1))


help("max")


#Tracer legraphique des valeurs propres p avec valeur propre moyenne
geom_hline(yintercept = VPM, linetype=4, color="red")
#Extraction des résultats pour les lignes avec get_ca_row
row=get_ca_row(AFC)
get_ca_row(AFC)
#Lecture des résultats pour les lignes 
head(row$coord)
#Coordonnées
head(row$cos2)
#Cos2: qualité de représentation
head(row$contrib)

fviz_ca_biplot(AFC, repel = TRUE)

#Graphique des points 
fviz_ca_biplot(AFC, map="colgreen", arrow=c(TRUE,FALSE), repel = TRUE)

get_ca_row(AFC)

help("fviz_ca_row")
fviz_ca_row(AFC, repel = TRUE)
#Colorer le graphique des points lignes en fonction de la qualité de représentation 
#variables à  valeur relativement faible de cos2 en blanc
#variables à  valeur moyenne de cos2 en bleu
#variables à  valeur élevée de cos2 en rouge
fviz_ca_row(AFC, col.row = "coord", gradient.cols=c("#00AFBB","#E78800","#FC4E07"))
fviz_ca_row(AFC, col.row = "cos2", gradient.cols=c("#00AFBB","#E78800","#FC4E07"))
corrplot(row$contrib,is.corr = FALSE,method = "shade")
fviz_ca_row(AFC, col.row = "contrib", gradient.cols=c("#00AFBB","#E78800","#FC4E07"),repel = TRUE)
#Diagramme des contributions des points lignes sur toutes les dimensions
corrplot(row$contrib,is.corr = FALSE,method = "shade")
library(Factoshiny)
#Réaliser l'AFC avec une interface graphique 
Factoshiny(Touristes)