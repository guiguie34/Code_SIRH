install.packages(ggplot2)
library(ggplot2)
sirh <- read.csv("C:/Users/guill/Desktop/sirh.csv")
attach(sirh)
View(sirh)
# Rennommage colonnes pour plus de clarté
names(sirh) <- c("heure", "Filiere","Satisfaction","MethodePref","MethodePref2","NotionPref","Enseignant","Volume","Interactif","SuggestionInteractif","Apprentissage","Mutuelle","Confinement","Distanciel","Distianciel2")
attach(sirh)

dim(sirh)
str(sirh)
attributes(sirh)  
nrow(sirh)

#Barplot pour l'origine des elèves

barplot(table(Filiere),ylab = "Effectif",xlab = "Filière d'origine",main = "Origine des élèves en IG") 


#Satisfaction des élèves

(tableA<- table(factor(Satisfaction)))
test<-cumsum(tableA)/nrow(sirh)
plot(cumsum(tableA)/nrow(sirh),type="b",col="blue",main="Représentation de la fréquence cumulé de l'état de satisfactions des élèves", xlab = "Avis", ylab="frequence cumulé",xaxt = "n")
axis(1, at=1:4, labels=c("Pas du tout satisfait", "Peu satisfait", "Satisfait", "Tres satisfait"))

#Methode d'apprentissage preférée et methode qui la semble la plus adaptée aux IG pour progresser

MethodePref <- table(MethodePref)
MethodePref2 <- table(MethodePref2)
MethodePref2["Cours magistraux (CM)"] <- 0
MethodePref2 <- MethodePref2[c(1,5,2,3,4)]
Methodes <- c(MethodePref,MethodePref2)
Methodes <- matrix(Methodes, nc=2,nr=5)
Methodes <- t(Methodes)
colnames(Methodes) = c("En autonomie", "CM", "TD", "TP", "Tutorat")
barplot(Methodes,beside = T,ylab = "Effectif",xlab = "Méthode",main = "Methode d'enseignement preférée", col=c("red","green"),ylim=c(0,25))
legend("topright", 
       legend = c("Preférée pour progresser", "Préferée"), 
       fill = c("green", "red"))


#Comment aborder le cours?

table(NotionPref)
barplot(table(NotionPref),ylab = "Effectif",xlab = "Manière",main = "Comment aborder les notions en cours ?", col = c("#E69F00", "#56B4E9", "#009E73"),names.arg =c("Apprentissage théorique puis application","Etude de cas","Projet"), ylim=c(0,20) ) 

#Insister sur les notions

table(Enseignant)
barplot(table(Enseignant),ylab = "Effectif",xlab = "Avis",main = "Les enseignants doivent-ils plus insister sur les notions de cours ?", col = c("#ff0404", "#04ff15"), ylim=c(0,20) ) 

#Interactivté des cours 
table(Interactif)

Interactif["Non il faut les rendre moins interactifs"] <- 0
barplot(table(Interactif),ylab = "Effectif",xlab = "Opinion",main = "Faut-il rendre les cours davantage interactif ?", ylim=c(0,20), col = c("#ff0000", "#ff7400","#09ff00","#e3ff00")) 


#Enseignement mutuel

a <-table(Mutuelle)
#Mutuelle <-factor(Mutuelle,levels = c("Très mauvaise idée", "Mauvaise idée",  "Bonne idée", "Trés bonne idée"))
barplot(table(Mutuelle),ylab = "Effectif",xlab = "Avis",names.arg =c("Bonne idée","Mauvaise idée","Trés bonne idée","Très mauvaise idée"),main = "Que pensez vous du principe de classe mutuelle ?", ylim=c(0,25),col = c("#e3ff00", "#ff7400","#09ff00","#ff0000") )

#Volume langue étrangère

table(Volume)
barplot(table(Volume),ylab = "Effectif",xlab = "Opinion",main = "Faut-il ajouter plus de cours en langue étrangère ?",col = c("#ff0404", "#04ff15"), ylim=c(0,30) ) 

#Confinement

table(Confinement)
barplot(table(Confinement),ylab = "Effectif",xlab = "Réponse",main = "Les enseignements durant la période de confinement vous semble-t-il bien construits ?", ylim=c(0,30),col = c("#ff7400", "#ff0000","#e3ff00","#09ff00") ) 

#Distanciel

table(Distanciel)
barplot(table(Distanciel),ylab = "Effectif",xlab = "Réponse",main = "Cours non présentiel de manière permanente serait-il bénéfique pour votre scolarité ?", ylim=c(0,25),col = c("#ff0404", "#04ff15") ) 

