
# 2. Chargement des données :

# load csv
produit <- read.csv("./TP2/Data Produit.csv", header = TRUE, sep = ",", dec = ".")

# description de la table 
str(produit) 

# chaque elem vc ses occurences 
table(produit$Produit) 

# data d'apprentissage
produit_EA <- produit[1:400,] 

# data de test
produit_ET <- produit[401:600,] 

# supp colonne id
produit_EA <- produit_EA[,-1] 

# lbadil pr supp colonne 
#produit_EA <- subset(produit_EA, select = -ID) 

#min max median mean 1st quali
summary(produit)

# 3. Apprentissage d'un arbre de décision rpart
# install & activate the package
install.packages("rpart")
library("rpart")

# en cas d'erreur
library("installr")
updateR() #maelina

# creation de l arbre avec les params par def 
# Produit est la var à prédire 
# . => ts les var du data frame sont  prédictives
tree1 <- rpart(Produit ~ ., produit_EA)
# ta hakda explicitement c ok
tree2 <- rpart(Produit ~ Age + Sexe + Habitat + Revenus + Marie+ Enfants + Emprunt, produit_EA)
rm(tree2)

# 4. Représentation graphique de l'arbre

plot(tree1)
text(tree1, pretty=0) #ila kan vrai kiduz ela lisr -_-

#5. Évaluation des performances de l'arbre

test_arbre <- predict(tree1, produit_ET, type = "class")
# vecteur genéré par la commande 
test_arbre

# nbr d occurence de OUI et NON 
table(test_arbre)

#ajouter colonne prediction au nv de la table de test pour comparer
produit_ET$Prediction <- test_arbre

#afficher table
View(produit_ET)

# les exemples li predic == reel

produit_ET[produit_ET$Produit==produit_ET$Prediction, ]

# nbr de fois li dakshi perfect 
nbr_succes <- length(produit_ET[produit_ET$Produit==produit_ET$Prediction,"ID"])
# same res
nbr_succes <- nrow(produit_ET[produit_ET$Produit==produit_ET$Prediction,])

taux_succes <- nbr_succes/nrow(produit_ET)

# les exemples li predic != reel

# nbr de fois li dakshi eyaan 
# par def had length kateati le nbr de colonne d la table ms i on precise une colonne on va avoir le nbr de ligne
nbr_echec <- length(produit_ET[produit_ET$Produit!=produit_ET$Prediction,"ID"])
# same res
nbr_echec <- nrow(produit_ET[produit_ET$Produit!=produit_ET$Prediction,])

taux_echec <- nbr_echec/nrow(produit_ET)

# 6. Application de l'arbre pour la prédiction

produit_pro <- read.csv("./TP2/Data Produit Prospects.csv", header = TRUE, sep = ",",dec = ".")
pred_tree1 <- predict(tree1, produit_pro, type="class")
table(pred_tree1)
pred_tree1
produit_pro$Prediction <- pred_tree1

produit_pro_oui <- produit_pro[produit_pro$Prediction=="Oui",]
produit_pro_non <- produit_pro[produit_pro$Prediction=="Non",]
