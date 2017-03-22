setwd("/Users/lucieraguet/Documents/R/Seminaire_ECP/Analyse_Fiches")
rm(list=ls())
par(mfrow=c(1,1))

tab = read.csv("analyse_fiches.csv", header = T, sep =";")
View(tab)

tab_hot = tab[which(tab[,5]=="hotel"),]
tab_port = tab[which(tab[,5]=="port"),]
tab_ent = tab[which(tab[,5]=="entreprise"),]
tab_cc = tab[which(tab[,5]=="centre commercial"),]

library(lars)

##### Régression Lasso Générale avec distance à la côte #####

tab = tab[!is.na(tab[,6]),]
tab_mat = as.matrix(tab[,6:106])
tab_cible = scale(as.matrix(tab[,4]))
reg_lasso=lars(tab_mat,tab_cible,type="lasso")
plot(reg_lasso)
coef_l=predict.lars(reg_lasso,tab_mat,type="coefficients",mode="lambda",s=0.01)
coef_l = sort(coef_l$coefficients, decreasing = TRUE)

##### Régression Lasso uniquement pour les hôtels avec distance à la côte #####

tab_hot = tab_hot[!is.na(tab_hot[,6]),]
tab_mat_hot = as.matrix(tab_hot[,6:106])
tab_cible_hot = scale(as.matrix(tab_hot[,4]))
reg_lasso_hot=lars(tab_mat_hot,tab_cible_hot,type="lasso")
plot(reg_lasso_hot)
coef_l_hot=predict.lars(reg_lasso_hot,tab_mat_hot,type="coefficients",mode="lambda",s=0.01)
coef_l_hot = sort(coef_l_hot$coefficients, decreasing = TRUE)

##### Régression Lasso Générale sans distance à la côte #####

tab_2_mat = as.matrix(tab[,7:106])
tab_2_cible = scale(as.matrix(tab[,4]))
reg_2_lasso=lars(tab_2_mat,tab_2_cible,type="lasso")
plot(reg_2_lasso)
coef_2_l=predict.lars(reg_2_lasso,tab_2_mat,type="coefficients",mode="lambda",s=0.01)
coef_2_l = sort(coef_2_l$coefficients, decreasing = TRUE)

##### Régression Lasso uniquement pour les hôtels sans distance à la côte #####

tab_2_mat_hot = as.matrix(tab_hot[,7:106])
tab_2_cible_hot = scale(as.matrix(tab_hot[,4]))
reg_2_lasso_hot=lars(tab_2_mat_hot,tab_2_cible_hot,type="lasso")
plot(reg_2_lasso_hot)
coef_2_l_hot=predict.lars(reg_2_lasso_hot,tab_2_mat_hot,type="coefficients",mode="lambda",s=0.01)
coef_2_l_hot = sort(coef_2_l_hot$coefficients, decreasing = TRUE)

##### Régression Lasso mots sélectionnés manuellement - Sans distance à la côte #####

tabl = read.csv("freq_manuelle.csv", header=T, sep =";")

tabl_mat = as.matrix(tabl[,7:14])
tabl_cible = scale(as.matrix(tabl[,4]))
regl_lasso=lars(tabl_mat,tabl_cible,type="lasso")
plot(regl_lasso)
coefl_l=predict.lars(regl_lasso,tabl_mat,type="coefficients",mode="lambda",s=0.0)
coefl_l = sort(coefl_l$coefficients, decreasing = TRUE)
