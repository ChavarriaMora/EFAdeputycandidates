#Consultoria para voz y voto de AR
#Elias chavarria-Mora
#main code

library(dplyr)
library(tidyr) #both good for tidying
library (ggplot2)
library (Amelia) #Gary King et al package for value imputation, some crazy bootstrapping bayesian shit
library (Rcpp)
library (psych)

setwd("C:/Elias/1 Serious/Arbeit/1 - AmeliaRueda")
df <- read.csv ("cuestionariover316.csv", encoding="UTF-8" )

#recode all the column names
colnames(df)<- c("marca_temporal", "genero", "edad", "estudios", "religion", "ocupacion", "partido", "provincia",
                 "otro_partido", "otros_cargos_pop", "izder_individ","izder_part", "progcons_individ", "progcons_part", 
                 "doctrina", "val1", "val2", "val3", "val4", "val5", "val6", "val7", "val8", "val9",
                 "val10", "val11", "val12", "val13", "val14", "val15", "val16", "val17", "val18", "val19", "nombre", "curul", "x", "codigo_part", "posicion_verificada") 

#impute, either the king way or by mean

#King method
df_imp<-amelia(df, m=5, idvars = c("marca_temporal", "genero", "edad", "estudios", "religion", "ocupacion", "partido", "provincia", 
                                  "otro_partido", "otros_cargos_pop", "izder_individ","izder_part", "progcons_individ", "progcons_part", 
                                  "doctrina", "nombre", "curul", "x", "codigo_part", "posicion_verificada")) #m=5 is the number of iterations of imputation. There are time series and spatial data but irrelevant for this

#Salvar el df con imputaciones
df_impm5<-df_imp$imputations$imp5


#write.csv(df_impm5,'df_impm1.csv')

#for just mean
#df = transform(df, val17 = ifelse(is.na(val17), mean(val17, na.rm=TRUE), val17))

#factor analysis
#df_impm1 <- read.csv ("df_impm1.csv", encoding="UTF-8" )


#we have to choose the number of factors, parallel analysis can be used
df_small<-df_impm5[,16:34]
fa.parallel(x=df_small, fm="minres", fa="fa")
#this creates a parallel analysis scree plot, x axis is factor number, y axis is eigen values of principal components
#basically, chose the number of factors such that the blue line (actual data) is above the red line (similated data)
#in this case, 4


#EFA
EFA<-fa (df_small, nfactors=4, fm="minres") 
#fa is the psych function for factor analysis, fm is the factoring technique, the package has 5
#the default is minres which uses OLS
#nfactors has to be indicated even for EFA
#default rotation is oblimin

print(EFA) #summarym includes the factor loading matrix

#Repito proceso eliminando 4 preguntas que no correlacionaron con ningun factor
df_smaller<-df_small[-c(14)]
fa.parallel(x=df_smaller, fm="minres", fa="fa")
EFA3<-fa (df_smaller, nfactors=5, fm="minres") #con 5 factores, sin pregunta 14
print(EFA3)
EFA2<-fa (df_smaller, nfactors=4, fm="minres") #con 4 factores, sin pregunta 14
print(EFA2)

#get factor scores
Factor_scores<-as.data.frame(factor.scores(df_smaller,EFA2)$scores)

#data for standardizing: 
summary (df_smaller) 

#extract the weights to calculate the score
weights_efa<-weights(EFA2)
weights_efa
write.csv(weights_efa,'weights_efa_Final.csv')


#Formulas for factor scores, get max and min
#MR1, max=3.655, min=-5.89
(0 -3.867 )* -0.016+(6-4.769 )*0.049 +(0- 2.827)*-0.031 +(0- 2.501)*-0.04 +(6-1.895 )*0.034 +(0- 2.014)*-0.032 +(6- 2.751)*0.001 +(0-5.274 )*-0.123 +(0- 3.059)*-0.071 +(6- 3.9)*0.454 +(6-4.757 )*0.269 +(6-3.865 )*0.154 +(6-5.568 )*0.089 +(0-3.571 )*-0.051 +(6-5.572 )*0.02 +(6-3.595 )*0.073 +(0-3.129 )*-0.058 +(6-3.306 )*0.026
(6 -3.867 )* -0.016+(0-4.769 )*0.049 +(6- 2.827)*-0.031 +(6- 2.501)*-0.04 +(0-1.895 )*0.034 +(6- 2.014)*-0.032 +(0- 2.751)*0.001 +(6-5.274 )*-0.123 +(6- 3.059)*-0.071 +(0- 3.9)*0.454 +(0-4.757 )*0.269 +(0-3.865 )*0.154 +(0-5.568 )*0.089 +(6-3.571 )*-0.051 +(0-5.572 )*0.02 +(0-3.595 )*0.073 +(6-3.129 )*-0.058 +(0-3.306 )*0.026

#MR2, max=4.038, min=-5.572
(6-3.867 )*0.273 +(0-4.769 )*-0.117 +(6- 2.827)*0.003 +(6- 2.501)*0.185 +(6-1.895 )*0.122 +(6- 2.014)*0.001 +(0- 2.751)*-0.015 +(0-5.274 )*-0.057 +(6- 3.059)*0.059 +(0- 3.9)*-0.002 +(6-4.757 )*0.0129 +(0-3.865 )*-0.017 +(6-5.568 )*0.018 +(6-3.571 )*0.061 +(6-5.572 )*0.35 +(6-3.595 )*0.028 +(6-3.129 )*0.01 +(6-3.306 )*0.271
(0-3.867 )*0.273 +(6-4.769 )*-0.117 +(0- 2.827)*0.003 +(0- 2.501)*0.185 +(0-1.895 )*0.122 +(0- 2.014)*0.001 +(6- 2.751)*-0.015 +(6-5.274 )*-0.057 +(0- 3.059)*0.059 +(6- 3.9)*-0.002 +(0-4.757 )*0.0129 +(6-3.865 )*-0.017 +(0-5.568 )*0.018 +(0-3.571 )*0.061 +(0-5.572 )*0.35 +(0-3.595 )*0.028 +(0-3.129 )*0.01 +(0-3.306 )*0.271

#MR3, max=2.218, min=-8.359
(0-3.867 )*-0.056 +(6-4.769 )*0.162 +(6- 2.827)*0 +(0- 2.501)*-0.069 +(0-1.895 )*-0.134 +(0- 2.014)*-0.068 +(0- 2.751)*-0.038 +(6-5.274 )*0.346 +(6- 3.059)*0.04 +(0- 3.9)*-0.034 +(6-4.757 )*0.098 +(0-3.865 )*-0.02 +(6-5.568 )*0.3 +(6-3.571 )*0.004 +(6-5.572 )*0.347 +(0-3.595 )*-0.014 +(0-3.129 )*-0.017 +(6-3.306 )*0.016
(6-3.867 )*-0.056 +(0-4.769 )*0.162 +(0- 2.827)*0 +(6- 2.501)*-0.069 +(6-1.895 )*-0.134 +(6- 2.014)*-0.068 +(6- 2.751)*-0.038 +(0-5.274 )*0.346 +(0- 3.059)*0.04 +(6- 3.9)*-0.034 +(0-4.757 )*0.098 +(6-3.865 )*-0.02 +(0-5.568 )*0.3 +(0-3.571 )*0.004 +(0-5.572 )*0.347 +(6-3.595 )*-0.014 +(6-3.129 )*-0.017 +(0-3.306 )*0.016

#MR4, max=5.083, min=-5.476
(0-3.867 )*-0.031 +(6-4.769 )*0.212 +(6- 2.827)*0.186 +(0- 2.501)*-0.056 +(6-1.895 )*0.057 +(6- 2.014)*0.118 +( 6- 2.751)*0.184 +(6-5.274 )*0.092 +(6- 3.059)*0.171 +(0- 3.9)*-0.059 +(6-4.757 )*0.011 +(0-3.865 )*-0.0001 +(6-5.568 )*0.009 +(6-3.571 )*0.119 +(0-5.572 )*-0.103 +(6-3.595 )*0.011 +(6-3.129 )*0.252 +(6-3.306)*0.089
(6-3.867 )*-0.031 +(0-4.769 )*0.212 +(0- 2.827)*0.186 +(6- 2.501)*-0.056 +(0-1.895 )*0.057 +(0- 2.014)*0.118 +( 0- 2.751)*0.184 +(0-5.274 )*0.092 +(0- 3.059)*0.171 +(6- 3.9)*-0.059 +(0-4.757 )*0.011 +(6-3.865 )*-0.0001 +(0-5.568 )*0.009 +(0-3.571 )*0.119 +(6-5.572 )*-0.103 +(0-3.595 )*0.011 +(0-3.129 )*0.252 +(0-3.306)*0.089





#add factos scores to original db
#create an identifier for the df of pca scrores, I use marca temporal
Factor_scores$marca_temporal<-df_impm5$marca_temporal
#append to the original dataframe
df_full<-merge (df_impm5, Factor_scores, by='marca_temporal')
df_full$Dimension1<-(df_full$MR1+5.89)/(3.655+5.89)
df_full$Dimension2<-(df_full$MR2+5.572)/(4.038+5.572)
df_full$Dimension3<-(df_full$MR3+8.359)/(2.218+8.359)
df_full$Dimension4<-(df_full$MR4+5.476)/(5.083+5.476)

#Ademas, hago uno global, multiplicando dimension por el peso de cada factor
df_full$DimensionGlobal<-(df_full$Dimension1*0.28 + df_full$Dimension2 *0.26 + df_full$Dimension3 *0.18 +df_full$Dimension4 *0.29)

write.csv(df_full,'df_FINAL.csv', fileEncoding = 'UTF-8') #OJOOO, que utf=8


#End of main code


#pegar
agregar <- read.csv ("agregar.csv", encoding="UTF-8" )
df_full <- read.csv ("df_FINAL.csv", encoding="UTF-8" )
colnames(agregar)<- c("marca_temporal", "genero", "edad", "estudios", "religion", "ocupacion", "partido", "provincia",
                 "otro_partido", "otros_cargos_pop", "izder_individ","izder_part", "progcons_individ", "progcons_part", 
                 "doctrina", "val1", "val2", "val3", "val4", "val5", "val6", "val7", "val8", "val9",
                 "val10", "val11", "val12", "val13", "val14", "val15", "val16", "val17", "val18", "val19", "nombre", "curul", "x", "codigo_part", "posicion_verificada") 

df_full3<-agregar %>% full_join(df_full[, c("marca_temporal", "MR1","MR2","MR3","MR4", "Dimension1",
                                  "Dimension2","Dimension3","Dimension4","DimensionGlobal")], by = "marca_temporal") # keep rows without matching ID
write.csv(df_full3,'df_FINAL2.csv', fileEncoding = 'UTF-8') #OJOOO, que utf=8

#Principal components
help("prcomp")
#perform PCA
df_pca<-prcomp(df_imp$imputations$imp1[,c(16:34)], center = T, scale.=T, rank.=4) 

#need to include scale=T to do the normalization, center centers it around 0
#you can ad tol to ommitd components
#also, rank. which sets the maximum number of components. I can explore first and then choose based on keyser rule or whatever
#calculation is done with SVD of centered and scaled matrix, not by eigen on the covariance matrix. 

summary(df_pca)
#proportion of variance tells you how much of variance is explaines by each component, zB PC1 explaine 0.23, ie 23 %

df_pca$rotation #correlation between variables and components, ie factor loadings

#save a matrix of the scores for each component
pcascores<-df_pca$x 

#save scores as df
pcasdf<-as.data.frame(df_pca$x)

#create an identifier for the df of pca scrores, I use marca temporal
pcasdf$marca_temporal<-df$marca_temporal

#append to the original dataframe
df_full<-merge (df, pcasdf, by='marca_temporal')



###CORRER EL CODIGO HASTA ACA




#https://www.datacamp.com/community/tutorials/pca-analysis-r
#really good page for calculating how to project onto de new space

#1. get the data for centering, a vector of values for centering
centering_values<-df_pca$center
centering_values #you get one value for each val* column
help(scale) #leer que hace scale(center=T) en R
#column centering: Each column of X has the corresponding value of Center substracted from it

#Osea, para cada individuo, la aritmentica es:
#val1-centering_values$val1 etc para cada valor, deberia dar 19 val* primos, los voy a llamar scaling_values
#son un vector, ver abajo


#2. projecting into the space of the matrix (rotation)
#get the data for scaling
scaling_values %*% df_pca$rotation #%8% es matrix multiplication
#Es nultiplicar los 19 val* primos, scaling values, por el rotation matrix, ie df_pca$rotation, que son los factor loadings


#Entonces, para cada factor, la aritmetica es:
scaling_values<- c((val1-XXX), (val2-XXX), (val3-XXX),  (val4-XXX),  (val5-XXX),  (val6-XXX),  (val7-XXX),  (val8-XXX),
                   (val9-XXX), (val10-XXX),  (val11-XXX),  (val12-XXX),  (val13-XXX),  (val14-XXX),  (val15-XXX),  
                   (val16-XXX),  (val17-XXX), (val18-XXX), (val19-XXX)) 

#Y luego, para cada PC
PC1=(scaling_values[1]*df_pca$rotation[1,1])+(scaling_values[2]*df_pca$rotation[1,2]) #+etc etc, toma un rato escribirlo pero se puede

#Haga una exploracion de los limites de cada PCA, osea min y max, igual creo que para ver el maximo posible se pueden 
#calcular los casos extremos
min(df_full$PC1) -3
max(df_full$PC1) 5



#create scree plot
var_explained<-df_pca$sdev^2 / sum(df_pca$sdev^2)
qplot(c(1:19),var_explained) + #1:19 is all the components
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)  #tambien sugiere 4 factores!!


















#Experiments
#Ejemplo de uso de varias preguntas para una análisis de reduccion de dimensionalidad: EFA
library (rio) #for importing
library(dplyr)
library(tidyr) #both good for tidying
library(psych) #psychometric package, commonly used for EFA
library(GPArotation) #also needed for the EFA rotation 
library (Amelia) #Gary King et al package for value imputation, some crazy bootstrapping bayesian shit
library (ggplot2)

setwd("C:/Elias/1 Serious/Academia/Datasets/LAPOP")
LAPOP <- import ("2009-2018 LAPOP.dta")
LAPOP_Smaller <-LAPOP%>% #database de solo preguntas que interesan
  filter (wave==2018) %>%
  select (b1, b2, b3, b4, b6, b12, b13, b18, b20, b21, b31, b32, b37)

#Remove rows with missing values and keep only complete cases
LAPOP_Smaller=LAPOP_Smaller[complete.cases(LAPOP_Smaller),]


help(fa)


#we have to choose the number of factors, parallel analysis can be used
fa.parallel(x=LAPOP_Smaller, fm="minres", fa="fa")
#this creates a parallel analysis scree plot, x axis is factor number, y axis is eigen values of principal components
#basically, chose the number of factors such that the blue line (actual data) is above the red line (similated data)
#in this case, 6


#EFA
EFA<-fa (LAPOP_Smaller, nfactors=6, fm="minres") 
#fa is the psych function for factor analysis, fm is the factoring technique, the package has 5
#the default is minres which uses OLS
#nfactors has to be indicated even for EFA
#default rotation is oblimin

#extract the weights to calculate the score
weights(EFA)

#to get the factor scores
Factor_scores<-factor.scores(LAPOP_Smaller,EFA)$scores


#ojo, so uso scores con el correlation matrix, me da la correlation matrix
#hay diferentes formas de calcular los scores, el standard es Thurstone que es basicamente un OLS tal que
#Score=weight_1*questions_1+weight_2*questions_2+....


#ojo: lo mejor parece ser CPA, pero creo que lo mejor seria correr CPA, EFA y CFA

####
#Ok, con una verision mini de la base, usando PCA
setwd("C:/Elias/1 Serious/Arbeit/1 - AmeliaRueda")
df <- read.csv ("cuestionarioPRUEBA.csv", encoding="UTF-8" )

#recode all the column names
colnames(df) #tells you the current column names

#you can get tne new names just with the following command
colnames(df)<- c("marca_temporal", "genero", "edad", "estudios", "religion", "ocupacion", "partido", "provincia",
                 "otro_partido", "otros_cargos_pop", "izder_individ","izder_part", "progcons_individ", "progcons_part", 
                 "doctrina", "val1", "val2", "val3", "val4", "val5", "val6", "val7", "val8", "val9",
                 "val10", "val11", "val12", "val13", "val14", "val15", "val16", "val17", "val18", "val19") 

str(df) #chack the types of each variable, the vals are integers

#smaller dataset of only the vals
df_val <-df%>% select (val1, val2,val3,val4,val5,val6,val7,val8,val9,val10,val11,val12,val13,val14,val15,val16,
                       val17,val18,val19)

#ver missing values
sum(is.na(df_val$val1))
sum(is.na(df_val$val2))
sum(is.na(df_val$val3))
sum(is.na(df_val$val4))
sum(is.na(df_val$val5))
sum(is.na(df_val$val6))
sum(is.na(df_val$val7))
sum(is.na(df_val$val8))
sum(is.na(df_val$val9))
sum(is.na(df_val$val10))
sum(is.na(df_val$val11))
sum(is.na(df_val$val12))
sum(is.na(df_val$val13))
sum(is.na(df_val$val14))
sum(is.na(df_val$val15))
sum(is.na(df_val$val16))
sum(is.na(df_val$val17))
sum(is.na(df_val$val18))
sum(is.na(df_val$val19))

#I can have NO NAs, impute with Gary King method
#CAnnot use it now because the number of observations is too low so will have to use a sad old scool method
#df_val_imp<-amelia(df_val, m=5) #m=5 is the number of iterations of imputation. There are time series and spatial data but irrelevant for this


#ok, just impute by mean for val 17
df_val = transform(df_val, val17 = ifelse(is.na(val17), mean(val17, na.rm=TRUE), val17))

#PCA steps
#Normalize the variables: scale for mean of 0 and s.d. of 1
#calculate the covariance matrix
#calculate the eigenvalues of the covariance matrix

?prcomp

results<-prcomp(df_val, center = T, scale.=T) #need to include scale=T to do the normalization, center centers it around 0
#you can ad tol to ommitd components
#also, rank. which sets the maximum number of components. I can explore first and then choose based on keyser rule or whatever
#calculation is done with SVD of centered and scaled matrix, not by eigen on the covariance matrix. 

results$rotation<--1*results$rotation #eigenvectors in R point in negative direction by default, needs a rotation
#check if this is really necessary, I have not sen anythying related to it

results$rotation

#the score for each observations is in results$x
results$x
results$x<--1*results$x#also needs to be rotated by -1
head(results$x)

#calculate total variance explained by each principal component
var_explained<-results$sdev^2 / sum(results$sdev^2)

#create scree plot
qplot(c(1:19),var_explained) + #1:19 is all the components
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

#AS of now, there is waaaay too high a number of components and they explain too little. Could it be better with more observations?



#Center and scale the new individuals data using the center and the scale of the PCA
#Calculate the predicted coordinates by multiplying the scaled values with the eigenvectors (loadings) 
#of the principal components. So, I need to center and scale, shouldn't be that hard
#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/
#the prcomp should return the center (variable means) and scale (variables standard deviation)


#an easy example, with mtcars
mtcars.pca<-prcomp(mtcars[,c(1:7, 10, 11)], center=T, scale.=T)
summary(mtcars.pca)
#ojo, sale que proportion of variance para pc1 e 0.62, eso significa que explica 63% de variancia

str(mtcars.pca)
#summarizes object, includes several things, including
#$center is the center point of each component
#$scale is the scaling
#sdev is the standard dev of each component
#$rotation is the relation between the original variables and the component
#$x the scores for each component


#Now, what is important is to project each new sample:
#1. scale the values in regards to the PCA's center mtcars.pca$center
#2. apply the rotation of the PCA matrix 
#code for 1 would be: sc<-scale (t(OBSERVATION[C(LIST OF VARS)], center=mtcars.pca$center))
#code for 2: pred<-sc%*%mtcars.pca$rotation
#necesito saber el codigo matematico de esto
#https://www.datacamp.com/community/tutorials/pca-analysis-r
#really good page