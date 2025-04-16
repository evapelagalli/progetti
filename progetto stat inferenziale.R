####1)carico i dati

my_data <- read.csv("neonati.csv",stringsAsFactors=T, sep=",")  
attach(my_data)

#carico le librerie
library(moments)
library(DescTools)
library(ggplot2)
library(ggpubr)
library(lmtest)
library(car)
library(rgl)
library(corrplot)

#2
str(my_data)
#sono tutte variabili quantitative discrete ad accezione di 
#Madre Fumatrice che è una dummy al posto di una varibile qualitativa nominale
#Tipo di parto, Ospedale e Sesso che sono variabili qualitatitive nominali
summary(my_data)
#tramite il summary noto un'anomalia della variabile Anni madre
which(Anni.madre==0)
#l'osservazione 1380 è molto strana perchè l'eta della madre è zero! Forse 20, 30, 40
table(Anni.madre)
#anche l'osservazione 1152 sembra avere un errore 
which(Anni.madre==1)

#cancello le righe 1380 e 1152
data=my_data[-c(1380,1152),]
summary(data)
attach(data)

#3
##table per le variabili qualitative
table(Tipo.parto)
table(Ospedale)
table(Sesso)
table(Fumatrici)
N=nrow(data)
table(Tipo.parto)/N
table(Ospedale)/N
table(Sesso)/N
table(Fumatrici)/N
#la gran parte dei parti sono naturali (71%). La gran parte delle madri è non fumatrice (96%).Il numero dei parti nei tre ospedali è piuttosto equidistribuito come anche la variabile Sesso

#fumatrici
barplot(table(Fumatrici),main="Fumatrici")
(table(Fumatrici)/N)*100
#il 96 % delle donne non è fumatrice.
#indice di gini
gini.index <- function(x){
  ni = table(x)
  fi = ni/length(x)
  fi2 = fi^2
  J = length(table(x))
  
  gini = 1-sum(fi2)
  gini.norm = gini/((J-1)/J)
  
  return(gini.norm)
}

#valore basso di eterogeneita(0 indica la massima omogeneità, 1 la massima eterogenietà)
gini.index(Fumatrici)#0.16
#risultato che mi aspettavo dato che la gran parte delle donne è non fumatrice

#Tipo di parto
table(Tipo.parto)
barplot(table(Tipo.parto),main="Tipo parto")
(table(Tipo.parto)/N)*100 # il 71 % dei parti è naturale
gini.index(Tipo.parto)#0.83

#Ospedale
table(Ospedale)
barplot(table(Ospedale),main="Ospedale")
(table(Ospedale)/N)*100 # caso di equidistribuzione
#indice di eterogeintà di Gini

gini.index(Ospedale)#1 quindi è il caso di massima eterogeinità

#Sesso
table(Sesso)
barplot(table(Sesso),main="Sesso")
(table(Sesso)/N)*100 # quasi stessa percentuale
#indice di eterogeintà di Gini
gini.index(Sesso)#1 quindi è il caso di massima eterogeinità


## per le variabili quantitative uso degli istogrammi e boxplot

#la variabile Anni.madre sembra essere piuttosto simmetrica
hist(Anni.madre, main="Anni madre",xlab="Anni madre")
boxplot(Anni.madre, main="Anni madre")
# la media è 28 anni con valori anomali sotto i 14 anni e mezzo e sopra i 42 anni e mezzo
#calcolo l'intervallo per definire gli outlier
25-1.5*IQR(Anni.madre)
32+1.5*IQR(Anni.madre)
#sd campionaria
sd(Anni.madre)

#library(moments)
skewness(Anni.madre)#0,15: la variabile è leggermente asimmetrica positivamente (con una leggera coda a dx)
#eseguo il test di Shapiro per testare la normalità della variabile
shapiro.test(Anni.madre)# rifiuto l'ipotesi nulla di normalità



#N.gravidanze è asimmetrica a dx con zero che è la moda. Il 44% delle donne è diventata madre per la prima volta
#una sola donna ha 12 figli
hist(N.gravidanze,main="Numero gravidanze")
#sd campionaria
sd(N.gravidanze)

skewness(N.gravidanze)
table(N.gravidanze)
(1095/N)*100#il 44% delle donne è diventata madre per la prima volta
#la variabile è chiaramente non normale dal grafico


#Gestazione
hist(Gestazione,main="Gestazione")
#ricerca personale:[In media la gravidanza giunge a termine al compimento della 40° settimana, che è il giorno in cui viene indicata la data presunta del parto. Per questo, una gravidanza a termine è compresa tra le 38 e le 40 settimane, mentre generalmente si aspetta fino a 41+3 settimane per indurre il parto.
#Un neonato prematuro è un feto partorito prima di 37 settimane di gestazione.]
length(which(Gestazione<37))
(162/N)*100# il 6.49 % dei bimbi è prematuro

#la moda è 40 settimane proprio come afferma la mia ricerca
#library(DescTools)
Mode(Gestazione)
skewness(Gestazione)#asimmetrica a sx
#sd campionaria
sd(Gestazione)


#peso in grammi
hist(Peso, main="Peso")
skewness(Peso)#  asimm a sx
#RIcerca personale:[In media il peso nascita è di circa 3300 grammi, con qualche differenza tra maschi e femmine (i maschi pesano circa 150 grammi in più), mentre non ci sono particolari differenze per quanto riguarda la lunghezza, pari mediamente a 50 centimetri.
#Alla nascita, il peso medio di un essere umano varia dai 3200 ai 3400 grammi]

#nei dati la media è 3284
mean(data$Peso)#3284

tapply(Peso, Sesso, mean)#una femmina pesa mediamente 3161, un maschio 3408.496 
boxplot(Peso, main="Peso")#diversi valori anomali
IQR(Peso)
quantile(Peso)
2990-1.5*IQR(Peso)#2045
3620+1.5*IQR(Peso)#4565
length(which(Peso<2045 | Peso>4565))/N # in fondo solo il 2,76 % è considerato outliers

#sd campionaria
sd(Peso)
#testo la normalita in quanto questa sarà la variabilte di risposta e rifiuto l'ipotesi di normalità
#è un cattivo risultato perchè significa che è violata una delle ipotesi del modello di regressione lineare
#ma è conforme con il risultato che dopo 40 settimane viene indotto il parto e quindi i valori più alti non sono più osservati quando viene interrotta la gestazione
shapiro.test(Peso)


#lunghezza
hist(Lunghezza,main="Lunghezza")
skewness(Lunghezza)#asimmetria a sx
mean(Lunghezza)
#la media e 494.7 mm, quindi questo conferma la mia ricerca
#sd campionaria
sd(Lunghezza)
shapiro.test(Lunghezza)#rifiuto l'ipotesi di normalità


#diametro Cranio

hist(Cranio,main="Cranio")
skewness(Cranio)#asimmetria a sx
boxplot(Cranio,main="Cranio")
#sd campionaria
sd(Cranio)




#4
#Poichè la varianza della popolazione non è nota a priori e i dati di partenza non sono normali, uso la t di student
# non mi vengono forniti i parametri della popolazione, quindi li ricavo da una ricerca da me svolta:

#[Alla nascita, il peso medio di un essere umano varia dai 3200 ai 3400 grammi, la lunghezza è 50 cm]

#peso
t.test(Peso, 
       mu = 3300,
       conf.level = 0.95, 
       alternative = "two")

# non rifiuto HO, Infatti la statitica test rientra nella zona di accettazione con valori soglia di -1.505 e pvalue superiore al 5%

#lunghezza
t.test(Lunghezza, 
       mu = 500,
       conf.level = 0.95, 
       alternative = "two")

# in questo caso  rifiuto Ho con un p value inferiore al 5%

#5

#vediamo se è vera la distizione del Peso rispetto al Sesso
boxplot(Peso~Sesso,main="Sesso")# Il boxplot più alto per i maschi
#sarebbe però meglio da approfondire con un t test
#calcolo il t test per Il Peso considerando campioni indipendenti
#pongo var.equal=T per utilizzare la varianza campionaria pooled

t.test(data=data,
       Peso~Sesso,
       paired= FALSE,
       var.equal=T)


#l'IC non contiene lo zero
#la stat t è -12,111  e p value molto basso e inferiore al 5%. Rifiuto Ho: esiste una differenza statisticamente significativa del Peso rispetto al Sesso

#vediamo se è vera la distizione rispetto al Sesso
boxplot(Lunghezza~Sesso)# Il boxplot più alto per i maschi
#calcolo il t test per la lunghezza considerando campioni indipendenti
t.test(data=data,
       Lunghezza~Sesso,
       paired= FALSE,
       var.equal=T)

#l'IC non contiene lo zero
#la stat t è -9,5761  e p value molto basso e inferiore al 5%. Rifiuto Ho: esiste una differenza statisticamente significativa della lunghezza rispetto al Sesso
#l'informazione da me trovata è quindi smentita da questo campione

#calcolo il t test per Cranio considerando campioni indipendenti
t.test(data=data,
       Cranio~Sesso,
       paired= FALSE,
       var.equal=T)

#l'IC non contiene lo zero
#la stat t è -7.4344  e p value molto basso e inferiore al 5%. Rifiuto Ho: esiste una differenza statisticamente significativa del Cranio rispetto al Sesso



#6)test del chi quadrato
data_chi_q= data[,c(8,9)]
table(data_chi_q)
test.indipendenza<- chisq.test(table(data_chi_q))
test.indipendenza

test.indipendenza$expected
#Con alfa= 0.05, accetto HO ovvero l'ipotesi di indipendenza.Questa voce va smentita

#il grafico conferma questa ipotesi in quanto sembrano essere rispettate le stesse proporzioni tra i due tipi di parti
ggpubr::ggballoonplot(data=as.data.frame(table(data_chi_q)),
                      fill="blue")
#in proporzione il numero di parti cesarei è circa la stessa
242/(242+574)#0.2965686
254/(254+594)#0.2995283
232/(232+602)#0.2781775
#il parto naturale è il più diffuso in tutti e tre gli ospedali


#Indice di Kendal
tabella <- as.table(table(data_chi_q))

countsToCases <- function(x, countcol = "Freq") {
  # Get the row indices to pull from x
  idx <- rep.int(seq_len(nrow(x)), x[[countcol]])
  
  # Drop count column
  x[[countcol]] <- NULL
  
  # Get the rows from x
  x[idx, ]
}


dati_long <- countsToCases(as.data.frame(tabella))

cor.test(as.numeric(dati_long$Tipo.parto),
         as.numeric(dati_long$Ospedale),
         method = "kendall")

#con un pvalue pari a 0.4078 non rifiuto l'ipotesi nulla. Non sembrano esserci concordanze o discordanze significative

##ANALISI MULTIDIMENSIONALE
#Relazione fra Peso e Lunghezza
plot(Lunghezza,Peso)
cor(Lunghezza,Peso)#0.7960415 indica una correlazione lineare positiva.
mod_lin_pl<-lm(Peso~Lunghezza, data=data)  
mod_lin_pl$coefficients# bo=-4571.62746    b1=15.88009
summary(mod_lin_pl)
#rifiuto l'ipotesi nulla che le stime siano significatiavmente ugualia zero. Ciò è importante soprattutto per b1. 
#R quadro = 0.6337 valore non molto alto essendo il massimo pari ad 1. Ciò mi fa pensare che il modello lineare non sia il migliore
abline(mod_lin_pl,col=2)

par(mfrow=c(1,2))
plot(residuals(mod_lin_pl))#è evidente un valore anomalo
abline(h=mean(residuals(mod_lin_pl)))
plot(density(residuals(mod_lin_pl)))

shapiro.test(mod_lin_pl$residuals)# rifiuto l'ipotesi di normalità dei residui. Infatti anche la variabile Peso non era normale
lmtest::bptest(mod_lin_pl)# rifiuto l'ipotesi di omoschedasticità
lmtest::dwtest(mod_lin_pl)#non rifiuto l'ipotesi di indipendenza dei residui

#In definitiva, non sono verificate tutte le ipotesi e  quindi applico il modello con "cautela". 
#Kendall
cor(Peso,Lunghezza, method="kendall")#0.59 (max 1)

#grafico della relazione lineare distinguendo per Sesso
#per lunghezze maggiori, il peso è maggiore per i maschi, per lunghezze minori è il contraio
#c'è un punto di incontro delle due rette

ggplot(data=data)+
  geom_point(aes(x=Lunghezza,
                 y=Peso,
                 col=Sesso),position = "jitter")+
  geom_smooth(aes(x=Lunghezza,
                  y=Peso,
                  col=Sesso),se=F,method = "lm")


  
#relazione quadratica
mod_quad_pl<-lm(Peso~Lunghezza+I(Lunghezza^2), data=data)  

summary(mod_quad_pl)
#aumento dell'R^2 
0.637-0.6337#0.0033
#anova
anova(mod_lin_pl,mod_quad_pl)
#rifiuto l'ipotesi nulla che mi porta a preferire il modello quadratico
#anche BIC, AIC e MSE preferiscono il quadratico
BIC(mod_lin_pl,mod_quad_pl)
AIC(mod_lin_pl,mod_quad_pl)
MSE<-function(y_oss,y_prev){
  return(sum((y_oss-y_prev)^2)/length(y_prev))
}
mse_train_1pl<-MSE(data$Peso, fitted(mod_lin_pl))
mse_train_1pl#101014.2
mse_train_2pl<-MSE(data$Peso, fitted(mod_quad_pl))
mse_train_2pl#100092.7
#secondo tutti i criteri di selezione è preferibile il modello di regressione quadratica tra Peso e Lunghezza

#grafico della relazione quadratica distinguendo per Sesso
ggplot(data=data)+
  geom_point(aes(x=Lunghezza,
                 y=Peso,
                 col=Sesso),position = "jitter")+
  geom_smooth(aes(x=Lunghezza,
                  y=Peso,
                  col=Sesso),se=F)+
  geom_line(aes(x=Lunghezza,
                y=mod_quad_pl$fitted.values,
                col=Sesso),linetype = "dashed")

##analisi con i glm
#poiché la variabile Peso non è normale, provo ad applicare un modello glm(family=gaussian e link=log)
glmLog_Peso_Lunghezza<- glm(Peso ~ Lunghezza, data = data, family = gaussian(link = "log"))
glmLog_Peso_Lunghezza#AIC 35900
glmLog_Peso_Lunghezza_quad<- glm(Peso ~ Lunghezza+I(Lunghezza^2), data = data, family = gaussian(link = "log"))
glmLog_Peso_Lunghezza_quad#AIC 35850
#confrontiamo 
BIC(mod_lin_pl,mod_quad_pl,glmLog_Peso_Lunghezza,glmLog_Peso_Lunghezza_quad)#è preferibile il modello quadratico dei glm
AIC(mod_lin_pl,mod_quad_pl,glmLog_Peso_Lunghezza,glmLog_Peso_Lunghezza_quad)#è preferibile il modello quadratico dei glm
mse_train_3pl<-MSE(data$Peso, fitted(glmLog_Peso_Lunghezza))
mse_train_3pl#101717.7
mse_train_4pl<-MSE(data$Peso, fitted(glmLog_Peso_Lunghezza_quad))
mse_train_4pl#99585.37# è preferibile il modello quadratico dei glm


#calcolo R^2
1 - (deviance(glmLog_Peso_Lunghezza_quad)/glmLog_Peso_Lunghezza_quad$null.deviance)#R^2 0.6388635
#aumento R^2 rispetto al modello quadratico (link=identity)
0.6388635-0.637 


#Analisi Peso e Cranio
par(mfrow=c(1,1))
plot(Cranio,Peso)
cor(Cranio,Peso)#0.7048438 indica una correlazione lineare positiva.
mod_lin_cp<-lm(Peso~Cranio, data=data)  
mod_lin_cp$coefficients# bo=-4377.68313    b1=22.53297
summary(mod_lin_cp)
#rifiuto l'ipotesi nulla che le stime siano significativamente uguali a zero. Ciò è importante soprattutto per b1. 
#R quadro 0.4968 indica che il modello lineare non è il migliore soprattutto per i valori più bassi
abline(mod_lin_cp,col=2)

par(mfrow=c(1,2))
plot(residuals(mod_lin_cp))
abline(h=mean(residuals(mod_lin_cp)))
plot(density(residuals(mod_lin_cp)))

shapiro.test(mod_lin_cp$residuals)#rifiuto l'ipotesi di normalità dei residui
lmtest::bptest(mod_lin_cp)# non rifiuto l'ipotesi di omoschedasticità 
lmtest::dwtest(mod_lin_cp)#non rifiuto l'ipotesi di indipendenza degli errori

#è violata un'ipotesi dei residui e il valore di R quadro è a meta tra 0 e 1. Esiste una relazione positiva fra le due variabili. Il modello lineare non è il migliore a coglierla
##Kendall
cor(Peso,Cranio, method="kendall")#0.47

#grafico relazione lineare distinguendo per Sesso
ggplot(data=data)+
  geom_point(aes(x=Cranio,
                 y=Peso,
                 col=Sesso),position = "jitter")+
  geom_smooth(aes(x=Cranio,
                  y=Peso,
                  col=Sesso),se=F,method = "lm")

#relazione quadratica
mod_quad_cp<-lm(Peso~Cranio+I(Cranio^2), data=data)  

summary(mod_quad_cp)
#aumento dell'R ^2 
0.5056-0.4968

#
#anova
anova(mod_lin_cp,mod_quad_cp)
#rifiuto l'ipotesi nulla che mi porta a preferire il modello quadratico
#anche BIC e AIC preferiscono il quadratico
BIC(mod_lin_cp,mod_quad_cp)
AIC(mod_lin_cp,mod_quad_cp)
mse_train_1_cp<-MSE(data$Peso, fitted(mod_lin_cp))
mse_train_1_cp#138758.8
mse_train_2_cp<-MSE(data$Peso, fitted(mod_quad_cp))
mse_train_2_cp#136327.1
#secondo tutti i criteri di selezione è preferibile il modello di regressione quadratica tra Peso e Cranio

#grafico della relazione quadratica distinguendo per Sesso
ggplot(data=data)+
  geom_point(aes(x=Cranio,
                 y=Peso,
                 col=Sesso),position = "jitter")+
  geom_smooth(aes(x=Cranio,
                  y=Peso,
                  col=Sesso),se=F)+
  geom_line(aes(x=Cranio,
                y=mod_quad_cp$fitted.values,
                col=Sesso),linetype = "dashed")

##analisi con i glm
#poiché la variabile Peso non è normale, provo ad applicare un modello glm(family=gaussian e link=log)
glmLog_Peso_Cranio<- glm(Peso ~ Cranio, data = data, family = gaussian(link = "log"))
glmLog_Peso_Cranio#AIC 36770
glmLog_Peso_Cranio_quad<- glm(Peso ~ Cranio+I(Cranio^2), data = data, family = gaussian(link = "log"))
glmLog_Peso_Cranio_quad#AIC 36640
#confrontiamo 
BIC(mod_lin_cp,mod_quad_cp,glmLog_Peso_Cranio,glmLog_Peso_Cranio_quad)#è preferibile il modello quadratico (link=identity)
AIC(mod_lin_cp,mod_quad_cp,glmLog_Peso_Cranio,glmLog_Peso_Cranio_quad)#è preferibile il modello quadratico (link=identity)
mse_train_3cp<-MSE(data$Peso, fitted(glmLog_Peso_Cranio))
mse_train_3cp#144392.7
mse_train_4cp<-MSE(data$Peso, fitted(glmLog_Peso_Cranio_quad))
mse_train_4cp#136786.5
# è preferibile il modello quadratico (link=identity)

#calcolo R^2
1 - (deviance(glmLog_Peso_Cranio_quad)/glmLog_Peso_Cranio_quad$null.deviance)#R^2 0.5039573
#L'R^2 è maggiore per il modello quadratico (link=identity) 0.5052 


#Analisi n. settimane di gestazione e peso
par(mfrow=c(1,1))
plot(Gestazione,Peso)
cor(Gestazione,Peso)#0.5919592 indica una correlazione lineare positiva non molto forte
mod_lin_gp<-lm(Peso~Gestazione, data=data)  
mod_lin_gp$coefficients# bo=-3200.3715     b1= 166.3577

summary(mod_lin_gp)
#rifiuto l'ipotesi nulla che le stime siano significativamente uguali a zero. Ciò è importante soprattutto per b1. 
#R quadro  0.3504 indica che il modello lineare non è adatto
abline(mod_lin_gp,col=2)

par(mfrow=c(1,2))
plot(residuals(mod_lin_gp))
abline(h=mean(residuals(mod_lin_gp)))


plot(density(residuals(mod_lin_gp)))
#con alfa= 0.05
shapiro.test(mod_lin_gp$residuals)# rifiuto l'ipotesi di normalità dei residui

lmtest::bptest(mod_lin_gp)# non rifiuto l'ipotesi di omoschedasticità (è evidente anche dal grafico)
lmtest::dwtest(mod_lin_gp)#rifiuto l'ipotesi di indipendenza degli errori

#sono violate due ipotesi dei residui e il valore di R quadro è basso ad indicare che il modello lineare non è il migliore per stimare la relazione fra le due variabili. Esiste una relazione positiva fra le due variabili.
##Kendall
cor(Peso,Gestazione, method="kendall")#0.33
#relazione quadratica
mod_quad_gp<-lm(Peso~Gestazione+I(Gestazione^2), data=data)  

summary(mod_quad_gp)
#aumento dell'R ^2 
0.3621-0.3504
#anova
anova(mod_lin_gp,mod_quad_gp)
#rifiuto l'ipotesi nulla che mi porta a preferire il modello quadratico
#anche BIC e AIC preferiscono il quadratico
BIC(mod_lin_gp,mod_quad_gp)
AIC(mod_lin_gp,mod_quad_gp)
mse_train_1gp<-MSE(data$Peso, fitted(mod_lin_gp))
mse_train_1gp#179126.4
mse_train_2gp<-MSE(data$Peso, fitted(mod_quad_gp))
mse_train_2gp#175896.4
#secondo tutti i criteri di selezione è preferibile il modello di regressione quadratica tra Peso e Gestazione

##analisi con i glm
#poiché la variabile Peso non è normale, provo ad applicare un modello glm(family=gaussian e link=log)
glmLog_Peso_Gestazione<- glm(Peso ~ Gestazione, data = data, family = gaussian(link = "log"))
glmLog_Peso_Gestazione#AIC 37380
glmLog_Peso_Gestazione_quad<- glm(Peso ~ Gestazione+I(Gestazione^2), data = data, family = gaussian(link = "log"))
glmLog_Peso_Gestazione_quad#AIC 37250
#confrontiamo 
BIC(mod_lin_gp,mod_quad_gp,glmLog_Peso_Gestazione,glmLog_Peso_Gestazione_quad)#è preferibile il modello quadratico dei glm
AIC(mod_lin_gp,mod_quad_gp,glmLog_Peso_Gestazione,glmLog_Peso_Gestazione_quad)#è preferibile il modello quadratico dei glm
mse_train_3gp<-MSE(data$Peso, fitted(glmLog_Peso_Gestazione))
mse_train_3gp#184397.9
mse_train_4gp<-MSE(data$Peso, fitted(glmLog_Peso_Gestazione_quad))
mse_train_4gp#174576.5# è preferibile il modello quadratico dei glm

#calcolo R^2
1 - (deviance(glmLog_Peso_Gestazione_quad)/glmLog_Peso_Gestazione_quad$null.deviance)#R^2 0.3669157
#aumento R^2 rispetto al modello quadratico (link=identity)
0.3669157-0.3621



#Analisi N.gravidanze e Peso
par(mfrow=c(1,1))
plot(N.gravidanze,Peso)
cor(N.gravidanze,Peso)#0.0023 indica assenza di correlazione lineare
##Kendall
cor(Peso,N.gravidanze, method="kendall")#0.01 indica che non è presente né concordanza né discordanza


#ripetiamo i test per Anni.madre e peso
plot(Anni.madre,Peso)
cor(Anni.madre,Peso)#-0.02 indica assenza di correlazione lineare
##Kendall
cor(Peso,Anni.madre, method="kendall")#-0.004 indica che non è presente né concordanza né discordanza


#per le variabili qualitiative usa i boxplot  e calcoliamo il t test
#vediamo se è vera la distizione del Peso e fumo
boxplot(Peso~Fumatrici)# Il boxplot poco più basso per Fumatrici
#sarebbe però meglio da approfondire con un t test
#calcolo il t test per Il Peso considerando campioni indipendenti
t.test(data=data,
       Peso~Fumatrici,
       paired= FALSE,
       var.equal=T)

#accetto l'ipotesi nulla: le medie del Peso non sono significativamente diverse fra le due modalità del Fumo 

#vediamo se è vera la distizione del Peso  e Tipo parto
boxplot(Peso~Tipo.parto)# non sembra esserci una differenza significativa
#sarebbe però meglio da approfondire con un t test
#calcolo il t test per Il Peso considerando campioni indipendenti
t.test(data=data,
       Peso~Tipo.parto,
       paired= FALSE,
       var.equal=T)

#non rifiuto l'ipotesi nulla: le medie del Peso non sono significativamente diverse fra le due modalità 

#vediamo se è vera la distizione del Peso  e Ospedale
#  
boxplot(Peso~Ospedale)# non sembra esserci una differenza significativa
#sarebbe però meglio da approfondire con un t test
#calcolo il t test per Il Peso considerando campioni indipendenti
pairwise.t.test(Peso, Ospedale, 
                paired = F,
                pool.sd = T,
                p.adjust.method = "bonferroni")

#non rifiuto l'ipotesi nulla: le medie del Peso non sono significativamente diverse fra i tre ospedali



#########creo un dataset solo di variabili quantitative
dati_quant=data[,c(1,2,4,5,6,7)]
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- (cor(x, y))
  txt <- format(c(r, 1), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = 1.5)
}
#correlazioni
pairs(dati_quant,lower.panel=panel.cor, upper.panel=panel.smooth)
######
#analizzando questo grafico ho notato che:
#Anni madre e N. gravidanze rispetto a tutte le altre vraiabili prese singolarmente formano dei plot che sono nubi sparse e dunque non modellabili con delle funzioni
#al contrario Gestazione, Peso, Lunghezza, Cranio sembrano crescere insieme. Si intravedono delle curvature che mi spingono a protendere più per modelli quadratici che lineari

#definisco il grafico della matrice di correlazione che mostra le correlazioni che già abbiamo calcolato
#in più ne mostra una non prima analizzata fra Anni.Madre e N.gravidanze
#library(corrplot)
res <- cor(dati_quant)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#vediamo la relazione tra gestazione e fumo
boxplot(Gestazione~Fumatrici)# La media del boxplot più alto per le fumatrici
#sarebbe però meglio da approfondire con un t test
#calcolo il t test per Il Peso considerando campioni indipendenti
t.test(data=data,
       Gestazione~Fumatrici,
       paired= FALSE,
       var.equal=T)

#accetto l'ipotesi nulla e quindi il fumo non è una variabile che discrimina la Gestazione



#2)
mod<-lm(Peso~., data=data)
summary(mod)
#l'intercetta dove ricadono le modalità base line è significativamente diversa da zero
# i risultati ottenuti sono coerenti con quelli ottenuti applicando il modello lineare tra il Peso e ognuna delle variabili del dataset
# Sapevamo già che il Sesso era una variabile importante per spiegare il Peso, come anche Gestazione, Lunghezza e Cranio
#Anni madre e Fumatrici non hanno nessuna stella e abbiamo già visto dai plot che sono nubi sparse rispetto al Peso
#Numero di gravidanze ha una sola stella, ad indicare una significativa più debole nel spiegare la risposta come anche le modalità Parto naturale e Ospedale 3
#R quadro aggiustato è pari a 0.73 ad indicare che è questo modello è buono, ma si può perfezionare perché molto semplice

#vediamo se i regressori sono correlati fra di loro
vif(mod)
#guardando i generalized VIF non sembra esserci un problema di multicollinerarità. Ciò è un buon risultato


#3 
#provo a cancellare le variabili senza stella e vediamo se riesco ad alzare il valore dell'R quadro
mod1<-lm(Peso~.-Fumatrici-Anni.madre, data=data)
summary(mod1)
# non ho un miglioramento dell R^2 aggiustato, ma meno variabili quindi tengo questo modello
#BIC,AIC,Anova protendono per il modello più semplice mod1. Il MSE preferisce mod
BIC(mod,mod1)
AIC(mod, mod1)
anova(mod1,mod)
mse_mod<-MSE(data$Peso, fitted(mod))
mse_mod#74757.08
mse_mod1<-MSE(data$Peso, fitted(mod1))
mse_mod1#74808.45# 


#tolgo anche Ospedale e Tipo Parto
mod2<-lm(Peso~.-Fumatrici-Anni.madre-Ospedale-Tipo.parto, data=data)
summary(mod2)
# R^2 agg passa a 0.7265
0.7278 -0.7265
#per anova e AIC è meglio mod1: per BIC è meglio mod2
anova(mod2,mod1)#meglio mod1
BIC(mod,mod1,mod2)#meglio il modello più semplice mod2(sappiamo che tende a penalizzare di più)
AIC(mod, mod1,mod2)#meglio il mod1 
mse_mod2<-MSE(data$Peso, fitted(mod2))
mse_mod2#75277.04 


#quindi R^2 agg si è abbassato. Anova,MSE e Aic preferiscono mod1; BIC preferisce mod2

#tolgo numero di gravidanze

mod3<-lm(Peso~.-Fumatrici-Anni.madre-Ospedale-Tipo.parto-N.gravidanze, data=data)
summary(mod3)#R^2 agg si abbassa a 0.7257
0.7278-0.7257

anova(mod3,mod1)#meglio mod1
BIC(mod,mod1,mod2,mod3)#meglio mod2 quindi lasciare N.gravidanze
AIC(mod, mod1,mod2,mod3)#meglio mod1 quindi lasciare N.gravidanze che effetivamente aveva due stelle
mse_mod3<-MSE(data$Peso, fitted(mod3))
mse_mod3#75525.66


#stepwise
# la funzione implementata da R restutuisce il modello mod 2 come il migliore con BIC
MASS::stepAIC(mod,direction="both",k=log(N))

# la funzione implementata da R restutuisce il modello mod 1 come il migliore con AIC
MASS::stepAIC(mod,direction="both",k=2)


#4
#con interazioni delle variabili 
mod4<-update(mod2,~.+Lunghezza:Cranio:Gestazione, data=data)
summary(mod4)#   R^2 agg 0.7295

mod5<-update(mod2,~.+Lunghezza:Cranio:Gestazione:N.gravidanze, data=data)
summary(mod5)#  R^2  agg 0.7269


mod6<-update(mod2,~.+Cranio:Gestazione, data=data)
summary(mod6)#   R^2 agg 0.7301 

mod7<-update(mod2,~.+Cranio:Lunghezza, data=data)
summary(mod7)#   R^2 agg 0.7289


mod8<-update(mod2,~.+Gestazione:Lunghezza, data=data)
summary(mod8)# o  R^2 agg 0.7292


#in definitiva le interazioni migliorano la previsione e il modello migliore è mod6

#poiché in precedenza ho visto che esistono relazioni quadratiche da preferire alle linerare provo a considerarle

mod9<-update(mod2,~.+I(Gestazione^2),data=data)
summary(mod9)#R^2 agg 0.7269
mod10<-update(mod2,~.+I(Lunghezza^2),data=data)
summary(mod10)#R^2 agg 0.7363
mod11<-update(mod2,~.+I(Cranio^2),data=data)
summary(mod11)#R^2 agg 0.7301 che è il massimo prima raggiunto con le interazioni
#mettiamole tutte
mod12<-update(mod2,~.+I(Cranio^2)+I(Gestazione^2)+(Lunghezza^2),data=data)
summary(mod12)#R^2 agg cresce ancora a 0.7303
#considero le interazioni e le relazioni quadratiche insieme
mod13<-update(mod2,~.+I(Cranio^2)+I(Gestazione^2)+(Lunghezza^2)+Cranio:Gestazione,data=data)
summary(mod13)#0.7319

mod14<-update(mod2,~.+I(Cranio^2)+I(Gestazione^2)+(Lunghezza^2)+I(Cranio^2):I(Gestazione^2),data=data)
summary(mod14)#0.7312

mod15<-update(mod2,~.+I(Cranio^2)+I(Gestazione^2)+(Lunghezza^2)+I(Cranio^2):I(Gestazione^2):I(Lunghezza^2),data=data)
summary(mod15)#0.7315

mod16<-update(mod2,~.+I(Cranio^2)+I(Gestazione^2)+(Lunghezza^2)+Cranio:Gestazione:Lunghezza,data=data)
summary(mod16)#0.7313

mod17<-update(mod2,~.+I(Cranio^2)+I(Gestazione^2)+(Lunghezza^2)+Lunghezza:Gestazione,data=data)
summary(mod17)#0.7327

mod18<-update(mod2,~.+I(Cranio^2)+I(Gestazione^2)+(Lunghezza^2)+I(Lunghezza^2):Gestazione,data=data)
summary(mod18)#0.7359

mod19<-update(mod2,~.+I(Cranio^2)+I(Gestazione^2)+(Lunghezza^2)+I(Gestazione^2):Lunghezza,data=data)
summary(mod19)#0.7322

mod20<-update(mod2,~.+I(Cranio^2)+I(Gestazione^2)+(Lunghezza^2)+I(Lunghezza^2):Cranio:Gestazione,data=data)
summary(mod20)#0.7332

mod21<-update(mod2,~.+I(Cranio^2)+I(Gestazione^2)+(Lunghezza^2)+I(Lunghezza^2):I(Cranio^2):Gestazione,data=data)
summary(mod21)#0.7317

mod22<-update(mod2,~.+I(Gestazione^2)+(Lunghezza^2)+I(Lunghezza^2):Gestazione,data=data)
summary(mod22)#0.7356

mod23<-update(mod2,~.+I(Cranio^2)+(Lunghezza^2)+I(Lunghezza^2):Gestazione,data=data)
summary(mod23)#0.732

mod24<-update(mod2,~.+I(Cranio^2)+I(Gestazione^2)+(Lunghezza^2)+I(Cranio^2):I(Lunghezza^2),data=data)
summary(mod24)#0.7327

mod25<-update(mod2,~.+I(Cranio^2)+I(Gestazione^2)+I(Lunghezza^2)+I(Cranio^2):Gestazione:I(Lunghezza^2),data=data)
summary(mod25)#0.7397

mod26<-update(mod2,~.+I(Cranio^2)+I(Gestazione^2)+I(Lunghezza^2)+I(Cranio^2):I(Gestazione^2):I(Lunghezza^2),data=data)
summary(mod26)#0.7395
mod27<-update(mod2,~.+I(Cranio^2)+I(Gestazione^2)+I(Lunghezza^2)+I(Cranio^2):Gestazione:I(Lunghezza^2)+N.gravidanze:Gestazione,data=data)
summary(mod27)#0.7399
mod28<-update(mod2,~.+I(Cranio^2)+I(Gestazione^2)+I(Lunghezza^2)+I(Cranio^2):I(Gestazione^2)+I(Cranio^2):I(Lunghezza^2)+N.gravidanze:Gestazione,data=data)
summary(mod28)#0.742
mod29<-update(mod2,~.+I(Cranio^2)+I(Gestazione^2)+I(Lunghezza^2)+I(Cranio^2):Gestazione+I(Cranio^2):Lunghezza+N.gravidanze:Gestazione,data=data)
summary(mod29)#0.7424
mod30<-update(mod2,~.+I(Cranio^2)+I(Gestazione^2)+I(Lunghezza^2)+I(Cranio^2):Gestazione+I(Cranio^2):Lunghezza,data=data)
summary(mod30)#0.7424 meno variabili quindi megliore
mod31<-update(mod2,~.+I(Cranio^2)+I(Gestazione^2)+I(Lunghezza^2)+I(Lunghezza^2):Gestazione+I(Cranio^2):Lunghezza,data=data)
summary(mod31)#0.7403 
mod32<-update(mod2,~.+I(Cranio^2)+I(Gestazione^2)+I(Lunghezza^2)+I(Lunghezza^2):Gestazione+I(Lunghezza^2):Cranio,data=data)
summary(mod32)#0.7402
mod33<-update(mod2,~.+I(Cranio^2)+I(Gestazione^2)+I(Lunghezza^2)+I(Cranio^2):Lunghezza,data=data)
summary(mod33)#0.7403 
mod34<-update(mod2,~.+I(Cranio^2)+I(Gestazione^2)+I(Lunghezza^2)+I(Cranio^2):Gestazione+I(Cranio^2):Lunghezza+I(Lunghezza^2):Gestazione,data=data)
summary(mod34)#0.7427 
mod35<-update(mod2,~.+I(Cranio^2)+I(Gestazione^2)+I(Lunghezza^2)+I(Gestazione^2):Cranio+I(Lunghezza^2):Cranio+I(Gestazione^2):Lunghezza,data=data)
summary(mod35)#0.7424
mod36<-update(mod1,~.+I(Lunghezza^2)+Cranio:Gestazione,data=data)
summary(mod36)#0.7424 meno variabili quindi megliore
mod37<-update(mod1,~.+I(Cranio^2)+I(Gestazione^2)+I(Lunghezza^2)+I(Cranio^2):Gestazione+I(Cranio^2):Lunghezza,data=data)
summary(mod37)#0.7439 


#
#vediamo il miglior modello in base ai diversi criteri

a=BIC(mod,mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8,mod9,mod10,mod11,mod12,mod13,mod14,mod15,mod16,mod17,mod18,mod19,mod20,mod21,mod22,mod23,mod24,mod25,mod26,mod27,mod28,mod29,mod30,mod31,mod32,mod33,mod34,mod35,mod36,mod37)
a=as.data.frame(a)
min(a$BIC)
a
#meglio il modello mod30 per BIC
b=AIC(mod,mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8,mod9,mod10,mod11,mod12,mod13,mod14,mod15,mod16,mod17,mod18,mod19,mod20,mod21,mod22,mod23,mod24,mod25,mod26,mod27,mod28,mod29,mod30,mod31,mod32,mod33,mod34,mod35,mod36,mod37)
b=as.data.frame(b)
min(b$AIC)
b
#meglio il modello mod37 per AIC


#5)RESIDUI
#considero mod30
par(mfrow=c(2,2))
plot(mod30)


#secondo le ipotesi del modello, i residui dovrebbero distribuirsi come una nube sparsa con media zero.
#dal primo grafico è evidente che i residui hanno media zero intorno ad una funzione che è quasi una retta orizzontale di valore zero
#qui la nube è sparsa in maniera differente fra i valori più bassi e più alti:i valori più frequenti sono quelli del range 2500-4000
#quasi tutti i residui, tranne gli estremi, si distribuiscono lungo la bisettrice del qq plot
#dal terzo grafico non sembra essere omoschedasticità
#dal quarto grafico c'è un valore che supera la soglia di 0.5, ma la gran parte dei valori è dentro le soglie

#osservazioni evidenziate nei grafici
my_data[155,]
my_data[1306,]
my_data[1551,]
my_data[1399,]

mean(data$Peso)#3284

tapply(Peso, Sesso, mean)#una femmina pesa mediamente 3161, un maschio 3408.496 
#effetivamente l'osservazione 1551 è molto particolare, potremmo dire non standard
#infatti la bambina ha un peso molto elevato
#Abbiamo visto che le femmine hanno un peso inferiore ai maschi
# il peso di questa bambina supera anche quello medio dei maschi a 38 settimane
#oss 155 e 1306 hanno un peso  più alto della media. L'osservazione 1306 è anche femmina
my_data[1399,]#per questo neonato maschio il peso è inferiore alla media 


lmtest::bptest(mod30)# Rifiuto l'omoschedasticità come era evidente anche dal grafico
lmtest::dwtest(mod30)#non rifiuto l'incorrelazione
shapiro.test(mod30$residuals)#rifiuto la normalità
plot(density(residuals(mod30)))#dal grafico sembra esserci la normalità, ma il test mi porta a dover rifiutare l'ipotesi nulla
skewness(residuals(mod30))# leggera asimmetrica a dx


#due ipotesi su tre dei residui sono violate
#leverage
lev<-hatvalues(mod30)
plot(lev)
p<-sum(lev)
n<-length(lev)
soglia=2*p/n
abline(h=soglia,col=2)
leverage=lev[lev>soglia]
length(leverage)#162 valori sono di leva



#outliers
plot(rstudent(mod30))
abline(h=c(-2,2))
car::outlierTest(mod30)
#con la correzione di Bonferroni ci sono 4 punti che sono outlier

#distanza di cook
cook<-cooks.distance(mod30)
plot(cook,ylim = c(0,1)) 
cook
table((cook>=0.5))
which((cook>=0.5))

#1551 

cook[1549]#2.409388  


#è il valore evidente dal quarto grafico

#quindi non sono verificate tutte le ipotesi dei residui




#provo a cancellare le osservazioni anomale. 
#La posizione è differente perchè ho cancellato due osservazioni nel dataset chiamato data

my_data[1306,]==data[1305,]
my_data[155,]==data[155,]
my_data[1399,]==data[1397,]
my_data[1694,]==data[1692,]
my_data[1551,]==data[1549,]


data2=data[-c(1305,155,1397,1692,1551),]
#ristimo il modello con i regressori di mod30
mod38<-lm(Peso ~ N.gravidanze + Gestazione + Lunghezza + Cranio + 
            Sesso + I(Cranio^2) + I(Gestazione^2) + I(Lunghezza^2) + 
            Gestazione:I(Cranio^2) + Lunghezza:I(Cranio^2),data=data2)
summary(mod38)#0.7509
#è aumento l'R^2 agg ad indicare che questi valori hanno un'influenza sulle stime
#rifacciamo il test dei residui e ottengo gli stessi risultati

lmtest::bptest(mod38)# Rifiuto l'omoschedasticità come era evidente anche dal grafico
lmtest::dwtest(mod38)#non rifiuto l'incorrelazione
shapiro.test(mod38$residuals)#rifiuto la normalità
plot(density(residuals(mod38)))#dal grafico sembra esserci la normalità, ma il test mi porta a dover rifiutare l'ipotesi nulla
skewness(residuals(mod38))# leggera asimmetrica a dx




#6)
#in definitiva il modello scelto mod30 ha un R^2 aggiustato pari a 0.7424 che può migliore a 0.7509 eliminado le assorvazioni più anomale
#Il valore dell'R^2 ha un valore massimo pari ad 1, quindi il modello è abbastanza buono per prevedere il peso dei neonati
#ricordiamo però che non sono verificate tutte le ipotesi. In particolare:
#la variabile Peso non è normale
#i residui non sono normali e sono eteroschedastici
#dai grafici sembra che l'eteroschedasticità sia più forte della non normalità
# Posso dire che il modello scelto è abbastanza buono nonostante queste imperfezioni
#si tratta anche di un modello che è in grado di cogliere effetti quadratici e interazioni fra variabili
#un'imperfezione si vede dai seguenti grafici
par(mfrow=c(1,2))
plot( fitted(mod),data$Peso)
plot( fitted(mod30),data$Peso)
#il modello mod30 tende ad aumentare  anche se di poco il valore della stima del Peso per valori più bassi
#rispetto a mod ho comunque ottenuto un miglioramento

#se però vogliamo migliorare il modello posso provare ad applicare i glm 
glm_tot<- glm(Peso ~ ., data = data, family = gaussian(link = "log"))
glm_tot#AIC 35140
#stepwise
#i regressori restituiti sono gli stessi dei modelli scelti precedentemente
MASS::stepAIC(glm_tot,direction="both",k=log(N))
MASS::stepAIC(glm_tot,direction="both",k=2)

#applico un modello glm con i regressori di mod30
glmLog_bic<- glm(Peso ~ N.gravidanze + Gestazione + Lunghezza + Cranio + 
               Sesso + I(Cranio^2) + I(Gestazione^2) + I(Lunghezza^2) + 
               Gestazione:I(Cranio^2) + Lunghezza:I(Cranio^2), data = data, family = gaussian(link = "log"))
glmLog_bic#AIC 35010
#confronto i due R^2 dato che nell'output dei glm non c'è R^2 aggiustato
#R^2 è maggiore per mod30
1 - (deviance(glmLog_bic)/glmLog_bic$null.deviance)#R^2 0.7432913
summary(mod30)#0.7434 

BIC(mod30,glmLog_bic)#è preferibile mod30
AIC(mod30,glmLog_bic)#è preferibile mod30
mse_mod30<-MSE(data$Peso, fitted(mod30))
mse_mod30#70750.49
mse_modglm<-MSE(data$Peso, fitted(glmLog_bic))
mse_modglm#70788.82
#è preferibile mod 30 secondo MSE



#stimo il modello con i regressori di mod37
glmLog_aic<- glm(Peso ~ N.gravidanze + Gestazione + Lunghezza + Cranio + 
                   Tipo.parto + Ospedale + Sesso + I(Cranio^2) + I(Gestazione^2) + 
                   I(Lunghezza^2) + Gestazione:I(Cranio^2) + Lunghezza:I(Cranio^2), data = data, family = gaussian(link = "log"))
glmLog_aic#AIC 35000
#confronto i due R^2 dato che nell'output dei glm non c'è R^2 aggiustato
#R^2 è maggiore per mod37
1 - (deviance(glmLog_aic)/glmLog_aic$null.deviance)#R^2 0.7450982
summary(mod37)#0.7452#
#confronto
BIC(mod30,mod37,glmLog_bic,glmLog_aic)#è preferibile mod30
AIC(mod30,mod37,glmLog_bic,glmLog_aic)#è preferibile mod37
mse_mod37<-MSE(data$Peso, fitted(mod37))
mse_mod37#70261.14
mse_modglm_aic<-MSE(data$Peso, fitted(glmLog_aic))
mse_modglm_aic#70290.55
#per il MSE è preferibile mod37
#In definitiva i modelli migliori ottenuti sono  mod30 e mod37

#7
#ci sono due possibilità
#1) trovo un modello solo con le vraiabili disponibili
#2) Applico uno dei 4 modelli scelti usando valori plausibili

#Vediamo la prima opzione
#cerco il miglior modello con i regressori richiesti

mod40<-lm(Peso~.-Fumatrici-Anni.madre-Ospedale-Tipo.parto-Lunghezza-Cranio-Ospedale, data=data)
summary(mod40)#0.3778
mod41<-update(mod40,~.+Gestazione:N.gravidanze)
summary(mod41)#0.3776 mreglio il modello precedente
plot(Peso, N.gravidanze)
mod42<-update(mod40,~.+I(Gestazione^2))
summary(mod42)#0.3909
mod43<-update(mod40,~.+I(Gestazione^2):N.gravidanze)
summary(mod43)#0.3776

predict(mod42,data.frame(N.gravidanze=3,Sesso="F",Gestazione=39))#3271.668
#rimane comunque unvalore basso dell'R^2 in quanto togliendo le altre variabili prima significative stiamo
#perdendo informazione
#l'ideale è avere anche i valori di queste altre variabili



#metodo più raffinato usando valori stimati delle ecografie
#seleziono le unità che hanno questi valori

dati3=data[which(Sesso=="F"& Gestazione==39 &N.gravidanze==3),]
summary(dati3)

#calcolo le mode per le var qualitative e le medie per le quantitaive
#mode
table(dati3$Anni.madre)#moda 30
#fumatrici 0
#tipo parto nat
#table(dati3$Ospedale)#osp 1

#medie
#lunghezza=490.3 
#Cranio=340.2 


newdata = data.frame(N.gravidanze=3,Sesso="F",Gestazione=39,Lunghezza=490.3,Cranio=340.2,Fumatrici=0,Tipo.parto="Nat",Ospedale="osp1")
predict(mod30,newdata = newdata)#3213.833

#penso che sia una stima migliore perchè il modello è più raffinato

#secondo mod37

predict(mod37,newdata = newdata)#3213.292 


#i valori sono simili
#media dei valori stimati dai 4 modelli
mean(c(3213.833,3213.292))# 3213.562


#8

#install.packages("rgl")
#library(rgl)
car::scatter3d(Peso~Lunghezza+Cranio)#grafico in 3 dimensioni
scatter3d(Peso~Lunghezza+Cranio+Sesso)#se aggiungo il Sesso, vedo che il piano dei maschi in rosa è più in alto
scatter3d(Peso~Gestazione+Cranio+Sesso)
scatter3d(Peso~Gestazione+Lunghezza+Sesso)





