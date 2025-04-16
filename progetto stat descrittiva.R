
####1)carico i dati
my_data <- read.csv("realestate_texas - Copia.csv", sep=",")  
attach(my_data)

#librerie
library(ggplot2)

library(dplyr)
library(moments)
library(gridExtra)
library(gghalves)

###2) tipi di variabili
#city = qualitativa nominale
#year = quantitativa continua (su scala di intervalli)
#month = qualitativa ma espressa in numeri
#sales = quantitativa discreta (su scala di rapporti)
#volume = quantitativa continua (su scala di rapporti)
#median = quantitaiva continua anche se assume solo valori interi (su scala di rapporti)
#listing = quantitativa discreta (su scala di rapporti)
#month inventory = quantitativa continua (su scala di rapporti)


###3)moda per le variabili qualitative
#Le seguenti variabili non hanno la moda in quanto tutte le modalità hanno la stessa frequenza osservata
table(city)
table(year)
table(month)


##median
#non ha senso calcolare la mediana per le varaibili city e month, allora le rimuovo
#per  calcolare la mediana
#library(dplyr)
mydata2 = select(my_data, -1, -3)

for (i in 1:ncol(mydata2)){
  
  # calculo mediana
  median_val <- median(mydata2[,i])
  cat(names(mydata2[i]), ": ",median_val,"\n")
}

#quantili
quantile(year)
quantile(sales)

for (i in 1:ncol(mydata2)){
  
  # calculo quantili
  quantile_val <- quantile(mydata2[,i])
  cat(names(mydata2[i])," 0%  25%  50%  75% 100% : ","\n" ,quantile_val,"\n")
}

#decili

quantile(mydata2$sales,seq(0,1,0.1))

for (i in 1:ncol(mydata2)){
  
  # calculo decili
  decile_val <- quantile(mydata2[,i],seq(0,1,0.1))
  cat(names(mydata2[i])," 0%   10%   20%   30%   40%   50%   60%   70%   80%   90%  100%  : ","\n" ,decile_val,"\n")
}

#summary delle variabili
summary(sales)
summary(volume)
summary(median_price)
summary(listings)
summary(months_inventory)


##indici di variabilità
#creo un nuovo dataframe
mydata3 = select(mydata2, -1 )

#range o intervallo di variazione
max(mydata3[,2])-min(mydata3[,2])
#oppure
diff(range(mydata3[,2]))

for (i in 1:ncol(mydata3)){
  
  # calculo range
  range_val <- max(mydata3[,i])-min(mydata3[,i])
  cat("range",names(mydata3[i]) ,":",range_val,"\n")
}
#IQR
quantile(mydata3$volume, na.rm=TRUE)
IQR(mydata3$volume, na.rm=TRUE)

for (i in 1:ncol(mydata3)){
  
  # calculating range of ith column
 quantile_val <- quantile(mydata3[,i], na.rm=TRUE)
 iqr_val <-IQR(mydata3[,i], na.rm=TRUE)
  cat("IQR",names(mydata3[i]) ,":",iqr_val,"\n")
}



##varianza e sd
var(mydata3$sales)
sd(mydata3$sales)

for (i in 1:ncol(mydata3)){
  
  # calcolo varianza 
  var_val <- var(mydata3[,i])
  cat("varianza",names(mydata3[i]) ,":",var_val,"\n")
}

for (i in 1:ncol(mydata3)){
  
  # calculo sd 
  sd_val <- sd(mydata3[,i])
  cat("sd",names(mydata3[i]) ,":",sd_val,"\n")
}



#forma
#library(moments)

skewness(mydata3$sales)
for (i in 1:ncol(mydata3)){
  
  # calculo asimmetria 
  forma_val <- skewness(mydata3[,i])
  
  cat("asimmetria",names(mydata3[i]) ,":",forma_val,"\n")
}

#la var più asimmetrica è volume

#4)

#per un confronto della variabilità
CV<-function(x){
  return(sd(x)/mean(x)*100)
}
 CV(mydata3$sales)
 
 
 for (i in 1:ncol(mydata3)){
   
   # calculo coeff di variazione 
   cv_val <- (sd(mydata3[,i])/mean(mydata3[,i])*100)
   
   cat("cv",names(mydata3[i]) ,":",cv_val,"\n")
 }
  
#la variabile più variabile è volume
 
 #5
 min(my_data$sales)
 max(my_data$sales)
 
 classi<-cut(sales,breaks=5)

 N=dim(my_data)[1]
 ni<-table(classi)
 fi<-ni/N
 Ni<-cumsum(ni)
 Fi<-Ni/N
 cbind(ni,fi,Ni,Fi)
 distr_freq<-as.data.frame(cbind(ni,fi,Ni,Fi))
 fi2 = fi^2
J=nrow(distr_freq)
gini = 1-sum(fi2)
gini.norm = gini/((J-1)/J)#0.9132812 l'eterogeneità è elevata

barplot(distr_freq$ni,
        xlab = "Classi",
        ylab = "Frequenze assolute",
        names.arg = rownames(distr_freq),
        ylim = c(0,90),
        col="blue")

title(main="Sales")

#eterogeneità di gini
gini.index <- function(x){
  ni = table(x)
  fi = ni/length(x)
  fi2 = fi^2
  J = length(table(x))
  
  gini = 1-sum(fi2)
  gini.norm = gini/((J-1)/J)
  
  return(gini.norm)
}

#massima eterogeneità, lo abbiamo già visto da table
gini.index(city)#1
gini.index(month)#1
gini.index(year)#1


#7
table(city)
 #probabilità che la città sià Beaumont è 
60/240#25%
#probabilità che il mese sia luglio è 
table(month)
20/240#8.3%
#probabilità che il mese sia luglio è 
dati_2012= filter(my_data,year==2012 & month==12)
4/240#1,66%

#8
my_data$mean_price<-(my_data$volume*1000000)/my_data$sales
#9
my_data$efficacia<-round((my_data$listings)/my_data$sales, digits=1)


# #10
#city
group_city<-my_data %>%
  
  
  group_by(city) %>%
  
  
  summarise(media_sales=mean(sales),
            sd_sales=sd(sales),
            media_volume=mean(volume),
            sd_volume=sd(volume)
            )
            
group_city         


# year
group_year<-my_data %>%
  
  
  group_by(year) %>%
  
  
  summarise(media_sales=mean(sales),
            sd_sales=sd(sales),
            media_volume=mean(volume),
            sd_volume=sd(volume)
  )

group_year

#mesi
group_mesi<-my_data %>%
  
  
  group_by(month) %>%
  
  
  summarise(media_sales=mean(sales),
            sd_sales=sd(sales),
            media_volume=mean(volume),
            sd_volume=sd(volume)
  )

group_mesi



#grafici
#1)

ggplot(data = my_data)+
  geom_boxplot(aes(
    x=city,
    y=median_price),
    fill="lightblue")+
labs(title="Boxplot",
     x ="Città", y = "Prezzo mediano")+
theme(plot.title = element_text(hjust = 0.5))
#Bryan-College Station  è la città con media più alta fra le 4 seguita da Tyler, Beaumont e Wichita Falls
#Sono presenti outliers in tre boxplot
#nessuna variabile è perfettamente simmetrica.La più asimmetrica è Bryan-College Station
tapply(median_price,city,skewness )
# non è molto evidente capire quale boxplot  presenti la maggiore variabilità, mi aiuto allora con la seguente formula
tapply(median_price,city,sd )

#2)
ggplot(data = my_data)+
  geom_boxplot(aes(
    x=city,
    y=volume),
    fill="lightblue")+
  labs(title="Boxplot",
       x ="Città", y = "Valore totale vendite")+
  theme(plot.title = element_text(hjust = 0.5))
#dal plot sembra che Bryan College Station sia la più variabile, asimmetrica e con un maggiore intervallo di variazione
tapply(volume,city,sd )
tapply(volume,city,skewness )
tapply(volume,city,IQR )
# trasformo prima gli anni in factor
my_data$anni_factor= as.factor(my_data$year)
attach(my_data)
ggplot(data = my_data)+
  geom_boxplot(aes(
    x=anni_factor,
    y=volume),
    fill="lightblue")+
  labs(title="Boxplot",
       x ="Anni", y = "Valore totale vendite")+
  theme(plot.title = element_text(hjust = 0.5))

# Con il passare degli anni è aumentata la variabilità e l'intervallo di variazione
#maggiori dettagli usando la funzione tapply

tapply(volume,anni_factor,sd )
tapply(volume,anni_factor,skewness )
tapply(volume,anni_factor,IQR )
tapply(volume,anni_factor,mean)


#2010 e 2011 si somigliano a livello di simmetria come 2012 e 2013. 2014 è la più simmetrica
tapply(volume,anni_factor,skewness )
tapply(volume,anni_factor,IQR )

#insieme
ggplot(data = my_data)+
  geom_boxplot(aes(
    x=anni_factor,
    y=volume,
    fill=city))+
  labs(title="Boxplot",
       x ="Anni", y = "Valore totale vendite", fill="Città")+
  theme(plot.title = element_text(hjust = 0.5))
#con il passare degli anni non sono cambiate le differenze fra le città rispetto alla variabile volume
#Ad esempio Witchita Falls rimane la città più economica, seguita da Beaumont e  Bryan-College Station. Tyler è la più costosa
#La città Bryana_College Station è la più variabile ogni anno
#Le città con maggiore IQR sono Bryan-College Station e Tyler 8dipende dall'anno)
#La distribuzione più asimmetrica è quella del 2012 nella città di Beaumont
tapply(volume,list(anni_factor,city),sd )
tapply(volume,list(anni_factor,city),min)
tapply(volume,list(anni_factor,city),max)
tapply(volume,list(anni_factor,city),IQR)
tapply(volume,list(anni_factor,city),skewness)

#boxplot con grafico sull'asimmetria
#library(gghalves)

ggplot(data=my_data)+theme_classic()+
  geom_half_violin(mapping = aes(x=city,y=volume),
                   side = "l",fill="lightblue")+
  geom_half_boxplot(mapping = aes(x=city,y=volume),
                    side = "r",fill="pink")+
  labs(title="Boxplot e distribuzioni",
       x ="Città", y = "Valore totale vendite")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data=my_data)+theme_classic()+
  geom_half_violin(mapping = aes(x=anni_factor,y=volume),
                   side = "l",fill="lightblue")+
  geom_half_boxplot(mapping = aes(x=anni_factor,y=volume),
                    side = "r",fill="pink")+
  labs(title="Boxplot e distribuzioni",
       x ="Anni", y = "Valore totale vendite")+
  theme(plot.title = element_text(hjust = 0.5))

#3
  my_data$mesi_factor= as.factor(my_data$month)
attach(my_data)

#considero i 5 anni
p_5anni<-ggplot(data=my_data, aes(x=mesi_factor, y=sales, fill=city)) +
  geom_bar(stat="identity")+
  labs(title="Totale delle vendite nei mesi",
       x ="Mesi", y = " Totale vendite", fill = "Città")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette="Paired")
p_5anni

# Nei mesi primaveri ed estivi c'è un aumneto delle vendite. La città con più vendite è Tyler
#Wichita con meno vendite nei mesi
#non riusciamo ad apprezzare le differenze anno per anno

#grafico normalizzato
p_normalizzato<-ggplot(data=my_data)+
  geom_bar(
    aes(x=mesi_factor, y=sales, fill=city),position="fill",stat="identity") +
  
  scale_y_continuous(breaks = seq(0,1,0.1))+
  labs(title="Totale delle vendite nei mesi (grafico normalizzato)",
       x ="Mesi", y = "Totale vendite", fill = "Città")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette="Paired")


p_normalizzato

#considero l'anno 2010

p_2010<-ggplot(subset(my_data,anni_factor %in% "2010"), aes(x=mesi_factor, y=sales, fill=city)) +
  geom_bar(stat="identity")+
labs(title="2010",
        x ="Mesi", y = " Totale Vendite", fill = "Città")+
  
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette="Paired")
  
p_2010



#considero l'anno 2011

p_2011<-ggplot(subset(my_data,anni_factor %in% "2011"), aes(x=mesi_factor, y=sales, fill=city)) +
  geom_bar(stat="identity")+
  labs(title="2011",
       x ="Mesi", y = "Totale Vendite", fill = "Città")+
  
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette="Paired")

p_2011



#considero l'anno 2012

p_2012<-ggplot(subset(my_data,anni_factor %in% "2012"), aes(x=mesi_factor, y=sales, fill=city)) +
  geom_bar(stat="identity")+
  labs(title="2012",
       x ="Mesi", y = "Totale Vendite", fill = "Città")+
  
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette="Paired")

p_2012

#
#considero l'anno 2013

p_2013<-ggplot(subset(my_data,anni_factor %in% "2013"), aes(x=mesi_factor, y=sales, fill=city)) +
  geom_bar(stat="identity")+
  labs(title="2013",
       x ="Mesi", y = "Totale Vendite", fill = "Città")+
  
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette="Paired")

p_2013

#considero l'anno 2014

p_2014<-ggplot(subset(my_data,anni_factor %in% "2014"), aes(x=mesi_factor, y=sales, fill=city)) +
  geom_bar(stat="identity")+
  labs(title="2014",
       x ="Mesi", y = " Totale Vendite", fill = "Città")+
  
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette="Paired")

p_2014

#visulaizzo i grafici insieme
#library(gridExtra)
grid.arrange(p_2010, p_2011,p_2012,p_2013,p_2014, ncol=3)

#grafici normalizzati

#2010 normalizzato
p_normalizzato_2010<-ggplot(subset(my_data,anni_factor %in% "2010"))+
  geom_bar(
    aes(x=mesi_factor, y=sales, fill=city),position="fill",stat="identity") +
  
  scale_y_continuous(breaks = seq(0,1,0.1))+
  labs(title="2010 (grafico normalizzato)",
       x ="Mesi", y = "Totale Vendite", fill = "Città")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette="Paired")


p_normalizzato_2010
#2011 normalizzato
p_normalizzato_2011<-ggplot(subset(my_data,anni_factor %in% "2011"))+
  geom_bar(
    aes(x=mesi_factor, y=sales, fill=city),position="fill",stat="identity") +
  
  scale_y_continuous(breaks = seq(0,1,0.1))+
  labs(title="2011 (grafico normalizzato)",
       x ="Mesi", y = "Totale Vendite", fill = "Città")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette="Paired")


p_normalizzato_2011
#2012 normalizzato
p_normalizzato_2012<-ggplot(subset(my_data,anni_factor %in% "2012"))+
  geom_bar(
    aes(x=mesi_factor, y=sales, fill=city),position="fill",stat="identity") +
  
  scale_y_continuous(breaks = seq(0,1,0.1))+
  labs(title="2012 (grafico normalizzato)",
       x ="Mesi", y = "Totale Vendite", fill = "Città")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette="Paired")


p_normalizzato_2012

#2013 normalizzato
p_normalizzato_2013<-ggplot(subset(my_data,anni_factor %in% "2013"))+
  geom_bar(
    aes(x=mesi_factor, y=sales, fill=city),position="fill",stat="identity") +
  
  scale_y_continuous(breaks = seq(0,1,0.1))+
  labs(title="2013 (grafico normalizzato)",
       x ="Mesi", y = "Totale Vendite", fill = "Città")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette="Paired")


p_normalizzato_2013

#2014 normalizzato
p_normalizzato_2014<-ggplot(subset(my_data,anni_factor %in% "2014"))+
  geom_bar(
    aes(x=mesi_factor, y=sales, fill=city),position="fill",stat="identity") +
  
  scale_y_continuous(breaks = seq(0,1,0.1))+
  labs(title="2014 (grafico normalizzato)",
       x ="Mesi", y = "Totale Vendite", fill = "Città")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette="Paired")


p_normalizzato_2014




#tabellina
a= aggregate(sales ~ year+city, my_data,sum)
tabella=as.data.frame(a)

##SERIE STORICHE
p_tabella<-ggplot(tabella, aes(x=year, y=sales, color=city)) +
  ggtitle("Serie storica delle vendite per anni e città")+
  labs(title="Serie storica delle vendite per anni e città",
       x ="Anni", y = "Numero totale vendite", color="Città")+
  geom_line(aes(color=city))+
  geom_point(aes(color=city))
  
p_tabella

#serie storica nel 2010
dati_2010= filter(my_data, year==2010)
dati_2010


p_2010_lc<-ggplot(dati_2010, aes(x=mesi_factor, y=sales, group=city, color=city)) +
  
  labs(title="Serie storica delle vendite per mesi nel 2010",
       x ="Mesi", y = "Numero totale vendite",color="Città")+

  geom_line(aes(color=city))+
  geom_point(aes(color=city))
p_2010_lc

#2011
dati_2011= filter(my_data, year==2011)
dati_2011


p_2011_lc<-ggplot(dati_2011, aes(x=mesi_factor, y=sales, group=city, color=city)) +
  
  labs(title="Serie storica delle vendite per mesi nel 2011",
       x ="Mesi", y = "Numero totale vendite",color="Città")+
  guides(group=guide_legend("City"))+
  geom_line(aes(color=city))+
  geom_point(aes(color=city))
p_2011_lc

#2012
dati_2012= filter(my_data, year==2012)
dati_2012


p_2012_lc<-ggplot(dati_2012, aes(x=mesi_factor, y=sales, group=city, color= city)) +
  
  labs(title="Serie storica delle vendite per mesi nel 2012",
       x ="Mesi", y = "Numero totale vendite",color="Città")+
  guides(group=guide_legend("City"))+
  geom_line(aes(color=city))+
  geom_point(aes(color=city))
p_2012_lc

#2013
dati_2013= filter(my_data, year==2013)
dati_2013


p_2013_lc<-ggplot(dati_2013, aes(x=mesi_factor, y=sales, group=city, color=city)) +
  
  labs(title="Serie storica delle vendite per mesi nel 2013",
       x ="Mesi", y = "Numero totale vendite",color="Città")+
  guides(group=guide_legend("City"))+
  geom_line(aes(color=city))+
  geom_point(aes(color=city))
p_2013_lc

#2014
dati_2014= filter(my_data, year==2014)
dati_2014


p_2014_lc<-ggplot(dati_2014, aes(x=mesi_factor, y=sales, group=city, color=city)) +
  
  labs(title="Serie storica delle vendite per mesi nel 2014",
       x ="Mesi", y = "Numero totale vendite", color="Città")+
  guides(group=guide_legend("Città"))+
  geom_line(aes(color=city))+
  geom_point(aes(color=city))
p_2014_lc
#library(gridExtra)
grid.arrange(p_2010_lc, p_2011_lc,p_2012_lc,p_2013_lc,p_2014_lc, ncol=3)
