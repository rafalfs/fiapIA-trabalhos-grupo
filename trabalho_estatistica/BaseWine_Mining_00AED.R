

install.packages("psych")
install.packages("plotly")
install.packages("gmodels")
install.packages("corrgram")



# mostrar até 2 casas decimais
options("scipen" = 2)

# Ler arquivo csv


Vinhos <- read.csv2("C:/BaseWine_Red_e_White2018.csv", row.names=1)
 
fix(Vinhos)
#mostrar as variáveis
str(Vinhos)
#mostra as variáveis
names(Vinhos)

attach(Vinhos)

# Frequência absoluta 
table(as.factor(Vinhos$quality), Vinhos$Vinho, useNA = "ifany")

table(as.factor(Vinhos$quality), Vinhos$Vinho)

# 2-Way Cross Tabulation
library(gmodels)
CrossTable(as.factor(Vinhos$quality), Vinhos$Vinho) 
 

summary(Vinhos)


aggregate(Vinhos,
          by = list( Vinho),
          FUN = mean)

mean(Vinhos$fixedacidity) # média

median(Vinhos$fixedacidity) # médiana

quantile(Vinhos$fixedacidity,type=4)  # Quartis

quantile(Vinhos$fixedacidity,.65,type=4) # exato percentil

range(Vinhos$fixedacidity)  # amplitude

diff(range(Vinhos$fixedacidity)) #diferença entre o maior e o menor valor

min(Vinhos$fixedacidity)  # valor mínimo de x

max(Vinhos$fixedacidity)  # valor máximo de x

var(Vinhos$fixedacidity) # para obter a variância

sd(Vinhos$fixedacidity)  # para obter o desvio padrão

CV_fixedacidity<-sd(Vinhos$fixedacidity)/mean(Vinhos$fixedacidity)*100  # para obter o coefiiente de variação
CV_fixedacidity


#comando para gerar em 3 linhas e 4 colunas os histogramas
par (mfrow=c(3,4))
hist(fixedacidity)
hist(volatileacidity)
hist(citricacid )
hist(residualsugar)
hist(chlorides)
hist(freesulfurdioxide)
hist(totalsulfurdioxide)
hist(density)
hist(pH)
hist(sulphates)
hist(alcohol)
hist(quality)
dev.off()

hist(quality, col=c("pink"), col.main="darkgray", prob=T)
     
atach(Vinhos)

#comando para gerar em 3 linhas e 4 colunas os histogramas
par (mfrow=c(3,4))
boxplot(fixedacidity, main='fixedacidity')
boxplot(volatileacidity , main='volatileacidity')
boxplot(citricacid , main='citricacid')
boxplot(residualsugar, main='residualsugar')
boxplot(chlorides, main='chlorides')
boxplot(freesulfurdioxide, main='freesulfurdioxide')
boxplot(totalsulfurdioxide, main='totalsulfurdioxide')
boxplot(density, main='density')
boxplot(pH, main='pH')
boxplot(sulphates, main='sulphates')
boxplot(alcohol, main='alcohol')
boxplot(Vinhos$quality, main='quality')
dev.off()

boxplot(quality ~ Vinho, main='quality')

boxplot(fixedacidity ~ Vinho, main='fixedacidity',col=c('red','blue'))
boxplot(volatileacidity ~ Vinho , main='volatileacidity')
boxplot(citricacid ~ Vinho, main='citricacid')
boxplot(residualsugar ~ Vinho, main='residualsugar',col=c('red','blue'))
boxplot(chlorides ~ Vinho, main='chlorides')
boxplot(freesulfurdioxide ~ Vinho, main='freesulfurdioxide')
boxplot(totalsulfurdioxide ~ Vinho, main='totalsulfurdioxide')
boxplot(density ~ Vinho, main='density')
boxplot(pH ~ Vinho, main='pH')
boxplot(sulphates ~ Vinho, main='sulphates')
boxplot(alcohol ~ Vinho, main='alcohol')


# Gráfico de dispersão ( pch=caracter, lwd=largura)

plot(freesulfurdioxide~totalsulfurdioxide)
plot(freesulfurdioxide~totalsulfurdioxide, pch=1, lwd=3)

plot(freesulfurdioxide~totalsulfurdioxide)
abline(v=mean(freesulfurdioxide), col="red")
abline(h=mean(totalsulfurdioxide), col="green")



attach(Vinhos)
Vinhos$fx_redSugar <- cut(residualsugar,breaks=c(0,10,20,30,max(residualsugar)))  
Vinhos$fx_redSugar  
str(Vinhos)
CrossTable( Vinhos$fx_redSugar , Vinhos$Vinho) 


attach(Vinhos)


library(psych)

describe(Vinhos)

# describe
# A data.frame of the relevant statistics:
# item name
# item number
# number of valid cases
# mean
# standard deviation
# trimmed mean (with trim defaulting to .1)
# median (standard or interpolated
# mad: median absolute deviation (from the median)
# minimum
# maximum
# skew
# kurtosis
# standard error


summary(Vinhos)
white <- subset(Vinhos, Vinho=="WHITE", select=c(quality,fixedacidity,volatileacidity,citricacid,residualsugar,
                                                 chlorides,freesulfurdioxide,totalsulfurdioxide,density,pH,
                                                 sulphates,alcohol))
#Estatísticas descritivas
summary(white)
 
str(white)

attach(white)
 

#Estatísticas descritivas

par (mfrow=c(3,4))
boxplot(fixedacidity, main='fixedacidity')
boxplot(volatileacidity , main='volatileacidity')
boxplot(citricacid , main='citricacid')
boxplot(residualsugar, main='residualsugar')
boxplot(chlorides, main='chlorides')
boxplot(freesulfurdioxide, main='freesulfurdioxide')
boxplot(totalsulfurdioxide, main='totalsulfurdioxide')
boxplot(density, main='density')
boxplot(pH, main='pH')
boxplot(sulphates, main='sulphates')
boxplot(alcohol, main='alcohol')
boxplot(quality, main='quality')
dev.off()

boxplot.stats(white$residualsugar)


AIQ_residualsugar<-quantile(white$residualsugar,.75,type=2)-quantile(white$residualsugar,.25,type=2)
AIQ_residualsugar

limsup_residualsugar= quantile(white$residualsugar,.75,type=4)+1.5*AIQ_residualsugar
limsup_residualsugar
liminf_residualsugar= quantile(white$residualsugar,.25,type=2)-1.5*AIQ_residualsugar
liminf_residualsugar


#excluir outliers

plot(quality~residualsugar)

white1<-subset(white, residualsugar<=40)   

fix(white1)

attach(white1)

summary(white1)

plot(residualsugar,alcohol)
abline(v=mean(residualsugar), col="red")
abline(h=mean(alcohol), col="green")


# matriz de correlações
matcor <- cor(white1)
print(matcor, digits = 2)


library(corrgram)
corrgram(matcor, type = "cor", lower.panel = panel.shade, upper.panel = panel.pie)

panel.cor <- function(x, y, digits=2, prefix ="", cex.cor,
                      ...)  {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y , use = "pairwise.complete.obs")
  txt <- format(c(r, 0.123456789), digits = digits) [1]
  txt <- paste(prefix, txt, sep = "")
  if (missing(cex.cor))
    cex <- 0.8/strwidth(txt)
  # abs(r) é para que na saída as correlações ficam proporcionais
  text(0.5, 0.5, txt, cex = cex * abs(r))
}
#pdf(file = "grafico.pdf")
pairs(white1, lower.panel=panel.smooth, upper.panel=panel.cor)

dev.off()
     
# fim


