# Práctica 5 - Ajuste, residuos y reporte #
# MLM ISUC 2015 ###########################                       #################

pacman::p_load(lme4,foreign,influence.ME, stargazer, doBy,psych,texreg,xtable,plyr,sjPlot,nlme)

# 1.  Análisis de ajuste de los modelos

# 1.1 R2 B&R ------

HSBdata <- read.dta ("http://www.ats.ucla.edu/stat/stata/faq/hsb.dta")
attach(HSBdata)

# Model 0: Null model
results_0 = lmer(mathach ~ 1 + (1 | id))
summary(results_0)

# Model 1: Fixed effects at individual level
results_1 = lmer(mathach ~ 1 + minority + ses + (1 | id))
summary(results_1)

# Model 2: Fixed effects at school level
results_2 = lmer(mathach ~ 1 + meanses + sector + (1 | id))
summary(results_2)

# Model 3: Random slope effects at individual level
results_3 = lmer(mathach ~ 1 + minority + ses + (1 + ses | id))
summary(results_1)

# Extraer componentes de la varianza para cálculo de R2

varcomp_0=as.data.frame(VarCorr(results_0))
tau00_0=varcomp_0[1,4]
sigma2_0=varcomp_0[2,4]

varcomp_1=as.data.frame(VarCorr(results_1))
tau00_1=varcomp_1[1,4]
sigma2_1=varcomp_1[2,4]

varcomp_2=as.data.frame(VarCorr(results_2))
tau00_2=varcomp_2[1,4]
sigma2_2=varcomp_2[2,4]

# Cálculo

# Modelo 1 (predictores individuales)

R2_1_L1=(sigma2_0-sigma2_1)/sigma2_0
R2_1_L1

# Modelo 2 (predictores grupales)
R2_2_L1=(sigma2_0-sigma2_2)/sigma2_0
R2_2_L1
R2_2_L1=(tau00_0-tau00_2)/tau00_0
R2_2_L1

# ICC (para comparación)
ICC_0=tau00_0/(tau00_0+sigma2_0)
ICC_0

# 1.2 Test de devianza (deviance)

# Deviance modelo sin multilevel (para contraste con nulo)
interceptOnly <-gls(mathach~1, data = HSBdata, method = "ML")

# Deviance NULO
results_0ml = lmer(mathach ~ 1 + (1 | id), REML=FALSE)
results_0ml <- update(results_0,REML=FALSE) # alternativa

results_0ml
-23557.91*2 # Deviance 47115.8, igual que en output

# Deviance modelo 1
results_1ml = lmer(mathach ~ 1 + minority + ses + (1 | id), REML=FALSE)
results_1ml # deviance=46443.64

# Cálculo deviance test (anidado-mayor)
47115.8-46443.64 #= 672.16, 2 DF (2 parametros adicionales)

qchisq(.95, df=2) # 5.99 valor crítico chi2 para 2 df, menor que deviance, por lo tanto la diferencia entre los modelos es distinta de 0.

# De manera directa...
anova(results_0ml,results_1ml)

# Para contraste modelo con intercepto aleatorio y sin
interceptOnly <-gls(mathach~1, data = HSBdata, method = "ML")
summary(interceptOnly)
results_0ml2 = lme(mathach ~ 1,random=~ 1 | id, method="ML")
anova(interceptOnly,results_0ml2)

# Contraste modelo con y sin pendiente aleatoria
anova(results_1,results_3)


# 2. Exploración de datos, descriptivos y reporte

hist(mathach, prob=TRUE)
curve(dnorm(x, mean=mean(mathach), sd=sd(mathach)), add=TRUE)
shapiro.test(mathach)

describe(HSBdata)

# Summary table multilevel variables L1
summary(HSBdata)
HSBdata$`_merge`=NULL # drop merge variable

stargazer(HSBdata,title="Estadísticos descriptivos") # tabla exportable a latex

# Describir número de casos por escuela (libreria doBy)
casesL2=summaryBy(mathach ~ id, data=HSBdata, FUN=list(length)) # numero de casos por escuela, distribución de los casos

# Correlaciones
HSBvarL1=subset(HSBdata,select=minority:mathach)
cor(HSBvarL1)

  # A latex con xtable: via funcion corstars
  corstarsl <- function(x){  # -------------
  require(Hmisc) 
  x <- as.matrix(x) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P 
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  
  ## build a new matrix that includes the correlations with their apropriate stars 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew) 
}
  xtable(corstarsl(HSBvarL1))

HSBvarL2=summaryBy(size + sector + meanses ~id, data=HSBdata,FUN=mean)
HSBvarL2$id=NULL
str(HSBvarL2)
cor(HSBvarL2)

HSBvarL2=rename(HSBvarL2, c("size.mean"="size", "sector.mean"="sector","meanses.mean"="meanses"))

  # A latex con xtable: via funcion corstars
  xtable(corstarsl(HSBvarL2))

# Scatter plots
  
  sjp.scatter(HSBvarL1$ses,HSBvarL1$mathach,
              axisTitle.x = "SES", 
              axisTitle.y = "math achievement",
              showGroupFitLine = TRUE)

  # Por grupos (sexo), debe ser factor
  is.factor(HSBvarL1$female) 
  HSBvarL1$female=as.factor(HSBvarL1$female)
  is.factor(HSBvarL1$female)  
  HSBvarL1$female=factor(HSBvarL1$female,levels = c(0,1),
                         labels = c("male", "female"))
  
  sjp.scatter(HSBvarL1$ses,HSBvarL1$mathach,HSBvarL1$female,
              axisTitle.x = "SES", 
              axisTitle.y = "math achievement",
              showGroupFitLine = TRUE)

  
  # Nivel 2 
  HSBvarL2=summaryBy(mathach +size + sector + meanses ~id, data=HSBdata,FUN=mean) 
  summary(HSBvarL2)
  
  sjp.scatter(HSBvarL2$meanses,HSBvarL2$mathach,
              axisTitle.x = "Mean SES", 
              axisTitle.y = "math achievement",
              showGroupFitLine = TRUE)          

  is.factor(HSBvarL2$sector)           
  HSBvarL2$sector=as.factor(HSBvarL2$sector)
  is.factor(HSBvarL2$sector)  
  HSBvarL2$sector=factor(HSBvarL2$sector,levels = c(0,1),
                         labels = c("publico", "catolico"))
              
  sjp.scatter(HSBvarL2$meanses,HSBvarL2$mathach,HSBvarL2$sector,
              axisTitle.x = "Mean SES", 
              axisTitle.y = "math achievement",
              showGroupFitLine = TRUE)     
  
# Model report

texreg(results_1)
texreg(list(results_0, results_1,results_2),dcolumn = TRUE)


