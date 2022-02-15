library(bnlearn)
library(lattice)
library(BiocManager)
library(gridExtra)
library(gRain)
library(Rgraphviz)
library(openxlsx)

data<-read.xlsx("data_BN.xlsx")
head(data)
dim(data)
str(data)
data$sex<-as.factor(data$sex)
data$age<-as.factor(data$age)
data$urbanization<-as.factor(data$urbanization)
data$education<-as.factor(data$education)
data$geographic_area<-as.factor(data$geographic_area)
data$allergy<-as.factor(data$allergy)
data$smoke<-as.factor(data$smoke)
data$sedentary<-as.factor(data$sedentary)
data$asthma<-as.factor(data$asthma)
str(data)
names(data)
names(data)<-c("SEX","AGE","URB","EDU","GEO","ALG","SMK","SED","ASTHMA")
names(data)

table(data$ASTHMA)
table(is.na.data.frame(data))



#RAPPRESENTAZIONE GRAFICA DELLA RETE B

#1. EXPERT SYSTEM

dag<-empty.graph(nodes=c("SEX","AGE","URB","EDU","GEO","ALG","SMK","SED","ASTHMA"),num=1)
archi<-matrix(c("SEX","EDU",
                "AGE","EDU",
                "GEO","EDU",
                "EDU","URB",
                "URB","ALG",
                "URB","SED",
                "SMK","ASTHMA",
                "ALG","ASTHMA",
                "SED","ASTHMA"),
              byrow=TRUE,ncol=2,
              dimnames=list(NULL,c("from","to")))
arcs(dag)<-archi
dag
nodes(dag)
arcs(dag)
class(dag)
graphviz.plot(dag,main="expert system")
modelstring(dag)
#definiamo il modello di rete bayesiana utilizzando la funzione model2network()
dag2<-model2network("[SEX][AGE][GEO][SMK][EDU|SEX:AGE:GEO][URB|EDU][ALG|URB][SED|URB][ASTHMA|ALG:SMK:SED]")
dag2
graphviz.plot(dag2)
all.equal(dag,dag2)
#misure di network scores
score(dag,data=data,type="bic")
score(dag,data=data,type="aic")
score(dag,data=data,type="bde",iss=8)


#2. MODELLO CON ALGO HC 

#black list
black_list<-matrix(c("EDU","ASTHMA",
                     "ASTHMA","EDU",
                     "ASTHMA","URB",
                     "ASTHMA","SED",
                     "ASTHMA","ALG",
                     "ASTHMA","SMK",
                     "SMK","SEX",
                     "AGE","SEX",
                     "GEO","SEX",
                     "ALG","SEX",
                     "URB","SEX",
                     "EDU","SEX",
                     "SED","SEX",
                     "ASTHMA","SEX",
                     "SMK","EDU",
                     "ALG","EDU",
                     "URB","EDU",
                     "SED","EDU",
                     "ASTHMA","EDU",
                     "SMK","GEO",
                     "AGE","GEO",
                     "EDU","GEO",
                     "ALG","GEO",
                     "URB","GEO",
                     "SEX","GEO",
                     "SED","GEO",
                     "ASTHMA","GEO",
                     "SMK","AGE",
                     "GEO","AGE",
                     "EDU","AGE",
                     "ALG","AGE",
                     "URB","AGE",
                     "SEX","AGE",
                     "SED","AGE",
                     "ASTHMA","AGE",
                     "AGE","ALG",
                     "EDU","ALG"),
                   byrow=TRUE,ncol=2,
                   dimnames=list(NULL,c("from","to")))
#white list
white_list<-matrix(c("ALG","ASTHMA",
                     "SMK","ASTHMA",
                     "SED","ASTHMA"),
                   byrow=TRUE,ncol=2,
                   dimnames=list(NULL,c("from","to")))

#1: BIC
learned<-hc(data,score="bic",whitelist=white_list,blacklist=black_list)
graphviz.plot(learned,main="learned - hc with bic")
modelstring(learned)
learned
score(learned,data=data,type="bic")
nodes(learned)
arcs(learned)

#2: BDE
learned2<-hc(data,score="bde",whitelist=white_list,blacklist=black_list)
graphviz.plot(learned2,main="learned2 - hc with bde")
modelstring(learned2)
learned2
score(learned2,data=data,type="bde")
nodes(learned)
arcs(learned)

#3: AIC
learned3<-hc(data,score="aic",whitelist=white_list,blacklist=black_list)
graphviz.plot(learned3,main="learned3 - hc with aic")
modelstring(learned3)
learned3
score(learned3,data=data,type="aic")
nodes(learned)
arcs(learned)



#Confronto EXPERT SYSTEM e HC sulla base degli score

#bic
#expert system
a<-score(dag,data=data,type="bic")
#learned
b<-score(learned,data=data,type="bic")
bic<-c(a,b)
names(bic)<-c("expert system","learned")
bic
#aic
#expert system
c<-score(dag,data=data,type="aic")
#learned3
d<-score(learned,data=data,type="aic")
aic<-c(c,d)
names(aic)<-c("expert system","learned2")
aic
#bde
#expert system
e<-score(dag,data=data,type="bde",iss=8)
#learned2
f<-score(learned,data=data,type="bde")
bde<-c(e,f)
names(bde)<-c("expert system","learned3")
bde


#Si sceglie il modello dell'Expert System
#grafo
hlight<-list(nodes=nodes(dag),arcs=arcs(dag),col="blue",textCol="red",lwd=2,
             fill=c("yellow"))
grafo<-graphviz.plot(dag,highlight=hlight,main="Expert System Graph")


#RAPPRESENTAZIONE PROBABILISTICA DELLA RETE B
bn.bayes<-bn.fit(dag,data=data,method="bayes",iss=8)
bn.bayes
bn.bayes$URB #esempio
cpt<-list(SEX=bn.bayes$SEX$prob,AGE=bn.bayes$AGE$prob,URB=bn.bayes$URB$prob,
          EDU=bn.bayes$EDU$prob,GEO=bn.bayes$GEO$prob,ALG=bn.bayes$ALG$prob,
          SMK=bn.bayes$SMK$prob,SED=bn.bayes$SED$prob,ASTHMA=bn.bayes$ASTHMA$prob)
bn<-custom.fit(dag,cpt)
bn
class(bn)
nparams(bn)
nodes(bn)
arcs(bn)



#ESEMPIO:
#verificare l'ipotesi di indipendenza condizionale: URB è indip. da SEX | EDU
ci.test("URB","SEX","EDU",test="mi",data=data)
ci.test("URB","SEX","EDU",test="x2",data=data)
#gradi di libertà per G2 e X2 per l'ipotesi di indipendenza condizionale: URB è indip. da SEX | EDU
(nlevels(data$URB)-1)*(nlevels(data$SEX)-1)*(nlevels(data$EDU))



#CONDITIONAL INDEPENDENCE QUERIES
# CONNESSIONE SERIALE
# URB e SEX sono d-separati
dsep(dag,"URB","SEX","EDU")
# TRUE, sono condizionalmente indipendenti
#CONNESSIONE DIVERGENTE
# ALG e SED sono d-separati
dsep(dag,"ALG","SED","URB")
# TRUE, sono condizionalmente indipendenti
# CONNESSIONE CONVERGENTE
# se non vi è evidenza su ASTHMA , allora ALG e SMK sono d-separati, infatti
dsep(dag,"ALG","SMK")
# invece, se vi è evidenza su ASTHMA ALG e SMK non sono d-separati e sono dipendenti condizionalmente
dsep(dag,"ALG","SMK","ASTHMA")



#PROCEDURE DI INFERENZA ESATTA
par(mfrow=c(1,1))
graphviz.plot(dag)
modelstring(bn)
bn

junction<-compile(as.grain(bn))

#CONDITIONAL PROBABILITY QUERIES

# valutiamo la condizionata di ASTHMA dato ALG e SMK
querygrain(junction,nodes=c("ASTHMA","ALG","SED"),type="conditional")

# valutiamo la congiunta di ASTHMA e ALG
querygrain(junction,nodes=c("ASTHMA","ALG"),type="joint")

# valutiamo la marginale di ASTHMA
querygrain(junction,nodes="ASTHMA",type="marginal")

#domanda: chi ha allergia e fuma ha maggiore probabilità di avere asma?
j1<-setEvidence(junction,nodes=c("ALG","SMK"),states=c("yes","yes"))
j1$isPropagated

#quali probabilità sono cambiate/si sono aggiornate?
querygrain(junction,nodes="ASTHMA",type="marginal")
querygrain(j1,nodes="ASTHMA",type="marginal")

querygrain(junction,nodes=c("ASTHMA","SED"),type="conditional")
querygrain(j1,nodes=c("ASTHMA","SED"),type="conditional")

