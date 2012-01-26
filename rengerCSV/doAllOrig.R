### R code from vignette source 'd:/PhD/Projekte/Modelling/gdxrrw/gdxrrw.rnw'

###################################################
### code chunk number 1: gdxrrw.rnw:58-60
###################################################
refsample<-read.csv("REFSAMPLE.dat",sep="\t", header=T)
str(refsample)


###################################################
### code chunk number 2: gdxrrw.rnw:72-74
###################################################
refsamplel <- reshape(refsample,idvar=c("PNR","HHNR"),varying=list(3:6),direction="long",times=c("APER","CAR","WKSTAT","ZERO"))
str(refsamplel)


###################################################
### code chunk number 3: gdxrrw.rnw:97-101
###################################################
refsamplel[[1]]<-as.factor(refsamplel[[1]])
refsamplel[[2]]<-as.factor(refsamplel[[2]])
refsamplel[[3]]<-as.factor(refsamplel[[3]])
attr(refsamplel,"symName") <- 'refsample';


###################################################
### code chunk number 4: gdxrrw.rnw:106-107
###################################################
wgdx.df('entropy',refsamplel)


###################################################
### code chunk number 5: gdxrrw.rnw:122-149
###################################################
# Read a dataframe x and save the data as a parameter with the name "parname"
# in a gdx file with the name  "gdxname". The parameter is defined over "dim"
# dimensions and the dataframe has "totcol" columns.

gdxwritedf<- function(x,dim,totcol,parname) {
# Define the sets that define xxx
   count <-dim-2
   indices<-c(names(x)[[1]])
   for (i in 1:count){
       names(x)[[i]]
       indices<-c(indices,names(x)[[i+1]])
   }
# Define the names of the set elements:
   count<-totcol-dim
   secindices<-c(names(x)[[dim+1]])
   for (i in 1:count){
       secindices<-c(secindices,names(x)[[i+2]])
   }

# Reshape the data from wide format to long format:
   xl <- reshape(x,idvar=indices,varying=list(dim:totcol),direction="long",times=secindices)
   for (i in 1:dim){
       xl[[i]]<-as.factor(xl[[i]])
   }
   attr(xl,"symName") <- parname
   return(xl)
}


###################################################
### code chunk number 6: gdxrrw.rnw:153-154
###################################################
t<-gdxwritedf(refsample,3,6,"refsample")


###################################################
### code chunk number 7: gdxrrw.rnw:157-158
###################################################
wgdx.df("data.gdx",t)


###################################################
### code chunk number 8: gdxrrw.rnw:163-166
###################################################
t2<-(gdxwritedf(refsample,3,6,"refsample2"))
tl<-list(t,t2)
wgdx.lst("data.gdx",tl)


###################################################
### code chunk number 9: gdxrrw.rnw:175-178
###################################################
hhnr <-list(unique(refsample$HHNR))
hhlst <- list (name='hhnr', type='set', ts='Household ID', uels=c(hhnr))
wgdx("test",hhlst)


###################################################
### code chunk number 10: gdxrrw.rnw:181-186
###################################################
pnr  <-list(unique(refsample$PNR))
plst <-list(name='pnr', type='set', ts='Person ID', uels=c(pnr))
q<-list(colnames(refsample[,3:6]))
qlst <-list(name='q', type='set', ts='Questions', uels=c(q))
wgdx("refsamplesets.gdx",qlst,plst,hhlst)


###################################################
### code chunk number 11: gdxrrw.rnw:194-198
###################################################
refsampleG<-rgdx.param("data.gdx","refsample")
head(refsampleG)
hhnrset<-rgdx.set("refsamplesets.gdx","hhnr")
head(hhnrset)


###################################################
### code chunk number 12: gdxrrw.rnw:212-213
###################################################
str(hhnrset)


