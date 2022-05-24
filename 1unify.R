#This code unifies deagregated and regular WOS data, eliminating duplicates in the process

library(bibliometrix)
library(ggplot2)
library(tidyverse)
library(readxl)
library(dplyr)
setwd('D:\\trabajo CIAT')


#Import Wos using bibliometrix

#Here should be a list of bibliographic wos files as plain text .txt files, as  
#in the example provided
archivos<-c('yuca\\mis_datos\\crudos\\comopt\\wos1.txt',
            'yuca\\mis_datos\\crudos\\comopt\\wos2.txt',
            'yuca\\mis_datos\\crudos\\comopt\\wos3.txt',
            'yuca\\mis_datos\\crudos\\comopt\\wos4.txt',
            'yuca\\mis_datos\\crudos\\comopt\\wos5.txt',
            'yuca\\mis_datos\\crudos\\comopt\\wos6.txt',
            'yuca\\mis_datos\\crudos\\comopt\\wos7.txt',
            'yuca\\mis_datos\\crudos\\comopt\\wos8.txt',
            'yuca\\mis_datos\\crudos\\comopt\\wos9.txt',
            'yuca\\mis_datos\\crudos\\comopt\\wos10.txt',
            'yuca\\mis_datos\\crudos\\comopt\\wos11.txt',
            'yuca\\mis_datos\\crudos\\comopt\\wos12.txt',
            'yuca\\mis_datos\\crudos\\comopt\\wos13.txt',
            'yuca\\mis_datos\\crudos\\comopt\\wos14.txt',
            'yuca\\mis_datos\\crudos\\comopt\\wos16.txt',
            'yuca\\mis_datos\\crudos\\comopt\\wos15.txt')

wos<-convert2df(archivos,dbsource = 'wos',format = 'plaintext')


#Here should be a list of desegregated bibliographic wos files as excel .xls  
#files, as in the example provided
archivosDeag <- c('yuca\\mis_datos\\crudos\\deag\\asexcel\\deagWos2.xls',
                  'yuca\\mis_datos\\crudos\\deag\\asexcel\\deagWos3.xls',
                  'yuca\\mis_datos\\crudos\\deag\\asexcel\\deagWos4.xls',
                  'yuca\\mis_datos\\crudos\\deag\\asexcel\\deagWos5.xls',
                  'yuca\\mis_datos\\crudos\\deag\\asexcel\\deagWos6.xls',
                  'yuca\\mis_datos\\crudos\\deag\\asexcel\\deagWos7.xls',
                  'yuca\\mis_datos\\crudos\\deag\\asexcel\\deagWos8.xls',
                  'yuca\\mis_datos\\crudos\\deag\\asexcel\\deagWos10.xls',
                  'yuca\\mis_datos\\crudos\\deag\\asexcel\\deagWos11.xls',
                  'yuca\\mis_datos\\crudos\\deag\\asexcel\\deagWos12.xls',
                  'yuca\\mis_datos\\crudos\\deag\\asexcel\\deagWos13.xls',
                  'yuca\\mis_datos\\crudos\\deag\\asexcel\\deagWos14.xls',
                  'yuca\\mis_datos\\crudos\\deag\\asexcel\\deagWos15.xls',
                  'yuca\\mis_datos\\crudos\\deag\\asexcel\\deagWos16.xls',
                  'yuca\\mis_datos\\crudos\\deag\\asexcel\\deagWos9.xls')

deag<-read_excel('yuca\\mis_datos\\crudos\\deag\\asexcel\\deagWos1.xls',skip = 10)

for(i in archivosDeag){
  temp<-read_excel(i,skip = 10)
  deag<-rbind(deag,temp)
  
}


#########################################################
# Trying to unify the databases
# Main issue: finding relative elements and joining
nodoideag<-subset(deag,is.na(deag$DOI))
doideag<-subset(deag,!(is.na(deag$DOI)))

n_ocur<-data.frame(table(doideag$DOI))
n_ocur<-n_ocur[n_ocur$Freq > 1,]

nodoideag<-rbind(nodoideag,subset(deag,DOI %in% n_ocur$Var1))
doideag<-subset(doideag,!(DOI %in% n_ocur$Var1))

doiwos<-subset(wos,!is.na(wos$DI))
nodoiwos<-subset(wos,is.na(wos$DI))

n_ocur<-data.frame(table(doiwos$DI))
n_ocur<-n_ocur[n_ocur$Freq > 1,]

nodoiwos<-rbind(nodoiwos,subset(wos,DI %in% n_ocur$Var1))
doiwos<-subset(doiwos,!(DI %in% n_ocur$Var1))




newdat<-doiwos
for(i in colnames(deag)){
  newdat[i]<-rep(NA,dim(newdat)[1])
}
dois<-intersect(doideag$DOI,doiwos$DI)
for(doi in dois){
  for(fila in colnames(deag)){
    newdat[newdat$DI==doi,fila]<-doideag[doideag$DOI==doi,fila]
  }
}
doisfal<-subset(newdat,is.na(newdat$`Total Citations`))$DI

newdat<-subset(newdat,!(is.na(newdat$`Total Citations`)))
nodoiwos<-rbind(nodoiwos,subset(wos,DI %in% doisfal))




#remove title duplicates (both)

nodoiwos['TI']<-tolower(nodoiwos$TI)

n_ocur<-data.frame(table(nodoiwos$TI))
n_ocur<-n_ocur[n_ocur$Freq > 1,]

nodoiwos<-subset(nodoiwos,!(TI %in% n_ocur$Var1))

deag['Title']<-tolower(deag$Title)

n_ocur<-data.frame(table(deag$Title))
n_ocur<-n_ocur[n_ocur$Freq > 1,]

nodoideag<-subset(deag,!(Title %in% n_ocur$Var1))
for(i in colnames(deag)){
  nodoiwos[i]<-rep(NA,dim(nodoiwos)[1])
}


titulos<-intersect(nodoideag$Title,nodoiwos$TI)
for(titulo in titulos){
  for(fila in colnames(deag)){
    nodoiwos[nodoiwos$TI==titulo,fila]<-nodoideag[nodoideag$Title==titulo,fila]
  }
}

unclasified<-subset(nodoiwos,is.na(nodoiwos$`Total Citations`))
nodoiwos<-subset(nodoiwos,!(is.na(nodoiwos$`Total Citations`)))

wos['TI']<-tolower(wos$TI)

unclasified<-subset(unclasified,select = colnames(wos))
unclasified<-rbind(unclasified,subset(wos,TI %in% n_ocur$Var1))

newdat<-rbind(newdat,nodoiwos)


####################  writing & reading #####################

# This section is aimed at saving writing the unified database, and any 
#instances where it was not possible to perform unification.



# write.csv(newdat,'yuca\\mis_datos\\unificandodeag\\doiTitleUnified.csv')
# write.csv(unclasified,'yuca\\mis_datos\\unificandodeag\\unclasified.csv')
# newdat<-read.csv('yuca\\mis_datos\\unificandodeag\\doiTitleUnified.csv')