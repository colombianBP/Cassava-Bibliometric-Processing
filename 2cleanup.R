# This file contains code snippets designed to generate files intended to 
# facilitate manual filtering of the database

library(tidyverse)
library(readxl)
library(dplyr)
library(bibliometrix)

## For variable C1:

# Snippet1, for a df datav2 generate a dataframe which contains a list of unique
# institution identifiers (name before a ',') and a list of all full names where
# it appears


vecnames<-character()
vecdoi<-character()
for(i in seq.int(1,dim(datav2)[1])){
  nombres<-strsplit(datav2$C1[i],';')[[1]]
  nombres<-trimws(nombres)
  for(j in seq.int(1,length(nombres))){
    nombres2<-strsplit(nombres,',')[[j]][1]
    vecnames<-append(vecnames,nombres2)
    vecdoi<-append(vecdoi,nombres[j])
  }
  
}
datac1<-data.frame(names=vecnames,doi=vecdoi)
agg <- group_by(datac1, names)
df <- summarise(agg,doiagregated=paste(doi, collapse = ';'))

# Snippet2, for a df datav3 and a manually curated c1clean correction file, 
# dummie variables are created to indicate recall to a specific CGIAR 
# organization, or identififacion as an enterprice or university.
# Then, a columns contatining country association and depuraton status are added
# C1 column is eddited in order to homogenize manually corrected organization 
# names (first part before the first ',')


columnas<-c('CIAT','CGIAR','UNI','AFRICARICE','CIFOR','ICRISAT','IFPRI','IITA','ILRI','CIMMYT','CIP','IRRI','IWMI','ICRAF','ICARDA','BIOVERSITY INTL','HARVESTPLUS','ALLIANCE','EMPRESA')

datav3$C1<-as.character(datav3$C1)

c1clean$c1<-trimws(c1clean$c1)
c1clean$country<-toupper(trimws(c1clean$country))

### Reset dummies ###
for (i in columnas) {
  lista=integer()
  datos<-c1clean[c1clean[i]==1,]
  nombres<-trimws(datos$c1)
  for(j in seq(dim(datav3)[1])){
    nombres2<-strsplit(as.character(datav3$C1[j]),";")[[1]]
    valor=0
    for (w in nombres2) {
      if(trimws(unlist(strsplit(w,',')))[[1]] %in% nombres){
        valor=1
      }
    }
    lista=append(lista,valor)
  }
  print(sum(lista))
  datav3[i]<-lista
}

# Clean c1
aja=0
lista<-character()
paises<-character()
estado<-character()
indices<-integer()

for (i in seq(dim(datav3)[1])) {
  lista2<-character()
  paises2<-character()
  estado2<-character()
  nombres<-trimws(unlist(strsplit(datav3$C1[i],';')))
  for(j in nombres){
    nom<-trimws(unlist(strsplit(j,',')[[1]]))
    nombre<-nom[1]
    indice<-match(nombre,c1clean$c1)
    indices<-append(indice,indices)
    cambio<-c1clean$replace[indice]
    if(!(is.null(cambio)|is.na(cambio))){
      aja<- aja+1
      nombre<-cambio
      paises2<-append(paises2,c1clean$country[indice])
      estado2<-append(estado2,c1clean$checked[indice])
    } else if (!(is.na(indice))){
      paises2<-append(paises2,c1clean$country[indice])
      estado2<-append(estado2,c1clean$checked[indice])
    } else{
      paises2<-append(paises2,'NA')
      estado2<-append(estado2,'NA')
    }
    if(length(nom)>1){
      lista2<-append(lista2,paste0(append(nombre,nom[2:length(nom)]),collapse = ','))
    } else{
      lista2<-append(lista2,nombre)
    }
  }
  lista<-append(lista,paste0(lista2,collapse=';'))
  paises<-append(paises,paste0(paises2,collapse=';'))
  estado<-append(estado,paste0(estado2,collapse=';'))
}
indices<-unique(indices)
print(length(indices))
datav3$C1<-lista
datav3['C1_country']<-paises
datav3['C1_checked']<-estado

# Snippet3, from a df datav3 create a new df for manual cleaning, preserving  
# information stored in newly generated country and tidiness status columns 


c1<-character()
completo<-character()
paises<-character()
checked<-character()

for( i in seq(dim(datav3)[1])){
  part<-strsplit(datav3$C1[i],';')[[1]]
  lpaises<-strsplit(datav3$C1_country[i],';')[[1]]
  lchecked<-strsplit(datav3$C1_checked[i],';')[[1]]
  for(j in seq(length(part))){
    nom<-trimws(strsplit(part[j],',')[[1]][1])
    if(!(nom %in% c1)){
      c1<-append(c1,nom)
      completo<-append(completo,part[j])
      paises<-append(paises,lpaises[j])
      checked<-append(checked,lchecked[j])
    }else{
      indice<-match(nom,c1)
      completo[indice]<-paste0(c(completo[indice],part[j]),collapse = ';')
      paises[indice]<-paste0(c(paises[indice],lpaises[j]),collapse = ';')
      checked[indice]<-paste0(c(checked[indice],lchecked[j]),collapse = ';')
    }
  }
}

tabla<-data.frame(c1,completo,paises,checked)


## For variable FU:


# Snippet1, for a df datav2 generate a dataframe which contains a list of unique
# institution identifiers (name before a ',') and a list of all full names where
# it appears


vecnames<-character()
vecdoi<-character()
for(i in seq.int(1,dim(datav2)[1])){
  nombres<-strsplit(datav2$FU[i],';')[[1]]
  nombres<-trimws(nombres)
  for(j in seq.int(1,length(nombres))){
    nombres2<-strsplit(nombres,',')[[j]][1]
    vecnames<-append(vecnames,nombres2)
    vecdoi<-append(vecdoi,nombres[j])
  }
  
}
datafu<-data.frame(names=vecnames,doi=vecdoi)
agg <- group_by(datafu, names)
df <- summarise(agg,doiagregated=paste(doi, collapse = ';'))


# Snippet2, for a df datav4 and a manually generated df of corrections and 
# associated countries, homogenices names and generates a new column of country
# affiliations


aja=0
lista<-character()
paises<-character()
indices<-integer()

for (i in seq(dim(datav4)[1])) {
  lista2<-character()
  paises2<-character()
  estado2<-character()
  nombres<-trimws(unlist(strsplit(datav4$FU[i],';')))
  for(j in nombres){
    nom<-trimws(unlist(strsplit(j,',')[[1]]))
    nombre<-nom[1]
    indice<-match(nombre,fucor$fu)
    indices<-append(indice,indices)
    cambio<-fucor$Replace[indice]
    if(!(is.null(cambio)|is.na(cambio)|cambio==""|cambio=='NA')){
      aja<- aja+1
      nombre<-cambio
      paises2<-append(paises2,fucor$Country[indice])
    } else if (!(is.na(indice))){
      paises2<-append(paises2,fucor$Country[indice])
    } else{
      paises2<-append(paises2,'NA')
    }
    if(length(nom)>1){
      lista2<-append(lista2,paste0(append(nombre,nom[2:length(nom)]),collapse = ','))
    } else{
      lista2<-append(lista2,nombre)
    }
  }
  lista<-append(lista,paste0(lista2,collapse=';'))
  paises<-append(paises,paste0(paises2,collapse=';'))
}
indices<-unique(indices)
print(length(indices))

datav5<-datav4
datav5$FU<-lista
datav5['FU_country']<-paises


# Snippet3, from a database datav5 generate a df for secondary manual cleanup
# preserving previously generated country data

vecnames<-character()
vecdoi<-character()
vecvon<-character()
for(i in seq.int(1,dim(datav5)[1])){
  nombres<-strsplit(datav5$FU[i],';')[[1]]
  nombres<-trimws(nombres)
  paises<-strsplit(datav5$FU_country[i],';')[[1]] #test
  for(j in seq.int(1,length(nombres))){
    nombres2<-strsplit(nombres,',')[[j]][1]
    vecnames<-append(vecnames,nombres2)
    vecdoi<-append(vecdoi,nombres[j])
    vecvon<-append(vecvon,paises[j])
  }
  
}
datac1<-data.frame(names=vecnames,doi=vecdoi,pais=vecvon)
agg <- group_by(datac1, names)
df <- summarise(agg,doiagregated=paste(doi, collapse = ';'),countryagregated=paste(pais,collapse = ';'))

# For variable WC

# Snippet 1, from a df datav2, creates a table of toppics to by manually reclassified


wc<-character()


for (i in datav2$WC) {
  individual <- unique(strsplit(i,';')[[1]])
  for (j in individual) {
    wc <- append(wc,trimws(j))
  }
}

tablewc<-data.frame(table(wc))

# Snippet 2, from a df datav5 and a cat reclassification df, we replace the 
# categories with our reclassification df.

nombres<-colnames(cat)[4:18]

ini<-datav5$WC
ini<-strsplit(ini,';')

contador=1
for(i in ini){
  ini[[contador]]<-trimws(i)
  contador<-contador+1
}

dic<-cat$wc
dicn<-character(length = length(dic))
contador<-1
for(i in dic){
  for(j in nombres){
    if(!(is.na(cat[contador,j]))){
      dicn[contador]<-paste0(c(j,dicn[contador]),collapse = ';')
    }
  }
  contador=contador+1
}

names(dicn)<-dic

vector<-character()

for(i in seq.int(length(ini))){
  vec<-character()
  for (j in ini[[i]]) {
    vec<-append(vec,dicn[j])
  }
  vec<-paste0(vec,collapse='')
  vector<-append(vector,vec)
}

datav5$WC<-vector
