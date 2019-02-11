library(rvest) #scrapping
library(stringr)
library(splitstackshape)
library(tidyr)
library(XML) #scrapping

setwd("C:\\Users\\XXX\\Documents\\Estadísticas\\España")


####ENLACES PARTIDOS LIGA ESPAÑOLA
url="http://www.futbolfantasy.com/laliga/calendario/2019"
parsed<-htmlParse(url)
links<-xpathSApply(parsed,path = "//a",xmlGetAttr,"href")
links=matrix(links,ncol=1)
links_partidos=t(as.data.frame(links[c(77:305)]))#462,184
nrow(links_partidos)
result1=data.frame()
fecha1=data.frame()
arbitro=data.frame()
equipos2=data.frame()
home=data.frame(c("Local","Visitante"))
colnames(home)<-c()
gol=data.frame()
