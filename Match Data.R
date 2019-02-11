library(rvest) #scrapping
library(stringr)
library(splitstackshape)
library(tidyr)
library(XML) #scrapping

setwd("C:\\Users\\XXX\\Documents\\Estadísticas\\España") 

###############FUTBOLFANTASY################

####GETTING MATCHES' URL
url="http://www.futbolfantasy.com/laliga/calendario/2019"
parsed<-htmlParse(url)
links<-xpathSApply(parsed,path = "//a",xmlGetAttr,"href")
links=matrix(links,ncol=1)
links_partidos=t(as.data.frame(links[c(77:305)]))#selecting matches URL

####CREATING DATA FRAMES
result1=data.frame()
fecha1=data.frame()
arbitro=data.frame()
equipos2=data.frame()
home=data.frame(c("Local","Visitante"))
colnames(home)<-c()
gol=data.frame() 

#STARTING URLs LOOP
for(n in 1:nrow(links_partidos)){
  
tryCatch({  
  url=as.character(links_partidos[n,1])
  url
  html <- read_html(url)#reading url
  score<-html_nodes(html,".statsglobales") #selecting nodes
  score<-html_text(score) #getting text
  score
c<-gsub("(\t|\n)"," ",score) #remove /t and /n
c<-strsplit(as.character(c[[1]]),"                ")
d<-data.frame()

tryCatch({
#loop for getting the stats  
  
for(i in 1:length(c[[1]])){
  z<-trimws(c[[1]][i], "left")
  z1<-strsplit(z,"     ")
  for(j in 1:(length(z1[[1]]))){
    
    if(j==1 | j==3){
      z2<- as.numeric(z1[[1]][j])
    } else {
      z2<-z1[[1]][j]
    }
  
    d[i,j]<-z2
    rm(z2)
  } 
}
},  error=function(e){})

d<-d[1:11,]
row.names(d)<-d$V2
d<-d[,-2]
result<-data.frame(t(d))
result=cbind(result,rbind(result[2,],result[1,]))
result1=rbind(result1,result)
rm(result)

####DATE

fecha<-html_nodes(html,".fecha")
fecha_partido<-html_text(fecha[1])
fecha_partido<-gsub("(\t|\n)"," ",as.character(fecha_partido)) 
fecha_partido<-gsub("h"," ",fecha_partido)
fecha_partido<-gsub(",","",fecha_partido)
fecha<-trimws(as.character(fecha_partido))
fecha=sapply(as.data.frame(strsplit(fecha,split = " ")),as.character)
fecha[2,1]<-as.numeric(gsub(" .", "", fecha[2,1]))
fecha_2=cbind(fecha[2,1],fecha[3,1],fecha[6,1],fecha[8,1],fecha[11,1])
rownames(fecha_2) <- c()
fecha_2=rbind(fecha_2,fecha_2)
fecha1=rbind(fecha1,fecha_2)

####REFEREE

arb<-html_nodes(html,".link") %>% html_text()
arb<-gsub("(\t|\n)"," ",arb) 
arb <- matrix(arb,ncol=1)
arb=data.frame(arb[c(1:1)])
arb=rbind(arb,arb)
arbitro=rbind(arbitro,arb)
  
####TEAMS HOME & AWAY
equipos<-html_nodes(html,".nombre")
local<-as.character(html_text(equipos[2])) 
visitante=as.character(html_text(equipos[3]))
equipos1=as.data.frame(rbind(local,visitante))
equipos3=rbind(visitante,local)
equipos1=cbind(equipos1,equipos3)
equipos2=rbind(equipos2,equipos1)

###GOALS

score<-html_nodes(html,".local.score")
gol_local<-as.numeric(html_text(score[1])) 

score<-html_nodes(html,".visitante.score") 
gol_visitante<-as.numeric(html_text(score[1])) 

gol1=rbind(gol_local,gol_visitante)
gol2=rbind(gol_visitante,gol_local)

gol4=rbind(gol_local-gol_visitante,gol_visitante-gol_local)

###SETTING RESULT  
gol3=cbind(gol1,gol2)
if(gol3[1,1]>gol3[1,2]){
  ganador=rbind("Victoria","Derrota")
  gol3=cbind(gol3,ganador)
}else if(gol3[1,1]==gol3[1,2]){
  ganador=rbind("Empate","Empate")
  gol3=cbind(gol3,ganador)
} else{
  ganador=rbind("Derrota","Victoria")
  gol3=cbind(gol3,ganador)
}

gol=rbind(gol,cbind(gol3,gol4))

},  error=function(e){})}  

result1=na.omit(result1) #in case of any NA row
row.names(equipos2)=c()
colnames(equipos2)=c()
equipos2=cbind(home,equipos2) #bind to get home and away teams

resultados=cbind("España","18-19",fecha1,arbitro,equipos2,gol,result1)

names(resultados)<-c("Liga","Temporada","Jornada","Dia_Semana","Mes","Año","Hora", "Arbitro", "Local-Vis","Equipos","Rival","Gol_Fav",
                     "Gol_Cont","Resultado", "Dif.Goles","Tiros", "Tiros_Puerta","Centros","Centros_precisos", "Corners","Faltas", "Intercepciones","Robos", 
                     "Amarillas", "Rojas","Penaltis_Fav", "Tiros_Contra", "Tiros_Puerta_Contra","Centros_Contra","Centros_precisos_Contra", 
                     "Corners_Contra","Faltas_Contra", "Intercepciones_Contra","Robos_Contra","Amarillas_Contra", 
                     "Rojas_Contra","Penaltis_Contra")
head(resultados)

resultados[resultados$Arbitro=='Sergio Álvarez',][8]='Antonio Miguel Mateu Lahoz' #correcting referee


write.csv(resultados,"España_18-19.csv",row.names=FALSE) 
