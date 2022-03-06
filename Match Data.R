######################
# Code works 22-03-06#
######################

#Used libraries
library(rvest) #scraping
library(stringr)

#Used Variables
season="21-22"
league="España"

#Initialize DF
result1=data.frame()
date1=data.frame()
referee=data.frame()
teams2=data.frame()
home_away=data.frame(c("Local","Visitante"))
colnames(home_away)<-c()
goal=data.frame()

####Links for the spanish league calendar
url="https://www.futbolfantasy.com/laliga/calendario/2022"
url=read_html(url)

links_matches=as.data.frame(html_nodes(url,".partido.terminado")%>% html_attr("href")) #Only finished games.

#links_matches=as.data.frame(links_matches[196:269,])
#n=2

#Loop for getting all information
for(n in 1:nrow(links_matches)){
  
  #tryCatch({  
    #Read and get all stats
    url=as.character(links_matches[n,1])
    
    html <- read_html(url)
    
    #Get match's stats
    local_stats<-html_nodes(html,".stat") %>% html_nodes(".local")
    local_stats<-html_text(local_stats[1:11])
    
    away_stats<-html_nodes(html,".stat") %>% html_nodes(".visitante")
    away_stats<-html_text(away_stats[1:11]) 
    
    #Bind home and away stats
    result=cbind(rbind(local_stats,away_stats),rbind(away_stats,local_stats))
    result1=rbind(result1,result)
    
    rm(result) #remove result
    
    ####Dates
    
    date<-html_nodes(html,".fecha") 
    date_match<-html_text(date[1])
    
    #Remove characters
    date_match<-gsub("(\t|\n)"," ",as.character(date_match)) 
    date_match<-gsub("h"," ",date_match)
    date_match<-gsub(",","",date_match)
    
    #Trim start and finish space
    date<-trimws(as.character(date_match))
    
    #Split string and convert to character
    date=sapply(as.data.frame(strsplit(date,split = " ")),as.character)
    
    #Remove punctuation
    date[2,1]<-as.numeric(gsub(" .", "", date[2,1]))
    
    #Bind
    date_2=cbind(date[2,1],date[3,1],date[6,1],date[8,1],date[11,1])
    rownames(date_2) <- c()
    date_2=rbind(date_2,date_2)
    date1=rbind(date1,date_2)
    
    ####Referee
    
    ref<-html_nodes(html,".link") %>% html_text()
    
    #Remove \t and \n  
    ref<-gsub("(\t|\n)"," ",ref) 
    ref <- matrix(ref,ncol=1)
    ref=data.frame(ref[c(1:1)])
    #Bind
    ref=rbind(ref,ref)
    referee=rbind(referee,ref)
    
    ####Home and away teams
    
    teams<-html_nodes(html,".nombre") 
    
    #Extract the names
    home<-as.character(html_text(teams[1])) 
    away=as.character(html_text(teams[2]))
    
    #Bind
    teams1=as.data.frame(rbind(home,away))
    teams3=rbind(away,home)
    teams1=cbind(teams1,teams3)
    teams2=rbind(teams2,teams1)
    
    ###Goals
    
    score<-html_nodes(html,".local.score")
    goal_home<-as.numeric(html_text(score[1])) 
    
    score<-html_nodes(html,".visitante.score") 
    goal_away<-as.numeric(html_text(score[1])) 
    
    #Bind
    goal1=rbind(goal_home,goal_away)
    goal2=rbind(goal_away,goal_home)
    
    goal4=rbind(goal_home-goal_away,goal_away-goal_home)
    goal3=cbind(goal1,goal2)
    
    #Add column based on the goals
    if(goal3[1,1]>goal3[1,2]){
      winner=rbind("Victoria","Derrota")
      goal3=cbind(goal3,winner)
    }else if(goal3[1,1]==goal3[1,2]){
      winner=rbind("Empate","Empate")
      goal3=cbind(goal3,winner)
    } else{
      winner=rbind("Derrota","Victoria")
      goal3=cbind(goal3,winner)
    }
    
    #Bind
    
    goal=rbind(goal,cbind(goal3,goal4))
    
    print(n)
    
  #},  error=function(e){})
}  

#Delete rows in case of NA
result1=na.omit(result1)

#Rename columns
row.names(teams2)=c()
colnames(teams2)=c()
teams2=cbind(home_away,teams2)

#results2=results

#Bind in one df
results=cbind(league,season,date1,referee,teams2,goal,result1)

#Rename columns

names(results)<-c("Liga","Temporada","Jornada","Dia_Semana","Mes","Año","Hora", "Arbitro", "Local_Vis","Equipo","Rival","Gol_Fav",
                  "Gol_Cont","Resultado", "Dif_Goles","Tiros", "Tiros_Puerta","Centros","Centros_precisos", "Corners","Faltas", "Intercepciones","Robos", 
                  "Amarillas", "Rojas","Penaltis_Fav", "Tiros_Contra", "Tiros_Puerta_Contra","Centros_Contra","Centros_precisos_Contra", 
                  "Corners_Contra","Faltas_Contra", "Intercepciones_Contra","Robos_Contra","Amarillas_Contra", 
                  "Rojas_Contra","Penaltis_Contra")

#Check DF
tail(results)

#Save DF
write.csv(results,"21_22_Spain.csv",row.names=FALSE)
