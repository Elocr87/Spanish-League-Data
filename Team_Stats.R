#Used libraries
library(rvest) #scraping
library(stringr) #str_split
library(dplyr) #Convert to numeric
library(glue) 

#Set working directory

setwd("C:\\Users\\devil\\Documents\\Estadísticas\\España")

#Used Variables
season="2021_2022"
league="Spain"

#Links for the spanish league stats
url=glue("https://resultados.as.com/resultados/futbol/primera/{season}/ranking/")
url=read_html(url)

#Get all stats' links
url_stats=html_nodes(url,".link-mas-info")%>%html_nodes("a")%>%html_attr("href")

#Paste the URL
url_stats=paste("https://resultados.as.com", url_stats,sep='')

n=0 #start iteration

for(url in url_stats){
        
        #Get stat name
        stat_name_split=str_split(url,'/')
        stat_name=stat_name_split[[1]][length(stat_name_split[[1]])-1]
        stat_name=gsub("-","_",stat_name)
        
        #Read url
        url=read_html(url)
        
        #Get the name of the clubs
        club_name=html_elements(url,'[class="name"]')%>%html_text()
        
        #Get the stats
        stat=html_elements(url,'[class="cantidad"]')%>%html_text()
        stat=as.integer(gsub('[^0-9.-]','',stat))
        
        if(n==0){
                df_final=as.data.frame(cbind(club_name,stat)) #Column bind name and stats
                
                names(df_final)=c('Club',stat_name) #Rename columns
                
                df_final=df_final[order(df_final$Club),]  #Sort DF
                
        } else{
                df_tmp_2=as.data.frame(cbind(club_name,stat)) #Column bind name and stats
                
                names(df_tmp_2)=c('Club',stat_name) #Rename columns
                
                #df_tmp_2=df_tmp_2[order(df_tmp_2$Club),]  
                
                df_final=merge(x = df_final, y = df_tmp_2, by = "Club", all.x = TRUE) #Merge stats by club
                
        }
        
        print(n)
        #Increase N by 1
        n=n+1
        
        #Delay the loop 0.5 seconds
        Sys.sleep(0.5)
}

#Transform all 'NA' to '0'

df_final[is.na(df_final)] = '0'

#Convert all numerical columns to number

df_final = df_final %>% mutate_at(c(2:ncol(df_final)), as.numeric)

#Rename team

df_final[df_final=='R. Sociedad']='Real Sociedad'

#Check DF
tail(df_final)

#Save DF
write.csv(df_final,glue("{season}_{league}_team_stats.csv"),row.names=FALSE)