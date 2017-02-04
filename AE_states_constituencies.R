library(rvest)


# Function to create Directory
create_directory <- function(path1, path2) {
  if(!dir.exists(file.path(path1,path2))){
    dir.create(file.path(path1,path2), showWarnings = FALSE)
  }
  return(0)
}

# Function to write data into files
create_file <- function(info,filename){
  
  if(!file.exists(filename)){
    write(info, file=filename, append = TRUE)
  }else{
    file.remove(file=filename)
    write(info, file=filename, append = TRUE)   
  }
  return(0)
}

wd <- getwd()
wd=substr(wd,0,nchar(wd)-8)

# Creating the AE Directory if non existent
create_directory(wd,"AE")

wd= paste(wd,"AE/",sep="")
#print(wd)

# Polling the myneta homepage to get information about the states
my_neta <- read_html("http://myneta.info/")
states_nodes <- html_nodes(my_neta,,"//a[contains(@href,'state_assembly')]")
states_text <- html_text(states_nodes)
states_link <- html_attr(states_nodes, "href")

#print(states_text)

# The Scraping Begins  
 for(i in 1:length(states_text)){
 
   # Creating individual directory for each state    
    create_directory(wd,states_text[i])

   # Removing the Gap in the URL  
    states_link[i] <- gsub(" ","%20", states_link[i])
 
   # Polling individual state    
    each_state_url <- read_html(states_link[i])
     
    each_state_year = NULL
    each_state_link = NULL
   # Extracting all years of the state election
    each_state_year <- html_nodes(each_state_url,,"//h3[contains(@class,'title yellow first')]")
   # Extracting the corresponding links of all years of the state election  
    each_state_link <- html_attr(html_nodes(each_state_url,,"//a[contains(text(),'All')]"),'href')
   # print(each_state_link)
   # Loop to fetch each year name and create individual directory under each state 
   # election_year = NULL
   # election_year = c()
    #print(each_state_year)
    for(j in 1:length(each_state_year)){
      
       # Removing the gaps to maintiain clear year name
       year<-gsub(" ","", html_text(each_state_year[j]))
       year<-substr(year,nchar(year)-3,nchar(year)) 
       
       # Creating Directory for each year of the AE election
       dir_year <- paste(wd,states_text[i],sep = "")
       create_directory(dir_year,year)
       
       constituencies_links = NULL
       constituencies_names = NULL
       
       # Polling individual states to get the names and links of the constituencies
       constituencies_url <- read_html(each_state_link[j])
       constituencies_links <- html_attr(html_nodes(constituencies_url,,"//a[contains(@href,'show_candidates&constituency_id')]"),'href')
       constituencies_names <- html_text(html_nodes(constituencies_url,,"//a[contains(@href,'show_candidates&constituency_id')]"))
       
       
       ##print(constituencies_links)
       ##print(constituencies_names)
       dir_constituency <- paste(dir_year,"/",year,sep="")
       #constituencies_file <- paste(dir_constituency,"/","constituencies_links",sep = "")
       
       
       if(length(constituencies_names>=1)){
             for(k in 1:length(constituencies_names)){
               #print(dir_constituency)
               #print(constituencies_names[k])
               
               # The constituency directories for each year are created
               create_directory(dir_constituency, constituencies_names[k])
               
               match <- regexpr("http",constituencies_links[k])
                 if(match[1]>0){
                    #print("match")
                  }else{
                    # The constituencies are appenened with the header that is not present in the intitial scrap
                    constituencies_links[k] <- paste(each_state_link[j],constituencies_links[k],sep = "")
                  }
               constituencies_file <- paste(dir_constituency,"/",constituencies_names[k],"/","constituencies_link",sep = "")
               create_file(constituencies_links[k],constituencies_file)
              }
          }
      # All constituencies links are stored in one file
      #create_file(constituencies_links,constituencies_file)
      # election_year <- append(election_year, substr(year,nchar(year)-3,nchar(year)))
    }

    # print(election_year)
   
}

#print(states_link)
