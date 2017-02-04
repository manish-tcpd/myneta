library(rvest)
library(curl)
args=commandArgs(TRUE)

print(args)

# Function to create Directory
create_directory <- function(path1, path2) {
  if(!dir.exists(file.path(path1,path2))){
    dir.create(file.path(path1,path2), showWarnings = FALSE)
  }
  return(0)
}


# Function to write data into files
create_file <- function(info,filename,code_no){
  
  if(!file.exists(filename)){
    write(info, file=filename, append = TRUE)
  }else{
    if(code_no == "0"){
      write(info, file=filename, append = TRUE)}else{
        file.remove(filename)
        write(info, file=filename, append = TRUE)
      }   
  }
  return(0)
}



wd=getwd()
print(wd)
tryCatch({
  for (i in 1:length(args)){
    
    # Fetching the candidates links from each candidate folder
    candidates_link = readLines(args[i])
    #print(candidates_link)
    match_one <- regexpr('candidates_list',args[i])
    
    # Removing the trailer to move back in the directory
    candidates_file_path = substr(args[i],0,match_one[1]-1)
    #print(candidates_file_path)
    
    for(j in 1:length(candidates_link)){
      #print(candidates_link)
      x<- tryCatch({
        
        print(candidates_link[j])
        # Getting the unique state name that myneta changes based on the state election
        match <- regexpr("candidate",candidates_link[j])
        #print(match)
        state_match <- regexpr("myneta.info", candidates_link[j])
        state_unique_name = substr(candidates_link[j],state_match[1]+12, match[1]-2)
        #print(state_unique_name)
        
        # Getting the unique id for each candidate so that the file name is created based on unique state name and id
        candidate_id = strsplit(candidates_link[j],"id=")
        html_file_name = paste(candidates_file_path, state_unique_name,"_", candidate_id[[1]][2],".html",sep="")
        #print(html_file_name)
        
        # Scraping the webpage of the candidate & creating a html fime
        download.file(candidates_link[j],destfile = html_file_name, method = "libcurl")
        
        # create_file(candidate_html_page,html_file_name)
      },
      warning=function(w){     
        return(paste( "Warning:", conditionMessage(w)));
      }, 
      error = function(e) {      
        #print(wd)
        discrepencies_file <- substr(wd,0,nchar(wd)-8)
        discrepencies_file <- paste (discrepencies_file,"candidates_discrepency",sep="")
        create_file(candidates_link,discrepencies_file,0)
        print(discrepencies_file)
        return(paste( "Error:", conditionMessage(e)));
        
      }, 
      finally={
        print("This is try-catch inner loop. check the output.")
      })
      print(x);
    }
  }
},
warning=function(w){     
  return(paste( "Warning:", conditionMessage(w)));
}, 
error = function(e) {      
  print(wd)
  discrepencies_file <- substr(wd,0,nchar(wd)-8)
  discrepencies_file <- paste (discrepencies_file,"candidates_name_discrepency",sep="")
  create_file(args[i],discrepencies_file,0)
  print(discrepencies_file)
  return(paste( "Error:", conditionMessage(e)));
  
}, 
finally={
  print("This is try-catch outer loop . check the output.")
})





