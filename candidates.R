library(rvest)
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

for (i in 1:length(args)){
  tryCatch({    
    # Fetching the path of the constituencies_link of a constituency
    constituencies_link = readLines(args[i])
    #constiuency_page = readLines(constituencies_link)
    # Fetching the page of the candidates
    candidates_url <- read_html(constituencies_link[1])
    
    # Fetching the links & Names of the candidates pages
    candidates_links <- html_attr(html_nodes(candidates_url,,"//a[contains(@href,'candidate.php?candidate_id')]"),'href')
    candidates_names <- html_text(html_nodes(candidates_url,,"//a[contains(@href,'candidate.php?candidate_id')]"))
    
    print (args[i])
    
    match_one <- regexpr('constituencies_link',args[i])
    if(match_one[1] > 0){
      constituency_file_path = substr(args[i],0,match_one[1]-1)
      # Creating candidates directory
      create_directory(constituency_file_path,"candidates")
      candidates_file_path = paste(constituency_file_path,"candidates",sep="")  
      
      x<- tryCatch({
        for(l in 1:length(candidates_names)){
          # Removing the constituency trailer and adding the candidates link to the header
          match <- regexpr("index",constituencies_link[1])
          candidate_unique_link = paste(substr(constituencies_link[1],0,match[1]-1),candidates_links[l],sep="")
          candidates_links[l] = candidate_unique_link
          #print(candidate_unique_link)
          
          # Getting the unique state name that myneta changes based on the state election
          state_match <- regexpr("myneta.info", constituencies_link[1])
          state_unique_name = substr(constituencies_link[1],state_match[1]+12, match[1]-2)
          #print(state_unique_name)
          
          # Getting the unique id for each candidate so that the file name is created based on unique state name and id
          candidate_id = strsplit(candidates_links[l],"id=")
          html_file_name = paste(candidates_file_path,"/",state_unique_name,"_", candidate_id[[1]][2],".html",sep="")
          print(html_file_name)
          
          # Scraping the webpage of the candidate & creating a html fime
          # candidate_html_page = readLines(candidate_unique_link)
          #  create_file(candidate_html_page,html_file_name)
          
        }
        create_file(candidates_links, paste(candidates_file_path,"/","candidates_list",sep=""),1)
        #print(candidates_names)
      },
      warning=function(w){     
        return(paste( "Warning:", conditionMessage(w)));
      }, 
      error = function(e) {      
        print(wd)
        discrepencies_file <- substr(wd,0,nchar(wd)-8)
        discrepencies_file <- paste (discrepencies_file,"constituecy_discrepency",sep="")
        create_file(constituencies_link,discrepencies_file,0)
        print(discrepencies_file)
        return(paste( "Error:", conditionMessage(e)));
        
      }, 
      finally={
        print("This is try-catch test 1. check the output.")
      })
      print(x);
    }
    
    
  },
  warning=function(w){     
    return(paste( "Warning:", conditionMessage(w)));
  }, 
  error = function(e) {      
    print(wd)
    discrepencies_file <- substr(wd,0,nchar(wd)-8)
    discrepencies_file <- paste (discrepencies_file,"name_discrepency",sep="")
    create_file(args[i],discrepencies_file,0)
    print(discrepencies_file)
    return(paste( "Error:", conditionMessage(e)));
    
  }, 
  finally={
    print("This is try-catch test 2. check the output.")
  })
}

