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
      
    # Fetching the constituency discrepency links from the file
    constituency_discrepency_link = readLines(args[i])
    #print(constituency_discrepency_link)
    
    
    for(j in 1:length(constituency_discrepency_link)){
      
      print(constituency_discrepency_link[j])
      # Reading each of the constituency link that as a problem with the data
      constituat
      
      #details <- strsplit(details, split = "→")

      print(details)  
      #print(candidates_link)
    # x<- tryCatch({
    #         
    # 
    #     },
    #      warning=function(w){     
    #        return(paste( "Warning:", conditionMessage(w)));
    #      }, 
    #      error = function(e) {      
    #        #print(wd)
    #        discrepencies_file <- substr(wd,0,nchar(wd)-8)
    #        discrepencies_file <- paste (discrepencies_file,"candidates_discrepency",sep="")
    #        #create_file(candidates_link,discrepencies_file,0)
    #        print(discrepencies_file)
    #        return(paste( "Error:", conditionMessage(e)));
    #       
    #      }, 
    #      finally={
    #        print("This is try-catch inner loop. check the output.")
    #      })
    #      print(x);
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





