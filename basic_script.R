# This script is to scrap the basic information that is available in MYNETA

library(rvest)
library(xml2)
#parameters for the generic dataframe. Headers define the name of the various parameters. CSS paths give the path for the particular parameter. This has been fetched through the selector gadget. I also learnt the commands using CSS diner.
#first and last are integer types used for string manipulation functions. They are constant for the whole website. Hence put them in two different parameters
wd<- getwd()
#wd=substr(wd,0,nchar(wd)-7)
#wd= paste(wd,"myneta/",sep="")
print(wd)

main_dataset <- data.frame("URL" = character(),
                           "ID"=character(),
                           "Name" = character(),
                           "Result" = character(),
                           "Constituency"= character(),
                           "District"= character(),
                           "Party"= character(),
                           "Relation"= character(),
                           "Age"= character(),
                           "Address"= character(),
                           "Name_Enrolled_as_Voter_in"= character(),
                           "Email"= character(),
                           "Contact_Number"= character(),
                           "Education"= character(),
                           "Details"= character(),
                           "Self_Profession"= character(),
                           "Spouse_Profession"= character(),
                           "Movable_Assests"= character(),
                           "Immovable_Assests" = character(),
                           "Total_Assests"= character(),
                           "Liabilities"= character(),
                           "No_Of_Cases"= character(),
                           "Pan_Given"= character(),
                           "Financial_Year"= character(),
                           "Total_Income_Showed_In_ITR" = character()
)

# Headers here are the column names 
headers <- c("Name","Constituency","Party","S/o|D/o|W/o","Age","Address","Name Enrolled as Voter in","Email","Contact Number","Education", "Details","Self_Profession","Movable_Assets","Immovable_Assets","Total_Assests","Liabilities","Total_No_Cases","Pan_Given")

# CSS paths are used to scrap the information about particular information correspoding to the headers mentioned 

# This is because the data pre 2010 doesn't have the Details of PAN table
csspaths_pre_2010 <-c(".main-title",".grid_3.alpha>h5",".grid_2.alpha:nth-child(3)",".grid_2.alpha:nth-child(4)",".grid_2.alpha:nth-child(5)",".grid_2.alpha:nth-child(6)",".grid_3.alpha:nth-child(7)",".grid_2.alpha:nth-child(8)",".grid_2.alpha:nth-child(9)",".left-margin:nth-child(3)",".left-margin:nth-child(4)",".grid_3.alpha>p","tr:nth-child(10) td:nth-child(7) b","td:nth-child(7) b",".red b",".blue b",".omega div .left-green-border div","tr:nth-child(11) b")
# This is because the data post 2010 has the PAN table and the CSS selector corresponing to the PAN details is different
csspaths_post_2010 <-c(".main-title",".grid_3.alpha>h5",".grid_2.alpha:nth-child(3)",".grid_2.alpha:nth-child(4)",".grid_2.alpha:nth-child(5)",".grid_2.alpha:nth-child(6)",".grid_3.alpha:nth-child(7)",".grid_2.alpha:nth-child(8)",".grid_2.alpha:nth-child(9)",".left-margin:nth-child(3)",".left-margin:nth-child(4)",".grid_3.alpha>p","tr:nth-child(12) td:nth-child(7) b","tr:nth-child(8) td:nth-child(7) b",".red b",".blue b",".omega div .left-green-border div","tr:nth-child(2) td:nth-child(2)")

# This both are used to perform string operations in the respective candidate details
first<- c(0,10,16,39,19,35,28,7,17,0,2,18,5,5,4,4,0,0)
last<- c(1,2,2,9,9,9,2,0,0,0,0,0,1,1,0,0,0,0)

# Data set pre 2010
data_set_pre<- data.frame(headers,csspaths_pre_2010,first,last)

# Data set post 2010
data_set_post <- data.frame(headers,csspaths_post_2010,first,last)

#print(data_set_pre)
#print(data_set_post)

count <- 0

args=commandArgs(TRUE)



if(length(args)>0){
  for(i in 1:length(args)){
    
    # reading the html page of the candidate
    inp <- read_html(args[i])
    print(args[i])
    string_split <- strsplit(args[i],".html")
    string_split <- strsplit(string_split[[1]][1],"candidates/")
    string_split <- strsplit(string_split[[1]][2],"_")
    
    # This is the unique state code that is given to each state election
    state_code <- string_split[[1]][1]
    
    # This is the unique id given to each candidate in each election
    id <- string_split[[1]][2]
    
    # This is the URL address of each candidate
    candidate_URL <- paste("http://www.myneta.info/",state_code,"/candidate.php?candidate_id=",id, sep = "")
    
    # Removing all the elements in the emptylist Creating empty list to append all the details of a particualar candidate.
    emptylist = NULL
    emptylist = c()
    
    # Appending the URL of the candidate
    emptylist<-append(emptylist,list(candidate_URL))
    
    # Appending the id of the candiadate
    emptylist<-append(emptylist,list(id))

    # Checking whether the page has data or it has Page not found
    f_node <- html_node(inp,"div.grid_9 h2")
    f_text <- html_text(f_node)
    if(f_text == "Page Not Found!!"){
      count <- count +1
      
      # Filling all the fields with NA.
      for(i in 1:23){
        emptylist<-append(emptylist,"NA")
        }
      
      # Creating the dataframe based on the condition above
      emptydataframe=as.data.frame(emptylist)
      colnames(emptydataframe)<-colnames(main_dataset)
      main_dataset<-rbind(main_dataset,emptydataframe)
      
    }else{
      
      # This post test is to check if the PAN details table exists in the page or not
      post_test <- html_text(html_node(inp,".blue:nth-child(1)"))
      
      #print(post_test)
      
      if(post_test ==" Details of PAN and status of Income Tax return "){
        data_set <- data_set_post
      }else{
        data_set <- data_set_pre  
      }
      
      # Printing the data_set 
      #print(data_set)
      
      # The loop is being used for individual details of the candidates  
      for(a in 1:nrow(data_set)){
        
        # Each row of the data set is being polled individually to poll each header
        a_header<-as.character(data_set[a,1])
        a_csspath<-as.character(data_set[a,2])
        a_first <- as.integer(data_set[a,3])
        a_last <-as.integer(data_set[a,4])
        
        # Each node is being polled to fetch the individual information of the header based on the css path
        a_node<-html_node(inp,a_csspath)
        
        # The tags are being removed to get the exact text related to the header
        a_text<-html_text(a_node)
        #print(a_header)
        #print(a_text)
        
        # To scrap the Name and the result
        if(a_header == "Name"){
            
            # To check whether the canidate result 
            pos= regexpr("Winner",a_text)
               if(pos[1]!=-1){
                     a_last<- 9
                     a_result <-"WON"
                     }else{
                      a_result <- "LOST"
                     }
              
            
              # This gives the canidate Full name
              a_keep=substr(a_text,a_first,nchar(a_text)-a_last)
              
          
              # Appending the candidate name & result to the list
              emptylist<-append(emptylist,list(a_keep))
              emptylist<-append(emptylist,list(a_result))
        }
        # To scrap the District & Constituency information
        else if (a_header=="Constituency"){
          pos1 = gregexpr("\\(",a_text)
          pos2 = gregexpr(")", a_text)
          
          if(is.na(pos2[[1]][2])){
                  
            a_district=substr(a_text,pos1[[1]][1]+1,pos2[[1]][1]-1)
            a_keep=substr(a_text,a_first,pos1[[1]][1]-3)
                    
          }else{
            
            a_district=substr(a_text,pos1[[1]][2]+1,pos2[[1]][2]-1)
            a_keep=substr(a_text,a_first,pos2[[1]][1]) 
          }
          # Printing the Constituency name & District name
          #print(a_keep)
          #print(a_district)
          # Appending the list with constituency & District names
          emptylist<-append(emptylist,list(a_keep))
          emptylist<-append(emptylist,list(a_district))
          
        }
        
        # To get the information of Name Enrolled as Voter in 
        else if (a_header=="Name Enrolled as Voter in")  
          {
            a_keep = strsplit(a_text,"\t\t\t")
            a_keep = paste(a_keep[[1]][1],a_keep[[1]][2],"")
            a_keep = substr(a_keep,a_first,nchar(a_keep)-a_last)
            # Printing the Name Enrolled as Voter in
            #print(a_keep)
            
            # The information is appended to the list
            emptylist<-append(emptylist,list(a_keep))
             
        }
        
        # To get the information of the profession of the candidate
        else if (a_header=="Self_Profession")
          {
             # To get the information of the profession of the Spouse  
             pos = regexpr("Spouse Profession",a_text)
             a_last <- nchar(a_text)-pos[1]+1
        
             a_spouseprofession = substr(a_text,pos[1]+18,nchar(a_text)-1)
             # Printing the Spouse Profession
             #print(a_spouseprofession)
             
             a_keep=substr(a_text,a_first,nchar(a_text)-a_last)
            
             # Printing the Self Profession  
             #print(a_keep)
             
             # Appending the Self Profession & the Spouse Profession
             emptylist<-append(emptylist,list(a_keep))
             emptylist<-append(emptylist,list(a_spouseprofession))
        }
        
        # To find the total No.of Cases
        else if (a_header=="Total_No_Cases")
           {
             pos = regexpr("Number of Criminal Cases",a_text)
     
             if(pos[[1]][1]== -1){a_keep=0}else{
                 a_keep=substr(a_text,pos[[1]]+1+nchar("Number of Criminal Cases:"),nchar(a_text))
             }
             # Printing the total No.of Cases
             #print(a_keep)
             emptylist<-append(emptylist,list(a_keep))
          }
        # To find the details of the PAN & hte income details
        else if (a_header=="Pan_Given")
          {   
              # Condition to check whether pre 2010
              if(a_csspath == "tr:nth-child(11) b"){
                if(a_text != "PAN given-Y"){ 
                  # When PAN is not given
                  a_keep=""
                  a_financial_year=""
                  a_info_itr=""}else{
                  
                  # When PAN is given
                  a_keep <- "Y"
                  a_financial_year=""
                  a_info_itr=""
                }
              }else{  
                 # When the data is post 2010
                 a_keep <- html_text(html_node(inp,"tr:nth-child(2) td:nth-child(2)"))
                 a_financial_year<-html_text(html_node(inp,"tr:nth-child(2) td:nth-child(3)"))
                 
                 # String manipulations to find the income tax returns
                 a_info_itr<-html_text(html_node(inp,"tr:nth-child(2) b"))
                 a_info_itr<-substr(a_info_itr,4,nchar(a_info_itr))
                 a_info_itr<- strsplit(a_info_itr,",")
                 a_temp=NULL
                 for(a in 1:length(a_info_itr[[1]])){
                   a_temp<-paste(a_temp,a_info_itr[[1]][a])
                    #print(a_temp)
                 }
                 a_info_itr<-gsub(" ","",a_temp)
              }
          
              #print(a_keep)
              #print(a_financial_year)
              #print(a_info_itr)
              
              # Appending all the the three details in the list 
              emptylist<-append(emptylist,list(a_keep))
              emptylist<-append(emptylist,list(a_financial_year))
              emptylist<-append(emptylist,list(a_info_itr))
        }
        # To find the details of Total_Assets & Liabilities
        else if(a_header=="Total_Assests"| a_header=="Liabilities"){
          
          
          #print(a_text)
          if(a_text=="Nil"){
            a_keep="0"
          }else{
            a_keep=substr(a_text,a_first,nchar(a_text)-a_last)
            a_keep = strsplit(a_keep,",")
            a_temp=NULL
            for(a in 1:length(a_keep[[1]])){
              a_temp=paste(a_temp,a_keep[[1]][a])
              #print(a_temp)
            }
            a_keep=gsub(" ","",a_temp)
          }
          #print(a_header)  
          #print(a_keep)
          # Appending the data
          emptylist<-append(emptylist,list(a_keep))
        }
        
        # To find the details of Immovable_Assets
        else if (a_header== "Immovable_Assets"){
          
          # This condition is for post 2010
          if(a_csspath == "tr:nth-child(8) td:nth-child(7) b"){
            if(a_text=="Nil"){
              a_keep="0"
            }else{
              
              a_keep=substr(a_text,a_first,nchar(a_text)-a_last)
              a_keep = strsplit(a_keep,",")
              a_temp=NULL
              for(a in 1:length(a_keep[[1]])){
                a_temp=paste(a_temp,a_keep[[1]][a])
                #print(a_temp)
              }
              a_keep=gsub(" ","",a_temp)
            }
            #print(a_header)  
            #print(a_keep)
    
          }
          else{
            
            # We are using the html_nodes tag here because the immovable assets is present in the 3rd element.
            a_node<-html_nodes(inp,a_csspath)
            
            # The tags are being removed to get the exact text related to the header
            a_text<-html_text(a_node)
            
            # This change is respect to the difference with the pre-2010 data
            a_first <- "4"
            
            a_keep=substr(a_text[3],a_first,nchar(a_text[3])-a_last)
            a_keep = strsplit(a_keep,",")
            a_temp=NULL
            for(a in 1:length(a_keep[[1]])){
              a_temp=paste(a_temp,a_keep[[1]][a])
              #print(a_temp)
            }
            a_keep=gsub(" ","",a_temp)
            #print(a_header)
            #print(a_keep)
          }
          # Appending the data
          #print(a_keep)  
          emptylist<-append(emptylist,list(a_keep))
          #print(emptylist)
          }
        
        # To find the details of Movable Assets and other variables
        else{
            if(a_header=="Movable_Assets")
               {  
              
                  # This is the condition for pre-2010 data
                  if (data_set[14,2] == "td:nth-child(7) b" ){
                    
                    if(a_text=="Nil"){
                      a_keep="0"
                    }else{ 
                      # This is changed based on the positioning of the string
                      a_first <- "4"
                      
                      a_keep=substr(a_text,a_first,nchar(a_text))
                      a_keep = strsplit(a_keep,",")
                      a_temp=NULL
                      for(a in 1:length(a_keep[[1]])){
                        a_temp=paste(a_temp,a_keep[[1]][a])
                        #print(a_temp)
                      }
                      a_keep=gsub(" ","",a_temp)
                    }
                    #print(a_header)
                    #print(a_keep)
                  }
                  else{
                    if(a_text=="Nil"){
                      a_keep="0"
                    }
                    else{
                      a_keep=substr(a_text,a_first,nchar(a_text)-a_last)
                      a_keep = strsplit(a_keep,",")
                      a_temp=NULL
                      for(a in 1:length(a_keep[[1]])){
                        a_temp=paste(a_temp,a_keep[[1]][a])
                        #print(a_temp)
                      }
                      a_keep=gsub(" ","",a_temp)
                    }
                    #print(a_header)
                    #print(a_keep)
                  }
              # Appending the data
              emptylist<-append(emptylist,list(a_keep))  
             
               }else{
                 a_keep=substr(a_text,a_first,nchar(a_text)-a_last)
                 # Appending the data
                 emptylist<-append(emptylist,list(a_keep))
               }
             #print(a_header)            
             #print(a_keep)
             
          }
          
          }        
     
      #print(emptylist)
      emptydataframe=as.data.frame(emptylist)
      colnames(emptydataframe)<-colnames(main_dataset)
      main_dataset<-rbind(main_dataset,emptydataframe)

       filename = paste("/out","csv",sep=".")
       filename = paste(wd,filename,sep="")
       if(!file.exists(filename)){
         write.csv(emptydataframe,file=filename,row.names = FALSE)
       }else{
         write.table(emptydataframe,file=filename,append=TRUE,row.names=FALSE,col.names=FALSE, sep=",")
       }
      
      #print(main_dataset)
      #print(main_dataset$Immovable_Assests)
    } 
    }
    
    #print(candidate_URL)

  }else{
  print("There are no arguments given. Please Check")
}





#   # emptydataframe=as.data.frame(emptylist)
#   # colnames(emptydataframe)<-colnames(main_dataset)
#   # main_dataset<-rbind(main_dataset,emptydataframe)
#   # 
#   
# }
#print(args[1])
#print(main_dataset)

# filename = paste(args[2],args[3],"MN2",sep="_")
# filename = paste(filename,"csv",sep=".")
# filename = paste(wd,filename,sep="")
# if(!file.exists(filename)){
#   write.csv(emptydataframe,file=filename,row.names = FALSE)
# }else{
#   write.table(emptydataframe,file=filename,append=TRUE,row.names=FALSE,col.names=FALSE, sep=",")
# }



#write.table(emptydataframe,file='up1.csv',append=TRUE,row.names=FALSE,col.names=FALSE, sep=",")
#print("done")
#summary(main_dataset)


