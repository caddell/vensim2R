library(stringi)
library(stringr)
library(dplyr)
library(readr)

text <- read_delim("yourfilehere.txt", delim = "\t", col_names = FALSE, trim_ws = TRUE, skip_empty_rows = TRUE)

text <- text %>% na.omit()

df <- text %>% rename(text = X1) %>% 
  mutate(text = str_remove_all(text, " ")) %>% 
  mutate(text = str_to_lower(text))


#add space before and after puncuation
addspace_punct <- function(string){
  #split string into individual characters
  temp <- unlist(strsplit(string, split = ""))
  
  #loop through characters
  for (i in 1:length(temp)) {
    #if character is puncuation, add spaces
    if(!grepl(pattern = '[[:alnum:]]|\\.', temp[i], perl = TRUE)){
      temp[i] <- paste(" ", temp[i], " ")
    }
  }
  temp <- paste(temp, collapse = "")
  return(temp)
}

#pull stock's intiial condition
stock_int_cond <- function(string){
  #check if stock has initial condition
  if(str_detect(string = string, pattern = "[,].*[)]")){
    #find start of initial condition
    start <- str_locate(string = string, pattern = "[,].*[)]")[1]
    #find end of initial condition
    end <- str_locate(string = string, pattern = "[,].*[)]")[2]
    #pull initial condition
    int_cond <- substr(string, start+1, end-1)
    stock_str <- substr(string, start = 1, stop = str_locate(string, pattern = "="))
    return(paste(stock_str, int_cond))
  }else{
    warning("No Initial Condition Found")
    return(NA)
  }
}


#pull stocks
stocks <- df %>% 
  filter(str_detect(text, "integ"))

#clean stocks
stocks <- stocks %>% 
  mutate(text = trimws(text)) %>%
  rowwise() %>% 
  mutate(int_cond = stock_int_cond(text)) %>% 
  #remove int condition
  mutate(text = str_remove_all(text, "integ\\(")) %>% 
  mutate(text = str_remove_all(text, "\\,.*")) %>% 
  mutate(text = addspace_punct(text)) 

#pull auxs
auxs <- df %>% 
  filter(!str_detect(text, "integ|finaltime|initialtime|saveper|timestep")) %>%
  filter(str_detect(text, "\\=\\d*$|\\=.\\d|\\=[[:digit:]]*[.]\\d*")) %>% 
  rowwise() %>% 
  mutate(text = addspace_punct(text)) %>% 
  mutate(id = trimws(str_remove(text, "=.*")))
  
#pull interactions
interactions <- df %>% 
  filter(!str_detect(text, "integ|finaltime|initialtime|saveper|timestep")) %>%
  filter(!str_detect(text, "\\=\\d*$|\\=.\\d|\\=[[:digit:]]*[.]\\d*")) %>%  
  rowwise() %>% 
  mutate(text = addspace_punct(text)) %>% 
  mutate(id = trimws(str_remove(text, "=.*")))


#pull model setup
setup <- df %>% 
  filter(str_detect(text, "finaltime|initialtime|saveper|timestep")) %>%
  rowwise() %>% 
  mutate(text = addspace_punct(text))


############build the file
build_model <- function(stocks, auxs, interations, setup){
  output <- "####System Dyanmics Model"
  output <- c(output, "library(deSolve)", " ","#model setup")
  
  #add model setup
  for (i in 1:nrow(setup)) {
    output <- c(output, paste(setup$text[i]))
  }
  #add line break after setup
  output <- c(output, " ")
  
  #create time vector
  output <- c(output, "#create a time vector", "simtime <- seq(initialtime, finaltime, by= timestep)", " ")
  

  
  #add auxs initial condition
  output <- c(output, "#add auxs", "auxs <- c(")
  for (i in 1:nrow(auxs)) {
    if(i<nrow(auxs)){
      output <- c(output, paste("  ", auxs$text[i],","))
    }else{
      output <- c(output, paste("  ", auxs$text[i],")"), " ")
    }
  }

  #add stocks initial condition
  output <- c(output, "#add stocks", "stocks <- c(")
  for (i in 1:nrow(stocks)) {
    if(i<nrow(stocks)){
      output <- c(output, paste("  ", stocks$int_cond[i],","))
    }else{
      output <- c(output, paste("  ", stocks$int_cond[i],")"), " ")
    }
  }
  
  
  output <- c(output, "# This is the model function")
  output<-c(output, "model <- function(time, stocks, auxs){", "  with(as.list(c(stocks, auxs)),{")
  
  #add aux calcuations
  output <- c(output, "#add aux calculations")
  for (i in 1:nrow(interactions)) {
      output <- c(output, paste("  ", interactions$text[i]))
  }
  
  #add stock calcuations
  output <- c(output, "#add stock calculations")
  for (i in 1:nrow(stocks)) {
      output <- c(output, paste("  ", stocks$text[i]))
  }
  
  #build return
  output <- c(output, " ", "#return data", "return(list(c(")
  #add stocks
  for(i in 1:nrow(stocks)){
    if(i < nrow(stocks)){
      output <- c(output, paste("  ", stocks$text[i], ", "))
    }else{
      output <- c(output, paste("  ", stocks$text[i], "),"))
    }
  }
  
  #add aux and interactions
  for(i in 1:nrow(auxs)){
    output <- c(output, paste("  ", auxs$id[i], " = ", auxs$id[i], ","))
  }
  for(i in 1:nrow(interactions)){
    if(i < nrow(interactions)){
      output <- c(output, paste("  ", interactions$id[i], " = ", interactions$id[i], ","))
    }else{
      output <- c(output, paste("  ", interactions$id[i], " = ", interactions$id[i],"))"), "  })", "}")
    }
  }
  
  output <- c(output, " ", "data <- data.frame(ode(y= stocks, times = simtime, func = model, parms = auxs, method = 'euler'))")
  
  return(output)
  
}

test <- build_model(stocks, auxs, interactions, setup)


#write the file to working directory
fileConn<-file("test_file.R")
writeLines(test, fileConn)
close(fileConn)



