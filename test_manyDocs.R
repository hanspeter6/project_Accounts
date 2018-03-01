 ## here any number of keys and many docs:

# libraries
library(tm)
library(pdftools)
library(stringr)
library(dplyr)

# vector of all pdf files in current working directory
files <- list.files(path = "/Users/HansPeter/Dropbox/Finances/Bank Statements/Cheque Account", pattern = "pdf$")

# check to see format consistency:
duds <- vector()
for(i in 1:length(files)) {
        try1 <- pdf_text(paste0("/Users/HansPeter/Dropbox/Finances/Bank Statements/Cheque Account/", files[i])) # list of single string per pdf page
        try2 <- unlist(strsplit(try1,"\n"))  # by line
        if(!any(str_detect(try2, "Computer Generated Tax Invoice"))) {
                duds <- append(duds,i)
        }
        
}

# isolate only correctly formatted files:
files <- files[-duds]

# example key words
keys <- c("moment","pps", "salary","atm")

# date pattern for use elsewhere
date_pattern <- "\\d{1,2}/\\d{2}/\\d{4}"

# for many files:
key_frames <- vector() # initiate list of key outputs
all_frame <- vector() # initiate list output documents
for(f in 1:length(files)) {
        
        temp1 <- pdf_text(paste0("/Users/HansPeter/Dropbox/Finances/Bank Statements/Cheque Account/", files[f])) # list of single string per pdf page
        temp2 <- unlist(strsplit(temp1,"\n"))  # by line
        
        # identify end of pages:
        end_pages <- which(str_detect(temp2, "Page")) - 1
        
        # identify how many pages:
        page_num <- length(end_pages)
        
        # id start of each page:
        start_pages <- which(str_detect(temp2, regex("Transaction Description", ignore_case = TRUE))) + 1
        
        # identify end of document # need to think if this is generally true?
        end_document <-  which(str_detect(temp2, "SERVICE FEE")) - 1
        
        # since end of documt < end of second page:
        
        # create subs of temp2 cutting off top, end and page break stuff of all but last page:
        pages_temp <- list()
        for(i in 1: (page_num - 1)) {
                pages_temp[[i]] <- temp2[start_pages[i]:end_pages[i]]
        }
        
        # last page also
        pages_temp[[page_num]] <- temp2[start_pages[page_num]:end_document]
        pages_temp <- unlist(pages_temp)
        
        # create vectors of info for document (all its pages)
        date <- vector()
        description <- vector()
        charge <- vector()
        debit <- vector()
        credit <- vector()
        balance <- vector()
        for(i in 1: length(pages_temp)) {
                
                date[i] <- str_trim(str_sub(pages_temp[i], 1, 16))
                description[i] <- str_trim(str_sub(pages_temp[i], 18, 60))
                charge[i] <- str_trim(str_sub(pages_temp[i], 70, 77))
                debit[i] <- str_trim(str_sub(pages_temp[i], 110, 135)) 
                credit[i] <- str_trim(str_sub(pages_temp[i], 140, 159)) 
                balance[i] <- str_trim(str_sub(pages_temp[i], 162, 180))
                
        }
        tab_pdf <- cbind(date, description = stripWhitespace(description), charge, debit, credit, balance, file = files[f])  
        
        # # now combine description:
        # id which places have date
        date_places <- which(str_detect(tab_pdf[,1], date_pattern)) 
        
        for(i in 1:nrow(tab_pdf)) {
                if(!i %in% date_places) {
                        tab_pdf[i-1,2] <- str_c(tab_pdf[i-1,2], tab_pdf[i,2], sep = ' ')
                        
                }
        }
        
        # get rid of no date cases:
        tab_pdf <- data.frame(tab_pdf[date_places,], stringsAsFactors = FALSE)
        
        # format variables:
        tab_pdf$date <- as.Date(tab_pdf$date, format = "%d/%m/%Y")
        
        # for numbers, first need to get rid of white space btw thousands
        tab_pdf$charge <- str_replace_all(tab_pdf$charge, ' ','')
        tab_pdf$debit <- str_replace_all(tab_pdf$debit, ' ','')
        tab_pdf$credit <- str_replace_all(tab_pdf$credit, ' ','')
        tab_pdf$balance <- str_replace_all(tab_pdf$balance, ' ','')
        
        # for balance also need to get rid of '-':
        tab_pdf$balance <- str_replace_all(tab_pdf$balance, '-', '')
        
        # change to numeric
        tab_pdf$charge <- as.numeric(tab_pdf$charge)
        tab_pdf$debit <- as.numeric(tab_pdf$debit)
        tab_pdf$credit <- as.numeric(tab_pdf$credit)
        tab_pdf$balance <- as.numeric(tab_pdf$balance)
        
        # keep list of tables per document
        all_frame <- rbind(all_frame, tab_pdf)
        
        # now id and extract on keywords:
        temp_keys_output <- vector()
        for(i in 1: length(keys)) {
                a <- tab_pdf[str_detect(tab_pdf$description, regex(keys[i], ignore_case = TRUE)),]
                b <- a %>%
                        dplyr::mutate(key = keys[i]) %>%
                        dplyr::select(key, dplyr::everything())
                temp_keys_output <- rbind(temp_keys_output, b)
        }
        
        # lists of key outputs
        key_frames <- rbind(key_frames,temp_keys_output)
        key_frames <- key_frames %>%
                dplyr::arrange(key,date)
        
}

# checking
View(key_frames[sample(nrow(key_frames), 20),]) # problems with some...did checked parameters for columns again...
View(all_frame[sample(nrow(all_frame), 20),])

try1 <- pdf_text(files[56]) # list of single string per pdf page
try2 <- unlist(strsplit(try1,"\n"))  # by line
str_locate(try2, "22\\s802.05") #151:159 # salary credit
str_locate(try2, "29\\s980.66") #172:180 # balance
str_locate(try2, "6\\s000" ) # (6000) 117:124 and 124:128  #debit


t1 <- pdf_text(files[58]) # list of single string per pdf page
t2 <- unlist(strsplit(t1,"\n"))  # by line
str_locate(t2, "24\\s109.17") # 143:151 # salary credit
str_locate(t2, "7\\s518.29") # 164:172  #balance
str_locate(t2, "2\\s000" ) # (6000) 116:120 and   #debit
