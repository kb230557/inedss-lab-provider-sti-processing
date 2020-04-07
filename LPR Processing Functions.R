

#=========================GENERAL USE FUNCTIONS=========================#


#General function to see if element is present before clicking (prevents nosuchelement exceptions)
ifVisiblethenClick <- function(element, selectorType = "css") {
  
  checkElement <- try(rD$findElement(using = selectorType, value = element))
  
  while(class(checkElement) == "try-error") {
    
    Sys.sleep(1)
    
    checkElement <- try(rD$findElement(using = selectorType, value = element))
    
  }
  
  rD$findElement(using = selectorType, value = element)$clickElement()
  
}



#General function to see if page has loaded before next action taken (prevents nosuchelement exceptions)
#NOT WORKING...NOT SURE WHY
# isPageLoaded <- function(element) {
#   
#   #Check to make sure page loaded before proceeding
#   pageCheck <- try(rD$findElement(using = "css", value = element))
#   
#   #If not loaded, loop and check until it is
#   while(class(pageCheck) == "try-error") {
#     
#     Sys.sleep(1)
#     
#     pageCheck <- try(rD$findElement(using = "css", value = element))
#     
#   }
# 
#   
# }



#General function to accept an alert with wait time; default is 3 seconds
acceptAlertwithWait <- function(wait = 3) {
  
  Sys.sleep(wait)
  rD$acceptAlert()
  Sys.sleep(wait)
  
}




#=========================FUNCTIONS FOR MAIN LPR PAGE=========================#

selectDisease <- function(disease) {
  
  Sys.sleep(2)
  
  #Click on link to open filter selection
  rD$findElement(using = "css", value = ".filterActive")$clickElement()
  
  #Switch to the filter pop-up window
  windows_filter <- rD$getWindowHandles()   
  rD$switchToWindow(windows_filter[[4]])
  
  Sys.sleep(2)
  
  #Select 'Not Reviewed'
  processStatusOptions <- rD$findElement(using = "css", value = "#casestatusid")$findChildElements(using = "css", value = "option" ) %>%
    map_chr(., function(x) x$getElementText()[[1]])
  
  process_n_child <- which(grepl("Not Reviewed", processStatusOptions))
  
  rD$findElement(using = "css", value = paste0("#casestatusid > option:nth-child(", process_n_child, ")"))$clickElement()

  
  #Select disease (using function argument)
  diseaseOptions <- rD$findElement(using = "css", value = "#diseaseid")$findChildElements(using = "css", value = "option" ) %>%
    map_chr(., function(x) x$getElementText()[[1]])
  
  disease_n_child <- which(grepl(disease, diseaseOptions))
  
  rD$findElement(using = "css", value = paste0("#diseaseid > option:nth-child(", disease_n_child, ")"))$clickElement()
  
  #Apply filter
  rD$findElement(using = "css", value = "input[name = \"filter\"]")$clickElement()
  
  Sys.sleep(2)
  
  #Switch back to LPR window
  rD$switchToWindow(windows_filter[[3]])
  
  Sys.sleep(2)
  
}



#=========================FUNCTIONS FOR CASE REPORT PAGE=========================#

#Click finding matches button and determine if any potential matches returned 
#NOT WORKING, MIGHT BE BECAUSE OF IS PAGE LOADED FUNCTION, HAVEN'T TRIED WITHOUT
# findMatches <- function() {
#   
#   #Find matches button present; click button
#   rD$findElement(using = "css", value = "input[name = \"findmatchtop\"]")$clickElement()
#   
#   #Check to make sure Patient Match page loaded
#   isPageLoaded(".pageDesc")
#   
#   #Checking to see if any matches found
#   highestProb <- try(rD$findElement(using = "css", value = 'fieldset.fieldsetNameBlock:nth-child(4) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(1) > td:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(1)'))
# 
#   return(highestProb)
#   
# }





#=========================FUNCTIONS FOR PATIENT MATCH PAGE=========================#

#Determine whether a match was found using values from the probability table
determinePatientMatch <- function(threshold_low) {
  
  #Check if probability table exists
  highestProb <- try(rD$findElement(using = "css", value = 'fieldset.fieldsetNameBlock:nth-child(4) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(1) > td:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(1)'))
  
  #If not, return "no matches found"
  if (class(highestProb) == "try-error") {
    
    return(list(patientMatchFound = "no match", row = NA))  #no potential matches returned
    
  } 
  
  #If table does exist, extract highest value   
  highestProbVal <- as.numeric(str_remove(highestProb$getElementText()[[1]], "%"))
    
  #Execute steps depending on highest value
  #highest probability match less than established threshold for no match
  if (highestProbVal < threshold_low) {
    
    return(list(patientMatchFound = "no match", row = NA))  
    
  } 
  
  #100% match returned
  if (highestProbVal == 100) {
    
    return(list(patientMatchFound = "match", row = 2))    
    
  }
  
  #81%-99% match returned
  #Extract case name
  case_name <- rD$findElement(using = "css", value = "fieldset.fieldsetNameBlock:nth-child(2) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(1)")$getElementText()[[1]]
  #Remove middle inital if present
  case_name <- strsplit(case_name, " ")[[1]] %>%
    .[nchar(.) > 1] %>%
    paste(., collapse = " ")
  #Extract case DOB
  case_DOB <- rD$findElement(using = "css", value = "fieldset.fieldsetNameBlock:nth-child(2) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(3)")$getElementText()[[1]]
  
  patientMatchTable <- rD$findElement(using = "css", value = "fieldset.fieldsetNameBlock:nth-child(4) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(1) > td:nth-child(1) > table:nth-child(1)")  
  patientRows <- length(patientMatchTable$findChildElements(using = "css", "tbody > tr"))
  
  #Compare case name and DOB to potential matches
  for (i in 2:ifelse(patientRows == 3,3,4)) {
      
    match_name <- rD$findElement(using = "css", value = paste0("fieldset.fieldsetNameBlock:nth-child(4) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(1) > td:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(",i,") > td:nth-child(2) > a:nth-child(1)"))$getElementText()[[1]]
    match_name <- strsplit(match_name, " ")[[1]] %>%
      .[nchar(.) > 1] %>%
      paste(., collapse = " ")
    
    match_DOB <- rD$findElement(using = "css", value = paste0("fieldset.fieldsetNameBlock:nth-child(4) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(1) > td:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(",i,") > td:nth-child(3)"))$getElementText()[[1]]
      
    name_match <- any(grepl(case_name, match_name), grepl(match_name, case_name))
    dob_match <- match_DOB == case_DOB
      
    if (all(name_match,dob_match)) {
        
      return(list(patientMatchFound = "match", row = i)) 
        
    } 
    
  }    
      
  return(list(patientMatchFound = "unclear", row = NA)) 
        

}
  


#Add a new person on the Matching Patients page
addNewPersonandCase <- function() {
  
  #Click add new person button
  rD$findElement(using = "css", value = "input[name = \"add\"]")$clickElement()
  
  #Accept alert
  acceptAlertwithWait()
  
  #Close record
  ifVisiblethenClick("input[name = \"close\"]")
  
}



#Cancel out of the Matching Patients page
exitMatchingPatients <- function() {
  
  #Cancel the matching page
  rD$findElement(using = "css", value = "input[value = \"Cancel\"]")$clickElement()
  
  #Close out of case report
  ifVisiblethenClick("#closetop")
  
}




#=========================FUNCTIONS FOR CASE MATCH PAGE=========================#

#Determine if there is a match on the page
determineCaseMatch <- function() {
  
  #Getting number of entries in Case Match Table
  diseaseEntryNum <- length(diseaseMatchTable$findChildElements(using = "css", "tbody > tr"))
  
  if (diseaseEntryNum == 1) { #no entries in table (means repository case is for different disease)
    
    return(list(caseMatchFound = "no match", row = NA, repositoryMatch = "older"))  
    
  } 
  
  #Storing case report date
  currentOnset <- rD$findElement(using = "css", value = "fieldset.fieldsetNameBlock:nth-child(2) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(6)")$getElementText()[[1]] %>%
    lubridate::mdy()
  
  anySame <- vector("logical", diseaseEntryNum-1)
  anyNewer <- vector("logical", diseaseEntryNum-1)
  
  for (i in 2:diseaseEntryNum) {
    
    #Storing match date
    matchEvent <- rD$findElement(using = "css", value = paste0("fieldset.fieldsetNameBlock:nth-child(4) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(",i,") > td:nth-child(2)"))$getElementText()[[1]] %>%
      lubridate::mdy()
    
    #Vector comparing match date to current report
    anySame[i - 1] <- ifelse(abs(as.integer(difftime(currentOnset, matchEvent, units = c("days")))) > 30, FALSE, TRUE)
    anyNewer[i - 1] <- ifelse(as.integer(difftime(currentOnset, matchEvent, units = c("days"))) < 0, TRUE, FALSE)
    
  }
  
  #Returning no match if case(s) listed but event date is blank (e.g. for Not A Case)
  if (length(anySame) == 1 && is.na(anySame)) {
    
    return(list(caseMatchFound = "no match", row = NA, repositoryMatch = "older")) 
    
  }
  
  #Steps to execute if cases are present (with events dates) to determine a match
  if (any(anySame)) {
    
    #identifying row of match 
    row <- which(anySame) + 1
    
    #exiting out in rare cases when both repository cases are a match
    if (length(row) != 1) {
      
      return(list(caseMatchFound = "unclear match", row = NA, repositoryMatch = NA)) 
      
    }
    
    #storing disposition and jurisdiction
    jurisdictionMatchVal <- rD$findElement(using = "css", value = paste0("fieldset.fieldsetNameBlock:nth-child(4) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(", row, ") > td:nth-child(5)"))$getElementText()[[1]]
    dispoFirstVal <- rD$findElement(using = "css", value = paste0("fieldset.fieldsetNameBlock:nth-child(4) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(", row, ") > td:nth-child(4)"))$getElementText()[[1]]
    matchDate <- rD$findElement(using = "css", value = paste0("fieldset.fieldsetNameBlock:nth-child(4) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(", row, ") > td:nth-child(2)"))$getElementText()[[1]] %>%
      lubridate::mdy()
    
    
    return(list(caseMatchFound = "match", 
                row = row,
                jurisdictionMatchVal = jurisdictionMatchVal,
                dispoFirstVal = dispoFirstVal,
                repositoryMatch = ifelse((matchDate - currentOnset) >= 0, "newer", "older")))
    
  } else {  #no match on cases
    
    return(list(caseMatchFound = "no match", 
                repositoryMatch = ifelse(any(anyNewer), "newer","older"))) 
    
  }
  
  
}


#If no entries in Case Match table, merge person and add case
mergePersonandAddCase <- function() {
  
  #Click button to merge person 
  rD$findElement(using = "css", value = "input[name = \"addcase\"]")$clickElement()
  
  #Giving time for page to load  ####SUSCEPTIBLE TO SCRIPT FAILURE -- CLEAN IF PROBLEMS ARISE
  Sys.sleep(3)
  
  ###On Merge Comparison page###
  shortMerge()
  
  #Merge case again
  rD$findElement(using = "css", value = "input[name = \"merge\"]")$clickElement()
  
  #Accept alert
  acceptAlertwithWait()
  
  #Close record
  ifVisiblethenClick("input[name = \"close\"]")
  
}



#Merge Case for active investigations in our jurisdiction
mergeCase <- function() {
  
  #Click into current infection
  rD$findElement(using = "css", value = paste0("fieldset.fieldsetNameBlock:nth-child(4) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(", matchInfo$row, ") > td:nth-child(1) > a:nth-child(1)"))$clickElement()
  
  ###On Merge Comparison page###
  longMerge()
  
  #Click button to merge case
  rD$findElement(using = "css", value ="input[name = \"merge\"]")$clickElement()
  
  #Accept alert
  acceptAlertwithWait()
  
  #Close record
  ifVisiblethenClick("input[name = \"close\"]")
  
}




#Cancel out of page Case Match page (for 2+ entries)
exitCaseMatching <- function() {
  
  #Cancel the Case Match page
  rD$findElement(using = "css", value = "input[value = \"Cancel\"]")$clickElement() 
  
  #Cancel the Patient Match page
  ifVisiblethenClick("input[value = \"Cancel\"]")
  
  #Close out of the record
  ifVisiblethenClick("#closetop")
  
}



#Transfer case for OOJ/same infections
transferSameInfection <- function() {
  
  #Click into current infection
  rD$findElement(using = "css", value = paste0("fieldset.fieldsetNameBlock:nth-child(4) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(", matchInfo$row, ") > td:nth-child(1) > a:nth-child(1)"))$clickElement()
  
  #Transfer case
  ifVisiblethenClick("input[value = \"Transfer\"]")
  
  #Accept alert
  acceptAlertwithWait()
  
  #Close record
  ifVisiblethenClick("input[name = \"close\"]")
  
  
}


#Disregard case for Cook/completed/same infections
disregardCompletedSameInfection <- function() {
  
  #Click into current infection
  rD$findElement(using = "css", value = paste0("fieldset.fieldsetNameBlock:nth-child(4) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(", matchInfo$row, ") > td:nth-child(1) > a:nth-child(1)"))$clickElement()
  
  #Disregard case
  ifVisiblethenClick("input[name = \"disregard\"]")
  
  #Accept alert
  acceptAlertwithWait()
  
  #Close record
  ifVisiblethenClick("input[name = \"close\"]")
  
}




#=========================FUNCTIONS FOR MERGE COMPARISON PAGE=========================#

#General function to use in long merge and short merge functions - if respository empty and match is not, click match
ifEmptythenClick <- function(section, merge_row) {
  
  matchEmpty <- grepl("Unknown|^ $", rD$findElement(using = "css", value = paste0(section, " > tbody:nth-child(1) > tr:nth-child(", merge_row, ") > td:nth-child(3)"))$getElementText()[[1]])
  repositoryEmpty <- grepl("Unknown|^ $", rD$findElement(using = "css", value = paste0(section, " > tbody:nth-child(1) > tr:nth-child(", merge_row, ") > td:nth-child(4)"))$getElementText()[[1]])
  
  if (matchEmpty == FALSE & repositoryEmpty == TRUE) {
    
    rD$findElement(using = "css", value = paste0(section, " > tbody:nth-child(1) > tr:nth-child(", merge_row, ") > td:nth-child(1) > input:nth-child(1)"))$clickElement()
    
  }
  
  if (matchEmpty == FALSE & repositoryEmpty == FALSE & matchInfo$repositoryMatch == "older") {
    
    rD$findElement(using = "css", value = paste0(section, " > tbody:nth-child(1) > tr:nth-child(", merge_row, ") > td:nth-child(1) > input:nth-child(1)"))$clickElement()
    
  }
  
}


#Processing demographic merge section
shortMerge <- function() {
  
  #Store CSS for section (more useful for long merge page)
  constructTableCSS <- function(n) {paste0("#container > div:nth-child(4) > form:nth-child(4) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(",n,") > td:nth-child(1) > table:nth-child(1)")}
  demographicCSS <- constructTableCSS(4)
  
  #Processing sex, gender, race, ethnicity
  for (i in c(3,4,7,8)) {
    
    ifEmptythenClick(demographicCSS, i)
    
  }
  
  #Processing phones
  repositoryHome <- rD$findElement("css", paste0(demographicCSS, " > tbody:nth-child(1) > tr:nth-child(9) > td:nth-child(4)"))$getElementText()[[1]]
  repositoryCell <- rD$findElement("css", paste0(demographicCSS, " > tbody:nth-child(1) > tr:nth-child(10) > td:nth-child(4)"))$getElementText()[[1]]
  matchHome <- rD$findElement("css", paste0(demographicCSS, " > tbody:nth-child(1) > tr:nth-child(9) > td:nth-child(3)"))$getElementText()[[1]]
  matchCell <- rD$findElement("css", paste0(demographicCSS, " > tbody:nth-child(1) > tr:nth-child(10) > td:nth-child(3)"))$getElementText()[[1]]
  
  if (matchHome != " " & matchHome != repositoryCell & matchInfo$repositoryMatch == "older") {
    
    rD$findElement(using = "css", value = paste0(demographicCSS," > tbody:nth-child(1) > tr:nth-child(9) > td:nth-child(1) > input:nth-child(1)"))$clickElement()
    
  } else if (matchHome != " " & matchHome != repositoryCell & repositoryHome == " " ) {
    
    rD$findElement(using = "css", value = paste0(demographicCSS," > tbody:nth-child(1) > tr:nth-child(9) > td:nth-child(1) > input:nth-child(1)"))$clickElement()
    
  }
  
  if (matchCell != " " & matchCell != repositoryHome & matchInfo$repositoryMatch == "older") {
    
    rD$findElement(using = "css", value = paste0(demographicCSS," > tbody:nth-child(1) > tr:nth-child(10) > td:nth-child(1) > input:nth-child(1)"))$clickElement()
    
  } else if (matchCell != " " & matchCell != repositoryHome & repositoryCell == " " ) {
    
    rD$findElement(using = "css", value = paste0(demographicCSS," > tbody:nth-child(1) > tr:nth-child(10) > td:nth-child(1) > input:nth-child(1)"))$clickElement()
    
  }
  
  #Processing address
  if (matchInfo$repositoryMatch == "older") { #means repository case is older, current report is newer info

        rD$findElement(using = "css", value = paste0(demographicCSS, " > tbody:nth-child(1) > tr:nth-child(11) > td:nth-child(1) > input:nth-child(1)"))$clickElement()
    
  } 
  
}



longMerge <- function() {
  
  #Demographic Processing
  shortMerge()
  
  #Set up for other tables
  constructTableCSS <- function(n) {paste0("#container > div:nth-child(4) > form:nth-child(4) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(",n,") > td:nth-child(1) > table:nth-child(1)")}
  diagnosisCSS <- constructTableCSS(7)
  labCSS <- constructTableCSS(10)
  eptCSS <- constructTableCSS(13)
  addCSS <- constructTableCSS(16)
  
  #Diagnosis Processing -- for all except Infection Site, Treatment, and Comment, click if repository empty
  for (i in c(1,2,4:8,15:21)) {
    
    ifEmptythenClick(diagnosisCSS, i)
    
  }
  
  #Diagnosis Site
  repositorySiteEmpty <- grepl("Unknown|^ $", rD$findElement(using = "css", value = paste0(diagnosisCSS, " > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(4)"))$getElementText()[[1]])
  matchSiteEmpty <- grepl("Unknown|^ $", rD$findElement(using = "css", value = paste0(diagnosisCSS, " > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(3)"))$getElementText()[[1]])
  
  if (repositorySiteEmpty == FALSE & matchSiteEmpty == FALSE & caseSource == "P") {
    
    rD$findElement("css", paste0(diagnosisCSS, " > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(1) > input:nth-child(1)"))$clickElement()
    
  } else {
    
    ifEmptythenClick(diagnosisCSS, 3)
    
  }
  
  #Diagnosis Treatment and Start Date
  for (i in c(9, 11, 13)) {
    
    repositoryRxNotUseful <- rD$findElement("css", paste0(diagnosisCSS, " > tbody:nth-child(1) > tr:nth-child(", i, ") > td:nth-child(4)"))$getElementText()[[1]] %>%
      grepl("No Treatment|^ $", .)
    matchRxEmpty <- rD$findElement("css", paste0(diagnosisCSS, " > tbody:nth-child(1) > tr:nth-child(", i, ") > td:nth-child(3)"))$getElementText()[[1]] %>%
      grepl("Unknown|^ $", .)
    
    if (matchRxEmpty == FALSE & repositoryRxNotUseful == TRUE) {
      
      rD$findElement("css", paste0(diagnosisCSS, " > tbody:nth-child(1) > tr:nth-child(", i, ") > td:nth-child(1) > input:nth-child(1)"))$clickElement()
      rD$findElement("css", paste0(diagnosisCSS, " > tbody:nth-child(1) > tr:nth-child(", i + 1, ") > td:nth-child(1) > input:nth-child(1)"))$clickElement()
      
    } else if (matchRxEmpty == FALSE & repositoryRxNotUseful == FALSE & matchInfo$repositoryMatch == "older") {
      
      rD$findElement("css", paste0(diagnosisCSS, " > tbody:nth-child(1) > tr:nth-child(", i, ") > td:nth-child(1) > input:nth-child(1)"))$clickElement()
      rD$findElement("css", paste0(diagnosisCSS, " > tbody:nth-child(1) > tr:nth-child(", i + 1, ") > td:nth-child(1) > input:nth-child(1)"))$clickElement()
      
    }
    
    
  }

  #Diagnosis Comment -- accept Match if not empty and repository only contains test ordering info
  repositoryCommentNotUseful <- rD$findElement("css", paste0(diagnosisCSS, " > tbody:nth-child(1) > tr:nth-child(22) > td:nth-child(4)"))$getElementText()[[1]] %>%
    grepl("Test Ordering|^ $", .)
  
  if (repositoryCommentNotUseful == TRUE) {
    
    rD$findElement("css", paste0(diagnosisCSS, " > tbody:nth-child(1) > tr:nth-child(22) > td:nth-child(1) > input:nth-child(1)"))$clickElement()
    
  }
  
  #Lab Processing -- process first two, skip specimens (i.e. accept LPR default checking) 
  ifEmptythenClick(labCSS, 1)
  ifEmptythenClick(labCSS, 2)
  
  
  #EPT Processing -- for all rows if repository is NULL and match is not, click to bring over
  for (i in 1:3) {
    
    ifEmptythenClick(eptCSS, i)
    
  }
  
  #Additional Details Processing -- for all rows if repository is NULL and match is not, click to bring over
  for (i in 1:19){
    
    ifEmptythenClick(addCSS, i)
    
  }
}




#=========================RESOURCES=========================#
#https://github.com/slourens/ISDSWebinar/blob/master/RSeleniumExample.R
#https://cran.r-project.org/web/packages/RSelenium/RSelenium.pdf
#https://docs.ropensci.org/RSelenium/articles/basics.html#connecting-to-a-selenium-server
#https://ropensci.org/tutorials/rselenium_tutorial/

