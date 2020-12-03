

#=========================GENERAL USE FUNCTIONS=========================#

#General use functions now loaded from Github




#=========================FUNCTIONS FOR MAIN LPR PAGE=========================#

selectDisease <- function(disease) {
  
  Sys.sleep(5)
  
  #Click on link to open filter selection
  click(".filterActive")
  
  #Switch to the filter pop-up window
  windows_filter <- rD$getWindowHandles()   
  rD$switchToWindow(windows_filter[[4]])
  
  Sys.sleep(5)
  
  #Select 'Not Reviewed'
  process_n_child <- find_child_element("#casestatusid", "option", target = "Not Reviewed")
  click(paste0("#casestatusid > option:nth-child(", process_n_child, ")"))

  
  #Select disease (using function argument)
  disease_n_child <- find_child_element("#diseaseid", "option", target = disease)
  click(paste0("#diseaseid > option:nth-child(", disease_n_child, ")"))
  
  #Apply filter
  click(name.is("filter"))
  
  Sys.sleep(5)
  
  #Switch back to LPR window
  rD$switchToWindow(windows_filter[[3]])
  
  Sys.sleep(5)
  
}





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
  case_name <- get_text("fieldset.fieldsetNameBlock:nth-child(2) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(1)")
  #Remove middle inital if present
  case_name <- strsplit(case_name, " ")[[1]] %>%
    .[nchar(.) > 1] %>%
    paste(., collapse = " ")
  #Extract case DOB
  case_DOB <- get_text("fieldset.fieldsetNameBlock:nth-child(2) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(3)")
  
  patientMatchTable <- rD$findElement(using = "css", value = "fieldset.fieldsetNameBlock:nth-child(4) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(1) > td:nth-child(1) > table:nth-child(1)")  
  patientRows <- length(patientMatchTable$findChildElements(using = "css", "tbody > tr"))
  
  #Compare case name and DOB to potential matches
  for (i in 2:ifelse(patientRows == 3,3,4)) {
      
    match_name <- get_text(paste0("fieldset.fieldsetNameBlock:nth-child(4) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(1) > td:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(",i,") > td:nth-child(2) > a:nth-child(1)"))
    match_name <- strsplit(match_name, " ")[[1]] %>%
      .[nchar(.) > 1] %>%
      paste(., collapse = " ")
    
    match_DOB <- get_text(paste0("fieldset.fieldsetNameBlock:nth-child(4) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(1) > td:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(",i,") > td:nth-child(3)"))
      
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
  click(name.is("add"))
  
  #Accept alert
  acceptAlertwithWait()
  
  #Close record
  ifVisiblethenClick(name.is("close"))
  
}



#Cancel out of the Matching Patients page
exitMatchingPatients <- function() {
  
  #Cancel the matching page
  click(value.is("Cancel"))
  
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
  currentOnset <- get_text("fieldset.fieldsetNameBlock:nth-child(2) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(6)") %>%
    lubridate::mdy()
  
  anySame <- vector("logical", diseaseEntryNum-1)
  anyNewer <- vector("logical", diseaseEntryNum-1)
  
  for (i in 2:diseaseEntryNum) {
    
    #Storing match date
    matchEvent <- get_text(paste0("fieldset.fieldsetNameBlock:nth-child(4) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(",i,") > td:nth-child(2)")) %>%
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
    jurisdictionMatchVal <- get_text(paste0("fieldset.fieldsetNameBlock:nth-child(4) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(", row, ") > td:nth-child(5)"))
    dispoFirstVal <- get_text(paste0("fieldset.fieldsetNameBlock:nth-child(4) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(", row, ") > td:nth-child(4)"))
    matchDate <- get_text(paste0("fieldset.fieldsetNameBlock:nth-child(4) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(", row, ") > td:nth-child(2)")) %>%
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
  click(name.is('addcase'))
  
  #Giving time for page to load  ####SUSCEPTIBLE TO SCRIPT FAILURE -- CLEAN IF PROBLEMS ARISE
  Sys.sleep(3)
  
  ###On Merge Comparison page###
  shortMerge()
  
  #Merge case again
  click(name.is("merge"))
  
  #Accept alert
  acceptAlertwithWait()
  
  #Close record
  ifVisiblethenClick(name.is("close"))
  
}



#Merge Case for active investigations in our jurisdiction
mergeCase <- function() {
  
  #Click into current infection
  click(paste0("fieldset.fieldsetNameBlock:nth-child(4) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(", matchInfo$row, ") > td:nth-child(1) > a:nth-child(1)"))
  
  ###On Merge Comparison page###
  longMerge()
  
  #Click button to merge case
  click(name.is("merge"))
  
  #Accept alert
  acceptAlertwithWait()
  
  #Close record
  ifVisiblethenClick(name.is("close"))
  
}




#Cancel out of page Case Match page (for 2+ entries)
exitCaseMatching <- function() {
  
  #Cancel the Case Match page
  click(value.is("Cancel"))
  
  #Cancel the Patient Match page
  ifVisiblethenClick(value.is("Cancel"))
  
  #Close out of the record
  ifVisiblethenClick("#closetop")
  
}



#Transfer case for OOJ/same infections
transferSameInfection <- function() {
  
  #Click into current infection
  click(paste0("fieldset.fieldsetNameBlock:nth-child(4) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(", matchInfo$row, ") > td:nth-child(1) > a:nth-child(1)"))
  
  #Transfer case
  ifVisiblethenClick(value.is("Transfer"))
  
  #Accept alert
  acceptAlertwithWait()
  
  #Close record
  ifVisiblethenClick(name.is("close"))
  
  
}


#Disregard case for Cook/completed/same infections
disregardCompletedSameInfection <- function() {
  
  #Click into current infection
  click(paste0("fieldset.fieldsetNameBlock:nth-child(4) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(", matchInfo$row, ") > td:nth-child(1) > a:nth-child(1)"))
  
  #Disregard case
  ifVisiblethenClick(name.is("disregard"))
  
  #Accept alert
  acceptAlertwithWait()
  
  #Close record
  ifVisiblethenClick(name.is("close"))
  
}




#=========================FUNCTIONS FOR MERGE COMPARISON PAGE=========================#

#General function to use in long merge and short merge functions - if respository empty and match is not, click match
ifEmptythenClick <- function(section, merge_row) {
  
  matchEmpty <- grepl("Unknown|^ $", get_text(paste0(section, " > tbody:nth-child(1) > tr:nth-child(", merge_row, ") > td:nth-child(3)")))
  repositoryEmpty <- grepl("Unknown|^ $", get_text(paste0(section, " > tbody:nth-child(1) > tr:nth-child(", merge_row, ") > td:nth-child(4)")))
  
  if (matchEmpty == FALSE & repositoryEmpty == TRUE) {
    
    click(paste0(section, " > tbody:nth-child(1) > tr:nth-child(", merge_row, ") > td:nth-child(1) > input:nth-child(1)"))
    
  }
  
  if (matchEmpty == FALSE & repositoryEmpty == FALSE & matchInfo$repositoryMatch == "older") {
    
    click(paste0(section, " > tbody:nth-child(1) > tr:nth-child(", merge_row, ") > td:nth-child(1) > input:nth-child(1)"))
    
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
  repositoryHome <- get_text(paste0(demographicCSS, " > tbody:nth-child(1) > tr:nth-child(9) > td:nth-child(4)"))
  repositoryCell <- get_text(paste0(demographicCSS, " > tbody:nth-child(1) > tr:nth-child(10) > td:nth-child(4)"))
  matchHome <- get_text(paste0(demographicCSS, " > tbody:nth-child(1) > tr:nth-child(9) > td:nth-child(3)"))
  matchCell <- get_text(paste0(demographicCSS, " > tbody:nth-child(1) > tr:nth-child(10) > td:nth-child(3)"))
  
  if (matchHome != " " & matchHome != repositoryCell & matchInfo$repositoryMatch == "older") {
    
    click(paste0(demographicCSS," > tbody:nth-child(1) > tr:nth-child(9) > td:nth-child(1) > input:nth-child(1)"))
    
  } else if (matchHome != " " & matchHome != repositoryCell & repositoryHome == " " ) {
    
    click(paste0(demographicCSS," > tbody:nth-child(1) > tr:nth-child(9) > td:nth-child(1) > input:nth-child(1)"))
    
  }
  
  if (matchCell != " " & matchCell != repositoryHome & matchInfo$repositoryMatch == "older") {
    
    click(paste0(demographicCSS," > tbody:nth-child(1) > tr:nth-child(10) > td:nth-child(1) > input:nth-child(1)"))
    
  } else if (matchCell != " " & matchCell != repositoryHome & repositoryCell == " " ) {
    
    click(paste0(demographicCSS," > tbody:nth-child(1) > tr:nth-child(10) > td:nth-child(1) > input:nth-child(1)"))
    
  }
  
  #Processing address
  if (matchInfo$repositoryMatch == "older") { #means repository case is older, current report is newer info

        click(paste0(demographicCSS, " > tbody:nth-child(1) > tr:nth-child(11) > td:nth-child(1) > input:nth-child(1)"))
    
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
  repositorySiteEmpty <- grepl("Unknown|^ $", get_text(paste0(diagnosisCSS, " > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(4)")))
  matchSiteEmpty <- grepl("Unknown|^ $", get_text(paste0(diagnosisCSS, " > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(3)")))
  
  if (repositorySiteEmpty == FALSE & matchSiteEmpty == FALSE & caseSource == "P") {
    
    click(paste0(diagnosisCSS, " > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(1) > input:nth-child(1)"))
    
  } else {
    
    ifEmptythenClick(diagnosisCSS, 3)
    
  }
  
  #Diagnosis Treatment and Start Date
  for (i in c(9, 11, 13)) {
    
    repositoryRxNotUseful <- get_text(paste0(diagnosisCSS, " > tbody:nth-child(1) > tr:nth-child(", i, ") > td:nth-child(4)")) %>%
      grepl("No Treatment|^ $", .)
    matchRxEmpty <- get_text(paste0(diagnosisCSS, " > tbody:nth-child(1) > tr:nth-child(", i, ") > td:nth-child(3)")) %>%
      grepl("Unknown|^ $", .)
    
    if (matchRxEmpty == FALSE & repositoryRxNotUseful == TRUE) {
      
      click(paste0(diagnosisCSS, " > tbody:nth-child(1) > tr:nth-child(", i, ") > td:nth-child(1) > input:nth-child(1)"))
      click(paste0(diagnosisCSS, " > tbody:nth-child(1) > tr:nth-child(", i + 1, ") > td:nth-child(1) > input:nth-child(1)"))
      
    } else if (matchRxEmpty == FALSE & repositoryRxNotUseful == FALSE & matchInfo$repositoryMatch == "older") {
      
      click(paste0(diagnosisCSS, " > tbody:nth-child(1) > tr:nth-child(", i, ") > td:nth-child(1) > input:nth-child(1)"))
      click(paste0(diagnosisCSS, " > tbody:nth-child(1) > tr:nth-child(", i + 1, ") > td:nth-child(1) > input:nth-child(1)"))
      
    }
    
    
  }

  #Diagnosis Comment -- accept Match if not empty and repository only contains test ordering info
  repositoryCommentNotUseful <- get_text(paste0(diagnosisCSS, " > tbody:nth-child(1) > tr:nth-child(22) > td:nth-child(4)")) %>%
    grepl("Test Ordering|^ $", .)
  
  if (repositoryCommentNotUseful == TRUE) {
    
    click(paste0(diagnosisCSS, " > tbody:nth-child(1) > tr:nth-child(22) > td:nth-child(1) > input:nth-child(1)"))
    
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

