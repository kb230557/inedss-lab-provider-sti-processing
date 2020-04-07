library(RSelenium)
library(magrittr)
library(purrr)
library(stringr)
library(keyring)
library(methods)

#key_set("idph_portal")  #sets Kelley's IDPH password
#key_set("tobi_idph_portal")  #sets Tobi's IDPH password

#Note: IE is preferred browser for INEDSS but requires special drivers for Selenium. 
#Chrome has issues with switching tabs so proceeding with Firefox.

setwd("S:/Enhanced Surveillance/General CD/Automated STI LPR Processing")

source('LPR Processing Functions.R')

#sets probability threshold for when to add a new person (e.g. "no match") on Person Match page
match_prob_low <- 80

#stop session -- ALWAYS RUN THIS CODE AT THE END TO STOP THE SERVER, ALSO APPEARS AT END OF SCRIPT
#remDr$server$stop() 


#=================LOGGING INTO THE PORTAL AND NAVIGATING TO LAB PROVIDER=================#

#Open selenium session
remDr <- rsDriver(browser = "firefox")

#remDr$server$error() #get errors if thrown

#Extract the client for navigation
rD <- remDr[['client']]

#Navigating to log-in page
rD$navigate("https://dph.partner.illinois.gov/my.policy")

#Check for cookies error
login_error <- try(rD$findElement("css", "#newSessionDIV > a:nth-child(1)"))
if (class(login_error) != "try-error") {login_error$clickElement()}

#Clicking link to access log-in screen
rD$findElement("css", ".interaction_table_text_cell > a:nth-child(1)")$clickElement()

#Pausing execution to give time to log in and load page
Sys.sleep(5)

#Enter credentials and log in
rD$findElement(using = "css", value = "#input_1")$sendKeysToElement(list("kelley.bemis", key = "tab", key_get("idph_portal")))
rD$findElement("css", "input[value = \"Logon\"]")$clickElement()

#Pausing execution to give time to log in and load page
Sys.sleep(10)

#Mousing over applications button
rD$findElement(using = "xpath", value = '//*[@id="zz6_RootAspMenu"]/li/ul/li[1]/a/span/span')$mouseMoveToLocation()

#Finding production apps button
rD$findElement(using = "xpath", value = '//*[@id="zz6_RootAspMenu"]/li/ul/li[1]/a')$clickElement()

#Finding INEDSS buttons  -- XPATH WILL BE DIFFERENT DEPENDING ON APPS USER HAS
ifVisiblethenClick('//*[@id="column"]/table[5]/tbody/tr/td[2]/a', selectorType = "xpath") #Kelley
#ifVisiblethenClick('//*[@id="column"]/table[3]/tbody/tr/td[2]/a', selectorType = "xpath") #Tobi

#Pausing execution to give time to load page
Sys.sleep(10)

#Switching focus to INEDSS tab   
windows <- rD$getWindowHandles()   
rD$switchToWindow(windows[[2]])

#Clicking login button
rD$findElement(using = "css", value = "input[name = \"login\"]")$clickElement()

#Clicking into lab provider
ifVisiblethenClick("a[target = \"Lab Window\"]")

#Pausing execution to give time to load page
Sys.sleep(10)

#Switching focus to LPR tab 
windows_lpr <- rD$getWindowHandles()
rD$switchToWindow(windows_lpr[[3]])

#If INEDSS times out
# rD$findElement(using = "css", value = "#container > div:nth-child(4) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(1) > td:nth-child(1) > form:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(4) > td:nth-child(1) > a:nth-child(1)")$clickElement()
# rD$findElement(using = "css", value = "input[name = \"reuserid\"]")$sendKeysToElement(list("kelley.bemis", key = "tab", key_get("idph_portal")))
# rD$findElement(using = "css", value = "input[name = \"loginButton\"]")$clickElement()


#=================STARTING LOOP FOR GONORRHEA=================#

#Select which disease to work on (spelling and capitalization must be correct)
selectDisease("Gonorrhea")

#Give page time to load
Sys.sleep(5) 


repeat {

#Checking to make sure case available to process, if not stop script
nextCase <- try(rD$findElement(using = "css", value = ".indessTable > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(1) > a:nth-child(1)"))
  
if (class(nextCase) != "try-error") {
  
  #Storing report source for use on long merge page if needed
  caseSource <- rD$findElement(using = "css", value = ".indessTable > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(6)")$getElementText()[[1]]
  
  nextCase$clickElement()
  
} else {
  
  #stop("All cases processed")
  break
  
}


#Check to make sure Case Report page loaded       ##NOTE: isPageLoaded function is in source script but not working correctly and not sure why, troubleshoot later###
#isPageLoaded("#closetop")    
pageCheck <- try(rD$findElement(using = "css", value = "#closetop"))
while(class(pageCheck) == "try-error") {
  
  Sys.sleep(1)
  
  pageCheck <- try(rD$findElement(using = "css", value = "#closetop"))
  
}

#Clicking either find matches or close if remerge button is present
remergeButton <- try(rD$findElement(using = "css", value = "input[value = \"Remerge\"]"))

if (class(remergeButton) != "try-error") {
  
  #Remerge button present; click cancel
  rD$findElement(using = "css", value = "#closetop")$clickElement()
  
} else {
  
  #Find matches button present; click button
  rD$findElement(using = "css", value = "input[name = \"findmatchtop\"]")$clickElement()
  
  #Check to make sure Patient Match page loaded
  #isPageLoaded(".pageDesc")
  pageCheck <- try(rD$findElement(using = "css", value = ".pageDesc"))
  while(class(pageCheck) == "try-error") {
    
    Sys.sleep(1)
    
    pageCheck <- try(rD$findElement(using = "css", value = ".pageDesc"))
    
  }

  #Determine if patient match has been found on the page
  matchStatus <- determinePatientMatch(match_prob_low)

  #Execute steps based on whether patient found
  if (matchStatus$patientMatchFound == "no match") {
  
    #If no patient matches, add new person and case
    addNewPersonandCase()
  
  } else if (matchStatus$patientMatchFound == "unclear") {
    
    #If patient match is unclear, cancel the matching page
    exitMatchingPatients()
    
  } else { #should be patientMatchFound == "match"
  
      #clicking match entry
      rD$findElement(using = "css", value = paste0("fieldset.fieldsetNameBlock:nth-child(4) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(1) > td:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(",matchStatus$row,") > td:nth-child(2) > a:nth-child(1)"))$clickElement()

      #Checking to see if Case Match page loaded
      diseaseMatchTable <- try(rD$findElement(using = "css", value = "fieldset.fieldsetNameBlock:nth-child(4) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(1) > table:nth-child(1)"))
      while(class(diseaseMatchTable) == "try-error") {
        
        Sys.sleep(1)
        
        diseaseMatchTable <- try(rD$findElement(using = "css", value = "fieldset.fieldsetNameBlock:nth-child(4) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(1) > table:nth-child(1)"))
        
      }
      
      
      #Determining if a case match exists and storing relevant info
      matchInfo <- determineCaseMatch()
      
      if (matchInfo$caseMatchFound == "no match") {
        
        mergePersonandAddCase()
        
      } else if (matchInfo$caseMatchFound == "unclear match") {
        
        exitCaseMatching()
        
      } else {  #should only be if case match found
        
        if (grepl("Cook", matchInfo$jurisdictionMatchVal)){
            
            if (matchInfo$dispoFirstVal == "Completed") {  #same case, investigation closed
              
              disregardCompletedSameInfection()
              
            } else {  #same case, investigation open 
              
              mergeCase()
              
            }
      
        } else {  #OOJ
          
          transferSameInfection()
          
        }
        
      }
 
  
  
} #ifmatches found at all closure


} #remerge / find matches if

#Sys.sleep(18)
  
#wait for main page to load again after processing
mainPage <- try(rD$findElement(using = "css", value = ".filterActive"))
mainPageWait <- 0

while(class(mainPage) == "try-error" & mainPageWait <120) {
  
  Sys.sleep(1)
  
  mainPageWait <- mainPageWait + 1
  
  mainPage <- try(rD$findElement(using = "css", value = ".filterActive"))
  
}

}#end of overall loop to go through cases



#=================STARTING LOOP FOR CHLAMYDIA=================#
#Select which disease to work on (spelling and capitalization must be correct)
selectDisease("Chlamydia")

#Give page time to load
Sys.sleep(5) 

#Exact repeat of loop above
repeat {
  
  #Checking to make sure case available to process, if not stop script
  nextCase <- try(rD$findElement(using = "css", value = ".indessTable > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(1) > a:nth-child(1)"))
  
  if (class(nextCase) != "try-error") {
    
    #Storing report source for use on long merge page if needed
    caseSource <- rD$findElement(using = "css", value = ".indessTable > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(6)")$getElementText()[[1]]
    
    nextCase$clickElement()
    
  } else {
    
    stop("All cases processed")
    #break
    
  }
  
  
  #Check to make sure Case Report page loaded       ##NOTE: isPageLoaded function is in source script but not working correctly and not sure why, troubleshoot later###
  #isPageLoaded("#closetop")    
  pageCheck <- try(rD$findElement(using = "css", value = "#closetop"))
  while(class(pageCheck) == "try-error") {
    
    Sys.sleep(1)
    
    pageCheck <- try(rD$findElement(using = "css", value = "#closetop"))
    
  }
  
  #Clicking either find matches or close if remerge button is present
  remergeButton <- try(rD$findElement(using = "css", value = "input[value = \"Remerge\"]"))
  
  if (class(remergeButton) != "try-error") {
    
    #Remerge button present; click cancel
    rD$findElement(using = "css", value = "#closetop")$clickElement()
    
  } else {
    
    #Find matches button present; click button
    rD$findElement(using = "css", value = "input[name = \"findmatchtop\"]")$clickElement()
    
    #Check to make sure Patient Match page loaded
    #isPageLoaded(".pageDesc")
    pageCheck <- try(rD$findElement(using = "css", value = ".pageDesc"))
    while(class(pageCheck) == "try-error") {
      
      Sys.sleep(1)
      
      pageCheck <- try(rD$findElement(using = "css", value = ".pageDesc"))
      
    }
    
    #Determine if patient match has been found on the page
    matchStatus <- determinePatientMatch(match_prob_low)
    
    #Execute steps based on whether patient found
    if (matchStatus$patientMatchFound == "no match") {
      
      #If no patient matches, add new person and case
      addNewPersonandCase()
      
    } else if (matchStatus$patientMatchFound == "unclear") {
      
      #If patient match is unclear, cancel the matching page
      exitMatchingPatients()
      
    } else { #should be patientMatchFound == "match"
      
      #clicking match entry
      rD$findElement(using = "css", value = paste0("fieldset.fieldsetNameBlock:nth-child(4) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(1) > td:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(",matchStatus$row,") > td:nth-child(2) > a:nth-child(1)"))$clickElement()
      
      #Checking to see if Case Match page loaded
      diseaseMatchTable <- try(rD$findElement(using = "css", value = "fieldset.fieldsetNameBlock:nth-child(4) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(1) > table:nth-child(1)"))
      while(class(diseaseMatchTable) == "try-error") {
        
        Sys.sleep(1)
        
        diseaseMatchTable <- try(rD$findElement(using = "css", value = "fieldset.fieldsetNameBlock:nth-child(4) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(1) > table:nth-child(1)"))
        
      }
      
      #Determining if a case match exists and storing relevant info
      matchInfo <- determineCaseMatch()
      
      if (matchInfo$caseMatchFound == "no match") {
        
        mergePersonandAddCase()
        
      } else if (matchInfo$caseMatchFound == "unclear match") {
        
        exitCaseMatching()
        
      } else {  #should only be if case match found
        
        if (grepl("Cook", matchInfo$jurisdictionMatchVal)){
          
          if (matchInfo$dispoFirstVal == "Completed") {  #same case, investigation closed
            
            disregardCompletedSameInfection()
            
          } else {  #same case, investigation open 
            
            mergeCase()
            
          }
          
        } else {  #OOJ
          
          transferSameInfection()
          
        }
        
      }
      
      
      
    } #ifmatches found at all closure
    
    
  } #remerge / find matches if
  
  #Sys.sleep(18)
  
  #wait for main page to load again after processing
  mainPage <- try(rD$findElement(using = "css", value = ".filterActive"))
  mainPageWait <- 0
  
  while(class(mainPage) == "try-error" & mainPageWait <120) {
    
    Sys.sleep(1)
    
    mainPageWait <- mainPageWait + 1
    
    mainPage <- try(rD$findElement(using = "css", value = ".filterActive"))
    
  }
  
}#end of overall loop to go through cases


#stop session -- ALWAYS RUN THIS CODE AT THE END TO STOP THE SERVER, ALSO APPEARS AT BEGINNING OF SCRIPT
remDr$server$stop() #stop session


