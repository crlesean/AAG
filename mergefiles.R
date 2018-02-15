#mergefiles, a function, requires the user to input the folder name (example: Director_20180214)
#containing the .csv files titled, "logon.csV" & "connection_fail.csv" when prompted.
#mergefiles will then merge the two files and output a .csv file titled, "useranalysis.csv".
#if there already exists a file titled, "useranalysis.csv" mergefiles will append new data rows
#to "useranalysis.csv".

#Prior to running the function 'mergefiles', EXTRACT ALL FILES in the zipped folder
#containing the .csv files titled, "logon.csV" & "connection_fail.csv".

#Before running AAG_Metrics ensure that .csv files produced by this program are closed.

#Packages to install before running mergefiles
install.packages("plyr")
install.packages("dplyr")
install.packages("splitstackshape")
install.packages("lubridate")
install.packages("ggplot2")
library(plyr)
library(dplyr)
library(splitstackshape)
library(lubridate)
library(ggplot2)

AAG_Metrics <- function(){

  #SECTION 1:
  #Manipulates the .csv files and creates weekly (Director_20182708_merged.csv) and an aggregate .csv file (AAG_Master_File.csv).

  #Changes working directory
  setwd("C:/Users/crlesean/Desktop/AAG/AAG_Metrics")

  #Prompts the user to input the folder name.
  folder <- readline("Please type the folder name that contains the two files that you would like to merge (ex.'Director_20180214').")

  #Takes user input and concatenates it to specify csv location.
  folder1 <- paste(folder,"\\logon.csv", sep = "")
  folder2 <- paste(folder,"\\connection_fail.csv", sep = "")

  #Reads the two .csv files and assigns to an object name.
  logon <-read.csv2(folder1,sep = ",")
  connection_fail <-read.csv2(folder2,sep = ",")

  #Renames column name to User.Name for both csv files
  names(logon)[names(logon)=="ï..User.Name"] <- "User.Name"
  names(connection_fail)[names(connection_fail)=="ï..Associated.User"] <- "User.Name"

  #Renames column name to Logon.Attempt.Time for both csv files
  names(logon)[names(logon)=="Logon.time"] <- "Logon.Attempt.Time"
  names(connection_fail)[names(connection_fail)=="Failure.Time"] <- "Logon.Attempt.Time"

  #Renames column name to Logon.Attempt.Outcome for connection_fail.csv file
  names(connection_fail)[names(connection_fail)=="Failure.Type"] <- "Logon.Attempt.Outcome"

  #Creates new columns in both csv files so that rbind will work properly.
  logon$Logon.Attempt.Outcome <- "Success"
  logon$Failure.Reason <- "n/a"

  connection_fail$Total..sec. <- "n/a"
  connection_fail$Brokering..sec. <- "n/a"
  connection_fail$VM.Start..sec. <- "n/a"
  connection_fail$HDX.Connection..sec. <- "n/a"
  connection_fail$Authentication..sec. <- "n/a"
  connection_fail$GPOs..sec. <- "n/a"
  connection_fail$Logon.scripts..sec. <- "n/a"
  connection_fail$Profile.load..sec. <- "n/a"
  connection_fail$Interactive.sessions..sec. <- "n/a"

  #Useranalysis is the resulting data frame after merging the two csv files
  useranalysis <- rbind(logon, connection_fail)

  #Splits the "Logon.Attempt.Time column to create an additional column titled "Weekday"
  weekday <- strptime(cSplit(useranalysis,"Logon.Attempt.Time"," ")$Logon.Attempt.Time_1, format = ("%m/%d/%Y"))
  weekday <- weekdays(weekday)
  useranalysis$Weekday <- weekday

  #Creates date column using Logon.Attempt.Time column
  useranalysis$Date <- as.Date(useranalysis$Logon.Attempt.Time, format = "%m/%d/%Y")

  #Creates Hour column using Logon.Attempt.Time column
  useranalysis$Hour <- as.POSIXlt(useranalysis$Logon.Attempt.Time, format = "%m/%d/%Y %H:%M:%S")$hour

  #Creates new csv file using initial user input and saves in folder of weekly merged files
  newfile <- paste(folder,"_merged.csv", sep = "")
  write.csv(useranalysis, file = paste("C:/Users/crlesean/Desktop/AAG/AAG_Metrics/weekly_merged/",newfile))

  #Reads all files located in the "weekly_merged" folder and merges them creating an output file titled "AAG_Master_File.csv"
  location <- "C:/Users/crlesean/Desktop/AAG/AAG_Metrics/weekly_merged"
  filenames <- list.files(location)
  setwd(location)
  all_files <- Reduce(rbind, lapply(filenames, read.csv))
  all_files <- all_files[-1]
  all_files <- unique(all_files)
  write.csv(all_files, file = "C:/Users/crlesean/Desktop/AAG/AAG_Metrics/AAG_Master_File.csv")
  setwd("C:/Users/crlesean/Desktop/AAG/AAG_Metrics")


  #THIS ENDS SECTION 1#
  #Potential improvement on section 1
  #1)




  ###---------------------------------------------------------------------------------------------------------------------------###




  #SECTION 2:
  #Focuses on quantitative analysis of weekly (Director_20182708_merged.csv) and aggregate (AAG_Master_File.csv) .csv files.
  #In this section will we calculate metrics and create visuals for weekly performance and aggregate performance for the sum
  #of all weeks where website performance data is collected.

  #Displays login success for entire data set


}


install.packages("stringr")
library(stringr)

AAG_Visual <- function(){



  #SECTION 1:
  #This section will provide visualization to the data that was created using the AAG_Metrics function.
  #This function will all the user to analyze the entire data set or enter a specific date range to analyze

  #Changes working directory
  setwd("C:/Users/crlesean/Desktop/AAG/AAG_Metrics")

  Master <-read.csv2("C:/Users/crlesean/Desktop/AAG/AAG_Metrics/AAG_Master_File.csv",sep = ",")

  ggplot(Master, aes(x = Logon.Attempt.Outcome)) +
    theme_bw() +
    geom_bar() +
    labs(y = "Logon Attempts",
         title = "Logon Outcome Successes and Failures by Failure.Type")

  ggplot(subset(Master, Failure.Reason != "n/a"), aes(x = Failure.Reason)) +
    theme_bw() +
    geom_bar() +
    labs(y = "Logon Failures",
         title = "Logon Failures by Failure.Reason")

  ggplot(subset(Master, Failure.Reason != "n/a"), aes(x = Delivery.Group, fill = Failure.Reason)) +
    theme_bw() +
    geom_bar() +
    labs(y = "Logon Failures",
         title = "Logon Failures by Failure.Reason and Delivery.Group")

  ggplot(subset(Master, Failure.Reason != "n/a"), aes(x = User.Name, fill = Failure.Reason)) +
    theme_bw() +
    geom_bar() +
    labs(y = "Logon Failures",
         title = "Logon Failures by Failure.Reason and User.Name")

  ggplot(subset(Master, Failure.Reason != "n/a"), aes(x = Weekday, fill = Failure.Reason)) +
    theme_bw() +
    geom_bar() +
    labs(y = "Logon Failures",
         title = "Logon Failures by Failure.Reason and Weekday")


}







AAG_Master <- function(){

  #Reads all files located in the "weekly_merged" folder and merges them creating an output file titled "AAG_Master_File.csv"
  location <- "C:/Users/crlesean/Desktop/AAG/AAG_Metrics/weekly_merged"
  filenames <- list.files(location)
  #filenames <- substr(filenames, 2, 29)
  setwd(location)
  all_files <- Reduce(rbind, lapply(filenames, read.csv))
  write.csv(all_files, file = "C:/Users/crlesean/Desktop/AAG/AAG_Metrics/AAG_Master_File.csv")
  setwd("C:/Users/crlesean/Desktop/AAG/AAG_Metrics")
}

