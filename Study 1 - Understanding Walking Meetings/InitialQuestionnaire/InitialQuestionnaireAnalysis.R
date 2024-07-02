# # set the wd to the script location
# library(rstudioapi)
# current_path <- getActiveDocumentContext()$path 
# setwd(dirname(current_path))

#$Rev: 10193 $ complete.
data_turk <- read.csv("./MTurk 1 Results/survey_392544_R_data_file.csv", quote = "'\"", na.strings=c("", "\"\""), stringsAsFactors=FALSE)
data_normal <- read.csv("./Survey 1 Results/survey_615638_R_data_file.csv", quote = "'\"", na.strings=c("", "\"\""), stringsAsFactors=FALSE)

#######
## Label data ##
#######

for (i in c(0,1)){
  if (i==0){
    data <- data_turk
  }  else{
    data_turk <- data
    data <- data_normal
  }
  # LimeSurvey Field type: F
  data[, 1] <- as.numeric(data[, 1])
  attributes(data)$variable.labels[1] <- "id"
  names(data)[1] <- "id"
  
  # LimeSurvey Field type: DATETIME23.2
  data[, 2] <- as.character(data[, 2])
  attributes(data)$variable.labels[2] <- "submitdate"
  names(data)[2] <- "submitdate"
  
  #Field hidden
  
  # LimeSurvey Field type: A
  data[, 3] <- as.character(data[, 3])
  attributes(data)$variable.labels[3] <- "startlanguage"
  names(data)[3] <- "startlanguage"
  
  # LimeSurvey Field type: F
  data[, 4] <- as.numeric(data[, 4])
  attributes(data)$variable.labels[4] <- "Age"
  names(data)[4] <- "D001"
  
  # LimeSurvey Field type: A
  data[, 5] <- as.character(data[, 5])
  attributes(data)$variable.labels[5] <- "Gender"
  data[, 5] <- factor(data[, 5], levels=c("A1","A2","A3","A4","A5"),labels=c("Male", "Female", "Non-binary", "Write your gender - an input box will appear", "Prefer not to say"))
  names(data)[5] <- "D002"
  
  # LimeSurvey Field type: F
  data[, 6] <- as.numeric(data[, 6])
  attributes(data)$variable.labels[6] <- "Write your gender here"
  names(data)[6] <- "D003"
  
  # LimeSurvey Field type: A
  data[, 7] <- as.character(data[, 7])
  attributes(data)$variable.labels[7] <- "Occupation"
  names(data)[7] <- "D004"
  
  # LimeSurvey Field type: A
  data[, 8] <- as.character(data[, 8])
  attributes(data)$variable.labels[8] <- "What is your current country of residence?"
  names(data)[8] <- "D005"
  
  # LimeSurvey Field type: A
  data[, 9] <- as.character(data[, 9])
  attributes(data)$variable.labels[9] <- "How often do you participate in meetings?"
  data[, 9] <- factor(data[, 9], levels=c("A1","A2","A3","A4","A5"),labels=c("Less than once per week", "Once per week", "Several times per week", "Every day", "Multiple times per day"))
  names(data)[9] <- "M001"
  
  # LimeSurvey Field type: A
  data[, 10] <- as.character(data[, 10])
  attributes(data)$variable.labels[10] <- "How often do you participate in meetings with two or fewer colleagues?"
  data[, 10] <- factor(data[, 10], levels=c("A1","A2","A3","A4","A5"),labels=c("Less than once per week", "Once per week", "Several times per week", "Every day", "Multiple times per day"))
  names(data)[10] <- "M002"
  
  # LimeSurvey Field type: A
  data[, 11] <- as.character(data[, 11])
  attributes(data)$variable.labels[11] <- "How long do your meetings usually last?"
  data[, 11] <- factor(data[, 11], levels=c("A1","A2","A3","A4"),labels=c("Less than 30 minutes", "30 minutes to 1 hour", "1 to 2 hours", "More than 2 hours"))
  names(data)[11] <- "M003"
  
  # LimeSurvey Field type: F
  data[, 12] <- as.numeric(data[, 12])
  attributes(data)$variable.labels[12] <- "[Notebook + pen/pencil] Which technologies do you most often bring with you to in-person meetings?"
  data[, 12] <- factor(data[, 12], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[12] <- "T001_SQ001"
  
  # LimeSurvey Field type: F
  data[, 13] <- as.numeric(data[, 13])
  attributes(data)$variable.labels[13] <- "[Laptop] Which technologies do you most often bring with you to in-person meetings?"
  data[, 13] <- factor(data[, 13], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[13] <- "T001_SQ002"
  
  # LimeSurvey Field type: F
  data[, 14] <- as.numeric(data[, 14])
  attributes(data)$variable.labels[14] <- "[Tablet] Which technologies do you most often bring with you to in-person meetings?"
  data[, 14] <- factor(data[, 14], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[14] <- "T001_SQ003"
  
  # LimeSurvey Field type: F
  data[, 15] <- as.numeric(data[, 15])
  attributes(data)$variable.labels[15] <- "[Smartphone] Which technologies do you most often bring with you to in-person meetings?"
  data[, 15] <- factor(data[, 15], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[15] <- "T001_SQ004"
  
  # LimeSurvey Field type: F
  data[, 16] <- as.numeric(data[, 16])
  attributes(data)$variable.labels[16] <- "[Audio recording device] Which technologies do you most often bring with you to in-person meetings?"
  data[, 16] <- factor(data[, 16], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[16] <- "T001_SQ005"
  
  # LimeSurvey Field type: F
  data[, 17] <- as.numeric(data[, 17])
  attributes(data)$variable.labels[17] <- "[Nothing] Which technologies do you most often bring with you to in-person meetings?"
  data[, 17] <- factor(data[, 17], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[17] <- "T001_SQ006"
  
  # LimeSurvey Field type: F
  data[, 18] <- as.numeric(data[, 18])
  attributes(data)$variable.labels[18] <- "[Other (please specify below)] Which technologies do you most often bring with you to in-person meetings?"
  data[, 18] <- factor(data[, 18], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[18] <- "T001_SQ007"
  
  # LimeSurvey Field type: F
  data[, 19] <- as.numeric(data[, 19])
  attributes(data)$variable.labels[19] <- "Please specify"
  names(data)[19] <- "T001a"
  
  # LimeSurvey Field type: A
  data[, 20] <- as.character(data[, 20])
  attributes(data)$variable.labels[20] <- "[During meetings I use a Notebook + pen/pencil for:] Complete the following statements about how you use your chosen technologies during in-person meetings."
  names(data)[20] <- "T002_SQ001"
  
  # LimeSurvey Field type: A
  data[, 21] <- as.character(data[, 21])
  attributes(data)$variable.labels[21] <- "[During meetings I use a Laptop for:] Complete the following statements about how you use your chosen technologies during in-person meetings."
  names(data)[21] <- "T002_SQ002"
  
  # LimeSurvey Field type: A
  data[, 22] <- as.character(data[, 22])
  attributes(data)$variable.labels[22] <- "[During meetings I use a Tablet for:] Complete the following statements about how you use your chosen technologies during in-person meetings."
  names(data)[22] <- "T002_SQ003"
  
  # LimeSurvey Field type: A
  data[, 23] <- as.character(data[, 23])
  attributes(data)$variable.labels[23] <- "[During meetings I use a Smartphone for:] Complete the following statements about how you use your chosen technologies during in-person meetings."
  names(data)[23] <- "T002_SQ004"
  
  # LimeSurvey Field type: A
  data[, 24] <- as.character(data[, 24])
  attributes(data)$variable.labels[24] <- "[During meetings I use an Audio recording device for:] Complete the following statements about how you use your chosen technologies during in-person meetings."
  names(data)[24] <- "T002_SQ005"
  
  # LimeSurvey Field type: F
  data[, 25] <- as.numeric(data[, 25])
  attributes(data)$variable.labels[25] <- "[During meetings I use {T001a} for:] Complete the following statements about how you use your chosen technologies during in-person meetings."
  names(data)[25] <- "T002_SQ007"
  
  # LimeSurvey Field type: F
  data[, 26] <- as.numeric(data[, 26])
  attributes(data)$variable.labels[26] <- "[I do not use notes] Which of the following scenarios describe your note-taking habits during meetings? (Select all that apply)"
  data[, 26] <- factor(data[, 26], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[26] <- "T003_SQ001"
  
  # LimeSurvey Field type: F
  data[, 27] <- as.numeric(data[, 27])
  attributes(data)$variable.labels[27] <- "[I rely on my own notes] Which of the following scenarios describe your note-taking habits during meetings? (Select all that apply)"
  data[, 27] <- factor(data[, 27], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[27] <- "T003_SQ002"
  
  # LimeSurvey Field type: F
  data[, 28] <- as.numeric(data[, 28])
  attributes(data)$variable.labels[28] <- "[I rely on protocols/meeting minutes and don\'t take my own notes] Which of the following scenarios describe your note-taking habits during meetings? (Select all that apply)"
  data[, 28] <- factor(data[, 28], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[28] <- "T003_SQ003"
  
  # LimeSurvey Field type: F
  data[, 29] <- as.numeric(data[, 29])
  attributes(data)$variable.labels[29] <- "[I use both my own notes and protocols/meeting minutes] Which of the following scenarios describe your note-taking habits during meetings? (Select all that apply)"
  data[, 29] <- factor(data[, 29], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[29] <- "T003_SQ004"
  
  # LimeSurvey Field type: F
  data[, 30] <- as.numeric(data[, 30])
  attributes(data)$variable.labels[30] <- "[Other (please specify below)] Which of the following scenarios describe your note-taking habits during meetings? (Select all that apply)"
  data[, 30] <- factor(data[, 30], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[30] <- "T003_SQ005"
  
  # LimeSurvey Field type: F
  data[, 31] <- as.numeric(data[, 31])
  attributes(data)$variable.labels[31] <- "Please specify"
  names(data)[31] <- "T003a"
  
  # LimeSurvey Field type: F
  data[, 32] <- as.numeric(data[, 32])
  attributes(data)$variable.labels[32] <- "[No notes] How do you usually take notes during a meeting? (Select all that apply)"
  data[, 32] <- factor(data[, 32], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[32] <- "T004_SQ001"
  
  # LimeSurvey Field type: F
  data[, 33] <- as.numeric(data[, 33])
  attributes(data)$variable.labels[33] <- "[Handwritten] How do you usually take notes during a meeting? (Select all that apply)"
  data[, 33] <- factor(data[, 33], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[33] <- "T004_SQ002"
  
  # LimeSurvey Field type: F
  data[, 34] <- as.numeric(data[, 34])
  attributes(data)$variable.labels[34] <- "[Laptop] How do you usually take notes during a meeting? (Select all that apply)"
  data[, 34] <- factor(data[, 34], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[34] <- "T004_SQ003"
  
  # LimeSurvey Field type: F
  data[, 35] <- as.numeric(data[, 35])
  attributes(data)$variable.labels[35] <- "[Tablet] How do you usually take notes during a meeting? (Select all that apply)"
  data[, 35] <- factor(data[, 35], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[35] <- "T004_SQ004"
  
  # LimeSurvey Field type: F
  data[, 36] <- as.numeric(data[, 36])
  attributes(data)$variable.labels[36] <- "[Smartphone] How do you usually take notes during a meeting? (Select all that apply)"
  data[, 36] <- factor(data[, 36], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[36] <- "T004_SQ005"
  
  # LimeSurvey Field type: F
  data[, 37] <- as.numeric(data[, 37])
  attributes(data)$variable.labels[37] <- "[Audio recording] How do you usually take notes during a meeting? (Select all that apply)"
  data[, 37] <- factor(data[, 37], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[37] <- "T004_SQ006"
  
  # LimeSurvey Field type: F
  data[, 38] <- as.numeric(data[, 38])
  attributes(data)$variable.labels[38] <- "[Other (please specify below)] How do you usually take notes during a meeting? (Select all that apply)"
  data[, 38] <- factor(data[, 38], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[38] <- "T004_SQ007"
  
  # LimeSurvey Field type: F
  data[, 39] <- as.numeric(data[, 39])
  attributes(data)$variable.labels[39] <- "Please specify"
  names(data)[39] <- "T004a"
  
  # LimeSurvey Field type: A
  data[, 40] <- as.character(data[, 40])
  attributes(data)$variable.labels[40] <- "How often do you participate in walking meetings?"
  data[, 40] <- factor(data[, 40], levels=c("A1","A2","A3","A4"),labels=c("Never", "Less than once per month", "1-4 times per month", "More than 4 times per month"))
  names(data)[40] <- "MM001"
  
  # LimeSurvey Field type: F
  data[, 41] <- as.numeric(data[, 41])
  attributes(data)$variable.labels[41] <- "[Physical activity] Which of the following are reasons that you do or would want to participate in walking meetings? (select all that apply)"
  data[, 41] <- factor(data[, 41], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[41] <- "WM002_SQ001"
  
  # LimeSurvey Field type: F
  data[, 42] <- as.numeric(data[, 42])
  attributes(data)$variable.labels[42] <- "[Fresh air] Which of the following are reasons that you do or would want to participate in walking meetings? (select all that apply)"
  data[, 42] <- factor(data[, 42], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[42] <- "WM002_SQ002"
  
  # LimeSurvey Field type: F
  data[, 43] <- as.numeric(data[, 43])
  attributes(data)$variable.labels[43] <- "[I think better while I walk] Which of the following are reasons that you do or would want to participate in walking meetings? (select all that apply)"
  data[, 43] <- factor(data[, 43], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[43] <- "WM002_SQ003"
  
  # LimeSurvey Field type: F
  data[, 44] <- as.numeric(data[, 44])
  attributes(data)$variable.labels[44] <- "[Better conversation flow] Which of the following are reasons that you do or would want to participate in walking meetings? (select all that apply)"
  data[, 44] <- factor(data[, 44], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[44] <- "WM002_SQ004"
  
  # LimeSurvey Field type: F
  data[, 45] <- as.numeric(data[, 45])
  attributes(data)$variable.labels[45] <- "[I find walking meetings enjoyable] Which of the following are reasons that you do or would want to participate in walking meetings? (select all that apply)"
  data[, 45] <- factor(data[, 45], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[45] <- "WM002_SQ005"
  
  # LimeSurvey Field type: F
  data[, 46] <- as.numeric(data[, 46])
  attributes(data)$variable.labels[46] <- "[Develop team dynamics] Which of the following are reasons that you do or would want to participate in walking meetings? (select all that apply)"
  data[, 46] <- factor(data[, 46], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[46] <- "WM002_SQ006"
  
  # LimeSurvey Field type: F
  data[, 47] <- as.numeric(data[, 47])
  attributes(data)$variable.labels[47] <- "[Walking meetings improve my mental well-being] Which of the following are reasons that you do or would want to participate in walking meetings? (select all that apply)"
  data[, 47] <- factor(data[, 47], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[47] <- "WM002_SQ007"
  
  # LimeSurvey Field type: F
  data[, 48] <- as.numeric(data[, 48])
  attributes(data)$variable.labels[48] <- "[Other (please specify)] Which of the following are reasons that you do or would want to participate in walking meetings? (select all that apply)"
  data[, 48] <- factor(data[, 48], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[48] <- "WM002_SQ008"
  
  # LimeSurvey Field type: F
  data[, 49] <- as.numeric(data[, 49])
  attributes(data)$variable.labels[49] <- "Please specify."
  names(data)[49] <- "WM002a"
  
  # LimeSurvey Field type: F
  data[, 50] <- as.numeric(data[, 50])
  attributes(data)$variable.labels[50] <- "[Difficulty in taking notes] Which of the following are reasons that you do not participate in walking meetings? (Select all that apply)"
  data[, 50] <- factor(data[, 50], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[50] <- "MM002_SQ001"
  
  # LimeSurvey Field type: F
  data[, 51] <- as.numeric(data[, 51])
  attributes(data)$variable.labels[51] <- "[Lack of ability to present visuals] Which of the following are reasons that you do not participate in walking meetings? (Select all that apply)"
  data[, 51] <- factor(data[, 51], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[51] <- "MM002_SQ002"
  
  # LimeSurvey Field type: F
  data[, 52] <- as.numeric(data[, 52])
  attributes(data)$variable.labels[52] <- "[Limitations in group size] Which of the following are reasons that you do not participate in walking meetings? (Select all that apply)"
  data[, 52] <- factor(data[, 52], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[52] <- "MM002_SQ003"
  
  # LimeSurvey Field type: F
  data[, 53] <- as.numeric(data[, 53])
  attributes(data)$variable.labels[53] <- "[Not part of my routine] Which of the following are reasons that you do not participate in walking meetings? (Select all that apply)"
  data[, 53] <- factor(data[, 53], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[53] <- "MM002_SQ004"
  
  # LimeSurvey Field type: F
  data[, 54] <- as.numeric(data[, 54])
  attributes(data)$variable.labels[54] <- "[Bad weather] Which of the following are reasons that you do not participate in walking meetings? (Select all that apply)"
  data[, 54] <- factor(data[, 54], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[54] <- "MM002_SQ005"
  
  # LimeSurvey Field type: F
  data[, 55] <- as.numeric(data[, 55])
  attributes(data)$variable.labels[55] <- "[Moving around is distracting] Which of the following are reasons that you do not participate in walking meetings? (Select all that apply)"
  data[, 55] <- factor(data[, 55], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[55] <- "MM002_SQ006"
  
  # LimeSurvey Field type: F
  data[, 56] <- as.numeric(data[, 56])
  attributes(data)$variable.labels[56] <- "[I would feel awkward asking someone to do a walking meeting] Which of the following are reasons that you do not participate in walking meetings? (Select all that apply)"
  data[, 56] <- factor(data[, 56], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[56] <- "MM002_SQ007"
  
  # LimeSurvey Field type: F
  data[, 57] <- as.numeric(data[, 57])
  attributes(data)$variable.labels[57] <- "[Other (please specify)] Which of the following are reasons that you do not participate in walking meetings? (Select all that apply)"
  data[, 57] <- factor(data[, 57], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[57] <- "MM002_SQ008"
  
  # LimeSurvey Field type: A
  data[, 58] <- as.character(data[, 58])
  attributes(data)$variable.labels[58] <- "Please specify."
  names(data)[58] <- "MM002a"
  
  # LimeSurvey Field type: F
  data[, 59] <- as.numeric(data[, 59])
  attributes(data)$variable.labels[59] <- "[Notebook + pen/pencil] If you were to participate in a walking meeting, which technologies would you be most likely to bring?"
  data[, 59] <- factor(data[, 59], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[59] <- "MM003_SQ001"
  
  # LimeSurvey Field type: F
  data[, 60] <- as.numeric(data[, 60])
  attributes(data)$variable.labels[60] <- "[Laptop] If you were to participate in a walking meeting, which technologies would you be most likely to bring?"
  data[, 60] <- factor(data[, 60], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[60] <- "MM003_SQ002"
  
  # LimeSurvey Field type: F
  data[, 61] <- as.numeric(data[, 61])
  attributes(data)$variable.labels[61] <- "[Tablet] If you were to participate in a walking meeting, which technologies would you be most likely to bring?"
  data[, 61] <- factor(data[, 61], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[61] <- "MM003_SQ003"
  
  # LimeSurvey Field type: F
  data[, 62] <- as.numeric(data[, 62])
  attributes(data)$variable.labels[62] <- "[Smartphone] If you were to participate in a walking meeting, which technologies would you be most likely to bring?"
  data[, 62] <- factor(data[, 62], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[62] <- "MM003_SQ004"
  
  # LimeSurvey Field type: F
  data[, 63] <- as.numeric(data[, 63])
  attributes(data)$variable.labels[63] <- "[Audio recording device] If you were to participate in a walking meeting, which technologies would you be most likely to bring?"
  data[, 63] <- factor(data[, 63], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[63] <- "MM003_SQ005"
  
  # LimeSurvey Field type: F
  data[, 64] <- as.numeric(data[, 64])
  attributes(data)$variable.labels[64] <- "[Nothing] If you were to participate in a walking meeting, which technologies would you be most likely to bring?"
  data[, 64] <- factor(data[, 64], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[64] <- "MM003_SQ007"
  
  # LimeSurvey Field type: F
  data[, 65] <- as.numeric(data[, 65])
  attributes(data)$variable.labels[65] <- "[Other (please specify below)] If you were to participate in a walking meeting, which technologies would you be most likely to bring?"
  data[, 65] <- factor(data[, 65], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[65] <- "MM003_SQ006"
  
  # LimeSurvey Field type: F
  data[, 66] <- as.numeric(data[, 66])
  attributes(data)$variable.labels[66] <- "Please specify."
  names(data)[66] <- "MM003a"
  
  # LimeSurvey Field type: A
  data[, 67] <- as.character(data[, 67])
  attributes(data)$variable.labels[67] <- "[During walking meetings I would use a Notebook + pen/pencil for:] Complete the following statements about how you use your chosen technologies during walking meetings."
  names(data)[67] <- "MM003b_SQ001"
  
  # LimeSurvey Field type: A
  data[, 68] <- as.character(data[, 68])
  attributes(data)$variable.labels[68] <- "[During walking meetings I would use a Laptop for:] Complete the following statements about how you use your chosen technologies during walking meetings."
  names(data)[68] <- "MM003b_SQ002"
  
  # LimeSurvey Field type: A
  data[, 69] <- as.character(data[, 69])
  attributes(data)$variable.labels[69] <- "[During walking meetings I would use a Tablet for:] Complete the following statements about how you use your chosen technologies during walking meetings."
  names(data)[69] <- "MM003b_SQ003"
  
  # LimeSurvey Field type: A
  data[, 70] <- as.character(data[, 70])
  attributes(data)$variable.labels[70] <- "[During walking meetings I would use a Smartphone for:] Complete the following statements about how you use your chosen technologies during walking meetings."
  names(data)[70] <- "MM003b_SQ004"
  
  # LimeSurvey Field type: A
  data[, 71] <- as.character(data[, 71])
  attributes(data)$variable.labels[71] <- "[During walking meetings I would use an Audio recording device for:] Complete the following statements about how you use your chosen technologies during walking meetings."
  names(data)[71] <- "MM003b_SQ005"
  
  # LimeSurvey Field type: F
  data[, 72] <- as.numeric(data[, 72])
  attributes(data)$variable.labels[72] <- "[During walking meetings I would use {MM003a} for:] Complete the following statements about how you use your chosen technologies during walking meetings."
  names(data)[72] <- "MM003b_SQ006"
  
  # LimeSurvey Field type: F
  data[, 73] <- as.numeric(data[, 73])
  attributes(data)$variable.labels[73] <- "[People who I supervise] Which of the following individuals would you join on a walking meeting? (Select all that apply)"
  data[, 73] <- factor(data[, 73], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[73] <- "MM004_SQ001"
  
  # LimeSurvey Field type: F
  data[, 74] <- as.numeric(data[, 74])
  attributes(data)$variable.labels[74] <- "[Coworkers] Which of the following individuals would you join on a walking meeting? (Select all that apply)"
  data[, 74] <- factor(data[, 74], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[74] <- "MM004_SQ002"
  
  # LimeSurvey Field type: F
  data[, 75] <- as.numeric(data[, 75])
  attributes(data)$variable.labels[75] <- "[My boss] Which of the following individuals would you join on a walking meeting? (Select all that apply)"
  data[, 75] <- factor(data[, 75], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[75] <- "MM004_SQ003"
  
  # LimeSurvey Field type: F
  data[, 76] <- as.numeric(data[, 76])
  attributes(data)$variable.labels[76] <- "[Coworkers I feel close with] Which of the following individuals would you join on a walking meeting? (Select all that apply)"
  data[, 76] <- factor(data[, 76], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[76] <- "MM004_SQ004"
  
  # LimeSurvey Field type: F
  data[, 77] <- as.numeric(data[, 77])
  attributes(data)$variable.labels[77] <- "[Other (please specify)] Which of the following individuals would you join on a walking meeting? (Select all that apply)"
  data[, 77] <- factor(data[, 77], levels=c(1,0),labels=c("Yes", "Not selected"))
  names(data)[77] <- "MM004_SQ005"
  
  # LimeSurvey Field type: F
  data[, 78] <- as.numeric(data[, 78])
  attributes(data)$variable.labels[78] <- "Please specify."
  names(data)[78] <- "MM004a"
  
  # LimeSurvey Field type: A
  data[, 79] <- as.character(data[, 79])
  attributes(data)$variable.labels[79] <- "Additional comments (Optional)"
  names(data)[79] <- "AC001"
  
  # LimeSurvey Field type: A
  data[, 80] <- as.character(data[, 80])
  attributes(data)$variable.labels[80] <- "Mturk Code or Email"
  names(data)[80] <- "AC002"
  
  i <- i+1
}


# combine the two surveys
data_normal <- data

data <- rbind(data_turk, data_normal)


##########
# Plotting and processing from here on out ##
##########
library(ggplot2)
library(scales)
library(dplyr)
library(ggpubr)

##########
## DEMOGRAPHICS ##
##########

#number of respondents
N = length(data$id)

# average age of the respondents 
age_avg = mean(data$D001)
sd_age = sd(data$D001)

# check age distribution
shapiro.test(data$D001)
ggdensity(data$D001)

# gender split
female_count = sum(data$D002 == "Female")
female_perc = 100*female_count/N
male_count = N-female_count
male_pec = 100-female_perc


##########
## MEETINGS ##
##########

# plot the answers to M001 - how often do you participate in meetings?
M001_count = as.data.frame(table(data$M001))
names(M001_count) = c("Response", "Count")
M001_count

M001_plot <- ggplot(M001_count, aes(x=Response,y=Count)) + 
  geom_bar(stat="identity", alpha=0.6) + 
  ggtitle(attributes(data)$variable.labels[9]) + 
  scale_y_continuous(limits=c(0,max(M001_count$Count)+1), expand=c(0,0)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank())
M001_plot

# plot the answers to M001 - how often do you participate in meetings with 2 or fewer colleagues?
M002_count = as.data.frame(table(data$M002))
names(M002_count) = c("Response", "Count")
M002_count

M002_plot <- ggplot(M002_count, aes(x=Response,y=Count)) + 
  geom_bar(stat="identity") + 
  ggtitle(attributes(data)$variable.labels[10]) + 
  scale_y_continuous(limits=c(0,max(M002_count$Count)+1),breaks=pretty_breaks(), expand=c(0,0)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank())
M002_plot

# plot the answers to M001 - how long do your meetings usually last??
M003_count = as.data.frame(table(data$M003))
names(M003_count) = c("Response", "Count")
M003_count

M003_plot <- ggplot(M003_count, aes(x=Response,y=Count)) + 
  geom_bar(stat="identity") + 
  ggtitle(attributes(data)$variable.labels[11]) + 
  scale_y_continuous(limits=c(0,max(M003_count$Count)+1), breaks=pretty_breaks(), expand=c(0,0)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank())
M003_plot


##########
## TECHONOLOGY IN MEETINGS ##
##########

# tally and plot the answers to T001 - which tech do you bring to meetings?
tech_options = c("Notebook + pen/pencil","Laptop","Tablet","Smartpone", "Audio recording device","Nothing","Other")

T001_tally = c(sum(data$T001_SQ001 == "Yes"),
               sum(data$T001_SQ002 == "Yes"),
               sum(data$T001_SQ003 == "Yes"),
               sum(data$T001_SQ004 == "Yes"),
               sum(data$T001_SQ005 == "Yes"),
               sum(data$T001_SQ006 == "Yes"),
               sum(data$T001_SQ007 == "Yes"))

T001_count <- data.frame("Technology" = tech_options,
                         "Count" = T001_tally)
T001_count

T001_plot <- ggplot(T001_count, aes(x=reorder(Technology, -Count),y=Count)) + 
  geom_bar(stat="identity") + 
  ggtitle("Which technologies do you most often bring with you to in-person meetings?") + 
  scale_y_continuous(limits=c(0,max(T001_count$Count)+1), breaks=pretty_breaks(), expand=c(0,0)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank())
T001_plot

# T002 is written answers, no plotting

# tally and plot the answers to T003 - which of the following describes your note-taking habits?
note_options = c("No Notes",
                 "Own Notes",
                 "Minutes",
                 "Own notes + Minutes")

T003_tally = c(sum(data$T003_SQ001 == "Yes"),
               sum(data$T003_SQ002 == "Yes"),
               sum(data$T003_SQ003 == "Yes"),
               sum(data$T003_SQ004 == "Yes"))

T003_count <- data.frame("Notes" = note_options,
                         "Count" = T003_tally)
T003_count

T003_plot <- ggplot(T003_count, aes(x=reorder(Notes, -Count),y=Count)) + 
  geom_bar(stat="identity", alpha=0.6) + 
  ggtitle("Note-taking habits") + 
  scale_y_continuous(limits=c(0,max(T003_count$Count)+1), breaks=pretty_breaks(), expand=c(0,0)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank(), text=element_text(size=20))
T003_plot

# tally and plot the answers to T004 - how do you normally take notes during a meeting?
note_tech = c("No Notes", "Handwritten","Laptop","Tablet","Smartpone", "Audio Recording Device","Other")

T004_tally = c(sum(data$T004_SQ001 == "Yes"),
               sum(data$T004_SQ002 == "Yes"),
               sum(data$T004_SQ003 == "Yes"),
               sum(data$T004_SQ004 == "Yes"),
               sum(data$T004_SQ005 == "Yes"),
               sum(data$T004_SQ006 == "Yes"),
               sum(data$T004_SQ007 == "Yes"))

T004_count <- data.frame("NoteTech" = note_tech,
                         "Count" = T004_tally)
T004_count

T004_plot <- ggplot(T004_count, aes(x=reorder(NoteTech, -Count),y=Count)) + 
  geom_bar(stat="identity") + 
  ggtitle("How do you usually take notes during a meeting?") + 
  scale_y_continuous(limits=c(0,max(T004_count$Count)+1), breaks=pretty_breaks(), expand=c(0,0)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank())
T004_plot


##########
## WALKING MEETINGS ##
##########

# plot the answers to MM001 - how often do you participate in walking meetings?
MM001_count = as.data.frame(table(data$MM001))
names(MM001_count) = c("Response", "Count")
MM001_count

MM001_plot <- ggplot(MM001_count, aes(x=Response,y=Count)) + 
  geom_bar(stat="identity", alpha=0.6) + 
  ggtitle(attributes(data)$variable.labels[40]) + 
  scale_y_continuous(limits=c(0,max(MM001_count$Count)+1),breaks=pretty_breaks(), expand=c(0,0)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank(), text=element_text(size=15))
MM001_plot

# tally and plot the answers to WM002 - Which of the following are reasons that you do or would want to participate in walking meetings?
do_walk = c("Physical Activity",
            "Fresh Air",
            "I think better while I walk",
            "Better conversation flow",
            "Enjoyment",
            "Team dynamics",
            "Mental well-being")

WM002_tally = c(sum(data$WM002_SQ001 == "Yes"),
                sum(data$WM002_SQ002 == "Yes"),
                sum(data$WM002_SQ003 == "Yes"),
                sum(data$WM002_SQ004 == "Yes"),
                sum(data$WM002_SQ005 == "Yes"),
                sum(data$WM002_SQ006 == "Yes"),
                sum(data$WM002_SQ007 == "Yes"))

WM002_count <- data.frame("DoWalk" = do_walk,
                          "Count" = WM002_tally)
WM002_count

WM002_plot <- ggplot(WM002_count, aes(x=reorder(DoWalk, -Count),y=Count)) + 
  geom_bar(stat="identity", alpha=0.6) + 
  ggtitle("Motivation for walking meetings") + 
  scale_y_continuous(limits=c(0,max(WM002_count$Count)+1), breaks=pretty_breaks(), expand=c(0,0)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5), axis.title.y=element_blank()) +
  coord_flip()
WM002_plot

# tally and plot the answers to MM002 - Which of the following are reasons that you do or would want to participate in walking meetings?
dont_walk = c("Difficulty taking notes",
              "Lack of ability to present visuals",
              "Limitations in group size",
              "Not part of my routine",
              "Bad weather",
              "Moving around is distracting",
              "I would feel awkward asking")

MM002_tally = c(sum(data$MM002_SQ001 == "Yes"),
                sum(data$MM002_SQ002 == "Yes"),
                sum(data$MM002_SQ003 == "Yes"),
                sum(data$MM002_SQ004 == "Yes"),
                sum(data$MM002_SQ005 == "Yes"),
                sum(data$MM002_SQ006 == "Yes"),
                sum(data$MM002_SQ007 == "Yes"))

MM002_count <- data.frame("DontWalk" = dont_walk,
                          "Count" = MM002_tally)
MM002_count

MM002_plot <- ggplot(MM002_count, aes(x=reorder(DontWalk, -Count),y=Count)) + 
  geom_bar(stat="identity", alpha=0.6) + 
  ggtitle("Hurdles to walking meetings") + 
  scale_y_continuous(limits=c(0,max(MM002_count$Count)+1), breaks=pretty_breaks(), expand=c(0,0)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5), axis.title.y=element_blank()) +
  coord_flip()
MM002_plot

# tally and plot the answers to MM003 - which tech do would you bring to walking meetings?
tech_options = c("Notebook + pen/pencil","Laptop","Tablet","Smartpone", "Audio recording device","Nothing")

MM003_tally = c(sum(data$MM003_SQ001 == "Yes"),
                sum(data$MM003_SQ002 == "Yes"),
                sum(data$MM003_SQ003 == "Yes"),
                sum(data$MM003_SQ004 == "Yes"),
                sum(data$MM003_SQ005 == "Yes"),
                sum(data$MM003_SQ007 == "Yes"))

MM003_count <- data.frame("Technology" = tech_options,
                          "Count" = MM003_tally)
MM003_count

MM003_plot <- ggplot(MM003_count, aes(x=reorder(Technology, -Count),y=Count)) + 
  geom_bar(stat="identity", alpha=0.6) + 
  ggtitle("Technology brought to walking meetings") + 
  scale_y_continuous(limits=c(0,max(MM003_count$Count)+1), breaks=pretty_breaks(), expand=c(0,0)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank())
MM003_plot

# MM003b is written answers, no plots for now

# tally and plot the answers to MM004 - 	Which of the following individuals would you join on a walking meeting? 
walk_ppl = c("People who I supervise", "Coworkers","My boss","Coworkers I feel close with","Other")

MM004_tally = c(sum(data$MM004_SQ001 == "Yes"),
                sum(data$MM004_SQ002 == "Yes"),
                sum(data$MM004_SQ003 == "Yes"),
                sum(data$MM004_SQ004 == "Yes"),
                sum(data$MM004_SQ005 == "Yes"))

MM004_count <- data.frame("Partners" = walk_ppl,
                          "Count" = MM004_tally)
MM004_count

MM004_plot <- ggplot(MM004_count, aes(x=reorder(Partners, -Count),y=Count)) + 
  geom_bar(stat="identity") + 
  ggtitle("Which of the following individuals would you join on a walking meeting? ") + 
  scale_y_continuous(limits=c(0,max(MM004_count$Count)+1), breaks=pretty_breaks(), expand=c(0,0)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank())
MM004_plot

# Compare the two groups
data_nonwalkers$Condition = "Non-walkers"
data_walkers$Condition = "Walkers"
data_testing=rbind(data_nonwalkers,data_walkers)
t.test(as.numeric(data_testing$WM002_SQ001)~data_testing$Condition)
t.test(as.numeric(data_testing$WM002_SQ002)~data_testing$Condition)
t.test(as.numeric(data_testing$WM002_SQ003)~data_testing$Condition)
t.test(as.numeric(data_testing$WM002_SQ004)~data_testing$Condition)
t.test(as.numeric(data_testing$WM002_SQ005)~data_testing$Condition)
t.test(as.numeric(data_testing$WM002_SQ006)~data_testing$Condition)
t.test(as.numeric(data_testing$WM002_SQ007)~data_testing$Condition)
t.test(as.numeric(data_testing$WM002_SQ008)~data_testing$Condition)

t.test(as.numeric(data_testing$MM002_SQ001)~data_testing$Condition)
t.test(as.numeric(data_testing$MM002_SQ002)~data_testing$Condition)
t.test(as.numeric(data_testing$MM002_SQ003)~data_testing$Condition)
t.test(as.numeric(data_testing$MM002_SQ004)~data_testing$Condition)
t.test(as.numeric(data_testing$MM002_SQ005)~data_testing$Condition)
t.test(as.numeric(data_testing$MM002_SQ006)~data_testing$Condition)
t.test(as.numeric(data_testing$MM002_SQ007)~data_testing$Condition)
t.test(as.numeric(data_testing$MM002_SQ008)~data_testing$Condition)



