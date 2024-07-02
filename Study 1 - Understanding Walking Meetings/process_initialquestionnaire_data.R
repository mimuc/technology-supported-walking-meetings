# # set the wd to the script location
# library(rstudioapi)
# current_path <- getActiveDocumentContext()$path 
# setwd(dirname(current_path))

#$Rev: 10193 $ complete.
data <- read.csv("initial_questionnaire.csv", quote = "'\"", na.strings=c("", "\"\""), stringsAsFactors=FALSE)




##########
# Plotting and processing from here on out ##
##########
library(ggplot2)
library(scales)
library(dplyr)
library(ggpubr)
library(ggsignif)

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



#############
# Split by walkers and non-experienced
#################

data_walkers <- data[(data$MM001 == "1-4 times per month")|(data$MM001 == "More than 4 times per month")|(data$MM001 == "Less than once per month"),]
data_nonwalkers <- data[(data$MM001 == "Never"),]

##### Experienced Walkers #####

##########
# Plotting and processing from here on out ##
##########




##########
## DEMOGRAPHICS ##
##########

#number of respondents
N_w = length(data_walkers$id)

# average age of the respondents 
age_avg_w = mean(data_walkers$D001)

# gender split
female_count_w = sum(data_walkers$D002 == "Female")
female_perc_w = 100*female_count_w/N_w
male_count_w = N_w-female_count_w
male_pec_w = 100-female_perc_w

##########
## MEETINGS ##
##########

# plot the answers to M001 - how often do you participate in meetings?
M001_count = as.data.frame(table(data_walkers$M001))
names(M001_count) = c("Response", "Count")
M001_count

M001_plot <- ggplot(M001_count, aes(x=Response,y=Count)) + 
  geom_bar(stat="identity") + 
  ggtitle(attributes(data_walkers)$variable.labels[9]) + 
  scale_y_continuous(limits=c(0,max(M001_count$Count)+1),breaks=pretty_breaks(), expand=c(0,0)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank())
M001_plot

# plot the answers to M001 - how often do you participate in meetings with 2 or fewer colleagues?
M002_count = as.data.frame(table(data_walkers$M002))
names(M002_count) = c("Response", "Count")
M002_count

M002_plot <- ggplot(M002_count, aes(x=Response,y=Count)) + 
  geom_bar(stat="identity") + 
  ggtitle(attributes(data_walkers)$variable.labels[10]) + 
  scale_y_continuous(limits=c(0,max(M002_count$Count)+1),breaks=pretty_breaks(), expand=c(0,0)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank())
M002_plot

# plot the answers to M001 - how long do your meetings usually last??
M003_count = as.data.frame(table(data_walkers$M003))
names(M003_count) = c("Response", "Count")
M003_count

M003_plot <- ggplot(M003_count, aes(x=Response,y=Count)) + 
  geom_bar(stat="identity") + 
  ggtitle(attributes(data_walkers)$variable.labels[11]) + 
  scale_y_continuous(limits=c(0,max(M003_count$Count)+1), breaks=pretty_breaks(), expand=c(0,0)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank())
M003_plot

##########
## TECHONOLOGY IN MEETINGS ##
##########

# tally and plot the answers to T001 - which tech do you bring to meetings?
tech_options = c("Notebook + pen/pencil","Laptop","Tablet","Smartpone", "Audio recording device","Nothing","Other")

T001_tally = c(sum(data_walkers$T001_SQ001 == "Yes"),
               sum(data_walkers$T001_SQ002 == "Yes"),
               sum(data_walkers$T001_SQ003 == "Yes"),
               sum(data_walkers$T001_SQ004 == "Yes"),
               sum(data_walkers$T001_SQ005 == "Yes"),
               sum(data_walkers$T001_SQ006 == "Yes"),
               sum(data_walkers$T001_SQ007 == "Yes"))

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
note_options = c("No notes",
                 "Own notes",
                 "Minutes",
                 "Own notes + Minutes",
                 "Other")

T003_tally = c(sum(data_walkers$T003_SQ001 == "Yes"),
               sum(data_walkers$T003_SQ002 == "Yes"),
               sum(data_walkers$T003_SQ003 == "Yes"),
               sum(data_walkers$T003_SQ004 == "Yes"),
               sum(data_walkers$T003_SQ005 == "Yes"))

T003_count <- data.frame("Notes" = note_options,
                         "Count" = T003_tally)
T003_count

T003_plot <- ggplot(T003_count, aes(x=reorder(Notes, -Count),y=Count)) + 
  geom_bar(stat="identity") + 
  ggtitle("Which of the following scenarios describe your note-taking habits during meetings?") + 
  scale_y_continuous(limits=c(0,max(T003_count$Count)+1), breaks=pretty_breaks(), expand=c(0,0)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank())
T003_plot

# tally and plot the answers to T004 - how do you normally take notes during a meeting?
note_tech = c("No Notes", "Handwritten","Laptop","Tablet","Smartpone", "Audio recording device","Other")

T004_tally = c(sum(data_walkers$T004_SQ001 == "Yes"),
               sum(data_walkers$T004_SQ002 == "Yes"),
               sum(data_walkers$T004_SQ003 == "Yes"),
               sum(data_walkers$T004_SQ004 == "Yes"),
               sum(data_walkers$T004_SQ005 == "Yes"),
               sum(data_walkers$T004_SQ006 == "Yes"),
               sum(data_walkers$T004_SQ007 == "Yes"))

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
MM001_count = as.data.frame(table(data_walkers$MM001))
names(MM001_count) = c("Response", "Count")
MM001_count

MM001_plot <- ggplot(MM001_count, aes(x=Response,y=Count)) + 
  geom_bar(stat="identity") + 
  ggtitle(attributes(data_walkers)$variable.labels[40]) + 
  scale_y_continuous(limits=c(0,max(MM001_count$Count)+1),breaks=pretty_breaks(), expand=c(0,0)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank())
MM001_plot

# tally and plot the answers to WM002 - Which of the following are reasons that you do or would want to participate in walking meetings?
do_walk = c("Physical Activity",
            "Fresh Air",
            "I think better while I walk",
            "Better conversation flow",
            "Enjoyment",
            "Team dynamics",
            "Mental well-being")

WM002_tally = c(sum(data_walkers$WM002_SQ001 == "Yes"),
                sum(data_walkers$WM002_SQ002 == "Yes"),
                sum(data_walkers$WM002_SQ003 == "Yes"),
                sum(data_walkers$WM002_SQ004 == "Yes"),
                sum(data_walkers$WM002_SQ005 == "Yes"),
                sum(data_walkers$WM002_SQ006 == "Yes"),
                sum(data_walkers$WM002_SQ007 == "Yes"))

WM002_count <- data.frame("DoWalk" = do_walk,
                          "Count" = WM002_tally)
WM002_count

WM002_plot <- ggplot(WM002_count, aes(x=reorder(DoWalk, -Count),y=Count)) + 
  geom_bar(stat="identity") + 
  ggtitle("Motivations for Walking Meetings") + 
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
              "I would feel awkward asking",
              "Other")

MM002_tally = c(sum(data_walkers$MM002_SQ001 == "Yes"),
                sum(data_walkers$MM002_SQ002 == "Yes"),
                sum(data_walkers$MM002_SQ003 == "Yes"),
                sum(data_walkers$MM002_SQ004 == "Yes"),
                sum(data_walkers$MM002_SQ005 == "Yes"),
                sum(data_walkers$MM002_SQ006 == "Yes"),
                sum(data_walkers$MM002_SQ007 == "Yes"),
                sum(data_walkers$MM002_SQ008 == "Yes"))

MM002_count <- data.frame("DontWalk" = dont_walk,
                          "Count" = MM002_tally)
MM002_count

MM002_plot <- ggplot(MM002_count, aes(x=reorder(DontWalk, -Count),y=Count)) + 
  geom_bar(stat="identity") + 
  ggtitle("Which of the following are reasons that you do not participate in walking meetings?") + 
  scale_y_continuous(limits=c(0,max(MM002_count$Count)+1), breaks=pretty_breaks(), expand=c(0,0)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5), axis.title.y=element_blank()) +
  coord_flip()
MM002_plot

# tally and plot the answers to MM003 - which tech do would you bring to walking meetings?
tech_options = c("Notebook + pen/pencil","Laptop","Tablet","Smartpone", "Audio recording device","Nothing","Other")

MM003_tally = c(sum(data_walkers$MM003_SQ001 == "Yes"),
                sum(data_walkers$MM003_SQ002 == "Yes"),
                sum(data_walkers$MM003_SQ003 == "Yes"),
                sum(data_walkers$MM003_SQ004 == "Yes"),
                sum(data_walkers$MM003_SQ005 == "Yes"),
                sum(data_walkers$MM003_SQ007 == "Yes"),
                sum(data_walkers$MM003_SQ006 == "Yes"))

MM003_count <- data.frame("Technology" = tech_options,
                          "Count" = MM003_tally)
MM003_count

MM003_plot <- ggplot(MM003_count, aes(x=reorder(Technology, -Count),y=Count)) + 
  geom_bar(stat="identity") + 
  ggtitle("If you were to participate in a walking meeting, which technologies would you be most likely to bring?") + 
  scale_y_continuous(limits=c(0,max(MM003_count$Count)+1), breaks=pretty_breaks(), expand=c(0,0)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank())
MM003_plot

# MM003b is written answers, no plots for now

# tally and plot the answers to MM004 - 	Which of the following individuals would you join on a walking meeting? 
walk_ppl = c("People who I supervise", "Coworkers","My boss","Coworkers I feel close with","Other")

MM004_tally = c(sum(data_walkers$MM004_SQ001 == "Yes"),
                sum(data_walkers$MM004_SQ002 == "Yes"),
                sum(data_walkers$MM004_SQ003 == "Yes"),
                sum(data_walkers$MM004_SQ004 == "Yes"),
                sum(data_walkers$MM004_SQ005 == "Yes"))

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






##### NON walkers #####

##########
# Plotting and processing from here on out ##
##########

##########
## DEMOGRAPHICS ##
##########

#number of respondents
N_nw = length(data_nonwalkers$id)

# average age of the respondents 
age_avg_nw = mean(data_nonwalkers$D001)

# gender split
female_count_nw = sum(data_nonwalkers$D002 == "Female")
female_perc_nw = 100*female_count_nw/N_nw
male_count_nw = N_nw-female_count_nw
male_pec_nw = 100-female_perc_nw

##########
## MEETINGS ##
##########

# plot the answers to M001 - how often do you participate in meetings?
M001_count = as.data.frame(table(data_nonwalkers$M001))
names(M001_count) = c("Response", "Count")
M001_count

M001_plot <- ggplot(M001_count, aes(x=Response,y=Count)) + 
  geom_bar(stat="identity") + 
  ggtitle(attributes(data_nonwalkers)$variable.labels[9]) + 
  scale_y_continuous(limits=c(0,max(M001_count$Count)+1),breaks=pretty_breaks(), expand=c(0,0)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank())
M001_plot

# plot the answers to M001 - how often do you participate in meetings with 2 or fewer colleagues?
M002_count = as.data.frame(table(data_nonwalkers$M002))
names(M002_count) = c("Response", "Count")
M002_count

M002_plot <- ggplot(M002_count, aes(x=Response,y=Count)) + 
  geom_bar(stat="identity") + 
  ggtitle(attributes(data_nonwalkers)$variable.labels[10]) + 
  scale_y_continuous(limits=c(0,max(M002_count$Count)+1),breaks=pretty_breaks(), expand=c(0,0)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank())
M002_plot

# plot the answers to M001 - how long do your meetings usually last??
M003_count = as.data.frame(table(data_nonwalkers$M003))
names(M003_count) = c("Response", "Count")
M003_count

M003_plot <- ggplot(M003_count, aes(x=Response,y=Count)) + 
  geom_bar(stat="identity") + 
  ggtitle(attributes(data_nonwalkers)$variable.labels[11]) + 
  scale_y_continuous(limits=c(0,max(M003_count$Count)+1), breaks=pretty_breaks(), expand=c(0,0)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank())
M003_plot

##########
## TECHONOLOGY IN MEETINGS ##
##########

# tally and plot the answers to T001 - which tech do you bring to meetings?
tech_options = c("Notebook + pen/pencil","Laptop","Tablet","Smartpone", "Audio recording device","Nothing","Other")

T001_tally = c(sum(data_nonwalkers$T001_SQ001 == "Yes"),
               sum(data_nonwalkers$T001_SQ002 == "Yes"),
               sum(data_nonwalkers$T001_SQ003 == "Yes"),
               sum(data_nonwalkers$T001_SQ004 == "Yes"),
               sum(data_nonwalkers$T001_SQ005 == "Yes"),
               sum(data_nonwalkers$T001_SQ006 == "Yes"),
               sum(data_nonwalkers$T001_SQ007 == "Yes"))

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
note_options = c("No notes",
                 "Own notes",
                 "Minutes",
                 "Own notes + Minutes",
                 "Other")

T003_tally = c(sum(data_nonwalkers$T003_SQ001 == "Yes"),
               sum(data_nonwalkers$T003_SQ002 == "Yes"),
               sum(data_nonwalkers$T003_SQ003 == "Yes"),
               sum(data_nonwalkers$T003_SQ004 == "Yes"),
               sum(data_nonwalkers$T003_SQ005 == "Yes"))

T003_count <- data.frame("Notes" = note_options,
                         "Count" = T003_tally)
T003_count

T003_plot <- ggplot(T003_count, aes(x=reorder(Notes, -Count),y=Count)) + 
  geom_bar(stat="identity") + 
  ggtitle("Which of the following scenarios describe your note-taking habits during meetings?") + 
  scale_y_continuous(limits=c(0,max(T003_count$Count)+1), breaks=pretty_breaks(), expand=c(0,0)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank())
T003_plot

# tally and plot the answers to T004 - how do you normally take notes during a meeting?
note_tech = c("No Notes", "Handwritten","Laptop","Tablet","Smartpone", "Audio recording device","Other")

T004_tally = c(sum(data_nonwalkers$T004_SQ001 == "Yes"),
               sum(data_nonwalkers$T004_SQ002 == "Yes"),
               sum(data_nonwalkers$T004_SQ003 == "Yes"),
               sum(data_nonwalkers$T004_SQ004 == "Yes"),
               sum(data_nonwalkers$T004_SQ005 == "Yes"),
               sum(data_nonwalkers$T004_SQ006 == "Yes"),
               sum(data_nonwalkers$T004_SQ007 == "Yes"))

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
MM001_count = as.data.frame(table(data_nonwalkers$MM001))
names(MM001_count) = c("Response", "Count")
MM001_count

MM001_plot <- ggplot(MM001_count, aes(x=Response,y=Count)) + 
  geom_bar(stat="identity") + 
  ggtitle(attributes(data_nonwalkers)$variable.labels[40]) + 
  scale_y_continuous(limits=c(0,max(MM001_count$Count)+1),breaks=pretty_breaks(), expand=c(0,0)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank())
MM001_plot

# tally and plot the answers to WM002 - Which of the following are reasons that you do or would want to participate in walking meetings?
do_walk = c("Physical Activity",
            "Fresh Air",
            "I think better while I walk",
            "Better conversation flow",
            "Enjoyment",
            "Team dynamics",
            "Mental well-being",
            "Other")

WM002_tally = c(sum(data_nonwalkers$WM002_SQ001 == "Yes"),
                sum(data_nonwalkers$WM002_SQ002 == "Yes"),
                sum(data_nonwalkers$WM002_SQ003 == "Yes"),
                sum(data_nonwalkers$WM002_SQ004 == "Yes"),
                sum(data_nonwalkers$WM002_SQ005 == "Yes"),
                sum(data_nonwalkers$WM002_SQ006 == "Yes"),
                sum(data_nonwalkers$WM002_SQ007 == "Yes"),
                sum(data_nonwalkers$WM002_SQ008 == "Yes"))

WM002_count <- data.frame("DoWalk" = do_walk,
                          "Count" = WM002_tally)
WM002_count

WM002_plot <- ggplot(WM002_count, aes(x=reorder(DoWalk, -Count),y=Count)) + 
  geom_bar(stat="identity") + 
  ggtitle("Which of the following are reasons that you do or would want to participate in walking meetings?") + 
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
              "I would feel awkward asking",
              "Other")

MM002_tally = c(sum(data_nonwalkers$MM002_SQ001 == "Yes"),
                sum(data_nonwalkers$MM002_SQ002 == "Yes"),
                sum(data_nonwalkers$MM002_SQ003 == "Yes"),
                sum(data_nonwalkers$MM002_SQ004 == "Yes"),
                sum(data_nonwalkers$MM002_SQ005 == "Yes"),
                sum(data_nonwalkers$MM002_SQ006 == "Yes"),
                sum(data_nonwalkers$MM002_SQ007 == "Yes"),
                sum(data_nonwalkers$MM002_SQ008 == "Yes"))

MM002_count <- data.frame("DontWalk" = dont_walk,
                          "Count" = MM002_tally)
MM002_count

MM002_plot <- ggplot(MM002_count, aes(x=reorder(DontWalk, -Count),y=Count)) + 
  geom_bar(stat="identity") + 
  ggtitle("Which of the following are reasons that you do not participate in walking meetings?") + 
  scale_y_continuous(limits=c(0,max(MM002_count$Count)+1), breaks=pretty_breaks(), expand=c(0,0)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5), axis.title.y=element_blank()) +
  coord_flip()
MM002_plot

# tally and plot the answers to MM003 - which tech do would you bring to walking meetings?
tech_options = c("Notebook + pen/pencil","Laptop","Tablet","Smartpone", "Audio recording device","Nothing","Other")

MM003_tally = c(sum(data_nonwalkers$MM003_SQ001 == "Yes"),
                sum(data_nonwalkers$MM003_SQ002 == "Yes"),
                sum(data_nonwalkers$MM003_SQ003 == "Yes"),
                sum(data_nonwalkers$MM003_SQ004 == "Yes"),
                sum(data_nonwalkers$MM003_SQ005 == "Yes"),
                sum(data_nonwalkers$MM003_SQ007 == "Yes"),
                sum(data_nonwalkers$MM003_SQ006 == "Yes"))

MM003_count <- data.frame("Technology" = tech_options,
                          "Count" = MM003_tally)
MM003_count

MM003_plot <- ggplot(MM003_count, aes(x=reorder(Technology, -Count),y=Count)) + 
  geom_bar(stat="identity") + 
  ggtitle("If you were to participate in a walking meeting, which technologies would you be most likely to bring?") + 
  scale_y_continuous(limits=c(0,max(MM003_count$Count)+1), breaks=pretty_breaks(), expand=c(0,0)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank())
MM003_plot

# MM003b is written answers, no plots for now

# tally and plot the answers to MM004 - 	Which of the following individuals would you join on a walking meeting? 
walk_ppl = c("People who I supervise", "Coworkers","My boss","Coworkers I feel close with","Other")

MM004_tally = c(sum(data_nonwalkers$MM004_SQ001 == "Yes"),
                sum(data_nonwalkers$MM004_SQ002 == "Yes"),
                sum(data_nonwalkers$MM004_SQ003 == "Yes"),
                sum(data_nonwalkers$MM004_SQ004 == "Yes"),
                sum(data_nonwalkers$MM004_SQ005 == "Yes"))

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




#########
## Comparison

# plot the answers to M001 - how often do you participate in meetings?
MM001_count_walk = as.data.frame(table(data_walkers$MM001))
MM001_count_nowalk = as.data.frame(table(data_nonwalkers$MM001))
names(MM001_count_walk) = c("Response", "Count")
names(MM001_count_nowalk) = c("Response", "Count")
MM001_count_walk
MM001_count_nowalk
MM001_combo = data.frame("Response" = MM001_count_walk$Response,
                         "Count" = c(MM001_count_walk$Count, MM001_count_nowalk$Count),
                         "Condition" = c(rep("Experienced",length(MM001_count_walk$Response)), rep("Not Experienced",length(MM001_count_walk$Response))))
MM001_combo

MM001_plot <- ggplot(MM001_combo, aes(x=Response,y=Count,fill=Condition)) + 
  geom_bar(stat="identity", position="dodge") + 
  ggtitle(attributes(data_nonwalkers)$variable.labels[40]) + 
  scale_y_continuous(limits=c(0,max(MM001_combo$Count)+1),breaks=pretty_breaks(), expand=c(0,0)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank())
MM001_plot


# tally and plot the answers to WM002 - Which of the following are reasons that you do or would want to participate in walking meetings?
do_walk = c("Physical Activity",
            "Fresh Air",
            "I think better while I walk",
            "Better conversation flow",
            "Enjoyment",
            "Team dynamics",
            "Mental well-being")
WM002_tally_walk = c(100*sum(data_walkers$WM002_SQ001 == "Yes")/length(data_walkers$WM002_SQ001),
                     100*sum(data_walkers$WM002_SQ002 == "Yes")/length(data_walkers$WM002_SQ001),
                     100*sum(data_walkers$WM002_SQ003 == "Yes")/length(data_walkers$WM002_SQ001),
                     100*sum(data_walkers$WM002_SQ004 == "Yes")/length(data_walkers$WM002_SQ001),
                     100*sum(data_walkers$WM002_SQ005 == "Yes")/length(data_walkers$WM002_SQ001),
                     100*sum(data_walkers$WM002_SQ006 == "Yes")/length(data_walkers$WM002_SQ001),
                     100*sum(data_walkers$WM002_SQ007 == "Yes")/length(data_walkers$WM002_SQ001))
WM002_tally_nowalk = c(100*sum(data_nonwalkers$WM002_SQ001 == "Yes")/length(data_nonwalkers$WM002_SQ001),
                       100*sum(data_nonwalkers$WM002_SQ002 == "Yes")/length(data_nonwalkers$WM002_SQ001),
                       100*sum(data_nonwalkers$WM002_SQ003 == "Yes")/length(data_nonwalkers$WM002_SQ001),
                       100*sum(data_nonwalkers$WM002_SQ004 == "Yes")/length(data_nonwalkers$WM002_SQ001),
                       100*sum(data_nonwalkers$WM002_SQ005 == "Yes")/length(data_nonwalkers$WM002_SQ001),
                       100*sum(data_nonwalkers$WM002_SQ006 == "Yes")/length(data_nonwalkers$WM002_SQ001),
                       100*sum(data_nonwalkers$WM002_SQ007 == "Yes")/length(data_nonwalkers$WM002_SQ001))
WM002_tally = c(100*sum(data$WM002_SQ001 == "Yes")/length(data$WM002_SQ001),
                100*sum(data$WM002_SQ002 == "Yes")/length(data$WM002_SQ001),
                100*sum(data$WM002_SQ003 == "Yes")/length(data$WM002_SQ001),
                100*sum(data$WM002_SQ004 == "Yes")/length(data$WM002_SQ001),
                100*sum(data$WM002_SQ005 == "Yes")/length(data$WM002_SQ001),
                100*sum(data$WM002_SQ006 == "Yes")/length(data$WM002_SQ001),
                100*sum(data$WM002_SQ007 == "Yes")/length(data$WM002_SQ001))


# WM002_count <- data.frame("DoWalk" = do_walk,
#                           "Percentage" = c(WM002_tally_walk, WM002_tally_nowalk, WM002_tally),
#                           "Condition" = c(rep("Experienced",length(do_walk)),rep("Not Experienced",length(do_walk)),rep("All",length(do_walk))))
WM002_count <- data.frame("DoWalk" = do_walk,
                          "Percentage" = c(WM002_tally_walk, WM002_tally_nowalk),
                          "Condition" = c(rep("Experienced",length(do_walk)),rep("Not Experienced",length(do_walk))))
WM002_count

WM002_plot <- ggplot(WM002_count, aes(x=reorder(DoWalk, -Percentage),y=Percentage, fill=Condition)) + 
  geom_bar(stat="identity", position="dodge") + 
  #ggtitle("Motivations for Walking Meetings") + 
  scale_y_continuous(limits=c(0,max(WM002_count$Percentage)+1), breaks=pretty_breaks(), expand=c(0,0)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5, size=25), axis.title.y=element_blank(),text=element_text(size=25), legend.position="bottom") +
  coord_flip()
WM002_plot

t.test(data_walkers$WM002_SQ001 == "Yes", data_nonwalkers$WM002_SQ001 == "Yes")
t.test(data_walkers$WM002_SQ002 == "Yes", data_nonwalkers$WM002_SQ002 == "Yes")
t.test(data_walkers$WM002_SQ003 == "Yes", data_nonwalkers$WM002_SQ003 == "Yes")
t.test(as.numeric(data_walkers$WM002_SQ004 == "Yes"), as.numeric(data_nonwalkers$WM002_SQ004 == "Yes"))
t.test(data_walkers$WM002_SQ005 == "Yes", data_nonwalkers$WM002_SQ005 == "Yes")
t.test(data_walkers$WM002_SQ006 == "Yes", data_nonwalkers$WM002_SQ006 == "Yes")
t.test(data_walkers$WM002_SQ007 == "Yes", data_nonwalkers$WM002_SQ007 == "Yes")
# tally and plot the answers to MM002 - Which of the following are reasons that you do or would want to participate in walking meetings?
dont_walk = c("Difficulty taking notes",
              "Lack visuals",
              "Group size limitation",
              "Not part of my routine",
              "Bad weather",
              "Moving is distracting",
              "Feel awkward asking")



MM002_tally_walk = c(100*sum(data_walkers$MM002_SQ001 == "Yes")/length(data_walkers$MM002_SQ001),
                     100*sum(data_walkers$MM002_SQ002 == "Yes")/length(data_walkers$MM002_SQ001),
                     100*sum(data_walkers$MM002_SQ003 == "Yes")/length(data_walkers$MM002_SQ001),
                     100*sum(data_walkers$MM002_SQ004 == "Yes")/length(data_walkers$MM002_SQ001),
                     100*sum(data_walkers$MM002_SQ005 == "Yes")/length(data_walkers$MM002_SQ001),
                     100*sum(data_walkers$MM002_SQ006 == "Yes")/length(data_walkers$MM002_SQ001),
                     100*sum(data_walkers$MM002_SQ007 == "Yes")/length(data_walkers$MM002_SQ001))
MM002_tally_nowalk = c(100*sum(data_nonwalkers$MM002_SQ001 == "Yes")/length(data_nonwalkers$MM002_SQ001),
                       100*sum(data_nonwalkers$MM002_SQ002 == "Yes")/length(data_nonwalkers$MM002_SQ001),
                       100*sum(data_nonwalkers$MM002_SQ003 == "Yes")/length(data_nonwalkers$MM002_SQ001),
                       100*sum(data_nonwalkers$MM002_SQ004 == "Yes")/length(data_nonwalkers$MM002_SQ001),
                       100*sum(data_nonwalkers$MM002_SQ005 == "Yes")/length(data_nonwalkers$MM002_SQ001),
                       100*sum(data_nonwalkers$MM002_SQ006 == "Yes")/length(data_nonwalkers$MM002_SQ001),
                       100*sum(data_nonwalkers$MM002_SQ007 == "Yes")/length(data_nonwalkers$MM002_SQ001))


MM002_count <- data.frame("DontWalk" = dont_walk,
                          "Percentage" = c(MM002_tally_walk, MM002_tally_nowalk),
                          "Condition" = c(rep("Experienced",length(dont_walk)),rep("Not Experienced",length(dont_walk))))


MM002_count

MM002_plot <- ggplot(MM002_count, aes(x=reorder(DontWalk, -Percentage),y=Percentage, fill=Condition)) + 
  geom_bar(stat="identity", position="dodge") + 
  #ggtitle("Reasons Not to do Walking Meetings") + 
  scale_y_continuous(limits=c(0,max(MM002_count$Percentage)+1), breaks=pretty_breaks(), expand=c(0,0)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5, size=25), axis.title.y=element_blank(),text=element_text(size=25), legend.position="bottom") +
  coord_flip()
MM002_plot

t.test(data_walkers$MM002_SQ001 == "Yes", data_nonwalkers$MM002_SQ001 == "Yes")
t.test(data_walkers$MM002_SQ002 == "Yes", data_nonwalkers$MM002_SQ002 == "Yes")
t.test(data_walkers$MM002_SQ003 == "Yes", data_nonwalkers$MM002_SQ003 == "Yes")
t.test(data_walkers$MM002_SQ004 == "Yes", data_nonwalkers$MM002_SQ004 == "Yes")
t.test(data_walkers$MM002_SQ005 == "Yes", data_nonwalkers$MM002_SQ005 == "Yes")
t.test(data_walkers$MM002_SQ006 == "Yes", data_nonwalkers$MM002_SQ006 == "Yes")
t.test(data_walkers$MM002_SQ007 == "Yes", data_nonwalkers$MM002_SQ007 == "Yes")


# tally and plot the answers to T004 - how do you normally take notes during a meeting?
note_tech = c("No Notes", "Handwritten","Laptop","Tablet","Smartpone", "Audio recording device")


T004_tally_walk = c(100*sum(data_walkers$T004_SQ001 == "Yes")/length(data_walkers$T004_SQ001),
                    100*sum(data_walkers$T004_SQ002 == "Yes")/length(data_walkers$T004_SQ001),
                    100*sum(data_walkers$T004_SQ003 == "Yes")/length(data_walkers$T004_SQ001),
                    100*sum(data_walkers$T004_SQ004 == "Yes")/length(data_walkers$T004_SQ001),
                    100*sum(data_walkers$T004_SQ005 == "Yes")/length(data_walkers$T004_SQ001),
                    100*sum(data_walkers$T004_SQ006 == "Yes")/length(data_walkers$T004_SQ001))
T004_tally_nowalk = c(100*sum(data_nonwalkers$T004_SQ001 == "Yes")/length(data_nonwalkers$T004_SQ001),
                      100*sum(data_nonwalkers$T004_SQ002 == "Yes")/length(data_nonwalkers$T004_SQ001),
                      100*sum(data_nonwalkers$T004_SQ003 == "Yes")/length(data_nonwalkers$T004_SQ001),
                      100*sum(data_nonwalkers$T004_SQ004 == "Yes")/length(data_nonwalkers$T004_SQ001),
                      100*sum(data_nonwalkers$T004_SQ005 == "Yes")/length(data_nonwalkers$T004_SQ001),
                      100*sum(data_nonwalkers$T004_SQ006 == "Yes")/length(data_nonwalkers$T004_SQ001))


T004_count <- data.frame("NoteTech" = note_tech,
                         "Percentage" = c(T004_tally_walk, T004_tally_nowalk),
                         "Condition" = c(rep("Experienced",length(note_tech)),rep("Not Experienced",length(note_tech))))

T004_count

T004_plot <- ggplot(T004_count, aes(x=reorder(NoteTech, -Percentage),y=Percentage, fill=Condition)) + 
  geom_bar(stat="identity", position = "dodge") + 
  ggtitle("How do you usually take notes during a meeting?") + 
  scale_y_continuous(limits=c(0,max(T004_count$Percentage)+1), breaks=pretty_breaks(), expand=c(0,0)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5), axis.title.y=element_blank()) +
  coord_flip()
T004_plot


# Minutes and own notes

note_options = c("No notes",
                 "Own notes",
                 "Minutes",
                 "Own notes + Minutes")

T003_tally = c(sum(data$T003_SQ001 == "Yes"),
               sum(data$T003_SQ002 == "Yes"),
               sum(data$T003_SQ003 == "Yes"),
               sum(data$T003_SQ004 == "Yes"))


T003_tally_walk = c(100*sum(data_walkers$T003_SQ001 == "Yes")/length(data_walkers$T003_SQ001),
                    100*sum(data_walkers$T003_SQ002 == "Yes")/length(data_walkers$T003_SQ001),
                    100*sum(data_walkers$T003_SQ003 == "Yes")/length(data_walkers$T003_SQ001),
                    100*sum(data_walkers$T003_SQ004 == "Yes")/length(data_walkers$T003_SQ001))
T003_tally_nowalk = c(100*sum(data_nonwalkers$T003_SQ001 == "Yes")/length(data_nonwalkers$T003_SQ001),
                      100*sum(data_nonwalkers$T003_SQ002 == "Yes")/length(data_nonwalkers$T003_SQ001),
                      100*sum(data_nonwalkers$T003_SQ003 == "Yes")/length(data_nonwalkers$T003_SQ001),
                      100*sum(data_nonwalkers$T003_SQ004 == "Yes")/length(data_nonwalkers$T003_SQ001))


T003_count <- data.frame("NoteOptions" = note_options,
                         "Percentage" = c(T003_tally_walk, T003_tally_nowalk),
                         "Condition" = c(rep("Experienced",length(note_options)),rep("Not Experienced",length(note_options))))

T003_count

T003_plot <- ggplot(T003_count, aes(x=reorder(NoteOptions, -Percentage),y=Percentage, fill=Condition)) + 
  geom_bar(stat="identity", position = "dodge") + 
  #ggtitle("Note-taking Habits") + 
  scale_y_continuous(limits=c(0,max(T003_count$Percentage)+1), breaks=pretty_breaks(), expand=c(0,0)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5), axis.title.y=element_blank(), text=element_text(size=25), legend.position="bottom") +
  coord_flip()
T003_plot


# with whom

# tally and plot the answers to MM004 - 	Which of the following individuals would you join on a walking meeting? 
walk_ppl = c("People who I supervise", "Coworkers","My boss","Coworkers I feel close with")

MM004_tally_walk = c(100*sum(data_walkers$MM004_SQ001 == "Yes")/length(data_walkers$MM004_SQ001),
                     100*sum(data_walkers$MM004_SQ002 == "Yes")/length(data_walkers$MM004_SQ001),
                     100*sum(data_walkers$MM004_SQ003 == "Yes")/length(data_walkers$MM004_SQ001),
                     100*sum(data_walkers$MM004_SQ004 == "Yes")/length(data_walkers$MM004_SQ001))
MM004_tally_nowalk = c(100*sum(data_nonwalkers$MM004_SQ001 == "Yes")/length(data_nonwalkers$MM004_SQ001),
                       100*sum(data_nonwalkers$MM004_SQ002 == "Yes")/length(data_nonwalkers$MM004_SQ001),
                       100*sum(data_nonwalkers$MM004_SQ003 == "Yes")/length(data_nonwalkers$MM004_SQ001),
                       100*sum(data_nonwalkers$MM004_SQ004 == "Yes")/length(data_nonwalkers$MM004_SQ001))


MM004_count <- data.frame("Walkppl" = walk_ppl,
                          "Percentage" = c(MM004_tally_walk, MM004_tally_nowalk),
                          "Condition" = c(rep("Experienced",length(walk_ppl)),rep("Not Experienced",length(walk_ppl))))

MM004_count

MM004_plot <- ggplot(MM004_count, aes(x=reorder(Walkppl, -Percentage),y=Percentage, fill=Condition)) + 
  geom_bar(stat="identity", position = "dodge") + 
  #ggtitle("With Whom") + 
  scale_y_continuous(limits=c(0,max(MM004_count$Percentage)+1), breaks=pretty_breaks(), expand=c(0,0)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5), axis.title.y=element_blank(), text=element_text(size=25), legend.position="bottom") +
  coord_flip()
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