library('tidyverse')
library('scales')
library('ggsignif')
library(ggplot2)
library(ggpubr)

df <- read.csv('./design_fiction_data.csv', header=TRUE)

#First some pre-processing
df$Q21_num[df$Q21 == "I have never participated in a walking meeting"] <- 0
df$Q21_num[df$Q21 == "I have participated in several walking meetings, but they are not a regular part of my routine"] <- 1
df$Q21_num[df$Q21 == "Walking meetings are a regular part of my routine"] <- 2


# demographic info

N = length(df$Q2)

age_mean <- mean(df$Q18)
sd_age <- sd(df$Q18)
ggdensity(df$Q18)

# gender split
female_count = sum(df$Q19 == "Female")
female_perc = 100*female_count/N
male_count = N-female_count
male_pec = 100-female_perc

# country split
usa_count = sum(df$Q20 == "USA")
usa_perc = 100*usa_count/N

# walking meeting experience
mean_walk_exp = mean(df$Q21_num)
exp_levels <- c("None", "Participated, but not regularly", "Regular part of routine")
exp_counts <- c(sum(df$Q21_num == 0), sum(df$Q21_num == 1), sum(df$Q21_num == 2))
walk_exp <- data.frame("Experience" = exp_levels,
                       "Count" = exp_counts)
walk_exp_plot <- ggplot(data=walk_exp, aes(x=Experience, y = Count)) +
  geom_bar(alpha=0.6, position = 'dodge', stat = "Identity") + 
  ggtitle("Level of Experience with Walking Meetings") + 
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank(), legend.position = "none") +
  scale_y_continuous(limits=c(0,45), breaks=pretty_breaks(), expand=c(0,0))
  
walk_exp_plot


# define the conditions

categories = c("Walking Meeting",
               "Remote Walking Meeting",
               "Walking Lecture",
               "Remote Walking Lecture")

df$meet_cond <- rep(0,N)
df$meet_cond[df$Q70 == "I have read the scenario above"] <- categories[1]
df$meet_cond[df$Q69 == "I have read the scenario above"] <- categories[2]

df$lect_cond <- rep(0,N)
df$lect_cond[df$Q71 == "I have read the scenario above"] <- categories[3]
df$lect_cond[df$Q72 == "I have read the scenario above"] <- categories[4]

df$double_cond <- rep(0,N)
df$double_cond[df$meet_cond == categories[1] & df$lect_cond == categories[3]] <- "Present|Present"
df$double_cond[df$meet_cond == categories[2] & df$lect_cond == categories[3]] <- "Remote|Present"
df$double_cond[df$meet_cond == categories[1] & df$lect_cond == categories[4]] <- "Present|Remote"
df$double_cond[df$meet_cond == categories[2] & df$lect_cond == categories[4]] <- "Remote|Remote"

a <- ggplot(data=df, aes(x=double_cond)) +
  geom_bar( color="#e9ecef", alpha=0.6, position = 'dodge')
a

a <- ggplot(data=df, aes(x=meet_cond)) +
  geom_bar( color="#e9ecef", alpha=0.6, position = 'dodge')
a

a <- ggplot(data=df, aes(x=lect_cond)) +
  geom_bar( color="#e9ecef", alpha=0.6, position = 'dodge')
a



#PANAS meetings
df$pos_panas_meet <- rep(0,N)
df$neg_panas_meet <- rep(0,N)
df$pos_panas_meet[df$meet_cond == categories[1]] <-
                                      df$Q7_1[df$meet_cond == categories[1]] +
                                      df$Q7_3[df$meet_cond == categories[1]] +
                                      df$Q7_5[df$meet_cond == categories[1]] +
                                      df$Q7_7[df$meet_cond == categories[1]] +
                                      df$Q7_9[df$meet_cond == categories[1]]
df$neg_panas_meet[df$meet_cond == categories[1]] <-
                                      df$Q7_2[df$meet_cond == categories[1]] +
                                      df$Q7_4[df$meet_cond == categories[1]] +
                                      df$Q7_6[df$meet_cond == categories[1]] +
                                      df$Q7_8[df$meet_cond == categories[1]] +
                                      df$Q7_10[df$meet_cond == categories[1]]

df$pos_panas_meet[df$meet_cond == categories[2]] <-
                                      df$Q41_1[df$meet_cond == categories[2]] +
                                      df$Q41_3[df$meet_cond == categories[2]] +
                                      df$Q41_5[df$meet_cond == categories[2]] +
                                      df$Q41_7[df$meet_cond == categories[2]] +
                                      df$Q41_9[df$meet_cond == categories[2]]
df$neg_panas_meet[df$meet_cond == categories[2]] <-
                                      df$Q41_2[df$meet_cond == categories[2]] +
                                      df$Q41_4[df$meet_cond == categories[2]] +
                                      df$Q41_6[df$meet_cond == categories[2]] +
                                      df$Q41_8[df$meet_cond == categories[2]] +
                                      df$Q41_10[df$meet_cond == categories[2]]

pos_panas_meet_present_mean <- mean(df$pos_panas_meet[df$meet_cond == categories[1]])
neg_panas_meet_present_mean <- mean(df$neg_panas_meet[df$meet_cond == categories[1]])
pos_panas_meet_remote_mean <- mean(df$pos_panas_meet[df$meet_cond == categories[2]])
neg_panas_meet_remote_mean <- mean(df$neg_panas_meet[df$meet_cond == categories[2]])


# PANAS lectures
df$pos_panas_lect <- rep(0,N)
df$neg_panas_lect <- rep(0,N)
df$pos_panas_lect[df$lect_cond == categories[3]] <-
  df$Q25_1[df$lect_cond == categories[3]] +
  df$Q25_3[df$lect_cond == categories[3]] +
  df$Q25_5[df$lect_cond == categories[3]] +
  df$Q25_7[df$lect_cond == categories[3]] +
  df$Q25_9[df$lect_cond == categories[3]]
df$neg_panas_lect[df$lect_cond == categories[3]] <-
  df$Q25_2[df$lect_cond == categories[3]] +
  df$Q25_4[df$lect_cond == categories[3]] +
  df$Q25_6[df$lect_cond == categories[3]] +
  df$Q25_8[df$lect_cond == categories[3]] +
  df$Q25_10[df$lect_cond == categories[3]]

df$pos_panas_lect[df$lect_cond == categories[4]] <-
  df$Q56_1[df$lect_cond == categories[4]] +
  df$Q56_3[df$lect_cond == categories[4]] +
  df$Q56_5[df$lect_cond == categories[4]] +
  df$Q56_7[df$lect_cond == categories[4]] +
  df$Q56_9[df$lect_cond == categories[4]]
df$neg_panas_lect[df$lect_cond == categories[4]] <-
  df$Q56_2[df$lect_cond == categories[4]] +
  df$Q56_4[df$lect_cond == categories[4]] +
  df$Q56_6[df$lect_cond == categories[4]] +
  df$Q56_8[df$lect_cond == categories[4]] +
  df$Q56_10[df$lect_cond == categories[4]]

pos_panas_lect_present_mean <- mean(df$pos_panas_lect[df$lect_cond == categories[3]])
neg_panas_lect_present_mean <- mean(df$neg_panas_lect[df$lect_cond == categories[3]])
pos_panas_lect_remote_mean <- mean(df$pos_panas_lect[df$lect_cond == categories[4]])
neg_panas_lect_remote_mean <- mean(df$neg_panas_lect[df$lect_cond == categories[4]])

pos_panas_meet_present_mean
pos_panas_meet_remote_mean
pos_panas_lect_present_mean
pos_panas_lect_remote_mean

neg_panas_meet_present_mean
neg_panas_meet_remote_mean
neg_panas_lect_present_mean
neg_panas_lect_remote_mean

panas <- data.frame("condition" = rep(0,2*N),
                    "Positive" = rep(0,2*N),
                    "Negative" = rep(0,2*N))
panas$condition <- c(df$meet_cond, df$lect_cond)
panas$Positive <- c(df$pos_panas_meet, df$pos_panas_lect)
panas$Negative <- c(df$neg_panas_meet, df$neg_panas_lect)

panas_pos_box <- ggplot(panas, aes(x = condition, y = Positive, color = condition)) +
  geom_boxplot() + 
  scale_x_discrete(name = "Condition") + 
  scale_y_continuous(name = "Positive Affect",
                     breaks = seq(0,30,1),
                     limits = c(0,30)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank()) +
  ggtitle("Positive Affect")
panas_pos_box

panas_neg_box <- ggplot(panas, aes(x = condition, y = Negative, color = condition)) +
  geom_boxplot() + 
  scale_x_discrete(name = "Condition") + 
  scale_y_continuous(name = "Positive Affect",
                     breaks = seq(0,30,1),
                     limits = c(0,30)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank()) +
  ggtitle("Negative Affect")
panas_neg_box


panas_cat <- c("Active", "Afraid", "Alert", "Ashamed", "Attentive", "Hostile", "Determined", "Nervous", "Inspired", "Upset")

# df$panas_meet_active[df$meet_cond == categories[1]] <- df$Q7_1[df$meet_cond == categories[1]]
# df$panas_meet_afraid[df$meet_cond == categories[1]] <- df$Q7_2[df$meet_cond == categories[1]]
# df$panas_meet_alert[df$meet_cond == categories[1]] <- df$Q7_3[df$meet_cond == categories[1]]
# df$panas_meet_ashamed[df$meet_cond == categories[1]] <- df$Q7_4[df$meet_cond == categories[1]]
# df$panas_meet_attentive[df$meet_cond == categories[1]] <- df$Q7_5[df$meet_cond == categories[1]]
# df$panas_meet_hostile[df$meet_cond == categories[1]] <- df$Q7_6[df$meet_cond == categories[1]]
# df$panas_meet_determined[df$meet_cond == categories[1]] <- df$Q7_7[df$meet_cond == categories[1]]
# df$panas_meet_nervous[df$meet_cond == categories[1]] <- df$Q7_8[df$meet_cond == categories[1]]
# df$panas_meet_inspired[df$meet_cond == categories[1]] <- df$Q7_9[df$meet_cond == categories[1]]
# df$panas_meet_upset[df$meet_cond == categories[1]] <- df$Q7_10[df$meet_cond == categories[1]]
# 
# 
# df$panas_meet_active[df$meet_cond == categories[2]] <- df$Q41_1[df$meet_cond == categories[2]]
# df$panas_meet_afraid[df$meet_cond == categories[2]] <- df$Q41_2[df$meet_cond == categories[2]]
# df$panas_meet_alert[df$meet_cond == categories[2]] <- df$Q41_3[df$meet_cond == categories[2]]
# df$panas_meet_ashamed[df$meet_cond == categories[2]] <- df$Q41_4[df$meet_cond == categories[2]]
# df$panas_meet_attentive[df$meet_cond == categories[2]] <- df$Q41_5[df$meet_cond == categories[2]]
# df$panas_meet_hostile[df$meet_cond == categories[2]] <- df$Q41_6[df$meet_cond == categories[2]]
# df$panas_meet_determined[df$meet_cond == categories[2]] <- df$Q41_7[df$meet_cond == categories[2]]
# df$panas_meet_nervous[df$meet_cond == categories[2]] <- df$Q41_8[df$meet_cond == categories[2]]
# df$panas_meet_inspired[df$meet_cond == categories[2]] <- df$Q41_9[df$meet_cond == categories[2]]
# df$panas_meet_upset[df$meet_cond == categories[2]] <- df$Q41_10[df$meet_cond == categories[2]]
# 
# df$panas_lect_active[df$lect_cond == categories[3]] <- df$Q25_1[df$lect_cond == categories[3]]
# df$panas_lect_afraid[df$lect_cond == categories[3]] <- df$Q25_2[df$lect_cond == categories[3]]
# df$panas_lect_alert[df$lect_cond == categories[3]] <- df$Q25_3[df$lect_cond == categories[3]]
# df$panas_lect_ashamed[df$lect_cond == categories[3]] <- df$Q25_4[df$lect_cond == categories[3]]
# df$panas_lect_attentive[df$lect_cond == categories[3]] <- df$Q25_5[df$lect_cond == categories[3]]
# df$panas_lect_hostile[df$lect_cond == categories[3]] <- df$Q25_6[df$lect_cond == categories[3]]
# df$panas_lect_determined[df$lect_cond == categories[3]] <- df$Q25_7[df$lect_cond == categories[3]]
# df$panas_lect_nervous[df$lect_cond == categories[3]] <- df$Q25_8[df$lect_cond == categories[3]]
# df$panas_lect_inspired[df$lect_cond == categories[3]] <- df$Q25_9[df$lect_cond == categories[3]]
# df$panas_lect_upset[df$lect_cond == categories[3]] <- df$Q25_10[df$lect_cond == categories[3]]
# 
# df$panas_lect_active[df$lect_cond == categories[4]] <- df$Q56_1[df$lect_cond == categories[4]]
# df$panas_lect_afraid[df$lect_cond == categories[4]] <- df$Q56_2[df$lect_cond == categories[4]]
# df$panas_lect_alert[df$lect_cond == categories[4]] <- df$Q56_3[df$lect_cond == categories[4]]
# df$panas_lect_ashamed[df$lect_cond == categories[4]] <- df$Q56_4[df$lect_cond == categories[4]]
# df$panas_lect_attentive[df$lect_cond == categories[4]] <- df$Q56_5[df$lect_cond == categories[4]]
# df$panas_lect_hostile[df$lect_cond == categories[4]] <- df$Q56_6[df$lect_cond == categories[4]]
# df$panas_lect_determined[df$lect_cond == categories[4]] <- df$Q56_7[df$lect_cond == categories[4]]
# df$panas_lect_nervous[df$lect_cond == categories[4]] <- df$Q56_8[df$lect_cond == categories[4]]
# df$panas_lect_inspired[df$lect_cond == categories[4]] <- df$Q56_9[df$lect_cond == categories[4]]
# df$panas_lect_upset[df$lect_cond == categories[4]] <- df$Q56_10[df$lect_cond == categories[4]]
# 
# panas$active <- c(df$panas_meet_active, df$panas_lect_active)
# panas$afraid <- c(df$panas_meet_afraid, df$panas_lect_afraid)
# panas$alert <- c(df$panas_meet_alert, df$panas_lect_alert)
# panas$ashamed <- c(df$panas_meet_ashamed, df$panas_lect_ashamed)
# panas$attentive <- c(df$panas_meet_attentive, df$panas_lect_attentive)
# panas$hostile <- c(df$panas_meet_hostile, df$panas_lect_hostile)
# panas$determined <- c(df$panas_meet_determined, df$panas_lect_determined)
# panas$nervous <- c(df$panas_meet_nervous, df$panas_lect_nervous)
# panas$inspired <- c(df$panas_meet_inspired, df$panas_lect_inspired)
# panas$upset <- c(df$panas_meet_upset, df$panas_lect_upset)

panas_meet_present_active <- mean(df$Q7_1[df$meet_cond == categories[1]])
panas_meet_present_afraid <- mean(df$Q7_2[df$meet_cond == categories[1]])
panas_meet_present_alert <- mean(df$Q7_3[df$meet_cond == categories[1]])
panas_meet_present_ashamed <- mean(df$Q7_4[df$meet_cond == categories[1]])
panas_meet_present_attentive <- mean(df$Q7_5[df$meet_cond == categories[1]])
panas_meet_present_hostile <- mean(df$Q7_6[df$meet_cond == categories[1]])
panas_meet_present_determined <- mean(df$Q7_7[df$meet_cond == categories[1]])
panas_meet_present_nervous <- mean(df$Q7_8[df$meet_cond == categories[1]])
panas_meet_present_inspired <- mean(df$Q7_9[df$meet_cond == categories[1]])
panas_meet_present_upset <- mean(df$Q7_10[df$meet_cond == categories[1]])

panas_meet_present_cats = c(panas_meet_present_active,
                            panas_meet_present_afraid,
                            panas_meet_present_alert,
                            panas_meet_present_ashamed,
                            panas_meet_present_attentive,
                            panas_meet_present_hostile,
                            panas_meet_present_determined,
                            panas_meet_present_nervous,
                            panas_meet_present_inspired,
                            panas_meet_present_upset)

panas_meet_remote_active <- mean(df$Q41_1[df$meet_cond == categories[2]])
panas_meet_remote_afraid <- mean(df$Q41_2[df$meet_cond == categories[2]])
panas_meet_remote_alert <- mean(df$Q41_3[df$meet_cond == categories[2]])
panas_meet_remote_ashamed <- mean(df$Q41_4[df$meet_cond == categories[2]])
panas_meet_remote_attentive <- mean(df$Q41_5[df$meet_cond == categories[2]])
panas_meet_remote_hostile <- mean(df$Q41_6[df$meet_cond == categories[2]])
panas_meet_remote_determined <- mean(df$Q41_7[df$meet_cond == categories[2]])
panas_meet_remote_nervous <- mean(df$Q41_8[df$meet_cond == categories[2]])
panas_meet_remote_inspired <- mean(df$Q41_9[df$meet_cond == categories[2]])
panas_meet_remote_upset <- mean(df$Q41_10[df$meet_cond == categories[2]])

panas_meet_remote_cats = c(panas_meet_remote_active,
                            panas_meet_remote_afraid,
                            panas_meet_remote_alert,
                            panas_meet_remote_ashamed,
                            panas_meet_remote_attentive,
                            panas_meet_remote_hostile,
                            panas_meet_remote_determined,
                            panas_meet_remote_nervous,
                            panas_meet_remote_inspired,
                            panas_meet_remote_upset)

panas_lect_present_active <- mean(df$Q25_1[df$lect_cond == categories[3]])
panas_lect_present_afraid <- mean(df$Q25_2[df$lect_cond == categories[3]])
panas_lect_present_alert <- mean(df$Q25_3[df$lect_cond == categories[3]])
panas_lect_present_ashamed <- mean(df$Q25_4[df$lect_cond == categories[3]])
panas_lect_present_attentive <- mean(df$Q25_5[df$lect_cond == categories[3]])
panas_lect_present_hostile <- mean(df$Q25_6[df$lect_cond == categories[3]])
panas_lect_present_determined <- mean(df$Q25_7[df$lect_cond == categories[3]])
panas_lect_present_nervous <- mean(df$Q25_8[df$lect_cond == categories[3]])
panas_lect_present_inspired <- mean(df$Q25_9[df$lect_cond == categories[3]])
panas_lect_present_upset <- mean(df$Q25_10[df$lect_cond == categories[3]])

panas_lect_present_cats = c(panas_lect_present_active,
                            panas_lect_present_afraid,
                            panas_lect_present_alert,
                            panas_lect_present_ashamed,
                            panas_lect_present_attentive,
                            panas_lect_present_hostile,
                            panas_lect_present_determined,
                            panas_lect_present_nervous,
                            panas_lect_present_inspired,
                            panas_lect_present_upset)

panas_lect_remote_active <- mean(df$Q56_1[df$lect_cond == categories[4]])
panas_lect_remote_afraid <- mean(df$Q56_2[df$lect_cond == categories[4]])
panas_lect_remote_alert <- mean(df$Q56_3[df$lect_cond == categories[4]])
panas_lect_remote_ashamed <- mean(df$Q56_4[df$lect_cond == categories[4]])
panas_lect_remote_attentive <- mean(df$Q56_5[df$lect_cond == categories[4]])
panas_lect_remote_hostile <- mean(df$Q56_6[df$lect_cond == categories[4]])
panas_lect_remote_determined <- mean(df$Q56_7[df$lect_cond == categories[4]])
panas_lect_remote_nervous <- mean(df$Q56_8[df$lect_cond == categories[4]])
panas_lect_remote_inspired <- mean(df$Q56_9[df$lect_cond == categories[4]])
panas_lect_remote_upset <- mean(df$Q56_10[df$lect_cond == categories[4]])

panas_lect_remote_cats = c(panas_lect_remote_active,
                           panas_lect_remote_afraid,
                           panas_lect_remote_alert,
                           panas_lect_remote_ashamed,
                           panas_lect_remote_attentive,
                           panas_lect_remote_hostile,
                           panas_lect_remote_determined,
                           panas_lect_remote_nervous,
                           panas_lect_remote_inspired,
                           panas_lect_remote_upset)


panas_means <- data.frame("Affects" = panas_cat,
                          "Present Meeting" = panas_meet_present_cats,
                          "Remote Meeing" = panas_meet_remote_cats,
                          "Present Lecture" = panas_lect_present_cats,
                          "Remote Lecture" = panas_lect_remote_cats)

panas_means_plot <- ggplot(data=panas_means, aes(x=Affects, y = Present.Meeting, group=1)) +
  geom_line() +
  geom_point()

panas_means_plot

panas_means_combined <- data.frame("Condition" = c(rep("Present Meeting",10), rep("Remote Meeting",10), rep("Present Lecture",10), rep("Remote Lecture",10)),
                                  "Affect" = c(panas_cat, panas_cat, panas_cat, panas_cat),
                                  "Mean Score" = c(panas_meet_present_cats, panas_meet_remote_cats, panas_lect_present_cats, panas_lect_remote_cats))

panas_means_plot <- ggplot(data=panas_means_combined, aes(x=Affect, y = Mean.Score, group=Condition, color=Condition)) +
  geom_line(aes(linetype=Condition), size=1) +
  geom_point(aes(shape=Condition), size=4) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank()) +
  ggtitle("Affect Profile") + 
  coord_flip()
 

panas_means_plot


# Would you use a system like this
df$Q10_num[df$Q10 == "Yes"] <- 1
df$Q10_num[df$Q10 == "No"] <- 0
df$Q44_num[df$Q44 == "Yes"] <- 1
df$Q44_num[df$Q44 == "No"] <- 0
df$Q28_num[df$Q28 == "Yes"] <- 1
df$Q28_num[df$Q28 == "No"] <- 0
df$Q59_num[df$Q59 == "Yes"] <- 1
df$Q59_num[df$Q59 == "No"] <- 0

df$wouldyou_meet[df$meet_cond == categories[1]] <- df$Q10_num[df$meet_cond == categories[1]]
df$wouldyou_meet[df$meet_cond == categories[2]] <- df$Q44_num[df$meet_cond == categories[2]]
df$wouldyou_lect[df$lect_cond == categories[3]] <- df$Q28_num[df$lect_cond == categories[3]]
df$wouldyou_lect[df$lect_cond == categories[4]] <- df$Q59_num[df$lect_cond == categories[4]]

wouldyou_meet_present_mean <- mean(df$wouldyou_meet[df$meet_cond == categories[1]])
wouldyou_meet_remote_mean <- mean(df$wouldyou_meet[df$meet_cond == categories[2]])
wouldyou_lect_present_mean <- mean(df$wouldyou_lect[df$lect_cond == categories[3]])
wouldyou_lect_remote_mean <-mean(df$wouldyou_lect[df$lect_cond == categories[4]])

panas$wouldyou <- c(df$wouldyou_meet, df$wouldyou_lect)

barplot(table(df$wouldyou_meet[df$meet_cond == categories[1]]), ylim = c(0,40))
barplot(table(df$wouldyou_meet[df$meet_cond == categories[2]]), ylim = c(0,40))
barplot(table(df$wouldyou_lect[df$lect_cond == categories[3]]), ylim = c(0,40))
barplot(table(df$wouldyou_lect[df$lect_cond == categories[4]]), ylim = c(0,40))

#combine
wouldyou_meet_present <- 100*sum(df$wouldyou_meet[df$meet_cond == categories[1]])/sum(df$meet_cond == categories[1])
wouldyou_meet_remote <-100*sum(df$wouldyou_meet[df$meet_cond == categories[2]])/sum(df$meet_cond == categories[2])
wouldyou_lect_present <- 100*sum(df$wouldyou_lect[df$lect_cond == categories[3]])/sum(df$lect_cond == categories[3])
wouldyou_lect_remote <- 100*sum(df$wouldyou_lect[df$lect_cond == categories[4]])/sum(df$lect_cond == categories[4])
wouldyou_combo <- data.frame("Count" = c(wouldyou_meet_present, wouldyou_meet_remote, wouldyou_lect_present, wouldyou_lect_remote),
                             "Condition" = c("Present Meeting", "Remote Meeting", "Present Lecture", "Remote Lecture"))

wouldyou_lect_plot <- ggplot(wouldyou_combo, aes(x=Condition, y=Count)) + 
  geom_bar(stat="identity", alpha = 0.6) + 
  ggtitle("Would you use this system (% Affirmative)") + 
  theme_classic() +
  scale_y_continuous(limits=c(0,100), breaks=pretty_breaks(), expand=c(0,0)) +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank(), legend.position="none")
wouldyou_lect_plot

# how often
# present meeting Q12, remote meeting Q46, present lect Q30, remote lect Q61
df$often_meet[df$meet_cond == categories[1]] <- as.character(df$Q12[df$meet_cond == categories[1]])
df$often_meet[df$meet_cond == categories[2]] <- as.character(df$Q46[df$meet_cond == categories[2]])
df$often_lect[df$lect_cond == categories[3]] <- as.character(df$Q30[df$lect_cond == categories[3]])
df$often_lect[df$lect_cond == categories[4]] <- as.character(df$Q61[df$lect_cond == categories[4]])

panas$often <- c(df$often_meet, df$often_lect)

barplot(table(df$often_meet[df$meet_cond == categories[1]]), ylim = c(0,25))
barplot(table(df$often_meet[df$meet_cond == categories[2]]), ylim = c(0,25))
barplot(table(df$often_lect[df$lect_cond == categories[3]]), ylim = c(0,25))
barplot(table(df$often_lect[df$lect_cond == categories[4]]), ylim = c(0,25))


# perceived effectiveness
#walking 35
#remote 66
#lecfture 23
#remote lexture 51

df$eff_own_meet <- rep(0,N)
df$eff_own_meet[!is.na(df$Q35_1)] <-  df$Q35_1[!is.na(df$Q35_1)]
df$eff_own_meet[!is.na(df$Q66_1)] <-  df$Q66_1[!is.na(df$Q66_1)]

df$eff_partner_meet <- rep(0,N)
df$eff_partner_meet[!is.na(df$Q35_2)] <-  df$Q35_2[!is.na(df$Q35_2)]
df$eff_partner_meet[!is.na(df$Q66_2)] <-  df$Q66_2[!is.na(df$Q66_2)]

df$eff_company_meet <- rep(0,N)
df$eff_company_meet[!is.na(df$Q35_3)] <-  df$Q35_3[!is.na(df$Q35_3)]
df$eff_company_meet[!is.na(df$Q66_3)] <-  df$Q66_3[!is.na(df$Q66_3)]

eff_own_meet_mean <- mean(df$eff_own_meet)
eff_partner_meet_mean <- mean(df$eff_partner_meet)
eff_company_meet_mean <- mean(df$eff_company_meet)

eff_own_meet_present_mean <- mean(df$eff_own_meet[df$meet_cond == categories[1]])
eff_partner_meet_present_mean <- mean(df$eff_partner_meet[df$meet_cond == categories[1]])
eff_company_meet_present_mean <- mean(df$eff_company_meet[df$meet_cond == categories[1]])
eff_own_meet_remote_mean <- mean(df$eff_own_meet[df$meet_cond == categories[2]])
eff_partner_meet_remote_mean <- mean(df$eff_partner_meet[df$meet_cond == categories[2]])
eff_company_meet_remote_mean <- mean(df$eff_company_meet[df$meet_cond == categories[2]])


  
df$eff_own_lect <- rep(0,N)
df$eff_own_lect[!is.na(df$Q23_1)] <-  df$Q23_1[!is.na(df$Q23_1)]
df$eff_own_lect[!is.na(df$Q51_1)] <-  df$Q51_1[!is.na(df$Q51_1)]

df$eff_classmates_lect <- rep(0,N)
df$eff_classmates_lect[!is.na(df$Q23_2)] <-  df$Q23_2[!is.na(df$Q23_2)]
df$eff_classmates_lect[!is.na(df$Q51_2)] <-  df$Q51_2[!is.na(df$Q51_2)]

df$eff_instructor_lect <- rep(0,N)
df$eff_instructor_lect[!is.na(df$Q23_3)] <-  df$Q23_3[!is.na(df$Q23_3)]
df$eff_instructor_lect[!is.na(df$Q51_3)] <-  df$Q51_3[!is.na(df$Q51_3)]

eff_own_lect_mean <- mean(df$eff_own_lect)
eff_classmates_lect_mean <- mean(df$eff_classmates_lect)
eff_instructor_lect_mean <- mean(df$eff_instructor_lect)

eff_own_lect_present_mean <- mean(df$eff_own_lect[df$lect_cond == categories[3]])
eff_partner_lect_present_mean <- mean(df$eff_classmates_lect[df$lect_cond == categories[3]])
eff_company_lect_present_mean <- mean(df$eff_instructor_lect[df$lect_cond == categories[3]])
eff_own_lect_remote_mean <- mean(df$eff_own_lect[df$lect_cond == categories[4]])
eff_partner_lect_remote_mean <- mean(df$eff_classmates_lect[df$lect_cond == categories[4]])
eff_company_lect_remote_mean <- mean(df$eff_instructor_lect[df$lect_cond == categories[4]])


eff_own_meet_mean
eff_partner_meet_mean
eff_company_meet_mean
eff_own_lect_mean
eff_classmates_lect_mean
eff_instructor_lect_mean

eff_own_meet_present_mean
eff_partner_meet_present_mean
eff_company_meet_present_mean
eff_own_meet_remote_mean
eff_partner_meet_remote_mean
eff_company_meet_remote_mean

eff_own_lect_present_mean
eff_partner_lect_present_mean
eff_company_lect_present_mean
eff_own_lect_remote_mean
eff_partner_lect_remote_mean
eff_company_lect_remote_mean

# RSME (mental effort)
#walking 17
#remote 52
#lecfture 36
#remote lexture 67

df$rsme_meet <- rep(0,N)
df$rsme_meet[!is.na(df$Q17)] <-  df$Q17[!is.na(df$Q17)]
df$rsme_meet[!is.na(df$Q52)] <-  df$Q52[!is.na(df$Q52)]
rsme_meet_mean <- mean(df$rsme_meet)
rsme_meet_present_mean <- mean(df$rsme_meet[df$meet_cond == categories[1]])
rsme_meet_remote_mean <- mean(df$rsme_meet[df$meet_cond == categories[2]])

df$rsme_lect <- rep(0,N)
df$rsme_lect[!is.na(df$Q36)] <-  df$Q36[!is.na(df$Q36)]
df$rsme_lect[!is.na(df$Q67)] <-  df$Q67[!is.na(df$Q67)]
rsme_lect_mean <- mean(df$rsme_lect)
rsme_lect_present_mean <- mean(df$rsme_meet[df$lect_cond == categories[3]])
rsme_lect_remote_mean <- mean(df$rsme_meet[df$lect_cond == categories[4]])

rsme_meet_mean
rsme_lect_mean
rsme_meet_present_mean
rsme_meet_remote_mean
rsme_lect_present_mean
rsme_lect_remote_mean


panas$rsme <- c(df$rsme_meet, df$rsme_lect)


rsme_box <- ggplot(panas, aes(x = condition, y = rsme, color = condition)) +
  geom_boxplot() + 
  scale_x_discrete(name = "Condition") + 
  scale_y_continuous(name = "Positive Affect",
                     breaks = seq(0,150,5),
                     limits = c(0,150)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank()) +
  ggtitle("RSME")
rsme_box
# none are significant

##################
# 
# ### Positive and negative aspects
# #present meetings
# meet_present_count <- length(df$meet_cond[df$meet_cond==categories[1]])
# aspects_meet_present <- data.frame("Positive" = rep(0,meet_present_count*3),
#                                    "Negative" = rep(0,meet_present_count*3))
# 
# aspects_meet_present$Positive[seq(1,meet_present_count)] <- 
#   as.character(df$Q8_1_TEXT[df$meet_cond == categories[1]])
# aspects_meet_present$Positive[seq(meet_present_count+1, 2*meet_present_count)] <- 
#   as.character(df$Q8_2_TEXT[df$meet_cond == categories[1]])
# aspects_meet_present$Positive[seq(2*meet_present_count+1, 3*meet_present_count)] <- 
#   as.character(df$Q8_3_TEXT[df$meet_cond == categories[1]])
# 
# aspects_meet_present$Negative[seq(1,meet_present_count)] <- 
#   as.character(df$Q9_1_TEXT[df$meet_cond == categories[1]])
# aspects_meet_present$Negative[seq(meet_present_count+1, 2*meet_present_count)] <- 
#   as.character(df$Q9_2_TEXT[df$meet_cond == categories[1]])
# aspects_meet_present$Negative[seq(2*meet_present_count+1, 3*meet_present_count)] <- 
#   as.character(df$Q9_3_TEXT[df$meet_cond == categories[1]])
# 
# file_meet_present <- file("meet_present_aspects_pos.txt")
# write.table(aspects_meet_present$Positive, file_meet_present, sep = "\t", row.names = FALSE, col.names = FALSE)
# file_meet_present <- file("meet_present_aspects_neg.txt")
# write.table(aspects_meet_present$Negative, file_meet_present, sep = "\t", row.names = FALSE, col.names = FALSE)
# 
# 
# 
# #remote meetings
# meet_remote_count <- length(df$meet_cond[df$meet_cond==categories[2]])
# aspects_meet_remote <- data.frame("Positive" = rep(0,meet_remote_count*3),
#                                    "Negative" = rep(0,meet_remote_count*3))
# 
# aspects_meet_remote$Positive[seq(1,meet_remote_count)] <- 
#   as.character(df$Q42_1_TEXT[df$meet_cond == categories[2]])
# aspects_meet_remote$Positive[seq(meet_remote_count+1, 2*meet_remote_count)] <- 
#   as.character(df$Q42_2_TEXT[df$meet_cond == categories[2]])
# aspects_meet_remote$Positive[seq(2*meet_remote_count+1, 3*meet_remote_count)] <- 
#   as.character(df$Q42_3_TEXT[df$meet_cond == categories[2]])
# 
# aspects_meet_remote$Negative[seq(1,meet_remote_count)] <- 
#   as.character(df$Q43_1_TEXT[df$meet_cond == categories[2]])
# aspects_meet_remote$Negative[seq(meet_remote_count+1, 2*meet_remote_count)] <- 
#   as.character(df$Q43_2_TEXT[df$meet_cond == categories[2]])
# aspects_meet_remote$Negative[seq(2*meet_remote_count+1, 3*meet_remote_count)] <- 
#   as.character(df$Q43_3_TEXT[df$meet_cond == categories[2]])
# 
# file_meet_remote <- file("meet_remote_aspects_pos.txt")
# write.table(aspects_meet_remote$Positive, file_meet_remote, sep = "\t", row.names = FALSE, col.names = FALSE)
# file_meet_remote <- file("meet_remote_aspects_neg.txt")
# write.table(aspects_meet_remote$Negative, file_meet_remote, sep = "\t", row.names = FALSE, col.names = FALSE)
# 
# 
# #present lect
# lect_present_count <- length(df$lect_cond[df$lect_cond==categories[3]])
# aspects_lect_present <- data.frame("Positive" = rep(0,lect_present_count*3),
#                                   "Negative" = rep(0,lect_present_count*3))
# 
# aspects_lect_present$Positive[seq(1,lect_present_count)] <- 
#   as.character(df$Q26_1_TEXT[df$lect_cond == categories[3]])
# aspects_lect_present$Positive[seq(lect_present_count+1, 2*lect_present_count)] <- 
#   as.character(df$Q26_2_TEXT[df$lect_cond == categories[3]])
# aspects_lect_present$Positive[seq(2*lect_present_count+1, 3*lect_present_count)] <- 
#   as.character(df$Q26_3_TEXT[df$lect_cond == categories[3]])
# 
# aspects_lect_present$Negative[seq(1,lect_present_count)] <- 
#   as.character(df$Q27_1_TEXT[df$lect_cond == categories[3]])
# aspects_lect_present$Negative[seq(lect_present_count+1, 2*lect_present_count)] <- 
#   as.character(df$Q27_2_TEXT[df$lect_cond == categories[3]])
# aspects_lect_present$Negative[seq(2*lect_present_count+1, 3*lect_present_count)] <- 
#   as.character(df$Q27_3_TEXT[df$lect_cond == categories[3]])
# 
# file_lect_present <- file("lect_present_aspects_pos.txt")
# write.table(aspects_lect_present$Positive, file_lect_present, sep = "\t", row.names = FALSE, col.names = FALSE)
# file_lect_present <- file("lect_present_aspects_neg.txt")
# write.table(aspects_lect_present$Negative, file_lect_present, sep = "\t", row.names = FALSE, col.names = FALSE)
# 
# 
# 
# #remote lect
# lect_remote_count <- length(df$lect_cond[df$lect_cond==categories[4]])
# aspects_lect_remote <- data.frame("Positive" = rep(0,lect_remote_count*3),
#                                   "Negative" = rep(0,lect_remote_count*3))
# 
# aspects_lect_remote$Positive[seq(1,lect_remote_count)] <- 
#   as.character(df$Q57_1_TEXT[df$lect_cond == categories[4]])
# aspects_lect_remote$Positive[seq(lect_remote_count+1, 2*lect_remote_count)] <- 
#   as.character(df$Q57_2_TEXT[df$lect_cond == categories[4]])
# aspects_lect_remote$Positive[seq(2*lect_remote_count+1, 3*lect_remote_count)] <- 
#   as.character(df$Q57_3_TEXT[df$lect_cond == categories[4]])
# 
# aspects_lect_remote$Negative[seq(1,lect_remote_count)] <- 
#   as.character(df$Q58_1_TEXT[df$lect_cond == categories[4]])
# aspects_lect_remote$Negative[seq(lect_remote_count+1, 2*lect_remote_count)] <- 
#   as.character(df$Q58_2_TEXT[df$lect_cond == categories[4]])
# aspects_lect_remote$Negative[seq(2*lect_remote_count+1, 3*lect_remote_count)] <- 
#   as.character(df$Q58_3_TEXT[df$lect_cond == categories[4]])
# 
# file_lect_remote <- file("lect_remote_aspects_pos.txt")
# write.table(aspects_lect_remote$Positive, file_lect_remote, sep = "\t", row.names = FALSE, col.names = FALSE)
# file_lect_remote <- file("lect_remote_aspects_neg.txt")
# write.table(aspects_lect_remote$Negative, file_lect_remote, sep = "\t", row.names = FALSE, col.names = FALSE)
# 
# 
# 
# ## Would you use a system like this? Why or why not? 45 29 60
# whynot_meet_present <- data.frame(rep(0,meet_present_count))
# whynot_meet_present <- as.character(df$Q11[df$meet_cond == categories[1]])
# 
# whynot_meet_remote <- data.frame(rep(0,meet_remote_count))
# whynot_meet_remote <- as.character(df$Q45[df$meet_cond == categories[2]])
# 
# whynot_lect_present <- data.frame(rep(0,lect_present_count))
# whynot_lect_present <- as.character(df$Q29[df$lect_cond == categories[3]])
# 
# whynot_lect_remote <- data.frame(rep(0,lect_remote_count))
# whynot_lect_remote <- as.character(df$Q60[df$lect_cond == categories[4]])
# 
# file_meet_present_whynot <- file("meet_present_whynot.txt")
# write.table(whynot_meet_present, file_meet_present_whynot, sep = "\t", row.names = FALSE, col.names = FALSE)
# file_meet_remote_whynot <- file("meet_remote_whynot.txt")
# write.table(whynot_meet_remote, file_meet_remote_whynot, sep = "\t", row.names = FALSE, col.names = FALSE)
# file_lect_present_whynot <- file("lect_present_whynot.txt")
# write.table(whynot_lect_present, file_lect_present_whynot, sep = "\t", row.names = FALSE, col.names = FALSE)
# file_lect_remote_whynot <- file("lect_remote_whynot.txt")
# write.table(whynot_lect_remote, file_lect_remote_whynot, sep = "\t", row.names = FALSE, col.names = FALSE)
# 
# 
# 
# 
# ## change or add about scenario
# change_meet_present <- data.frame(rep(0,meet_present_count))
# change_meet_present <- as.character(df$Q15[df$meet_cond == categories[1]])
# 
# change_meet_remote <- data.frame(rep(0,meet_remote_count))
# change_meet_remote <- as.character(df$Q49[df$meet_cond == categories[2]])
# 
# change_lect_present <- data.frame(rep(0,lect_present_count))
# change_lect_present <- as.character(df$Q33[df$lect_cond == categories[3]])
# 
# change_lect_remote <- data.frame(rep(0,lect_remote_count))
# change_lect_remote <- as.character(df$Q64[df$lect_cond == categories[4]])
# 
# file_meet_present_change <- file("meet_present_change.txt")
# write.table(change_meet_present, file_meet_present_change, sep = "\t", row.names = FALSE, col.names = FALSE)
# file_meet_remote_change <- file("meet_remote_change.txt")
# write.table(change_meet_remote, file_meet_remote_change, sep = "\t", row.names = FALSE, col.names = FALSE)
# file_lect_present_change <- file("lect_present_change.txt")
# write.table(change_lect_present, file_lect_present_change, sep = "\t", row.names = FALSE, col.names = FALSE)
# file_lect_remote_change <- file("lect_remote_change.txt")
# write.table(change_lect_remote, file_lect_remote_change, sep = "\t", row.names = FALSE, col.names = FALSE)
# 
# 
# 
# ## additional things
# addition_meet_present <- data.frame(rep(0,meet_present_count))
# addition_meet_present <- as.character(df$Q34[df$meet_cond == categories[1]])
# 
# addition_meet_remote <- data.frame(rep(0,meet_remote_count))
# addition_meet_remote <- as.character(df$Q65[df$meet_cond == categories[2]])
# 
# addition_lect_present <- data.frame(rep(0,lect_present_count))
# addition_lect_present <- as.character(df$Q16[df$lect_cond == categories[3]])
# 
# addition_lect_remote <- data.frame(rep(0,lect_remote_count))
# addition_lect_remote <- as.character(df$Q50[df$lect_cond == categories[4]])
# 
# file_meet_present_addition <- file("meet_present_addition.txt")
# write.table(addition_meet_present, file_meet_present_addition, sep = "\t", row.names = FALSE, col.names = FALSE)
# file_meet_remote_addition <- file("meet_remote_addition.txt")
# write.table(addition_meet_remote, file_meet_remote_addition, sep = "\t", row.names = FALSE, col.names = FALSE)
# file_lect_present_addition <- file("lect_present_addition.txt")
# write.table(addition_lect_present, file_lect_present_addition, sep = "\t", row.names = FALSE, col.names = FALSE)
# file_lect_remote_addition <- file("lect_remote_addition.txt")
# write.table(addition_lect_remote, file_lect_remote_addition, sep = "\t", row.names = FALSE, col.names = FALSE)
# 
# 
# ## other situations
# situation_meet_present <- data.frame(rep(0,meet_present_count))
# situation_meet_present <- as.character(df$Q24[df$meet_cond == categories[1]])
# 
# situation_meet_remote <- data.frame(rep(0,meet_remote_count))
# situation_meet_remote <- as.character(df$Q53[df$meet_cond == categories[2]])
# 
# situation_lect_present <- data.frame(rep(0,lect_present_count))
# situation_lect_present <- as.character(df$Q37[df$lect_cond == categories[3]])
# 
# situation_lect_remote <- data.frame(rep(0,lect_remote_count))
# situation_lect_remote <- as.character(df$Q68[df$lect_cond == categories[4]])
# 
# file_meet_present_situation <- file("meet_present_situation.txt")
# write.table(situation_meet_present, file_meet_present_situation, sep = "\t", row.names = FALSE, col.names = FALSE)
# file_meet_remote_situation <- file("meet_remote_situation.txt")
# write.table(situation_meet_remote, file_meet_remote_situation, sep = "\t", row.names = FALSE, col.names = FALSE)
# file_lect_present_situation <- file("lect_present_situation.txt")
# write.table(situation_lect_present, file_lect_present_situation, sep = "\t", row.names = FALSE, col.names = FALSE)
# file_lect_remote_situation <- file("lect_remote_situation.txt")
# write.table(situation_lect_remote, file_lect_remote_situation, sep = "\t", row.names = FALSE, col.names = FALSE)
# 
# 
# ##### additional thoughts
# file_additional_thoughts <- file("additional_thoughts.txt")
# write.table(df$Q14, file_additional_thoughts, sep = "\t", row.names = FALSE, col.names = FALSE)

##################

### Meeting colleagues

# present
meet_topics <- c("Brainstorming", "Planning", "Review", "Update")
topic_meet_present = data.frame("Topic" = meet_topics,
                                     "Count" = rep(0,length(meet_topics)))
topic_meet_present$Count[1] <-  sum(grepl("Brainstorming/ideation", df$Q31, fixed=TRUE))
topic_meet_present$Count[2] <-  sum(grepl("Planning", df$Q31, fixed=TRUE))
topic_meet_present$Count[3] <-  sum(grepl("Detailed review of work", df$Q31, fixed=TRUE))
topic_meet_present$Count[4] <-  sum(grepl("Regular progress update", df$Q31, fixed=TRUE))


topic_meet_present_plot <- ggplot(topic_meet_present, aes(x=Topic,y=Count, fill=Count)) + 
  geom_bar(stat="identity", alpha = 0.6, position="dodge") + 
  ggtitle("Walking Meeting Scenario - Meeting Topics") + 
  theme_classic() +
  scale_y_continuous(limits=c(0,26), breaks=pretty_breaks(), expand=c(0,0)) +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank(), legend.position="none")

topic_meet_present_plot


#remote
topic_meet_remote = data.frame("Topic" = meet_topics,
                                "Count" = rep(0,length(meet_topics)))
topic_meet_remote$Count[1] <-  sum(grepl("Brainstorming/ideation", df$Q62, fixed=TRUE))
topic_meet_remote$Count[2] <-  sum(grepl("Planning", df$Q62, fixed=TRUE))
topic_meet_remote$Count[3] <-  sum(grepl("Detailed review of work", df$Q62, fixed=TRUE))
topic_meet_remote$Count[4] <-  sum(grepl("Regular progress update", df$Q62, fixed=TRUE))


topic_meet_remote_plot <- ggplot(topic_meet_remote, aes(x=Topic,y=Count, fill=Count)) + 
  geom_bar(stat="identity", alpha = 0.6, position="dodge") + 
  ggtitle("Remote Meeting Scenario - Meeting Topics") + 
  theme_classic() +
  scale_y_continuous(limits=c(0,26), breaks=pretty_breaks(), expand=c(0,0)) +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank(), legend.position="none")
topic_meet_remote_plot


# combine
topic_meet <- data.frame("Topic" = rep(meet_topics, 2),
                         "Count" = c(topic_meet_present$Count, topic_meet_remote$Count),
                         "Condition" = c(rep("Present Meeting",length(meet_topics)), rep("Remote Meeting", length(meet_topics))))

topic_meet_plot <- ggplot(topic_meet, aes(x=Topic, y=Count, fill=Condition)) + 
  geom_bar(stat="identity", alpha = 0.6, position="dodge") + 
  ggtitle("Meeting Scenarios - Meeting Topics") + 
  theme_classic() +
  scale_y_continuous(limits=c(0,26), breaks=pretty_breaks(), expand=c(0,0)) +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank())
topic_meet_plot


### Lecture topics

# present
lect_topics <- c("Practical Course", "Theoretical Course")
topic_lect_present = data.frame("Topic" = lect_topics,
                                "Count" = rep(0,length(lect_topics)))
topic_lect_present$Count[1] <-  sum(grepl("Practical course", df$Q13, fixed=TRUE))
topic_lect_present$Count[2] <-  sum(grepl("Theoretical course", df$Q13, fixed=TRUE))



topic_lect_present_plot <- ggplot(topic_lect_present, aes(x=Topic,y=Count, fill=Count)) + 
  geom_bar(stat="identity", alpha = 0.6, position="dodge") + 
  ggtitle("Walking Lecture Scenario - Type of Lecture") + 
  theme_classic() +
  scale_y_continuous(limits=c(0,26), breaks=pretty_breaks(), expand=c(0,0)) +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank(), legend.position="none")
topic_lect_present_plot


#remote
topic_lect_remote = data.frame("Topic" = lect_topics,
                               "Count" = rep(0,length(lect_topics)))
topic_lect_remote$Count[1] <-  sum(grepl("Practical course", df$Q47, fixed=TRUE))
topic_lect_remote$Count[2] <-  sum(grepl("Theoretical course", df$Q47, fixed=TRUE))


topic_lect_remote_plot <- ggplot(topic_lect_remote, aes(x=Topic,y=Count, fill=Count)) + 
  geom_bar(stat="identity", alpha = 0.6, position="dodge") + 
  ggtitle("Remote Lecture Scenario - Type of Lecture") + 
  theme_classic() +
  scale_y_continuous(limits=c(0,35), breaks=pretty_breaks(), expand=c(0,0)) +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank(), legend.position="none")
topic_lect_remote_plot


topic_lect <- data.frame("Topic" = rep(lect_topics, 2),
                         "Count" = c(topic_lect_present$Count, topic_lect_remote$Count),
                         "Condition" = c(rep("Present Lecture",length(lect_topics)), rep("Remote Lecture", length(lect_topics))))

topic_lect_plot <- ggplot(topic_lect, aes(x=Topic, y=Count, fill=Condition)) + 
  geom_bar(stat="identity", alpha = 0.6, position="dodge") + 
  ggtitle("Lecture Scenarios - Type of Lecture") + 
  theme_classic() +
  scale_y_continuous(limits=c(0,35), breaks=pretty_breaks(), expand=c(0,0)) +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank())
topic_lect_plot



### Colleagues

# present meeting
meet_colleagues <- c("Supervisor", "Colleague", "Supervisee", "Close Colleague")

colleague_meet_present = data.frame("Colleague" = meet_colleagues,
                                "Count" = rep(0,length(meet_colleagues)))
colleague_meet_present$Count[1] <-  sum(grepl("Supervisor", df$Q32, fixed=TRUE))
colleague_meet_present$Count[2] <-  sum(grepl("Colleague", df$Q32, fixed=TRUE))
colleague_meet_present$Count[3] <-  sum(grepl("Someone I supervise", df$Q32, fixed=TRUE))
colleague_meet_present$Count[4] <-  sum(grepl("Colleagues I am close with", df$Q32, fixed=TRUE))


colleague_meet_present_plot <- ggplot(colleague_meet_present, aes(x=Colleague,y=Count)) + 
  geom_bar(stat="identity") + 
  ggtitle("Walking Meeting Scenario - Type of Colleagues") + 
  theme_classic() +
  scale_y_continuous(limits=c(0,31), breaks=pretty_breaks(), expand=c(0,0)) +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank())
colleague_meet_present_plot


#remote meeting
colleague_meet_remote = data.frame("Colleague" = meet_colleagues,
                               "Count" = rep(0,length(meet_colleagues)))
colleague_meet_remote$Count[1] <-  sum(grepl("Supervisor", df$Q63, fixed=TRUE))
colleague_meet_remote$Count[2] <-  sum(grepl("Colleague", df$Q63, fixed=TRUE))
colleague_meet_remote$Count[3] <-  sum(grepl("Someone I supervise", df$Q63, fixed=TRUE))
colleague_meet_remote$Count[4] <-  sum(grepl("Colleagues I am close with", df$Q63, fixed=TRUE))


colleague_meet_remote_plot <- ggplot(colleague_meet_remote, aes(x=Colleague,y=Count)) + 
  geom_bar(stat="identity") + 
  ggtitle("Walking Meeting Scenario - Type of Colleagues") + 
  theme_classic() +
  scale_y_continuous(limits=c(0,31), breaks=pretty_breaks(), expand=c(0,0)) +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank())
colleague_meet_remote_plot


#combine
colleague_meet <- data.frame("Colleague" = rep(meet_colleagues, 2),
                             "Count" = c(colleague_meet_present$Count, colleague_meet_remote$Count),
                             "Condition" = c(rep("Present Meeting",length(meet_colleagues)), rep("Remote Meeting", length(meet_colleagues))))

colleague_meet_plot <- ggplot(colleague_meet, aes(x=Colleague, y=Count, fill=Condition)) + 
  geom_bar(stat="identity", alpha = 0.6, position="dodge") + 
  ggtitle("Meeting Scenarios - With Whom") + 
  theme_classic() +
  scale_y_continuous(limits=c(0,35), breaks=pretty_breaks(), expand=c(0,0)) +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank())
colleague_meet_plot


# present lecture
lect_colleagues <- c("Other Students", "Students I am close with", "Lecturer", "No one")

colleague_lect_present = data.frame("Colleague" = lect_colleagues,
                                    "Count" = rep(0,length(lect_colleagues)))
colleague_lect_present$Count[1] <-  sum(grepl("Other students", df$Q22, fixed=TRUE))
colleague_lect_present$Count[2] <-  sum(grepl("Students I am close with", df$Q22, fixed=TRUE))
colleague_lect_present$Count[3] <-  sum(grepl("A lecturer", df$Q22, fixed=TRUE))
colleague_lect_present$Count[4] <-  sum(grepl("No one", df$Q22, fixed=TRUE))


colleague_lect_present_plot <- ggplot(colleague_lect_present, aes(x=Colleague,y=Count)) + 
  geom_bar(stat="identity") + 
  ggtitle("Walking Lecture Scenario - Type of Colleagues") + 
  theme_classic() +
  scale_y_continuous(limits=c(0,31), breaks=pretty_breaks(), expand=c(0,0)) +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank())
colleague_lect_present_plot


#remote lecture
colleague_lect_remote = data.frame("Colleague" = lect_colleagues,
                                   "Count" = rep(0,length(lect_colleagues)))
colleague_lect_remote$Count[1] <-  sum(grepl("Other students", df$Q48, fixed=TRUE))
colleague_lect_remote$Count[2] <-  sum(grepl("Students I am close with", df$Q48, fixed=TRUE))
colleague_lect_remote$Count[3] <-  sum(grepl("A lecturer", df$Q48, fixed=TRUE))
colleague_lect_remote$Count[4] <-  sum(grepl("No one", df$Q48, fixed=TRUE))


colleague_lect_remote_plot <- ggplot(colleague_lect_remote, aes(x=Colleague,y=Count)) + 
  geom_bar(stat="identity") + 
  ggtitle("Remote Lecture Scenario - Type of Colleagues") + 
  theme_classic() +
  scale_y_continuous(limits=c(0,31), breaks=pretty_breaks(), expand=c(0,0)) +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank())
colleague_lect_remote_plot


#combine
colleague_lect <- data.frame("Colleague" = rep(lect_colleagues, 2),
                         "Count" = c(colleague_lect_present$Count, colleague_lect_remote$Count),
                         "Condition" = c(rep("Present Lecture",length(lect_colleagues)), rep("Remote Lecture", length(lect_colleagues))))

colleague_lect_plot <- ggplot(colleague_lect, aes(x=Colleague, y=Count, fill=Condition)) + 
  geom_bar(stat="identity", alpha = 0.6, position="dodge") + 
  ggtitle("Lecture Scenarios - With Whom") + 
  theme_classic() +
  scale_y_continuous(limits=c(0,35), breaks=pretty_breaks(), expand=c(0,0)) +
  theme(plot.title = element_text(hjust=0.5), axis.title.x=element_blank())
colleague_lect_plot
