library('tidyverse')
library('scales')
library('ggsignif')
library('likert')
library('viridis')
library('ggplot2')
library('ggpubr')
library('rstatix')
library('broom')
library('emmeans')
library("reshape2")  
library('ARTool')
library(FSA)
library(afex)
library(stringr)
library(emmeans)
library(car)
library(lmerTest)

tukey_detect<-function(dv,Tukey_crit=1.5){
  IQR=IQR(dv,na.rm = TRUE)
  Quant_25=quantile(dv,probs=0.25,na.rm = TRUE)
  Quant_75=quantile(dv,probs=0.75,na.rm = TRUE)
  upper=Quant_75+Tukey_crit*IQR
  lower=Quant_25-Tukey_crit*IQR
  outlier_Tukey=ifelse(dv>upper,1,ifelse(dv<lower,1,0))
  print(outlier_Tukey)
  as.numeric(paste(outlier_Tukey))
}


#################
# Read in data
####################

df_text <- read.csv('./Survey/walking_stick_questionnaire.csv', header=TRUE)
df_num <- read.csv('./Survey/walking_stick_questionnaire_num.csv', header=TRUE)




###############
# Demographics
#################
N = length(df_num$ID)

age_mean = mean(df_num$Age)
age_min = min(df_num$Age)
age_max = max(df_num$Age)
age_sd = sd(df_num$Age)

N_female = sum(df_text$Gender == "Female")
N_male = sum(df_text$Gender == "Male")
female_perc = 100*N_female/N
male_perc = 100*N_male/N

n_gyear = sum(df_text$Q13 == "Greater than 1 year")
n_lyear = sum(df_text$Q13 == "Less than 1 year")
n_l6month = sum(df_text$Q13 == "Less than 6 months")
n_lmonth = sum(df_text$Q13 == "Less than 1 month")

n_gyear+n_lyear+n_l6month+n_lmonth
df_num$Q13 <- df_num$Q13 - 3
df_num$Condition <- rep('d', length(df_num$ID))
df_num$Condition[grep('NP*', df_num$ID)] <- 'Microphone'
df_num$Condition[strtoi(df_num$ID) < 100] <- 'Stick'
df_num$Condition[strtoi(df_num$ID) > 100] <- 'Button'
df_num$Condition

age_mean_mic = mean(df_num$Age[df_num$Condition == 'Microphone'])
age_mean_stick = mean(df_num$Age[df_num$Condition == 'Stick'])
age_mean_button = mean(df_num$Age[df_num$Condition == 'Button'])

names(df_num)[names(df_num) == "QID1_1"] <- "Stress"
names(df_num)[names(df_num) == "QID1_2"] <- "Engagement"
names(df_num)[names(df_num) == "QID1_3"] <- "Contribution"
names(df_num)[names(df_num) == "QID1_4"] <- "Future"

df_num$Condition <- factor(df_num$Condition, levels = c("Microphone", "Stick", "Button"))

###############
# Perceptions
#################


# Stress_box <- ggplot(df_num, aes(x = Condition, y = Stress, fill=Condition)) +
#   geom_boxplot() + 
#   scale_x_discrete(name = "Condition") + 
#   scale_y_continuous(name = "Total score") +
#   theme_classic() +
#   ggtitle("Stress") +
#   theme(text = element_text(size=25), legend.position="bottom", plot.title = element_text(hjust=0.5)) +
#   stat_summary(fun="mean", geom="point", shape=4, size=1.5,
#                position=position_dodge(width=0.75), color="black")
# Stress_box
# 
# wilcox.test(df_num$Stress[df_num$Condition == "Microphone"], df_num$Stress[df_num$Condition == "Stick"], paired=FALSE, p.adjust.method = "bonf")
# wilcox.test(df_num$Stress[df_num$Condition == "Microphone"], df_num$Stress[df_num$Condition == "Button"], paired=FALSE, p.adjust.method = "bonf")
# wilcox.test(df_num$Stress[df_num$Condition == "Stick"], df_num$Stress[df_num$Condition == "Button"], paired=FALSE, p.adjust.method = "bonf")
# 
# 
# # Enaged_box <- ggplot(df_num, aes(x = Condition, y = Engagement, fill=Condition)) +
# #   geom_boxplot() + 
# #   scale_x_discrete(name = "Condition") + 
# #   scale_y_continuous(name = "Total score") +
# #   theme_classic() +
# #   ggtitle("Engagement in Conversation") +
# #   theme(text = element_text(size=25), legend.position="bottom", plot.title = element_text(hjust=0.5)) +
# #   stat_summary(fun="mean", geom="point", shape=4, size=1.5,
# #                position=position_dodge(width=0.75), color="black")
# # Enaged_box
# 
# wilcox.test(df_num$Engagement[df_num$Condition == "Microphone"], df_num$Engagement[df_num$Condition == "Stick"], paired=FALSE, p.adjust.method = "bonf")
# wilcox.test(df_num$Engagement[df_num$Condition == "Microphone"], df_num$Engagement[df_num$Condition == "Button"], paired=FALSE, p.adjust.method = "bonf")
# wilcox.test(df_num$Engagement[df_num$Condition == "Stick"], df_num$Engagement[df_num$Condition == "Button"], paired=FALSE, p.adjust.method = "bonf")
# 
# 
# # Contributing_box <- ggplot(df_num, aes(x = Condition, y = Contribution, fill=Condition)) +
# #   geom_boxplot() + 
# #   scale_x_discrete(name = "Condition") + 
# #   scale_y_continuous(name = "Total score") +
# #   theme_classic() +
# #   ggtitle("Contribution to Conversation") +
# #   theme(text = element_text(size=25), legend.position="bottom", plot.title = element_text(hjust=0.5)) +
# #   stat_summary(fun="mean", geom="point", shape=4, size=1.5,
# #                position=position_dodge(width=0.75), color="black")
# # Contributing_box
# 
# wilcox.test(df_num$Contribution[df_num$Condition == "Microphone"], df_num$Contribution[df_num$Condition == "Stick"], paired=FALSE, p.adjust.method = "bonf")
# wilcox.test(df_num$Contribution[df_num$Condition == "Microphone"], df_num$Contribution[df_num$Condition == "Button"], paired=FALSE, p.adjust.method = "bonf")
# wilcox.test(df_num$Contribution[df_num$Condition == "Stick"], df_num$Contribution[df_num$Condition == "Button"], paired=FALSE, p.adjust.method = "bonf")
# 
# 
# # Future_box <- ggplot(df_num, aes(x = Condition, y = Future, fill=Condition)) +
# #   geom_boxplot() + 
# #   scale_x_discrete(name = "Condition") + 
# #   scale_y_continuous(name = "Total score") +
# #   theme_classic() +
# #   ggtitle("Use in the Future") +
# #   theme(text = element_text(size=25), legend.position="bottom", plot.title = element_text(hjust=0.5)) +
# #   stat_summary(fun="mean", geom="point", shape=4, size=1.5,
# #                position=position_dodge(width=0.75), color="black")
# # Future_box
# 
# wilcox.test(df_num$Future[df_num$Condition == "Microphone"], df_num$Future[df_num$Condition == "Stick"], paired=FALSE, p.adjust.method = "bonf")
# wilcox.test(df_num$Future[df_num$Condition == "Microphone"], df_num$Future[df_num$Condition == "Button"], paired=FALSE, p.adjust.method = "bonf")
# wilcox.test(df_num$Future[df_num$Condition == "Stick"], df_num$Future[df_num$Condition == "Button"], paired=FALSE, p.adjust.method = "bonf")

# Alternatively, ART Anova
shapiro.test(df_num$Stress) # not normal
m = art(Stress ~ Condition, data = df_num)
anova(m) # not sig
kruskal.test(Stress ~ Condition, data = df_num) # not sig

shapiro.test(df_num$Engagement) # not normal
m = art(Engagement ~ Condition, data = df_num)
anova(m) # not sig
kruskal.test(Engagement ~ Condition, data = df_num) # not sig

shapiro.test(df_num$Contribution) # not normal
m = art(Contribution ~ Condition, data = df_num)
anova(m) # sig
art.con(m, ~Condition) # mic to stick is sig
kruskal.test(Contribution ~ Condition, data = df_num) # not sig

shapiro.test(df_num$Future) # not normal
m = art(Future ~ Condition, data = df_num)
anova(m) # not sig
kruskal.test(Future ~ Condition, data = df_num) # not sig


# WITH HOW LONG PARTICIPANTS KNOW EACH OTHER
anova <- aov(Stress ~ Condition+Q13, data=df_num)
summary(anova) # Know is not sig
anova <- aov(Engagement ~ Condition+Q13, data=df_num)
summary(anova) # Know is not sig
anova <- aov(Contribution ~ Condition+Q13, data=df_num)
summary(anova) # Know is not sig
anova <- aov(Future ~ Condition+Q13, data=df_num)
summary(anova) # Know is not sig


df_perceptions <- df_num[c("Condition", "Stress", "Engagement", "Contribution", "Future")]
df_perceptions_long <- melt(df_perceptions, id="Condition")

Combined_box <- ggplot(df_perceptions_long, aes(x = variable, y = value, fill=Condition)) +
  geom_boxplot() + 
  scale_x_discrete(name = "Condition") + 
  scale_y_continuous(name = "Score") +
  theme_classic() +
  ggtitle("Subjective Perceptions") +
  theme(text = element_text(size=20), legend.position="top", plot.title = element_blank(),legend.title=element_blank(), axis.title.x=element_blank()) +
  stat_summary(fun="mean", geom="point", shape=4, size=1.5,
               position=position_dodge(width=0.75), color="black") +
  geom_signif(y_position = c(5.2), xmin=c(2.7), xmax=c(3.0), annotation=c("*"))
Combined_box
# ggsave('./plots/perceptions.pdf', width=12, height=6)

###############
# State Mindfulness Scale (SMS)
#################


df_num$SMS <- apply(df_num[,c(match("SMS_1",names(df_num)):match("SMS_21",names(df_num)))], 1, sum)
df_num$SMS


SMS_box <- ggplot(df_num, aes(x = Condition, y = SMS, fill=Condition)) +
  geom_boxplot() + 
  scale_x_discrete(name = "Condition") + 
  scale_y_continuous(name = "SMS Score") +
  theme_classic() +
  ggtitle("State Mindfulness Scale") +
  theme(text = element_text(size=25), legend.position="none", plot.title = element_blank(),legend.title=element_blank(), axis.title.x=element_blank()) +
  stat_summary(fun="mean", geom="point", shape=4, size=1.5,
               position=position_dodge(width=0.75), color="black")
SMS_box
# ggsave('./plots/sms.pdf', width=12, height=9)


shapiro.test(df_num$SMS) # Normal!
m = aov (SMS ~ Condition, df_num)
summary(m) # Not sig

# WITH HOW LONG PARTICIPANTS KNOW EACH OTHER
anova <- aov(SMS ~ Condition+Q13, data=df_num)
summary(anova) # Know is not sig

###############
# Trait Mindfulness
#################


df_num$TMS <- apply(df_num[,c(match("Q11_1",names(df_num)):match("Q11_15",names(df_num)))], 1, sum)
df_num$TMS


TMS_box <- ggplot(df_num, aes(x = Condition, y = TMS, fill=Condition)) +
  stat_boxplot(geom ='errorbar', width=0.25) + 
  geom_boxplot() + 
  scale_x_discrete(name = "Condition") + 
  scale_y_continuous(name = "TMS score") +
  theme_classic() +
  ggtitle("Trait Mindfulness") +
  theme(text = element_text(size=25), legend.position="none", plot.title = element_blank(),legend.title=element_blank(), axis.title.x=element_blank()) +
  stat_summary(fun="mean", geom="point", shape=4, size=1.5,
               position=position_dodge(width=0.75), color="black")
TMS_box


shapiro.test(df_num$TMS) # Normal!
m = aov (TMS ~ Condition, df_num)
summary(m) # Not sig

# WITH HOW LONG PARTICIPANTS KNOW EACH OTHER
anova <- aov(TMS ~ Condition+Q13, data=df_num)
summary(anova) # Know is not sig

###################
# Stait and Trait ANCOVA
###################
shapiro.test(df_num$SMS)# Normal!
shapiro.test(df_num$SMS[df_num$Condition == "Microphone"])
shapiro.test(df_num$SMS[df_num$Condition == "Stick"])
shapiro.test(df_num$SMS[df_num$Condition == "Button"])
shapiro.test(df_num$TMS)# Normal!
shapiro.test(df_num$TMS[df_num$Condition == "Microphone"])
shapiro.test(df_num$TMS[df_num$Condition == "Stick"])
shapiro.test(df_num$TMS[df_num$Condition == "Button"])

SMSTMS_scatter <- ggplot(df_num, aes(x = TMS, y = SMS, color=Condition)) +
  geom_point() + 
  geom_smooth(method=lm) +
  scale_x_discrete(name = "State Mindfulness") + 
  scale_y_continuous(name = "Trait Mindfulness") +
  theme_classic() +
  ggtitle("Trait vs State Mindfulness") +
  theme(text = element_text(size=25), legend.position="bottom", plot.title = element_text(hjust=0.5))
SMSTMS_scatter

df_num %>% anova_test(SMS ~ Condition+TMS)

# Fit the model, the covariate goes first
model <- lm(SMS ~ TMS + Condition, data = df_num)
# Inspect the model diagnostic metrics
#model.metrics <- augment(model) %>%
#  select(-.hat, -.sigma, -.fitted, -.se.fit) # Remove details
#head(model.metrics, 3)


res.aov <- df_num %>% anova_test(SMS ~ TMS + Condition)
get_anova_table(res.aov)

# Pairwise comparisons
pwc <- df_num %>% 
  emmeans_test(
    SMS ~ Condition, covariate = TMS,
    p.adjust.method = "bonferroni"
  )
pwc
get_emmeans(pwc)

# Visualization: line plots with p-values
pwc <- pwc %>% add_xy_position(x = "COndition", fun = "mean_se")
ggline(get_emmeans(pwc), x = "Condition", y = "emmean") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  stat_pvalue_manual(pwc, hide.ns = TRUE, tip.length = FALSE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )



anova <- aov(SMS ~ TMS + Condition + Error(ID), data=df_num)
summary(anova) # Not sig

# WITH HOW LONG PARTICIPANTS KNOW EACH OTHER
anova <- aov(SMS ~ TMS + Condition + Q13 + Error(ID), data=df_num)
summary(anova) # Not sig


###############
# SUS
#################


# df_num$SUS <- apply(df_num[,c(match("SUS_1",names(df_num)):match("SUS_10",names(df_num)))], 1, sum)
df_num$SUS <- (df_num$SUS_1 - 1) + (5 - df_num$SUS_2) + (df_num$SUS_3 - 1) + (5 - df_num$SUS_4) + (df_num$SUS_5 - 1) + (5 - df_num$SUS_6) + (df_num$SUS_7 - 1) + (5 - df_num$SUS_8 )+ (df_num$SUS_9 - 1) + (5 - df_num$SUS_10)
df_num$SUS <- df_num$SUS*2.5
df_num$SUS

SUS_box <- ggplot(df_num, aes(x = Condition, y = SUS, fill=Condition)) +
  geom_boxplot() + 
  scale_x_discrete(name = "Condition") + 
  scale_y_continuous(name = "SUS Score") +
  theme_classic() +
  ggtitle("System Usability Scale") +
  theme(text = element_text(size=25), legend.position="none", plot.title = element_blank(),legend.title=element_blank(), axis.title.x=element_blank()) +
  stat_summary(fun="mean", geom="point", shape=4, size=1.5,
               position=position_dodge(width=0.75), color="black")
SUS_box
# ggsave('./plots/sus.pdf', width=12, height=9)

mean(df_num$SUS[df_num$Condition == "Microphone"])
mean(df_num$SUS[df_num$Condition == "Stick"])
mean(df_num$SUS[df_num$Condition == "Button"])

shapiro.test(df_num$SUS) # Normal!
qqPlot(df_num$SUS)
anova <- aov(SUS ~ Condition, data=df_num)
summary(anova) # not sig
m = art (SUS ~ Condition, df_num)
anova(m) # not sig

# WITH HOW LONG PARTICIPANTS KNOW EACH OTHER
anova <- aov(SUS ~ Condition+Q13, data=df_num)
summary(anova) # Know is not sig

#############
## Attrack Diff
#############
attrak_questions_prag_num <- c(1,5,8,10,12,20,28)
attrak_questions_ident_num <- c(2,6,11,13,14,15,16)
attrak_questions_stim_num <- c(4,18,22,23,24,25,27)
attrak_questions_att_num <- c(3,7,9,17,19,21,26)


df_attrak <- data.frame(
                        "Attrak_1" = -1*(na.omit(df_num$Attrak_1)-4),
                        "Attrak_2" = na.omit(df_num$Attrak_2)-4,
                        "Attrak_3" = -1*(na.omit(df_num$Attrak_3)-4),
                        "Attrak_4" = -1*(na.omit(df_num$Attrak_4)-4),
                        "Attrak_5" = -1*(na.omit(df_num$Attrak_5)-4),
                        "Attrak_6" = -1*(na.omit(df_num$Attrak_6)-4),
                        "Attrak_7" = na.omit(df_num$Attrak_7)-4,
                        "Attrak_8" = -1*(na.omit(df_num$Attrak_8)-4),
                        "Attrak_9" = -1*(na.omit(df_num$Attrak_9)-4),
                        "Attrak_10" = na.omit(df_num$Attrak_10)-4,
                        "Attrak_11" = -1*(na.omit(df_num$Attrak_11)-4),
                        "Attrak_12" = -1*(na.omit(df_num$Attrak_12)-4),
                        "Attrak_13" = na.omit(df_num$Attrak_13)-4,
                        "Attrak_14" = na.omit(df_num$Attrak_14)-4,
                        "Attrak_15" = -1*(na.omit(df_num$Attrak_15)-4),
                        "Attrak_16" = na.omit(df_num$Attrak_16)-4,
                        "Attrak_17" = na.omit(df_num$Attrak_17)-4,
                        "Attrak_18" = na.omit(df_num$Attrak_18)-4,
                        "Attrak_19" = -1*(na.omit(df_num$Attrak_19)-4),
                        "Attrak_20" = na.omit(df_num$Attrak_20)-4,
                        "Attrak_21" = na.omit(df_num$Attrak_21)-4,
                        "Attrak_22" = -1*(na.omit(df_num$Attrak_22)-4),
                        "Attrak_23" = -1*(na.omit(df_num$Attrak_23)-4),
                        "Attrak_24" = na.omit(df_num$Attrak_24)-4,
                        "Attrak_25" = na.omit(df_num$Attrak_25)-4,
                        "Attrak_26" = -1*(na.omit(df_num$Attrak_26)-4),
                        "Attrak_27" = -1*(na.omit(df_num$Attrak_27)-4),
                        "Attrak_28" = na.omit(df_num$Attrak_28)-4,
                        "Condition" = df_num$Condition)

df_attrak$Condition <- factor(df_attrak$Condition, levels = c("Microphone", "Stick", "Button"))


df_num$Attrak_prag <- apply(df_attrak[attrak_questions_prag_num], 1, mean)
df_num$Attrak_ident <- apply(df_attrak[attrak_questions_ident_num], 1, mean)
df_num$Attrak_stim <- apply(df_attrak[attrak_questions_stim_num], 1, mean)
df_num$Attrak_att <- apply(df_attrak[attrak_questions_att_num], 1, mean)
df_num$Attrak <- apply(df_num[,c(match("Attrak_prag",names(df_num)):match("Attrak_att",names(df_num)))], 1, mean)
df_num$Attrak_HQ <- rowMeans(df_num[c("Attrak_ident", "Attrak_stim")])
df_num$Attrak_PQ <- df_num$Attrak_prag

df_attrak_plot <- data.frame("Condition" = df_num$Condition,
                             "PQ" = df_num$Attrak_prag,
                             "HQ-I" = df_num$Attrak_ident,
                             "HQ-S" = df_num$Attrak_stim,
                             "ATT" = df_num$Attrak_att,
                             "Average" = df_num$Attrak)
df_attrak_plot_long <- melt(df_attrak_plot, id="Condition")
df_attrak_plot_long$Condition <- factor(df_attrak_plot_long$Condition, levels = c("Microphone", "Stick", "Button"))

Combined_attrak_box <- ggplot(df_attrak_plot_long, aes(x = variable, y = value, fill=Condition)) +
  geom_boxplot() + 
  scale_x_discrete(name = "Condition") + 
  scale_y_continuous(name = "Score", limits = c(-3, 3), breaks = c(-3,-2,-1,0,1,2,3), expand=c(0,0)) +
  theme_classic() +
  theme(text = element_text(size=25), legend.position="top", plot.title = element_blank(), legend.title=element_blank(), axis.title.x=element_blank()) +
  stat_summary(fun="mean", geom="point", shape=4, size=1.5,
               position=position_dodge(width=0.75), color="black") +
  geom_vline(xintercept=4.5, linetype="dashed")
Combined_attrak_box
# ggsave('./plots/attrakdiff_box.pdf', width=16, height=8)


df_attrak_plot_HQPQ <- data.frame("Condition" = df_num$Condition,
                             "PQ" = df_num$Attrak_PQ,
                             "HQ" = df_num$Attrak_HQ,
                             "Average" = df_num$Attrak)
df_attrak_plot_long_HQPQ <- melt(df_attrak_plot_HQPQ, id="Condition")
df_attrak_plot_long_HQPQ$Condition <- factor(df_attrak_plot_long_HQPQ$Condition, levels = c("Microphone", "Stick", "Button"))

Combined_attrak_HQPQ_box <- ggplot(df_attrak_plot_long_HQPQ, aes(x = variable, y = value, fill=Condition)) +
  geom_boxplot() + 
  scale_x_discrete(name = "Condition") + 
  scale_y_continuous(name = "Score", limits = c(-3, 3), breaks = c(-3,-2,-1,0,1,2,3), expand=c(0,0)) +
  theme_classic() +
  theme(text = element_text(size=25), legend.position="top", plot.title = element_blank(), legend.title=element_blank(), axis.title.x=element_blank()) +
  stat_summary(fun="mean", geom="point", shape=4, size=1.5,
               position=position_dodge(width=0.75), color="black") +
  geom_vline(xintercept=2.5, linetype="dashed")
Combined_attrak_HQPQ_box
# ggsave('./plots/attrakdiff_box_HQPQ.pdf', width=16, height=8)


shapiro.test(df_num$Attrak) # NOT NORMAL
anova <- aov(Attrak ~ Condition, data=df_num)
summary(anova) # not sig
m = art(Attrak ~ Condition, data=df_num)
anova(m) # not sig
kruskal.test(Attrak ~ Condition, data = df_num) # not sig

shapiro.test(df_num$Attrak_PQ) # NORMAL!
anova <- aov(Attrak_PQ ~ Condition, data=df_num)
summary(anova) # not sig
m = art(Attrak_PQ ~ Condition, data=df_num)
anova(m) # not sig
kruskal.test(Attrak_PQ ~ Condition, data = df_num) # not sig

shapiro.test(df_num$Attrak_HQ) # NORMAL!
anova <- aov(Attrak_HQ ~ Condition, data=df_num)
summary(anova) # not sig
m = art(Attrak_HQ ~ Condition, data=df_num)
anova(m) # not sig
kruskal.test(Attrak_HQ ~ Condition, data = df_num) # not sig


# WITH HOW LONG PARTICIPANTS KNOW EACH OTHER
# cannot run ART because there is not every combination
kruskal.test(Attrak ~ Q13, data = df_num) # not sig
kruskal.test(Attrak_PQ ~ Q13, data = df_num) # not sig
kruskal.test(Attrak_HQ ~ Q13, data = df_num) # not sig

# Attrak_box <- ggplot(df_num, aes(x = Condition, y = Attrak, fill=Condition)) +
#   geom_boxplot() + 
#   scale_x_discrete(name = "Condition") + 
#   scale_y_continuous(name = "Total score") +
#   theme_classic() +
#   ggtitle("Attrak Mean") +
#   theme(text = element_text(size=25), legend.position="bottom", plot.title = element_text(hjust=0.5)) +
#   stat_summary(fun="mean", geom="point", shape=4, size=1.5,
#                position=position_dodge(width=0.75), color="black")
# Attrak_box
# 
# Attrak_prag_box <- ggplot(df_num, aes(x = Condition, y = Attrak_prag, fill=Condition)) +
#   geom_boxplot() + 
#   scale_x_discrete(name = "Condition") + 
#   scale_y_continuous(name = "Total score") +
#   theme_classic() +
#   ggtitle("Pragmatic Quality (PQ)") +
#   theme(text = element_text(size=25), legend.position="bottom", plot.title = element_text(hjust=0.5)) +
#   stat_summary(fun="mean", geom="point", shape=4, size=1.5,
#                position=position_dodge(width=0.75), color="black")
# Attrak_prag_box
# 
# Attrak_ident_box <- ggplot(df_num, aes(x = Condition, y = Attrak_ident, fill=Condition)) +
#   geom_boxplot() + 
#   scale_x_discrete(name = "Condition") + 
#   scale_y_continuous(name = "Total score") +
#   theme_classic() +
#   ggtitle("Hedonic Quality-Identity (HQ-I)") +
#   theme(text = element_text(size=25), legend.position="bottom", plot.title = element_text(hjust=0.5)) +
#   stat_summary(fun="mean", geom="point", shape=4, size=1.5,
#                position=position_dodge(width=0.75), color="black")
# Attrak_ident_box
# 
# Attrak_stim_box <- ggplot(df_num, aes(x = Condition, y = Attrak_stim, fill=Condition)) +
#   geom_boxplot() + 
#   scale_x_discrete(name = "Condition") + 
#   scale_y_continuous(name = "Total score") +
#   theme_classic() +
#   ggtitle("Hedonic Quality-Stimulation (HQ-S)") +
#   theme(text = element_text(size=25), legend.position="bottom", plot.title = element_text(hjust=0.5)) +
#   stat_summary(fun="mean", geom="point", shape=4, size=1.5,
#                position=position_dodge(width=0.75), color="black")
# Attrak_stim_box
# 
# Attrak_att_box <- ggplot(df_num, aes(x = Condition, y = Attrak_att, fill=Condition)) +
#   geom_boxplot() + 
#   scale_x_discrete(name = "Condition") + 
#   scale_y_continuous(name = "Total score") +
#   theme_classic() +
#   ggtitle("Attractiveness (ATT)") +
#   theme(text = element_text(size=25), legend.position="bottom", plot.title = element_text(hjust=0.5)) +
#   stat_summary(fun="mean", geom="point", shape=4, size=1.5,
#                position=position_dodge(width=0.75), color="black")
# Attrak_att_box


# do 95 confidence for each and draw rectangle
error <- qnorm(0.975)*sd(df_num$Attrak_PQ[df_num$Condition == "Microphone"])/sqrt(length(df_num$Attrak_PQ[df_num$Condition == "Microphone"]))
pq_left_mic <- mean(df_num$Attrak_PQ[df_num$Condition == "Microphone"]) - error
pq_right_mic <- mean(df_num$Attrak_PQ[df_num$Condition == "Microphone"]) + error


error <- qnorm(0.975)*sd(df_num$Attrak_HQ[df_num$Condition == "Microphone"])/sqrt(length(df_num$Attrak_HQ[df_num$Condition == "Microphone"]))
hq_left_mic <- mean(df_num$Attrak_HQ[df_num$Condition == "Microphone"]) - error
hq_right_mic <- mean(df_num$Attrak_HQ[df_num$Condition == "Microphone"]) + error

# do 95 confidence for each and draw rectangle
error <- qnorm(0.975)*sd(df_num$Attrak_PQ[df_num$Condition == "Stick"])/sqrt(length(df_num$Attrak_PQ[df_num$Condition == "Stick"]))
pq_left_stick <- mean(df_num$Attrak_PQ[df_num$Condition == "Stick"]) - error
pq_right_stick <- mean(df_num$Attrak_PQ[df_num$Condition == "Stick"]) + error

error <- qnorm(0.975)*sd(df_num$Attrak_HQ[df_num$Condition == "Stick"])/sqrt(length(df_num$Attrak_HQ[df_num$Condition == "Stick"]))
hq_left_stick <- mean(df_num$Attrak_HQ[df_num$Condition == "Stick"]) - error
hq_right_stick <- mean(df_num$Attrak_HQ[df_num$Condition == "Stick"]) + error

# do 95 confidence for each and draw rectangle
error <- qnorm(0.975)*sd(df_num$Attrak_PQ[df_num$Condition == "Button"])/sqrt(length(df_num$Attrak_PQ[df_num$Condition == "Button"]))
pq_left_button <- mean(df_num$Attrak_PQ[df_num$Condition == "Button"]) - error
pq_right_button <- mean(df_num$Attrak_PQ[df_num$Condition == "Button"]) + error

error <- qnorm(0.975)*sd(df_num$Attrak_HQ[df_num$Condition == "Button"])/sqrt(length(df_num$Attrak_HQ[df_num$Condition == "Button"]))
hq_left_button <- mean(df_num$Attrak_HQ[df_num$Condition == "Button"]) - error
hq_right_button <- mean(df_num$Attrak_HQ[df_num$Condition == "Button"]) + error

mic_x <- mean(df_num$Attrak_PQ[df_num$Condition == "Microphone"])
mic_y <- mean(df_num$Attrak_HQ[df_num$Condition == "Microphone"])

stick_x <- mean(df_num$Attrak_PQ[df_num$Condition == "Stick"])
stick_y <- mean(df_num$Attrak_HQ[df_num$Condition == "Stick"])

button_x <- mean(df_num$Attrak_PQ[df_num$Condition == "Button"])
button_y <- mean(df_num$Attrak_HQ[df_num$Condition == "Button"])

df_attrak_rect <- data.frame(pq_left_mic,
                              pq_right_mic,
                              hq_left_mic,
                              hq_right_mic,
                              pq_left_stick,
                              pq_right_stick,
                              hq_left_stick,
                              hq_right_stick,
                              pq_left_button,
                              pq_right_button,
                              hq_left_button,
                              hq_right_button,
                             mic_x,
                             mic_y,
                             stick_x,
                             stick_y,
                             button_x,
                             button_y)

df_attrak_rect



attrak_rect_plot <- ggplot() +
  geom_rect(aes(xmin=-2.99, xmax=-1.01, ymin=-2.99, ymax=-1.01), fill="#E1E1E1", alpha=0.6) +
  geom_rect(aes(xmin=-2.99, xmax=-1.01, ymin=-0.99, ymax=0.99), fill="#E6E6E6", alpha=0.6) +
  geom_rect(aes(xmin=-2.99, xmax=-1.01, ymin=1.01, ymax=2.99), fill="#ECECEC", alpha=0.6) +
  geom_rect(aes(xmin=-0.99, xmax=0.99, ymin=-2.99, ymax=-1.01), fill="#E6E6E6", alpha=0.6) +
  geom_rect(aes(xmin=-0.99, xmax=0.99, ymin=-0.99, ymax=0.99), fill="#ECECEC", alpha=0.6) +
  geom_rect(aes(xmin=-0.99, xmax=0.99, ymin=1.01, ymax=2.99), fill="#F2F2F2", alpha=0.6) +
  geom_rect(aes(xmin=1.01, xmax=2.99, ymin=-2.99, ymax=-1.01), fill="#ECECEC", alpha=0.6) +
  geom_rect(aes(xmin=1.01, xmax=2.99, ymin=-0.99, ymax=0.99), fill="#F2F2F2", alpha=0.6) +
  geom_rect(aes(xmin=1.01, xmax=2.99, ymin=1.01, ymax=2.99), fill="#F6F6F6", alpha=0.6) +
  annotate("text", x=c(-2.8,-2.8,-0.8,-0.8,1.2,1.2,1.2), y=c(-1.2,2.6,0.8,2.6,-1.4,0.6,2.8),
           label= c("superfluous", "too self-\noriented","neutral","too self-\noriented","too task-\noriented","task-\noriented","desired"), 
           size=8, color="#AEAEAE", hjust=0) + 
  geom_rect(data=df_attrak_rect, mapping=aes(xmin=pq_left_mic, xmax=pq_right_mic, ymin=hq_left_mic, ymax=hq_right_mic), fill="#F8766D", alpha=0.5) +
  geom_rect(data=df_attrak_rect, mapping=aes(xmin=pq_left_stick, xmax=pq_right_stick, ymin=hq_left_stick, ymax=hq_right_stick), fill="#00BA38", alpha=0.5) +
  geom_rect(data=df_attrak_rect, mapping=aes(xmin=pq_left_button, xmax=pq_right_button, ymin=hq_left_button, ymax=hq_right_button), fill="#619CFF", alpha=0.5) +
  geom_point(aes(x=mic_x, y=mic_y), shape=15, color="#F8766D", size=5)+
  geom_point(aes(x=stick_x, y=stick_y), shape=15, color="#00BA38", size=5)+
  geom_point(aes(x=button_x, y=button_y), shape=15, color="#619CFF", size=5)+
  scale_x_continuous(limits  = c(-3, 3), breaks = c(-3,-1,1,3), expand=c(0,0)) +
  scale_y_continuous(limits = c(-3, 3), breaks = c(-3,-1,1,3), expand=c(0,0)) +
  xlab("Pragmatic Qualities (PQ)") +
  ylab("Hedonic Qualities HQ)") +
  ggtitle(" ")+
  theme_classic() +
  theme(
    axis.line = element_line(),
    title = element_text(size=15),
    axis.title.y = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    axis.title.x = element_text(size = 25),
    axis.text.x = element_text(size = 25)
  )
attrak_rect_plot

# ggsave('./plots/attrakdiff_squares.pdf', width=9, height=9)



############################
## Meeting Info
############################



df_meeting <- read.csv('./Meeting Transcripts/meeting_times.csv', header=TRUE)

df_know <- data.frame('PID' = df_num$ID,
                      'Know' = df_num$Q13)
df_know$PID <- factor(df_know$PID, levels = df_meeting$PID)
df_know <- df_know[order(df_know$PID),]

df_know



length(df_meeting$PID)

df_ratio <- data.frame("Condition" = df_meeting$Condition[df_meeting$speaking_ratio <= 1],
                       "Ratio" = df_meeting$speaking_ratio[df_meeting$speaking_ratio <= 1],
                       "Know" = df_know$Know[seq(1,length(df_know$Know), 2)])
df_ratio$Condition <- factor(df_ratio$Condition, levels = c("Microphone", "Stick", "Button"))
df_ratio$Know <- factor(df_ratio$Know, levels = c(1,2,3,4))

df_ratio
ratio_box <- ggplot(df_ratio, aes(x = Condition, y = Ratio, fill=Condition)) +
  stat_boxplot(geom ='errorbar', width=0.25) + 
  geom_boxplot() + 
  scale_x_discrete(name = "Condition") + 
  scale_y_continuous(name = "Speaking Time Ratio", limits = c(0, 1), expand=c(0,0)) +
  theme_classic() +
  theme(text = element_text(size=25), legend.position="top", plot.title = element_blank(), legend.title=element_blank(), axis.title.x=element_blank()) +
  stat_summary(fun="mean", geom="point", shape=4, size=1.5,
               position=position_dodge(width=0.75), color="black")
ratio_box

shapiro.test(df_ratio$Ratio) # Normal
m <- aov (Ratio ~ Condition, data = df_ratio)
summary(m) # not sig
mean(df_ratio$Ratio)

# WITH HOW LONG PARTICIPANTS KNOW EACH OTHER
anova <- aov(Ratio ~ Know, data=df_ratio)
summary(anova) # Know is not sig



time_box <- ggplot(df_meeting, aes(x = Condition, y = speaking_perc, fill=Condition)) +
  stat_boxplot(geom ='errorbar', width=0.25) + 
  geom_boxplot() + 
  theme_classic() +
  theme(text = element_text(size=25), legend.position="top", plot.title = element_blank(), legend.title=element_blank(), axis.title.x=element_blank()) +
  stat_summary(fun="mean", geom="point", shape=4, size=1.5,
               position=position_dodge(width=0.75), color="black")
time_box

shapiro.test(df_meeting$speaking_perc) # Normal
m <- aov (speaking_perc ~ Condition, data = df_meeting)
summary(m) # not sig


############################
## Meeting Transcripts
############################

df_convo <- read.csv('./Meeting Transcripts/conversations.csv', header=TRUE)

df_convo

df_convo$Condition <- rep('d', length(df_convo$ID))

df_convo$ID <- as.character(df_convo$ID)

df_convo$Condition[df_convo$ID < 100] <- 'Stick'
df_convo$Condition[df_convo$ID > 100] <- 'Button'
df_convo$Condition[grep('NP*', df_convo$ID)] <- 'Microphone'
unique(df_convo$Condition)
unique(df_convo$ID)

# outliers_wordcount <- tukey_detect(dv=df_convo$Wordcount, Tukey_crit = 1.5)
# df_convo[outliers_wordcount == 1,]$Wordcount <- mean(df_convo$Wordcount)

df_convo$Condition <- factor(df_convo$Condition, levels = c("Microphone", "Stick", "Button"))


words_ona_turn_box <- ggplot(df_convo, aes(x = Condition, y = Wordcount, fill=Condition)) +
  stat_boxplot(geom ='errorbar', width=0.25) + 
  geom_boxplot() + 
  scale_x_discrete(name = "Condition") + 
  scale_y_continuous(name = "Words on a Turn", limits = c(0, 240), expand=c(0,0)) +
  theme_classic() +
  theme(text = element_text(size=25), legend.position="none", plot.title = element_blank(), legend.title=element_blank(), axis.title.x=element_blank()) +
  stat_summary(fun="mean", geom="point", shape=4, size=1.5,
               position=position_dodge(width=0.75), color="black") +
  geom_signif(y_position = c(210,230), xmin=c(1,2), xmax=c(3,3), annotation=c("**", "**"))
words_ona_turn_box
# ggsave('./plots/wordsinturns.pdf', width=12, height=9)

shapiro.test(df_convo$Wordcount) # NOT Normal
m <- aov(Wordcount ~ Condition, data = df_convo)
summary(m)
TukeyHSD(m)
kruskal.test(Wordcount ~ Condition, data = df_convo) # sig
dunn=dunnTest(Wordcount ~ Condition, data=df_convo, method="bh")   
dunn # sig for all
wilcox_test(data=df_convo, Wordcount~Condition, paired=FALSE, p.adjust.method = "bonf")


total_words_in_convo <- function(wordcount_list, first_idx, last_idx) {
  new_list = rep(0,length(first_idx))
  for (i in seq(1,length(first_idx))){
    new_list[i] <- sum(wordcount_list[first_idx[i]:last_idx[i]])
  }
  return(new_list)
}


first_turn_idx <- match(unique(df_convo$Pair), df_convo$Pair)
last_turn_idx <- length(df_convo$Pair)-match(unique(df_convo$Pair),rev(df_convo$Pair))+1
num_turns <- last_turn_idx - first_turn_idx

words_pair <- total_words_in_convo(df_convo$Wordcount, first_turn_idx, last_turn_idx)


df_turns <- data.frame("Condition" = df_convo$Condition[first_turn_idx],
                       "Pair" = unique(df_convo$Pair),
                       "EndTime" = df_meeting$total_seconds[seq(1,length(df_meeting$total_seconds), 2)],
                       "Turns" = num_turns,
                       "Words" = words_pair,
                       "Silence" = df_meeting$silence[seq(1,length(df_meeting$total_seconds), 2)],
                       "Know" = df_know$Know[seq(1,length(df_know$Know), 2)])

df_turns$WordsPerTurn <- df_turns$Words/df_turns$Turns
df_turns$TurnsPerMinute <- 60*df_turns$Turns/df_turns$EndTime
df_turns$TurnLength <- df_turns$EndTime/df_turns$Turns
df_turns$Know <- factor(df_turns$Know, levels = c(1,2,3,4))
df_turns

words_per_turn_box <- ggplot(df_turns, aes(x = Condition, y = WordsPerTurn, fill=Condition)) +
  geom_boxplot() + 
  scale_x_discrete(name = "Condition") + 
  scale_y_continuous(name = "Turn Density (words per turn)", limits = c(0, 66), expand=c(0,0)) +
  ggtitle("") + 
  theme_classic() +
  theme(text = element_text(size=30), legend.position="none", plot.title = element_text(size=2), legend.title=element_blank(), axis.title.x=element_blank()) +
  stat_summary(fun="mean", geom="point", shape=4, size=1.5,
               position=position_dodge(width=0.75), color="black") +
  geom_signif(y_position = c(59,52), xmin=c(1,2), xmax=c(3,3), annotation=c("**", "**"), textsize=9)
words_per_turn_box
# ggsave('./plots/avg_wordsperturn.pdf', width=12, height=9)



shapiro.test(df_turns$WordsPerTurn) # Normal
anova <- aov(WordsPerTurn ~ Condition, data=df_turns)
summary(anova) # sig
tukey <- TukeyHSD(anova)
tukey # sig for button-mic and button-stick

# WITH HOW LONG PARTICIPANTS KNOW EACH OTHER
anova <- aov(WordsPerTurn ~ Know, data=df_turns)
summary(anova) # Know is not sig



turns_per_minute_box <- ggplot(df_turns, aes(x = Condition, y = TurnsPerMinute, fill=Condition)) +
  geom_boxplot() + 
  scale_x_discrete(name = "Condition") + 
  scale_y_continuous(name = "Interactivity (turns per minute)", limits = c(0, 14), expand=c(0,0)) +
  ggtitle("") + 
  theme_classic() +
  theme(text = element_text(size=30), legend.position="none", plot.title = element_text(size=2), legend.title=element_blank(), axis.title.x=element_blank()) +
  stat_summary(fun="mean", geom="point", shape=4, size=1.5,
               position=position_dodge(width=0.75), color="black") +
  geom_signif(y_position = c(12.5,11.5), xmin=c(1,2), xmax=c(3,3), annotation=c("**", "*"), textsize=9)
turns_per_minute_box
# ggsave('./plots/avg_turnsperminute.pdf', width=12, height=9)


shapiro.test(df_turns$TurnsPerMinute) # NOT normal
qqPlot(df_turns$TurnsPerMinute)
hist(df_turns$TurnsPerMinute)
df_turns$LogTurnsPerMinute = log(df_turns$TurnsPerMinute)
shapiro.test(df_turns$LogTurnsPerMinute) # NOT normal

anova <- aov(TurnsPerMinute ~ Condition, data=df_turns)
summary(anova) # sig
tukey <- TukeyHSD(anova)
tukey # sig button-mic and button-stick

# USE THIS
m = art(TurnsPerMinute ~ Condition, data = df_turns)
anova(m) # sig
art.con(m, ~Condition) # sig button-mic and button-stick

kruskal.test(TurnsPerMinute ~ Condition, data = df_turns) # sig
# dunn = dunnTest(TurnsPerMinute ~ Condition, data=df_turns, method="bh")   
# dunn # sig button-mic and button-stick
wilcox_test(data=df_turns, TurnsPerMinute~Condition, paired=FALSE, p.adjust.method = "bonf")


# WITH HOW LONG PARTICIPANTS KNOW EACH OTHER
anova <- aov(TurnsPerMinute ~ Condition+Know, data=df_turns)
summary(anova) # Know is not sig
m = art(TurnsPerMinute ~ Know, data = df_turns)
anova(m) # Doesnt work for Know because there isn't every combination


turnlength_box <- ggplot(df_turns, aes(x = Condition, y = TurnLength, fill=Condition)) +
  geom_boxplot() + 
  scale_x_discrete(name = "Condition") +
  ggtitle("") + 
  scale_y_continuous(name = "Turn Length (s)", limits = c(0, 32), expand=c(0,0)) +
  theme_classic() +
  theme(text = element_text(size=25), legend.position="none", plot.title = element_text(size=2), legend.title=element_blank(), axis.title.x=element_blank()) +
  stat_summary(fun="mean", geom="point", shape=4, size=1.5,
               position=position_dodge(width=0.75), color="black") +
  geom_signif(y_position = c(28,24.5), xmin=c(1,2), xmax=c(3,3), annotation=c("**", "*"), textsize=8) 
turnlength_box
# ggsave('./plots/avg_turnlength.pdf', width=12, height=9)

shapiro.test(df_turns$TurnLength) # Normal!
anova <- aov(TurnLength ~ Condition, data=df_turns)
summary(anova) # sig
tukey <- TukeyHSD(anova)
tukey # sig button-mic and button-stick

silence_box <- ggplot(df_turns, aes(x = Condition, y = Silence, fill=Condition)) +
  geom_boxplot() + 
  scale_x_discrete(name = "Condition") +
  ggtitle("") + 
  scale_y_continuous(name = "Silence (%)", limits = c(0, 31), expand=c(0,0)) +
  theme_classic() +
  ggtitle(" ")+
  theme(text = element_text(size=25), legend.position="none", plot.title = element_text(size=20), legend.title=element_blank(), axis.title.x=element_blank()) +
  stat_summary(fun="mean", geom="point", shape=4, size=1.5,
               position=position_dodge(width=0.75), color="black") +
  geom_signif(y_position = c(29), xmin=c(1), xmax=c(3), annotation=c("*"), textsize=8)
silence_box
# ggsave('./plots/silence.pdf', width=12, height=9)

shapiro.test(df_turns$Silence) # NOT normal
anova <- aov(Silence ~ Condition, data=df_turns)
summary(anova) # sig
tukey <- TukeyHSD(anova)
tukey # sig button-mic

# USE THIS
m = art(Silence ~ Condition, data = df_turns)
anova(m) # sig
art.con(m, ~Condition) # sig button-mic 

kruskal.test(Silence ~ Condition, data = df_turns) # sig
dunn = dunnTest(Silence ~ Condition, data=df_turns, method="bh")   
dunn # sig button-mic
wilcox_test(data=df_turns, Silence~Condition, paired=FALSE, p.adjust.method = "bonf")

