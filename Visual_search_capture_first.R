#### Capture first ####

data = read.csv('Capture_first.csv', header=TRUE, sep = ';')

number_participants = length(data[,1])

data_stat <- data[,c('Subjects', 'Group', 'Eyetracker',  'Educ', 'RecEMO.1','RecEMO.3','RecEMO.5', 'RecNEU.1', 'RecNEU.3', 'RecNEU.5')]
data_stat <- melt_function(data_stat,
                                   c('Subjects','Group', 'Eyetracker', 'Educ'),
                                   c('RecEMO.1','RecEMO.3','RecEMO.5', 'RecNEU.1', 'RecNEU.3', 'RecNEU.5'))
data_stat$Emotion <- c(rep('EMO',number_participants*3),
                               rep('NEU', number_participants*3))
data_stat$Distractor <- c(rep('1',number_participants),
                                  rep('3',number_participants),
                                  rep('5',number_participants),
                                  rep('1',number_participants),
                                  rep('3',number_participants),
                                  rep('5',number_participants))
data_stat <- data_stat[,c('Subjects','Group', 'Eyetracker', 'Educ', 'value','Emotion','Distractor')]
names(data_stat) <-c('Sujet', 'Group', 'Eyetracker', 'Educ', 'Capture_first', 'Emotion', 'Distractor')

#### ANOVA ####

data_stat$Group <- factor(data_stat$Group, labels = c('AD', 'HOA', 'YA'), levels = levels(data_stat$Group)[c(1,2,3)])
data_stat$Emotion <- factor(data_stat$Emotion, labels = c('EMO', 'NEU'))
data_stat$Distractor <- factor(data_stat$Distractor, labels = c('1', '3', '5'))
data_stat$Eyetracker <- factor(data_stat$Eyetracker, labels = c('Eyelink', 'Smi'))
data_stat$Educ <- factor(data_stat$Educ, labels = c('1', '2', '3', '4'))

# Contrasts
EmovsNeu <- c(0.5,-0.5)
HOAvsMA <- c(-0.5,0.5,0)
YAvsHOA <- c(0,0.5,-0.5)
YAvsrest <- c(-0.5,-0.5,1)
onevsrest <- c(1,-0.5,-0.5)
threevs5 <- c(0,-0.5,0.5)
EyelinkvsSmi <- c(0.5, -0.5)
LowvsHigh <- c(-1,-1,1,1)
Verylowvslow <- c(-1,1,0,0)
HighvsVeryhigh <- c(0,0,-1,1)
contrasts(data_stat$Emotion) <- cbind(EmovsNeu)
contrasts(data_stat$Group) <- cbind(HOAvsMA, YAvsrest)
contrasts(data_stat$Distractor) <- cbind(onevsrest, threevs5)
contrasts(data_stat$Eyetracker) <- cbind(EyelinkvsSmi)

# Model
# With Eyetracker
ADHOAModel <- ezANOVA(data = data_stat,
                      dv = .(Capture_first),
                      wid = .(Sujet),
                      between = c(Group, Eyetracker),
                      within = c(Emotion, Distractor),
                      type = 3,
                      detailed = TRUE,
                      return_aov = TRUE)

# Without Eyetracker
ADHOAModel <- ezANOVA(data = data_stat,
                      dv = .(Capture_first),
                      wid = .(Sujet),
                      between = .(Group),
                      within = c(Emotion, Distractor),
                      type = 3,
                      detailed = TRUE,
                      return_aov = TRUE)
summary.lm(ADHOAModel$aov$'Sujet:Emotion')
summary.lm(ADHOAModel$aov$'Sujet:Distractor')
ADHOAModel
confint(ADHOAModel$aov$'Sujet:Emotion')
confint(ADHOAModel$aov$'Sujet:Distractor')

rcontrast(t = 2.927, df = 74)
rcontrast(t = 2.042, df = 74)
rcontrast(t = -3.218, df = 148)
partial_eta_squared(SSn = 1.432517e-05,SSd = 2.867567e-05)
partial_eta_squared(SSn = 1.184474e-04,SSd = 1.097234e-05)
partial_eta_squared(SSn = 1.803908e-06,SSd = 5.315031e-06)
partial_eta_squared(SSn = 8.396997e-07,SSd = 5.315031e-06)
partial_eta_squared(SSn = 8.333254e-07,SSd = 1.097234e-05)

#Post hoc. Done with statistica.
#data_stat$combined <- cbind(data_stat$Distractor, data_stat$Emotion)
pairwise.t.test(data_stat$Capture_first, data_stat$Group, paired = FALSE, p.adjust.method = 'bonferroni')

#Simple effects
data_neu <- subset_function_keep(data_stat, data_stat$Emotion, 'NEU')
ADHOAModel <- ezANOVA(data = data_neu,
                      dv = .(Capture_first),
                      wid = .(Sujet),
                      between = .(Group),
                      within = c(Distractor),
                      type = 3,
                      detailed = TRUE,
                      return_aov = TRUE)
ADHOAModel
summary.lm(ADHOAModel$aov$'Sujet')
confint(ADHOAModel$aov$'Sujet')
rcontrast(t = 1.234, df = 74)
by(data_neu$Capture_first, list(data_neu$Group), stat.desc)
"ttestfromMeans(x1 = 1.657238e-03,
               x2 = 1.759512e-03,
               sd1 = 6.363259e-04,
               sd2 = 6.712250e-04,
               n1 = 18,
               n2 = 24)"

data_emo <- subset_function_keep(data_stat, data_stat$Emotion, 'EMO')
ADHOAModel <- ezANOVA(data = data_emo,
                      dv = .(Capture_first),
                      wid = .(Sujet),
                      between = .(Group),
                      within = c(Distractor),
                      type = 3,
                      detailed = TRUE,
                      return_aov = TRUE)
ADHOAModel
summary.lm(ADHOAModel$aov$'Sujet')
confint(ADHOAModel$aov$'Sujet')
rcontrast(t = 2.697, df = 74)

# Modèle multi-niveaux
"EmovsNeu <- c(0.5,-0.5)
HOAvsMA <- c(1,0,0)
YAvsHOA <- c(0,0,1)
onevsthree <- c(1,0,0)
threevs5 <- c(0,0,1)
contrasts(data_stat$Emotion) <- cbind(EmovsNeu)
contrasts(data_stat$Group) <- cbind(HOAvsMA, YAvsHOA)
contrasts(data_stat$Distractor) <- cbind(onevsthree, threevs5)

baseline <- lme(Capture_first ~ 1, random = ~1|Sujet/Emotion/Distractor, data = data_stat, method = 'ML')
EmotionM <- update(baseline, .~. + Emotion)
DistractorM <- update(EmotionM, .~. + Distractor)
GroupM <- update(DistractorM, .~. + Group)
Emotion_group <- update(GroupM, .~. + Emotion:Group)
Distractor_group <- update(Emotion_group, .~. + Distractor:Group)
Emotion_Distractor <- update(Distractor_group, .~. + Emotion:Distractor)
FullModel <- update(Emotion_Distractor, .~. + Emotion:Distractor:Group)
anova(baseline, EmotionM, DistractorM, GroupM, Emotion_group, Distractor_group, Emotion_Distractor, FullModel)
summary(Distractor_group)
summary(FullModel)

baseline <- gls(Capture_first ~ 1, data = data_stat, method = 'ML')
interceptOnly <- lme(Capture_first ~ 1, random = ~1|Eyetracker, data = data_stat, method = 'ML')
anova(baseline, interceptOnly)"

#### Graphs ####
data_graph <- data[,c('Subjects', 'Group','EMO.1','EMO.3','EMO.5', 'NEU.1', 'NEU.3', 'NEU.5')]

# Adjust means
data_graph_AD <- subset_function_keep(data_graph, data_graph$Group, 'AD')
data_graph_AD <- rmMeanAdjust(data_graph_AD, c(3,4,5,6,7,8), number_lines = 18)
data_graph_HOA <- subset_function_keep(data_graph, data_graph$Group, 'HOA')
data_graph_HOA <- rmMeanAdjust(data_graph_HOA, c(3,4,5,6,7,8), number_lines = 24)
data_graph_YA <- subset_function_keep(data_graph, data_graph$Group, 'YA')
data_graph_YA <- rmMeanAdjust(data_graph_YA, c(3,4,5,6,7,8), number_lines = 35)
data_graph <- rbind(data_graph_AD, data_graph_HOA, data_graph_YA)

data_graph <- melt_function(data_graph,
                                    c('Subjects','Group'),
                                    c('EMO.1','EMO.3','EMO.5', 'NEU.1', 'NEU.3', 'NEU.5'))
data_graph$Emotion <- c(rep('Negative',number_participants*3),
                                rep('Neutral', number_participants*3))
data_graph$Distractor <- c(rep('1',number_participants),
                                   rep('3',number_participants),
                                   rep('5',number_participants),
                                   rep('1',number_participants),
                                   rep('3',number_participants),
                                   rep('5',number_participants))
data_graph <- data_graph[,c('Subjects','Group','value','Emotion','Distractor')]
names(data_graph) <-c('Sujet', 'Group','Capture_first', 'Emotion', 'Distractor')

descstat(data_graph, data_graph$Capture_first, data_graph$Group, data_graph$Emotion)
descstat(data_graph, data_graph$Capture_first, data_graph$Group, data_graph$Distractor)
by(data_graph$Capture_first, list(data_graph$Distractor), stat.desc)
by(data_graph$Capture_first, list(data_graph$Group), stat.desc)
by(data_graph$Capture_first, list(data_graph$Emotion), stat.desc)

qplot(data = data_graph, x = Capture_first, y = Emotion, colour = Emotion, shape = Group)

box_graph(data = data_graph,
          VD = 'Capture_first',
          VI_list = c('Emotion'),
          number_VI = 1,
          number_lines = 462,
          VIbetween = 'Group',
          title_list = c('Group', 'Mean first capture time (ms)', 'Emotion'),
          title_graph = 'Boxplot_capturefirst1.tiff',
          width_spe = 22,
          height_spe = 10,
          dpi_spe = 100,
          yrange = c(0,1500))

box_graph(data = data_graph,
          VD = 'Capture_first',
          VI_list = c('Distractor', 'Emotion'),
          number_VI = 2,
          number_lines = 462,
          VIbetween = 'Group',
          title_list = c('Distractor', 'Mean first capture time (ms)', 'Emotion'),
          title_graph = 'Boxplot_capturefirst2.tiff',
          width_spe = 22,
          height_spe = 10,
          dpi_spe = 100,
          yrange = c(0,1500))

line_graph(data = data_graph,
           VD = 'Capture_first',
           VI_list = c('Emotion'),
           number_VI = 1,
           number_lines = 462,
           VIbetween = 'Group',
           title_list = c('Emotional valence', 'Mean first capture time (ms)', 'Group'),
           title_graph = 'Graph_capturefirst1.tiff',
           width_spe = 22,
           height_spe = 10,
           dpi_spe = 100,
           colours = c('#595959','#cccccc','#999999'),
           yrange = c(0,1100))

line_graph(data = data_graph,
           VD = 'Capture_first',
           VI_list = c('Distractor', 'Emotion'),
           number_VI = 2,
           number_lines = 462,
           VIbetween = 'Group',
           title_list = c('Number of distractors', 'Mean engagement delay (ms)', 'Emotional valence'),
           title_graph = 'Graph_capturefirst2.png',
           width_spe = 22,
           height_spe = 10, 
           dpi_spe = 300,
           colours = c('#FF0000','#999999'),
           yrange = c(0,1100))

bar_graph(data = data_graph,
          VD = 'Capture_first',
          VI_list = c('Emotion'),
          number_VI = 1,
          number_lines = 462,
          VIbetween = 'Group',
          title_list = c('Emotion', 'Mean first capture time (ms)'), 
          title_graph = 'Barplot_capturefirst1.tiff',
          width_spe = 22,
          height_spe = 10,
          dpi_spe = 100,
          colours = c('#595959','#cccccc','#999999'),
          yrange = c(0,1100))

bar_graph(data = data_graph,
          VD = 'Capture_first',
          VI_list = c('Distractor', 'Emotion'),
          number_VI = 2,
          number_lines = 462,
          VIbetween = 'Group',
          title_list = c('Distractor', 'Mean first capture time (ms)', 'Emotion'),
          title_graph = 'Barplot_capturefirst2.tiff',
          width_spe = 22,
          height_spe = 10,
          dpi_spe = 100,
          colours = c('#595959','#cccccc','#999999'),
          yrange = c(0,1100))

#### Assumptions ####
# Normality
norm_test(data_stat,
          data_stat$Group,
          data_stat$Emotion,
          data_stat$Distractor,
          data_stat$Capture_first)
qplot(sample = data_stat$Capture_first, stat = 'qq')

# Homogeneity
leveneTest(data_stat$Capture_first, data_stat$Group, center = median)

# Residuals
modellm <- lm(Capture_first ~ Emotion*Distractor*Group, data = data_stat)
data_resid <- resid_creation(data_stat, modellm)
data_resid <- resid_analysis(data_resid, n_predictors = 3, n_subjects = number_participants, model = modellm)
plot(modellm)
hist(data_resid$standardized.residuals)

#### Influence of educational level ####

data_AD <- subset_function_keep(data_stat, data_stat$Group, 'AD')
contrasts(data_AD$Emotion) <- cbind(EmovsNeu)
contrasts(data_AD$Group) <- cbind(HOAvsMA, YAvsrest)
contrasts(data_AD$Distractor) <- cbind(onevsrest, threevs5)
contrasts(data_AD$Eyetracker) <- cbind(EyelinkvsSmi)
contrasts(data_AD$Educ) <- cbind(LowvsHigh, Verylowvslow, HighvsVeryhigh)
ADModel <- ezANOVA(data = data_AD,
                   dv = .(Capture_first),
                   wid = .(Sujet),
                   between = c(Educ),
                   within = c(Emotion, Distractor),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE)
summary.lm(ADModel$aov$'Sujet:Emotion')
ADModel
confint(ADModel$aov$'Sujet:Emotion')

leveneTest(data_AD$Capture_first, data_AD$Educ, center = median)

# Residuals
modellm <- lm(Capture_first ~ Emotion:Distractor, data = data_AD)
data_resid <- resid_creation(data_AD, modellm)
data_resid <- resid_analysis(data_resid, n_predictors = 2, n_subjects = 18, model = modellm)
plot(modellm)
hist(data_resid$standardized.residuals)
