#### Capture first ####

data = read.csv('./Example/engagement.csv', header=TRUE, sep = ';')
data <- subset_function_remove(data, data$Subjects, '48-E')

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
onevsthree <- c(-0.5,0.5,0)
threevs5 <- c(0,-0.5,0.5)
EyelinkvsSmi <- c(0.5, -0.5)
LowvsHigh <- c(-1,-1,1,1)
Verylowvslow <- c(-1,1,0,0)
HighvsVeryhigh <- c(0,0,-1,1)
contrasts(data_stat$Emotion) <- cbind(EmovsNeu)
#contrasts(data_stat$Group) <- cbind(HOAvsMA, YAvsrest)
#contrasts(data_stat$Distractor) <- cbind(onevsrest, threevs5)
contrasts(data_stat$Eyetracker) <- cbind(EyelinkvsSmi)

contrasts(data_stat$Group) <- inverseMatrixContrast(c(YAvsHOA, HOAvsMA), 3)
contrasts(data_stat$Distractor) <- inverseMatrixContrast(c(onevsthree, threevs5), 3)

# Model
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
printANOVA('engagement_ANOVA.csv', './Example/', ADHOAModel)
confint(ADHOAModel$aov$'Sujet:Emotion')
confint(ADHOAModel$aov$'Sujet:Distractor')

rcontrast(t = 5.012, df = 73)
rcontrast(t = 28.108, df = 146)
rcontrast(t = 10.404, df = 146)
rcontrast(t = 1.192, df = 73)
rcontrast(t = 2.484, df = 73)
rcontrast(t = -3.218, df = 146)
partial_eta_squared(SSn = 1.565677e-05,SSd = 2.604017e-05)
partial_eta_squared(SSn = 1.597533e-06,SSd = 4.960784e-06)
partial_eta_squared(SSn = 1.145434e-04,SSd = 1.053306e-05)
partial_eta_squared(SSn = 9.488037e-07,SSd = 4.960784e-06)
partial_eta_squared(SSn = 8.333254e-07,SSd = 1.097234e-05)

#We compute t tests for group
data_group = data[,c('Subjects', 'Group')]
data_group$Capture_mean = rowMeans(data[,c('RecEMO.1','RecEMO.3','RecEMO.5', 'RecNEU.1', 'RecNEU.3', 'RecNEU.5')])
contrasts(data_group$Group) <- inverseMatrixContrast(c(YAvsHOA, HOAvsMA), 3)
ADHOAModel.aov <- aov(Capture_mean ~ Group, data=data_group)
summary.lm(ADHOAModel.aov)
confint(ADHOAModel.aov)

rcontrast(t = 3.905, df = 73)
rcontrast(t = 2.710, df = 73)

#Not orthogonal contrasts. Gives d and r values.
by(data_stat$Capture_first, list(data_stat$Group), stat.desc)
compute.es::mes(1.662683e-03, 1.834614e-03, 6.744868e-04, 6.629593e-04, 18, 24)
compute.es::mes(2.091285e-03, 1.834614e-03, 5.931089e-04, 6.629593e-04, 35, 24)

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
rcontrast(t = 1.515, df = 73)
by(data_neu$Capture_first, list(data_neu$Group), stat.desc)

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
rcontrast(t = 3.422, df = 73)

#### Graphs ####
data_graph <- data[,c('Subjects', 'Group','EMO.1','EMO.3','EMO.5', 'NEU.1', 'NEU.3', 'NEU.5')]

data_graph$Group <- factor(data_graph$Group, labels = c('YAs', 'HCs', 'Patients with AD'), levels = levels(data_graph$Group)[c(3,2,1)])

# Adjust means
data_graph_AD <- subset_function_keep(data_graph, data_graph$Group, 'Patients with AD')
data_graph_AD <- rmMeanAdjust(data_graph_AD, c(3,4,5,6,7,8), number_lines = 17)
data_graph_HOA <- subset_function_keep(data_graph, data_graph$Group, 'HCs')
data_graph_HOA <- rmMeanAdjust(data_graph_HOA, c(3,4,5,6,7,8), number_lines = 24)
data_graph_YA <- subset_function_keep(data_graph, data_graph$Group, 'YAs')
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
data_graph$Emotion <- factor(data_graph$Emotion)
data_graph$Distractor <- factor(data_graph$Distractor)
data_graph <- data_graph[,c('Subjects','Group','value','Emotion','Distractor')]
names(data_graph) <-c('Sujet', 'Group','Capture_first', 'Emotion', 'Distractor')

descstat(data_graph, data_graph$Capture_first, data_graph$Group, data_graph$Emotion)
descstat(data_graph, data_graph$Capture_first, data_graph$Group, data_graph$Distractor)

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
          yrange = c(0,1500),
          graphPath = './Example')

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
          yrange = c(0,1500),
          graphPath = './Example')

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
           yrange = c(0,1100),
           graphType = 'colour',
           graphPath = './Example')

line_graph(data = data_graph,
           VD = 'Capture_first',
           VI_list = c('Distractor', 'Emotion'),
           number_VI = 2,
           number_lines = 462,
           VIbetween = 'Group',
           title_list = c('Number of distractors', 'Mean engagement delay (ms)', 'Target emotional valence'),
           title_graph = 'engagement.png',
           width_spe = 22,
           height_spe = 10, 
           dpi_spe = 300,
           colours = c('#FF0000','#999999'),
           yrange = c(0,1100),
           graphType = 'colour',
           graphPath = './Example')

bar_graph(data = data_graph,
          VD = 'Capture_first',
          VI_list = c('Emotion'),
          number_VI = 1,
          VIbetween = 'Group',
          title_list = c('Emotion', 'Mean first capture time (ms)'), 
          title_graph = 'Barplot_capturefirst1.tiff',
          width_spe = 22,
          height_spe = 10,
          dpi_spe = 100,
          colours = c('#FF0000','#999999'),
          yrange = c(0,1100),
          graphType = 'colour',
          graphPath = './Example')

bar_graph(data = data_graph,
          VD = 'Capture_first',
          VI_list = c('Distractor', 'Emotion'),
          number_VI = 2,
          VIbetween = 'Group',
          title_list = c('Distractor', 'Mean first capture time (ms)', 'Emotion'),
          title_graph = 'Barplot_capturefirst2.tiff',
          width_spe = 22,
          height_spe = 10,
          dpi_spe = 100,
          colours = c('#595959',"#cccccc"),
          yrange = c(0,1100),
          graphType = 'colour',
          graphPath = './Example')

#### Tables ####

generateTableRes(data_graph,
                 nameVD = list('Engagement delay (in ms))'),
                 listVD = list(data_graph$Capture_first),
                 listVI = list(data_graph$Group, data_graph$Emotion, data_graph$Distractor),
                 filename = 'engagement_Desc.csv',
                 path = './Example/',
                 roundValue = 0)

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
