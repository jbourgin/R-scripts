#### Capture first ####

data = read.csv('./Example/engagement.csv', header=TRUE, sep = ';')

# Create a results directory
dir.create(file.path('./Example', 'results'), showWarnings = FALSE)

# Remove subject 48
data <- subset_function_remove(data, data$Subjects, '48-E')

number_participants = length(data[,1])

data_stat <- data[,c('Subjects', 'Group', 'RecEMO.1','RecEMO.3','RecEMO.5', 'RecNEU.1', 'RecNEU.3', 'RecNEU.5')]
data_stat <- melt_function(data_stat,
                                   c('Subjects','Group'),
                                   c('RecEMO.1','RecEMO.3','RecEMO.5', 'RecNEU.1', 'RecNEU.3', 'RecNEU.5'))
data_stat$Emotion <- c(rep('EMO',number_participants*3),
                               rep('NEU', number_participants*3))
data_stat$Distractor <- c(rep('1',number_participants),
                                  rep('3',number_participants),
                                  rep('5',number_participants),
                                  rep('1',number_participants),
                                  rep('3',number_participants),
                                  rep('5',number_participants))
data_stat <- data_stat[,c('Subjects','Group', 'value','Emotion','Distractor')]
names(data_stat) <-c('Sujet', 'Group', 'Capture_first', 'Emotion', 'Distractor')

# Descriptive stats
descstat(data_stat, data_stat$Capture_first, data_stat$Group, data_stat$Emotion)

#### ANOVA ####

data_stat$Group <- factor(data_stat$Group, labels = c('AD', 'HOA', 'YA'), levels = levels(data_stat$Group)[c(1,2,3)])
data_stat$Emotion <- factor(data_stat$Emotion, labels = c('EMO', 'NEU'))
data_stat$Distractor <- factor(data_stat$Distractor, labels = c('1', '3', '5'))

# Contrasts
EmovsNeu <- c(0.5,-0.5)
HOAvsMA <- c(-0.5,0.5,0)
YAvsHOA <- c(0,0.5,-0.5)
YAvsrest <- c(-0.5,-0.5,1)
onevsrest <- c(1,-0.5,-0.5)
onevsthree <- c(-0.5,0.5,0)
threevs5 <- c(0,-0.5,0.5)
contrasts(data_stat$Emotion) <- cbind(EmovsNeu)

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
confint(ADHOAModel$aov$'Sujet:Emotion')
confint(ADHOAModel$aov$'Sujet:Distractor')

# AOV Model
ADHOAaov <- aovModel(within1 = data_stat$Emotion,
                     within2 = data_stat$Distractor,
                     between = data_stat$Group,
                     dataframe = data_stat,
                     DV = data_stat$Capture_first,
                     participant = data_stat$Sujet)
summary(ADHOAaov)

# Print ANOVA results in csv file
printANOVA('engagement_ANOVA.csv', './Example/results/', ADHOAModel)

# Effect size
rcontrast(t = 5.012, df = 73)
partial_eta_squared(SSn = 1.565677e-05,SSd = 2.604017e-05)

# Group effect
data_group = data[,c('Subjects', 'Group')]
data_group$Capture_mean = rowMeans(data[,c('RecEMO.1','RecEMO.3','RecEMO.5', 'RecNEU.1', 'RecNEU.3', 'RecNEU.5')])
contrasts(data_group$Group) <- inverseMatrixContrast(c(YAvsHOA, HOAvsMA), 3)
ADHOAModel.aov <- aov(Capture_mean ~ Group, data=data_group)
summary.lm(ADHOAModel.aov)
confint(ADHOAModel.aov)

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

# Descriptive stats
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
          title_graph = 'box1_engagement.png',
          width_spe = 22,
          height_spe = 10,
          dpi_spe = 100,
          yrange = c(0,1500),
          graphPath = './Example/results')

box_graph(data = data_graph,
          VD = 'Capture_first',
          VI_list = c('Distractor', 'Emotion'),
          number_VI = 2,
          number_lines = 462,
          VIbetween = 'Group',
          title_list = c('Distractor', 'Mean first capture time (ms)', 'Emotion'),
          title_graph = 'box2_engagement.png',
          width_spe = 22,
          height_spe = 10,
          dpi_spe = 100,
          yrange = c(0,1500),
          graphPath = './Example/results')

line_graph(data = data_graph,
           VD = 'Capture_first',
           VI_list = c('Emotion'),
           number_VI = 1,
           number_lines = 462,
           VIbetween = 'Group',
           title_list = c('Emotional valence', 'Mean first capture time (ms)', 'Group'),
           title_graph = 'line1_engagement.png',
           width_spe = 22,
           height_spe = 10,
           dpi_spe = 100,
           colours = c('#595959','#cccccc','#999999'),
           yrange = c(0,1100),
           graphType = 'colour',
           graphPath = './Example/results')

line_graph(data = data_graph,
           VD = 'Capture_first',
           VI_list = c('Distractor', 'Emotion'),
           number_VI = 2,
           number_lines = 462,
           VIbetween = 'Group',
           title_list = c('Number of distractors', 'Mean engagement delay (ms)', 'Target emotional valence'),
           title_graph = 'line2_engagement.png',
           width_spe = 22,
           height_spe = 10, 
           dpi_spe = 300,
           colours = c('#FF0000','#999999'),
           yrange = c(0,1100),
           graphType = 'colour',
           graphPath = './Example/results')

bar_graph(data = data_graph,
          VD = 'Capture_first',
          VI_list = c('Distractor', 'Emotion'),
          number_VI = 2,
          VIbetween = 'Group',
          title_list = c('Distractor', 'Mean first capture time (ms)', 'Emotion'),
          title_graph = 'bar_engagement.png',
          width_spe = 22,
          height_spe = 10,
          dpi_spe = 100,
          colours = c('#595959',"#cccccc"),
          yrange = c(0,1100),
          graphType = 'colour',
          graphPath = './Example/results')

#### Tables ####

generateTableRes(data_graph,
                 nameVD = list('Engagement delay (in ms))'),
                 listVD = list(data_graph$Capture_first),
                 listVI = list(data_graph$Group, data_graph$Emotion, data_graph$Distractor),
                 filename = 'engagement_Desc.csv',
                 path = './Example/results/',
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
homogeneity_test(data_stat, data_stat$Capture_first, data_stat$Group)

# Residuals
modellm <- lm(Capture_first ~ Emotion*Distractor*Group, data = data_stat)
data_resid <- resid_creation(data_stat, modellm)
data_resid <- resid_analysis(data_resid, n_predictors = 3, n_subjects = number_participants, model = modellm)
plot(modellm)
hist(data_resid$standardized.residuals)
