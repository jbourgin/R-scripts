#### Errors ####
data <- read.csv("./Example/errors.csv", header=TRUE, sep = ";")

# Create a results directory
dir.create(file.path('./Example', 'results'), showWarnings = FALSE)

number_participants = length(data[,1])

data_stat <- data[,c("Sujet","Group","SquareNeg","SquareNeu","SquareP")]
data_stat <- melt(data_stat,
                  id = c("Sujet","Group"),
                  measured = c("SquareNeg","SquareNeu","SquareP"))
names(data_stat) <-c("Sujet","Group","Emotion","Error_rate")

#### ANOVA ####

data_stat$Emotion <- factor(data_stat$Emotion)
data_stat$Sujet <- factor(data_stat$Sujet)
data_stat$Group <- factor(data_stat$Group, labels = c("HCs", "Patients with AD"))

# Contrasts
EmovsNeu <- c(-1/3,2/3,-1/3)
NvsP <- c(0.5,0,-0.5)
Gp1vsGp2 <- c(-0.5,0.5)
NvsNeu <- c(1,0,0)
PvsNeu <- c(0,0,1)
contrasts(data_stat$Emotion) <- cbind(EmovsNeu, NvsP)
contrasts(data_stat$Group) <- cbind(Gp1vsGp2)

# Model
ADHOAModel <- ezANOVA(data = data_stat,
                      dv = .(Error_rate),
                      wid = .(Sujet),
                      between = .(Group),
                      within = .(Emotion),
                      type = 3,
                      detailed = TRUE,
                      return_aov = TRUE)
summary.lm(ADHOAModel$aov$'Sujet:Emotion')
ADHOAModel
confint(ADHOAModel$aov$'Sujet:Emotion')

# Print ANOVA results in csv file
printANOVA('errors_ANOVA.csv', './Example/results/', ADHOAModel)

# Effect size
rcontrast(t = 2.037,df = 78)

# Simple effect
HOAData <- subset(data_stat, Group == "HCs")

HOAModel <- ezANOVA(data = HOAData,
                    dv = .(Error_rate),
                    wid = .(Sujet),
                    within = .(Emotion),
                    type = 3,
                    detailed = TRUE,
                    return_aov = TRUE)
HOAModel
summary.lm(HOAModel$aov$'Sujet:Emotion')
confint(HOAModel$aov$'Sujet:Emotion')

# Effect size
rcontrast(t = 2.207, df = 48)
partial_eta_squared(SSn = 3.285286316,SSd = 5.1997995)

#### Graphs ####

data_graph <- data[,c("Sujet","Group","Negative","Neutral","Positive")]

data_graph$Group <- factor(data_graph$Group, labels = c('HCs', 'Patients with AD'), levels = levels(data_graph$Group)[c(2,1)])

# Adjust means
data_graph_AD <- subset_function_keep(data_graph, data_graph$Group, 'Patients with AD')
data_graph_AD <- rmMeanAdjust(data_graph_AD, c(3,4,5), number_lines = 16)
data_graph_HOA <- subset_function_keep(data_graph, data_graph$Group, 'HCs')
data_graph_HOA <- rmMeanAdjust(data_graph_HOA, c(3,4,5), number_lines = 25)
data_graph <- rbind(data_graph_AD, data_graph_HOA)

data_graph <- melt(data_graph,
                   id = c("Sujet","Group"),
                   measured = c("Negative","Neutral","Positive"))
names(data_graph) <-c("Sujet","Group","Emotion","Error_rate")

# Descriptive stat
descstat(data_graph, data_graph$Error_rate, data_graph$Group, data_graph$Emotion)

data_graph$Error_rate <- data_graph$Error_rate * 100
data_graph$Emotion = factor(data_graph$Emotion)

bar_graph(data = data_graph,
          VD = 'Error_rate',
          VI_list = c('Emotion'),
          number_VI = 1,
          VIbetween = 'Group',
          title_list = c('Emotional valence', 'Mean error rate in AS task'), 
          title_graph = 'bar_errors.png',
          width_spe = 22,
          height_spe = 10,
          dpi_spe = 100,
          colours = c('#FF0000','#999999','#00e64d'),
          yrange = c(0,70),
          graphType = 'colour',
          graphPath = './Example/results',
          groups = c("HCs"),
          startx = c("Negative"),
          endx = c("Neutral"),
          labely = c(35),
          labels = c("*"),
          tlength = 0.02)

#### Tables ####

generateTableRes(data_graph,
                 nameVD = list('Mean error rate in AS task'),
                 listVD = list(data_graph$Error_rate),
                 listVI = list(data_graph$Group, data_graph$Emotion),
                 filename = 'errors_Desc.csv',
                 path = './Example/results/',
                 roundValue = 0)

#### Assumptions ####
# Homogeneity
homogeneity_test(data_stat, data_stat$Error_rate, data_stat$Group)

# Residuals
modellm <- lm(Error_rate ~ Emotion*Group, data = data_stat)
data_resid <- resid_creation(data_stat, modellm)
data_resid <- resid_analysis(data_resid, n_predictors = 2, n_subjects = number_participants, model = modellm)
plot(modellm)
hist(data_resid$standardized.residuals)
