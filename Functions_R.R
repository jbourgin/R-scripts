rm(list=ls())

#libcurl needs to be installed.
usePackage <- function(i){
  if(! i %in% installed.packages()){
    install.packages(i, dependencies = TRUE)
    }
  require(i, character.only = TRUE)
}

usePackage("car")#Levene
usePackage("ggplot2") # graphs
usePackage("reshape")
usePackage("reshape2")
usePackage("WRS2") #Wilcox  tests
usePackage("pastecs") #stat descriptives
usePackage("ez")#ANOVA
usePackage("nlme")#Modeles multi niveaux
usePackage("predictmeans")#outliers pour modeles multiniveaux (cookD)
usePackage("cowplot")#Graphs side by side
usePackage("tidyr")
usePackage("Hmisc")
#usePackage("rlist")
usePackage("Rfit")
#usePackage("robustlmm")
#usePackage("npIntFactRep")
#usePackage("devtools")
#usePackage("WRS")
usePackage("plm")
usePackage("orcutt")
#usePackage("olsrr")
usePackage("fBasics")
usePackage("ggsignif")
usePackage("compute.es")
#usePackage("emmeans")

"install.packages(c('MASS', 'akima', 'robustbase'))
install.packages(c('cobs', 'robust', 'mgcv', 'scatterplot3d', 'quantreg', 'rrcov', 'lars', 'pwr', 'trimcluster', 'parallel', 'mc2d', 'psych', 'Rfit'))
install.packages('WRS', repos='http://R-Forge.R-project.org', type='source')"
#setwd('D:/Desktop')
setwd('/home/bourginj/Bureau')

#### Shape ####

melt_function <- function(data, id_names, id_measured) {
  melt(data, id = id_names, measured = id_measured)
}

subset_function_remove <- function(data, VI, cat_to_remove) {
  data2 <- subset(data, VI != cat_to_remove)
  return(data2)
}

subset_function_keep <- function(data, VI, cat_to_keep) {
  data2 <- subset(data, VI == cat_to_keep)
  return(data2)
}


#### Graphs ####

# Description des donn?es
descstat <- function (data, VD, VIGroup, VI) {
  by(VD, list(VIGroup, VI), stat.desc)
}

# Th?me graph
theme_perso <- function (base_size = 12, base_family = '') 
{
  theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.text.x = element_text(colour = 'black', size = 18, margin = margin(t = 0.8*11/2)), 
          axis.text.y = element_text(colour = 'black', size = 18, margin = margin(r = 0.8*11/2)),
          axis.title.x = element_text(colour = 'black', size = 22, margin = margin(t = 0.8*25/2)),
          axis.title.y = element_text(colour = 'black', size = 22, angle = 90, margin = margin(r = 0.8*25/2)),
          axis.ticks = element_line(colour = 'black'), 
          legend.text = element_text(size = 15),
          legend.title = element_text(size = 22),
          legend.key = element_rect(colour = 'grey80'), 
          legend.key.height=unit(3,"line"),
          panel.background = element_rect(fill = 'white', colour = NA), 
          panel.border = element_rect(fill = NA, colour = 'grey50'), 
          panel.grid.major = element_line(colour = 'white', size = 0.2), 
          panel.grid.minor = element_line(colour = 'white', size = 0.5), 
          strip.background = element_rect(fill = 'grey90', colour = 'grey50', size = 0.2),
          strip.text = element_text(colour = 'black', size = 22)
    )
}

# Graph line avec deux VI intra et une VI inter
line_graph <- function(data, VD, VI_list, number_VI, number_lines, VIbetween, title_list, title_graph, width_spe, height_spe, dpi_spe, colours, yrange) {
  if (number_VI == 2) {
    line<-ggplot(data, aes_string(VI_list[1],VD, linetype = VI_list[2])) + facet_wrap(VIbetween) + scale_colour_manual(values = colours) + theme_perso()
    line + coord_cartesian(ylim = yrange) + stat_summary(fun.y = mean, geom = 'line', aes_string(group=VI_list[2])) + stat_summary(fun.data = mean_cl_normal, geom = 'pointrange', position = position_dodge(width=0.04)) + labs(x = title_list[1], y = title_list[2], colour = title_list[3])
  } else if (number_VI == 1) {
    line<-ggplot(data, aes_string(VI_list[1],VD, colour = VIbetween)) + scale_colour_manual(values = colours) + theme_perso()
    line + coord_cartesian(ylim = yrange) + stat_summary(fun.y = mean, geom = 'line', aes_string(group=VIbetween)) + stat_summary(fun.data = mean_cl_normal, geom = 'pointrange', position = position_dodge(width=0.04)) + labs(x = title_list[1], y = title_list[2], colour = title_list[3])
  }
  ggsave(title_graph, width = width_spe, height = height_spe, dpi = dpi_spe)
}


# Graph boxplot avec une VI intra et une VI inter
box_graph <- function(data, VD, VI_list, number_VI, number_lines, VIbetween, title_list, title_graph, width_spe, height_spe, dpi_spe, yrange) {
  if (number_VI == 1) {
    boxplot <- ggplot(data,aes_string(VIbetween,VD, colour = VI_list[1]))
    boxplot + coord_cartesian(ylim = yrange) + geom_boxplot() + labs(x = title_list[1], y = title_list[2], colour = title_list[3])
  }
  else if (number_VI == 2) {
    boxplot <- ggplot(data,aes_string(VI_list[1],VD, colour = VI_list[2])) + facet_wrap(VIbetween) + theme_perso()
    boxplot + coord_cartesian(ylim = yrange) + geom_boxplot() + labs(x = title_list[1], y = title_list[2], colour = title_list[3])
  }
  ggsave(title_graph, width = width_spe, height = height_spe, dpi = dpi_spe)
}

# Bar plot avec une VI intra et une VI inter
bar_graph <- function(data, VD, VI_list, number_VI, VIbetween, title_list, title_graph, width_spe, height_spe, dpi_spe, colours, yrange) {
  if (number_VI == 1) {
    bar<-ggplot(data, aes_string(VI_list[1],VD, fill = VI_list[1])) +
      facet_wrap(VIbetween) +
      scale_fill_manual(values = colours, name = title_list[3]) +
      theme_perso()
    bar +
      coord_cartesian(ylim = yrange) +
      stat_summary(fun.y = mean, geom = 'bar', aes_string(group=VI_list[1]), position = 'dodge') +
      stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', position = position_dodge(width=0.90), width=0.2) +
      labs(x = title_list[1], y = title_list[2]) +
      guides(fill = FALSE)
    }
  else if (number_VI == 2) {
    bar<-ggplot(data, aes_string(VI_list[1],VD, fill = VI_list[2])) +
      facet_wrap(VIbetween) +
      scale_fill_manual(values = colours, name = title_list[3]) +
      theme_perso()
    bar +
      coord_cartesian(ylim = yrange) +
      stat_summary(fun.y = mean, geom = 'bar', aes_string(group=VI_list[2]), position = 'dodge') +
      stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', position = position_dodge(width=0.90), width=0.2) +
      labs(x = title_list[1], y = title_list[2], colour = title_list[3]) +
      guides(fill = FALSE)
  }
  ggsave(filename = title_graph, width = width_spe, height = height_spe, dpi = dpi_spe)
}

# Adjust means in within-subject designs
rmMeanAdjust <- function(data, list_nb_columns, number_lines) {
  list_columns <- c()
  pmean <- data.frame('pmean' = c(rep(0,number_lines)))
  for(i in list_nb_columns) {
    pmean <- pmean + data[,i]
    list_columns <- list.append(list_columns, data[,i])
  }
  pmean <- pmean/length(list_nb_columns)
  grandmean <- mean(list_columns)
  adj <- grandmean - pmean
  for(i in list_nb_columns) {
  data[,i] <- data[,i] + adj
  }
  return(data)
}

#### Assumptions ####

# Homog?n?it? (variables inter)
homogeneity_test <- function(data, VD, VI) {
  leveneTest(VD, VI, center = 'median')
}

# Multicollin?arit?. Prendre un mod?le sans interaction.

vif.lme <- function (fit) {
  ## adapted from rms::vif
  v <- vcov(fit)
  nam <- names(fixef(fit))
  ## exclude intercepts
  ns <- sum(1 * (nam == 'Intercept' | nam == '(Intercept)'))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)] }
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v }

#Normality
norm_test <- function(data, VIGroup, VI1, VI2, VD) {
  by(VD, list(VIGroup, VI1, VI2), stat.desc, basic = FALSE, norm = TRUE)
}

shapiro_test <- function(data, VIGroup, VI1, VI2, VD) {
  by(VD, list(VIGroup, VI1, VI2), shapiro.test)
}

# Sphericity
# If sphericity estimates < .75 : Huynh-Feldt correction. Else : Greenhouse-Geisser. If sphericity violated: use Bonferroni for post hoc, else use Tukey.
# Sphericity is available in ezANOVA output.

# Outliers
# Cook's distance : influence of a single case on the model

# Hat value (leverage) : influence of the observed value over the predicted values.

# Residuals
# Independent errors : Durbin-Watson test

# Normality of residuals

# Influential cases
resid_creation <- function(data, model_stat) {
  data2 <- data
  data2$residuals <- resid(model_stat) # Difficult to interpret across different models because measured in the same units as the outcome variable.
  data2$standardized.residuals <- rstandard(model_stat) # Residuals divded by an estimate of their standard deviation.
  data2$cooks.distance <- cooks.distance(model_stat) # Influence of a case on the model.Ok if < 1.
  data2$dfbeta <- dfbeta(model_stat) # Difference between a parameter estimated using all cases and estimated when one case is excluded. Allows to identify cases that have a large influence on the parameters of the model.
  data2$dffit <- dffits(model_stat) # Difference between the predicted value for a case when the model includes that case and when it excludes that case. Should be close to zero.
  data2$leverage <- hatvalues(model_stat) # Influence of a case over the predicted value. (k+1)/n where k is the number of predictors and n the number of participants. Cases with values greater than twice or three times the average are suspect.
  data2$covariance.ratios <- covratio(model_stat)
  return(data2)
}

resid_analysis <- function(data, n_predictors, n_subjects, model) {
  data$large.residual <- data$standardized.residuals>2|data$standardized.residuals<(-2)
  print(ols_test_normality(model))
  res1 <- rstandard(model)
  print(dagoTest(res1))
  print(paste('% of large residuals :', (sum(data$large.residual)/nrow(data))*100)) # Must be inferior to 5%
  print(data[data$large.residual, c('cooks.distance', 'leverage', 'covariance.ratios')]) # Cook < 1, leverage around (k+1)/n
  print('Cooks distance must be below 1.')
  print(paste('Leverage must be less than', 2*(n_predictors + 1)/77))
  print(paste('Covariance ratio must be less than', 1 + (3*(n_predictors+1)/n_subjects), 'and more than', 1 - (3*(n_predictors+1)/n_subjects)))
  print('Independence of errors (Durbin Watson Test) :')
  print(durbinWatsonTest(model)) # Should be close to 2
  return(data)
}

#### Stats ####

#Fonction pour trouver les tailles d'effet des contrastes
rcontrast <- function(t,df)
{
  r <- sqrt(t^2/(t^2+df))
  print(paste('r = ', r))
}

#pour calculer les partial eta squared, on divise SSn / (SSn + SSd). Omega squared may be best (not biased) but complicated. See sjstats (aov format required).
partial_eta_squared <- function(SSn, SSd) {
  eta_squared <- SSn / (SSn + SSd)
  print(paste('Eta? = ', eta_squared))
}

omega_squared_inter <- function(SSn, SSd, dfn, dfd) {
  MSr <- SSd/dfd
  SSt <- SSn + SSd
  omega_squared <- (SSn - (dfn*MSr))/(SSt+MSr)
  print(paste('Omega? = ', omega_squared))
}

ttestfromMeans <- function(x1, x2, sd1, sd2, n1, n2)
{
  df <- n1 + n2 - 2
  poolvar <- (((n1 - 1)*sd1^2) + ((n2 - 1)*sd2^2))/df
  t <- (x1 - x2)/sqrt(poolvar*((1/n1)+(1/n2)))
  sig <- 2*(1-(pt(abs(t),df)))
  paste("t(df = ", df, ") = ", t, ", p = ", sig, sep = "")
}


dCohen <- function(x1, x2, sd1, sd2)
{
  SDpooled = sqrt(((sd1*sd1) + (sd2*sd2))/2)
  d = (x2 - x1)/SDpooled
  paste("d = ", d,  sep = "")
}

#AOV is not appropriate when groups are not balanced (results for main effects of within variables won't be correct). AOV is Type I while ezANOVA is type III.
aovModel <- function(within1, within2, between, dataframe, DV, participant)
  {
  aov(DV~(between*within1*within2)+Error(participant/(within1*within2))+between, data = dataframe)
}

lmeModel <- function(DV, within1, within2, participant, between, dataframe)
{
  lme(DV ~ between*within1*within2, random = ~1|participant/within1/within2,data = dataframe, method = "ML")
  anova(ADHOAModel, type = "marginal")
  Anova(ADHOAModel, type = "III")
}

inverseMatrixContrast <- function(listContrasts, numberElements)
{
  mat <- matrix(c(1,1,1, listContrasts), ncol = 3)
  print(mat)
  Linv <- solve(t(mat))
  inverseMatrix <- Linv[,-1]
  return(inverseMatrix)
}

#### Tables ####

generateTable <- function(data, listVD, VI)
{
  colnames(mytable) <- c("Healthy Controls","Patients with MCI","Patients with AD")
  for(VD in listVD) {
    by(VD, VI, stat.desc)
    PlusMinus(x, y)
  }
  
  write.csv(MyData, file = "MyData.csv")
}