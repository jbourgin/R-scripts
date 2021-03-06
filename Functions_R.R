# Done by Jessica Bourgin, University Savoie Mont Blanc
# 4 February 2019
# jbourgin.github.io

# ---------------------------------------------------------------- --------

rm(list=ls())

#First install r. To get recent version of r:
#sudo apt install apt-transport-https software-properties-common
#sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
#sudo add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/'
#sudo apt update
#sudo apt install r-base
#Si pilote propriétaire (ex: nvidia) non utilisé, Rstudio va planter. Le sélectionner dans le gestionnaire de pilotes.
#libcurl needs to be installed.
#sudo apt install build-essential libcurl4-gnutls-dev libxml2-dev libssl-dev
#In Ubuntu, first do :
#sudo apt-get update
#sudo apt-get install libxml2-dev
#install.packages('devtools')
#To install WRS:
#install.packages('remotes')
#install.packages("WRS", repos="http://R-Forge.R-project.org")
usePackage <- function(i){
  if(! i %in% installed.packages()){
    install.packages(i, dependencies = TRUE)
    }
  require(i, character.only = TRUE)
}

usePackage("afex")
usePackage("someMTP")
usePackage("WRS2")
usePackage("latex2exp") # For Latex writing
usePackage("sfsmisc") # For p values in rlm tests
usePackage("car")#Levene
#usePackage("multcomp") # For posthoc
usePackage('DescTools') # For posthoc
#usePackage("XLConnect")#To export output to csv
usePackage("ggplot2") # graphs
usePackage("reshape")
usePackage("reshape2")
usePackage("rio") # To convert from xls to csv
usePackage("ggsignif") # To write p signif on graphs
#usePackage("WRS2") #Wilcox  tests
usePackage("pastecs") #stat descriptives
usePackage("ez")#ANOVA
usePackage("nlme")#Modeles multi niveaux
usePackage("predictmeans")#outliers pour modeles multiniveaux (cookD)
usePackage("cowplot")#Graphs side by side
usePackage("tidyr")
usePackage("Hmisc")
usePackage("rlist")#To manage lists
usePackage("Rfit")
#usePackage("robustlmm")
#usePackage("npIntFactRep")
#usePackage("devtools")
usePackage("WRS")
usePackage("plm")
usePackage("orcutt")
usePackage("olsrr")
usePackage("fBasics")
usePackage("ggsignif")
usePackage("compute.es")
usePackage("psycho") # Tests Crawford bayésiens.
#usePackage("emmeans")

"install.packages(c('MASS', 'akima', 'robustbase'))
install.packages(c('cobs', 'robust', 'mgcv', 'scatterplot3d', 'quantreg', 'rrcov', 'lars', 'pwr', 'trimcluster', 'parallel', 'mc2d', 'psych', 'Rfit'))
install.packages('WRS', repos='http://R-Forge.R-project.org', type='source')"
#setwd('D:/Desktop')
#setwd('/home/bourginj/Bureau')

#### Shape ####

# Melt dataframe
melt_function <- function(data, id_names, id_measured) {
  melt(data, id = id_names, measured = id_measured)
}

# Remove category from dataframe
subset_function_remove <- function(data, VI, cat_to_remove) {
  data2 <- subset(data, VI != cat_to_remove)
  return(data2)
}

# Keep category in dataframe and remove the rest
subset_function_keep <- function(data, VI, cat_to_keep) {
  data2 <- subset(data, VI == cat_to_keep)
  return(data2)
}


#### Graphs ####

# Data description
descstat <- function (data, VD, VIGroup, VI) {
  by(VD, list(VIGroup, VI), stat.desc)
}

# Figure theme
theme_perso <- function (base_size = 12, base_family = '')
{
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.text.x = element_text(colour = 'black', size = 25, margin = margin(t = 0.8*11/2)),
          axis.text.y = element_text(colour = 'black', size = 25, margin = margin(r = 0.8*11/2)),
          axis.title.x = element_text(colour = 'black', size = 30, margin = margin(t = 0.8*25/2)),
          axis.title.y = element_text(colour = 'black', size = 30, angle = 90, margin = margin(r = 0.8*25/2)),
          axis.ticks = element_line(colour = 'black'),
          legend.text = element_text(size = 25),
          legend.title = element_text(size = 30),
          legend.key = element_rect(colour = 'grey80'),
          legend.key.height=unit(3,"line"),
          panel.background = element_rect(fill = 'white', colour = NA),
          panel.border = element_rect(fill = NA, colour = 'grey50'),
          panel.grid.major = element_line(colour = 'white', size = 0.2),
          panel.grid.minor = element_line(colour = 'white', size = 0.5),
          strip.background = element_rect(fill = 'grey90', colour = 'grey50', size = 0.2),
          strip.text = element_text(colour = 'black', size = 30)
    )
}

# Figure theme
theme_french <- function (base_size = 12, base_family = '')
{
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.text.x = element_text(colour = 'black', size = 25, margin = margin(t = 0.8*11/2)),
          axis.text.y = element_text(colour = 'black', size = 25, margin = margin(r = 0.8*11/2)),
          axis.title.x = element_text(colour = 'black', size = 30, margin = margin(t = 0.8*25/2)),
          axis.title.y = element_text(colour = 'black', size = 30, angle = 90, margin = margin(r = 0.8*25/2)),
          axis.ticks = element_line(colour = 'black'),
          legend.text = element_text(size = 25),
          legend.title = element_text(size = 30),
          legend.key = element_rect(colour = 'grey80'),
          legend.key.height=unit(3,"line"),
          panel.background = element_rect(fill = 'white', colour = NA),
          panel.border = element_rect(fill = NA, colour = 'grey50'),
          panel.grid.major = element_line(colour = 'white', size = 0.2),
          panel.grid.minor = element_line(colour = 'white', size = 0.5),
          strip.background = element_rect(fill = 'grey90', colour = 'grey50', size = 0.2),
          strip.text = element_text(colour = 'black', size = 25, margin = margin(0.3,0,0.3,0, "cm"))# title of each panel
    )
}

# Scatterplot with regression lines
scatterReg <- function(data, VD, covar, VI, colours) {
  line<-ggplot(data, aes(x=covar,y=VD))
  line + geom_point(aes(color = VI, shape = VI)) + geom_smooth(aes(color = VI, fill = VI), method = "lm") +
    geom_rug(aes(color = VI)) +
    scale_color_manual(values = colours) +
    scale_fill_manual(values = colours)
}

# Line figure in color or black and white, and with 2 or 3 independent variables
line_graph <- function(data, VD, VI_list, number_VI, number_lines, VIbetween, title_list, title_graph, width_spe, height_spe, dpi_spe, colours, yrange, graphType, graphPath) {
  if (graphType == 'colour') {
    if (number_VI == 2) {
      line<-ggplot(data, aes_string(VI_list[1],VD, colour = VI_list[2])) + facet_wrap(VIbetween) + scale_colour_manual(values = colours) + theme_perso()
      line + coord_cartesian(ylim = yrange) +
        stat_summary(fun.y = mean, geom = 'line', aes_string(group=VI_list[2])) +
        stat_summary(fun.data = mean_cl_normal, geom = 'pointrange', position = position_dodge(width=0.04)) +
        labs(x = title_list[1], y = title_list[2], colour = title_list[3])
    } else if (number_VI == 1) {
      line<-ggplot(data, aes_string(VI_list[1],VD, colour = VIbetween)) + scale_colour_manual(values = colours) + theme_perso()
      line + coord_cartesian(ylim = yrange) +
        stat_summary(fun.y = mean, geom = 'line', aes_string(group=VIbetween)) +
        stat_summary(fun.data = mean_cl_normal, geom = 'pointrange', position = position_dodge(width=0.04)) +
        labs(x = title_list[1], y = title_list[2], colour = title_list[3])
    }
  } else
  {
    if (number_VI == 2) {
      line<-ggplot(data, aes_string(VI_list[1],VD, linetype = VI_list[2])) + facet_wrap(VIbetween) + scale_colour_manual(values = colours) + theme_perso()
      line + coord_cartesian(ylim = yrange) + stat_summary(fun.y = mean, geom = 'line', aes_string(group=VI_list[2])) + stat_summary(fun.data = mean_cl_normal, geom = 'pointrange', position = position_dodge(width=0.04)) + labs(x = title_list[1], y = title_list[2], colour = title_list[3])
    } else if (number_VI == 1) {
      line<-ggplot(data, aes_string(VI_list[1],VD, linetype = VIbetween)) + scale_colour_manual(values = colours) + theme_perso()
      line + coord_cartesian(ylim = yrange) + stat_summary(fun.y = mean, geom = 'line', aes_string(group=VIbetween)) + stat_summary(fun.data = mean_cl_normal, geom = 'pointrange', position = position_dodge(width=0.04)) + labs(x = title_list[1], y = title_list[2], colour = title_list[3])
    }
  }
  ggsave(title_graph, path = graphPath, width = width_spe, height = height_spe, dpi = dpi_spe)
}


# Box figure in color or black and white, and with 2 or 3 independent variables
box_graph <- function(data, VD, VI_list, number_VI, number_lines, VIbetween, title_list, title_graph, width_spe, height_spe, dpi_spe, yrange, graphPath) {
  if (number_VI == 1) {
    boxplot <- ggplot(data,aes_string(VIbetween,VD, colour = VI_list[1]))
    boxplot + coord_cartesian(ylim = yrange) + geom_boxplot() + labs(x = title_list[1], y = title_list[2], colour = title_list[3])
  }
  else if (number_VI == 2) {
    boxplot <- ggplot(data,aes_string(VI_list[1],VD, colour = VI_list[2])) + facet_wrap(VIbetween) + theme_perso()
    boxplot + coord_cartesian(ylim = yrange) + geom_boxplot() + labs(x = title_list[1], y = title_list[2], colour = title_list[3])
  }
  ggsave(title_graph, path = graphPath, width = width_spe, height = height_spe, dpi = dpi_spe)
}

# Scatterplot figure in color or black and white, and with 2 or 3 independent variables. Check shape.
scatter_graph <- function(data, VD, VI_list, number_VI, number_lines, VIbetween, title_list, title_graph, width_spe, height_spe, dpi_spe, colours, yrange, graphPath,
                          groups = c(), startx = c(), endx = c(), labely = c(), labels = c(), tlength = 0.005, noVIBetween = 0, renameLabels = 0, labelsx = c(), signifData = 0, shapes = c(), theme=theme_perso()) {
  if (noVIBetween == 1) {
    annot_df <- data.frame(Group = groups,
                           start = startx,
                           end = endx,
                           y = labely,
                           label = labels)
    scatterplot <- ggplot(data,aes_string(VI_list[1],VD, shape = VI_list[1], colour = VI_list[1])) +
      theme +
      scale_colour_manual(values = colours)
    if (signifData == 1) {
      scatterplot <- scatterplot +
      geom_signif(data = annot_df,
                  inherit.aes = FALSE,
                  mapping = aes(xmin=start, xmax=end, annotations=label, y_position=y),
                  tip_length = tlength,
                  textsize = 18,
                  manual = TRUE,
                  vjust = 0.6)
    }
    if (renameLabels == 1) {
      scatterplot <- scatterplot +
      scale_x_discrete(labels=labelsx)
    }
    scatterplot + coord_cartesian(ylim = yrange) + geom_point(size=3) + labs(x = title_list[1], y = title_list[2]) +
      guides(shape = FALSE) +
      guides(colour = FALSE) +
      stat_summary(fun.data=mean_cl_normal,geom="errorbar", color = "red", width=0.12, size=1.5, position=position_dodge(0.2)) +
      stat_summary(fun.y=mean, geom="point", color = "red", size=4)
  }
  else if (number_VI == 1) {
    annot_df <- data.frame(Group = groups,
      start = startx,
      end = endx,
      y = labely,
      label = labels)
    scatterplot <- ggplot(data,aes_string(VI_list[1],VD, shape = VI_list[1], colour = VI_list[1])) +
      facet_wrap(VIbetween) +
      geom_signif(data = annot_df,
                  inherit.aes = FALSE,
                  aes(xmin=start, xmax=end, annotations=label, y_position=y),
                  tip_length = tlength,
                  textsize = 18,
                  manual = TRUE,
                  vjust = 0.6) +
      theme +
      scale_colour_manual(values = colours)
    scatterplot + coord_cartesian(ylim = yrange) + geom_point(size=3) + labs(x = title_list[1], y = title_list[2]) +
      guides(shape = FALSE) +
      guides(colour = FALSE) +
      stat_summary(fun.data=mean_cl_normal,geom="errorbar", color = "red", width=0.12, size=1.5, position=position_dodge(0.2)) +
      stat_summary(fun.y=mean, geom="point", color = "red", size=4)
  }
  else if (number_VI == 2) {
    scatterplot <- ggplot(data,aes_string(VI_list[1],VD, shape = VI_list[2], colour = VI_list[2])) +
      scale_colour_manual(name=title_list[3], values = colours) +
      scale_shape_manual(name=title_list[3], values = shapes) +
      facet_wrap(VIbetween) +
      theme +
      geom_jitter(position=position_dodge(0.6), size=3)
    scatterplot + coord_cartesian(ylim = yrange) + labs(x = title_list[1], y = title_list[2], colour = title_list[3]) +
      #guides(shape = FALSE) +
      #guides(colour = FALSE) +
      stat_summary(fun.data=mean_cl_normal,geom="errorbar", width=0.12, size=1.5, position=position_dodge(0.2)) +
      stat_summary(fun.y=mean, geom="point", position=position_dodge(0.2), size=4)
  }
  ggsave(title_graph, path = graphPath, width = width_spe, height = height_spe, dpi = dpi_spe)
}

# Bar figure in color or black and white, and with 2 or 3 independent variables. Implements annotations (bars showing statistical differences between conditions)
# In annot_df, the first parameter (Group currently) must be changed to match the name of the facet variable.
bar_graph <- function(data, VD, VI_list, number_VI, VIbetween, title_list, title_graph, width_spe, height_spe,
                      dpi_spe, colours, yrange, graphType, graphPath, groups = c(), startx = c(), endx = c(), labely = c(), labels = c(), tlength = 0.005) {
  if (graphType == "BW") {
    colours = c('#595959','#cccccc','#999999')
  }
  annot_df <- data.frame(Group = groups,
                         start = startx,
                         end = endx,
                         y = labely,
                         label = labels)
  if (number_VI == 1) {
    bar<-ggplot(data, aes_string(VI_list[1],VD, fill = VI_list[1])) +
      scale_fill_manual(values = colours, name = title_list[3]) +
      geom_signif(data = annot_df,
                  inherit.aes = FALSE,
                  aes(xmin=start, xmax=end, annotations=label, y_position=y),
                  tip_length = tlength,
                  textsize = 18,
                  manual = TRUE,
                  vjust = 0.6) +
      facet_wrap(VIbetween) +
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
  ggsave(title_graph, path = graphPath, width = width_spe, height = height_spe, dpi = dpi_spe)
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

# Homogeneity
homogeneity_test <- function(data, VD, VI) {
  leveneTest(VD, VI, center = 'median')
}

# Multicollinearity (for models without interaction)

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

# Normality
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

CrawfordHowell <- function(case, control){
  tval <- (case - mean(control)) / (sd(control)*sqrt((length(control)+1) / length(control)))
  degfree <- length(control)-1
  pval <- 2*(1-pt(abs(tval), df=degfree)) #two-tailed p-value
  result <- data.frame(t = tval, df = degfree, p=pval)
  return(result)
}

# Rajout amy/hipp a la mano pour le moment avant Category
getMatrixSC <- function(data_test, first_group, second_group, testToKeep, numberColNonResult, title, toremove = c())
{
  data_test$Category <- factor(data_test$Category)
  data_test$char.gender <- factor(data_test$char.gender)
  c1 <- c(-1, 1, 0) # AD vs CN
  c2 <- c(0, 1, -1) # MCI vs CN
  mat <- cbind(c1,c2)
  contrasts(data_test$Category) <- mat
  # Construction of plist for seed level fdr
  pmatrix = matrix(0, 58,58)
  Fmatrix = matrix(0, 58,58)
  emptymatrixrow = matrix(0,8,56)
  emptymatrixcol = matrix(0,64,8)
  for(i in (1+numberColNonResult):(ncol(data_test))){
    categoryWithoutX <- gsub('X','',names(data_test[i]))
    nline = strtoi(strsplit(categoryWithoutX,'_')[[1]][1])
    ncolumn = strtoi(strsplit(categoryWithoutX,'_')[[1]][2])
    model <- aov(data_test[,i] ~ char.age+char.gender+Educ+Tissue_IC+Category, 
                 data = data_test, na.action=na.omit)
    print(summary(model))
    #model2 <- aov(data_test[,i] ~ data_test$Category, data = data_test, na.action=na.omit)
    sumaov <- summary.aov(model, split=list(Category=list("AD vs CN"=1, "MCI vs CN"=2)))
    print(sumaov)
    #tukeytest <- TukeyHSD(model2)
    #print(tukeytest)
    #ptokeep <- tukeytest[[1]][testToKeep]
    ptokeep <- sumaov[[1]][[5]][testToKeep]
    ftokeep <- sumaov[[1]][[4]][testToKeep]
    print(paste('P to keep: ',ptokeep))
    print(paste('F to keep: ',ftokeep))
    #sum_test = unlist(summary(model))
    #pmatrix[nline, ncolumn] = sum_test["Pr(>F)1"]
    #pmatrix[ncolumn, nline] = sum_test["Pr(>F)1"]
    pmatrix[nline, ncolumn] = ptokeep
    pmatrix[ncolumn, nline] = ptokeep
    #Fmatrix[nline, ncolumn] = sum_test["F value1"]
    #Fmatrix[ncolumn, nline] = sum_test["F value1"]
    Fmatrix[nline, ncolumn] = ftokeep
    Fmatrix[ncolumn, nline] = ftokeep
  }
  
  # FDR adjustment
  for (i in 1:nrow(pmatrix)){
    pmatrix[i,] <- p.adjust.w(pmatrix[i,], method = 'BHfwe', n = ncol(pmatrix))
  }
  
  # print value below .05 FDR
  threshold_list <- c()
  for(i in 1:nrow(pmatrix)){
    for(j in 1:ncol(pmatrix)){
      name_column <- 'None'
      if (paste('X',i,'_',j,sep='') %in% colnames(data_test)){
        name_column <- paste('X',i,'_',j,sep='')
      }
      else if (paste('X',j,'_',i,sep='') %in% colnames(data_test)){
        name_column <- paste('X',j,'_',i,sep='')
      }
      if (name_column != 'None'){
        test <- by(data_test[[name_column]], data_test$Category, stat.desc)
        if (test[[first_group]][['mean']] - test[[second_group]][['mean']] < 0){
          Fmatrix[i,j] = - Fmatrix[i,j]
        }
        if(pmatrix[i,j] < 0.05){
            threshold_list <- c(threshold_list,Fmatrix[i,j])
            print(paste('For column ',
                        name_column,
                        'F value',
                        Fmatrix[i,j],
                        ' Difference ',
                        names(test[first_group]),
                        '-',
                        names(test[second_group]),
                        ':',
                        test[[first_group]][['mean']] - test[[second_group]][['mean']],
                        ' with p ',
                        (pmatrix[i,j])))
        }
      }
    }
  }
  print(ncol(pmatrix))
  print(data_test$Category)
  if (length(toremove) > 0){
    Fmatrix <- Fmatrix[,-toremove]
    Fmatrix <- Fmatrix[-toremove,]
  }
  Fmatrix = rbind(emptymatrixrow,Fmatrix)
  Fmatrix = cbind(emptymatrixcol,Fmatrix)
  write.csv(Fmatrix,paste('./tables/',title,'.csv',sep=""),row.names=FALSE)
  if (max(threshold_list) < 0) {
    threshold = max(threshold_list)
  }
  else {
    threshold = min(abs(threshold_list))
  }
  print(paste('Threshold is ', threshold))
  return(list(pmatrix,Fmatrix))
}

# To adapt
getMatrixAmySc<- function(data_test, VI, numberColNonResult, title){
  pmatrix = matrix(0, 25,25)
  Fmatrix = matrix(0, 25, 25)
  for(i in (1+numberColNonResult):(ncol(data_test))){
    categoryWithoutX <- gsub('X','',names(data_test[i]))
    nline = strtoi(strsplit(categoryWithoutX,'_')[[1]][1])
    ncolumn = strtoi(strsplit(categoryWithoutX,'_')[[1]][2])
    model <- aov(data_test[,i] ~ data_test[[VI]]+Educ+char.age+char.gender, data = data_test, na.action=na.omit)
    #+data_test$char.gender+data_test$char.age+data_test$Tissue_IC
    print(summary(model))
    sum_test = unlist(summary(model))
    print(sum_test)
    pmatrix[nline, ncolumn] = sum_test["Pr(>F)1"]
    pmatrix[ncolumn, nline] = sum_test["Pr(>F)1"]
    Fmatrix[nline, ncolumn] = sum_test["F value1"]
    Fmatrix[ncolumn, nline] = sum_test["F value1"]
    print(paste("P to keep : ", sum_test["Pr(>F)1"]))
    print(paste("F to keep : ", sum_test["F value1"]))
  }
  
  # FDR adjustment
  for (i in 1:nrow(pmatrix)){
    pmatrix[i,] <- p.adjust.w(pmatrix[i,], method = 'BHfwe', n = ncol(pmatrix))
  }
  
  # print value below .05 fdr threshold
  for(i in 1:nrow(pmatrix)){
    for(j in 1:ncol(pmatrix)){
      if (is.na(pmatrix[i,j])){
        print('We do nothing')
      }
      else {
        name_column <- 'None'
        if (paste('X',i,'_',j,sep='') %in% colnames(data_test)){
          name_column <- paste('X',i,'_',j,sep='')
        }
        else if (paste('X',j,'_',i,sep='') %in% colnames(data_test)){
          name_column <- paste('X',j,'_',i,sep='')
        }
        if (name_column != 'None'){
          cortest <- cor.test(data_test[[VI]], data_test[[name_column]])
          if (cortest[['estimate']] < 0){
            Fmatrix[i,j] = - Fmatrix[i,j]
          }
          if(pmatrix[i,j] < 0.1){
            print(paste('For column ',
                        name_column,
                        'F value',
                        Fmatrix[i,j],
                        'R value',
                        cortest[["estimate"]],
                        ' with p ',
                        (pmatrix[i,j])))
          }
        }
      }
    }
  }
  print(ncol(pmatrix))
  write.csv(Fmatrix, paste('./tables/',title,'.csv',sep=""), row.names = FALSE)
  return(pmatrix)
}

# Function to test the differences for all levels of a VI on a VD. Generates a table and a figure.
meanVD <- function(VD, VI, title, data_stat)
{
  
  model <- aov(data_stat[[VD]] ~ data_stat[[VI]], data = data_stat)
  print(summary(model))
  print(TukeyHSD(model))
  
  #### Graphs ####
  
  scatter<-ggplot(data_stat, aes_string(VI,VD, shape = VI, colour = VI)) +
    scale_colour_manual(values = c('#000000','#696969','#cccccc','#000000')) +
    theme_perso()
  scatter +
    #coord_cartesian(ylim = yrange) +
    geom_point(size=3) +
    stat_summary(fun.data=mean_cl_normal,geom="errorbar", color = "red", width=0.12, size=1.5, position=position_dodge(0.2)) +
    stat_summary(fun.y=mean, geom="point", color = "red", size=4) +
    #stat_summary(fun.y = mean, geom = 'bar', aes_string(group=VI), position = 'dodge') +
    #stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', position = position_dodge(width=0.90), width=0.2) +
    labs(x = 'VI', y = 'VD') +
    guides(colour = FALSE) +
    guides(shape = FALSE)
  ggsave(paste(title, '.png'), path ='./figures', width = 11, height = 8)
  
  #### Tables ####
  
  generateTableRes(data_stat,
                   nameVD = list(VD),
                   listVD = list(data_stat[VD]),
                   listVI = list(data_stat[[VI]]),
                   filename = paste(title, '.csv'),
                   path = './tables/',
                   roundValue = 2,
                   title = TRUE)
  
  by(data_stat[[VD]], data_stat[[VI]], stat.desc)
  
  #### Assumptions ####
  
  print(by(data_stat[[VD]], data_stat[[VI]], stat.desc, basic = FALSE, norm = TRUE))
  #qplot(sample = data_stat[[VD]], stat = 'qq')
  # Homogeneity
  print(leveneTest(data_stat[[VD]], data_stat[[VI]], center = "median"))
  
  # Residuals
  #modellm <- lm(VD ~ VI, data = data_stat)
  #data_resid <- resid_creation(data_stat, modellm)
  #data_resid <- resid_analysis(data_resid, n_predictors = 1, n_subjects = number_participants, model = modellm)
  #plot(modellm)
  #hist(data_resid$standardized.residuals)
}


# Function to test the differences for all levels of a VI on a VD. Generates a table and a figure.
VDpointplot <- function(VD, VD2, title, titleVD = "VD", titleVD2 = "VD2", data_stat)
{
  
  #### Graphs ####
  pointplot <- ggplot(data_stat, aes_string(VD, VD2))  + theme_perso()
  pointplot + 
    geom_point() +
    scale_shape_manual(values=c(19,17)) +
    #scale_colour_manual(values=c('#000000','#808080')) +
    labs(x = titleVD, y = titleVD2) +
    #theme(axis.title.y = element_text(colour = 'black', size = 20, angle = 90, margin = margin(r = 0.8*25/2))) +
    geom_smooth(method = 'lm', alpha = 0.1)
  ggsave(paste(title, '.png'), path ='./figures', width = 11, height = 8)
  
}

# Function to test the differences for all levels of a VI on a VD. Generates a table and a figure.
meanVDScatter <- function(VD, VI, title, titleVI = "Diagnosis", titleVD = "Score", data_stat, groups = c(), startx = c(), endx = c(), labely = c(), labels = c(), renameLabels=0, labelsx = c(), number_participants, yrange)
{
  
  model <- aov(data_stat[[VD]] ~ data_stat[[VI]], data = data_stat)
  print(summary(model))
  print(TukeyHSD(model))
  
  #### Graphs ####
  
  scatter_graph(data = data_stat,
                VD = VD,
                VI_list = c(VI),
                number_VI = 1,
                number_lines = number_participants,
                VIbetween = VI,
                title_list = c(titleVI, titleVD, titleVI),
                title_graph = paste(title, '.png'),
                width_spe = 11,
                height_spe = 8,
                dpi_spe = 100,
                colours = c('#000000','#696969','#cccccc','#000000'),
                yrange = yrange,
                graphPath = './figures',
                groups = groups,
                startx = startx,
                endx = endx,
                labely = labely,
                labels = labels,
                tlength = 0.02,
                noVIBetween = 1,
                renameLabels = renameLabels,
                labelsx = labelsx)
  
  #### Tables ####
  
  generateTableRes(data_stat,
                   nameVD = list(VD),
                   listVD = list(data_stat[VD]),
                   listVI = list(data_stat[[VI]]),
                   filename = paste(title, '.csv'),
                   path = './tables/',
                   roundValue = 2,
                   title = TRUE)
  
  by(data_stat[[VD]], data_stat[[VI]], stat.desc)
  
  #### Assumptions ####
  
  print(by(data_stat[[VD]], data_stat[[VI]], stat.desc, basic = FALSE, norm = TRUE))
  #qplot(sample = data_stat[[VD]], stat = 'qq')
  # Homogeneity
  print(leveneTest(data_stat[[VD]], data_stat[[VI]], center = "median"))
  
  # Residuals
  #modellm <- lm(VD ~ VI, data = data_stat)
  #data_resid <- resid_creation(data_stat, modellm)
  #data_resid <- resid_analysis(data_resid, n_predictors = 1, n_subjects = number_participants, model = modellm)
  #plot(modellm)
  #hist(data_resid$standardized.residuals)
}



# Linear model
#http://r-statistics.co/Linear-Regression.html
linearModel <- function(dataSet, VD, VI) {
  linearMod <- lm(VD ~ VI, data=dataSet)
  print(linearMod)
  modelSummary <- summary(linearMod)  # capture model summary as an object
  modelCoeffs <- modelSummary$coefficients  # model coefficients
  beta.estimate <- modelCoeffs[VI, "Estimate"]  # get beta estimate for speed
  std.error <- modelCoeffs[VI, "Std. Error"]  # get std.error for speed
  t_value <- beta.estimate/std.error  # calc t statistic
  print(paste("t value = ", t_value))
  p_value <- 2*pt(-abs(t_value), df=nrow(dataSet)-ncol(dataSet))  # calc p Value
  print(paste("p value = ", p_value))
  f_statistic <- linearMod$fstatistic[1]  # fstatistic
  print(paste("f value = ", f_statistic))
  f <- summary(linearMod)$fstatistic  # parameters for model p-value calc
  model_p <- pf(f[1], f[2], f[3], lower=FALSE)
  plot(linearMod)
  resid(linearmod)
}

# For Boostrap
bootReg <- function(formula, data, i) {
  d <- data[i,]
  fit <- lm(formula, data = d)
  return(coef(fit))
}

# Size effects
rcontrast <- function(t,df)
{
  r <- sqrt(t^2/(t^2+df))
  print(paste('r = ', r))
}

# To calculate partial eta squared, we divide SSn by (SSn + SSd).
# Omega squared may be best (not biased) but complicated. See sjstats (aov format required).
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

# AOV is not appropriate when groups are not balanced (results for main effects of within variables won't be correct). AOV is Type I while ezANOVA is type III.
aovModel <- function(within1, within2, between, dataframe, DV, participant)
  {
  aov(DV~(between*within1*within2)+Error(participant/(within1*within2))+between, data = dataframe)
}

# lme model
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

writeVD <- function(roundValue, output, nline, ncolumn, meanValue, sdValue)
{
  if (roundValue == 0) {
    output[nline,ncolumn] <- sprintf("%.0f $\\pm$ %.0f", meanValue, sdValue)
  } else {
    output[nline,ncolumn] <- sprintf("%.2f $\\pm$ %.2f", meanValue, sdValue)
  }
  return(output)
}

#We determine the dimensions of the output table.
dimensionsTable <- function(listVI, listVD)
{
  lenTable <- 0
  if (length(listVI) == 1)
  {
    lenTable <- lenTable + length(listVD)
  }
  else {
    for (i in 1:length(listVI))
    {
      if (i < length(listVI))
      {
        lenTable <- lenTable + nlevels(listVI[[i]])
      }
    }
  }
  return(lenTable)
}

addTitle <- function(output, VI)
{
  for (i in 1:nlevels(VI))
  {
    output[1,i+1] <- levels(VI)[[i]]
  }
  return(output)
}

getlineCol <- function(lengthVIs, chosenVI, roundValue, numValue, statDesc, output) {
  if (length(lengthVIs) == 1) {
    nline = 1
    ncolumn = chosenVI[1] + 1
  } else if (length(lengthVIs) == 2) {
    nline = chosenVI[1]
    ncolumn = chosenVI[2] + 1
  } else if (length(lengthVIs) == 3) {
    nline = (lengthVIs[2] + 2) * (chosenVI[1] - 1) + chosenVI[2] +1
    ncolumn = chosenVI[3] + 1
  }
  print(chosenVI)
  print(nline)
  print(ncolumn)
  meanValue <- statDesc[[numValue]][[9]]
  sdValue <- statDesc[[numValue]][[13]]
  output <- writeVD(roundValue, output, nline, ncolumn, meanValue, sdValue)
  return(output)
}

recursive.Cell <- function(statDesc, lengthVIs, coeff, chosenVI, output, roundValue) {
  nVI = length(lengthVIs)
  nchosenVI = length(chosenVI)
  if (nchosenVI == nVI) {
    ind = 1
    for (i in nVI:1) {
      ind = ind + coeff * (chosenVI[i] - 1)
      if (i >= 2) {
        coeff = coeff / lengthVIs[i-1]
      }
    }
    numValue = ind
    output <- getlineCol(lengthVIs, chosenVI, roundValue, numValue, statDesc, output)
    return(output)
  } else {
    for (i in 1:lengthVIs[nchosenVI+1]) {
      z <- chosenVI
      z[nchosenVI + 1] = i
      output <- recursive.Cell(statDesc, lengthVIs, coeff, z, output, roundValue)
    }
    return(output)
  }
}

writeColTable <- function(listVI, output) {
  nVI = length(listVI)
  if (nVI == 2) {
    for (i in 1:nlevels(listVI[[1]])) {
      output[i,1] <- levels(listVI[[1]])[[i]]
    }
  } else if (nVI == 3) {
    n_line = 1
    for (i in 1:nlevels(listVI[[1]])) {
      output[n_line,1] <- levels(listVI[[1]])[[i]]
      n_line = n_line + 1
      for (j in 1:nlevels(listVI[[2]])) {
        output[n_line,1] <- levels(listVI[[2]])[[j]]
        n_line = n_line + 1
      }
      n_line = n_line + 1 # skip white line
    }
  }
  return(output)
}

generateTableResRec <- function(data, listVD, listVI, filename, roundValue = 2, title = FALSE) {
  lenTable <- dimensionsTable(listVI, listVD)
  output <- data.frame(matrix(NA, nrow = lenTable, ncol = nlevels(listVI[[length(listVI)]]) + 1))
  if (title == TRUE) {
    output <- addTitle(output, listVI[[length(listVI)]])
    nline <- nline + 1
  }
  output <- writeColTable(listVI, output)
  nVI = length(listVI)
  x = c()
  for(i in 1:nVI) {
    x[i] = nlevels(listVI[[i]])
  }
  coeff = 1
  for (i in 1:(nVI - 1)) {
    coeff = coeff * x[i]
  }
  statDesc <- by(listVD[[1]], listVI, stat.desc)
  print(statDesc)
  output <- recursive.Cell(statDesc, x, coeff, c(), output, roundValue)
  write.table(output, filename, na = "", row.names = FALSE, col.names = FALSE, sep = ";", quote = FALSE)
}

# Generates a table in csv file from dataframe
# Variables need to be factors. Currently does not work with more than 3 variables (bad disposition). Currently does not work with several VIs AND VDs.
generateTableRes <- function(data, nameVD, listVD, listVI, filename, path, roundValue = 2, title = FALSE)
  {
  lenTable <- dimensionsTable(listVI, listVD)
  output <- data.frame(matrix(NA, nrow = lenTable, ncol = nlevels(listVI[[length(listVI)]]) + 1))
  #We create the descriptive table.
  nline <- 1
  if (title == TRUE) {
    output <- addTitle(output, listVI[[length(listVI)]])
    nline <- nline + 1
  }
  for (i in 1:length(listVD))
  {
    statDesc <- by(listVD[[i]], listVI, stat.desc)
    print(statDesc)
    if (length(listVI) == 1)
    {
      output[nline,1] <- nameVD[[i]]
      for (j in 1:nlevels(listVI[[1]]))
      {
        numValue <- j
        meanValue <- statDesc[[numValue]][[9]]
        sdValue <- statDesc[[numValue]][[13]]
        output <- writeVD(roundValue, output, nline, ncolumn = j+1, meanValue, sdValue)
      }
      nline <- nline + 1
    }
    else
    {
      print("Stop torturing yourself. Try generateTableResRec instead ! Dig yourself out of the shit !")
  }
    nline <- nline + 1
    write.table(output, paste(path, filename, sep = ""), na = "", row.names = FALSE, col.names = FALSE, sep = ";", quote = FALSE)
  }
}

# Outputs ANOVA table in csv file
printANOVA <- function(filename, path, output)
{
  #lenLines <- 1
  for (s in names(output)) {
    if (is.data.frame(output[[s]]))
    {
      print(output[[s]])
      write.table(output[[s]], paste(path, filename, sep = ""), append = TRUE, na = "", row.names = FALSE, sep = ";", quote = FALSE)
      #writetoCsv(output[[s]], paste(path, filename, sep = ""), sep = ";", append = TRUE, na = "", row.names = FALSE, quote = FALSE)
      #tabout(paste(path, filename, sep = ""), output[[s]], lenLines)
      #print(length(output[[s]][,1]))
      #lenLines <- lenLines + length(output[[s]][,1])+2
    }
  }
  #xlsToCsv(filename, path)
}

#### Utils ####

# Converts xls file to csv file
xlsToCsv <- function(filename, pathFile) {
  xls <- dir(path = pathFile, pattern = filename)
  pathXls <- paste(pathFile, xls, sep = "")
  created <- mapply(convert, pathXls, gsub("xls", "csv", pathXls))
  unlink(pathXls) # delete xlsx files
}

# Outputs dataframe to xls file
tabout <- function(filename, output, rowNumber, headerValue=TRUE)
{
  writeWorksheetToFile(filename, output, startRow = rowNumber, header = headerValue, sheet="FirstSheet")
}
