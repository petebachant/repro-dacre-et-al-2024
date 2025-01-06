library(ggplot2)
library(ggstatsplot)
library(ggpmisc)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
library(readxl)
library(lme4)
library(Matrix)
library(lmerTest)
library(car)
library(reshape2) 
library(viridis)
library(scales)
library(MASS)
library(emmeans)
library(multcompView)
library(dplyr)


##################################################################
####            Response to temperature analysis               ### 
##################################################################
{
  
  # Load the data from the Excel file with correct column names
  data <- read_excel("RMR_Data_DDacre.xlsx", sheet = "RMR(Fertile)")
  
  #### Graph A - Metabolic_testing(Fertile_Diet)
  {
  
  
  # Calculate the Q10 effect (average gradient between 15°C and 20°C)
  temp_15 <- 15
  temp_20 <- 20
  
  # Filter data for the two temperature intervals
  data_sub <- data[data$Temperature %in% c(temp_15, temp_20), ]
  
  # Calculate the Q10 effect using the "FINAL" column
  Q10 <- (mean((data_sub$FINAL[data_sub$Temperature == temp_20])) /
            mean((data_sub$FINAL[data_sub$Temperature == temp_15]))) ^ (10 / (temp_20 - temp_15))
  
  # Fit linear regression for the trendline
  trendline_model <- lm(log(FINAL) ~ Temperature, data = data_sub)
  
  # Fit linear regression for the regression line using all data points
  regression_model <- lm(log(FINAL) ~ Temperature, data = data)
  
  # Get the coefficients of the regression equations
  trendline_eqn <- as.character(round(coef(trendline_model)[1], 2)) %>%
    paste(" + ", as.character(round(coef(trendline_model)[2], 2)), " * (Temperature)", sep = "")
  regression_eqn <- as.character(round(coef(regression_model)[1], 2)) %>%
    paste(" + ", as.character(round(coef(regression_model)[2], 2)), " * (Temperature)", sep = "")
  
  
  # Create a scatter plot with log-transformed "FINAL" data, trendline, regression line, equations, and standard error bands
  library(ggplot2)
  library(dplyr)
  
  ggplot(data, aes(x = Temperature, y = log(FINAL))) +  # Log-transform the "FINAL" variable
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ (mean(log(data_sub$FINAL[data_sub$Temperature == temp_20])) /
                                                mean(log(data_sub$FINAL[data_sub$Temperature == temp_15]))) ^ (10 / (temp_20 - temp_15)), 
                se = FALSE, color = "blue", linetype = "dashed") +  # Trendline without standard error bands
    geom_smooth(method = "lm", se = TRUE, aes(group = 1), color = "blue") +  # Regression line with standard error bands
    geom_text(aes(label = paste("A")), x = 15.5, y = 7.5, size = 8) +  # Align Q10 data to top-left
    geom_text(aes(label = paste("Q10 =", round(Q10, 2))), x = 25.5, y = 5.5, size = 4) +  # Align Q10 data to top-left
    geom_text(aes(label = paste("Q10 Trendline: ", trendline_eqn)), x = 27.6, y = 5.4, size = 4) +  # Align trendline data below Q10 data
    geom_text(aes(label = paste("Regression Line: ", regression_eqn)), x = 27.75, y = 5.3, size = 4) +  # Align regression line data below trendline data
    labs(x = "Temperature (°C)",  
         y = expression("Resting Metabolic Rate - Log( μlCO "[2]*"." ~ "g"^-1~".h"^-1~")"), 
         title = NULL) +  # Remove graph title
    theme_minimal() +
    theme(panel.grid = element_blank(),  # Remove the background grid
          panel.border = element_blank(),  # Remove the border around the whole graph
          axis.line = element_line(color = "black"),  # Customize axis lines
          axis.title = element_text(size = 14))  # Increase axis title text size
}

  #### Graph B - Metabolic_testing(Fertile_Diet)
  {
  
  # Calculate the Q10 effect (average gradient between 15°C and 20°C)
  temp_15 <- 15
  temp_20 <- 20
  
  # Filter data for the two temperature intervals
  data_sub <- data[data$Temperature %in% c(temp_15, temp_20), ]
  
  # Calculate the Q10 effect using the "FINAL" column
  Q10 <- (mean((data_sub$FINAL[data_sub$Temperature == temp_20])) /
            mean((data_sub$FINAL[data_sub$Temperature == temp_15]))) ^ (10 / (temp_20 - temp_15))
  
  # Fit linear regression for the trendline
  trendline_model <- lm(log(FINAL) ~ Temperature, data = data_sub)
  
  # Fit linear regression for the regression line using all data points
  regression_model <- lm(log(FINAL) ~ Temperature, data = data)
  
  # Get the coefficients of the regression equations
  trendline_eqn <- as.character(round(coef(trendline_model)[1], 2)) %>%
    paste(" + ", as.character(round(coef(trendline_model)[2], 2)), " * (Temperature)", sep = "")
  regression_eqn <- as.character(round(coef(regression_model)[1], 2)) %>%
    paste(" + ", as.character(round(coef(regression_model)[2], 2)), " * (Temperature)", sep = "")
  
  # Create a scatter plot with log-transformed "FINAL" data, trendline, regression line, equations, and standard error bands
  library(ggplot2)
  library(dplyr)
  
  ggplot(data, aes(x = Temperature, y = log(FINAL))) +  # Log-transform the "FINAL" variable
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ (mean(log(data_sub$FINAL[data_sub$Temperature == temp_20])) /
                                                mean(log(data_sub$FINAL[data_sub$Temperature == temp_15]))) ^ (10 / (temp_20 - temp_15)), 
                se = FALSE, color = "blue", linetype = "dashed") +  # Trendline without standard error bands
    geom_smooth(method = "lm", se = TRUE, aes(group = 1), color = "blue") +  # Regression line with standard error bands
    geom_text(aes(label = paste("B")), x = 15.5, y = 7.5, size = 8) +  # Align Q10 data to top-left
    geom_text(aes(label = paste("Q10 =", round(Q10, 2))), x = 25.1, y = 5.5, size = 4) +  # Align Q10 data to top-left
    geom_text(aes(label = paste("Q10 Trendline: ", trendline_eqn)), x = 27.25, y = 5.4, size = 4) +  # Align trendline data below Q10 data
    geom_text(aes(label = paste("Regression Line: ", regression_eqn)), x = 27.5, y = 5.3, size = 4) +  # Align regression line data below trendline data
    labs(x = "Temperature (°C)",  
         y = expression("Resting Metabolic Rate - Log( μlCO "[2]*"." ~ "g"^-1~".h"^-1~")"), 
         title = NULL) +  # Remove graph title
    theme_minimal() +
    theme(panel.grid = element_blank(),  # Remove the background grid
          panel.border = element_blank(),  # Remove the border around the whole graph
          axis.line = element_line(color = "black"),  # Customize axis lines
          axis.title = element_text(size = 14))  # Increase axis title text size
}

}

##################################################################
####   Comparison between sterile and fertile RMR analysis     ### 
##################################################################

{
  Sterile <- read_excel("RMR_Data_DDacre.xlsx", sheet = "Fertile_Sterile")
  View(Sterile)
  summary(Sterile)
  
  Sterile$Fertility<-as.factor(Sterile$Fertility)
  Sterile$Temperature<-as.factor(Sterile$Temperature)
  Sterile$Date<- as.Date(Sterile$Date)
  
  ##################################################################
  ####               Normal Distribution (Not normal)            ### 
  ##################################################################
  
  {
    library(ggpubr)
    {
      ggdensity(Sterile$FINAL, 
                main = "Density plot of Respiration",
                xlab = "FINAL")
      
      ###    If a density curve is left skewed, then the mean is less than the median.
      ###    If a density curve is right skewed, then the mean is greater than the median.
      ###    If a density curve has no skew, then the mean is equal to the median.
    }
    
    
    ggqqplot(Sterile$FINAL)
    { 
      ### If the data is normally distributed, the points will fall on the 
      ### 45-degree reference line. If the data is not normally distributed, 
      ### the points will deviate from the reference line. 
      
    }
    
    shapiro.test(Sterile$FINAL)
    {
      ### the p-value > 0.05 implying that the distribution of the data are not 
      ### significantly different from normal distribution. In other words, we can 
      ### assume the normality.
      
      ###W = 0.93219, p-value = 0.002462
      
    }
    library(moments)
    skewness(Exposure$FINAL, na.rm = TRUE)
  }
  
  ##################################################################
  ####                         GLM                               ### 
  ##################################################################
  {
    
    model_Sterile <- glm(log(FINAL) ~ Fertility+Temperature+Mass+Temperature*Fertility+Temperature*Mass,data = Sterile) 
    
    #(log transformation glm with Fertility+Temperature+Mass+Temperature*Fertility+Temperature*Mass)
    #Residual deviance:  1.0134  on 51  degrees of freedom
    #AIC: -54.587
    
    plot(model_Sterile)
    Anova(model_Sterile, type = "III") 
    summary(model_Sterile)
    anov_table <- Anova(model_Sterile, type = "III")
    print(anov_table)
    
    TukeyHSD(aov(model_Sterile))
    
  }
  
  ##################################################################
  ####                     Bar-Chart                             ### 
  ##################################################################
  
  {
    
    y_label <- expression(atop("Resting Metabolic Rate", "" ~ (µl ~ CO[2] ~ g^-1 ~ h^-1)))
    
    # Calculate mean and standard deviation by Diet and Semiochemical
    grouped_data <- Sterile %>%
      group_by(Fertility, Temperature) %>%
      summarize(Mean_FINAL = mean(FINAL),
                SD_FINAL = sd(FINAL))
    
    # Plot bar chart with fitted standard deviation
    Exposure_Bar <- ggplot(grouped_data, aes(x = Temperature, y = Mean_FINAL, fill = Fertility)) +
      geom_bar(stat = "identity", position = "dodge") +
      annotate(geom = "text", x = 0.75, y = 1600, label = "a") +
      
      annotate(geom = "text", x = 1.2, y = 1600, label = "b") +
      
      annotate(geom = "text", x = 1.75, y = 1600, label = "c") +
      
      annotate(geom = "text", x = 2.2, y = 1600, label = "c") +
      
      annotate(geom = "text", x = 2.75, y = 1600, label = "d") +
      
      annotate(geom = "text", x = 3.2, y = 1600, label = "d") +
      geom_errorbar(aes(ymin = Mean_FINAL - SD_FINAL, ymax = Mean_FINAL + SD_FINAL),
                    width = 0.2, position = position_dodge(0.9)) +
      labs(x = "Temperature °C", y = y_label, fill = "Fertility") +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black", size = 1, linetype = "solid"),
        
      )
    
    Exposure_Bar <- Exposure_Bar +
      theme(axis.text = element_text(size = 15),
            axis.title = element_text(size = 20),
            plot.title = element_text(size = 20),
            axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))
    
    Exposure_Bar+scale_fill_brewer(palette = "Dark2")
    
  }
  
}

##################################################################
####      Exposure duration to Semiochemcial analysis          ### 
##################################################################

{
  Exposure <- read_excel("RMR_Data_DDacre.xlsx", sheet = "EXP")
  View(Exposure)
  summary(Exposure)
  
  Exposure$EXP<-as.factor(Exposure$EXP)
  Exposure$Date<- as.Date(Exposure$Date)
  Exposure$Temperature<-as.factor(Exposure$Temperature)
  
  ##################################################################
  ####               Normal Distribution (Normal)                ### 
  ##################################################################
  
  {
    {
      ggdensity(Exposure$FINAL, 
                main = "Density plot of Respiration",
                xlab = "FINAL")
      
      ###    If a density curve is left skewed, then the mean is less than the median.
      ###    If a density curve is right skewed, then the mean is greater than the median.
      ###    If a density curve has no skew, then the mean is equal to the median.
    }
    
    
    ggqqplot(Exposure$FINAL)
    { 
      ### If the data is normally distributed, the points will fall on the 
      ### 45-degree reference line. If the data is not normally distributed, 
      ### the points will deviate from the reference line. 
      
    }
    
    shapiro.test(Exposure$FINAL)
    {
      ### the p-value > 0.05 implying that the distribution of the data are not 
      ### significantly different from normal distribution. In other words, we can 
      ### assume the normality.
      
      ###W = 0.9811, p-value = 0.09002
      
    }
    library(moments)
    skewness(Exposure$FINAL, na.rm = TRUE)
    #0.4180728 positive - right skew
  }
  
  ##################################################################
  ####                         GLM                               ### 
  ##################################################################
  
  {
    
    model_EXP <- glm(log(FINAL) ~ EXP+Semiochemical+Mass+Semiochemical*EXP,data = Exposure,
                     family = Gamma(link = "log")) 
    
    #(log transformation with gamma distribution, EXP+Semiochemical+Mass+Semiochemical*EXP)
    #Residual deviance: 0.074051  on 113  degrees of freedom
    #AIC: -77.897
    
    
    plot(model_EXP)
    result <- Exposure$FINAL
    qqplot(result)
    Anova(model_EXP, type = "III") 
    summary(model_EXP)
    anov_table <- Anova(model_EXP, type = "III")
    print(anov_table)
    
    TukeyHSD(aov(model_EXP))
    
  }
  
  ##################################################################
  ####                     Bar-Chart                             ### 
  ##################################################################
  
  {
    
    #######################################################
    ###               Finding the letters               ###
    #######################################################
    {
    model_EXP <- glm((FINAL) ~ Semiochemical*EXP, 
                     data = Exposure) 
    
    anova_EXP <- aov(model_EXP, type = "III") 
    tukey_EXP <- TukeyHSD(anova_EXP)
    
    
    library(multcompView)
    cld <- multcompLetters4(anova_EXP,tukey_EXP)
    print (cld)
    }
    
    y_label <- expression(atop("Resting Metabolic Rate", "" ~ (µl ~ CO[2] ~ g^-1 ~ h^-1)))
    
    
    # Calculate mean and standard deviation by Diet and Semiochemical
    grouped_data <- Exposure %>%
      group_by(Semiochemical, EXP) %>%
      summarize(Mean_FINAL = mean(FINAL),
                SD_FINAL = sd(FINAL))
    
    # Plot bar chart with fitted standard deviation
    Exposure_Bar <- ggplot(grouped_data, aes(x = EXP, y = Mean_FINAL, fill = Semiochemical)) +
      geom_bar(stat = "identity", position = "dodge") +
      annotate(geom = "text", x = 0.75, y = 1100, label = "bc") +
      
      annotate(geom = "text", x = 1.2, y = 1100, label = "ab") +
      
      annotate(geom = "text", x = 1.8, y = 1100, label = "c") +
      
      annotate(geom = "text", x = 2.25, y = 1100, label = "b") +
      
      annotate(geom = "text", x = 2.8, y = 1100, label = "a") +
      
      annotate(geom = "text", x = 3.2, y = 1100, label = "a") +
      geom_errorbar(aes(ymin = Mean_FINAL - SD_FINAL, ymax = Mean_FINAL + SD_FINAL),
                    width = 0.2, position = position_dodge(0.9)) +
      labs(x = "Days Exposure", y = y_label, fill = "Semiochemical") +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black", size = 1, linetype = "solid"),
        
      )
    
    
    
    Exposure_Bar <- Exposure_Bar +
      theme(axis.text = element_text(size = 15),
            axis.title = element_text(size = 20),
            plot.title = element_text(size = 20),
            axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))
    
    Exposure_Bar+scale_fill_brewer(palette = "Dark2")
    
  }
  
  
}

##################################################################
####             Diet and Semiochemcial analysis               ### 
##################################################################

{
  Metabolic_testingF <- read_excel("RMR_Data_DDacre.xlsx", sheet = "RMR(Fertile)")
  View(Metabolic_testingF)
  summary(Metabolic_testingF)
  
  Metabolic_testingF$Date<- as.Date(Metabolic_testingF$Date)
  Metabolic_testingF$Temperature<-as.factor(Metabolic_testingF$Temperature)
  Metabolic_testingF$Diet<-as.factor(Metabolic_testingF$Diet)
  Metabolic_testingF$Semiochemical<-as.factor(Metabolic_testingF$Semiochemical)
  
  ##################################################################
  ####               Normal Distribution (Not Normal)            ### 
  ##################################################################
  
  {
    library(ggpubr)
    {
      ggdensity(Metabolic_testingF$FINAL, 
                main = "Density plot of Respiration",
                xlab = "FINAL")
      
      ###    If a density curve is left skewed, then the mean is less than the median.
      ###    If a density curve is right skewed, then the mean is greater than the median.
      ###    If a density curve has no skew, then the mean is equal to the median.
    }
    
    
    ggqqplot(Metabolic_testingF$FINAL)
    { 
      ### If the data is normally distributed, the points will fall on the 
      ### 45-degree reference line. If the data is not normally distributed, 
      ### the points will deviate from the reference line. 
      
    }
    
    
    shapiro.test(Metabolic_testingF$FINAL)
    {
      ### the p-value > 0.05 implying that the distribution of the data are not 
      ### significantly different from normal distribution. In other words, we can 
      ### assume the normality.
      
      ###W = 0.92887, p-value = 2.535e-14
      
    }
    library(moments)
    skewness(Metabolic_testingF$FINAL, na.rm = TRUE)
    #0.9508519 right-skew
  }
  
  ##################################################################
  ####                         GLM                               ### 
  ##################################################################
  {
    
    model_Fertile <- glm(log(FINAL) ~ Semiochemical+Diet+ Temperature +Date+Temperature*Diet+ Temperature*Semiochemical, 
                         data = Metabolic_testingF,
                         family =inverse.gaussian)
    
    ### inverse.gaussian (Semiochemical+Diet+ Temperature +Date+Temperature*Diet+ Temperature*Semiochemical)
    #Residual deviance: 0.07180  on 463  degrees of freedom
    #AIC: -161.88
    
    plot(model_Fertile)
    Anova(model_Fertile, type = "III") 
    summary(model_Fertile) 
    anov_table_Semio <- Anova(model_Fertile, type = "III")
    print(anov_table_Semio)
    
    TukeyHSD(aov(model_Fertile))
    
    
    
  }
  
  ##################################################################
  ####                     Bar-Chart                             ### 
  ##################################################################
  {
  
  ### Fertile Temperature and semiochemical
  {
    
    #######################################################
    ###               Finding the letters               ###
    #######################################################
    {
      model_Fertile <- glm((FINAL) ~ Temperature*Semiochemical, 
                           data = Metabolic_testingF) 
      
      anova_Fertile <- aov(model_Fertile, type = "III") 
      tukey_Fertile <- TukeyHSD(anova_Fertile)
      
      
      library(multcompView)
      cld <- multcompLetters4(anova_Fertile,tukey_Fertile)
      print (cld)
    }
    
    y_label <- expression(atop("Resting Metabolic Rate", "" ~ (µl ~ CO[2] ~ g^-1 ~ h^-1)))
    
    
    # Calculate mean and standard deviation by Diet and Semiochemical
    grouped_data <- Metabolic_testingF %>%
      group_by(Semiochemical, Temperature) %>%
      summarize(Mean_FINAL = mean(FINAL),
                SD_FINAL = sd(FINAL))
    
    # Plot bar chart with fitted standard deviation
    Exposure_Bar <- ggplot(grouped_data, aes(x = Temperature, y = Mean_FINAL, fill = Semiochemical)) +
      
      geom_bar(stat = "identity", position = "dodge") +
      annotate(geom = "text", x = 0.7, y = 1600, label = "e") +
      
      annotate(geom = "text", x = 1, y = 1600, label = "e") +
      
      annotate(geom = "text", x = 1.3, y = 1600, label = "e") +
      
      annotate(geom = "text", x = 1.7, y = 1600, label = "b") +
      
      annotate(geom = "text", x = 2, y = 1600, label = "de") +
      
      annotate(geom = "text", x = 2.3, y = 1600, label = "d") +
      
      annotate(geom = "text", x = 2.7, y = 1600, label = "c") +
      
      annotate(geom = "text", x = 3, y = 1600, label = "c") +
      
      annotate(geom = "text", x = 3.3, y = 1600, label = "c") +
      
      annotate(geom = "text", x = 3.7, y = 1600, label = "a") +
      
      annotate(geom = "text", x = 4, y = 1600, label = "b") +
      
      annotate(geom = "text", x = 4.3, y = 1600, label = "b") +
      geom_errorbar(aes(ymin = Mean_FINAL - SD_FINAL, ymax = Mean_FINAL + SD_FINAL),
                    width = 0.2, position = position_dodge(0.9)) +
      labs(x = "Temperature", y = y_label, fill = "Semiochemcial") +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black", size = 1, linetype = "solid"),)
    
    
    
    Exposure_Bar <- Exposure_Bar +
      theme(axis.text = element_text(size = 15),
            axis.title = element_text(size = 20),
            plot.title = element_text(size = 20),
            axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))
    
    Exposure_Bar+scale_fill_brewer(palette = "Dark2")
    
    
  }
  
  
  ### Fertile Temperature and Diet
  {
    
    #######################################################
    ###               Finding the letters               ###
    #######################################################
    {
    model_Fertile <- glm((FINAL) ~ Temperature*Diet, 
                         data = Metabolic_testingF) 
    
    anova_Fertile <- aov(model_Fertile, type = "III") 
    tukey_Fertile <- TukeyHSD(anova_Fertile)
    
    
    library(multcompView)
    cld <- multcompLetters4(anova_Fertile,tukey_Fertile)
    print (cld)
    }
    
    y_label <- expression(atop("Resting Metabolic Rate", "" ~ (µl ~ CO[2] ~ g^-1 ~ h^-1)))
    
    
    # Calculate mean and standard deviation by Diet and Semiochemical
    grouped_data <- Metabolic_testingF %>%
      group_by(Diet, Temperature) %>%
      summarize(Mean_FINAL = mean(FINAL),
                SD_FINAL = sd(FINAL))
    
    # Plot bar chart with fitted standard deviation
    Exposure_Bar <- ggplot(grouped_data, aes(x = Temperature, y = Mean_FINAL, fill = Diet)) +
      
      geom_bar(stat = "identity", position = "dodge") +
      annotate(geom = "text", x = 0.77, y = 1600, label = "f") +
      
      annotate(geom = "text", x = 1.225, y = 1600, label = "ef") +
      
      annotate(geom = "text", x = 1.77, y = 1600, label = "de") +
      
      annotate(geom = "text", x = 2.225, y = 1600, label = "d") +
      
      annotate(geom = "text", x = 2.77, y = 1600, label = "c") +
      
      annotate(geom = "text", x = 3.225, y = 1600, label = "b") +
      
      annotate(geom = "text", x = 3.77, y = 1600, label = "a") +
      
      annotate(geom = "text", x = 4.225, y = 1600, label = "a") +
      
      geom_errorbar(aes(ymin = Mean_FINAL - SD_FINAL, ymax = Mean_FINAL + SD_FINAL),
                    width = 0.2, position = position_dodge(0.9)) +
      labs(x = "Temperature", y = y_label, fill = "Diet") +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black", size = 1, linetype = "solid"),
        
      )
    
    
    
    Exposure_Bar <- Exposure_Bar +
      theme(axis.text = element_text(size = 15),
            axis.title = element_text(size = 20),
            plot.title = element_text(size = 20),
            axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))
    
    Exposure_Bar+scale_fill_brewer(palette = "Dark2")
    
  }
  
  }
  
  ##################################################################
  ####                   Date-of-Testing                        ### 
  ##################################################################
  {
    library(dplyr)
    library(lubridate)
    
    Metabolic_testingF$Month <- month(Metabolic_testingF$Date, label = TRUE)
    
    monthly_data <- Metabolic_testingF %>%
      group_by(Month) %>%
      summarize(Avg_Mass = mean(Mass, na.rm = TRUE))
    
    model_Monthly <- glm(Mass ~ Month, data = Metabolic_testingF)
    
    anova_Monthly <- aov(model_Monthly, type = "III") 
    tukey_Monthly <- TukeyHSD(anova_Monthly)
    
    cld <- multcompLetters4(anova_Monthly,tukey_Monthly)
    print (cld)
    
    #############################
    
    DateOfTesting <- ggplot(Metabolic_testingF, aes(x = Month, y = Mass)) +
      geom_boxplot() +
      geom_text(aes(label = paste("A")), x = 1, y = 0.027, size = 8) +
      
      annotate(geom = "text", x = 1, y = 0.03, label = "ab") +
      
      annotate(geom = "text", x = 2, y = 0.03, label = "cd") +
      
      annotate(geom = "text", x = 3, y = 0.03, label = "cd") +
      
      annotate(geom = "text", x = 4, y = 0.03, label = "d") +
      
      annotate(geom = "text", x = 5, y = 0.03, label = "abcd") +
      
      annotate(geom = "text", x = 6, y = 0.03, label = "cd") +
      
      annotate(geom = "text", x = 7, y = 0.03, label = "a") +
      
      annotate(geom = "text", x = 8, y = 0.03, label = "bc") +
      
      annotate(geom = "text", x = 9, y = 0.03, label = "abcd") +
      
      
      xlab("Month of Testing") + ylab("Mass (g)") +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 15),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))
    
    
    DateOfTesting + scale_fill_brewer(palette = "Dark2")
    
  }
  
  ##################################################################
  ####                   Scatter plot                            ### 
  ##################################################################
  {
    ### Simple setup (for mass)
    {
      #Read in the data (again for ease)
      Metabolic_testingF <- read_excel("RMR_Data_DDacre.xlsx", sheet = "RMR(Fertile)")
      
      Tempplot <- ggplot(Metabolic_testingF, aes(Date,(FINAL),colour = Diet)) +
        geom_point() +
        ggtitle("")+
        xlab("Date") + ylab("FINAL") +
        scale_fill_grey() +
        theme_bw() +
        geom_smooth(se = TRUE) +  # Use linear regression
        xlab("Date") + ylab(expression("Resting Metabolic Rate" ~ (µl ~ CO[2] ~ g^-1 ~ h^-1)))+
        scale_y_continuous(labels = scales::comma)  # Apply a scaling transformation
      
      # Adjust text and title sizes
      Tempplot + theme(
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        plot.title = element_text(size = 20),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line())
      
    }
  
  
}
