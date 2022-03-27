#Pantherophis ratsnake color - Hantak et al. 

library(dplyr)
library(tidyr)
library(ggplot2)
library(PerformanceAnalytics)
library(brms)
library(ggeffects)
library(jtools)

#Cleaned (filtered) data
snake_data <- read.csv("snake_data_cleaned_clim.csv", header = TRUE, stringsAsFactors = FALSE)
str(snake_data)

snake_data$elevation <- as.numeric(snake_data$elevation)
snake_data$precip <- as.numeric(snake_data$precip)
snake_data$s.color <- as.factor(snake_data$s.color)
snake_data$scientific_name <- as.factor(snake_data$scientific_name)

#Remove records that are missing data
snake_data <- snake_data %>% drop_na(latitude) 
snake_data <- snake_data %>% drop_na(elevation) 
snake_data <- snake_data %>% drop_na(temp) 
snake_data <- snake_data %>% drop_na(precip) 

#check normality of data 
hist(snake_data$latitude) #looks normal
hist(snake_data$elevation) 
hist(snake_data$temp) #looks normal
hist(snake_data$precip) #looks normal

elev.log <- log(snake_data$elevation) 
snake_data <- cbind(snake_data, elev.log)
str(snake_data)

# -Inf values are messing things up when I scale and center the variables 
snake_data <- snake_data %>% drop_na(elev.log) 
snake_data <- snake_data[!is.infinite(snake_data$elev.log),]

hist(snake_data$elev.log) 

test_vars2 <- c("elevation", "temp", "precip", "latitude")
snake_data_test2 <- snake_data[test_vars2]
chart.Correlation(snake_data_test2, histogram=TRUE, pch=19) 

test_vars3 <- c("elev.log", "temp", "precip", "latitude")
snake_data_test3 <- snake_data[test_vars3]
chart.Correlation(snake_data_test3, histogram=TRUE, pch=19) 

##Scale and center continuous predictors 
snake_data2 <- transform(snake_data, latitude=scale(latitude), elevation=scale(elevation), 
                         elev.log=scale(elev.log), temp=scale(temp), precip=scale(precip))
str(snake_data2)

hist(snake_data2$latitude) 
hist(snake_data2$elevation) 
hist(snake_data2$elev.log) 
hist(snake_data2$temp) 
hist(snake_data2$precip) 

#Separate ratsnake species
table(snake_data2$scientific_name)
P.a <- filter(snake_data2, scientific_name == "Pantherophis_alleghaniensis")
P.o <- filter(snake_data2, scientific_name == "Pantherophis_obsoletus")
P.s <- filter(snake_data2, scientific_name == "Pantherophis_spiloides")

#Changing color to numbers 
table(snake_data2$s.color)
snake_data2$color2[snake_data2$s.color == "black"] <- "0"
snake_data2$color2[snake_data2$s.color == "dark_gray"] <- "1"
snake_data2$color2[snake_data2$s.color == "brown"] <- "2"
snake_data2$color2[snake_data2$s.color == "light_gray"] <- "3"
snake_data2$color2[snake_data2$s.color == "orange"] <- "4"
snake_data2$color2[snake_data2$s.color == "yellow"] <- "5"
snake_data2$color2 <- factor(snake_data2$color2) #for logistic models
#snake_data2$color2 <- as.numeric(snake_data2$color2)
str(snake_data2)

table(P.a$s.color)
P.a$color2[P.a$s.color == "black"] <- "0"
P.a$color2[P.a$s.color == "dark_gray"] <- "1"
P.a$color2[P.a$s.color == "brown"] <- "2"
P.a$color2[P.a$s.color == "light_gray"] <- "3"
P.a$color2[P.a$s.color == "orange"] <- "4"
P.a$color2[P.a$s.color == "yellow"] <- "5"
P.a$color2 <- factor(P.a$color2) 

table(P.o$s.color)
P.o$color2[P.o$s.color == "black"] <- "0"
P.o$color2[P.o$s.color == "dark_gray"] <- "1"
P.o$color2[P.o$s.color == "brown"] <- "2"
P.o$color2[P.o$s.color == "light_gray"] <- "3"
P.o$color2[P.o$s.color == "orange"] <- "4"
P.o$color2[P.o$s.color == "yellow"] <- "5"
P.o$color2 <- factor(P.o$color2) 

table(P.s$s.color)
P.s$color2[P.s$s.color == "black"] <- "0"
P.s$color2[P.s$s.color == "dark_gray"] <- "1"
P.s$color2[P.s$s.color == "brown"] <- "2"
P.s$color2[P.s$s.color == "light_gray"] <- "3"
P.s$color2[P.s$s.color == "orange"] <- "4"
P.s$color2[P.s$s.color == "yellow"] <- "5"
P.s$color2 <- factor(P.s$color2) 

###################

###Bayesian generalized non-linear regression models 
#full model
mult.mod4 <- brm(s.color ~ elev.log + temp + precip + (1 | scientific_name), 
                 data = snake_data2, family = "categorical",
                 warmup = 1000, iter = 2000, chains = 4, cores = 2) 

#interaction
mult.mod4.1 <- brm(s.color ~ elev.log + temp * precip + (1 | scientific_name), 
                   data = snake_data2, family = "categorical",
                   warmup = 1000, iter = 2000, chains = 4, cores = 2) 

#for model selection
mult.mod4_1 <- brm(s.color ~ temp + precip + (1 | scientific_name), 
                   data = snake_data2, family = "categorical",
                   warmup = 1000, iter = 2000, chains = 4, cores = 2) 

mult.mod4_2 <- brm(s.color ~ elev.log + temp + (1 | scientific_name), 
                   data = snake_data2, family = "categorical",
                   warmup = 1000, iter = 2000, chains = 4, cores = 2)  

mult.mod4_3 <- brm(s.color ~ elev.log + precip + (1 | scientific_name), 
                   data = snake_data2, family = "categorical",
                   warmup = 1000, iter = 2000, chains = 4, cores = 2)  

mult.mod4_4 <- brm(s.color ~ temp + (1 | scientific_name), 
                   data = snake_data2, family = "categorical",
                   warmup = 1000, iter = 2000, chains = 4, cores = 2)  

mult.mod4_5 <- brm(s.color ~ precip + (1 | scientific_name), 
                   data = snake_data2, family = "categorical",
                   warmup = 1000, iter = 2000, chains = 4, cores = 2)  

mult.mod4_6 <- brm(s.color ~ elev.log + (1 | scientific_name), 
                   data = snake_data2, family = "categorical",
                   warmup = 1000, iter = 2000, chains = 4, cores = 2)  

## model selection
fit1 <- add_criterion(mult.mod4, criterion = c("loo", "waic"))
fit1.i <- add_criterion(mult.mod4.1, criterion = c("loo", "waic"))
fit2 <- add_criterion(mult.mod4_1, criterion = c("loo", "waic"))
fit3 <- add_criterion(mult.mod4_2, criterion = c("loo", "waic"))
fit4 <- add_criterion(mult.mod4_3, criterion = c("loo", "waic"))
fit5 <- add_criterion(mult.mod4_4, criterion = c("loo", "waic"))
fit6 <- add_criterion(mult.mod4_5, criterion = c("loo", "waic"))
fit7 <- add_criterion(mult.mod4_6, criterion = c("loo", "waic"))

loo_compare(fit1, fit1.i, criterion = "loo") 
model_weights(fit1, fit2, fit3, fit4, fit5, fit6, fit7, weights = "loo")
loo_compare(fit1, fit2, fit3, fit4, fit5, fit6, fit7, criterion = "loo") 


############
#P. alleghaniensis
str(P.a)
mult.p.a <- brm(s.color ~ elev.log + temp + precip, 
                data = P.a, family = "categorical",
                warmup = 1000, iter = 2000, chains = 4, cores = 2)  

#interaction
mult.p.a.i <- brm(s.color ~ elev.log + temp * precip, 
                  data = P.a, family = "categorical",
                  warmup = 1000, iter = 2000, chains = 4, cores = 2)  

ggpredict(mult.p.a.i, c("temp", "precip")) %>% plot()

#for model selection
mult.p.a2 <- brm(s.color ~ temp + precip, 
                 data = P.a, family = "categorical",
                 warmup = 1000, iter = 2000, chains = 4, cores = 2) 

mult.p.a3 <- brm(s.color ~ elev.log + temp, 
                 data = P.a, family = "categorical",
                 warmup = 1000, iter = 2000, chains = 4, cores = 2)  

mult.p.a4 <- brm(s.color ~ elev.log + precip, 
                 data = P.a, family = "categorical",
                 warmup = 1000, iter = 2000, chains = 4, cores = 2)  

mult.p.a5 <- brm(s.color ~ temp, 
                 data = P.a, family = "categorical",
                 warmup = 1000, iter = 2000, chains = 4, cores = 2) 

mult.p.a6 <- brm(s.color ~ precip, 
                 data = P.a, family = "categorical",
                 warmup = 1000, iter = 2000, chains = 4, cores = 2)  

mult.p.a7 <- brm(s.color ~ elev.log, 
                 data = P.a, family = "categorical",
                 warmup = 1000, iter = 2000, chains = 4, cores = 2) 

## model selection
fit_a1 <- add_criterion(mult.p.a, criterion = c("loo", "waic"))
fit_a1.i <- add_criterion(mult.p.a.i, criterion = c("loo", "waic"))
fit_a2 <- add_criterion(mult.p.a2, criterion = c("loo", "waic"))
fit_a3 <- add_criterion(mult.p.a3, criterion = c("loo", "waic"))
fit_a4 <- add_criterion(mult.p.a4, criterion = c("loo", "waic"))
fit_a5 <- add_criterion(mult.p.a5, criterion = c("loo", "waic"))
fit_a6 <- add_criterion(mult.p.a6, criterion = c("loo", "waic"))
fit_a7 <- add_criterion(mult.p.a7, criterion = c("loo", "waic"))

loo_compare(fit_a1, fit_a1.i, criterion = "loo")
model_weights(fit_a1, fit_a2, fit_a3, fit_a4, fit_a5, fit_a6, fit_a7, weights = "loo")
loo_compare(fit_a1, fit_a2, fit_a3, fit_a4, fit_a5, fit_a6, fit_a7, criterion = "loo") 

#############
#P. obsoletus
str(P.o)
mult.p.o <- brm(s.color ~ elev.log + temp + precip, 
                data = P.o, family = "categorical",
                warmup = 1000, iter = 2000, chains = 4, cores = 2)  

#interaction
mult.p.o.i <- brm(s.color ~ elev.log + temp * precip, 
                  data = P.o, family = "categorical",
                  warmup = 1000, iter = 2000, chains = 4, cores = 2)  

#for model selection
mult.p.o2 <- brm(s.color ~ temp + precip, 
                 data = P.o, family = "categorical",
                 warmup = 1000, iter = 2000, chains = 4, cores = 2)  

mult.p.o3 <- brm(s.color ~ elev.log + temp, 
                 data = P.o, family = "categorical",
                 warmup = 1000, iter = 2000, chains = 4, cores = 2)  

mult.p.o4 <- brm(s.color ~ elev.log + precip, 
                 data = P.o, family = "categorical",
                 warmup = 1000, iter = 2000, chains = 4, cores = 2) 

mult.p.o5 <- brm(s.color ~ temp, 
                 data = P.o, family = "categorical",
                 warmup = 1000, iter = 2000, chains = 4, cores = 2)  

mult.p.o6 <- brm(s.color ~ precip, 
                 data = P.o, family = "categorical",
                 warmup = 1000, iter = 2000, chains = 4, cores = 2)  

mult.p.o7 <- brm(s.color ~ elev.log, 
                 data = P.o, family = "categorical",
                 warmup = 1000, iter = 2000, chains = 4, cores = 2)  

## model selection
fit_o1 <- add_criterion(mult.p.o, criterion = c("loo", "waic"))
fit_o1.i <- add_criterion(mult.p.o.i, criterion = c("loo", "waic"))
fit_o2 <- add_criterion(mult.p.o2, criterion = c("loo", "waic"))
fit_o3 <- add_criterion(mult.p.o3, criterion = c("loo", "waic"))
fit_o4 <- add_criterion(mult.p.o4, criterion = c("loo", "waic"))
fit_o5 <- add_criterion(mult.p.o5, criterion = c("loo", "waic"))
fit_o6 <- add_criterion(mult.p.o6, criterion = c("loo", "waic"))
fit_o7 <- add_criterion(mult.p.o7, criterion = c("loo", "waic"))

loo_compare(fit_o1, fit_o1.i, criterion = "loo")
model_weights(fit_o1, fit_o2, fit_o3, fit_o4, fit_o5, fit_o6, fit_o7, weights = "loo")
loo_compare(fit_o1, fit_o2, fit_o3, fit_o4, fit_o5, fit_o6, fit_o7, criterion = "loo") 

######################
#P. spiloides
str(P.s)
mult.p.s <- brm(s.color ~ elev.log + temp + precip, 
                data = P.s, family = "categorical",
                warmup = 1000, iter = 2000, chains = 4, cores = 2) 

#interaction
mult.p.s.i <- brm(s.color ~ elev.log + temp * precip, 
                  data = P.s, family = "categorical",
                  warmup = 1000, iter = 2000, chains = 4, cores = 2) 

#model selection
mult.p.s2 <- brm(s.color ~ temp + precip, 
                 data = P.s, family = "categorical",
                 warmup = 1000, iter = 2000, chains = 4, cores = 2) 

mult.p.s3 <- brm(s.color ~ elev.log + temp, 
                 data = P.s, family = "categorical",
                 warmup = 1000, iter = 2000, chains = 4, cores = 2)  

mult.p.s4 <- brm(s.color ~ elev.log + precip, 
                 data = P.s, family = "categorical",
                 warmup = 1000, iter = 2000, chains = 4, cores = 2) 

mult.p.s5 <- brm(s.color ~ temp, 
                 data = P.s, family = "categorical",
                 warmup = 1000, iter = 2000, chains = 4, cores = 2)  

mult.p.s6 <- brm(s.color ~ precip, 
                 data = P.s, family = "categorical",
                 warmup = 1000, iter = 2000, chains = 4, cores = 2) 

mult.p.s7 <- brm(s.color ~ elev.log, 
                 data = P.s, family = "categorical",
                 warmup = 1000, iter = 2000, chains = 4, cores = 2) 

## model selection
fit_s1 <- add_criterion(mult.p.s, criterion = c("loo", "waic"))
fit_s1.i <- add_criterion(mult.p.s.i, criterion = c("loo", "waic"))
fit_s2 <- add_criterion(mult.p.s2, criterion = c("loo", "waic"))
fit_s3 <- add_criterion(mult.p.s3, criterion = c("loo", "waic"))
fit_s4 <- add_criterion(mult.p.s4, criterion = c("loo", "waic"))
fit_s5 <- add_criterion(mult.p.s5, criterion = c("loo", "waic"))
fit_s6 <- add_criterion(mult.p.s6, criterion = c("loo", "waic"))
fit_s7 <- add_criterion(mult.p.s7, criterion = c("loo", "waic"))

loo_compare(fit_s1, fit_s1.i, criterion = "loo")
model_weights(fit_s1, fit_s2, fit_s3, fit_s4, fit_s5, fit_s6, fit_s7, weights = "loo")
loo_compare(fit_s1, fit_s2, fit_s3, fit_s4, fit_s5, fit_s6, fit_s7, criterion = "loo") 

#################################
##plotting 
plot(conditional_effects(mult.mod4, categorical=T))
plot(conditional_effects(mult.p.a, categorical=T))
plot(conditional_effects(mult.p.o, categorical=T))
plot(conditional_effects(mult.p.s, categorical=T))




#############
#supplemental interaction fig
#For supplemental fig 2
ggpredict(mult.mod4.1, c("temp", "precip")) %>% plot(colors = "viridis") + labs(x="Temperature", y="Probability", title = "Predicted probabilities of all species color")
ggpredict(mult.p.a.i, c("temp", "precip")) %>% plot(colors = "viridis") + labs(x="Temperature", y="Probability", title = "Predicted probabilities of P. alleghaniensis color")
ggpredict(mult.p.o.i, c("temp", "precip")) %>% plot(colors = "viridis") + labs(x="Temperature", y="Probability", title = "Predicted probabilities of P. obsoletus color")
ggpredict(mult.p.s.i, c("temp", "precip")) %>% plot(colors = "viridis") + labs(x="Temperature", y="Probability", title = "Predicted probabilities of P. spiloides color")

######for manuscript fig 
####FULL MODEL

fig_precip_cv <- conditional_effects(mult.mod4, effects = "precip", categorical = TRUE, prob = c(0.95)) %>%  
  plot(theme = theme(panel.grid = element_blank()))

legend_title2 <- "Snake colors" #

fig_precip <- fig_precip_cv$`precip:cats__`+ 
  scale_fill_manual(values = c("black"= "black", 
                               "dark_gray"= "gray57", #change to NA to get rid of fill
                               "brown"= "brown",
                               "light_gray" = "gray88",
                               "orange" = "orange",
                               "yellow" = "yellow")) +
  scale_color_manual(legend_title2, values = c("black"= "black", 
                                               "dark_gray"= "gray57", 
                                               "brown"= "brown",
                                               "light_gray" = "gray88",
                                               "orange" = "orange",
                                               "yellow" = "yellow"),
                     labels=c("Black", "Dark gray", "Brown", "Light gray", "Orange", "Yellow")) +
  guides(fill = "none")+
  theme_classic()+
  theme(panel.grid = element_blank())+
  #theme(legend.position = "none")+
  xlab("Annual Precipitation (BIO 12)")+
  ylab("Probability")+
  theme(axis.text.x = element_text(color = "grey20", size = 12),
        axis.text.y = element_text(color = "grey20", size = 12),
        axis.title.x = element_text(color = "grey20", size = 16),
        axis.title.y = element_text(color = "grey20", size = 16)) +
  theme(legend.text=element_text(size=12)) +
  theme(legend.position='top',legend.title=element_text(size=14))+
  guides(colour = guide_legend(override.aes = list(size=2.4, fill=NA))) +
  theme(legend.position = "none") 

fig_precip


################################################################################

fig_temp_cv <- conditional_effects(mult.mod4, effects = "temp", categorical = TRUE, prob = c(0.95)) %>%  
  plot(theme = theme(panel.grid = element_blank()))

fig_temp <- fig_temp_cv$`temp:cats__`+ 
  scale_fill_manual(values = c("black"= "black", 
                               "dark_gray"= "gray57", #change to NA to get rid of fill
                               "brown"= "brown",
                               "light_gray" = "gray88",
                               "orange" = "orange",
                               "yellow" = "yellow")) +
  scale_color_manual(legend_title2, values = c("black"= "black", 
                                               "dark_gray"= "gray57", 
                                               "brown"= "brown",
                                               "light_gray" = "gray88",
                                               "orange" = "orange",
                                               "yellow" = "yellow"), 
                     labels=c("Black", "Dark gray", "Brown", "Light gray", "Orange", "Yellow")) +
  guides(fill = "none")+
  theme_classic()+
  theme(panel.grid = element_blank())+
  #theme(legend.position = "none")+
  xlab("Annual Mean Temperature (BIO 1)")+
  ylab("Probability")+
  theme(axis.text.x = element_text(color = "grey20", size = 12),
        axis.text.y = element_text(color = "grey20", size = 12),
        axis.title.x = element_text(color = "grey20", size = 16),
        axis.title.y = element_text(color = "grey20", size = 16)) +
  theme(legend.text=element_text(size=12)) +
  theme(legend.position='top',legend.title=element_text(size=14))+
  guides(colour = guide_legend(override.aes = list(size=2.4, fill=NA))) +
  ggtitle("All species") +
  theme(plot.title = element_text(color="darkblue", size=16, face="bold.italic"))
#theme(legend.position = "none") 

fig_temp


################################################################################

fig_elev_cv <- conditional_effects(mult.mod4, effects = "elev.log", categorical = TRUE, prob = c(0.95)) %>%  
  plot(theme = theme(panel.grid = element_blank()))

fig_elev <- fig_elev_cv$`elev.log:cats__`+ 
   scale_fill_manual(values = c("black"= "black", 
                               "dark_gray"= "gray57", #change to NA to get rid of fill
                               "brown"= "brown",
                               "light_gray" = "gray88",
                               "orange" = "orange",
                               "yellow" = "yellow")) +
  scale_color_manual(legend_title2, values = c("black"= "black", 
                                               "dark_gray"= "gray57", 
                                               "brown"= "brown",
                                               "light_gray" = "gray88",
                                               "orange" = "orange",
                                               "yellow" = "yellow"), 
                     labels=c("Black", "Dark gray", "Brown", "Light gray", "Orange", "Yellow")) +
  guides(fill = "none")+
  theme_classic()+
  theme(panel.grid = element_blank())+
  #theme(legend.position = "none")+
  xlab("Elevation (log)")+
  ylab("Probability")+
  theme(axis.text.x = element_text(color = "grey20", size = 12),
        axis.text.y = element_text(color = "grey20", size = 12),
        axis.title.x = element_text(color = "grey20", size = 16),
        axis.title.y = element_text(color = "grey20", size = 16)) +
  theme(legend.text=element_text(size=12)) +
  #theme(legend.position=c(.14, .81),legend.title=element_text(size=14))+
  theme(legend.position="top",legend.title=element_text(size=14))+
  guides(colour = guide_legend(override.aes = list(size=2.4, fill=NA))) +
  theme(legend.position = "none") #this last lines overrides the legend

fig_elev

########################################################################################################
#Pantherophis_alleghaniensis

fig_precip_cv_a <- conditional_effects(mult.p.a, effects = "precip", categorical = TRUE, prob = c(0.95)) %>%  
  plot(theme = theme(panel.grid = element_blank()))

legend_title2 <- "Snake colors" #

fig_precip_p_a <- fig_precip_cv_a$`precip:cats__`+ 
 scale_fill_manual(values = c("black"= "black", 
                               "dark_gray"= "gray57", #change to NA to get rid of fill
                               "brown"= "brown",
                               "light_gray" = "gray88",
                               "orange" = "orange",
                               "yellow" = "yellow")) +
  scale_color_manual(legend_title2, values = c("black"= "black", 
                                               "dark_gray"= "gray57", 
                                               "brown"= "brown",
                                               "light_gray" = "gray88",
                                               "orange" = "orange",
                                               "yellow" = "yellow"),
                     labels=c("Black", "Dark gray", "Brown", "Light gray", "Orange", "Yellow")) +
  guides(fill = "none")+
  theme_classic()+
  theme(panel.grid = element_blank())+
  #theme(legend.position = "none")+
  xlab("Annual Precipitation (BIO 12)")+
  ylab("Probability")+
  theme(axis.text.x = element_text(color = "grey20", size = 12),
        axis.text.y = element_text(color = "grey20", size = 12),
        axis.title.x = element_text(color = "grey20", size = 16),
        axis.title.y = element_text(color = "grey20", size = 16)) +
  theme(legend.text=element_text(size=12)) +
  theme(legend.position='top',legend.title=element_text(size=14))+
  guides(colour = guide_legend(override.aes = list(size=2.4, fill=NA))) +
  theme(legend.position = "none") 

fig_precip_p_a


################################################################################

fig_temp_cv_p_a <- conditional_effects(mult.p.a, effects = "temp", categorical = TRUE, prob = c(0.95)) %>%  
  plot(theme = theme(panel.grid = element_blank()))

fig_temp_p_a <- fig_temp_cv_p_a$`temp:cats__`+ 
  scale_fill_manual(values = c("black"= "black", 
                               "dark_gray"= "gray57", #change to NA to get rid of fill
                               "brown"= "brown",
                               "light_gray" = "gray88",
                               "orange" = "orange",
                               "yellow" = "yellow")) +
  scale_color_manual(legend_title2, values = c("black"= "black", 
                                               "dark_gray"= "gray57", 
                                               "brown"= "brown",
                                               "light_gray" = "gray88",
                                               "orange" = "orange",
                                               "yellow" = "yellow"), 
                     labels=c("Black", "Dark gray", "Brown", "Light gray", "Orange", "Yellow")) +
  guides(fill = "none")+
  theme_classic()+
  theme(panel.grid = element_blank())+
  #theme(legend.position = "none")+
  xlab("Annual Mean Temperature (BIO 1)")+
  ylab("Probability")+
  theme(axis.text.x = element_text(color = "grey20", size = 12),
        axis.text.y = element_text(color = "grey20", size = 12),
        axis.title.x = element_text(color = "grey20", size = 16),
        axis.title.y = element_text(color = "grey20", size = 16)) +
  theme(legend.text=element_text(size=12)) +
  theme(legend.position='top',legend.title=element_text(size=14))+
  guides(colour = guide_legend(override.aes = list(size=2.4, fill=NA))) +
  theme(legend.position = "none") + ggtitle("P. alleghaniensis") +
  theme(plot.title = element_text(color="darkblue", size=16, face="bold.italic"))

fig_temp_p_a

################################################################################

fig_elev_cv_p_a <- conditional_effects(mult.p.a, effects = "elev.log", categorical = TRUE, prob = c(0.95)) %>%  
  plot(theme = theme(panel.grid = element_blank()))

fig_elev_p_a <- fig_elev_cv_p_a$`elev.log:cats__`+ 
  scale_fill_manual(values = c("black"= "black", 
                               "dark_gray"= "gray57", #change to NA to get rid of fill
                               "brown"= "brown",
                               "light_gray" = "gray88",
                               "orange" = "orange",
                               "yellow" = "yellow")) +
  scale_color_manual(legend_title2, values = c("black"= "black", 
                                               "dark_gray"= "gray57", 
                                               "brown"= "brown",
                                               "light_gray" = "gray88",
                                               "orange" = "orange",
                                               "yellow" = "yellow"), 
                     labels=c("Black", "Dark gray", "Brown", "Light gray", "Orange", "Yellow")) +
  guides(fill = "none")+
  theme_classic()+
  theme(panel.grid = element_blank())+
  #theme(legend.position = "none")+
  xlab("Elevation (log)")+
  ylab("Probability")+
  theme(axis.text.x = element_text(color = "grey20", size = 12),
        axis.text.y = element_text(color = "grey20", size = 12),
        axis.title.x = element_text(color = "grey20", size = 16),
        axis.title.y = element_text(color = "grey20", size = 16)) +
  theme(legend.text=element_text(size=12)) +
  #theme(legend.position=c(.14, .81),legend.title=element_text(size=14))+
  theme(legend.position="top",legend.title=element_text(size=14))+
  guides(colour = guide_legend(override.aes = list(size=2.4, fill=NA))) +
  theme(legend.position = "none") #this last lines overrides the legend

fig_elev_p_a

########################################################################################################
#Pantherophis_obsoletus

fig_precip_cv_o <- conditional_effects(mult.p.o, effects = "precip", categorical = TRUE, prob = c(0.95)) %>%  
  plot(theme = theme(panel.grid = element_blank()))

legend_title2 <- "Snake colors" #

fig_precip_p_o <- fig_precip_cv_o$`precip:cats__`+ 
  scale_fill_manual(values = c("black"= "black", 
                               "dark_gray"= "gray57", #change to NA to get rid of fill
                               "brown"= "brown",
                               "light_gray" = "gray88",
                               "orange" = "orange",
                               "yellow" = "yellow")) +
  scale_color_manual(legend_title2, values = c("black"= "black", 
                                               "dark_gray"= "gray57", 
                                               "brown"= "brown",
                                               "light_gray" = "gray88",
                                               "orange" = "orange",
                                               "yellow" = "yellow"),
                     labels=c("Black", "Dark gray", "Brown", "Light gray", "Orange", "Yellow")) +
  guides(fill = "none")+
  theme_classic()+
  theme(panel.grid = element_blank())+
  #theme(legend.position = "none")+
  xlab("Annual Precipitation (BIO 12)")+
  ylab("Probability")+
  theme(axis.text.x = element_text(color = "grey20", size = 12),
        axis.text.y = element_text(color = "grey20", size = 12),
        axis.title.x = element_text(color = "grey20", size = 16),
        axis.title.y = element_text(color = "grey20", size = 16)) +
  theme(legend.text=element_text(size=12)) +
  theme(legend.position='top',legend.title=element_text(size=14))+
  guides(colour = guide_legend(override.aes = list(size=2.4, fill=NA))) +
  theme(legend.position = "none") 

fig_precip_p_o


################################################################################

fig_temp_cv_p_o <- conditional_effects(mult.p.o, effects = "temp", categorical = TRUE, prob = c(0.95)) %>%  
  plot(theme = theme(panel.grid = element_blank()))

fig_temp_p_o <- fig_temp_cv_p_o$`temp:cats__`+ 
  scale_fill_manual(values = c("black"= "black", 
                               "dark_gray"= "gray57", #change to NA to get rid of fill
                               "brown"= "brown",
                               "light_gray" = "gray88",
                               "orange" = "orange",
                               "yellow" = "yellow")) +
  scale_color_manual(legend_title2, values = c("black"= "black", 
                                               "dark_gray"= "gray57", 
                                               "brown"= "brown",
                                               "light_gray" = "gray88",
                                               "orange" = "orange",
                                               "yellow" = "yellow"), 
                     labels=c("Black", "Dark gray", "Brown", "Light gray", "Orange", "Yellow")) +
  guides(fill = "none")+
  theme_classic()+
  theme(panel.grid = element_blank())+
  #theme(legend.position = "none")+
  xlab("Annual Mean Temperature (BIO 1)")+
  ylab("Probability")+
  theme(axis.text.x = element_text(color = "grey20", size = 12),
        axis.text.y = element_text(color = "grey20", size = 12),
        axis.title.x = element_text(color = "grey20", size = 16),
        axis.title.y = element_text(color = "grey20", size = 16)) +
  theme(legend.text=element_text(size=12)) +
  theme(legend.position='top',legend.title=element_text(size=14))+
  guides(colour = guide_legend(override.aes = list(size=2.4, fill=NA))) +
  theme(legend.position = "none") + ggtitle("P. obsoletus") +
  theme(plot.title = element_text(color="darkblue", size=16, face="bold.italic"))

fig_temp_p_o

################################################################################

fig_elev_cv_p_o <- conditional_effects(mult.p.o, effects = "elev.log", categorical = TRUE, prob = c(0.95)) %>%  
  plot(theme = theme(panel.grid = element_blank()))

fig_elev_p_o <- fig_elev_cv_p_o$`elev.log:cats__`+ 
  scale_fill_manual(values = c("black"= "black", 
                               "dark_gray"= "gray57", #change to NA to get rid of fill
                               "brown"= "brown",
                               "light_gray" = "gray88",
                               "orange" = "orange",
                               "yellow" = "yellow")) +
  scale_color_manual(legend_title2, values = c("black"= "black", 
                                               "dark_gray"= "gray57", 
                                               "brown"= "brown",
                                               "light_gray" = "gray88",
                                               "orange" = "orange",
                                               "yellow" = "yellow"), 
                     labels=c("Black", "Dark gray", "Brown", "Light gray", "Orange", "Yellow")) +
  guides(fill = "none")+
  theme_classic()+
  theme(panel.grid = element_blank())+
  #theme(legend.position = "none")+
  xlab("Elevation (log)")+
  ylab("Probability")+
  theme(axis.text.x = element_text(color = "grey20", size = 12),
        axis.text.y = element_text(color = "grey20", size = 12),
        axis.title.x = element_text(color = "grey20", size = 16),
        axis.title.y = element_text(color = "grey20", size = 16)) +
  theme(legend.text=element_text(size=12)) +
  #theme(legend.position=c(.14, .81),legend.title=element_text(size=14))+
  theme(legend.position="top",legend.title=element_text(size=14))+
  guides(colour = guide_legend(override.aes = list(size=2.4, fill=NA))) +
  theme(legend.position = "none") #this last lines overrides the legend

fig_elev_p_o

########################################################################################################
#Pantherophis_spiloides

fig_precip_cv_s <- conditional_effects(mult.p.s, effects = "precip", categorical = TRUE, prob = c(0.95)) %>%  
  plot(theme = theme(panel.grid = element_blank()))

legend_title2 <- "Snake colors" #

fig_precip_p_s <- fig_precip_cv_s$`precip:cats__`+ 
  scale_fill_manual(values = c("black"= "black", 
                               "dark_gray"= "gray57", #change to NA to get rid of fill
                               "brown"= "brown",
                               "light_gray" = "gray88",
                               "orange" = "orange",
                               "yellow" = "yellow")) +
  scale_color_manual(legend_title2, values = c("black"= "black", 
                                               "dark_gray"= "gray57", 
                                               "brown"= "brown",
                                               "light_gray" = "gray88",
                                               "orange" = "orange",
                                               "yellow" = "yellow"),
                     labels=c("Black", "Dark gray", "Brown", "Light gray", "Orange", "Yellow")) +
  guides(fill = "none")+
  theme_classic()+
  theme(panel.grid = element_blank())+
  #theme(legend.position = "none")+
  xlab("Annual Precipitation (BIO 12)")+
  ylab("Probability")+
  theme(axis.text.x = element_text(color = "grey20", size = 12),
        axis.text.y = element_text(color = "grey20", size = 12),
        axis.title.x = element_text(color = "grey20", size = 16),
        axis.title.y = element_text(color = "grey20", size = 16)) +
  theme(legend.text=element_text(size=12)) +
  theme(legend.position='top',legend.title=element_text(size=14))+
  guides(colour = guide_legend(override.aes = list(size=2.4, fill=NA))) +
  theme(legend.position = "none") 

fig_precip_p_s

################################################################################

fig_temp_cv_p_s <- conditional_effects(mult.p.s, effects = "temp", categorical = TRUE, prob = c(0.95)) %>%  
  plot(theme = theme(panel.grid = element_blank()))

fig_temp_p_s <- fig_temp_cv_p_s$`temp:cats__`+ 
   scale_fill_manual(values = c("black"= "black", 
                               "dark_gray"= "gray57", #change to NA to get rid of fill
                               "brown"= "brown",
                               "light_gray" = "gray88",
                               "orange" = "orange",
                               "yellow" = "yellow")) +
  scale_color_manual(legend_title2, values = c("black"= "black", 
                                               "dark_gray"= "gray57", 
                                               "brown"= "brown",
                                               "light_gray" = "gray88",
                                               "orange" = "orange",
                                               "yellow" = "yellow"), 
                     labels=c("Black", "Dark gray", "Brown", "Light gray", "Orange", "Yellow")) +
  guides(fill = "none")+
  theme_classic()+
  theme(panel.grid = element_blank())+
  #theme(legend.position = "none")+
  xlab("Annual Mean Temperature (BIO 1)")+
  ylab("Probability")+
  theme(axis.text.x = element_text(color = "grey20", size = 12),
        axis.text.y = element_text(color = "grey20", size = 12),
        axis.title.x = element_text(color = "grey20", size = 16),
        axis.title.y = element_text(color = "grey20", size = 16)) +
  theme(legend.text=element_text(size=12)) +
  theme(legend.position='top',legend.title=element_text(size=14))+
  guides(colour = guide_legend(override.aes = list(size=2.4, fill=NA))) +
  theme(legend.position = "none") + ggtitle("P. spiloides") +
  theme(plot.title = element_text(color="darkblue", size=16, face="bold.italic"))

fig_temp_p_s

################################################################################

fig_elev_cv_p_s <- conditional_effects(mult.p.s, effects = "elev.log", categorical = TRUE, prob = c(0.95)) %>%  
  plot(theme = theme(panel.grid = element_blank()))

fig_elev_p_s <- fig_elev_cv_p_s$`elev.log:cats__`+ 
  scale_fill_manual(values = c("black"= "black", 
                               "dark_gray"= "gray57", #change to NA to get rid of fill
                               "brown"= "brown",
                               "light_gray" = "gray88",
                               "orange" = "orange",
                               "yellow" = "yellow")) +
  scale_color_manual(legend_title2, values = c("black"= "black", 
                                               "dark_gray"= "gray57", 
                                               "brown"= "brown",
                                               "light_gray" = "gray88",
                                               "orange" = "orange",
                                               "yellow" = "yellow"), 
                     labels=c("Black", "Dark gray", "Brown", "Light gray", "Orange", "Yellow")) +
  guides(fill = "none")+
  theme_classic()+
  theme(panel.grid = element_blank())+
  #theme(legend.position = "none")+
  xlab("Elevation (log)")+
  ylab("Probability")+
  theme(axis.text.x = element_text(color = "grey20", size = 12),
        axis.text.y = element_text(color = "grey20", size = 12),
        axis.title.x = element_text(color = "grey20", size = 16),
        axis.title.y = element_text(color = "grey20", size = 16)) +
  theme(legend.text=element_text(size=12)) +
  #theme(legend.position=c(.14, .81),legend.title=element_text(size=14))+
  theme(legend.position="top",legend.title=element_text(size=14))+
  guides(colour = guide_legend(override.aes = list(size=2.4, fill=NA))) +
  theme(legend.position = "none") #this last lines overrides the legend

fig_elev_p_s

################
snake_plots_all <- egg::ggarrange(fig_temp, fig_precip, fig_elev, fig_temp_p_a, fig_precip_p_a, fig_elev_p_a,
                                  fig_temp_p_s, fig_precip_p_s, fig_elev_p_s, fig_temp_p_o, fig_precip_p_o, fig_elev_p_o,
                                  ncol = 3, labels = c("A", "B", 'C', "D", "E", 'F', "G", "H", 'I', "J", "K", 'L'))

ggsave(snake_plots_all, filename = "/Users/Maggie/Dropbox/Pantherophis_project/Figs/Snake_col_resp_all.pdf",
       width = 12, height = 12)


########################################################################################################
#Species elevation differences
elev.fit <- aov(elev.log ~ scientific_name, data=snake_data2)
summary(elev.fit)
#plot(elev.fit)
TukeyHSD(elev.fit)
plot(TukeyHSD(elev.fit), las=2)
boxplot(elev.log ~ scientific_name, data=snake_data2,
        xlab= "", ylab = "Elevation (log)",
        names=c("P. alleghaniensis","P. obsoletus","P. spiloides"), col=c("cyan3", "darkorchid2", "coral"))









