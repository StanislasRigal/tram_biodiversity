# PEPITE_CE_Analysis

## Required packages

```{r}

library(see)
library(dplyr)
library(ggplot2) 
library(gmnl) 
library(mlogit)
library(tibble)
library(tidyr)
library(AER)
library(poLCA)
library(DescTools)
library(sf)
library(mapview)
library(exactextractr)
library(terra)
library(raster)
library(ggplot2)
library(viridis)
library(ggthemes)
library(reshape2)
library(plyr)
library(dplyr)
library(see)
library(ggstatsplot)
library(ggsignif)
library(ggridges)
library(hrbrthemes)

```

## Mixed logit

### 0. Prepare DCE data with covariables

```{r}
# load DCE data processed for the analysis

data_DCE_numeric <- read.csv("data_DCE_numeric_publi.csv")
data_DCE_numeric$Biome <- as.factor(data_DCE_numeric$Biome)

# transform data format for latent class analysis

data_DCE_mlogit <- mlogit.data(data_DCE_numeric,
                               choice = "choice",
                               shape = "long",
                               alt.var = "Scenario",
                               id.var = "survey_person",
                               chid.var = "chid")

```

### 1. Get the best model from saturated model

#### 1.1 Models

```{r}

# saturated model

mixl_Time1 <- gmnl(choice ~ Time + Landscape + Acces + Biodiversity + Biome + asc | 0 | 0 | Gender + Age + Income_cap + class_nat + survey_id +
                        job_travel + Perso_relation_nature + Perso_behaviour_nature + Perso_knowledge_biodiversity,
                      data = data_DCE_mlogit,
                      model = "mixl",
                      panel=TRUE,
                      ranp = c(Time = "t",
                               Landscape = "n",
                               Acces = "n",
                               Biodiversity = "n",
                               Biome1 = "n",
                               Biome2 = "n",
                               asc = "n"),
                      mvar = list(Time = c("Gender", "Age", "Income_cap","Perso_relation_nature","Perso_behaviour_nature","Perso_knowledge_biodiversity","survey_id","class_nat","job_travel"),
                                  Landscape = c("Gender", "Age", "Income_cap","Perso_relation_nature","Perso_behaviour_nature","Perso_knowledge_biodiversity","survey_id","class_nat","job_travel"),
                                  Acces = c("Gender", "Age", "Income_cap","Perso_relation_nature","Perso_behaviour_nature","Perso_knowledge_biodiversity","survey_id","class_nat","job_travel"),
                                  Biodiversity = c("Gender", "Age", "Income_cap","Perso_relation_nature","Perso_behaviour_nature","Perso_knowledge_biodiversity","survey_id","class_nat","job_travel"),
                                  asc = c("Gender", "Age", "Income_cap","Perso_relation_nature","Perso_behaviour_nature","Perso_knowledge_biodiversity","survey_id","class_nat","job_travel")),
                      R = 2000)
                      
# test each model after removing on interaction by attribute

mixl_Time2 <- gmnl(choice ~ Time + Landscape + Acces + Biodiversity + Biome + asc | 0 | 0 | Gender + Age + Income_cap + class_nat + survey_id +
                        job_travel + Perso_relation_nature + Perso_behaviour_nature + Perso_knowledge_biodiversity,
                      data = data_DCE_mlogit,
                      model = "mixl",
                      panel=TRUE,
                      ranp = c(Time = "t",
                               Landscape = "n",
                               Acces = "n",
                               Biodiversity = "n",
                               Biome1 = "n",
                               Biome2 = "n",
                               asc = "n"),
                      mvar = list(Time = c("Gender", "Age", "Income_cap","Perso_relation_nature","Perso_behaviour_nature","Perso_knowledge_biodiversity","class_nat","job_travel"),
                                  Landscape = c("Gender", "Age", "Income_cap","Perso_relation_nature","Perso_behaviour_nature","Perso_knowledge_biodiversity","survey_id","job_travel"),
                                  Acces = c("Gender", "Age", "Income_cap","Perso_behaviour_nature","Perso_knowledge_biodiversity","survey_id","class_nat","job_travel"),
                                  Biodiversity = c("Gender", "Age","Perso_relation_nature","Perso_behaviour_nature","Perso_knowledge_biodiversity","survey_id","class_nat","job_travel"),
                                  asc = c("Age", "Income_cap","Perso_relation_nature","Perso_behaviour_nature","Perso_knowledge_biodiversity","survey_id","class_nat","job_travel")),
                      R = 2000)


mixl_Time3 <- gmnl(choice ~ Time + Landscape + Acces + Biodiversity + Biome + asc | 0 | 0 | Gender + Age + Income_cap + class_nat + survey_id +
                        job_travel + Perso_relation_nature + Perso_behaviour_nature + Perso_knowledge_biodiversity,
                      data = data_DCE_mlogit,
                      model = "mixl",
                      panel=TRUE,
                      ranp = c(Time = "t",
                               Landscape = "n",
                               Acces = "n",
                               Biodiversity = "n",
                               Biome1 = "n",
                               Biome2 = "n",
                               asc = "n"),
                      mvar = list(Time = c("Gender", "Age", "Income_cap","Perso_relation_nature","Perso_behaviour_nature","class_nat","job_travel"),
                                  Landscape = c("Gender", "Age", "Income_cap","Perso_relation_nature","Perso_behaviour_nature","Perso_knowledge_biodiversity","job_travel"),
                                  Acces = c("Gender", "Age", "Income_cap","Perso_knowledge_biodiversity","survey_id","class_nat","job_travel"),
                                  Biodiversity = c("Gender", "Age","Perso_behaviour_nature","Perso_knowledge_biodiversity","survey_id","class_nat","job_travel"),
                                  asc = c("Income_cap","Perso_relation_nature","Perso_behaviour_nature","Perso_knowledge_biodiversity","survey_id","class_nat","job_travel")),
                      R = 2000)

mixl_Time4 <- gmnl(choice ~ Time + Landscape + Acces + Biodiversity + Biome + asc | 0 | 0 | Gender + Age + Income_cap + class_nat + survey_id +
                        job_travel + Perso_relation_nature + Perso_behaviour_nature + Perso_knowledge_biodiversity,
                      data = data_DCE_mlogit,
                      model = "mixl",
                      panel=TRUE,
                      ranp = c(Time = "t",
                               Landscape = "n",
                               Acces = "n",
                               Biodiversity = "n",
                               Biome1 = "n",
                               Biome2 = "n",
                               asc = "n"),
                      mvar = list(Time = c("Gender", "Age", "Income_cap","Perso_relation_nature","Perso_behaviour_nature","class_nat"),
                                  Landscape = c("Gender", "Age", "Income_cap","Perso_relation_nature","Perso_behaviour_nature","job_travel"),
                                  Acces = c("Gender", "Age", "Income_cap","survey_id","class_nat","job_travel"),
                                  Biodiversity = c("Gender", "Age","Perso_knowledge_biodiversity","survey_id","class_nat","job_travel"),
                                  asc = c("Income_cap","Perso_relation_nature","Perso_knowledge_biodiversity","survey_id","class_nat","job_travel")),
                      R = 2000)

mixl_Time5 <- gmnl(choice ~ Time + Landscape + Acces + Biodiversity + Biome + asc | 0 | 0 | Gender + Age + Income_cap + class_nat + survey_id +
                        job_travel + Perso_relation_nature + Perso_behaviour_nature + Perso_knowledge_biodiversity,
                      data = data_DCE_mlogit,
                      model = "mixl",
                      panel=TRUE,
                      ranp = c(Time = "t",
                               Landscape = "n",
                               Acces = "n",
                               Biodiversity = "n",
                               Biome1 = "n",
                               Biome2 = "n",
                               asc = "n"),
                      mvar = list(Time = c("Gender", "Age", "Income_cap","Perso_relation_nature","class_nat"),
                                  Landscape = c("Gender", "Age", "Income_cap","Perso_relation_nature","job_travel"),
                                  Acces = c("Gender", "Age","survey_id","class_nat","job_travel"),
                                  Biodiversity = c("Gender", "Age","survey_id","class_nat","job_travel"),
                                  asc = c("Income_cap","Perso_relation_nature","survey_id","class_nat","job_travel")),
                      R = 2000)

mixl_Time6 <- gmnl(choice ~ Time + Landscape + Acces + Biodiversity + Biome + asc | 0 | 0 | Gender + Age + Income_cap + class_nat + survey_id +
                        job_travel + Perso_relation_nature + Perso_behaviour_nature + Perso_knowledge_biodiversity,
                      data = data_DCE_mlogit,
                      model = "mixl",
                      panel=TRUE,
                      ranp = c(Time = "t",
                               Landscape = "n",
                               Acces = "n",
                               Biodiversity = "n",
                               Biome1 = "n",
                               Biome2 = "n",
                               asc = "n"),
                      mvar = list(Time = c("Gender", "Age", "Income_cap","Perso_relation_nature"),
                                  Landscape = c("Gender", "Age", "Income_cap","Perso_relation_nature"),
                                  Acces = c("Gender", "Age","class_nat","job_travel"),
                                  Biodiversity = c("Gender", "Age","class_nat","job_travel"),
                                  asc = c("Income_cap","Perso_relation_nature","survey_id","job_travel")),
                      R = 2000)


mixl_Time7 <- gmnl(choice ~ Time + Landscape + Acces + Biodiversity + Biome + asc | 0 | 0 | Gender + Age + Income_cap + class_nat + survey_id +
                        job_travel + Perso_relation_nature + Perso_behaviour_nature + Perso_knowledge_biodiversity,
                      data = data_DCE_mlogit,
                      model = "mixl",
                      panel=TRUE,
                      ranp = c(Time = "t",
                               Landscape = "n",
                               Acces = "n",
                               Biodiversity = "n",
                               Biome1 = "n",
                               Biome2 = "n",
                               asc = "n"),
                      mvar = list(Time = c("Gender", "Age", "Income_cap"),
                                  Landscape = c("Gender", "Age", "Income_cap"),
                                  Acces = c("Gender", "Age","job_travel"),
                                  Biodiversity = c("Gender", "Age","job_travel"),
                                  asc = c("Income_cap","Perso_relation_nature","job_travel")),
                      R = 2000)


mixl_Time8 <- gmnl(choice ~ Time + Landscape + Acces + Biodiversity + Biome + asc | 0 | 0 | Gender + Age + Income_cap + class_nat + survey_id +
                        job_travel + Perso_relation_nature + Perso_behaviour_nature + Perso_knowledge_biodiversity,
                      data = data_DCE_mlogit,
                      model = "mixl",
                      panel=TRUE,
                      ranp = c(Time = "t",
                               Landscape = "n",
                               Acces = "n",
                               Biodiversity = "n",
                               Biome1 = "n",
                               Biome2 = "n",
                               asc = "n"),
                      mvar = list(Time = c("Gender", "Age"),
                                  Landscape = c("Gender", "Age"),
                                  Acces = c("Gender", "Age","job_travel"),
                                  Biodiversity = c("Gender", "Age"),
                                  asc = c("Perso_relation_nature","job_travel")),
                      R = 2000)



mixl_Time9 <- gmnl(choice ~ Time + Landscape + Acces + Biodiversity + Biome + asc | 0 | 0 | Gender + Age + Income_cap + class_nat + survey_id +
                        job_travel + Perso_relation_nature + Perso_behaviour_nature + Perso_knowledge_biodiversity,
                      data = data_DCE_mlogit,
                      model = "mixl",
                      panel=TRUE,
                      ranp = c(Time = "t",
                               Landscape = "n",
                               Acces = "n",
                               Biodiversity = "n",
                               Biome1 = "n",
                               Biome2 = "n",
                               asc = "n"),
                      mvar = list(Time = c("Gender", "Age"),
                                  Landscape = c("Gender", "Age"),
                                  Acces = c("Gender", "Age","job_travel"),
                                  Biodiversity = c("Age"),
                                  asc = c("Perso_relation_nature","job_travel")),
                      R = 2000)



mixl_Time10 <- gmnl(choice ~ Time + Landscape + Acces + Biodiversity + Biome + asc | 0 | 0 | Gender + Age + Income_cap + class_nat + survey_id +
                        job_travel + Perso_relation_nature + Perso_behaviour_nature + Perso_knowledge_biodiversity,
                      data = data_DCE_mlogit,
                      model = "mixl",
                      panel=TRUE,
                      ranp = c(Time = "t",
                               Landscape = "n",
                               Acces = "n",
                               Biodiversity = "n",
                               Biome1 = "n",
                               Biome2 = "n",
                               asc = "n"),
                      mvar = list(Time = c("Gender", "Age"),
                                  Landscape = c("Gender", "Age"),
                                  Acces = c("Gender", "Age","job_travel"),
                                  Biodiversity = c("Gender", "Age"),
                                  asc = c("Perso_relation_nature","job_travel","survey_id")),
                      R = 2000)



mixl_Time11 <- gmnl(choice ~ Time + Landscape + Acces + Biodiversity + Biome + asc | 0 | 0 | Gender + Age + Income_cap + class_nat + survey_id +
                        job_travel + Perso_relation_nature + Perso_behaviour_nature + Perso_knowledge_biodiversity,
                      data = data_DCE_mlogit,
                      model = "mixl",
                      panel=TRUE,
                      ranp = c(Time = "t",
                               Landscape = "n",
                               Acces = "n",
                               Biodiversity = "n",
                               Biome1 = "n",
                               Biome2 = "n",
                               asc = "n"),
                      mvar = list(Time = c("Gender", "Age"),
                                  Landscape = c("Gender", "Age"),
                                  Acces = c("Gender", "Age","job_travel"),
                                  Biodiversity = c("Gender", "Age"),
                                  asc = c("Perso_relation_nature","survey_id")),
                      R = 2000)



mixl_Time12 <- gmnl(choice ~ Time + Landscape + Acces + Biodiversity + Biome + asc | 0 | 0 | Gender + Age + Income_cap + class_nat + survey_id +
                            job_travel + Perso_relation_nature + Perso_behaviour_nature + Perso_knowledge_biodiversity,
                          data = data_DCE_mlogit,
                          model = "mixl",
                          panel=TRUE,
                          ranp = c(Time = "t",
                                   Landscape = "n",
                                   Acces = "n",
                                   Biodiversity = "n",
                                   Biome1 = "n",
                                   Biome2 = "n",
                                   asc = "n"),
                          mvar = list(Time = c("Gender", "Age"),
                                      Landscape = c("Gender", "Age"),
                                      Acces = c("Gender", "Age"),
                                      Biodiversity = c("Gender", "Age","Perso_relation_nature"),
                                      asc = c("Perso_relation_nature","job_travel","survey_id")),
                          R = 2000)


```


#### 1.2 Model fit criteria

```{r}

AIC_BIC_mixl <- data.frame(model = factor(paste0("M",1:12), levels=paste0("M",1:12)),
                            AIC = c(AIC(mixl_Time1),AIC(mixl_Time2),AIC(mixl_Time3),
                                    AIC(mixl_Time4),AIC(mixl_Time5),AIC(mixl_Time6),
                                    AIC(mixl_Time7),AIC(mixl_Time8),AIC(mixl_Time9),
                                    AIC(mixl_Time10),AIC(mixl_Time11),AIC(mixl_Time12)),
                            BIC = c(BIC(mixl_Time1),BIC(mixl_Time2),BIC(mixl_Time3),
                                    BIC(mixl_Time4),BIC(mixl_Time5),BIC(mixl_Time6),
                                    BIC(mixl_Time7),BIC(mixl_Time8),BIC(mixl_Time9),
                                    BIC(mixl_Time10),BIC(mixl_Time11),BIC(mixl_Time12)))

ggplot(AIC_BIC_mixl, aes(x=model)) +
  geom_point(aes(y=AIC), col="grey") +
  geom_point(aes(y=BIC-200)) + 
  scale_y_continuous(
    name = "BIC",
    sec.axis = sec_axis( trans=~.-200, name="AIC")
  ) + xlab("Models") +
  theme_modern()


# best model

mixl_Time <- mixl_Time12

```

### 1.3 Plot model result

```{r}

boxLabels <- names(coef(mixl_Time))[1:(length(coef(mixl_Time))-7)]

df <- data.frame(yAxis = length(boxLabels):1,
                 Attribute = c("Time", "Landscape","Nature use","Biodiversity","Biome peri-urban","Biome rural","ASC",
                               rep("Time",2),rep("Landscape",2),rep("Nature use",2),rep("Biodiversity",3),rep("ASC",3)),
                 Variable = c(rep("Main estimate",7),rep(c("Gender","Age"),4),"INS",
                              "INS","Commuting","Informative framing"),
                 box_estimate_main = coef(mixl_Time)[1:length(boxLabels)], 
                 boxCILow = coef(mixl_Time)[1:length(boxLabels)]-1.96*summary(mixl_Time)$CoefTable[1:length(boxLabels),2], 
                 boxCIHigh = coef(mixl_Time)[1:length(boxLabels)]+1.96*summary(mixl_Time)$CoefTable[1:length(boxLabels),2],
                 var = (summary(mixl_Time)$CoefTable[1:length(boxLabels),2])^2,
                 signif = ifelse(summary(mixl_Time)$CoefTable[1:length(boxLabels),4] < 0.05,"yes","no")
                 )

df <- df %>% group_by(Attribute) %>% mutate(box_estimate_interaction = box_estimate_main[which(Variable=="Main estimate")]+box_estimate_main,
                                            sd_interaction = sqrt(var[which(Variable=="Main estimate")]+var))

df[which(df$Variable=="Main estimate"),c("box_estimate_interaction","sd_interaction")] <- NA

df$CIinteractionHigh <- df$box_estimate_interaction + 1.96*sqrt(df$var)
df$CIinteractionLow <- df$box_estimate_interaction - 1.96*sqrt(df$var)

df <- as.data.frame(df)
df$Attribute <- factor(df$Attribute, levels = c("Time", "Landscape","Nature use","Biodiversity","Biome peri-urban","Biome rural","ASC"))
df$Variable <- factor(df$Variable, levels = c("Main estimate","Gender","Age","INS","Commuting","Informative framing"))

ggplot(df[which(!(df$Attribute %in% c("Biome peri-urban","Biome rural"))),], aes(x=box_estimate_interaction,y = Variable, group=Attribute)) + 
  geom_vline(data=df[which(!(df$Attribute %in% c("Biome peri-urban","Biome rural")) & df$Variable=="Main estimate"),], aes(xintercept = box_estimate_main), linewidth = .25, linetype = "dotted") + 
  geom_vline(xintercept = 0, linewidth = .5, linetype="dashed") + 
  geom_errorbarh(aes(xmax = CIinteractionHigh, xmin = CIinteractionLow), linewidth = .5, height = 
                   .2, color = "gray50") +
  geom_errorbarh(data=df[which(!(df$Attribute %in% c("Biome peri-urban","Biome rural")) & df$Variable=="Main estimate"),],aes(xmax = boxCIHigh, xmin = boxCILow), linewidth = .5, height = 
                   .2, color = "gray50") +
  geom_point(size = 3.5, aes(color = Attribute, alpha=signif)) + 
  geom_point(data=df[which(!(df$Attribute %in% c("Biome peri-urban","Biome rural")) & df$Variable=="Main estimate"),],size = 3.5, aes(color = Attribute,x=box_estimate_main,alpha=signif)) +
  theme_modern() + theme(legend.position = "none") + scale_color_viridis_d() + scale_alpha_discrete(range = c(0.4, 1)) +
  ylab("") +
  xlab("Estimate") + facet_grid(. ~ Attribute, scales='free')

```


### 1.4 Unconditional distribution

```{r}

# Retrieve the estimated parameters for unconditional distribution

# for time

mu <- coef(mixl_Time)['Time']
sigma <- coef(mixl_Time)['sd.Time']

# Percent of population with a positive parameter for Time

1 - ptri(0, mode = coef(mixl_Time)['Time'],
         min = coef(mixl_Time)['Time']-1.96*coef(mixl_Time)['sd.Time'],
         max = coef(mixl_Time)['Time']+1.96*coef(mixl_Time)['sd.Time'])
         
df_uncond <- data.frame(x = seq(from = -2,
                        to = 0.75,
                        by = 0.005)) %>%
                        
  # Draw from the normal distribution for x given the mean and sd
  mutate(normal = dtri(x,
                       mode = mu,
                       min = mu-1.96*sigma,
                       max = mu+1.96*sigma))
                       
bn_Time <- effect.gmnl(mixl_Time,
                        par = "Time",# Choose conditional effect
                        effect = "ce")
df_cond <- data.frame(bn_Time = bn_Time$mean)


ggplot() +
  geom_area(data = df_uncond, aes(x = x, y = normal),
            fill = "grey", alpha = 0.5) +
  geom_density(data = df_cond, aes(x = bn_Time),
               fill = "orange", colour = NA, alpha = 0.5) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + 
  theme_modern() + ylab("f(x)") + xlab(expression(beta[n]["Travel time increase"])) 

# for landscape

mu <- coef(mixl_Time)['Landscape']
sigma <- coef(mixl_Time)['sd.Landscape']
df_uncond <- data.frame(x = seq(from = -3,to = 3,by = 0.005)) %>%
  mutate(normal = dnorm(x,mean = mu,sd = sigma))
bn_Landscape <- effect.gmnl(mixl_Time,par = "Landscape",effect = "ce")
df_cond <- data.frame(bn_Landscape = bn_Landscape$mean)

ggplot() +
  geom_area(data = df_uncond, aes(x = x, y = normal),
            fill = "grey", alpha = 0.5) +
  geom_density(data = df_cond, aes(x = bn_Landscape),
               fill = "orange", colour = NA, alpha = 0.5) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + 
  theme_modern() + ylab("f(x)") + xlab(expression(beta[n][Landscape]))

# for nature use

mu <- coef(mixl_Time)['Acces']
sigma <- coef(mixl_Time)['sd.Acces']
df_uncond <- data.frame(x = seq(from = -3,to = 5,by = 0.005)) %>%
  mutate(normal = dnorm(x,mean = mu,sd = sigma))
bn_Acces <- effect.gmnl(mixl_Time,par = "Acces",effect = "ce")
df_cond <- data.frame(bn_Acces = bn_Acces$mean)

ggplot() +
  geom_area(data = df_uncond, aes(x = x, y = normal),
            fill = "grey", alpha = 0.5) +
  geom_density(data = df_cond, aes(x = bn_Acces),
               fill = "orange", colour = NA, alpha = 0.5) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + 
  theme_modern() + ylab("f(x)") + xlab(expression(beta[n]["Nature use"]))

# for biodiversity

mu <- coef(mixl_Time)['Biodiversity']
sigma <- coef(mixl_Time)['sd.Biodiversity']
df_uncond <- data.frame(x = seq(from = -3,to = 3,by = 0.005)) %>%
  mutate(normal = dnorm(x,mean = mu,sd = sigma))
bn_Biodiversity <- effect.gmnl(mixl_Time,par = "Biodiversity",effect = "ce")
df_cond <- data.frame(bn_Biodiversity = bn_Biodiversity$mean)

ggplot() +
  geom_area(data = df_uncond, aes(x = x, y = normal),
            fill = "grey", alpha = 0.5) +
  geom_density(data = df_cond, aes(x = bn_Biodiversity),
               fill = "orange", colour = NA, alpha = 0.5) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + 
  theme_modern() + ylab("f(x)") + xlab(expression(beta[n][Biodiversity]))

# for ASC

mu <- coef(mixl_Time)['asc']
sigma <- coef(mixl_Time)['sd.asc']
df_uncond <- data.frame(x = seq(from = -10,to = 10,by = 0.005)) %>%
  mutate(normal = dnorm(x,mean = mu,sd = sigma))
bn_asc <- effect.gmnl(mixl_Time,par = "asc",effect = "ce")
df_cond <- data.frame(bn_asc = bn_asc$mean)

ggplot() +
  geom_area(data = df_uncond, aes(x = x, y = normal),
            fill = "grey", alpha = 0.5) +
  geom_density(data = df_cond, aes(x = bn_asc),
               fill = "orange", colour = NA, alpha = 0.5) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + 
  theme_modern() + ylab("f(x)") + xlab(expression(beta[n][ASC]))

```

#### 1.5 WTSAT

```{r}
wtp_model <- wtp.gmnl(mixl_Time, wrt = "Time")

wtp_landscape <- -wtp_model[1,1]
sd_wtp_landscape <- wtp_model[1,2]
wtp_natureuse <- -wtp_model[2,1]
sd_wtp_natureuse <- wtp_model[2,2]
wtp_biodiversity <- -wtp_model[3,1]
sd_wtp_biodiversity <- wtp_model[3,2]
wtp_biome1 <- -wtp_model[4,1]
sd_wtp_biome1 <- wtp_model[4,2]
wtp_biome2 <- -wtp_model[5,1]
sd_wtp_biome2 <- wtp_model[5,2]

```


## Naturalness and tramways

### 2. Naturalness

```{r}

# load naturalness data from https://uicn-ressources.fr/CartNat/

# naturalness

nat <- rast(raster("Layer4_FINAL.tif"))

# biophysic integrity

nat1 <- rast(raster("Layer1_FINAL.tif"))

# process spontaneity

nat2 <- rast(raster("Layer2_FINAL.tif"))

# spatio-temporal continuity

nat3 <- rast(raster("Layer3_FINAL.tif"))

```

### 3. Tramways

#### 3.1 Tramway projects

```{r}

# Load tramways for projects

tram_trace1 <- read_sf("tram_trace.kml",c("AA_Projets"))
tram_trace2 <- read_sf("tram_trace.kml",c("ANGERS_ligne_B"))
tram_trace3 <- read_sf("tram_trace.kml",c("AUBAGNE_Val'Tram_sept_2025"))
tram_trace4 <- read_sf("tram_trace.kml",c("BORDEAUX_A_ext_Merignac_dec_2021"))
tram_trace5 <- read_sf("tram_trace.kml",c("MARSEILLE_T3_ext_nord_sud_fin_2025"))
tram_trace6 <- read_sf("tram_trace.kml",c("MONTPELLIER_L5"))

# remove project in Paris and the initial project for L5 in Montpellier

tram_trace1 <- tram_trace1[grep("PARIS",tram_trace1$Name,invert = TRUE),]
tram_trace6 <- tram_trace6[grep("initial",tram_trace6$Name,invert = TRUE),]

# combine all

tram_trace <- rbind(tram_trace1,tram_trace2,tram_trace3,
                    tram_trace4,tram_trace5,tram_trace6)
mapview(tram_trace)

```

#### 3.2 Naturalness of tramway projects

```{r}

tram_trace <- st_transform(tram_trace, crs(nat))
tram_trace_buff <- st_buffer(tram_trace,6.5)

nat_tram_mean <- exact_extract(nat,tram_trace_buff,"mean")
nat_tram_all <- exact_extract(nat,tram_trace_buff)
nat1_tram_mean <- exact_extract(nat1,tram_trace_buff,"mean")
nat1_tram_all <- exact_extract(nat1,tram_trace_buff)
nat2_tram_mean <- exact_extract(nat2,tram_trace_buff,"mean")
nat2_tram_all <- exact_extract(nat2,tram_trace_buff)
nat3_tram_mean <- exact_extract(nat3,tram_trace_buff,"mean")
nat3_tram_all <- exact_extract(nat3,tram_trace_buff)


nat_tram_all_plot <- nat1_tram_all_plot <- nat2_tram_all_plot <- nat3_tram_all_plot <- list()
for(i in 1:length(nat_tram_all)){
  nat_tram_all_plot[[i]] <- data.frame(nat_tram_all[[i]] %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat1_tram_all_plot[[i]] <- data.frame(nat1_tram_all[[i]] %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat2_tram_all_plot[[i]] <- data.frame(nat2_tram_all[[i]] %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat3_tram_all_plot[[i]] <- data.frame(nat3_tram_all[[i]] %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
}

tram_trace_buff_vect <- vect(tram_trace_buff)
nat_masked <- list()
for(i in 1:nrow(tram_trace_buff_vect)){
  nat_crop <- crop(nat,tram_trace_buff_vect[i,])
  nat_masked[i] <- mask(nat_crop, tram_trace_buff_vect[i,])
}

nat_masked_df <- list()
for(i in 1:length(nat_masked)){
  nat_masked_spdf <- as(raster(nat_masked[[i]]), "SpatialPixelsDataFrame")
  nat_masked_df_tempo <- as.data.frame(nat_masked_spdf)
  colnames(nat_masked_df_tempo) <- c("value", "x", "y")
  nat_masked_df[[i]] <- nat_masked_df_tempo
}

```

#### 3.3 Existing tramways

```{r}

tram_extant1 <- read_sf("tram_trace.kml",c("Angers"))
tram_extant2 <- read_sf("tram_trace.kml",c("Aubagne"))
tram_extant3a <- read_sf("tram_trace.kml",c("A_La Gardette_Floirac Dravemont/Le Haillan Rostand")) #Bordeaux
tram_extant3b <- read_sf("tram_trace.kml",c("B_Pessac France Alouette/Berges de la Garonne"))
tram_extant3c <- read_sf("tram_trace.kml",c("C_Parc des Expositions_Gare de Blanquefort/Villenave Pyrénées"))
tram_extant3d <- read_sf("tram_trace.kml",c("D_Carle Vernet/Cantinolles"))
tram_extant4 <- read_sf("tram_trace.kml",c("Brest_Tracés")) 
tram_extant5 <- read_sf("tram_trace.kml",c("Caen_Tracés")) 
tram_extant6a <- read_sf("tram_trace.kml",c("T1_IUT Feyssine/Debourg")) # Lyon
tram_extant6b <- read_sf("tram_trace.kml",c("T2_Saint-Priest Bel Air/Perrache"))
tram_extant6c <- read_sf("tram_trace.kml",c("T3_Meyzieu Panettes/Gare Part-Dieu Villette"))
tram_extant6d <- read_sf("tram_trace.kml",c("T4_Hôpital Feyzin/La Doua Gaston Berger"))
tram_extant6e <- read_sf("tram_trace.kml",c("Lyon_Tracés"))
tram_extant7a <- read_sf("tram_trace.kml",c("T1_Noailles/Les Caillols")) # Marseille
tram_extant7b <- read_sf("tram_trace.kml",c("T2"))
tram_extant7c <- read_sf("tram_trace.kml",c("Marseille_Tracés"))
tram_extant8a <- read_sf("tram_trace.kml",c("Ligne 1_Mosson/Odysseum")) # Montpellier
tram_extant8b <- read_sf("tram_trace.kml",c("Ligne 2_St Jean de Védas Centre/Jacou"))
tram_extant8c <- read_sf("tram_trace.kml",c("Ligne 3_Juvignac/Perol Etang de l'Or"))
tram_extant8d <- read_sf("tram_trace.kml",c("Montpellier_Tracés"))
tram_extant9a <- read_sf("tram_trace.kml",c("L1_François Miterrand/Beaujoire ou Ranzay")) # Nantes
tram_extant9b <- read_sf("tram_trace.kml",c("L2_Gare de Pont Rousseau/Orvault Grand Val"))
tram_extant9c <- read_sf("tram_trace.kml",c("L3_Neustrie/Marcel Paul"))
tram_extant10a <- read_sf("tram_trace.kml",c("A_Parc des Sports/Graffenstaden")) # Strasbourg
tram_extant10b <- read_sf("tram_trace.kml",c("B_ Lingolsheim Tiergaertel/Hoenheim Gare"))
tram_extant10c <- read_sf("tram_trace.kml",c("C_Gare Centrale/Neuhof Rodolphe Reuss"))
tram_extant10d <- read_sf("tram_trace.kml",c("D_Poteries/Kehl Rathaus"))
tram_extant10e <- read_sf("tram_trace.kml",c("E_Campus d'Illkirch/Robertsau L'escale"))
tram_extant10f <- read_sf("tram_trace.kml",c("F_Elsau/Place d'Islande"))
tram_extant11 <- read_sf("tram_trace.kml",c("Tours_Tracés"))

# combine all

tram_extant <- rbind(tram_extant1,tram_extant2,tram_extant3a,tram_extant3b,tram_extant3c,tram_extant3d,
                    tram_extant4,tram_extant5,tram_extant6a,tram_extant6b,tram_extant6c,tram_extant6d,tram_extant6e,
                    tram_extant7a,tram_extant7b,tram_extant7c,tram_extant8a,tram_extant8b,tram_extant8c,tram_extant8d,tram_extant9a,tram_extant9b,tram_extant9c,
                    tram_extant10a,tram_extant10b,tram_extant10c,tram_extant10d,tram_extant10e,tram_extant10f,tram_extant11)
                    
mapview(tram_extant)

# remove part of Strasbourg tram in Germany

tram_extant <- tram_extant[c(1:106,108:114),] 

```

#### 3.4 Naturalness of existing tramways

```{r}

# link with the city center

ville_tram_extant <- c(rep("Angers",nrow(tram_extant1)),rep("Aubagne",nrow(tram_extant2)),
                       rep("Bordeaux",nrow(tram_extant3a)),rep("Bordeaux",nrow(tram_extant3b)),rep("Bordeaux",nrow(tram_extant3c)),rep("Bordeaux",nrow(tram_extant3d)),
                       rep("Brest",nrow(tram_extant4)),rep("Caen",nrow(tram_extant5)),
                       rep("Lyon",nrow(tram_extant6a)),rep("Lyon",nrow(tram_extant6b)),rep("Lyon",nrow(tram_extant6c)),rep("Lyon",nrow(tram_extant6d)),rep("Lyon",nrow(tram_extant6e)),
                       rep("Marseille",nrow(tram_extant7a)),rep("Marseille",nrow(tram_extant7b)),rep("Marseille",nrow(tram_extant7c)),
                       rep("Montpellier",nrow(tram_extant8a)),rep("Montpellier",nrow(tram_extant8b)),rep("Montpellier",nrow(tram_extant8c)),rep("Montpellier",nrow(tram_extant8d)),
                       rep("Nantes",nrow(tram_extant9a)),rep("Nantes",nrow(tram_extant9b)),rep("Nantes",nrow(tram_extant9c)),
                       rep("Strasbourg",nrow(tram_extant10a)),rep("Strasbourg",nrow(tram_extant10b)),rep("Strasbourg",nrow(tram_extant10c)),rep("Strasbourg",nrow(tram_extant10d)),rep("Strasbourg",(nrow(tram_extant10e)-1)),rep("Strasbourg",nrow(tram_extant10f)),
                       rep("Tours",nrow(tram_extant11)))

# exctract naturalness

tram_extant <- st_transform(tram_extant, crs(nat))
tram_extant_buff <- st_buffer(tram_extant,6.5)

nat_tram_extant_mean <- exact_extract(nat,tram_extant_buff,"mean")
nat_tram_extant_all <- exact_extract(nat,tram_extant_buff)
nat1_tram_extant_mean <- exact_extract(nat1,tram_extant_buff,"mean")
nat1_tram_extant_all <- exact_extract(nat1,tram_extant_buff)
nat2_tram_extant_mean <- exact_extract(nat2,tram_extant_buff,"mean")
nat2_tram_extant_all <- exact_extract(nat2,tram_extant_buff)
nat3_tram_extant_mean <- exact_extract(nat3,tram_extant_buff,"mean")
nat3_tram_extant_all <- exact_extract(nat3,tram_extant_buff)

nat_tram_extant_all_plot <- nat1_tram_extant_all_plot <- nat2_tram_extant_all_plot <- nat3_tram_extant_all_plot <- list()
for(i in 1:length(nat_tram_extant_all)){
  nat_tram_extant_all_plot[[i]] <- data.frame(nat_tram_extant_all[[i]] %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat1_tram_extant_all_plot[[i]] <- data.frame(nat1_tram_extant_all[[i]] %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat2_tram_extant_all_plot[[i]] <- data.frame(nat2_tram_extant_all[[i]] %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat3_tram_extant_all_plot[[i]] <- data.frame(nat3_tram_extant_all[[i]] %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
}

tram_extant_buff_vect <- vect(tram_extant_buff)
nat_masked_extant <- list()
for(i in 1:nrow(tram_extant_buff_vect)){
  nat_crop <- crop(nat,tram_extant_buff_vect[i,])
  nat_masked_extant[i] <- mask(nat_crop, tram_extant_buff_vect[i,])
}

nat_masked_extant_df <- list()
for(i in 1:length(nat_masked_extant)){
  nat_masked_extant_spdf <- as(raster(nat_masked_extant[[i]]), "SpatialPixelsDataFrame")
  nat_masked_extant_df_tempo <- as.data.frame(nat_masked_extant_spdf)
  colnames(nat_masked_extant_df_tempo) <- c("value", "x", "y")
  nat_masked_extant_df[[i]] <- nat_masked_extant_df_tempo
}

```

#### 3.5 Naturalness of existing vs projects

```{r}

# for the 13m buffer

project_trace_13m <- do.call(rbind.data.frame, nat_tram_all_plot)
extant_trace_13m <- do.call(rbind.data.frame, nat_tram_extant_all_plot)
plot_trace_13m <- rbind(data.frame(project_trace_13m,variable="Tram projects"),data.frame(extant_trace_13m,variable="Tram extant"))

ggplot(plot_trace_13m, aes(x = variable, y = value, weight = cover, fill=variable)) + 
  geom_boxplot(width=0.6, col="#0219f3") + 
  scale_fill_manual(name="alpha", values=alpha(c("#0219f3","#8d98ff"),0.5)) + theme_modern() +
  labs(y="Naturalness") + theme(axis.title.x = element_blank(),
                                legend.position = "none")

project_trace_13m_nat1 <- do.call(rbind.data.frame, nat1_tram_all_plot)
extant_trace_13m_nat1 <- do.call(rbind.data.frame, nat1_tram_extant_all_plot)
plot_trace_13m_nat1 <- rbind(data.frame(project_trace_13m_nat1,variable="Tram projects"),data.frame(extant_trace_13m_nat1,variable="Tram extant"))

ggplot(plot_trace_13m_nat1, aes(x = variable, y = value, weight = cover, fill=variable)) + 
  geom_boxplot(width=0.6, col="#0219f3") + 
  scale_fill_manual(name="alpha", values=alpha(c("#0219f3","#8d98ff"),0.5)) + theme_modern() +
  labs(y="Integrity") + theme(axis.title.x = element_blank(),
                                legend.position = "none")

project_trace_13m_nat2 <- do.call(rbind.data.frame, nat2_tram_all_plot)
extant_trace_13m_nat2 <- do.call(rbind.data.frame, nat2_tram_extant_all_plot)
plot_trace_13m_nat2 <- rbind(data.frame(project_trace_13m_nat2,variable="Tram projects"),data.frame(extant_trace_13m_nat2,variable="Tram extant"))

ggplot(plot_trace_13m_nat2, aes(x = variable, y = value, weight = cover, fill=variable)) + 
  geom_boxplot(width=0.6, col="#0219f3") + 
  scale_fill_manual(name="alpha", values=alpha(c("#0219f3","#8d98ff"),0.5)) + theme_modern() +
  labs(y="Spontaneity") + theme(axis.title.x = element_blank(),
                                legend.position = "none")

project_trace_13m_nat3 <- do.call(rbind.data.frame, nat3_tram_all_plot)
extant_trace_13m_nat3 <- do.call(rbind.data.frame, nat3_tram_extant_all_plot)
plot_trace_13m_nat3 <- rbind(data.frame(project_trace_13m_nat3,variable="Tram projects"),data.frame(extant_trace_13m_nat3,variable="Tram extant"))

ggplot(plot_trace_13m_nat3, aes(x = variable, y = value, weight = cover, fill=variable)) + 
  geom_boxplot(width=0.6, col="#0219f3") + 
  scale_fill_manual(name="alpha", values=alpha(c("#0219f3","#8d98ff"),0.5)) + theme_modern() +
  labs(y="Continuity") + theme(axis.title.x = element_blank(),
                                legend.position = "none")

plot_trace_13m_nat1$value <- scales::rescale(plot_trace_13m_nat1$value,to=c(min(na.omit(plot_trace_13m$value)), max(na.omit(plot_trace_13m$value))))
plot_trace_13m_nat2$value <- scales::rescale(plot_trace_13m_nat2$value,to=c(min(na.omit(plot_trace_13m$value)), max(na.omit(plot_trace_13m$value))))
plot_trace_13m_nat3$value <- scales::rescale(plot_trace_13m_nat3$value,to=c(min(na.omit(plot_trace_13m$value)), max(na.omit(plot_trace_13m$value))))

plot_trace_all_nat <- rbind(data.frame(plot_trace_13m,variable2="Naturalness"),
                            data.frame(plot_trace_13m_nat1,variable2="Integrity"),
                            data.frame(plot_trace_13m_nat2,variable2="Spontaneity"),
                            data.frame(plot_trace_13m_nat3,variable2="Continuity"))
plot_trace_all_nat$variable2 <- factor(plot_trace_all_nat$variable2, levels = c("Naturalness","Integrity","Spontaneity","Continuity"))

ggplot(plot_trace_all_nat, aes(x = variable2, y = value)) + 
  geom_point(position = position_jitterdodge(jitter.width=0.15,dodge.width = 0.6), 
             alpha = 0.2, size = 3, stroke = 0, na.rm = TRUE, aes(col=variable)) +
  geom_violin(width = 0.6, alpha = 0.1, na.rm = TRUE, aes(fill = variable, weight = cover)) +
  geom_boxplot(width = 0.6, alpha = 0.1, na.rm = TRUE,outlier.shape = NA,aes(fill = variable,weight = cover)) + 
  geom_signif(stat="identity",
              data=data.frame(x=c(0.875, 1.875, 2.875, 3.875), xend=c(1.125, 2.125, 3.125, 4.125),
                              y=c(470, 470,470,470), annotation=c("***", "NS"," *** ","  ***  ")),
              aes(x=x,xend=xend, y=y, yend=y, annotation=annotation)) +
  scale_fill_manual(values = c("Tram projects" = "#ffaf3c","Tram extant" = "#ff3c3c")) +
  scale_color_manual(values = c("Tram projects" = "#ffaf3c","Tram extant" = "#ff3c3c")) +
  theme_ggstatsplot() +
  labs(y="Naturalness scale") + theme(axis.title.x = element_blank(),
                               legend.position = "none")



summary(lm(value~variable,plot_trace_13m, weights = cover))
summary(lm(value~variable,plot_trace_13m_nat1, weights = cover))
summary(lm(value~variable,plot_trace_13m_nat2, weights = cover))
summary(lm(value~variable,plot_trace_13m_nat3, weights = cover))

# the same for other buffer values

dist_effect_tram <- dist_effect_tram_nat1 <- dist_effect_tram_nat2 <- dist_effect_tram_nat3 <- data.frame(dist=NA,effect=NA, pval=NA)

for(i in seq(from=1, to=50, by=1)){
  
  tram_trace_buff_compare <- st_buffer(tram_trace,i)
  nat_tram_all_compare <- exact_extract(nat,tram_trace_buff_compare)
  nat_tram_all_compare <- do.call(rbind.data.frame, nat_tram_all_compare)
  nat_tram_all_compare_plot <- data.frame(nat_tram_all_compare %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat1_tram_all_compare <- exact_extract(nat1,tram_trace_buff_compare)
  nat1_tram_all_compare <- do.call(rbind.data.frame, nat1_tram_all_compare)
  nat1_tram_all_compare_plot <- data.frame(nat1_tram_all_compare %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat2_tram_all_compare <- exact_extract(nat2,tram_trace_buff_compare)
  nat2_tram_all_compare <- do.call(rbind.data.frame, nat2_tram_all_compare)
  nat2_tram_all_compare_plot <- data.frame(nat2_tram_all_compare %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat3_tram_all_compare <- exact_extract(nat3,tram_trace_buff_compare)
  nat3_tram_all_compare <- do.call(rbind.data.frame, nat3_tram_all_compare)
  nat3_tram_all_compare_plot <- data.frame(nat3_tram_all_compare %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  
  tram_extant_buff_compare <- st_buffer(tram_extant,i)
  nat_tram_extant_all_compare <- exact_extract(nat,tram_extant_buff_compare)
  nat_tram_extant_all_compare <- do.call(rbind.data.frame, nat_tram_extant_all_compare)
  nat_tram_extant_all_compare_plot <- data.frame(nat_tram_extant_all_compare %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat1_tram_extant_all_compare <- exact_extract(nat1,tram_extant_buff_compare)
  nat1_tram_extant_all_compare <- do.call(rbind.data.frame, nat1_tram_extant_all_compare)
  nat1_tram_extant_all_compare_plot <- data.frame(nat1_tram_extant_all_compare %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat2_tram_extant_all_compare <- exact_extract(nat2,tram_extant_buff_compare)
  nat2_tram_extant_all_compare <- do.call(rbind.data.frame, nat2_tram_extant_all_compare)
  nat2_tram_extant_all_compare_plot <- data.frame(nat2_tram_extant_all_compare %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat3_tram_extant_all_compare <- exact_extract(nat3,tram_extant_buff_compare)
  nat3_tram_extant_all_compare <- do.call(rbind.data.frame, nat3_tram_extant_all_compare)
  nat3_tram_extant_all_compare_plot <- data.frame(nat3_tram_extant_all_compare %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  
  plot_trace_compare <- rbind(data.frame(nat_tram_all_compare_plot,variable="Tram projects"),data.frame(nat_tram_extant_all_compare_plot,variable="Tram extant"))
  plot_trace_compare_nat1 <- rbind(data.frame(nat1_tram_all_compare_plot,variable="Tram projects"),data.frame(nat1_tram_extant_all_compare_plot,variable="Tram extant"))
  plot_trace_compare_nat2 <- rbind(data.frame(nat2_tram_all_compare_plot,variable="Tram projects"),data.frame(nat2_tram_extant_all_compare_plot,variable="Tram extant"))
  plot_trace_compare_nat3 <- rbind(data.frame(nat3_tram_all_compare_plot,variable="Tram projects"),data.frame(nat3_tram_extant_all_compare_plot,variable="Tram extant"))
  
  results <- summary(lm(value~variable,plot_trace_compare, weights = cover))
  results_nat1 <- summary(lm(value~variable,plot_trace_compare_nat1, weights = cover))
  results_nat2 <- summary(lm(value~variable,plot_trace_compare_nat2, weights = cover))
  results_nat3 <- summary(lm(value~variable,plot_trace_compare_nat3, weights = cover))
  
  dist_effect_tram <- rbind(dist_effect_tram,data.frame(dist=i, effect=results$coef[2,1]/results$coef[1,1], pval=results$coef[2,4]))
  dist_effect_tram_nat1 <- rbind(dist_effect_tram_nat1,data.frame(dist=i, effect=results_nat1$coef[2,1]/results_nat1$coef[1,1], pval=results_nat1$coef[2,4]))
  dist_effect_tram_nat2 <- rbind(dist_effect_tram_nat2,data.frame(dist=i, effect=results_nat2$coef[2,1]/results_nat2$coef[1,1], pval=results_nat2$coef[2,4]))
  dist_effect_tram_nat3 <- rbind(dist_effect_tram_nat3,data.frame(dist=i, effect=results_nat3$coef[2,1]/results_nat3$coef[1,1], pval=results_nat3$coef[2,4]))
  
}


dist_effect_tram_plot <- cbind(dist_effect_tram,dist_effect_tram_nat1[,c("effect","pval")],
                               dist_effect_tram_nat2[,c("effect","pval")],dist_effect_tram_nat3[,c("effect","pval")])

names(dist_effect_tram_plot) <- c("Buffer","Naturalness","nat_pval","Integrity","int_pval",
                                  "Spontaneity","spo_pval","Continuity","con_pval")

dist_effect_tram_plot <- na.omit(dist_effect_tram_plot)
dist_effect_tram_plot$Buffer <- dist_effect_tram_plot$Buffer*2
dist_effect_tram_plot$nat_sig <- ifelse(dist_effect_tram_plot$nat_pval<0.05,1,0.2)
dist_effect_tram_plot$int_sig <- ifelse(dist_effect_tram_plot$int_pval<0.05,1,0.2)
dist_effect_tram_plot$spo_sig <- ifelse(dist_effect_tram_plot$spo_pval<0.05,1,0.2)
dist_effect_tram_plot$con_sig <- ifelse(dist_effect_tram_plot$con_pval<0.05,0.5,0.2)

ggplot(dist_effect_tram_plot, aes(x=Buffer)) +
  geom_point(aes(y=Naturalness, alpha=nat_sig), shape=19) +
  geom_point(aes(y=Integrity/2, alpha=int_sig), shape=0) +
  geom_point(aes(y=Spontaneity, alpha=spo_sig), shape=1) +
  geom_point(aes(y=Continuity, alpha=con_sig), shape=2) +
  scale_alpha(range = c(0.2, 1)) +
  scale_y_continuous(
    name = "Difference in Naturalness \u25CF, Spontaneity \u25CB, Continuity \u25B3 (%)",
    sec.axis = sec_axis( trans=~.*2, name="Difference in Integrity \u25A1 (%)")
  ) +  theme_modern() + theme(legend.position = "none")

ggsave("output/tram_nat_extant_project_buffer.png",
       width = 7,
       height = 6.5,
       dpi = 400)
       
```



### 4. Tree effect on natrualness (Montpellier ligne 1, most frequented ligne in France)

```{r}

ex_tram_arbre0 <- read_sf("traces_TRAMWAY_2021_tree.kml",c("Montpellier_pas_arbre"))
ex_tram_arbre2 <- read_sf("traces_TRAMWAY_2021_tree.kml",c("Montpellier_arbre_deux"))

ex_tram_arbre0 <- st_transform(ex_tram_arbre0, crs(nat))
ex_tram_arbre0_buff <- st_buffer(ex_tram_arbre0,16) # see buffer size effet below
nat_ex_tram_arbre0 <- exact_extract(nat,ex_tram_arbre0_buff)
nat_ex_tram_arbre0_all <- do.call(rbind.data.frame, nat_ex_tram_arbre0)
nat_ex_tram_arbre0_plot <- data.frame(nat_ex_tram_arbre0_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
nat1_ex_tram_arbre0 <- exact_extract(nat1,ex_tram_arbre0_buff)
nat1_ex_tram_arbre0_all <- do.call(rbind.data.frame, nat1_ex_tram_arbre0)
nat1_ex_tram_arbre0_plot <- data.frame(nat1_ex_tram_arbre0_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
nat2_ex_tram_arbre0 <- exact_extract(nat2,ex_tram_arbre0_buff)
nat2_ex_tram_arbre0_all <- do.call(rbind.data.frame, nat2_ex_tram_arbre0)
nat2_ex_tram_arbre0_plot <- data.frame(nat2_ex_tram_arbre0_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
nat3_ex_tram_arbre0 <- exact_extract(nat3,ex_tram_arbre0_buff)
nat3_ex_tram_arbre0_all <- do.call(rbind.data.frame, nat3_ex_tram_arbre0)
nat3_ex_tram_arbre0_plot <- data.frame(nat3_ex_tram_arbre0_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))

ex_tram_arbre2 <- st_transform(ex_tram_arbre2, crs(nat))
ex_tram_arbre2_buff <- st_buffer(ex_tram_arbre2,16)
nat_ex_tram_arbre2 <- exact_extract(nat,ex_tram_arbre2_buff)
nat_ex_tram_arbre2_all <- do.call(rbind.data.frame, nat_ex_tram_arbre2)
nat_ex_tram_arbre2_plot <- data.frame(nat_ex_tram_arbre2_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
nat1_ex_tram_arbre2 <- exact_extract(nat1,ex_tram_arbre2_buff)
nat1_ex_tram_arbre2_all <- do.call(rbind.data.frame, nat1_ex_tram_arbre2)
nat1_ex_tram_arbre2_plot <- data.frame(nat1_ex_tram_arbre2_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
nat2_ex_tram_arbre2 <- exact_extract(nat2,ex_tram_arbre2_buff)
nat2_ex_tram_arbre2_all <- do.call(rbind.data.frame, nat2_ex_tram_arbre2)
nat2_ex_tram_arbre2_plot <- data.frame(nat2_ex_tram_arbre2_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
nat3_ex_tram_arbre2 <- exact_extract(nat3,ex_tram_arbre2_buff)
nat3_ex_tram_arbre2_all <- do.call(rbind.data.frame, nat3_ex_tram_arbre2)
nat3_ex_tram_arbre2_plot <- data.frame(nat3_ex_tram_arbre2_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))

plot_ex_tram_arbre <- rbind(data.frame(nat_ex_tram_arbre0_plot,variable="no_tree"),
                            data.frame(nat_ex_tram_arbre2_plot,variable="trees"))

ggplot(plot_ex_tram_arbre, aes(x = variable, y = value, weight = cover)) + 
  geom_boxplot(width=0.6,  colour = I("#3366FF")) + theme_modern() +
  labs(y="Naturalness") + theme(axis.title.x = element_blank())

summary(lm(value~variable,plot_ex_tram_arbre, weights = cover))

plot_ex_tram_arbre_nat1 <- rbind(data.frame(nat1_ex_tram_arbre0_plot,variable="no_tree"),
                            data.frame(nat1_ex_tram_arbre2_plot,variable="trees"))

ggplot(plot_ex_tram_arbre_nat1, aes(x = variable, y = value, weight = cover)) + 
  geom_boxplot(width=0.6,  colour = I("#3366FF")) + theme_modern() +
  labs(y="Integrity") + theme(axis.title.x = element_blank())

summary(lm(value~variable,plot_ex_tram_arbre_nat1, weights = cover))

plot_ex_tram_arbre_nat2 <- rbind(data.frame(nat2_ex_tram_arbre0_plot,variable="no_tree"),
                                 data.frame(nat2_ex_tram_arbre2_plot,variable="trees"))

ggplot(plot_ex_tram_arbre_nat2, aes(x = variable, y = value, weight = cover)) + 
  geom_boxplot(width=0.6,  colour = I("#3366FF")) + theme_modern() +
  labs(y="Spontaneity") + theme(axis.title.x = element_blank())

summary(lm(value~variable,plot_ex_tram_arbre_nat2, weights = cover))

plot_ex_tram_arbre_nat3 <- rbind(data.frame(nat3_ex_tram_arbre0_plot,variable="no_tree"),
                                 data.frame(nat3_ex_tram_arbre2_plot,variable="trees"))

ggplot(plot_ex_tram_arbre_nat3, aes(x = variable, y = value, weight = cover)) + 
  geom_boxplot(width=0.6,  colour = I("#3366FF")) + theme_modern() +
  labs(y="Continuity") + theme(axis.title.x = element_blank())

summary(lm(value~variable,plot_ex_tram_arbre_nat3, weights = cover))


plot_ex_tram_arbre_nat1$value <- scales::rescale(plot_ex_tram_arbre_nat1$value,to=c(min(na.omit(plot_ex_tram_arbre$value)), max(na.omit(plot_ex_tram_arbre$value))))
plot_ex_tram_arbre_nat2$value <- scales::rescale(plot_ex_tram_arbre_nat2$value,to=c(min(na.omit(plot_ex_tram_arbre$value)), max(na.omit(plot_ex_tram_arbre$value))))
plot_ex_tram_arbre_nat3$value <- scales::rescale(plot_ex_tram_arbre_nat3$value,to=c(min(na.omit(plot_ex_tram_arbre$value)), max(na.omit(plot_ex_tram_arbre$value))))

plot_ex_tram_arbre_all <- rbind(data.frame(plot_ex_tram_arbre,variable3="Naturalness"),
                            data.frame(plot_ex_tram_arbre_nat1,variable3="Integrity"),
                            data.frame(plot_ex_tram_arbre_nat2,variable3="Spontaneity"),
                            data.frame(plot_ex_tram_arbre_nat3,variable3="Continuity"))
plot_ex_tram_arbre_all$variable3 <- factor(plot_ex_tram_arbre_all$variable3, levels = c("Naturalness","Integrity","Spontaneity","Continuity"))

ggplot(plot_ex_tram_arbre_all, aes(x = variable3, y = value)) + 
  geom_point(position = position_jitterdodge(jitter.width=0.15,dodge.width = 0.6), 
             alpha = 0.2, size = 3, stroke = 0, na.rm = TRUE, aes(col=variable2)) +
  geom_violin(width = 0.6, alpha = 0.1, na.rm = TRUE, aes(fill = variable2, weight = cover)) +
  geom_boxplot(width = 0.6, alpha = 0.1, na.rm = TRUE,outlier.shape = NA,aes(fill = variable2,weight = cover)) + 
  geom_signif(stat="identity",
              data=data.frame(x=c(0.875, 1.875, 2.875, 3.875), xend=c(1.125, 2.125, 3.125, 4.125),
                              y=c(410, 410,410,410), annotation=c("*", "NS","***","  NS  ")),
              aes(x=x,xend=xend, y=y, yend=y, annotation=annotation)) +
  scale_fill_manual(values = c("trees" = "#29c200","no_tree" = "#b1b1b1")) +
  scale_color_manual(values = c("trees" = "#29c200","no_tree" = "#b1b1b1")) +
  theme_ggstatsplot() +
  labs(y="Naturalness scale") + theme(axis.title.x = element_blank(),
                                      legend.position = "none")

```


### 5. Average naturalness center and periph

#### 5.1 Naturalness data for cities

```{r}

nat_com <- read.csv("nat_com.csv") 

list_ville <- data.frame(com_centre=c(rep("Angers",5),rep("Aubagne",4),rep("Avignon",1),rep("Bordeaux",2),
                                      rep("Grenoble",2),rep("Le Havre",2),rep("Lille",15),rep("Lyon",24),
                                      rep("Montpellier",4),rep("Nantes",6),rep("Nice",5),rep("Rouen",17),
                                      rep("Strasbourg",4),rep("Toulouse",6),rep("Tours",7),rep("Geneve",1),
                                      rep("Caen",2),rep("Paris",30)),
                         com_peri=c(
                           "Avrillé",  "Beaucouzé",  "Les Ponts-de-Cé",  "Saint-Barthélemy-d'Anjou",
                           "Verrières-en-Anjou","Auriol","La Bouilladisse","La Destrousse",
                           "Roquevaire","Le Pontet","Eysines","Villenave-d'Ornon",
                           "La Tronche",  "Saint-Martin-d'Hères","Harfleur","Montivilliers",
                           "Faches-Thumesnil","Haubourdin","Hem","La Madeleine",
                           "Loos","Marquette-lez-Lille","Neuville-en-Ferrain","Roubaix",
                           "Saint-André-lez-Lille","Seclin","Templemars","Tourcoing",
                           "Wambrechies","Wattignies","Wattrelos","Bron",
                           "Caluire-et-Cuire","Champagne-au-Mont-d'Or",  "Chaponost",  "Charbonnières-les-Bains",
                           "Chassieu",  "Dardilly",  "Décines-Charpieu",  "Écully",
                           "Francheville",  "Genas",  "La Mulatière",  "Meyzieu",
                           "Oullins",  "Pierre-Bénite",  "Rillieux-la-Pape",  "Saint-Fons",
                           "Saint-Genis-Laval",  "Saint-Priest",  "Sainte-Foy-lès-Lyon",  "Tassin-la-Demi-Lune",
                           "Vaulx-en-Velin",  "Vénissieux",  "Villeurbanne",  "Clapiers",
                           "Lavérune",  "Montferrier-sur-Lez",  "Saint-Jean-de-Védas",  "Bouguenais",
                           "Carquefou",  "La Chapelle-sur-Erdre",  "Orvault",  "Rezé",
                           "Saint-Herblain",  "Cagnes-sur-Mer",  "Drap",  "La Trinité",
                           "Saint-André-de-la-Roche",  "Saint-Laurent-du-Var", "Barentin",  "Grand-Couronne",
                           "La Londe",  "Le Grand-Quevilly",  "Le Petit-Quevilly",  "Orival",
                           "Petit-Couronne",  "Saint-Aubin-lès-Elbeuf",  "Sotteville-lès-Rouen",  "Tourville-la-Rivière",
                           "Déville-lès-Rouen",  "Malaunay",  "Maromme",  "Notre-Dame-de-Bondeville",
                           "Elbeuf",  "Pavilly",  "Caudebec-lès-Elbeuf",  "Bischheim",
                           "Eckbolsheim",  "Schiltigheim",  "Wolfisheim",  "Aussonne",
                           "Auzeville-Tolosane",  "Beauzelle",  "Blagnac",  "Colomiers",
                           "Cornebarrieu",  "Chambray-lès-Tours",  "Fondettes",  "Joué-lès-Tours",
                           "La Riche",  "Saint-Avertin",  "Saint-Cyr-sur-Loire",  "Saint-Pierre-des-Corps",
                           "Ferney-Voltaire",  "Saint-Contest",  "Bretteville-sur-Odon",  "Antony",
                           "Saint-Germain-en-Laye",  "Athis-Mons",  "Saint-Denis",  "Longjumeau",
                           "Morsang-sur-Orge",  "Montreuil",  "Achères",  "Grigny",
                           "Romainville",  "Champlan",  "Noisy-le-Sec",  "Juvisy-sur-Orge",
                           "Épinay-sur-Orge",  "Évry-Courcouronnes",  "Colombes",  "Paray-Vieille-Poste",
                           "Châtenay-Malabry",  "Savigny-sur-Orge",  "Bois-Colombes",  "Poissy",
                           "Clamart",  "Palaiseau",  "Chilly-Mazarin",  "Fontenay-sous-Bois",
                           "Ris-Orangis",  "Massy",  "Aubervilliers",  "Asnières-sur-Seine",
                           "Rosny-sous-Bois")
)

list_ville <- list_ville[which(list_ville$com_centre!="Paris"),]

fig4_data <-  rbind(data.frame(nat_com[which(nat_com$com_name %in% unique(list_ville$com_centre)),],variable="com_center"),
                    data.frame(nat_com[which(nat_com$com_name %in% unique(list_ville$com_peri)),],variable="com_peri"))

fig4_data$com_name <- fig4_data$com_code <- NULL

```

#### 5.2  Link with tramways

```{r}

# naturalness for tramways

project_tram_mean <- ldply(nat_tram_all_plot, .fun = function(x){return(weighted.mean(x$value,x$cover, na.rm=TRUE))})
extant_trace_mean <- ldply(nat_tram_extant_all_plot, .fun = function(x){return(weighted.mean(x$value,x$cover, na.rm=TRUE))})

fig4_data <- rbind(fig4_data,data.frame(nat=unlist(project_tram_mean),variable="project_tram_area"),
                   data.frame(nat=unlist(extant_trace_mean),variable="extant_tram"))

# retrieve proportion of commune area affected by tram

spdf <- geojson_read("raw_data/georef-france-commune-arrondissement-municipal-millesime.geojson",  what = "sp")

spdf <- st_as_sf(spdf)
tram_trace_buff_16 <- st_buffer(tram_trace,16)
spdf <- st_transform(spdf, crs = st_crs(tram_trace_buff_16))

tram_trace_com <- st_intersection(tram_trace_buff_16,spdf)
tram_trace_com$area <- as.numeric(st_area(tram_trace_com))
st_geometry(tram_trace_com) <- NULL
com_affected <- data.frame(tram_trace_com %>% group_by(com_name) %>% summarize(area_affected=sum(area)))

com_affected_total <- spdf[which(spdf$com_name %in% com_affected$com_name),]
com_affected_total$area <- as.numeric(st_area(com_affected_total))
st_geometry(com_affected_total) <- NULL
com_affected_total <- data.frame(com_affected_total %>% group_by(com_name) %>% summarize(area_total=sum(area)))

com_affected_prop <- merge(com_affected,com_affected_total,by="com_name")
com_affected_prop$prop <- com_affected_prop$area_affected/com_affected_prop$area_total

# naturalness of affected commune (current, after tram without tree, after tram with trees)

nat_affected_com_current <- exact_extract(nat,spdf[which(spdf$com_name %in% com_affected$com_name),])

nat_affected_com_current_mean <- ldply(nat_affected_com_current, .fun = function(x){return(weighted.mean(x$value,x$cover, na.rm=TRUE))})
nat_affected_com_current_mean$com_name <- spdf[which(spdf$com_name %in% com_affected$com_name),]$com_name
nat_affected_com_current_mean$area <- as.numeric(st_area(spdf[which(spdf$com_name %in% com_affected$com_name),]))
nat_affected_com_current_mean <- data.frame(nat_affected_com_current_mean %>% group_by(com_name) %>% summarize(com_current=weighted.mean(V1,area)))


tram_trace_com <- st_intersection(tram_trace_buff_16,spdf[which(spdf$com_name %in% com_affected$com_name),])
tram_trace_com <- tram_trace_com %>% group_by(com_name) %>% dplyr::summarise(across(geometry, ~ sf::st_combine(.)), .groups = "keep") %>% 
  dplyr::summarise(across(geometry, ~ sf::st_union(.)), .groups = "drop")
no_tram_trace_com <- st_difference(spdf[which(spdf$com_name %in% com_affected$com_name),],tram_trace_buff_16)
no_tram_trace_com <- no_tram_trace_com %>% group_by(com_name) %>% dplyr::summarise(across(geometry, ~ sf::st_combine(.)), .groups = "keep") %>% 
  dplyr::summarise(across(geometry, ~ sf::st_union(.)), .groups = "drop")

nat_affected_com_ex_tram <- exact_extract(nat,no_tram_trace_com)
nat_affected_com_in_tram <- exact_extract(nat,tram_trace_com)

nat_affected_com_ex_tram_mean <- ldply(nat_affected_com_ex_tram, .fun = function(x){return(weighted.mean(x$value,x$cover, na.rm=TRUE))})
nat_affected_com_ex_tram_mean$com_name <- no_tram_trace_com$com_name

nat_affected_com_in_tram_mean <- ldply(nat_affected_com_in_tram, .fun = function(x){return(weighted.mean(x$value,x$cover, na.rm=TRUE))})
nat_affected_com_in_tram_mean$com_name <- no_tram_trace_com$com_name
nat_affected_com_in_tram_mean$nat_post_tram <- nat_affected_com_in_tram_mean$V1 * (1-dist_effect_tram$effect[which(dist_effect_tram$dist==16)])
nat_affected_com_in_tram_mean$nat_post_tram_tree <- nat_affected_com_in_tram_mean$V1 * (1-dist_effect_tram$effect[which(dist_effect_tram$dist==16)]+dist_effect_tree$effect[which(dist_effect_tree$dist==16)])
names(nat_affected_com_in_tram_mean)[1] <- "nat_current"

nat_affected_com_post_tram_mean <- merge(nat_affected_com_ex_tram_mean,nat_affected_com_in_tram_mean,by="com_name")
nat_affected_com_post_tram_mean <- merge(nat_affected_com_post_tram_mean,com_affected_prop, by="com_name")

nat_affected_com_post_tram_mean$com_post_tram <- nat_affected_com_post_tram_mean$V1*(1-nat_affected_com_post_tram_mean$prop)+nat_affected_com_post_tram_mean$nat_post_tram*nat_affected_com_post_tram_mean$prop
nat_affected_com_post_tram_mean$com_post_tram_tree <- nat_affected_com_post_tram_mean$V1*(1-nat_affected_com_post_tram_mean$prop)+nat_affected_com_post_tram_mean$nat_post_tram_tree*nat_affected_com_post_tram_mean$prop

nat_affected_com_final <- merge(nat_affected_com_post_tram_mean,nat_affected_com_current_mean,by="com_name")

#nat_affected_com_final_long <- melt(nat_affected_com_final[,c("com_post_tram","com_post_tram_tree","com_current")])
#names(nat_affected_com_final_long)[2] <- "nat"
nat_affected_com_final_long <- melt(nat_affected_com_final[,c("com_current","nat_current","nat_post_tram","nat_post_tram_tree")])
names(nat_affected_com_final_long)[2] <- "nat"

# plot 

fig4_data <- rbind(data.frame(nat_com[which(nat_com$com_name %in% unique(list_ville$com_centre)),],variable="com_center")[,c("nat","variable")],
                   nat_affected_com_final_long)
saveRDS(fig4_data,"output/fig4_data.rds")

names(fig4_data)[1] <- "Naturalness"
fig4_data$variable[which(fig4_data$variable=="com_center")] <- "Center"
fig4_data$variable[which(fig4_data$variable=="com_current")] <- "Peripheric"
fig4_data$variable[which(fig4_data$variable=="nat_current")] <- "Tram project spatial extent (current)"
fig4_data$variable[which(fig4_data$variable=="nat_post_tram")] <- "Tram project spatial extent (expected)"
fig4_data$variable[which(fig4_data$variable=="nat_post_tram_tree")] <- "Tram project spatial extent (trees)"

ggplot(fig4_data) +
  geom_density_ridges_gradient(aes(x = Naturalness, y = variable, fill = ..x..),scale = 1.5, rel_min_height = 0.01) +
  scale_fill_viridis() + 
  theme_minimal() +
  geom_segment(aes(x = mean(fig4_data$Naturalness[which(fig4_data$variable=="Center")]),
                   xend = mean(fig4_data$Naturalness[which(fig4_data$variable=="Center")]),
                   y=1,yend=2.3),linetype="dashed") +
  geom_segment(aes(x = mean(fig4_data$Naturalness[which(fig4_data$variable=="Peripheric")]),
                   xend = mean(fig4_data$Naturalness[which(fig4_data$variable=="Peripheric")]),
                   y=2,yend=2.63),linetype="dashed") +
  geom_segment(aes(x = mean(fig4_data$Naturalness[which(fig4_data$variable=="Tram project spatial extent (current)")]),
                   xend = mean(fig4_data$Naturalness[which(fig4_data$variable=="Tram project spatial extent (current)")]),
                   y=3,yend=3.65),linetype="dashed") +
  geom_segment(aes(x = mean(fig4_data$Naturalness[which(fig4_data$variable=="Tram project spatial extent (expected)")]),
                   xend = mean(fig4_data$Naturalness[which(fig4_data$variable=="Tram project spatial extent (expected)")]),
                   y=4,yend=4.8),linetype="dashed") +
  geom_segment(aes(x = mean(fig4_data$Naturalness[which(fig4_data$variable=="Tram project spatial extent (trees)")]),
                   xend = mean(fig4_data$Naturalness[which(fig4_data$variable=="Tram project spatial extent (trees)")]),
                   y=5,yend=5.7),linetype="dashed") +
  geom_segment(
    x = mean(fig4_data$Naturalness[which(fig4_data$variable=="Tram project spatial extent (current)")]), y = 4.5,
    xend = mean(fig4_data$Naturalness[which(fig4_data$variable=="Tram project spatial extent (expected)")]), yend = 4.5,
    lineend = "round", # See available arrow types in example above
    linejoin = "round",
    size = 1, 
    arrow = arrow(length = unit(0.1, "inches")),
    colour = "#ff0000" # Also accepts "red", "blue' etc
  ) +
  geom_segment(
    x = mean(fig4_data$Naturalness[which(fig4_data$variable=="Tram project spatial extent (current)")]), y = 5.5,
    xend = mean(fig4_data$Naturalness[which(fig4_data$variable=="Tram project spatial extent (trees)")]), yend = 5.5,
    lineend = "round", # See available arrow types in example above
    linejoin = "round",
    size = 1, 
    arrow = arrow(length = unit(0.1, "inches")),
    colour = "#ffa200" # Also accepts "red", "blue' etc
  ) +
  annotate("text", x=mean(fig4_data$Naturalness[which(fig4_data$variable=="Tram project spatial extent (current)")])-
             (mean(fig4_data$Naturalness[which(fig4_data$variable=="Tram project spatial extent (current)")])-mean(fig4_data$Naturalness[which(fig4_data$variable=="Tram project spatial extent (trees)")]))/2,
           y=5.3, label= "-7.4 %", col="#ffa200") +
  annotate("text", x=mean(fig4_data$Naturalness[which(fig4_data$variable=="Tram project spatial extent (current)")])-
             (mean(fig4_data$Naturalness[which(fig4_data$variable=="Tram project spatial extent (current)")])-mean(fig4_data$Naturalness[which(fig4_data$variable=="Tram project spatial extent (expected)")]))/2,
           y=4.3, label= "-13.9 %", col="#ff0000") +
  theme(
    legend.position="none",
    axis.title.y = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

```

#### 5.2  Check buffer choice influence

```{r}


dist_effect_tree <- dist_effect_tree_nat1 <- dist_effect_tree_nat2 <- dist_effect_tree_nat3 <- data.frame(dist=NA,effect=NA, pval=NA)
dist_effect_tree2 <- dist_effect_tree2_nat1 <- dist_effect_tree2_nat2 <- dist_effect_tree2_nat3 <- data.frame(dist=NA, effect1=NA, pval1=NA, effect2=NA, pval2=NA)

for(i in seq(from=1, to=50, by=1)){
  
  ex_tram_arbre0 <- st_transform(ex_tram_arbre0, crs(nat))
  ex_tram_arbre0_buff <- st_buffer(ex_tram_arbre0,i)
  nat_ex_tram_arbre0 <- exact_extract(nat,ex_tram_arbre0_buff)
  nat_ex_tram_arbre0_all <- do.call(rbind.data.frame, nat_ex_tram_arbre0)
  nat_ex_tram_arbre0_plot <- data.frame(nat_ex_tram_arbre0_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat1_ex_tram_arbre0 <- exact_extract(nat1,ex_tram_arbre0_buff)
  nat1_ex_tram_arbre0_all <- do.call(rbind.data.frame, nat1_ex_tram_arbre0)
  nat1_ex_tram_arbre0_plot <- data.frame(nat1_ex_tram_arbre0_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat2_ex_tram_arbre0 <- exact_extract(nat2,ex_tram_arbre0_buff)
  nat2_ex_tram_arbre0_all <- do.call(rbind.data.frame, nat2_ex_tram_arbre0)
  nat2_ex_tram_arbre0_plot <- data.frame(nat2_ex_tram_arbre0_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat3_ex_tram_arbre0 <- exact_extract(nat3,ex_tram_arbre0_buff)
  nat3_ex_tram_arbre0_all <- do.call(rbind.data.frame, nat3_ex_tram_arbre0)
  nat3_ex_tram_arbre0_plot <- data.frame(nat3_ex_tram_arbre0_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  
  ex_tram_arbre1 <- st_transform(ex_tram_arbre1, crs(nat))
  ex_tram_arbre1_buff <- st_buffer(ex_tram_arbre1,i)
  nat_ex_tram_arbre1 <- exact_extract(nat,ex_tram_arbre1_buff)
  nat_ex_tram_arbre1_all <- do.call(rbind.data.frame, nat_ex_tram_arbre1)
  nat_ex_tram_arbre1_plot <- data.frame(nat_ex_tram_arbre1_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat1_ex_tram_arbre1 <- exact_extract(nat1,ex_tram_arbre1_buff)
  nat1_ex_tram_arbre1_all <- do.call(rbind.data.frame, nat1_ex_tram_arbre1)
  nat1_ex_tram_arbre1_plot <- data.frame(nat1_ex_tram_arbre1_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat2_ex_tram_arbre1 <- exact_extract(nat2,ex_tram_arbre1_buff)
  nat2_ex_tram_arbre1_all <- do.call(rbind.data.frame, nat2_ex_tram_arbre1)
  nat2_ex_tram_arbre1_plot <- data.frame(nat2_ex_tram_arbre1_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat3_ex_tram_arbre1 <- exact_extract(nat3,ex_tram_arbre1_buff)
  nat3_ex_tram_arbre1_all <- do.call(rbind.data.frame, nat3_ex_tram_arbre1)
  nat3_ex_tram_arbre1_plot <- data.frame(nat3_ex_tram_arbre1_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  
  ex_tram_arbre2 <- st_transform(ex_tram_arbre2, crs(nat))
  ex_tram_arbre2_buff <- st_buffer(ex_tram_arbre2,i)
  nat_ex_tram_arbre2 <- exact_extract(nat,ex_tram_arbre2_buff)
  nat_ex_tram_arbre2_all <- do.call(rbind.data.frame, nat_ex_tram_arbre2)
  nat_ex_tram_arbre2_plot <- data.frame(nat_ex_tram_arbre2_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat1_ex_tram_arbre2 <- exact_extract(nat1,ex_tram_arbre2_buff)
  nat1_ex_tram_arbre2_all <- do.call(rbind.data.frame, nat1_ex_tram_arbre2)
  nat1_ex_tram_arbre2_plot <- data.frame(nat1_ex_tram_arbre2_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat2_ex_tram_arbre2 <- exact_extract(nat2,ex_tram_arbre2_buff)
  nat2_ex_tram_arbre2_all <- do.call(rbind.data.frame, nat2_ex_tram_arbre2)
  nat2_ex_tram_arbre2_plot <- data.frame(nat2_ex_tram_arbre2_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat3_ex_tram_arbre2 <- exact_extract(nat3,ex_tram_arbre2_buff)
  nat3_ex_tram_arbre2_all <- do.call(rbind.data.frame, nat3_ex_tram_arbre2)
  nat3_ex_tram_arbre2_plot <- data.frame(nat3_ex_tram_arbre2_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  
  plot_ex_tram_arbre <- rbind(data.frame(nat_ex_tram_arbre0_plot,variable="0_tree"),
                              data.frame(nat_ex_tram_arbre1_plot,variable="1_trees"),
                              data.frame(nat_ex_tram_arbre2_plot,variable="2_trees"))
  plot_ex_tram_arbre$variable2 <- ifelse(plot_ex_tram_arbre$variable=="0_tree","0_tree","1_trees")
  
  result <- summary(lm(value~variable2,plot_ex_tram_arbre, weights = cover))
  result2 <- summary(lm(value~variable,plot_ex_tram_arbre, weights = cover))
  
  dist_effect_tree <- rbind(dist_effect_tree,data.frame(dist=i, effect=result$coef[2,1]/result$coef[1,1], pval=result$coef[2,4]))
  dist_effect_tree2 <- rbind(dist_effect_tree2,data.frame(dist=i, effect1=result2$coef[2,1], pval1=result2$coef[2,4], effect2=result2$coef[3,1], pval2=result2$coef[3,4]))
  
  plot_ex_tram_arbre_nat1 <- rbind(data.frame(nat1_ex_tram_arbre0_plot,variable="0_tree"),
                              data.frame(nat1_ex_tram_arbre1_plot,variable="1_trees"),
                              data.frame(nat1_ex_tram_arbre2_plot,variable="2_trees"))
  plot_ex_tram_arbre_nat1$variable2 <- ifelse(plot_ex_tram_arbre_nat1$variable=="0_tree","0_tree","1_trees")
  
  result_nat1 <- summary(lm(value~variable2,plot_ex_tram_arbre_nat1, weights = cover))
  result2_nat1 <- summary(lm(value~variable,plot_ex_tram_arbre_nat1, weights = cover))
  
  dist_effect_tree_nat1 <- rbind(dist_effect_tree_nat1,data.frame(dist=i, effect=result_nat1$coef[2,1]/result_nat1$coef[1,1], pval=result_nat1$coef[2,4]))
  dist_effect_tree2_nat1 <- rbind(dist_effect_tree2_nat1,data.frame(dist=i, effect1=result2_nat1$coef[2,1], pval1=result2_nat1$coef[2,4], effect2=result2_nat1$coef[3,1], pval2=result2_nat1$coef[3,4]))
  
  plot_ex_tram_arbre_nat2 <- rbind(data.frame(nat2_ex_tram_arbre0_plot,variable="0_tree"),
                                   data.frame(nat2_ex_tram_arbre1_plot,variable="1_trees"),
                                   data.frame(nat2_ex_tram_arbre2_plot,variable="2_trees"))
  plot_ex_tram_arbre_nat2$variable2 <- ifelse(plot_ex_tram_arbre_nat2$variable=="0_tree","0_tree","1_trees")
  
  result_nat2 <- summary(lm(value~variable2,plot_ex_tram_arbre_nat2, weights = cover))
  result2_nat2 <- summary(lm(value~variable,plot_ex_tram_arbre_nat2, weights = cover))
  
  dist_effect_tree_nat2 <- rbind(dist_effect_tree_nat2,data.frame(dist=i, effect=result_nat2$coef[2,1]/result_nat2$coef[1,1], pval=result_nat2$coef[2,4]))
  dist_effect_tree2_nat2 <- rbind(dist_effect_tree2_nat2,data.frame(dist=i, effect1=result2_nat2$coef[2,1], pval1=result2_nat2$coef[2,4], effect2=result2_nat2$coef[3,1], pval2=result2_nat2$coef[3,4]))
  
  plot_ex_tram_arbre_nat3 <- rbind(data.frame(nat3_ex_tram_arbre0_plot,variable="0_tree"),
                                   data.frame(nat3_ex_tram_arbre1_plot,variable="1_trees"),
                                   data.frame(nat3_ex_tram_arbre2_plot,variable="2_trees"))
  plot_ex_tram_arbre_nat3$variable2 <- ifelse(plot_ex_tram_arbre_nat3$variable=="0_tree","0_tree","1_trees")
  
  result_nat3 <- summary(lm(value~variable2,plot_ex_tram_arbre_nat3, weights = cover))
  result2_nat3 <- summary(lm(value~variable,plot_ex_tram_arbre_nat3, weights = cover))
  
  dist_effect_tree_nat3 <- rbind(dist_effect_tree_nat3,data.frame(dist=i, effect=result_nat3$coef[2,1]/result_nat3$coef[1,1], pval=result_nat3$coef[2,4]))
  dist_effect_tree2_nat3 <- rbind(dist_effect_tree2_nat3,data.frame(dist=i, effect1=result2_nat3$coef[2,1], pval1=result2_nat3$coef[2,4], effect2=result2_nat3$coef[3,1], pval2=result2_nat3$coef[3,4]))
  
}


dist_effect_tree_plot <- cbind(dist_effect_tree,dist_effect_tree_nat1[,c("effect","pval")],
                               dist_effect_tree_nat2[,c("effect","pval")],dist_effect_tree_nat3[,c("effect","pval")])

names(dist_effect_tree_plot) <- c("Buffer","Naturalness","nat_pval","Integrity","int_pval",
                                  "Spontaneity","spo_pval","Continuity","con_pval")

dist_effect_tree_plot <- na.omit(dist_effect_tree_plot)
dist_effect_tree_plot$Buffer <- dist_effect_tree_plot$Buffer*2
dist_effect_tree_plot$nat_sig <- ifelse(dist_effect_tree_plot$nat_pval<0.05,1,0.2)
dist_effect_tree_plot$int_sig <- ifelse(dist_effect_tree_plot$int_pval<0.05,1,0.2)
dist_effect_tree_plot$spo_sig <- ifelse(dist_effect_tree_plot$spo_pval<0.05,1,0.2)
dist_effect_tree_plot$con_sig <- ifelse(dist_effect_tree_plot$con_pval<0.05,0.5,0.2)

ggplot(dist_effect_tree_plot, aes(x=Buffer)) +
  geom_point(aes(y=Naturalness, alpha=nat_sig), shape=19) +
  geom_point(aes(y=Integrity/4, alpha=int_sig), shape=0) +
  geom_point(aes(y=Spontaneity, alpha=spo_sig), shape=1) +
  geom_point(aes(y=Continuity, alpha=con_sig), shape=2) +
  scale_alpha(range = c(0.2, 1)) +
  scale_y_continuous(
    name = "Difference in Naturalness \u25CF, Spontaneity \u25CB, Continuity \u25B3 (%)",
    sec.axis = sec_axis( trans=~.*4, name="Difference in Integrity \u25A1 (%)")
  ) +  theme_modern() + theme(legend.position = "none")
  
```