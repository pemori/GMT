# Code for article: Contour furrowing reduces erosion and enhances soil moisture on semiarid hillslopes
# Published in Geoderma Regional
# Code author: Pedro Mondaca (pedro.mondaca@usm.cl)

library(readxl)
library(dplyr)
#library(plotly)
library(Rmisc)
library(ggplot2)
library(raster)


#Soil moisture -------

dat<-read_excel("data.xlsx", sheet="soil_moisture")

names <- c('moist','depth','year')
dat[,names] <- lapply(dat[,names] , as.numeric)
names <- c('site','plot','trt')
dat[,names] <- lapply(dat[,names] , as.factor)
#data<-dat %>% na.omit
str(dat)

dat$date <- as.Date(dat$date)
dat <- dat %>%
  mutate(days = date - date[1] + 1)
dat$days
tail(dat$days)

control<-dat %>%
  dplyr::filter(trt=="control")

veg<-dat %>%
  dplyr::filter(trt=="veg")

cf<-dat %>%
  dplyr::filter(trt=="cf")

cfveg<-dat %>%
  dplyr::filter(trt=="cfveg")

#--- Soil moisture during the studied period ----#

library(akima) 
library(fields)

#control
zxy = aggregate(control$moist, by=list(control$days, -(control$depth)), function(x) mean(x,na.rm=T))
names(zxy)=c("days", "depth", "moist")
zxy
s <- interp(zxy$days,zxy$depth,zxy$moist)
image(s, col=blues9, bigplot = NULL, zlim=c(0,40), axes=F)  
box(lwd=2)
axis(2)
axis(1, at=seq(0,420,30), labels=rownames(s))

image.plot(s,col=blues9, zlim=c(0,100),legend.only = T, legend.mar = 0)

s$z #s$z is the soil moisture (%) according the interpolation
colMeans(s$z,na.omit(TRUE)) #en el tiempo..
# 1 mm = 1 L/m^2 , considerando 80 cm de prof, tengo un volumen de estudio de 0.8 m3.
#La humedad volúmetrica está en % de vol. Entonces la multiplico por 0.8 m3
av.con<-(colMeans(s$z,na.omit(TRUE))/100) #-> Average of volumetric water in the soil profile
av.con

a.con<- av.con * 800 #Multiplicado por 800 L (0.8 m^3)
a.con # L en cada m2 de suelo

#plot(1:40, a.con, type = "b", pch = 19, 
#     col = "blue", xlab = "Studied period",ylim = c(20,80), ylab = "Subsoil water content (L/m^2)")

#veg
zxy = aggregate(veg$moist, by=list(veg$days, -(veg$depth)), function(x) mean(x,na.rm=T))
names(zxy)=c("days", "depth", "moist")
zxy
s <- interp(zxy$days,zxy$depth,zxy$moist)
image(s, col=blues9, bigplot = NULL, zlim=c(0,40), axes=F)  
box(lwd=2)
axis(2)
axis(1, at=seq(0,420,30), labels=rownames(s))

image.plot(s,col=blues9, zlim=c(0,100),legend.only = T, legend.mar = 0)

s$z #s$z is the soil moisture (%) according the interpolation
colMeans(s$z,na.omit(TRUE)) #en el tiempo..
# 1 mm = 1 L/m^2 , considerando 80 cm de prof, tengo un volumen de estudio de 0.8 m3.
#La humedad volúmetrica está en % de vol. Entonces la multiplico por 0.8 m3
av.veg<-(colMeans(s$z,na.omit(TRUE))/100) #-> Average of volumetric water in the soil profile
av.veg
a.veg<- av.veg * 800 #Multiplicado por 800 L (0.8 m^3)
a.veg # L en cada m2 de suelo

#plot(1:40, a.veg, type = "b", pch = 19, 
#     col = "blue", xlab = "Studied period",ylim = c(20,80), ylab = "Subsoil water content (L/m^2)")


#cf
zxy = aggregate(cf$moist, by=list(cf$days, -(cf$depth)), function(x) mean(x,na.rm=T))
names(zxy)=c("days", "depth", "moist")
zxy
s <- interp(zxy$days,zxy$depth,zxy$moist)
image(s, col=blues9, bigplot = NULL, zlim=c(0,40), axes=F)  
box(lwd=2)
axis(2)
axis(1, at=seq(0,420,30), labels=rownames(s))

image.plot(s,col=blues9, zlim=c(0,100),legend.only = T, legend.mar = 0)

s$z #s$z is the soil moisture (%) according the interpolation
colMeans(s$z,na.omit(TRUE)) #en el tiempo..
# 1 mm = 1 L/m^2 , considerando 80 cm de prof, tengo un volumen de estudio de 0.8 m3.
#La humedad volúmetrica está en % de vol. Entonces la multiplico por 0.8 m3
av.cf<-(colMeans(s$z,na.omit(TRUE))/100) #-> Average of volumetric water in the soil profile
av.cf
a.cf<- av.cf * 800 #Multiplicado por 800 L (0.8 m^3)
a.cf # L en cada m2 de suelo

#plot(1:40, a.cf, type = "b", pch = 19, 
#     col = "blue", xlab = "Studied period",ylim = c(20,80), ylab = "Subsoil water content (L/m^2)")


#cfveg
zxy = aggregate(cfveg$moist, by=list(cfveg$days, -(cfveg$depth)), function(x) mean(x,na.rm=T))
names(zxy)=c("days", "depth", "moist")
zxy
s <- interp(zxy$days,zxy$depth,zxy$moist)
image(s, col=blues9, bigplot = NULL, zlim=c(0,40), axes=F)  
box(lwd=2)
axis(2)
axis(1, at=seq(0,420,30), labels=rownames(s))

image.plot(s,col=blues9, zlim=c(0,100),legend.only = T, legend.mar = 0)

s$z #s$z is the soil moisture (%) according the interpolation
colMeans(s$z,na.omit(TRUE)) #en el tiempo..
# 1 mm = 1 L/m^2 , considerando 80 cm de prof, tengo un volumen de estudio de 0.8 m3.
#La humedad volúmetrica está en % de vol. Entonces la multiplico por 0.8 m3
av.cfveg<-(colMeans(s$z,na.omit(TRUE))/100) #-> Average of volumetric water in the soil profile
av.cfveg * 100
a.cfveg<- av.cfveg * 800 #Multiplicado por 800 L (0.8 m^3)
a.cfveg # L en cada m2 de suelo

#plot(1:40, a.cfveg, type = "b", pch = 19, 
#     col = "blue", xlab = "Studied period",ylim = c(20,80), ylab = "Subsoil water content (L/m^2)")

plot(1:40, av.con*100, type = "b", pch = 19, 
     col = "blue", xlab = "Studied period",ylim = c(5,15), ylab = "Subsoil water content (L/m^2)")
plot(1:40, av.cf*100, type = "b", pch = 19, 
     col = "blue", xlab = "Studied period",ylim = c(5,15), ylab = "Subsoil water content (L/m^2)")
plot(1:40, av.veg*100, type = "b", pch = 19, 
     col = "blue", xlab = "Studied period",ylim = c(5,15), ylab = "Subsoil water content (L/m^2)")
plot(1:40, av.cfveg*100, type = "b", pch = 19, 
     col = "blue", xlab = "Studied period",ylim = c(5,15), ylab = "Subsoil water content (L/m^2)")


litros <- cbind.data.frame(a.con,a.cf,a.veg,a.cfveg)
litros

library(writexl)
write_xlsx(litros, path = "litros.xlsx")

#STOCK DE AGUA EN EL TIEMPO----

#n=6, por el número de plots
control
ac.con = aggregate(control$moist, by=list(control$days, control$plot,control$trt,control$site), function(x) mean(x,na.rm=T))
ac.con <- na.omit(ac.con)
ac.con

ac.cf = aggregate(cf$moist, by=list(cf$days, cf$plot,cf$trt,cf$site), function(x) mean(x,na.rm=T))
ac.cf <- na.omit(ac.cf)
ac.cf

ac.veg = aggregate(veg$moist, by=list(veg$days, veg$plot,veg$trt,veg$site), function(x) mean(x,na.rm=T))
ac.veg <- na.omit(ac.veg)
ac.veg

ac.cfveg = aggregate(cfveg$moist, by=list(cfveg$days, cfveg$plot,cfveg$trt,cfveg$site), function(x) mean(x,na.rm=T))
ac.cfveg <- na.omit(ac.cfveg)
ac.cfveg

ac.all <- rbind(ac.con, ac.cf,ac.veg,ac.cfveg)
ac.all

#Calculate area under curve to every plot ------
library(dplyr)
library(purrr)
library(MESS)
auc_con <- ac.con %>% 
  mutate(Group.4 = Group.4 %>% as.character()) %>%
  #mutate(Group.2 = Group.2 %>% as.character()) %>% 
  split(~Group.4) %>% 
  map_dbl(~auc(.x$Group.1, .x$x, type = 'linear'))
auc_con <-as.data.frame(auc_con)
con<-auc_con$auc_con
auc_con

auc_all <- ac.all %>% 
  mutate(Group.2 = Group.2 %>% as.character()) %>%
  split(~Group.2)  %>% 
  map_dbl(~auc(.x$Group.1, .x$x, type = 'spline'))
auc_all
auc_all <-as.data.frame(auc_all)
auc_all


# auc_cf <- ac.cf %>% 
#   mutate(Group.2 = Group.2 %>% as.character()) %>% 
#   split(~Group.2) %>% 
#   map_dbl(~auc(.x$Group.1, .x$x, type = 'spline'))
# auc_cf <-as.data.frame(auc_cf)
# cf<- auc_cf$auc_cf
# 
# auc_veg <- ac.veg %>% 
#   mutate(Group.2 = Group.2 %>% as.character()) %>%  # Es necesario pasarlo a caracteres o te da otra cosa! Truco de la vida!
#   split(~Group.2) %>% 
#   map_dbl(~auc(.x$Group.1, .x$x, type = 'spline'))     # Debe llevar virguilla #.x / el punto es para que aplique a todos los vectores.
# auc_veg
# auc_veg <-as.data.frame(auc_veg)
# veg<- auc_veg$auc_veg
# 
# auc_cfveg <- ac.cfveg %>% 
#   mutate(Group.2 = Group.2 %>% as.character()) %>% 
#   split(~Group.2) %>% 
#   map_dbl(~auc(.x$Group.1, .x$x, type = 'spline'))
# auc_cfveg <-as.data.frame(auc_cfveg)
# cfveg<-auc_cfveg$auc_cfveg
# 
# list(con,cf,veg,cfveg)
# 
# auc_all<- c(con, cf, veg, cfveg)
# auc_all<-as.data.frame(auc_all)
# auc_all
#trt<-c(rep(c("con"),6), rep(c("cf"),6), rep(c("veg"),6), rep(c("cfveg"), 6))
#auc_all$trt <- factor(trt)
auc_all$auc_all <- (((auc_all$auc_all)/40)* 0.8) # divido por 40 ya que auc está represantada por 40 unidades en x. Al dividirlo por 40 obtengo el promedio (L/m2/día)
#multipicado por 0.8 xq el interpolado solo aplica al suelo superficial.
#OJO, no es el acumulado!
#AUC multiplied by 0.8 to use for ANOVA analyses #auc = L/m3/d
auc_all
#Calculate minimal stocks of water by plot
min_ac.con <- ac.con%>%
  group_by (Group.2) %>%
  slice (which.min (x))
min_ac.con

min_ac.cf <- ac.cf%>%
  group_by (Group.2) %>%
  slice (which.min (x))
min_ac.cf

min_ac.veg <- ac.veg%>%
  group_by (Group.2) %>%
  slice (which.min (x))
min_ac.veg

min_ac.cfveg <- ac.cfveg%>%
  group_by (Group.2) %>%
  slice (which.min (x))
min_ac.cfveg


min_ac<-rbind(min_ac.con, min_ac.cf,min_ac.veg,min_ac.cfveg)
min_ac$x<-((min_ac$x)/100) * 800 ##Multiplicado por 800 L (0.8 m^3)

min_ac #df with the minimal volumetric water content data to use for ANOVA analyses #x in L/m2
auc_all
library(writexl)
write_xlsx(min_ac, "min_ac.xlsx")
write_xlsx(auc_all, "auc_all.xlsx")

min_ac$Group.2<-as.character(min_ac$Group.2)
library(tidyverse)
min_ac_2<-min_ac %>% remove_rownames %>% column_to_rownames(var="Group.2")
min_ac_2
idx <- match(rownames(min_ac_2), rownames(auc_all))
idx
auc_ord<-auc_all[idx,]
auc_ord
rownames(auc_all) <- rownames(auc_ord)

min_ac_2$auc<-auc_ord
min_ac_2
data<-min_ac_2
plot(data$x, data$auc)

# ANOVA, trabajar con data (x= min moist, auc=area under curve)

#ANOVA
library(car)
library(emmeans)
library(multcomp)

#GLMER
library(lme4) #Para glmer
library(lmerTest) #random

# # Grouped data
# library(rstatix)
# min_ac %>%
#   # group_by(trt) %>%
#   games_howell_test(x ~ Group.3)

#Para min moist
glm_min<-glm(x~Group.3,family=gaussian(link = log),data=data)
glmer_min<-glmer(x~Group.3+(1|Group.4),family=gaussian(link = log),data=data)
Anova(glm_min)
Anova(glmer_min)
anova(glm_min,glmer_min)

#post hoc glmer
library(multcomp)
 summary(glht(glmer_min, mcp(Group.3="Dunnet")))

 aggregate(x ~ Group.3, data = data, 
           FUN = function(x) c(mean = mean(x), se = sd(x)))
 
#Para AUC
glm_auc<-glm(auc~Group.3,family=gaussian(link = log),data=data)
glmer_auc<-glmer(auc~Group.3+(1|Group.4),family=gaussian(link = log),data=data)
Anova(glm_auc)
Anova(glmer_auc)
anova(glm_auc,glmer_auc)

#post hoc glmer
library(multcomp)
summary(glht(glmer_auc, mcp(Group.3="Dunnet")))

aggregate(auc ~ Group.3, data = data, 
          FUN = function(x) c(mean = mean(x), se = sd(x)))

data

data_summary <- data %>% 
  group_by(Group.3) %>% 
  summarise(mean_min = mean(x),
            std_min = sd(x), mean)
data_summary

#Water runoff----------

# Libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(dplyr)

data<-read_excel("data.xlsx", sheet="accum_w")
names <- c('water_runoff')
data[,names] <- lapply(data[,names] , as.numeric)
names <- c('site','plot','trt')
data[,names] <- lapply(data[,names] , as.factor)
dat<-data[complete.cases(data),]
str(dat)

dat$date <- as.Date(dat$date)
dat <- dat %>%
  mutate(days = date - date[1] + 1)
dat$days
dat

# runoff_sum <- aggregate(x = dat[c("water_runoff")],
#                      FUN = sum,
#                      by = list(Group.date = dat$days, dat$trt,dat$plot))
# runoff_sum

dat <- dat %>% group_by(plot) %>% mutate(cum_sum = cumsum(water_runoff))
dat

library(dplyr)
library(scales)
con_ro<-dat %>%
  dplyr::filter(trt=="control")
con_ro

lp_con<-ggplot(con_ro, aes(x = days, y = cum_sum, group = plot)) + geom_point() +
        geom_line(col='red') + 
        geom_ribbon(aes(ymin = cum_sum*0.75, ymax = cum_sum*1.25), alpha = 0.1) + 
        scale_y_continuous(trans = 'log10') + 
        scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw()

lp_con          

cf_ro<-dat %>%
  dplyr::filter(trt=="cf")
cf_ro

lp_cf<-ggplot(cf_ro, aes(x = days, y = cum_sum, group = plot)) + geom_point() +
  geom_line(col='red') + 
  geom_ribbon(aes(ymin = cum_sum*0.75, ymax = cum_sum*1.25), alpha = 0.1) + 
  scale_y_continuous(trans = 'log10') + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw()
lp_cf


#veg
veg_ro<-dat %>%
  dplyr::filter(trt=="veg")
veg_ro

lp_veg<-ggplot(veg_ro, aes(x = days, y = cum_sum, group = plot)) + geom_point() +
  geom_line(col='red') + 
  geom_ribbon(aes(ymin = cum_sum*0.75, ymax = cum_sum*1.25), alpha = 0.1) + 
  scale_y_continuous(trans = 'log10') + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw()
lp_veg


#cfveg
cfveg_ro<-dat %>%
  dplyr::filter(trt=="cfveg")
cfveg_ro

lp_cfveg<-ggplot(cfveg_ro, aes(x = days, y = cum_sum, group = plot)) + geom_point() +
  geom_line(col='red') + 
  geom_ribbon(aes(ymin = cum_sum*0.75, ymax = cum_sum*1.25), alpha = 0.1) + 
  scale_y_continuous(trans = 'log10') + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw()
lp_cfveg

#printed at 980*525

#soil runoff------
library(readxl)
library(dplyr)
data<-read_excel("data.xlsx", sheet="runoff_dep")
#data<-read_excel("C:\\Users\\pedro\\OneDrive\\Escritorio\\Roxana-Pablo\\Humedad\\data_nov_mod.xlsx", sheet="runoff_dep")
names <- c('sediment')
data[,names] <- lapply(data[,names] , as.numeric)
names <- c('site','plot','trt')
data[,names] <- lapply(data[,names] , as.factor)
dat<-data[complete.cases(data),]
str(dat)

dat$date <- as.Date(dat$date)
dat <- dat %>%
  mutate(days = date - date[1] + 1)
dat$days
dat

dat$cum_sum <- ave(dat$sediment, dat$plot, FUN=cumsum)
View(dat)
dat$cum_sum <- ((dat$cum_sum/16)*10000)/10^6  #results are in g/16m2, so I divided by 16 and multiplied by 10.000 to g/ha. Then, I divided by 10^6 for pass from g to t
write_xlsx(dat, "cum_sum.xlsx")

library(scales)
library(ggplot2)
#runoff control
con_ro<-dat %>%
  dplyr::filter(trt=="control")
con_ro

lp_con<-ggplot(con_ro, aes(x = days, y = cum_sum, group = plot)) + geom_point() +
  geom_line(col='red') + 
  ylim(0,2)+
  theme_bw()
lp_con      

#cf
cf_ro<-dat %>%
  dplyr::filter(trt=="cf")
cf_ro

lp_cf<-ggplot(cf_ro, aes(x = days, y = cum_sum, group = plot)) + geom_point() +
  geom_line(col='red') + 
  #geom_ribbon(aes(ymin = cum_sum*0.75, ymax = cum_sum*1.25), alpha = 0.1) + 
  ylim(0,2)+
  theme_bw()

lp_cf

#veg
veg_ro<-dat %>%
  dplyr::filter(trt=="veg")
veg_ro

lp_veg<-ggplot(veg_ro, aes(x = days, y = cum_sum, group = plot)) + geom_point() +
  geom_line(col='red') + 
  ylim(0,2)+
  theme_bw()
lp_veg     

#cfveg
cfveg_ro<-dat %>%
  dplyr::filter(trt=="cfveg")
cfveg_ro


lp_cfveg<-ggplot(cfveg_ro, aes(x = days, y = cum_sum, group = plot)) + geom_point() +
  geom_line(col='red') + 
  ylim(0,2)+
  theme_bw()

lp_cfveg     

#Falta ANOVA de acumulado final

aggregate(dat$cum_sum, by = list(dat$plot), max)
dat_ro<- aggregate(cum_sum ~ plot + trt + site, data = dat, sum)
dat_ro

#ANOVA
library(car)
library(emmeans)
library(multcomp)

#GLMER
library(lme4) #Para glmer
library(lmerTest) #random

#Para runoff
glm_ro<-glm(cum_sum~trt,family=gaussian(link = log),data=dat_ro)
glmer_ro<-glmer(cum_sum~trt+(1|site),family=gaussian(link = log),data=dat_ro)
Anova(glm_ro)
Anova(glmer_ro)
anova(glm_ro,glmer_ro)


#post hoc glmer
library(multcomp)
summary(glht(glmer_ro, mcp(trt="Dunnet")))

aggregate(cum_sum ~ trt, data = dat_ro, 
          FUN = function(x) c(mean = mean(x), se = sd(x)))


str(data)
cumsum<-dat$cum_sum[73:96]
cumsum
data2<-as.data.frame(cbind(data,cumsum))
data2
