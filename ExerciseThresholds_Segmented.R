if (!require("ggplot2")) install.packages("ggplot2")
if (!require("ggtext")) install.packages("ggtext")
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("here")) install.packages("here")
if (!require("readxl")) install.packages("readxl")
if (!require("segmented")) install.packages("segmented")

library(ggplot2)
library(ggtext)
library(gridExtra)
library(here)
library(readxl)
library(segmented)

# assuming the data file is in the same directory as the source file
df1 <- read_excel(here("Sample_Data_1.xlsx"),sheet = "95")
# df1 <- read_excel(here("Sample_Data_2.xlsx"),sheet = "38")
df1

df1$RT <- df1$VE/df1$PETCO2
attach(df1)

#GET

gr1_get <- ggplot(df1, aes(x = VO2, y = VCO2)) + geom_point()+
  labs(title = "**GET Estimation**", 
       x ="**VO<sub>2</sub> (ml&middot;min<sup>-1</sup>)**",
       y = "**VCO<sub>2</sub> (ml&middot;min<sup>-1</sup>)**")+
  theme_minimal() +
  theme(plot.title = element_markdown(size = 14, hjust=0.5),
    axis.title.y = element_markdown(),
    axis.title.x = element_markdown())
lm1 <- lm(VCO2 ~ VO2)
gr2_get <- gr1_get + geom_abline(intercept = lm1$coefficients[1], 
                 slope = lm1$coefficients[2])

seg_get <- segmented(lm1, seg.Z = ~ VO2, npsi = 1)
summary(seg_get)
# estimation of the breakpoint
seg_get$psi
# estimation of the slopes
slope(seg_get)

fit1 <- fitted(seg_get)
model1 <- data.frame(VO2 = VO2, VCO2 = fit1)

# plot the fitted model
gr3_get <- gr2_get + geom_line(data = model1, aes(x = VO2, y = VCO2), colour = "red",linewidth=1)+
  geom_vline(xintercept = seg_get$psi[,2], linetype = "dashed",colour="red",linewidth=1)
gr3_get

#RCP
gr1_rcp <- ggplot(df1, aes(x = VCO2, y = VE)) + geom_point()+
  labs(title = "**RCP Estimation**", 
       x ="**VCO<sub>2</sub> (ml&middot;min<sup>-1</sup>)**",
       y = "**V<sub>E</sub> (L&middot;min<sup>-1</sup>)**")+
    theme_minimal() +
    theme(plot.title = element_markdown(size = 14, hjust=0.5),
          axis.title.y = element_markdown(),
          axis.title.x = element_markdown())

lm2 <- lm(VE ~ VCO2)

gr2_rcp <- gr1_rcp + geom_abline(intercept = lm2$coefficients[1], 
                         slope = lm2$coefficients[2])

seg_rcp <- segmented(lm2, seg.Z = ~ VCO2, npsi = 1)
summary(seg_rcp)
# estimation of the breakpoint
seg_rcp$psi
# estimation of the slopes
slope(seg_rcp)

fit2 <- fitted(seg_rcp)
model2 <- data.frame(VCO2 = VCO2, VE = fit2)

# plot the fitted model
gr3_rcp <- gr2_rcp + geom_line(data = model2, aes(x = VCO2, y = VE), colour = "red",linewidth=1)+
  geom_vline(xintercept = seg_rcp$psi[,2], linetype = "dashed",colour="red",linewidth=1)
gr3_rcp

#RT
gr1_rt <- ggplot(df1, aes(x = watt, y = RT)) + geom_point()+
  labs(title = "**Respiratory Thresholds Estimation**", 
       x ="**Power (watt)**",
       y = "**V<sub>E</sub> / P<sub>ET</sub>CO<sub>2</sub> (mmHg)**")+
  theme_minimal() +
  theme(plot.title = element_markdown(size = 14, hjust=0.5),
        axis.title.y = element_markdown(),
        axis.title.x = element_markdown())

lm3 <- lm(RT ~ watt)

gr2_rt <- gr1_rt + geom_abline(intercept = lm3$coefficients[1], 
                         slope = lm3$coefficients[2])

seg_rt <- segmented(lm3, seg.Z = ~ watt, npsi = 2)
summary(seg_rt)

# estimation of the breakpoints
seg_rt$psi
# estimation of the slopes
slope(seg_rt)

fit3 <- fitted(seg_rt)
model3 <- data.frame(watt = watt, VE_PETCO2 = fit3)

# plot the fitted model
gr3_rt <- gr2_rt + geom_line(data = model3, aes(x = watt, y = VE_PETCO2), colour = "red",linewidth=1)+
  geom_vline(xintercept = seg_rt$psi[,2], linetype = "dashed",colour="red",linewidth=1)
gr3_rt

# Obtaining the corresponding watt value for GET using interpolation
x_closest_get <- sapply(seg_get$psi[,2], function(bp) { 
  d <- abs(VO2 - bp)
  indd <-  which.min(d)
  e <- c(VO2[indd])
  c(index_pos=indd,closest_VO2=e)
})

watt_breakpoints_get <- approx(VO2, watt, xout=seg_get$psi[,2])$y

# Obtaining the corresponding watt value for RCP using interpolation
x_closest_rcp <- sapply(seg_rcp$psi[,2], function(bp) {
  d <- abs(VCO2 - bp)
  indd <-  which.min(d)
  e <- c(VCO2[indd])
  c(index_pos=indd,closest_VCO2=e)
})

watt_breakpoints_rcp <- approx(VCO2, watt, xout=seg_rcp$psi[,2])$y

# all breakpoints

str1 <- c("GET_VO2","RCP_VCO2","RT_watt")
str2 <- c("Threshold1","Threshold2")
m1 <- rbind(c(seg_get$psi[,2],NA),c(NA,seg_rcp$psi[,2]),seg_rt$psi[,2])
row.names(m1) <- str1
colnames(m1) <- str2

str1 <- c("GET_watt","RCP_watt","RT_watt")
str2 <- c("Threshold1","Threshold2")
m2 <- rbind(c(watt_breakpoints_get,NA),c(NA,watt_breakpoints_rcp),seg_rt$psi[,2])
row.names(m2) <- str1
colnames(m2) <- str2
m1
m2

grid.arrange(gr3_get,gr3_rcp,gr3_rt,gr3_rt,ncol=2)

detach(df1)
