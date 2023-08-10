setwd("C:\\Users\\carys\\Desktop\\Evolution Project\\Finch Data")
library(dplyr)
library(tidyverse)
library(ggplot2)
library(tigris)
library(scales)
library(lubridate)

#Rain by mm each year
Rain = read.csv("C:\\Users\\carys\\Desktop\\Evolution Project\\Finch Data\\Fig.04-12.csv")
#Broods Eggs & Fledglings with nino data
DS7 = read.csv("C:\\Users\\carys\\Desktop\\Evolution Project\\Finch Data\\Fig.05-06.csv")
#Hatchling/Fledgling data for along with env. conditions
DS8 = read.csv("C:\\Users\\carys\\Desktop\\Evolution Project\\Finch Data\\Fig.05-07.csv")
#Monthly Precipitation from Jan 1981
M_Rain = read.csv("C:\\Users\\carys\\Desktop\\Evolution Project\\Finch Data\\M_P.csv")
#Monthly Temperature from Jan 1856
M_Temp = read.csv("C:\\Users\\carys\\Desktop\\Evolution Project\\Finch Data\\M_SST.csv")

#General Circulation Model Data
#Dad included estimated hatching success using parabolic model I created earlier
GCM = read.csv("C:\\Users\\carys\\Desktop\\Evolution Project\\Finch Data\\GCM_Data_Daphne.csv")
#Projected GCM data
Projected = read.csv("C:\\Users\\carys\\Desktop\\Evolution Project\\Finch Data\\GCM-Projected.csv")
#GCM Monthly Temperature information modeled for past
Mod_Temp = read.csv("C:\\Users\\carys\\Desktop\\Evolution Project\\Finch Data\\ModeledTemp.csv")
#GCM Monthly Temperature information modeled for future
P_Temp = read.csv("C:\\Users\\carys\\Desktop\\Evolution Project\\Finch Data\\PredictedTemp.csv")
#GCM Observed Rainfall
O_Rain = read.csv("C:\\Users\\carys\\Desktop\\Evolution Project\\Finch Data\\Observed_Precipitation.csv")
#Future data modeled
Future = read.csv("C:\\Users\\carys\\Desktop\\Evolution Project\\Finch Data\\GCM_rainfall_plot.csv")

Projected <- rename(Projected, GCM.Projected.Precipitation = ï..GCM.Projected.Precipitation)
GCM <- select(GCM, GCM.Precipitation, X.mm.year.)

P_Temp <- rename(P_Temp, Year = ï..Year)
Mod_Temp <- rename(Mod_Temp, Year = ï..Year)
O_Rain <- rename(O_Rain, Year = ï..Year)

DS8 <- select(DS8, -ln..rainfall..1, -Hatching.1, -Fledging.1, -Species.1, -Year.1)
DS8 <- rename(DS8, lnrain = ln..rainfall.)
DS8$rainsquared <- DS8$lnrain ^ 2


ggplot(GCM, aes(x=GCM.Precipitation, y=X.mm.year.)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title=paste("fortis beak length by year"))
#check if X.mm.year. is observed or estimates

Years <- c(1977, 1990, 1986, 1982, 1984, 1980, 1979, 1981, 1976, 1978, 1991, 1987, 1998, 1983)
DS8 <- rename(DS8, ENSO = "Year")
DS8$Year <- Years


GCM <- rename(GCM, Year = GCM.Precipitation)
GCM <- left_join(GCM, Mod_Temp, by = "Year")


Merged <- left_join(DS8, GCM, by="Year")
model <- lm(Hatching ~ lnrain + rainsquared, data = Merged)

Merged$lnGCM <- log(Merged$X.mm.year.)
Merged$GCMsquared <- Merged$lnGCM ^ 2
model_GCM <- lm(Hatching ~ lnGCM + GCMsquared, data = Merged)

ggplot(Merged, aes(x=lnGCM, y=Hatching)) + 
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1) +
  labs(title=paste("fortis ln(rainfall) + hatching rates"))

ggplot(Merged, aes(x=Year, y=X.mm.year.)) + 
  geom_point() +
  stat_smooth(method = "lm") +
  labs(title=paste("fortis ln(rainfall) + hatching rates"))

ggplot(Merged, aes(x=lnrain, y=lnGCM)) + 
  geom_point() +
  stat_smooth(method = "lm") +
  labs(title=paste("fortis ln(rainfall) + hatching rates"))

Observed <- left_join(Mod_Temp, O_Rain)
Merged <- left_join(Merged, Observed)
#Very significant, Temperature not helping much
interactive <- lm(Hatching ~ ln.rainfall. + Temp + ln.rainfall. * Temp + I(ln.rainfall.^2), data = Merged)
interactive_two <- lm(Hatching ~ ln.rainfall. + I(ln.rainfall.^2), data = Merged)

rain_model <- lm(lnrain ~ ln.rainfall., data = Merged)

#plot(interactive)
#plot(Merged$Temp, Merged$Hatching)

#anova(interactive,interactive_two)

model2 <- lm(Hatching ~ lnGCM + GCMsquared, data = Merged)

Projected$GCMsquared <- Projected$ln.mm.yr.^ 2
Projected$HatchingPredic <- 12.6226 - 3.9933 * Projected$ln.mm.yr. + 0.3251 * Projected$GCMsquared

ggplot(Projected, aes(x=ln.mm.yr., y=HatchingPredic)) + 
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1) +
  labs(title=paste("fortis ln(rainfall) + hatching rates"))

ggplot(Projected, aes(x=GCM.Projected.Precipitation, y=HatchingPredic, color = Projected$ln.mm.yr.)) + 
  geom_point() +
  labs(title=paste("Predicted Hatching Rates for the future")) +
  scale_color_gradient(low="blue", high="red") +
  ylim(0,1) +
  labs(x=paste("Year")) +
  labs(y=paste("Predicted Hatching Rates")) +
  labs(color=paste("GCM Rainfall Prediction"))

Merged$ModeledHatching <- 12.6226 - 3.99333 * Merged$lnGCM + 0.3251 * Merged$GCMsquared
Merged$Error <- Merged$Hatching - Merged$ModeledHatching

ggplot(Merged, aes(x=Year, y=Error)) + 
  geom_point() +
  ylim(-0.5, 0.5) +
  labs(title=paste("Error Plot"))

model3 <- lm(Hatching ~ lnGCM, data = Merged)
ggplot(Merged, aes(x=lnGCM, y=Hatching)) + 
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1) +
  labs(title=paste("fortis ln(rainfall) + hatching rates"))

Projected$HatchingOrig <- -3.10856 + 1.35612 * Projected$ln.mm.yr. - 0.11963 * Projected$GCMsquared

ggplot(Projected, aes(x=GCM.Projected.Precipitation, y=HatchingOrig, color = Projected$ln.mm.yr.)) + 
  geom_point() +
  labs(title=paste("Predicted Hatching Rates for the future")) +
  scale_color_gradient(low="blue", high="red") +
  ylim(0,1) +
  labs(x=paste("Year")) +
  labs(y=paste("Predicted Hatching Rates")) +
  labs(color=paste("GCM Rainfall Prediction"))


  ggplot(Projected, aes(x=ln.mm.yr., y=HatchingOrig)) + 
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1) +
  xlim(0,8) +
  labs(title=paste("fortis ln(rainfall) + hatching rates"))

  ggplot(Future, aes(x=Year, y=Projected.Hatchling.success, color = GCM.rainfall.from.quantile.mapping)) + 
    geom_point() +
    labs(title=paste("Predicted Hatching Rates for the future")) +
    scale_color_gradient(low="blue", high="red") +
    ylim(0,1) +
    labs(x=paste("Year")) +
    labs(y=paste("Predicted Hatching Rates")) +
    labs(color=paste("GCM Rainfall - Quantile Mapped"))
  