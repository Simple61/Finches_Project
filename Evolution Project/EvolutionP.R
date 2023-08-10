setwd("C:\\Users\\carys\\Desktop\\Evolution Project\\Finch Data")
library(dplyr)
library(tidyverse)
library(ggplot2)
library(tigris)
library(scales)
#Beak data of fortis
DS1 = read.csv("C:\\Users\\carys\\Desktop\\Evolution Project\\Finch Data\\Fig.01-06(also7.3).csv")
#Beak data of scandens
DS2 = read.csv("C:\\Users\\carys\\Desktop\\Evolution Project\\Finch Data\\Fig.01-07.csv")
#Beak data of fortis organized by island
DS3 = read.csv("C:\\Users\\carys\\Desktop\\Evolution Project\\Finch Data\\Fig.02-01.csv")
#Beak data for specific species on specific islands
DS4 = read.csv("C:\\Users\\carys\\Desktop\\Evolution Project\\Finch Data\\Fig.02-02.csv")
#Diet of fortis and fuliginosa
DS5 = read.csv("C:\\Users\\carys\\Desktop\\Evolution Project\\Finch Data\\Fig.02-09.csv")
#Rain by mm each year
Rain = read.csv("C:\\Users\\carys\\Desktop\\Evolution Project\\Finch Data\\Fig.04-12.csv")
#Big and Small Seeds each year
Seeds = read.csv("C:\\Users\\carys\\Desktop\\Evolution Project\\Finch Data\\Fig.04-13.csv")
#Sorted Beak Lengths
DS6 = read.csv("C:\\Users\\carys\\Desktop\\Evolution Project\\Finch Data\\Fig.07-06.csv")
#Broods Eggs & Fledglings with nino data
DS7 = read.csv("C:\\Users\\carys\\Desktop\\Evolution Project\\Finch Data\\Fig.05-06.csv")
#Hatchling/Fledgling data for along with env. conditions
DS8 = read.csv("C:\\Users\\carys\\Desktop\\Evolution Project\\Finch Data\\Fig.05-07.csv")
#
DS9 = read.csv("C:\\Users\\carys\\Desktop\\Evolution Project\\Finch Data\\Fig.05-08.csv")
#Monthly Precipitation from Jan 1981
M_Rain = read.csv("C:\\Users\\carys\\Desktop\\Evolution Project\\Finch Data\\M_P.csv")
#Monthly Temperature from Jan 1856
M_Temp = read.csv("C:\\Users\\carys\\Desktop\\Evolution Project\\Finch Data\\M_SST.csv")
#General Circulation Model Data
GCM = read.csv("C:\\Users\\carys\\Desktop\\Evolution Project\\Finch Data\\GCM_Data_Daphne.csv")

ggplot(DS1, aes(x=Year, y=Beak.length)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title=paste("fortis beak length by year"))

ggplot(DS1, aes(x=Year, y=Beak.depth)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title=paste("fortis beak depth by year"))

ggplot(DS1, aes(x=Year, y=Beak.width)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title=paste("fortis beak width by year"))

ggplot(DS2, aes(x=Year, y=Beak.length)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title=paste("scandens beak length by year"))

ggplot(DS2, aes(x=Year, y=Beak.depth)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title=paste("scandens beak depth by year"))

ggplot(DS2, aes(x=Year, y=Beak.width)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title=paste("scandens beak width by year"))

Seeds$Total <- Seeds$small.seeds + Seeds$large.seeds
Seeds$SmallP <- (Seeds$small.seeds / Seeds$Total) * 100
Seeds$LargeP <- (Seeds$large.seeds / Seeds$Total) * 100

Environment <- left_join(Rain, Seeds)
Environment <- filter(Environment, Total > 0)

ggplot(Environment, aes(x=Rain..mm, y=SmallP)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title=paste("Percent of Small Seeds by Rainfall"))

ggplot(Environment, aes(x=Rain..mm, y=LargeP)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title=paste("Percent of Large Seeds by Rainfall"))

Environment_delayed <- Environment
Environment_delayed['Rain..mm'] <- c(NA, head(Environment['Rain..mm'], dim(Environment)[1] - 1)[[1]])

ggplot(Environment_delayed, aes(x=Rain..mm, y=SmallP)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title=paste("Percent of Small Seeds by Rainfall (with rainfall delayed)"))

ggplot(Environment_delayed, aes(x=Rain..mm, y=LargeP)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title=paste("Percent of Large Seeds by Rainfall (with rainfall delayed)"))

#no delay
DS1 <- left_join(DS1, Rain)
DS2 <- left_join(DS2, Rain)

ggplot(DS1, aes(x=Rain..mm, y=Beak.depth)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title=paste("G. fortis Rainfall + Beak Depth"))

ggplot(DS1, aes(x=Rain..mm, y=Beak.length)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title=paste("G. fortis Rainfall + Beak Length"))

ggplot(DS1, aes(x=Rain..mm, y=Beak.width)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title=paste("G. fortis Rainfall + Beak Width"))

ggplot(DS2, aes(x=Rain..mm, y=Beak.depth)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title=paste("G. scandens Rainfall + Beak Depth"))

ggplot(DS2, aes(x=Rain..mm, y=Beak.length)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title=paste("G. scandens Rainfall + Beak Length"))

ggplot(DS2, aes(x=Rain..mm, y=Beak.width)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title=paste("G. scandens Rainfall + Beak Width"))

#Delayed, beak one year after Rain

DS1_delayed <- DS1
DS2_delayed <- DS2

DS1_delayed['Rain..mm'] <- c(NA, head(DS1['Rain..mm'], dim(DS1)[1] - 1)[[1]])
DS2_delayed['Rain..mm'] <- c(NA, head(DS2['Rain..mm'], dim(DS2)[1] - 1)[[1]])

ggplot(DS1_delayed, aes(x=Rain..mm, y=Beak.depth)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title=paste("G. fortis Rainfall + Beak Dept (with rainfall delay)"))

ggplot(DS1_delayed, aes(x=Rain..mm, y=Beak.length)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title=paste("G. fortis Rainfall + Beak Length (with rainfall delay)"))

ggplot(DS1_delayed, aes(x=Rain..mm, y=Beak.width)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title=paste("G. fortis Rainfall + Beak Width (with rainfall delay)"))

ggplot(DS2_delayed, aes(x=Rain..mm, y=Beak.depth)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title=paste("G. scandens Rainfall + Beak Depth (with rainfall delay)"))

ggplot(DS2_delayed, aes(x=Rain..mm, y=Beak.length)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title=paste("G. scandens Rainfall + Beak Length (with rainfall delay)"))

ggplot(DS2_delayed, aes(x=Rain..mm, y=Beak.width)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title=paste("G. scandens Rainfall + Beak Width (with rainfall delay)"))

fortis_beak_width_rain <- lm(DS1_delayed$Beak.width ~ log(DS1_delayed$Rain..mm))
fortis_beak_length_rain <- lm(DS1_delayed$Beak.length ~ log(DS1_delayed$Rain..mm))
fortis_beak_depth_rain <- lm(DS1_delayed$Beak.depth ~ log(DS1_delayed$Rain..mm))
scandens_beak_width_rain <- lm(DS1_delayed$Beak.width ~ log(DS1_delayed$Rain..mm))
scandens_beak_length_rain <- lm(DS1_delayed$Beak.length ~ log(DS1_delayed$Rain..mm))
scandens_beak_depth_rain <- lm(DS1_delayed$Beak.depth ~ log(DS1_delayed$Rain..mm))

ggplot(DS7, aes(x=Broods, y= Eggs)) +
  geom_line() +
  geom_point() +
  labs(title=paste("Broods with egg rates, scandens el nino"))

ggplot(DS7, aes(x=Broods, y= Fledglings)) +
  geom_line() +
  geom_point() +
  labs(title=paste("Broods with fledgling rates, scandens el nino"))

ggplot(DS7, aes(x=Broods.1, y= Eggs.1)) +
  geom_line() +
  geom_point() +
  labs(title=paste("Broods with egg rates, scandens non nino"))

ggplot(DS7, aes(x=Broods.1, y= Fledglings.1)) +
  geom_line() +
  geom_point() +
  labs(title=paste("Broods with fledgling rates, scandens non nino"))

ggplot(DS7, aes(x=Broods.2, y= Eggs.2)) +
  geom_line() +
  geom_point() +
  labs(title=paste("Broods with egg rates, fortis el nino"))

ggplot(DS7, aes(x=Broods.2, y= Fledglings.2)) +
  geom_line() +
  geom_point() +
  labs(title=paste("Broods with fledgling rates, fortis el nino"))

ggplot(DS7, aes(x=Broods.3, y= Eggs.3)) +
  geom_line() +
  geom_point() +
  labs(title=paste("Broods with egg rates, fortis non nino"))

ggplot(DS7, aes(x=Broods.3, y= Fledglings.3)) +
  geom_line() +
  geom_point() +
  labs(title=paste("Broods with fledgling rates, fortis non nino"))

#Add years from Dr.Grant to DS8
Years <- c(1977, 1990, 1986, 1982, 1984, 1980, 1979, 1981, 1976, 1978, 1991, 1987, 1998, 1983)
DS8 <- rename(DS8, ENSO = "Year", ENSO.1 = "Year.1")
DS8$Year <- Years

#El Nino years not seperated from non nino years
ggplot(DS8, aes(x=ln..rainfall., y=Hatching)) + 
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1) +
  labs(title=paste("fortis ln(rainfall) + hatching rates")) +
  labs(x=paste("ln(rainfall)")) +
  labs(y=paste("Hatching Rate of G. fortis"))

hatchingRainfallPlot <- lm(DS8$Hatching ~ DS8$ln..rainfall. + I(DS8$ln..rainfall.^2), data = DS8)
summary(hatchingRainfallPlot)

ggplot(DS8, aes(x=ln..rainfall., y=Fledging)) + 
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1) +
  labs(title=paste("fortis ln(rainfall) + fledging rates"))

ggplot(DS8, aes(x=ln..rainfall..1, y=Hatching.1)) + 
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1) +
  labs(title=paste("scandens ln(rainfall) + hatching rates"))

hatchingRainfallPlotScandens <- lm(DS8$Hatching.1 ~ DS8$ln..rainfall..1 + I(DS8$ln..rainfall..1^2), data = DS8)
summary(hatchingRainfallPlotScandens)

ggplot(DS8, aes(x=ln..rainfall..1, y=Fledging.1)) + 
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1) +
  labs(title=paste("scandens ln(rainfall) + fledging rates"))


M_Temp <- filter(M_Temp, Year > 1972)
Climate <- as.data.frame(unique(M_Temp$Year))
Climate <- rename(Climate, Year = "unique(M_Temp$Year)")
Climate <- left_join(Climate, Rain)

Temp <- M_Temp %>%
  group_by(Year) %>%
  summarise_at(vars(degree_Celsius), list(SST_avg = mean))
Climate <- left_join(Climate, Temp)

Temp <- M_Rain %>%
  group_by(Year) %>%
  summarise_at(vars(mm.month), list(P_avg = mean))
Climate <- left_join(Climate, Temp)

ggplot(Climate, aes(x=P_avg, y=Rain..mm)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title=paste("Rain from book data correlated with other data"))

ggplot(Climate, aes(x=SST_avg, y=Rain..mm)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title=paste("Rain from book data correlated with SST data"))

Climate$lnP <- log(Climate$P_avg)
rain_model <- lm(log(Climate$Rain..mm) ~ log(Climate$P_avg) + Climate$SST_avg, data = Climate)
Climate$predictRain <- predict.lm(rain_model, newdata = Climate)

finch_model <- lm(DS8$Hatching ~ DS8$ln..rainfall. + I(DS8$ln..rainfall. ^ 2), data = DS8)
#in the Climate data copy the DS8$Hatching values in the appropriate rows. Then run the below predict line
finchPredict <- predict.lm(finch_model,newdata=Climate)

Rain$lnRain <- log(Rain$Rain..mm)

hatching_coeffs <- lm(DS8$Hatching ~ DS8$ln..rainfall. + I(DS8$ln..rainfall. ^ 2))
Climate$hatchlingsP <- hatching_coeffs$coefficients[1] + hatching_coeffs$coefficients[2] * Climate$predictRain + hatching_coeffs$coefficients[3] * (Climate$predictRain ^ 2)

combinedPlot <- hatchingRainfallPlot + geom_point(data = Climate, aes(x=predictRain, y=hatchlingsP,color="red")) + 
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, color = "green") +
  labs(title=paste("Prediction of Hatchlings based on Correlated Rain"))
combinedPlot
# < -0.5 La Nina, > 0.5 El Nino, Otherwise neither

GCM$NewHatchling <- 0.25717 + 0.18519 * GCM$GCM.rainfall.for.Daphne - 0.01986 * (GCM$GCM.rainfall.for.Daphne^2)
GCM_Model <- lm(DS8$Hatching ~ GCM$NewHatchling)

ggplot(GCM, aes(x=GCM.rainfall.for.Daphne, y=NewHatchling)) + 
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1) +
  labs(title=paste("scandens ln(rainfall) + fledging rates"))

write.csv(DS8, "DS8-rainfall-hatchling.csv", row.names=FALSE)

# ggplot(DS8, aes(x=ln..rainfall., y=Hatching)) + 
#   geom_point() +
#   stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1) +
#   labs(title=paste("fortis ln(rainfall) + hatching rates"))

ggplot(DS8, aes(x=ln..rainfall., y=Hatching)) + 
  geom_point() +
  geom_smooth() +
  labs(title=paste("fortis ln(rainfall) + hatching rates"))

yearly_temp <- GCM_Temp %>%
  group_by(year) %>%
  summarize(mean = mean(GCM_Temp$Celsius))

