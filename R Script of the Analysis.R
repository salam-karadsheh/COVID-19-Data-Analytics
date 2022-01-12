## Analyzing the economic facet of the pandemic

setwd("/Users/Help/Desktop/PSCI-107/FinalProject")

library(tidyverse)
library(tidyr)
library(rio)
library(lubridate)
library(ggplot2)
library(RColorBrewer)

# Load the data sets
## The first two data sets are taken from https://github.com/owid/covid-19-data/tree/master/public/data
## The last data set is taken from https://www.imf.org/external/datamapper/NGDP_RPCH@WEO/OEMDC/ADVEC/WEOWORLD
covid.data <- import("covid-latest.csv")
entire.data <- import("owid-covid-data.csv")
gdp.growth <- import("gdp_growth_data.csv")

## Let's do some preliminary analysis
head(covid.data)
summary(covid.data)
tail(covid.data)

head(gdp.growth)
summary(gdp.growth)
tail(gdp.growth)

# Each unit of analysis shows up once
table(duplicated(covid.data$location))
table(duplicated(gdp.growth$V1))

# Before we merge the two data sets together, we need to clean some variables
covid.data$location[!(covid.data$location %in% gdp.growth$V1)]

# We need to rename some of the countries to correctly match
gdp.growth$V1[gdp.growth$V1 == "Bahamas, The"] <- "Bahamas"
gdp.growth$V1[gdp.growth$V1 == "Brunei Darussalam"] <- "Brunei"
gdp.growth$V1[gdp.growth$V1 == "Cabo Verde"] <- "Cape Verde"
gdp.growth$V1[gdp.growth$V1 == "China, People's Republic of"] <- "China"
gdp.growth$V1[gdp.growth$V1 == "Congo, Republic of "] <- "Congo"
gdp.growth$V1[gdp.growth$V1 == "Côte d'Ivoire"] <- "Cote d'Ivoire"
gdp.growth$V1[gdp.growth$V1 == "Czech Republic"] <- "Czechia"
gdp.growth$V1[gdp.growth$V1 == "Congo, Dem. Rep. of the"] <- "Democratic Republic of Congo"
gdp.growth$V1[gdp.growth$V1 == "Congo, Republic of "] <- "Congo"
gdp.growth$V1[gdp.growth$V1 == "Gambia, The"] <- "Gambia"
gdp.growth$V1[gdp.growth$V1 == "Hong Kong SAR"] <- "Hong Kong"
gdp.growth$V1[gdp.growth$V1 == "Kyrgyz Republic"] <- "Kyrgyzstan"
gdp.growth$V1[gdp.growth$V1 == "Lao P.D.R."] <- "Laos"
gdp.growth$V1[gdp.growth$V1 == "Macao SAR"] <- "Macao"
gdp.growth$V1[gdp.growth$V1 == "Micronesia, Fed. States of"] <- "Micronesia (country)"
gdp.growth$V1[gdp.growth$V1 == "Cyprus"] <- "Northern Cyprus"
gdp.growth$V1[gdp.growth$V1 == "West Bank and Gaza"] <- "Palestine"
gdp.growth$V1[gdp.growth$V1 == "Russian Federation"] <- "Russia"
gdp.growth$V1[gdp.growth$V1 == "Slovak Republic"] <- "Slovakia"
gdp.growth$V1[gdp.growth$V1 == "Korea, Republic of"] <- "South Korea"
gdp.growth$V1[gdp.growth$V1 == "South Sudan, Republic of"] <- "South Sudan"
gdp.growth$V1[gdp.growth$V1 == "Taiwan Province of China"] <- "Taiwan"
gdp.growth$V1[gdp.growth$V1 == "Timor-Leste"] <- "Timor"

# Now let's check again
covid.data$location[!(covid.data$location %in% gdp.growth$V1)]
length(covid.data$location[(covid.data$location %in% gdp.growth$V1)]) == 
  length(gdp.growth$V1[(gdp.growth$V1 %in% covid.data$location)])

# Remove the the first two rows
gdp.growth <- gdp.growth[-c(1, 2),]

# We can now merge the data sets according to the corresponding countries
covid.by.country <- merge(covid.data, gdp.growth, by.x = "location", by.y = "V1", all.x = T)

# Subset the merged dataset to only include relevent variables
covid.by.country <- cbind.data.frame(covid.by.country$location, covid.by.country$continent, 
                                     covid.by.country$total_cases, covid.by.country$total_deaths, 
                                     covid.by.country$total_cases_per_million, covid.by.country$total_deaths_per_million, 
                                     covid.by.country$total_tests, covid.by.country$total_tests_per_thousand, 
                                     covid.by.country$positive_rate, covid.by.country$tests_per_case, 
                                     covid.by.country$people_vaccinated, covid.by.country$people_vaccinated_per_hundred, 
                                     covid.by.country$population, covid.by.country$population_density, 
                                     covid.by.country$median_age, covid.by.country$aged_65_older, 
                                     covid.by.country$gdp_per_capita, covid.by.country$extreme_poverty, 
                                     covid.by.country$human_development_index, covid.by.country$V41, 
                                     covid.by.country$V42)

# Let's rename the variable for convenience
covid.by.country <- rename(covid.by.country, 
                           Country = "covid.by.country$location", 
                           Continent = "covid.by.country$continent", 
                           Total_Cases = "covid.by.country$total_cases", 
                           Total_Deaths = "covid.by.country$total_deaths", 
                           Total_Cases_Per_Million = "covid.by.country$total_cases_per_million", 
                           Total_Deaths_Per_Million = "covid.by.country$total_deaths_per_million", 
                           Total_Tests = "covid.by.country$total_tests", 
                           Total_Tests_Per_Thousand = "covid.by.country$total_tests_per_thousand", 
                           Positive_Rate = "covid.by.country$positive_rate", 
                           Tests_Per_Case = "covid.by.country$tests_per_case", 
                           People_Vaccinated = "covid.by.country$people_vaccinated", 
                           People_Vaccinated_Per_Hundred = "covid.by.country$people_vaccinated_per_hundred", 
                           Population = "covid.by.country$population", 
                           Population.Density = "covid.by.country$population_density", 
                           Median_Age = "covid.by.country$median_age", 
                           Aged_65_And_Older = "covid.by.country$aged_65_older", 
                           GDP_Per_Capita = "covid.by.country$gdp_per_capita", 
                           Extreme_Poverty_Rate = "covid.by.country$extreme_poverty", 
                           Human_Development_Index = "covid.by.country$human_development_index", 
                           GDP_Growth_2019 = "covid.by.country$V41", 
                           GDP_Growth_2020 = "covid.by.country$V42")

## Change the class of the growth variables from character to numeric
class(covid.by.country$GDP_Growth_2019)
covid.by.country$GDP_Growth_2019 <- as.numeric(covid.by.country$GDP_Growth_2019)

class(covid.by.country$GDP_Growth_2020)
covid.by.country$GDP_Growth_2020 <- as.numeric(covid.by.country$GDP_Growth_2020)


## ANALYSIS
## Check if there's a considerable relationship between population density and cases
# P-value is so small that there is a relationship between the two variables
t.test(covid.by.country$Total_Cases_Per_Million, covid.by.country$Population.Density)


# First we need to find the general trend keeping the population density constant
plot(covid.by.country$GDP_Per_Capita, covid.by.country$Total_Cases_Per_Million, 
     main = "Covid Cases by GDP Per Capita For Each Country", 
     xlab = "GDP Per Capita", 
     ylab = "Total Covid Cases Per Million", xaxt = "n", pch = 16, font.lab = 2)
abline(lm(covid.by.country$Total_Cases_Per_Million ~ covid.by.country$GDP_Per_Capita + 
            covid.by.country$Population.Density), lwd = 3)
axis(1, at=axTicks(1), labels=sprintf("$%s", axTicks(1)))


# Now, let's get the mean stringency index from all countries
all.countries <- unique(entire.data$location[entire.data$location %in% covid.by.country$Country])
covid.by.country$mean_strictness <- NA

for (i in 1:length(all.countries)) {
  mean.value <- mean(entire.data$stringency_index[entire.data$location == all.countries[i]], na.rm = T)
  covid.by.country$mean_strictness[covid.by.country$Country == all.countries[i]] <- mean.value 
}


# Now let's classify the strictness and leniency of the guidelines based on the median. 
# This can be done without a for loop, but I wanted to practice using for loops
covid.by.country$strictness <- NA
for (i in 1:length(covid.by.country$strictness)) {
  if (!is.na(covid.by.country$mean_strictness[i])) {
    if (covid.by.country$mean_strictness[i] > median(covid.by.country$mean_strictness, na.rm = T)) {
      covid.by.country$strictness[i] <- "Strict Guidelines"
    }
    else if (covid.by.country$mean_strictness[i] < median(covid.by.country$mean_strictness, na.rm = T)) {
      covid.by.country$strictness[i] <- "Lenient Guidelines"
    } 
  }
}



## Now let's look at the stringency index values while controlling for population density
summary(lm(covid.by.country$Total_Cases_Per_Million[covid.by.country$strictness == "Strict Guidelines"] ~ 
             covid.by.country$GDP_Per_Capita[covid.by.country$strictness == "Strict Guidelines"]))

summary(lm(covid.by.country$Total_Cases_Per_Million[covid.by.country$strictness == "Lenient Guidelines"] ~ 
             covid.by.country$GDP_Per_Capita[covid.by.country$strictness == "Lenient Guidelines"]))

plot(covid.by.country$GDP_Per_Capita, covid.by.country$Total_Cases_Per_Million, 
     main = "Covid Cases by GDP Per Capita For Each Country \n (Stringency Index)", 
     xlab = "GDP Per Capita", 
     ylab = "Total Covid Cases Per Million", xaxt = "n", font.lab = 2)
axis(1, at=axTicks(1), labels=sprintf("$%s", axTicks(1)))
points(covid.by.country$GDP_Per_Capita[covid.by.country$strictness == "Strict Guidelines"], 
       covid.by.country$Total_Cases_Per_Million[covid.by.country$strictness == "Strict Guidelines"], 
       pch = 16, col = "darkblue")
points(covid.by.country$GDP_Per_Capita[covid.by.country$strictness == "Lenient Guidelines"], 
       covid.by.country$Total_Cases_Per_Million[covid.by.country$strictness == "Lenient Guidelines"], 
       pch = 16, col = "firebrick")
points(covid.by.country$GDP_Per_Capita[is.na(covid.by.country$strictness)], 
       covid.by.country$Total_Cases_Per_Million[is.na(covid.by.country$strictness)], 
       pch = NA_integer_) ## REMOVE THESE
abline(lm(covid.by.country$Total_Cases_Per_Million[covid.by.country$strictness == "Strict Guidelines"] ~ 
            covid.by.country$GDP_Per_Capita[covid.by.country$strictness == "Strict Guidelines"] +
            covid.by.country$Population.Density[covid.by.country$strictness == "Strict Guidelines"]), 
       col = "darkblue", lwd = 3)
abline(lm(covid.by.country$Total_Cases_Per_Million[covid.by.country$strictness == "Lenient Guidelines"] ~ 
            covid.by.country$GDP_Per_Capita[covid.by.country$strictness == "Lenient Guidelines"] +
            covid.by.country$Population.Density[covid.by.country$strictness == "Lenient Guidelines"]), 
       col = "firebrick", lwd = 3)
legend("topright", inset=.02, title="Stringency Index",
       legend = c("Strict Guidelines", "Lenient GuideLines"), col = c("darkblue", "firebrick"), 
       fill= c("darkblue", "firebrick"), cex = 0.8)



## Now let's look at GDP Growth for 2019 while controlling for population density
summary(lm(covid.by.country$Total_Cases_Per_Million ~ covid.by.country$GDP_Per_Capita + covid.by.country$Population.Density))
plot(covid.by.country$GDP_Per_Capita, covid.by.country$Total_Cases_Per_Million, 
     main = "Covid Cases by GDP Per Capita For Each Country \n(2019 GDP Growth)", 
     xlab = "GDP Per Capita", 
     ylab = "Total Covid Cases Per Million", xaxt = "n", font.lab = 2)
axis(1, at=axTicks(1), labels=sprintf("$%s", axTicks(1)))
points(covid.by.country$GDP_Per_Capita[covid.by.country$GDP_Growth_2019 > 
                                         median(covid.by.country$GDP_Growth_2019, na.rm = T)], 
       covid.by.country$Total_Cases_Per_Million[covid.by.country$GDP_Growth_2019 > 
                                                  median(covid.by.country$GDP_Growth_2019, na.rm = T)], 
       pch = 16, col = "orange")
points(covid.by.country$GDP_Per_Capita[covid.by.country$GDP_Growth_2019 <= 
                                         median(covid.by.country$GDP_Growth_2019, na.rm = T)], 
       covid.by.country$Total_Cases_Per_Million[covid.by.country$GDP_Growth_2019 <= 
                                                  median(covid.by.country$GDP_Growth_2019, na.rm = T)], 
       pch = 16, col = "blue")
abline(lm(covid.by.country$Total_Cases_Per_Million[covid.by.country$GDP_Growth_2019 > 
                                                     median(covid.by.country$GDP_Growth_2019, na.rm = T)] 
          ~ covid.by.country$GDP_Per_Capita[covid.by.country$GDP_Growth_2019 > 
                                              median(covid.by.country$GDP_Growth_2019, na.rm = T)] + 
         covid.by.country$Population.Density[covid.by.country$GDP_Growth_2019 > 
                                               median(covid.by.country$GDP_Growth_2019, na.rm = T)]), 
       lwd = 3, col = "orange")
abline(lm(covid.by.country$Total_Cases_Per_Million[covid.by.country$GDP_Growth_2019 <= 
                                                     median(covid.by.country$GDP_Growth_2019, na.rm = T)] 
          ~ covid.by.country$GDP_Per_Capita[covid.by.country$GDP_Growth_2019 <= 
                                              median(covid.by.country$GDP_Growth_2019, na.rm = T)] + 
         covid.by.country$Population.Density[covid.by.country$GDP_Growth_2019 <=
                                               median(covid.by.country$GDP_Growth_2019, na.rm = T)]), 
       lwd = 3, col = "blue")
legend("topright", inset=.02, title="Relative 2019 GDP Growth",
       legend = c("High Growth", "Low Growth/Loss"), col = c("orange", "blue"), 
       fill= c("orange", "blue"), cex = 0.8)



## Now let's look at the gdp growth for 2020 while controlling for the population density
plot(covid.by.country$GDP_Per_Capita, covid.by.country$Total_Cases_Per_Million, 
     main = "Covid Cases by GDP Per Capita For Each Country \n(2020 GDP Growth)", 
     xlab = "GDP Per Capita", 
     ylab = "Total Covid Cases Per Million", xaxt = "n", font.lab = 2)
axis(1, at=axTicks(1), labels=sprintf("$%s", axTicks(1)))
points(covid.by.country$GDP_Per_Capita[covid.by.country$GDP_Growth_2020 > 
                                         median(covid.by.country$GDP_Growth_2020, na.rm = T)], 
       covid.by.country$Total_Cases_Per_Million[covid.by.country$GDP_Growth_2020 > 
                                                  median(covid.by.country$GDP_Growth_2020, na.rm = T)], 
       pch = 16, col = "orange")
points(covid.by.country$GDP_Per_Capita[covid.by.country$GDP_Growth_2020 <= 
                                         median(covid.by.country$GDP_Growth_2020, na.rm = T)], 
       covid.by.country$Total_Cases_Per_Million[covid.by.country$GDP_Growth_2020 <= 
                                                  median(covid.by.country$GDP_Growth_2020, na.rm = T)], 
       pch = 16, col = "blue")
abline(lm(covid.by.country$Total_Cases_Per_Million[covid.by.country$GDP_Growth_2020 > 
                                                     median(covid.by.country$GDP_Growth_2020, na.rm = T)] 
          ~ covid.by.country$GDP_Per_Capita[covid.by.country$GDP_Growth_2020 > 
                                              median(covid.by.country$GDP_Growth_2020, na.rm = T)] + 
         covid.by.country$Population.Density[covid.by.country$GDP_Growth_2020 > 
                                               median(covid.by.country$GDP_Growth_2020, na.rm = T)]), 
       lwd = 3, col = "orange")
abline(lm(covid.by.country$Total_Cases_Per_Million[covid.by.country$GDP_Growth_2020 <= 
                                                     median(covid.by.country$GDP_Growth_2020, na.rm = T)] 
          ~ covid.by.country$GDP_Per_Capita[covid.by.country$GDP_Growth_2020 <= 
                                              median(covid.by.country$GDP_Growth_2020, na.rm = T)] + 
         covid.by.country$Population.Density[covid.by.country$GDP_Growth_2020 <=
                                               median(covid.by.country$GDP_Growth_2020, na.rm = T)]), 
       lwd = 3, col = "blue")
legend("topright", inset=.02, title="Relative 2020 GDP Growth",
       legend = c("High Growth", "Low Growth/Loss"), col = c("orange", "blue"), 
       fill= c("orange", "blue"), cex = 0.8)



## Let's look at Sweden 
covid.by.country$strictness[covid.by.country$Country == "Sweden"]
covid.by.country$Total_Cases_Per_Million[covid.by.country$Country == "Sweden"] > mean(covid.by.country$Total_Cases_Per_Million, na.rm = T)
covid.by.country$GDP_Per_Capita[covid.by.country$Country == "Sweden"] > mean(covid.by.country$GDP_Per_Capita, na.rm = T) 
covid.by.country$GDP_Growth_2019[covid.by.country$Country == "Sweden"] > median(covid.by.country$GDP_Growth_2019, na.rm = T) 
covid.by.country$GDP_Growth_2020[covid.by.country$Country == "Sweden"] > median(covid.by.country$GDP_Growth_2020, na.rm = T) 

## Let's look at Jordan
covid.by.country$strictness[covid.by.country$Country == "Jordan"]
covid.by.country$Total_Cases_Per_Million[covid.by.country$Country == "Jordan"] > mean(covid.by.country$Total_Cases_Per_Million, na.rm = T)
covid.by.country$GDP_Per_Capita[covid.by.country$Country == "Jordan"] > mean(covid.by.country$GDP_Per_Capita, na.rm = T) 
covid.by.country$GDP_Growth_2019[covid.by.country$Country == "Jordan"] > median(covid.by.country$GDP_Growth_2019, na.rm = T) 
covid.by.country$GDP_Growth_2020[covid.by.country$Country == "Jordan"] > median(covid.by.country$GDP_Growth_2020, na.rm = T) 
