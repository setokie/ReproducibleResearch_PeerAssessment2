library(tidyverse)
library(ggplot2)
library(Hmisc)


StormData <- read.csv("StormData.csv.bz2")

StormData$PROPDMGEXP <- as.numeric(ifelse(StormData$PROPDMGEXP == "K", 1000, 
                                          ifelse(StormData$PROPDMGEXP == "M", 1000000,
                                                 ifelse(StormData$PROPDMGEXP == "B", 1000000000, 0))))
StormData$PROPDMG <- StormData$PROPDMG * StormData$PROPDMGEXP

StormData$CROPDMGEXP <- as.numeric(ifelse(StormData$CROPDMGEXP == "K", 1000, 
                                          ifelse(StormData$CROPDMGEXP == "M", 1000000,
                                                 ifelse(StormData$CROPDMGEXP == "B", 1000000000, 0))))
StormData$CROPDMG <- StormData$CROPDMG * StormData$CROPDMGEXP


glimpse(StormData)

casualties <- StormData %>%
        group_by(EVTYPE) %>%
        summarise(Total.Fatalities = sum(FATALITIES), 
                  Total.Injuries = sum(INJURIES)) %>%
        arrange(desc(Total.Fatalities), desc(Total.Injuries))

casualties_top5 <- head(casualties, 5)

casualties_top5 <- casualties_top5 %>%
        gather(total, values, -EVTYPE)

ggplot(data = casualties_top5, aes(x = EVTYPE, y = values, fill = total, label = format(values, big.mark = ","))) +
        geom_col(position = "dodge") +
        facet_grid(total ~ ., scales = "free") +
        geom_label(color = "white", label.size = 0.1) +
        geom_text()
        ggtitle("Top 5 Weather Event's Casualties in United States from 1950 to 2011") +
        xlab("Weather Event") +
        ylab("Casualties") + 
        theme(plot.title = element_text(size = 12, hjust = 0.5))
        


test <- casualties_top10 %>%
        gather(total, values, -EVTYPE)

test1 <- StormData %>%
        group_by(EVTYPE) %>%
        summarise(Properties.Damage = sum(PROPDMG),
                  Crop.Damage = sum(CROPDMG)) %>%
        arrange(desc(Properties.Damage), desc(Crop.Damage))

test1 <- head(test1, 5)
test1 <- test1 %>%
        gather(damage, values, -EVTYPE)

ggplot(data = test1, aes(x = EVTYPE, y = values, fill = damage, label = format(values, big.mark = ",", scientific = FALSE))) +
        geom_col(position = "dodge") +
        facet_grid(damage ~ ., scales = "free") +
        geom_label(color = "white", label.size = 0.1) +
        ggtitle("Top 5 Weather Event's Economic Loss in United States from 1950 to 2011") +
        xlab("Weather Event") +
        ylab("Economic Loss (in USD)") + 
        theme(plot.title = element_text(size = 12, hjust = 0.5))

test2 <- StormData %>%
        group_by(EVTYPE) %>%
        summarise(Financial.Damage = sum(PROPDMG) + sum(CROPDMG)) %>%
        arrange(desc(Financial.Damage))
test2 <- head(test2, 5)
qplot(data = test2, x = EVTYPE, y = Financial.Damage, geom = "col")



# Property Damage
PropertyDamage <- StormData %>%
        group_by(EVTYPE) %>%
        summarise(Property.Damage = sum(PROPDMG)) %>%
        arrange(desc(Property.Damage))
PropertyDamage <- head(PropertyDamage, 5)
ggplot(data = PropertyDamage, aes(x = EVTYPE, y = Property.Damage, 
                                  label = paste0(format(round(Property.Damage / 1e6), 
                                                        trim = TRUE, big.mark = ","), "M"))) +
        geom_col(position = "dodge", fill = "steelblue") + 
        ggtitle("Top 5 Weather Event's Property Damage in United States from 1950 to 2011") +
        xlab("Weather Event") +
        ylab("Financial Loss (in USD)") + 
        theme(plot.title = element_text(size = 12, hjust = 0.5)) +
        scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-9)) + 
        geom_label(color = "Dark Gray")

# Crop Damage
CropDamage <- StormData %>%
        group_by(EVTYPE) %>%
        summarise(Crop.Damage = sum(CROPDMG)) %>%
        arrange(desc(Crop.Damage))
CropDamage <- head(CropDamage, 5)

ggplot(data = CropDamage, aes(x = EVTYPE, y = Crop.Damage, 
                                  label = paste0(format(round(Crop.Damage / 1e6), 
                                                        trim = TRUE, big.mark = ","), "M"))) +
        geom_col(position = "dodge", fill = "steelblue") + 
        ggtitle("Top 5 Weather Event's Crop Damage in United States from 1950 to 2011") +
        xlab("Weather Event") +
        ylab("Financial Loss (in USD)") + 
        theme(plot.title = element_text(size = 12, hjust = 0.5)) +
        scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-9)) + 
        geom_label(color = "Dark Gray")

# Total Damage
FinancialDamage <- StormData %>%
        group_by(EVTYPE) %>%
        summarise(Financial.Damage = sum(PROPDMG) + sum(CROPDMG)) %>%
        arrange(desc(Financial.Damage))
FinancialDamage <- head(FinancialDamage, 5)

ggplot(data = FinancialDamage, aes(x = EVTYPE, y = Financial.Damage, 
                              label = paste0(format(round(Financial.Damage / 1e6), 
                                                    trim = TRUE, big.mark = ","), "M"))) +
        geom_col(position = "dodge", fill = "steelblue") + 
        ggtitle("Top 5 Weather Event's Financial Damage in United States from 1950 to 2011") +
        xlab("Weather Event") +
        ylab("Financial Loss (in USD)") + 
        theme(plot.title = element_text(size = 12, hjust = 0.5)) +
        scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-9)) + 
        geom_label(color = "Dark Gray")
