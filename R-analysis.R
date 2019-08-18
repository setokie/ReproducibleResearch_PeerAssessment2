library(tidyverse)
library(ggplot2)
library(Hmisc)


StormData <- read.csv("StormData.csv.bz2")

StormData$PROPDMGEXP <- as.numeric(ifelse(StormData$PROPDMGEXP == "K", 1000, 
                                          ifelse(StormData$PROPDMGEXP == "M", 1000000, 1000000000)))
StormData$PROPDMG <- StormData$PROPDMG * StormData$PROPDMGEXP

StormData$CROPDMGEXP <- as.numeric(ifelse(StormData$CROPDMGEXP == "K", 1000, 
                                          ifelse(StormData$CROPDMGEXP == "M", 1000000, 1000000000)))
StormData$CROPDMG <- StormData$CROPDMG * StormData$CROPDMGEXP


glimpse(StormData)

casualties <- StormData %>%
        group_by(EVTYPE) %>%
        summarise(Fatalities = sum(FATALITIES), 
                  Injuries = sum(INJURIES)) %>%
        arrange(desc(Fatalities), desc(Injuries))

casualties_top5 <- head(casualties, 5)

ggplot(data = test, aes(x = EVTYPE, y = values, fill = total, label = format(values, big.mark = ","))) +
        geom_col(position = "dodge") +
        facet_grid(total ~ ., scales = "free") +
        geom_label(color = "white", label.size = 0.1) +
        ggtitle("Top 5 Weather Event's Casualties in United States from 1950 to 2011") +
        xlab("Weather Event") +
        ylab("Casualties") + 
        theme(plot.title = element_text(size = 12, hjust = 0.5))


test <- casualties_top10 %>%
        gather(total, values, -EVTYPE)


EconomicLoss <- StormData %>%
        group_by(EVTYPE) %>%
        summarise(Properties = sum(PROPDMG),
                  Crop = sum(CROPDMG)) %>%
        arrange(desc(Properties), desc(Crop))
EconomicLoss_top5 <- head(EconomicLoss, 5)
EconomicLoss_top5 <- EconomicLoss_top5 %>%
        gather(total, values, -EVTYPE)

ggplot(data = EconomicLoss_top5, aes(x = EVTYPE, y = values, fill = total, label = format(values, big.mark = ","))) +
        geom_col(position = "dodge") +
        facet_grid(total ~ ., scales = "free") +
        geom_label(color = "white", label.size = 0.1) +
        ggtitle("Top 5 Weather Event's Economic Loss in United States from 1950 to 2011") +
        xlab("Weather Event") +
        ylab("Economic Loss (in USD)") + 
        theme(plot.title = element_text(size = 12, hjust = 0.5))

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
