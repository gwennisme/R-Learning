### R-Learning - Generate Bar Charts ###
### Author: Gwendoline Tan (@gwennisme) ###
### Data Source: Makeover Monday 2018 Week 39 ###

### Load the libraries required ###
library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)
library(stringr)

### Retrieve data for processing ###

raw_data <- read.csv("EM2030 Advocates Survey extract - Makeover Monday.csv")

### Filter data for processing ###

priority_data <- raw_data %>% filter(Question.Grouping == "What should be the three biggest priorities today in relation to gender equality?" & Value == 1 & Labels != "Don't know" & Labels != "None of the above") %>% select(Value, Labels) %>% group_by(Labels) %>% summarise(counts = n()) %>% arrange(desc(counts))

govt_data <- raw_data %>% filter(Question.Grouping == "To what extent are the following factors relevant in explaining gaps in government data sources relating to gender equality?" & Labels != "Don't know" & Labels != "No answer") %>% mutate(Labels = factor(Labels, levels = c("Not at all relevant", "Not very relevant", "Fairly relevant", "Very relevant"))) %>% select(Wording, Value, Labels) %>% group_by(Wording, Labels, Value) %>% summarise(counts = n()) %>% arrange(Wording, Value)

### Plot bar chart (Top priorities in gender equality) ###

genderEqualityPlot <- ggplot(data = priority_data)+ geom_bar(mapping = aes(x = reorder(Labels, counts), y = counts), stat = "identity", fill="#FF6666") + coord_flip() + scale_x_discrete(labels=function(x) str_wrap(x, width=18)) + labs(x = "", y = "Total Counts", title = "Top Priorities in Addressing Gender Equality")

### Plot stacked bar chart (Factors explaning gaps in government sources) ###

gapsPlot <- ggplot(data = govt_data) + geom_bar(mapping = aes(x = Wording, y = counts, fill = Labels), stat = "identity") + coord_flip() + scale_fill_brewer(palette = 12) + scale_x_discrete(labels=function(x) str_wrap(x, width=13)) + labs(x = "", y = "Total Counts", title = "Factors Explaining Gaps in Government Data Sources")

### Plot both charts on the same page ###

grid.arrange(genderEqualityPlot, gapsPlot, ncol=2)
