# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# DATA8010 - Assignment 1
# Brian Higgins
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Libraries
# -----------------------------------------------------------------------------
# Note: install is commented out to avoid reloading. Please uncomment if you are
# missing any of the packages.

# library to load in excel spreadsheet
#install.packages("xlsx")
library(xlsx)

## library for plots
#install.packages("ggplot2")
library(ggplot2)

# library to create heat map
#install.packages("lattice")
library(lattice)

#install.packages("reshape2"), melt function for correlation heatmap
library(reshape2)

#install.packages("dplyr")
library(dplyr)

# Library used in animations
#install.packages("gganimate")
library(gganimate)

# Needed png as my animation did not run without it
#install.packages("png")
library(png)

# Needed to install this so my animation would run
#install.packages("gifski")
library(gifski)

# Library used with ggpiars to see plot/correlation score.
#install.packages("GGally")
library(GGally)

# Library used with creating a table
#install.packages("kableExtra")
library(kableExtra)

# used for within function
#install.packages("lubridate")
library(lubridate)

# used for to look at skewess
#install.packages("e1071")
library(e1071)

# used with descibe
#install.packages("psych")
library(psych) 

# -----------------------------------------------------------------------------
# Workspace Notes
# -----------------------------------------------------------------------------
# Often I created variables to keep code simpler and then deleted after use.
# The workspace items are deleted at the end of each section in case you would 
# like to see them in operation. This is just to declutter the workspace.
# Data frames, tables, plots are kept

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# 1. Using the xlsx or readxl package or otherwise, read this file into R and 
# generate a data frame.
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# Load data and put into a data frame
data <- read.xlsx("STAT8010_assignment1_2022.xls", sheetIndex =1)

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# 2. Explore the data. Convert the timestamp to an appropriate time/date variable 
# in R for both sheets (this must not be a character – R must understand the date AND time).
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# Look into data
str(data)
# 1 character column for the Date...Timestamp. Looks like some date issues as the first 
# element is different to the rest. Looking at the excel sheet it is in a different format.
# The rest of the columns are numeric columns.

# Look into columns more
summary(data)
# date - date column - To do: Will change this into time variable. Fix first element
# DO1, DO2, Controlling DO, PH numeric, Biomass, Titre - numeric
# Base Buffer and Media Batch look like categories as they have few elements.


# ---------------------------------------------
# 2.1 Explore each column
# ---------------------------------------------

# Get a count of unique items in each column.
for(i in colnames(data)){
  cat("Unique values in", i, ":", length(unique(data[,i])), "\n")
}
# Each value in Date is unique.
# Again Base.Buffer and Media.Batch is showing only 2,3 unique types so these can 
# be turned into factors for use later.
# Other columns have a number of unique values. Given the type of data, this is not an issue.

# Missing values in columns
 for (i in colnames(data)){
  cat("Missing values in", i, ":", sum(is.na(data[,i])), "\n")
}
## There are no missing values in any of the columns. 

# Duplicate values
for (i in colnames(data)){
  cat("Duplicate values in", i, ":", sum(duplicated(data[,i])), "\n")
}
# Similar results to the unique code above.
# A large amount of duplicates but given the type of data this is not an issue at this point.

# clean workspace
rm(i)

# ---------------------------------------------
# 2.2 Data Manipulation
# ---------------------------------------------

# 2.2.1. Fix the first first entry as it is in a different format. 
# 2.2.2. Change the "Date...Time stamp to a Date and Time type
# 2.2.3. Add factor columns for Base.Buffer and Media Buffer.

# 2.2.1 Fix the time by manually changing it to the same format as others.
# There is no seconds time so we will zero this.
data$Date...Timestamp[1] = "28 Mar 2020 17:28:00"

# 2.2.2 Change Date...Timstamp to a time format. 
data$Date <- data$Date <- as.POSIXct(data$Date...Timestamp,  format="%d %b %Y %H:%M:%S")

# 2.2.3.Add factor columns for Base.Buffer and Media.Batch
unique(data$Base.Buffer) 
# Only two values, 0,1 . Make this into a factor and add it to the data.
data$Base.Buffer.Factor <- factor(data$Base.Buffer)

unique(data$Media.Batch)
# only three values, 202201, 202202, 202203 for each batch.
data$Media.Batch.Factor <- factor(data$Media.Batch)

# ---------------------------------------------
# 2.3 Look at the top of the data.
# ---------------------------------------------

# create table to show head fo table in the report.
table_head <- head(data)

# make the table in the same style as used across report. Table added to report.
table_head %>%
  kbl(caption = "Head of the Data") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# table for report
# View(table_head)

# ---------------------------------------------
# 2.4 Quick look at how the columns look like
# ---------------------------------------------

# plot to show correlation of the columns 
ggpairs(data[,2:10], title="Qucik Plot to look at data and relationships",
        upper = list(continuous = wrap('cor', size = 3)),
        lower = list(continuous = wrap("points", alpha = 0.2,    size=0.1))) +
  theme(axis.text.y = element_blank()) +
  theme(axis.text.x = element_blank())

# At this point I have removed the x and y values as they just confuse the plot
# Also made the points smaller as they squashed the plots.

# Looks like Controlling Do and DO1 and DO have some parts that are strongly correlated but
# other parts that are not.
# Base Buffer and Media Batch look like they are categories.
# Biomass and Titre look somewhat correlated but its very wide


# ---------------------------------------------
# 2.5 Look at distribution of the numeric columns
# ---------------------------------------------

# Look at the distribution of the columns
par(mfrow=c(2,3))
hist(data$DO1, main="DO1 Data", xlab=" DO1", ylab="")
hist(data$DO2, main=" DO2 Data", xlab=" DO2", ylab="")
hist(data$Controlling.DO,main="Controlling DO Data", xlab=" Controlling DO", ylab="")
hist(data$pH,main="pH Data", xlab="pH", ylab="")
hist(data$Biomass,main="Biomass Data", xlab="Biomass", ylab="")
hist(data$Titre..mg.mL.,main="Titre Data", xlab=" Titre", ylab="")


# ---------------------------------------------
# 2.6 Look at distribution of the numeric columns
# ---------------------------------------------

# Create a custom table to take only certain statistical values
# Spent hours on this as there has to be away to do it with Dyplr.
# In the end I did a simpliar way to get what I wanted. Outputs were just too
# messy using "summarise_each" (deprecated now and replaced) and "across". 

# I left it included below if you are curious.
data %>%
  select(DO1,DO2, Controlling.DO, pH) %>%
  summarise_each (funs(min, max, mean, median, sd))

# I also left in an example of "across"
data %>% summarise(across(DO1:pH, funs(min, max, mean, median, sd)))
# I would need to create several vectors and join together for what I wanted.

# USed descibe in the end and dropped what I didnt want.
# Descibe lets me get what I want, almost, I wanted to drop some columns.
num_dist <- describe(data[2:7])
num_dist <- num_dist[-1:-2]
num_dist <- num_dist[-4:-5]
num_dist <- num_dist[-7:-9]
options(digits = 2)

# put in a table
num_dist %>%
  kbl(caption = "Summary Statistics for Numeric Columns") %>%
  kable_classic(full_width = F, html_font = "Cambria")

#clean up workspace.
rm(num_dist)

#-----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# 3. Ensure that all data frame variables are in the correct format and no errors 
# have occurred in steps 1 or 2.
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

str(data) # Looks ok. 
# I will keep Base.Buffer/Media.Batch as both numeric and as.factor to use later.

# Check the head of data
head(data) # Head looks ok. 

# Check the bottom of the data.
tail(data) # Tail looks ok.

# After part 2. The data frame variables are all in the correct format.
# The top and bottom of the data looks good.

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# 4. The Controlling DO changes (between DO1 and DO2) during the batch. Using R, 
# determine the number of times it changes during the batch. Do this using an 
# interrogation of the raw data and confirm this using ggplot2. Include your ggplot2 
# output in your report.
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# ---------------------------------------------
# 4.1. How many of each DO1/DO2 do In have in the controlling.DO
# ---------------------------------------------

# Get amount of different results in the columns
data %>% tally(data$Controlling.DO == data$DO1) # 95 differences
data %>% tally(data$Controlling.DO == data$DO2) # 575 differences
# So DO1 has a much samller amount.

# ---------------------------------------------
# 4.2. Determine number of times the Controlling DO changes between DO1 and DO2
# ---------------------------------------------

# Create an ifelse that outputs if the result is DO1 or DO2 to another column.
data$DO_Switch <- ifelse(data$Controlling.DO == data$DO1, "DO1",
                       ifelse (data$Controlling.DO == data$DO2, "DO2", "None"))
# Add a new column to the data frame as data$results
data$DO_Switch_factor <- as.factor(data$DO_Switch)

# Count how many times the results changes.
data %>%  filter(DO_Switch == "DO2" & lag(DO_Switch) == "DO1") %>%  nrow()
# Only changes once between controlling.DO and DO1 and DO2.

# ---------------------------------------------
# 4.3. Plot Controlling and DO1/DO2 together to show just number of changes.
# ---------------------------------------------

# plot when the controlling DO changes from DO1 to DO2
ggplot(data, aes(x=Date, y=Controlling.DO, color=DO_Switch)) +
  geom_line() +
  geom_line(group=1) +
  geom_line(aes(y=cummean(Controlling.DO), color="Mean")) +
  theme_bw() +
  ggtitle("Plot for the two Controlling DO periods") +
  labs(x = "Time line", y=" Controlling D0 ") +
  geom_vline(xintercept = as.POSIXct("2020-04-07 11:09:04"),linetype = "dashed", color = "blue") +
  geom_vline(xintercept = as.POSIXct("2020-04-03 15:17:51"),linetype = "dashed", color = "blue") +
  labs(color = "DO Types") 

# red points shows DO1 and Green Shows DO2 when the controlling Do takes over
# The mean is shown in blue
# Red lines show the gap where data points are missing between the switch over.

# ---------------------------------------------
# 4.4 Show how when DO1 changes the controlling DD switches from DO1 to DO2
# ---------------------------------------------
# I wanted another plot to show what when DO1 jumps, the controlling DO1 changes
# to DO2

ggplot(data, aes(x=Date)) +
  geom_line(aes(y=DO1,color="DO1")) +
  geom_line(aes(y=DO2,color ="DO2")) +
  geom_line(aes(y=Controlling.DO,color="Controlling DO"))+
  theme_bw() +
  ggtitle("Plot to show DO1, DO2 and Controlling.DO")+
  labs(x = "Time Line",
       y = "DO1, DO2, Controlling DO") +
  scale_color_manual( name=" Colour guide",
                      breaks=c("DO1", "DO2", "Controlling DO"),
                      values=c("DO1"="red", "DO2"="green", "Controlling DO"="blue"))

# When DO1 jumps in values, the controlling Do switches to DO2
  
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# 5. Create a new column in this data frame that shows the time interval in minutes 
# between switching the controlling DO probe..
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# This is not the best way to do this but after a few hours I just wanted to have it
# done. I get the right count. Later I will ask how to do it better

# # create two seperate data frames for D01 and DO2
d01 <- data %>% filter(DO_Switch =="DO1")
d02 <- data %>%filter(DO_Switch =="DO2")

# take the cumulative count for DO1 and for DO2
data_d01 <- d01 %>%
  mutate(Time_Min_Cum = difftime(Date, lag(Date, default = Date[1] ), unit = "mins") %>% 
           as.numeric() %>% 
           cumsum())
data_d02 <- d02 %>%
  mutate(Time_Min_Cum = difftime(Date, lag(Date, default = Date[1] ), unit = "mins") %>% 
           as.numeric() %>%
           cumsum())

# take just the cumulative column as a vector for D01 and D02
data_d01 <-data_d01[,15]
data_d02 <-data_d02[,15]

# add the two vectors as a single vector and then add to the dataframe as time_interval
Time_Min_Cum <-c(data_d01, data_d02)
data$Time_Interval <- Time_Min_Cum

# clean up workspace
rm(d01, d02, data_d01, data_d02, Time_Min_Cum)

# Plot to show cumulative count in minutes.
ggplot(data, aes(x=Date, y=Time_Interval))+
  geom_line() +
  theme_bw() +
  geom_vline(xintercept = as.POSIXct("2020-04-07 11:09:04"),linetype = "dashed", color = "blue") +
  ggtitle("Plot to show the Cumulative count in Minutes for each DO ") +
  labs(x = "Cumulative Count over Time Line") +
  labs(y = "Numer of minutes") 
  
  
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# 6. Make a table showing the following summary statistics for each batch: maximum pH, 
# minimum pH, mean pH, median pH, standard deviation of pH, initial ph, final ph, 
# start time/date, end time/date, number of days batch is active, and the number of 
# datapoints for the batch.
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# ---------------------------------------------
# Alternative way to do this
# ---------------------------------------------

# While learning R I learned I could have done the above code with less variables
# using summaries.  With more time I would have rewritten this.
# However I use this method later in the next question when creating a tables for
# tables in question 7.

data %>% 
  group_by(Media.Batch.Factor)%>%
  summarise(Min= min(pH), Max= max(pH))

# ---------------------------------------------
# 6.0. Seperate batches into 3
# ---------------------------------------------


# Create three batches
b01 <- data %>% filter(data$Media.Batch == 202201)

b02 <- data %>% filter(data$Media.Batch == 202202)

b03 <- data %>% filter(data$Media.Batch == 202203)

# ---------------------------------------------
# 6.1. Batch b01: 202201 Summary Details
# ---------------------------------------------

# 1. Min
b01_1 <- min(b01$pH) # min 6.7
# 2. Max
b01_2 <- max(b01$pH) # max 7.1
# 3. Mean
b01_3 <- round(mean(b01$pH),2) # mean 6.89
# 4. Median
b01_4 <- median(b01$pH) # median 6.88
# 5. Standard Deviation
b01_5 <- round(sd(b01$pH),2) #0.12
# 6. First value
b01_6 <- head(b01$pH, n=1) # 6.7
# 7. Last value
b01_7 <- tail(b01$pH, n=1) # 7
# 8. Start time
b01_8 <- as.character(head(b01$Date, n=1)) # "2020-03-31 14:03:28 BST"
# 9. End time
b01_9 <- as.character(tail(b01$Date, n=1)) # "2020-05-01 13:04:41 BST"
# 10. Number of days in each batch
date_1 = as.POSIXct(head(b01$Date, n=1),  format="%d %b %Y %H:%M:%S")
date_2 = as.POSIXct(tail(b01$Date, n=1),  format="%d %b %Y %H:%M:%S")
b01_10 <- round(difftime(date_2,date_1, units="days"),2) # 30.96 days
# 11. Number of data points in Batch 01 # I know time is unique from earlier check of duplicates.
b01_11 <- length(b01$Date...Timestamp) # 380

# place all the Batch 01 summary data together
batch_01_summary <- c(b01_1, b01_2, b01_3, b01_4, b01_5, b01_6,
                      b01_7, b01_8, b01_9, b01_10, b01_11)

batch_01_summary
# clean workspace
rm(date_1,date_2,b01_1, b01_2, b01_3, b01_4, b01_5, b01_6,
   b01_7, b01_8, b01_9, b01_10, b01_11)


# ---------------------------------------------
# 6.2 Batch bo2:202202 Summary Details
# ---------------------------------------------

# 1. Min
b02_1 <- min(b02$pH) # min 6.7
# 2. Max
b02_2 <- max(b02$pH) # max 7.1
# 3. Mean
b02_3 <- round(mean(b02$pH),2) # mean 6.9
# 4. Median
b02_4 <- median(b02$pH) # median 6.9
# 5. Stardard Deviation
b02_5 <- round(sd(b02$pH),2) # 0.12
# 6. First value
b02_6 <- head(b02$pH, n=1) # 6,82
# 7. Last value
b02_7 <- tail(b02$pH, n=1) # 6.74
# 8. Start time
b02_8 <- as.character(head(b02$Date, n=1)) #  "2020-05-01 13:17:32 BST
# 9. End time
b02_9 <- as.character(tail(b02$Date, n=1)) #  "2020-05-06 11:58:57 BST"
# 10. Number of days in each batch
date_1 = as.POSIXct(head(b02$Date, n=1),  format="%d %b %Y %H:%M:%S")
date_2 = as.POSIXct(tail(b02$Date, n=1),  format="%d %b %Y %H:%M:%S")
b02_10 <- round(difftime(date_2,date_1, units="days"),2) # 4.95 days
# 11. Number of data points in Batch 01 # I know time is unique from earlier check of duplicates.
b02_11 <- length(unique(b02$Date)) # 68

batch_02_summary <- c(b02_1, b02_2, b02_3, b02_4, b02_5, b02_6,
                      b02_7, b02_8, b02_9, b02_10, b02_11)

# clean workspace
rm(date_1,date_2,b02_1, b02_2, b02_3, b02_4, b02_5, b02_6,
   b02_7, b02_8, b02_9, b02_10, b02_11)

# ---------------------------------------------
# 6.3 Batch b03: 202203 Summary Details
# ---------------------------------------------

# 1. Min
b03_1 <- min(b03$pH) # min 6.7
# 2. Max
b03_2 <- max(b03$pH) # max 7.1
# 3. Mean
b03_3 <- round(mean(b03$pH),2) # mean 6.9
# 4. Median
b03_4 <- median(b03$pH) # median 6.9
# 5. Stardard Deviation
b03_5 <- round(sd(b03$pH),2) # 0.13
# 6. First value
b03_6 <- head(b03$pH, n=1) # 6,76
# 7. Last value
b03_7 <- tail(b03$pH, n=1) # 6.74
# 8. Start time
b03_8 <- as.character(head(b03$Date, n=1)) #  "2020-05-06 12:25:57 BST"
# 9. End time
b03_9 <- as.character(tail(b03$Date, n=1)) #  "2020-06-15 01:36:32 BST"
# 10. Number of days in each batch
date_1 = as.POSIXct(head(b03$Date, n=1),  format="%d %b %Y %H:%M:%S")
date_2 = as.POSIXct(tail(b03$Date, n=1),  format="%d %b %Y %H:%M:%S")
b03_10 <- round(difftime(date_2,date_1, units="days"),2) # 39.55 days
# 11. Number of data points in Batch 01 # I know time is unique from earlier check of duplicates.
b03_11 <- length(unique(b03$Date)) # 68

batch_03_summary <- c(b03_1, b03_2, b03_3, b03_4, b03_5, b03_6,
                      b03_7, b03_8, b03_9, b03_10, b03_11)

# clean workspace
rm(date_1,date_2,b03_1, b03_2, b03_3, b03_4, b03_5, b03_6,
   b03_7, b03_8, b03_9, b03_10, b03_11)

# ---------------------------------------------
# 6.4 Create a table data_frame
# ---------------------------------------------

# List of Summary Statistics
Statistics <- c("Maximum pH", "Minimum pH", "Mean pH", "Median pH", "Standard Deviation of pH",
              "Initial pH", "Final pH", "Start Time/Date", "End Time/Date", "Number of days batch avtive",
              "Number of data points")

# Create a data frame to use as a table and add the batch 01 and batch 02 summary details
table <- as.data.frame(Statistics)
table$Batch01 <- batch_01_summary
table$Batch02 <- batch_02_summary
table$Batch03 <- batch_03_summary

# clean workspace
rm(batch_01_summary, batch_02_summary,batch_03_summary, Statistics)


# ---------------------------------------------
# 6.5 Table
# ---------------------------------------------

# create table
table %>%
  kbl(caption = "Summary Statistics for Batches") %>%
  kable_classic(full_width = F, html_font = "Cambria")

View(table)
#clean workspace
# rm(table)

# drop working batch data frames
rm(b01,b02,b03)


# -----------------------------------------------------------------------------
# 7. Create timeseries plots of pH, Biomass, Titre and Controlling DO. This 
# should be done with either the Date or time active on the x-axis. A reader should 
# be able to quickly see the batch evolution from this plot also.

# -----------------------------------------------------------------------------

# Controlling DO swiches on the row 94 and data 07 Apr 2020 11:09:04

# ---------------------------------------------
# 7.1. One Time series that shows all four together but you lose scale on some.
# ---------------------------------------------


# time series with 4 columns divided by batch type
ggplot(data, aes(x = Date, color=factor(Media.Batch))) +
  geom_line( aes(y = pH)) +
  geom_line( aes(y = Biomass)) +
  geom_line( aes(y = Titre..mg.mL.)) +
  geom_line( aes(y = Controlling.DO)) +
  ggtitle("Time Series of pH, Titre, Controlling DO, Biomass ") +
  labs(x = "Time line", y=" pH   /   Titre   /   Controlling.DO   /   Biomass ") +
  labs(color = "Batches")  +
  theme_bw() +
  geom_vline(xintercept = as.POSIXct("2020-04-07 11:09:04"),linetype = "dashed", color = "blue") +
  geom_vline(xintercept = as.POSIXct("2020-05-24 12:26:09"),linetype = "dotted", color = "red") +
  geom_vline(xintercept = as.POSIXct("2020-06-06 21:02:32"),linetype = "dotted", color = "red")
  
# Good for an overall view of the four columns but has its disadvantages.
# As Biomass has the largest range and values it overshadows the other columns.
# In this plot, the pH looks quite flat but as we will see below when we plot each
# out indivdually, this is not the case at all.

# THe blue dashed line shows when the controlling Do changes between DO1 and Do2.
# The two red dotted lines show the large amount of data points missing. which is dicussed else where
# in the report.

# HOwever it is clear to see the three difference batches.

# ---------------------------------------------
# 7.2. Time Series for pH
# ---------------------------------------------

# plot pH
ggplot(data, aes(x = Date, color=factor(Media.Batch))) +
  geom_line( aes(y = pH)) +
  ggtitle("Time Series of pH") +
  labs(x = "Time line", y=" pH") +
  labs(color = "Batches")  +
  theme_bw() +
  geom_line(aes(y=cummean(pH), colour="Mean of pH")) +
  geom_vline(xintercept = as.POSIXct("2020-04-07 11:09:04"),linetype = "dashed", color = "blue") +
  geom_vline(xintercept = as.POSIXct("2020-05-24 12:26:09"),linetype = "dotted", color = "red") +
  geom_vline(xintercept = as.POSIXct("2020-06-06 21:02:32"),linetype = "dotted", color = "red")


# Is this data normally distributed?
boxplot(data$pH)
skewness(data$pH) # 0.0561965
# boxplot and skewnes value show a very small skew. So distibution is fairly symmetric.


# Summary of the pH data over time
table_total_pH <- data  %>% summarise(Min=min(pH),
                      Max= max(pH), 
                     Mean = mean(pH), 
                     Median = median(pH), 
                     "Standard Deviation" = round(sd(pH),2),
                     "IQR Range" = IQR(pH))

# make a table of the above with ktable
table_total_pH %>%
  kbl(caption = "Summary Statistics for pH") %>%
  kable_classic(full_width = F, html_font = "Cambria")
                    
# ---------------------------------------------
# 7.3. Time Series for Biomass
# ---------------------------------------------
# plot biomass
ggplot(data, aes(x = Date, color=factor(Media.Batch))) +
  geom_line( aes(y = Biomass)) +
  ggtitle("Time Series of Biomass") +
  labs(x = "Time line", y=" Biomass") +
  labs(color = "Batches")  +
  theme_bw() +
  geom_line(aes(y=cummean(Biomass), colour="Mean of Biomass"))+
  geom_vline(xintercept = as.POSIXct("2020-04-07 11:09:04"),linetype = "dashed", color = "blue") +
  geom_vline(xintercept = as.POSIXct("2020-05-24 12:26:09"),linetype = "dotted", color = "red") +
  geom_vline(xintercept = as.POSIXct("2020-06-06 21:02:32"),linetype = "dotted", color = "red")


# Is this data normally distributed?
# boxplot(data$Biomass)
skewness(data$Biomass) # 0.01786147

# Summary of the Biomass data over time
table_total_Biomass <- data  %>% summarise(Min=min(Biomass),
                                      Max= max(Biomass), 
                                      Mean = mean(Biomass), 
                                      Median = median(Biomass), 
                                      "Standard Deviation" = round(sd(Biomass),2),
                                      "IQR Range" = IQR(Biomass))



# make a table of the above with ktable
table_total_Biomass %>%
  kbl(caption = "Summary Statistics for Biomass") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# ---------------------------------------------
# 7.4. Time Series for Titre
# ---------------------------------------------

ggplot(data, aes(x = Date, color=factor(Media.Batch))) +
  geom_line( aes(y = Titre..mg.mL.)) +
  ggtitle("Time Series of Titre") +
  labs(x = "Time line", y=" Titre") +
  labs(color = "Batches")  +
  theme_bw() +
  geom_line(aes(y=cummean(Titre..mg.mL.), colour="Mean of Titre")) +
  geom_vline(xintercept = as.POSIXct("2020-04-07 11:09:04"),linetype = "dashed", color = "blue") +
  geom_vline(xintercept = as.POSIXct("2020-05-24 12:26:09"),linetype = "dotted", color = "red") +
  geom_vline(xintercept = as.POSIXct("2020-06-06 21:02:32"),linetype = "dotted", color = "red")


# Summary of the Titre data over time
table_total_Titre <- data  %>% summarise(Min=min(Titre..mg.mL.),
                                           Max= max(Titre..mg.mL.), 
                                           Mean = mean(Titre..mg.mL.), 
                                           Median = median(Titre..mg.mL.), 
                                           "Standard Deviation" = round(sd(Titre..mg.mL.),2),
                                           "IQR Range" = IQR(Titre..mg.mL.))

# make a table of the above with ktable
table_total_Titre %>%
  kbl(caption = "Summary Statistics for Titre") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Is this data normally distributed?
# boxplot(data$Titre..mg.mL.)
skewness(data$Titre..mg.mL.) # -0.08064536
# boxplot and skewness value show a very small skew. So distribution is fairly symmetric.

# ---------------------------------------------
# 7.5. Time Series for Controlling DO
# ---------------------------------------------

ggplot(data, aes(x = Date, color=factor(Media.Batch))) +
  geom_line( aes(y = Controlling.DO)) +
  ggtitle("Time Series of Controlling DO") +
  labs(x = "Time line", y=" Controlling.DO") +
  labs(color = "Batches")  +
  theme_bw() +
  geom_line(aes(y=cummean(Controlling.DO),colour="Mean of Controlling DO"))+
  geom_vline(xintercept = as.POSIXct("2020-04-07 11:09:04"),linetype = "dashed", color = "blue") +
  geom_vline(xintercept = as.POSIXct("2020-05-24 12:26:09"),linetype = "dotted", color = "red") +
  geom_vline(xintercept = as.POSIXct("2020-06-06 21:02:32"),linetype = "dotted", color = "red")
 
# Is this data normally distributed?
# boxplot(data$Controlling.DO) # looks evenly distruibuted
skewness(data$Controlling.DO) # -0.2754178

 
# Summary of the Titre data over time
table_total_controllong_do <- data  %>% summarise(Min=min(Controlling.DO),
                                         Max= max(Controlling.DO), 
                                         Mean = mean(Controlling.DO), 
                                         Median = median(Controlling.DO), 
                                         "Standard Deviation" = round(sd(Controlling.DO),2),
                                         "IQR Range" = IQR(Controlling.DO))

# make a table of the above with ktable
table_total_controllong_do %>%
  kbl(caption = "Summary Statistics for Controlling.DO") %>%
  kable_classic(full_width = F, html_font = "Cambria")


# clean up workspace
rm(table, table_total_Biomass, table_total_controllong_do, table_total_pH, table_total_Titre)

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# 8. Extend the code from step 7 to create an animated timeseries plot of Controlling DO, where
# the timeseries is revealed over several seconds. 

# The evolving date and time active in minutes should be shown in the plot title. 

# This animation should have a resolution of at least 20
# frames per second and should last no shorter than 20 seconds. Save this animation as a gif
# and submit this with your code file.
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# Animation for the Controlling Do
anim <- ggplot(data, aes(x = Date,color=factor(Media.Batch))) +
  geom_line( aes(y = Controlling.DO))+
  labs(title="Time Series to show Controlling DO with Batches",
    subtitle = 'Date: {format(frame_along, "%Y %b %d %H:%M")}', x = 'Timeline', y = 'Controlling DO',
    caption="Blue line: Controlling DO changes from DO1 to DO2",) +
  transition_reveal(Date) +
  labs(color = "Batches") +
  theme_bw() +
  geom_vline(xintercept = as.POSIXct("2020-04-07 11:09:04"),linetype = "dashed", color = "blue")

# Animate the code
animate(anim, nframe=1000, duration=30, fps=30)

# save the animation
anim_save("Animation_ControllingDO.gif")

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# 9. Create appropriate visualizations of the effect of Batch number, Base Buffer and Media Batch
# on Titre. Give a brief discussion of these plots and any effect these variables may have
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# ---------------------------------------------
# 9.1 plot titre (num) on base butter (categorical)
# ---------------------------------------------
ggplot(data, aes(y = Titre..mg.mL., x=Base.Buffer.Factor)) +
  geom_boxplot(alpha = 0.1) +
  geom_jitter(color="red", size=0.4, alpha=0.9) +
  ggtitle("Boxplot showing Titre by each Base Buffer") +
  labs(x = "Base Buffer", y=" Titre ") +
  theme_bw()
# you can see the data points in red, fairly evenly distributed (we will check that below)
# batch 01 has the most points, with batch 02 having the least

#Number of data points in each.
data %>% tally(Base.Buffer.Factor == 1) # 367 data points
data %>% tally(Base.Buffer.Factor == 2) # 303 data points

# skewness test
skewness(data$Base.Buffer.Factor == 1) #-0.19
skewness(data$Base.Buffer.Factor == 2) # 0.19 

# get the min, max and median for group 1 and group 2
table_bf_1 <- data %>% 
  filter(Base.Buffer.Factor == 1)  %>%
  summarise(Min= min(Titre..mg.mL.), max = max(Titre..mg.mL.),
            median = median(Titre..mg.mL.), IQR = IQR(Titre..mg.mL.))

table_bf_2 <- data %>% 
  filter(Base.Buffer.Factor == 2)  %>%
  summarise(Min= min(Titre..mg.mL.), max = max(Titre..mg.mL.), 
            median = median(Titre..mg.mL.),, IQR = IQR(Titre..mg.mL.))

# make a table of the above with ktable
table_bf_1 %>%
  kbl(caption = "Statistics for Base Buffer 1") %>%
  kable_classic(full_width = F, html_font = "Cambria")

table_bf_2 %>%
  kbl(caption = "Statistics for Base Buffer 2") %>%
  kable_classic(full_width = F, html_font = "Cambria")


# T tests - compare mean for two groups
# Not fully learned how to interrupt these results yet so have not included
# them in the report. We are just learning about them in the stats module.

# ---------------------------------------------
# 9.2 plot titre (num) on Media Batch (Categorical)
# ---------------------------------------------

# plot titire by each bathcer
ggplot(data, aes(y = Titre..mg.mL., x=Media.Batch.Factor))+
  geom_boxplot(alpha=0.1) +
  geom_jitter(color="blue", size=0.4, alpha=0.9) +
  ggtitle("Boxplot showing Titre by each Media Batch") +
  labs(x = " Media Batch Number", y=" Titre ") +
  theme_bw()


# count of the data points in each group
data %>% tally(Media.Batch == 202201) # 382 data points
data %>% tally(Media.Batch == 202202) # 68 data points
data %>% tally(Media.Batch == 202203) # 220 data points

# skewness test
skewness(data$Media.Batch == 202201) # -0.28
skewness(data$Media.Batch == 202202) # 2.6
skewness(data$Media.Batch == 202203) # 0.73

# get the min, max and median for batches 01,02,03
table_bf_3 <- data %>% 
  filter(Media.Batch == 202201)  %>%
  summarise(Min= min(Titre..mg.mL.), max = max(Titre..mg.mL.),
            median = median(Titre..mg.mL.), IQR = IQR(Titre..mg.mL.))

table_bf_4 <- data %>% 
  filter(Media.Batch == 202202)  %>%
  summarise(Min= min(Titre..mg.mL.), max = max(Titre..mg.mL.), 
            median = median(Titre..mg.mL.),, IQR = IQR(Titre..mg.mL.))

table_bf_5 <- data %>% 
  filter(Media.Batch == 202203)  %>%
  summarise(Min= min(Titre..mg.mL.), max = max(Titre..mg.mL.), 
            median = median(Titre..mg.mL.),, IQR = IQR(Titre..mg.mL.))

# make a table of the above with ktable
table_bf_3 %>%
  kbl(caption = "Statistics for Batch 01") %>%
  kable_classic(full_width = F, html_font = "Cambria")

table_bf_4 %>%
  kbl(caption = "Statistics for Batch 02") %>%
  kable_classic(full_width = F, html_font = "Cambria")

table_bf_5 %>%
  kbl(caption = "Statistics for Batch 03") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# F tests - for more than 2 groups.
# Not fully learned how to interrupt these results yet so have not included
# them in the report. We are just learning about them in the stats module.

# clean workspace
rm(table_bf_1, table_bf_2, table_bf_3, table_bf_4, table_bf_5)

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# 10. Use appropriate modelling techniques to analyse the relationship between Titre and the
# other variables. You should clearly state if any variable impacts Titre. 
# read book
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# ---------------------------------------------
# 10.1 Correlation between Titre and Biomass
# ---------------------------------------------

# Do pairs of just numeric columns
ggpairs(data[,2:7], title="Correlation of all Numeric Columns",
        upper = list(continuous = wrap('cor', size = 3)),
        lower = list(continuous = wrap("points", alpha = 0.2,    size=0.1))) +
  theme(axis.text.y = element_blank()) +
  theme(axis.text.x = element_blank())
        
# We have seen this before in a more general form.
# this time we are looking at the numeric columns and their affect on Titre.
# At first glance Biomass looks like it is strongly correlated with three *** and
# a value of 0.733. We should expect Biomass to have an effect on our model later.
# The other columns do not look like them have much of a correlation.
# The Controlling DO has a value of -0.079 but also has one star.
# This is interesting and we will investage this some more.

# ---------------------------------------------
# 10.2 Heat map of correlation
# ---------------------------------------------
# Another way is to do a heatmap and look at the correlation of the data
# in a different way.

# Create a correlation matrix of all the data.
cor_data <- round(cor(data[,2:9]),2) # remove date columnn # round data for easier interruption
# melt function takes in a data in wide format and puts into a single column
melted_cor <- melt(cor_data)
cor_data_melt <- melt(cor_data)

# get a heat map to visual show the correlations
ggplot(data=cor_data_melt, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  ggtitle("Correlation of all columns") +
  scale_fill_gradient2(low="blue", high="red", limit =c(-1,1),name="Correlation")+
  geom_text(aes(Var1, Var2, label=value)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) 

# As we say, there is strong correlations between the Controlling Do and DO1 and DO2
# Also between Biomass and Titre
# Low correlation between media. Batch and base buffer and DO1
# bettween Base Buffer and Media Batch themselves.

# The correlation coefficient between Tritre and Biomass is high
# Titre has does not have a high or low result for anything else.

# clean workspace
rm(melted_cor, cor_data, cor_data_melt)

# ---------------------------------------------
# 10.2 Check Titre against all the numeric columns
# ---------------------------------------------

# Do a Linear Regission model of the numeric column on the Y variable Titre
# We should expect from the above plots for Biomass to have the biggest influence.
num_lm_v0 <- lm(formula = Titre..mg.mL.~DO1 + DO2 + Controlling.DO + 
     pH + Biomass, data=data)

summary(num_lm_v0)
# Only Biomass has a strong corrleation and the rest can be dropped.
# R squared: 0.5393

#Improve on LM model by removing everything but the Biomass and Controlling.DO
num_lm_v1 <- lm(formula = Titre..mg.mL.~Biomass + Controlling.DO , data=data)
summary(num_lm_v1)

# R squared: 0.5389

# I left in the Controlling DO as I wanted to look to see if it could be used to
# improve the model. While it did the improvement was tiny so we will remove it 
# for the last model and only include Biomass.


#Improve on LM model by removing everything but the Biomass
num_lm_v2 <- lm(formula = Titre..mg.mL.~Biomass , data=data)
summary(num_lm_v2)

# R squared: 0.5367

# For the third Linear Regression model we have only used the one predictor, Biomass.
# There is only a 0.0026 drop in the R square value.
# This is a very good results as by dropping 4 predictors we have had a very small
# change in the R squared value but we have a much similar model.
# A similar model is always a good thing.

# ---------------------------------------------
# 10.3 Plot a scatter plot of Titre by Biomass.
# ---------------------------------------------
# plot Titre and Biomass
ggplot(data, aes(y=Titre..mg.mL., x=Biomass)) +
  geom_point(size=1.5, shape=20) +
  ggtitle("Scatter Plot of Titre and Biomass") +
  geom_smooth(method=lm, level=0.50) +
  theme_bw()

# In a graph on its own we can see that we can use Biomass to predicti Titre
# This is also shown in our Linear Regression model above.
# THe blue line shows the linear regression model with the shadew area given as 
# a 95 % Confidence Interval (by default)

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------


