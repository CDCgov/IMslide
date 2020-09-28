##########PLOT of case counts and death counts for COVID19 (epicurve) - Authors Jenna Hamlin (ptx4@cdc.gov) and Marcela Torres (ote7@cdc.gov) 

#install.packages("magrittr")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("lubridate")
#install.packages("scales")
#install.packages("cowplot")
#install.packages("gridExtra")
#install.packages("grid")
#install.packages("xlsx")
#install.packages("zoo")
#install.packages("here")

#load libraries
library(magrittr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(cowplot)
library(gridExtra)
library(grid)
library(xlsx)
library(zoo)
library(here)

#use package here so not need to use whole path of file location 
#read in data and convert columns Total\nCases to Total.cases, dates to as.Date, etc
dataUp<- read.csv(here::here("raw.data", "Cases+and+Deaths+Counts+for+IM+Slide 20200716.csv")) 

names(dataUp)[1]<-"Date"

dataUp$Date <-lubridate::mdy(dataUp$Date)
names(dataUp)[2]<-"Total.Cases"
names(dataUp)[3]<-"New.Cases"
names(dataUp)[4]<-"Total.Death.Cases"
names(dataUp)[5]<-"New.Deaths"

########## FUNCTIONS FOR PLOTTING
#moveingAverage function from http://www.cookbook-r.com/Manipulating_data/Calculating_a_moving_average/ 
movingAverage <- function(x, n=1, centered=FALSE) {
  
  if (centered) {
    before <- floor  ((n-1)/2)
    after  <- ceiling((n-1)/2)
  } else {
    before <- n-1
    after  <- 0
  }
  
  
  # Track the sum and count of number of non-NA items
  s     <- rep(0, length(x))
  count <- rep(0, length(x))
  
  # Add the centered data 
  new <- x
  # Add to count list wherever there isn't a 
  count <- count + !is.na(new)
  # Now replace NA_s with 0_s and add to total
  new[is.na(new)] <- 0
  s <- s + new
  
  # Add the data from before
  i <- 1
  while (i <= before) {
    # This is the vector with offset values to add
    new   <- c(rep(NA, i), x[1:(length(x)-i)])
    
    count <- count + !is.na(new)
    new[is.na(new)] <- 0
    s <- s + new
    
    i <- i+1
  }
  
  # Add the data from after
  i <- 1
  while (i <= after) {
    # This is the vector with offset values to add
    new   <- c(x[(i+1):length(x)], rep(NA, i))
    
    count <- count + !is.na(new)
    new[is.na(new)] <- 0
    s <- s + new
    
    i <- i+1
  }
  
  # return sum divided by count
  s/count
}

#theme function to hold same calls and apply to each plot
theme_my_own <- function(){
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+
    theme(legend.position="top", legend.title = element_blank())+
    theme(axis.text.x = element_text(colour="black",angle = 45, vjust = 0.5, hjust = 0.5,  family = "sans"))+
    theme(axis.text.y = element_text(colour="black", family = "sans"))+
    theme(text = element_text(size=14))+
    theme(legend.position = "None" )+ 
    theme(axis.line = element_line(colour = "black", linetype = "solid"), axis.text = element_text(size=14))
}

###Count Plot calculations
#change the way the Date column appears from M,D,Y to Y,M,D 
#calculate the seven day moving average of counts and add 
#it as a column to the dataframe
dataUp<- dataUp %>%
  mutate(movAveCount7Count = movingAverage(dataUp$New.Cases, 7, centered = FALSE)) %>%
  mutate(pctChangeNewCount = (New.Cases/(Total.Cases - New.Cases)*100)) 

#calculate the seven day moving average of % daily changes in counts
#the second mutate essentially converts values for days occur on or
#before March 10 2020, to NA, which then can be ingnored when plotting.
#This date was chosen baesd on previously accepted plots done in excel
dataUp<- dataUp %>%
  mutate(percentMovAve7Count = movingAverage(dataUp$pctChangeNewCount, 7, centered = FALSE)) %>%
  mutate(percentMovAve7Count = ifelse(Date <= ("2020-03-10"), NA,  percentMovAve7Count))

###Death Plot calculations same as above except for death counts
dataUp<- dataUp %>%
  mutate(movAveCount7Death = movingAverage(dataUp$New.Deaths, 7, centered = FALSE)) %>%
  mutate(pctChangeNewDeath = (New.Deaths/(Total.Death.Cases - New.Deaths)*100)) 

###Death Plot calculations same as above except for death counts
dataUp<- dataUp %>%
  mutate(percentMovAve7Death = movingAverage(dataUp$pctChangeNewDeath, 7, centered = FALSE)) %>%
  mutate(percentMovAve7Death = ifelse(Date <= as.Date("2020-03-10"), NA,  percentMovAve7Death))

#new metric - which is the (sum(current 7 days) - sum(past 7 days))/(sum(past 7 days))*100
dataUp$CasesCurrentWeek <- rollapply(dataUp$New.Cases,7, sum, by = 1, fill = NA, align = "right") 
dataUp$CasesPrevious2Weeks <- rollapply(dataUp$New.Cases,14, sum, by = 1, fill = NA, align = "right")
dataUp$DeathsCurrentWeek <- rollapply(dataUp$New.Deaths,7, sum, by = 1, fill = NA, align = "right") 
dataUp$DeathsPrevious2Weeks <- rollapply(dataUp$New.Deaths,14, sum, by = 1, fill = NA, align = "right") 

dataUp$CasesPercentChange <- round(((dataUp$CasesCurrentWeek - (dataUp$CasesPrevious2Weeks-dataUp$CasesCurrentWeek))/(dataUp$CasesPrevious2Weeks-dataUp$CasesCurrentWeek)*100), digits=0)
dataUp$DeathsPercentChange <- round(((dataUp$DeathsCurrentWeek - (dataUp$DeathsPrevious2Weeks-dataUp$DeathsCurrentWeek))/(dataUp$DeathsPrevious2Weeks-dataUp$DeathsCurrentWeek)*100), digits=0)

#save the updated file with all values as an excel spreadsheet as task force lead requests that this be done 
wb<-createWorkbook(type="xlsx")

CellStyle(wb, dataFormat=NULL, alignment = NULL,
          border=NULL, fill=NULL, font=NULL)

# Styles for the data table row/column names
TABLE_COLNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE) +
  Alignment(wrapText=FALSE, horizontal="ALIGN_CENTER") +
  Border(color="black", position=c("TOP", "BOTTOM"), 
         pen=c("BORDER_THIN", "BORDER_THIN")) 

#name the sheet 
sheet <- createSheet(wb, sheetName = "Case Counts and Deaths")

#add dataframe called dataUp and exclude row names
addDataFrame(dataUp, sheet, startRow=1, startColumn=1, row.names = FALSE, 
             colnamesStyle = TABLE_COLNAMES_STYLE)

# Change column width
setColumnWidth(sheet, colIndex=c(1:ncol(dataUp)), colWidth=21)

#save the workbook - this takes a bit of time to run. 
saveWorkbook(wb, here::here("updated.data", "IMslide_NEWtemplate_StateCounts_14July2020test-Slide1Updates.xlsx"))

#conversion scale to add the second line to the plot
caseScale <- 1000 #should this be 1000
deathScale <- 100

#convert dates to bimonthly for display and make sure to only include max date with data
#https://community.rstudio.com/t/scale-x-date-date-breaks-1st-and-15th-of-the-month/906/3
#change breaks to bimontly if you want that instead of by week. replace as bimonthly not 'bimontly'

bimonthly <- function(x) {
  x_range <- range(x, na.rm = TRUE)

  date_range <- c(
    floor_date(x_range[1], "month"),
    ceiling_date(x_range[2], "month")
  )
  monthly <- seq(date_range[1], date_range[2], by = "1 month")

  sort(c(monthly, monthly + days(14)))
}


#captions - one varies ther x and y coordinates here to move the text around
#as of July 4 - these are okay given the size we save the image using ggsave 
captionprobdeaths1<- textGrob("On 4/15, some states began \n reporting probable deaths.\n 4,062 of 6,489 reported deaths\n were probable.",
                                        x=0.45,  y=0.90, hjust=0,gp=gpar(col="black", fontsize=11, fontface="italic", sep = "\n"))

captionprobdeaths2<- textGrob("On 6/25, one state began \n reporting probable deaths.\n 1,824 of 2,516 reported\n deaths were probable.",
                              x=0.6,  y=0.7, hjust=0,gp=gpar(col="black", fontsize=11, fontface="italic", sep = "\n"))

captionprobdeaths2slide2<- textGrob("Removed 1,824 probable \n deaths from the deaths\n plotted on 6/25.",
                              x=0.75,  y=0.7, hjust=0,gp=gpar(col="black", fontsize=11, fontface="italic", sep = "\n"))


##########PLOTS
#bar chart plotted with x as date, and y as new cases, 
#%b represents the abbreviated month which will be plotted as labels on the x-axis.
#to plot two lines on different y axis - see here:
#https://community.rstudio.com/t/assign-2-geom-lines-to-different-y-axis/14797/5    
#dealing with the legend - see here:
#https://stackoverflow.com/questions/17148679/construct-a-manual-legend-for-a-complicated-plot


#dealing with the legend issue - see here https://stackoverflow.com/questions/62717954/make-a-common-legend-and-include-unique-legend-attributes/62718831#62718831
#one suggestion was to build a dummy plot to use the cowplot get.legend function.This is what is shown below
#a more elegent solution would probably turn the dataframe from wide to long format and then provide an id column
#with case count and death count id. I haven't done this yet

#create a dummy dataframe
df <- data.frame(class = c("New Cases","Moving Average Count - 7 days",
                           "% Change between total counts from most recent 7 days and total count from previous 7 days", "New Deaths"),
                 w = 1:2,
                 x = 2:3,
                 y = 3:4,
                 z = 4:5)

#create dummy plot just to extract the desired legend
p1<- ggplot(df, aes(x=x,y=y)) + 
  geom_col(aes(fill = class))+
  scale_fill_manual(breaks = c("New Cases", "Moving Average Count - 7 days","% Change between total counts from most recent 7 days and total count from previous 7 days", "New Deaths"),
    values = c("#a19c9c","orange", "gold", "dodgerblue4")) +
  theme(legend.title = element_blank())

###Count plot
#if they want the column percentMovAverageCount just change the variable in the second geom_line (e.g.CasesPercentChange to percentMovAverageCount)
countSlide <-
  dataUp%>%
  ggplot(aes(x= as.Date(Date), y=New.Cases)) +
  geom_bar(stat="identity", aes(colour = "New Cases"), fill = "#a19c9c")+
  scale_x_date(breaks = bimonthly, date_labels = "%b-%d", expand = c(0, 0)) +
  geom_line(aes(y = movAveCount7Count, colour = "Moving Average Count - 7 days"), size =1.5)+
  geom_line(aes(x = as.Date(Date), y = CasesPercentChange*caseScale, colour = "% Change between total counts from most recent 7 days and total count from previous 7 days"), size = 1.5)+
  scale_y_continuous(labels = scales::comma,
                     sec.axis = sec_axis(~ ./caseScale/100, labels = scales::percent_format(accuracy=1)),  expand = c(0.4, 0) )+
  coord_cartesian(ylim = c(0, 70000))+
  ylab("") +
  xlab("")+
  theme_my_own()+
  scale_colour_manual(
    breaks = c("New Cases","Moving Average Count - 7 days", "% Change between total counts from most recent 7 days and total count from previous 7 days"),
    values = c("#a19c9c", "orange", "gold"),
    guide = guide_legend(override.aes = list(
      border=c(NA, NA, NA), 
      fill=c("#a19c9c", "orange", "gold"))))+
  geom_hline(yintercept = 0, linetype = "dotted", size = 1.5)


###Death Plot
deathSlide <-
  dataUp%>%
  ggplot(aes(x=as.Date(Date), y=New.Deaths)) +
  geom_bar(stat="identity", aes(colour = "New Deaths"), fill = "dodgerblue4")+
  annotation_custom(captionprobdeaths1)+
  annotation_custom(captionprobdeaths2)+
  scale_x_date(breaks = bimonthly, date_labels = "%b-%d", expand = c(0, 0)) +
  geom_line(aes(y = movAveCount7Death, colour = "Moving Average Count - 7 days"), size =1.5)+
  geom_line(aes(y = DeathsPercentChange*deathScale, colour = "% Change between total counts from most recent 7 days and total count from previous 7 days"), size = 1.5)+
  scale_y_continuous(labels = scales::comma,
                     sec.axis = sec_axis(~ ./deathScale/100, labels = scales::percent_format(accuracy=1)),  expand = c(0.8, 0))+
  coord_cartesian(ylim = c(0, 4000))+
  ylab("") +
  xlab("")+
  theme_my_own()+
  scale_colour_manual(
    breaks = c("New Deaths","Moving Average Count - 7 days", "% Change between total counts from most recent 7 days and total count from previous 7 days"),
    values = c("dodgerblue4", "orange", "gold"),
    guide = guide_legend(override.aes = list(
      border=c(NA, NA, NA), 
      fill=c("dodgerblue4", "orange", "gold"))))+
  geom_hline(yintercept = 0, linetype = "dotted", size = 1.5)


#use cowplot to combine plots and the get legend fucntion to extract the combined legend from dummy plot
#sometimes cowplot can be slow to run especially on the VPN

combinedPlot <- cowplot::plot_grid(
  cowplot::plot_grid(
    countSlide,
    deathSlide,
    align = 'h'),
  # Add the legend of the dummy plot 
  cowplot::get_legend(p1 +  
                        theme(legend.text = element_text(size = 16))+
                        theme(legend.position = c(.5, 1), legend.direction = "horizontal")),
                        
  nrow = 2, rel_heights = c(4, 1))

#use ggsave to save the plot. This will overwrite the plot in the file. If you don't want to overwrite the previous plot
#then give it a unique id like something to do with the date. 
ggsave(here::here("images", "combinedPlot.July14.newMetric.slide1test.png"), combinedPlot, height = 8.5, width  = 18.00, units = "in")

###Death Plot without probable deaths reported by NJ on June 25

#reassign the value for 06.25.2020 to not include the probably deaths (n = 1824)
dataUp2 <- dataUp

dataUp2$New.Deaths[122]<-692

###Count Plot calculations
#change the way the Date column appears from M,D,Y to Y,M,D 
#calculate the seven day moving average of counts and add 
#it as a column to the dataframe
dataUp2<- dataUp2 %>%
  mutate(movAveCount7Count = movingAverage(dataUp$New.Cases, 7, centered = FALSE)) %>%
  mutate(pctChangeNewCount = (New.Cases/(Total.Cases - New.Cases)*100)) 

#calculate the seven day moving average of % daily changes in counts
#the second mutate essentially converts values for days occur on or
#before March 10 2020, to NA, which then can be ingnored when plotting.
#This date was chosen baesd on previously accepted plots done in excel
dataUp2<- dataUp2 %>%
  mutate(percentMovAve7Count = movingAverage(dataUp$pctChangeNewCount, 7, centered = FALSE)) %>%
  mutate(percentMovAve7Count = ifelse(Date <= as.Date("2020-03-10"), NA,  percentMovAve7Count))

###Death Plot calculations same as above except for death counts
dataUp2 <- dataUp2 %>%
  mutate(movAveCount7Death = movingAverage(dataUp$New.Deaths, 7, centered = FALSE)) %>%
  mutate(pctChangeNewDeath = (New.Deaths/(Total.Death.Cases - New.Deaths)*100)) 

###Death Plot calculations same as above except for death counts
dataUp2 <- dataUp2 %>%
  mutate(percentMovAve7Death = movingAverage(dataUp$pctChangeNewDeath, 7, centered = FALSE)) %>%
  mutate(percentMovAve7Death = ifelse(Date <= as.Date("2020-03-10"), NA,  percentMovAve7Death))


#new metric - which is the (sum(current 7 days) - sum(past 7 days))/(sum(past 7 days))*100
dataUp2$CasesCurrentWeek <- rollapply(dataUp2$New.Cases,7, sum, by = 1, fill = NA, align = "right") 
dataUp2$CasesPrevious2Weeks <- rollapply(dataUp2$New.Cases,14, sum, by = 1, fill = NA, align = "right")
dataUp2$DeathsCurrentWeek <- rollapply(dataUp2$New.Deaths,7, sum, by = 1, fill = NA, align = "right") 
dataUp2$DeathsPrevious2Weeks <- rollapply(dataUp2$New.Deaths,14, sum, by = 1, fill = NA, align = "right") 

dataUp2$CasesPercentChange <- round(((dataUp2$CasesCurrentWeek - (dataUp2$CasesPrevious2Weeks-dataUp2$CasesCurrentWeek))/(dataUp2$CasesPrevious2Weeks-dataUp2$CasesCurrentWeek)*100), digits=0)
dataUp2$DeathsPercentChange <- round(((dataUp2$DeathsCurrentWeek - (dataUp2$DeathsPrevious2Weeks-dataUp2$DeathsCurrentWeek))/(dataUp2$DeathsPrevious2Weeks-dataUp2$DeathsCurrentWeek)*100), digits=0)

#plot the deathslide without the probably deaths. 

deathSlide2 <-
  dataUp2%>%
  ggplot(aes(x=Date, y=New.Deaths)) +
  geom_bar(stat="identity", aes(colour = "New Deaths"), fill = "dodgerblue4")+
  annotation_custom(captionprobdeaths1)+
  annotation_custom(captionprobdeaths2)+
  scale_x_date(breaks = bimonthly, date_labels = "%b-%d", expand = c(0, 0)) +
  geom_line(aes(y = movAveCount7Death, colour = "Moving Average Count - 7 days"), size =1.5)+
  geom_line(aes(y = DeathsPercentChange*deathScale, colour = "% Change between total counts from most recent 7 days and total count from previous 7 days"), size = 1.5)+
  scale_y_continuous(labels = scales::comma,
                     sec.axis = sec_axis(~ ./deathScale/100, labels = scales::percent_format(accuracy=1)),  expand = c(0.8, 0))+
  coord_cartesian(ylim = c(0, 4000))+
  ylab("") +
  xlab("")+
  theme_my_own()+
  scale_colour_manual(
    breaks = c("New Deaths","Moving Average Count - 7 days", "% Change between total counts from most recent 7 days and total count from previous 7 days"),
    values = c("dodgerblue4", "orange", "gold"),
    guide = guide_legend(override.aes = list(
      border=c(NA, NA, NA), 
      fill=c("dodgerblue4", "orange", "gold"))))+
  geom_hline(yintercept = 0, linetype = "dotted", size = 1.5)

#use cowplot to combine plots and the get legend fucntion to extract the combined legend from dummy plot
#sometimes cowplot can be slow to run especially on the VPN

combinedPlot <- cowplot::plot_grid(
  cowplot::plot_grid(
    countSlide,
    deathSlide2,
    align = 'h'),
  # Add the legend of the dummy plot 
  cowplot::get_legend(p1 +  
                        theme(legend.text = element_text(size = 16))+
                        theme(legend.position = c(.5, 1), legend.direction = "horizontal")),
  
  nrow = 2, rel_heights = c(4, 1))

#use ggsave to save the plot. This will overwrite the plot in the file. If you don't want to overwrite the previous plot
#then give it a unique id like something to do with the date. 
ggsave(here::here("images", "combinedPlot.July14.newMeteric.slide2test.png"), combinedPlot, height = 8.5, width  = 18.00, units = "in")

###GET DAY OF PERCENT CHANGE
#Percent change of cases for day of calculated as % Change between total counts from most recent 7 days and total count from previous 7 days
tail(dataUp$CasesPercentChange, 1)

#Count Range Percent Change
round(tail(dataUp$CasesPercentChange, 7), digits = 1)

#Percent change of deaths for day of calculated as % Change between total counts from most recent 7 days and total count from previous 7 days
tail(dataUp$DeathsPercentChange, 1)

#Death Range Percent Change
round(tail(dataUp$DeathsPercentChange, 7), digits = 1)

##########CHECKS FOR CONVERSIONS - when getting the conversion scale to allow for a secondary y axis; 
#I plot one geom_line with the bar plot and then the other geom_line as a seperate plot to see what 
#the difference is between the lines (i.e. conversion)
#check for second line and scale conversion
#p2 <- dataUp%>%
#  ggplot()+
#  geom_line(aes(Date, CasesPercentChange), colour = "gold", size = 1.5)

##########FONTS AVAILABLE ON WINDOWS MACHINE - info from here can be used in the theme function
#accessing which family of font is available and how to define the particular use of a certain font
#this is here just in case one wants to use something different than the sans font but i think this 
#is the best given what is available without having to download a new package.
#wf <- windowFont()
#names(wf[wf =="TT Courier New"])
#output 
#[1] "mono"


#defining latest date for max label on x axis, used in bimonthly function
#latestDate<-max(dataUp$Date,na.rm=TRUE)

# bimonthly <- function(x) {
#  x_range <- range(x, na.rm = TRUE)
#  
#  date_range <- c(
#    floor_date(x_range[1], "month"),
#     latestDate#modified to print the last date in the last bar of x axis
# )
#   monthly <- seq(date_range[1], date_range[2], by = "1 month")
#  
#   breakbimonthly<-sort(c(monthly, monthly + days(14))) #modified to print the last date in the last bar of x axis
#  append(breakbimonthly,latestDate)#modified to print the last date in the last bar of x axis
# }


