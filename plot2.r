######## Exploratory Data Analysis -- Assignment 1

########### CLEAR AND INITIALISE
rm(list=ls()) #Clear all data
setwd('D:/Courses/DataScience/ExploratoryDataAnalysis/Assigments/Assignment1')
# Load relevant packages
require(dplyr)

########### LOAD DATA
data <- read.table(unz("exdata-data-household_power_consumption.zip", 
                       "household_power_consumption.txt"), header=T,sep=";",
                   colClasses=c(rep('character',9)))

str(data)

data<-mutate(data,Date=as.POSIXct(paste(Date,Time,sep=' '),
                                  format='%d/%m/%Y %H:%M:%S'))%>%dplyr::select(-Time)


### select relevant dates
startDate<-as.POSIXct('2007-02-01 00:00:00')
endDate<-as.POSIXct('2007-02-03 00:00:00') ### 3rd as we want all the data right up to end of the 2nd
data<-dplyr::filter(data,Date>=startDate&Date<endDate)

#### Convert numeric data
### Create function to transform data
convertNumericWithQuestionMark<-function(x)
{
        
        ix<-which(x!='?') ## assume anything but '?' is a number
        retVec<-rep(NA,length(x)) ## initialise all as NA
        retVec[ix]<-as.numeric(x[ix]) ## and only populate valid
        retVec
}

### transform to numeric data
dataNumeric<-select(data,-Date) %>% mutate_each(funs(convertNumericWithQuestionMark)) %>%
        mutate(Date=data$Date) %>% select(c(8,1:7))
str(dataNumeric)

######### PLOTS
#### Plot2
png(file='plot2.png',width=480,height=480,type='windows')
with(dataNumeric,plot(
        Date,Global_active_power,
        type='l',xlab='',ylab='Global Active Power (kilowatts)'))
dev.off()