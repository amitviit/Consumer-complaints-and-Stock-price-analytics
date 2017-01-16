library(plyr)

ConsumerComplaints <- read.csv(file.choose())

# filter out records for wells fargo
ConsumerComplaints <- subset(ConsumerComplaints, Company =="Wells Fargo & Company")

#copy <- ConsumerComplaints
ConsumerComplaints<- copy


ConsumerComplaints$Date.received <- as.Date(ConsumerComplaints$Date.received, format= "%m/%d/%Y")

ConsumerComplaints$Month <- format(ConsumerComplaints$Date.received, format ="%m")
ConsumerComplaints$Year <- format(ConsumerComplaints$Date.received, format ="%Y")

# count number of complaints in each month
ConComp.summary <-ddply(ConsumerComplaints,.(Year,Month),nrow)

# create a sequence variable for each period
ConComp.summary$period <- seq.int(nrow(ConComp.summary))


colnames(ConComp.summary) [3] <- "count"
ConComp.summary <- subset(ConComp.summary,select = c("period","count"))

# remove the last record
ConComp.summary <- ConComp.summary [-c(59),]

## WELLS FARGO STOCK DATA

WFStock <- read.csv(file.choose())

WFStock$Date <- as.Date(WFStock$Date, format ="%m/%d/%Y")
WFStock$Month <- format(WFStock$Date, format ="%m")
WFStock$Year <- format(WFStock$Date, format ="%Y")

#aggregate the stock by month, year and function max on close
WFStock <- aggregate(WFStock, by = list(WFStock$Month,WFStock$Year), FUN = max)
WFStock$period <- seq.int(nrow(WFStock))
WFStock <- subset(WFStock , select = c("period","Close"))

# join the two datasets on variable period
WFCC <- merge(ConComp.summary,WFStock , by = "period")

plot (WFCC$count, WFCC$Close)
WFCC.reg <- lm (Close ~ count , WFCC)
summary(WFCC.reg)

# We conclude that number of consumer complaints dont have any impact on the Wells Fargo Stock price.