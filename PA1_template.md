# Assignment of Coursera

## First question

We read the dataset in "data" and we make the column "date" as date format

'''{r}

data<-read.csv(file="activity.csv",header=TRUE)
data$date<-as.Date(as.character(data$date),"%Y-%m-%d")

'''

## Second question

Histogram of the total number of steps taken each day

'''{r}

library("ggplot2")
perday<-aggregate(data$steps,list(Date=data$date),sum)
qplot(perday$x,main="Histogram of the total number of steps taken each day",xlab="Number of steps")

'''

## Third question

The mean and the median number of steps taken each day. 

'''{r}

with(perday,c("mean"=mean(x,na.rm=TRUE),"median"=median(x,na.rm=TRUE)))

'''

## Fourth question

Time series plot of the average number of steps taken

'''{r}

avera<-tapply(data$steps,data$interval,mean,na.rm=TRUE)
plot(avera,type="l",xlim=range(0,2355),xlab="5 minutes interval",ylab="average steps",main="average number of steps taken")

'''

## Fiveth question

The five minutes interval that, on average, contains the maximum number of steps

'''{r}
		
avera[with(avera,which(x==max(x))),1]

'''

## Sixth question

Code to describe and show a strategy for imputing missing data

'''{r}

c("Number of missing value"=nrow(data[missing,]), "Percentage"=nrow(data[missing,])/nrow(data))
'''

To fill the missing values, we can replace its by the mean of their interval.


## Seventh question

Histogram of the total number of steps taken each day after missing values are imputed

'''{r}

data2<-data

for (i in 1:nrow(data2)){
	
	if (is.na(data2$steps[i])==TRUE){
	
		x<-i%%288
		if (x==0){
			x<-288
		}
		data2$steps[i]<-avera[x,2]
	}
}

perday2<-aggregate(data2$steps,list(Date=data$date),sum)

qplot(perday2$x,main="Histogram of the total number of steps taken each day",xlab="Number of steps")


'''

## Eighth question

Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

'''{r}


weekends = avera2$date[ as.POSIXlt(as.Date(avera2$date,'%m/%d/%Y'))$wday %in% c(0,6)]
data2<-transform(data2 ,weekend = ifelse(data2$date %in% as.Date(weekends,'%m/%d/%Y') ,1,0 ))

data_wkd<-subset(data2,weekend==0)
avera2<-tapply(data_wkd$steps,data_wkd$interval,mean,na.rm=TRUE)

data_wknd<-subset(data2,weekend==1)
avera3<-tapply(data_wknd$steps,data_wknd$interval,mean,na.rm=TRUE)

par(mfrow=c(1,2))
plot(avera2,type="l",xlab="5 minutes interval",ylab="average steps",main="avg nber steps in weekdays")
plot(avera3,type="l",xlab="5 minutes interval",ylab="average steps",main="avg nber steps in weekend")


'''




