instalar <- function() {
    install.packages("devtools")
    library(devtools)
    install_github("easyGgplot2", "kassambara")
    library(easyGgplot2)
    
}


probl_1 <- function() {
    require(ggplot2);
    
    if(file.exists("activity.csv")) {
        data_file <- read.csv("activity.csv", colClasses = "character", na.strings="NA");
        cc <- complete.cases(data_file[1]);
        data <- data_file[cc,];
        head(data, 10);
    } else {
        print("File does not exist!!!");
    }
    
    vsteps <- c()
    vdate <- unique(data$date)
    
    for(i in 1:length(vdate)) {
        dsub <- subset(data, date==vdate[i]);
        value <- sum(as.numeric(dsub$steps));
        vsteps[i] <- value;
    }
    
    date_final <- as.Date(vdate);
    #print(class(date_final))
    #date_final <- strptime(vdate, format="%Y-%m-%d");
    
    final_data_1 <- data.frame(vsteps, date_final, stringsAsFactors = FALSE);
    
    #final_data_1 <- cbind(vsteps, date_final)
    
    #hist(final_data_1[,1], freq=TRUE, labels=FALSE, xlab="Intervals", ylab="Frequency", main="Total number of steps taken each day", col="red");
    
    #final_data_1[,2] <- strptime(final_data_1[,2], format="%Y-%m-%d");
    
    plot(strptime(final_data_1[,2], format="%Y-%m-%d"), as.numeric(final_data_1[,1]), type = "h", xlab="Days", ylab="Frequency", col="red", lwd = 4);    
    
    #qplot(x=date_final, y=vsteps, data=final_data_1, xlab=NULL, ylab=NULL)
    
    #ggplot2.histogram(data=final_data_1, xName=NULL, yName=NULL, alpha=0.5 )
    
}


probl_2 <- function() {
    if(file.exists("activity.csv")) {
        data_file <- read.csv("activity.csv", colClasses = "character", na.strings="NA");
        cc <- complete.cases(data_file[1]);
        data <- data_file[cc,];
        head(data, 10);
    } else {
        print("File does not exist!!!");
    }
    
    vsteps <- c()
    vinterval <- unique(data$interval)
    
    for(i in 1:length(vinterval)) {
        dsub <- subset(data, interval==vinterval[i]);
        value <- mean(as.numeric(dsub$steps));
        vsteps[i] <- value;
    }
    
    final_data_2 <- data.frame(vsteps, vinterval, stringsAsFactors = FALSE);
    
    plot(as.numeric(final_data_2[,2]), as.numeric(final_data_2[,1]), type = "l", xlab="Interval", ylab="Average steps across all days", col="blue");    
    
    
}


calculaMean <- function() {
    data_all <- read.csv("activity.csv", colClasses = "character", na.strings="NA");
    cc <- complete.cases(data_all[1]);
    data_mean <- data_all[cc,];
    data <- data_all[!cc,];
    
    vinterval <- unique(data_all$interval);
    vmean <- c();
    
    for(i in 1:length(vinterval)) {
        dsub <- subset(data_mean, interval==vinterval[i]);
        value <- mean(as.numeric(dsub$steps));
        vmean[i] <- value;
    }
    
    return (vmean)
}

calculaMeanSteps <- function() {
    data_all <- read.csv("activity.csv", colClasses = "character", na.strings="NA");
    cc <- complete.cases(data_all[1]);
    data_mean <- data_all[cc,];
        
    vdays <- unique(data_all$date);
    vmean <- c();
    
    for(i in 1:length(vdays)) {
        dsub <- subset(data_mean, date==vdays[i]);
        value <- mean(as.numeric(dsub$steps));
        vmean[i] <- as.numeric(value);
        
        if(is.nan(vmean[i])) {
            vmean[i] = 0;
            
        }
        
    }
    
    return (vmean)
}


probl_3 <- function() {
    data_all <- read.csv("activity.csv", colClasses = "character", na.strings="NA");
    
    vmean <- calculaMean();
    vinterval <- unique(data_all$interval);
    
    for(n in 1:length(vinterval)) {
        data_interval <- data_all[data_all[,3] == vinterval[n], ];
        data_interval[is.na(data_interval), 1] <- round(vmean[n], digits = 3);
        
        if(n == 1) {
            data_final <- data.frame(data_interval);
        }
        
        if(n != 1) {
            data_final <- rbind(data_final, data_interval);
        }
        
    }
   
}

probl_3_2 <- function() {
    require("ggplot2");
    data_all <- read.csv("activity.csv", colClasses = "character", na.strings="NA");
    
    vmean <- calculaMeanSteps();
    vdays <- unique(data_all$date);
    
    for(n in 1:length(vdays)) {
        data_days <- data_all[data_all[,2] == vdays[n], ];
        data_days[is.na(data_days$steps), 1] <- round(vmean[n], digits = 3);
        
        if(n == 1) {
            data_final <- data.frame(data_days);
        }
        
        if(n != 1) {
            data_final <- rbind(data_final, data_days);
        }
        
    }
    
    #write.table(data_final, file = "activity_filled_NAs.csv",row.names=FALSE, col.names=TRUE, sep=",")
    
    
    #data_final$date <- factor(data_final$date, levels=vdays , labels=vdays);
    
    #data2 <-  cbind(data_final$steps, data_final$date, stringsAsFactors = FALSE);
    #names <- c("steps", "date");
    #colnames(data2) <- names;
    
    #as_date <- as.POSIXct(strptime(data_final$date, "%Y-%m-%d"));
    
    #data_final$date <- factor(data_final$date, levels=as_date);
    
    
    
    #hist(as.numeric(data2[,1]), freq=TRUE, xlab = "Days", ylab="Steps", main="Mean of steps taken each day", col="red");
    #barplot(as.numeric(data_final$steps), plot=TRUE, xlab="Days", ylab="Steps", col = "red");
    
    #c <- ggplot(data=data_final, aes(x = date, y = steps)) + geom_histogram(stat="identity") + facet_wrap(~ interval);
    #print(c)
    
    return (data_final);
}

probl_4 <- function() {
    require("lubridate");
    Sys.setlocale("LC_TIME", "English");
    
    data <- probl_3_2();
    #dates <- as.POSIXct(strptime(data$date, "%Y-%m-%d"));
    #data_final$date <- factor(data_final$date, levels=as_date);
    
    vinterval <- unique(data$interval);
    dates <- as.Date(data$date);
    
    day <- c()
    
    for(n in 1:length(dates)) {
        d <- weekdays(dates[n]);
        
        if (d == "Saturday" || d == "Sunday") {
            day[n] <- "Weekend";    
        } else {
            day[n] <- "Weekday";    
        }
        
    }
    
    data_final <- cbind(data, day);
    
    #print(head(data_final, 50))
    
    data_final$day <- factor(data_final$day, levels=c("Weekday", "Weekend"), labels=c("Weekday", "Weekend"));
    
    data_weekday <- subset(data_final, day == "Weekday");
    data_weekend <- subset(data_final, day == "Weekend");
    
    v_mean_weekday <- c()
    v_mean_weekend <- c()
    
    for (m in 1: length(vinterval)) {
        a <- subset(data_weekday, interval == vinterval[m]) 
        v_mean_weekday[m] <- round(mean(as.numeric(a$steps)), 3)
        
        b <- subset(data_weekend, interval == vinterval[m])
        v_mean_weekend[m] <- round(mean(as.numeric(b$steps)), 3)
    }    
    
    data_weekday <- cbind(data_weekday, v_mean_weekday)
    data_weekend <- cbind(data_weekend, v_mean_weekend)
    
    print(colnames(data_weekday))
    print(colnames(data_weekend))
    
    par(mfrow= c(2,1));
    
    plot(as.numeric(data_weekend$interval), as.numeric(data_weekend$v_mean_weekend), type = "l", xlab="Interval", ylab="Steps", col="blue", main = "Weekend");    
    
    plot(as.numeric(data_weekday$interval), as.numeric(data_weekday$v_mean_weekday), type = "l", xlab="Interval", ylab="Steps", col="blue", main = "Weekday");    
    
    
    #c <- ggplot(data=data_final, aes(x = as.numeric(interval), y = as.numeric(steps))) + geom_line() + facet_grid(~ day);
    #print(c)
  
}