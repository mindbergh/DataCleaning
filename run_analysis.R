############################################################################################
## This function merges the training set and the test set into one and cleans the data set,
## writes the cleaned data set into TidyData.txt and returns the cleaned data set
##
## Author: Ming FANG
## https://github.com/mindbergh/
## April 26 2014
############################################################################################

run_analysis <- function() {		
		features <- read.table("features.txt")
		meanstd <- grep(".*(mean|std).*", features$V2) 
		
		X_train <- read.table(".\\train\\X_train.txt")
		X_train <- X_train[meanstd]
		y_train <- read.table(".\\train\\y_train.txt")
		subject_train <- read.table(".\\train\\subject_train.txt")
		res <- cbind(subject_train, y_train, X_train)
		
		X_test <- read.table(".\\test\\X_test.txt")
		X_test <- X_test[meanstd]
		y_test <- read.table(".\\test\\y_test.txt")
		subject_test <- read.table(".\\test\\subject_test.txt")
		res <- rbind(res, cbind(subject_test, y_test, X_test))
		
		label <- read.table("activity_labels.txt")
		
		titles <- c("Subject","Activity")
		titles <- c(titles, as.character(features[meanstd,]$V2))
		names(res) <- titles
		for (i in 1:6) {
			res$Activity[res$Activity == i] <- as.character(label$V2[i])
		}
		
		molten = melt(res, id = c("Subject", "Activity"))
		meanData <- dcast(molten, formula = Subject + Activity ~ variable, mean)
		write.table(meanData, file = "TidyData.txt", sep="\t")
		return(meanData)
}
