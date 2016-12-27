# install.packages("pacman")
# install.packages("rmarkdown",repo="https://cloud.r-project.org")

# Things to add
# new.env()
# read data straight from kaggle
library(pacman)
pacman::p_load(rattle,stringr,magrittr,rmarkdown,knitr,ROCR,ggplot2)

setwd("C:/Users/timur/Documents/Kaggle/Human Resource Analytics/Employee-Resignation-Model")


# read in the csv data
# note we are specifying column classes different to the default
x <- read.csv("HR_comma_sep.csv"
              ,stringsAsFactors = F
              ,colClasses = c("numeric","numeric","integer","integer","integer","character","character","character","character","character"))

# Data has no id column, create one
x$id <- 1:dim(x)[1]


crs$dataset <- x
set.seed(42) 
crs$nobs <- nrow(crs$dataset) # 14999 observations 
# Create sample, validate, testing datasets with 70%/15%/15% split
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 10499 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 2249 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 2251 observations

# Independent variables
crs$input <- c("satisfaction_level", "last_evaluation", "number_project", "average_montly_hours",
               "time_spend_company", "Work_accident", "promotion_last_5years", "sales",
               "salary")

# Define which are numeric
crs$numeric <- c("satisfaction_level", "last_evaluation", "number_project", "average_montly_hours",
                 "time_spend_company")

# Define which are categoric
crs$categoric <- c("Work_accident", "promotion_last_5years", "sales", "salary")

crs$target  <- "left"

# Set ID var
crs$ident   <- "id"

# Run adaboost model; 50 iterations
crs$ada <- ada::ada(left ~ .,
                    data=crs$dataset[crs$train,c(crs$input, crs$target)],
                    control=rpart::rpart.control(maxdepth=30,
                                                 cp=0.010000,
                                                 minsplit=20,
                                                 xval=10),
                    iter=50)

# Print the results of the modelling.

print(crs$ada)
round(crs$ada$model$errs[crs$ada$iter,], 2)
cat('Variables actually used in tree construction:\n')
print(sort(names(listAdaVarsUsed(crs$ada))))
cat('\nFrequency of variables actually used:\n')
print(listAdaVarsUsed(crs$ada))

# Scoring the validation dataset
crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$validate, c(crs$input, crs$target)], type="prob")[,2]

no.miss   <- na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]$left)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "prec", "rec"), col="#CC0000FF", lty=1, add=FALSE)

legend("bottomleft", c("ada"), col=rainbow(1, 1, .8), lty=1:1, title="Models", inset=c(0.05, 0.05))

title(main="Precision/Recall Plot  x [validate]",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()



pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Ada Boost x [validate] left")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                  label=paste("AUC =", round(au, 2)))
print(p)
