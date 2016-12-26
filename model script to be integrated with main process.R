# This log generally records the process of building a model. However, with very 
# little effort the log can be used to score a new dataset. The logical variable 
# 'building' is used to toggle between generating transformations, as when building 
# a model, and simply using the transformations, as when scoring a dataset.

building <- TRUE
scoring  <- ! building


# A pre-defined value is used to reset the random seed so that results are repeatable.

seed <- 42 

#============================================================
# Rattle timestamp: 2016-12-24 13:14:11 x86_64-w64-mingw32 

# Load an R data frame.

crs$dataset <- x

# Display a simple summary (structure) of the dataset.

str(crs$dataset)

#============================================================
# Rattle timestamp: 2016-12-24 13:14:11 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(seed) 
crs$nobs <- nrow(crs$dataset) # 14999 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 10499 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 2249 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 2251 observations

# The following variable selections have been noted.

crs$input <- c("satisfaction_level", "last_evaluation", "average_montly_hours", "time_spend_company",
               "Work_accident", "left", "promotion_last_5years", "sales",
               "salary")

crs$numeric <- c("satisfaction_level", "last_evaluation", "average_montly_hours", "time_spend_company")

crs$categoric <- c("Work_accident", "left", "promotion_last_5years", "sales",
                   "salary")

crs$target  <- "number_project"
crs$risk    <- NULL
crs$ident   <- "id"
crs$ignore  <- NULL
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2016-12-24 13:14:24 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(seed) 
crs$nobs <- nrow(crs$dataset) # 14999 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 10499 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 2249 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 2251 observations

# The following variable selections have been noted.

crs$input <- c("satisfaction_level", "last_evaluation", "number_project", "average_montly_hours",
               "time_spend_company", "Work_accident", "promotion_last_5years", "sales",
               "salary")

crs$numeric <- c("satisfaction_level", "last_evaluation", "number_project", "average_montly_hours",
                 "time_spend_company")

crs$categoric <- c("Work_accident", "promotion_last_5years", "sales", "salary")

crs$target  <- "left"
crs$risk    <- NULL
crs$ident   <- "id"
crs$ignore  <- NULL
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2016-12-24 13:14:45 x86_64-w64-mingw32 

# Ada Boost 

# The `ada' package implements the boost algorithm.

# Build the Ada Boost model.

set.seed(seed)
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

# Time taken: 11.85 secs

#============================================================
# Rattle timestamp: 2016-12-24 13:16:38 x86_64-w64-mingw32 

# Evaluate model performance. 

# Precision/Recall Plot: requires the ROCR package

library(ROCR)

# Generate a Precision/Recall Plot for the ada model on x [validate].

crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$validate, c(crs$input, crs$target)], type="prob")[,2]

# Remove observations with missing target.

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


# Add a legend to the plot.

legend("bottomleft", c("ada"), col=rainbow(1, 1, .8), lty=1:1, title="Models", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Precision/Recall Plot  x [validate]",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()

#============================================================
# Rattle timestamp: 2016-12-24 13:17:49 x86_64-w64-mingw32 

# Evaluate model performance. 

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the ada model on x [validate].

crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$validate, c(crs$input, crs$target)], type="prob")[,2]

# Remove observations with missing target.

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

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

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
performance(pred, "auc")

