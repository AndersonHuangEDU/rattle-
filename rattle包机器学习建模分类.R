

library(rattle)   # Access the weather dataset and utilities.
library(magrittr) # Utilise %>% and %<>% pipeline operators.

building <- TRUE
scoring  <- ! building
# A pre-defined value is used to reset the random seed 
# so that results are repeatable.
crv$seed <- 42 

#=======================================================================


data(list = "iris", package = "datasets")
crs$dataset <- iris
names(crs$dataset) <- gsub("-", ".", names(crs$dataset))

#=======================================================================

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
  crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
  crs$test

# The following variable selections have been noted.

crs$input     <- c("Sepal.Length", "Sepal.Width", "Petal.Length",
                   "Petal.Width")

crs$numeric   <- c("Sepal.Length", "Sepal.Width", "Petal.Length",
                   "Petal.Width")

crs$categoric <- NULL

crs$target    <- "Species"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================

# The 'Hmisc' package provides the 'contents' function.

library(Hmisc, quietly=TRUE)

# Obtain a summary of the dataset.

contents(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])
summary(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])

#=======================================================================

# Display box plots for the selected variables. 

# Use ggplot2 to generate box plot for Sepal.Length

# Generate a box plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(Species=as.factor(Species)) %>%
  ggplot2::ggplot(ggplot2::aes(y=Sepal.Length)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::geom_boxplot(ggplot2::aes(x=Species, fill=Species), notch=TRUE) +
  ggplot2::stat_summary(ggplot2::aes(x=Species), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("Species\n\nRattle 2021-3月-30 01:05:08 黄雄恒的小新2021-Pro13") +
  ggplot2::ggtitle("Distribution of Sepal.Length (sample)\nby Species") +
  ggplot2::theme(legend.position="none")

# Use ggplot2 to generate box plot for Sepal.Width

# Generate a box plot.

p02 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(Species=as.factor(Species)) %>%
  ggplot2::ggplot(ggplot2::aes(y=Sepal.Width)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::geom_boxplot(ggplot2::aes(x=Species, fill=Species), notch=TRUE) +
  ggplot2::stat_summary(ggplot2::aes(x=Species), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("Species\n\nRattle 2021-3月-30 01:05:09 黄雄恒的小新2021-Pro13") +
  ggplot2::ggtitle("Distribution of Sepal.Width (sample)\nby Species") +
  ggplot2::theme(legend.position="none")

# Use ggplot2 to generate box plot for Petal.Length

# Generate a box plot.

p03 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(Species=as.factor(Species)) %>%
  ggplot2::ggplot(ggplot2::aes(y=Petal.Length)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::geom_boxplot(ggplot2::aes(x=Species, fill=Species), notch=TRUE) +
  ggplot2::stat_summary(ggplot2::aes(x=Species), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("Species\n\nRattle 2021-3月-30 01:05:09 黄雄恒的小新2021-Pro13") +
  ggplot2::ggtitle("Distribution of Petal.Length (sample)\nby Species") +
  ggplot2::theme(legend.position="none")

# Use ggplot2 to generate box plot for Petal.Width

# Generate a box plot.

p04 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(Species=as.factor(Species)) %>%
  ggplot2::ggplot(ggplot2::aes(y=Petal.Width)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::geom_boxplot(ggplot2::aes(x=Species, fill=Species), notch=TRUE) +
  ggplot2::stat_summary(ggplot2::aes(x=Species), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("Species\n\nRattle 2021-3月-30 01:05:09 黄雄恒的小新2021-Pro13") +
  ggplot2::ggtitle("Distribution of Petal.Width (sample)\nby Species") +
  ggplot2::theme(legend.position="none")

# Display the plots.

gridExtra::grid.arrange(p01, p02, p03, p04)

#=======================================================================

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for Sepal.Length

# Generate the plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(Species=as.factor(Species)) %>%
  dplyr::select(Sepal.Length, Species) %>%
  ggplot2::ggplot(ggplot2::aes(x=Sepal.Length)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=Species, colour=Species), alpha=0.55) +
  ggplot2::xlab("Sepal.Length\n\nRattle 2021-3月-30 01:05:18 黄雄恒的小新2021-Pro13") +
  ggplot2::ggtitle("Distribution of Sepal.Length (sample)\nby Species") +
  ggplot2::labs(fill="Species", y="Density")

# Use ggplot2 to generate histogram plot for Sepal.Width

# Generate the plot.

p02 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(Species=as.factor(Species)) %>%
  dplyr::select(Sepal.Width, Species) %>%
  ggplot2::ggplot(ggplot2::aes(x=Sepal.Width)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=Species, colour=Species), alpha=0.55) +
  ggplot2::xlab("Sepal.Width\n\nRattle 2021-3月-30 01:05:18 黄雄恒的小新2021-Pro13") +
  ggplot2::ggtitle("Distribution of Sepal.Width (sample)\nby Species") +
  ggplot2::labs(fill="Species", y="Density")

# Use ggplot2 to generate histogram plot for Petal.Length

# Generate the plot.

p03 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(Species=as.factor(Species)) %>%
  dplyr::select(Petal.Length, Species) %>%
  ggplot2::ggplot(ggplot2::aes(x=Petal.Length)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=Species, colour=Species), alpha=0.55) +
  ggplot2::xlab("Petal.Length\n\nRattle 2021-3月-30 01:05:18 黄雄恒的小新2021-Pro13") +
  ggplot2::ggtitle("Distribution of Petal.Length (sample)\nby Species") +
  ggplot2::labs(fill="Species", y="Density")

# Use ggplot2 to generate histogram plot for Petal.Width

# Generate the plot.

p04 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(Species=as.factor(Species)) %>%
  dplyr::select(Petal.Width, Species) %>%
  ggplot2::ggplot(ggplot2::aes(x=Petal.Width)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=Species, colour=Species), alpha=0.55) +
  ggplot2::xlab("Petal.Width\n\nRattle 2021-3月-30 01:05:18 黄雄恒的小新2021-Pro13") +
  ggplot2::ggtitle("Distribution of Petal.Width (sample)\nby Species") +
  ggplot2::labs(fill="Species", y="Density")

# Display the plots.

gridExtra::grid.arrange(p01, p02, p03, p04)

#=======================================================================
# Rattle timestamp: 2021-03-30 01:06:00 x86_64-w64-mingw32 

# Generate a correlation plot for the variables. 

# The 'corrplot' package provides the 'corrplot' function.

library(corrplot, quietly=TRUE)

# Correlations work for numeric variables only.

crs$cor <- cor(crs$dataset[crs$train, crs$numeric], use="pairwise", method="pearson")

# Order the correlations by their strength.

crs$ord <- order(crs$cor[1,])
crs$cor <- crs$cor[crs$ord, crs$ord]

# Display the actual correlations.

print(crs$cor)

# Graphically display the correlations.

corrplot(crs$cor, mar=c(0,0,1,0))
title(main="Correlation iris using Pearson",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#=======================================================================

# KMeans 

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# The 'reshape' package provides the 'rescaler' function.

library(reshape, quietly=TRUE)

# Generate a kmeans cluster of size 10.

crs$kmeans <- kmeans(sapply(na.omit(crs$dataset[crs$train, crs$numeric]), rescaler, "range"), 10)

#=======================================================================

# Report on the cluster characteristics. 

# Cluster sizes:

paste(crs$kmeans$size, collapse=' ')

# Data means:

colMeans(sapply(na.omit(crs$dataset[crs$train, crs$numeric]), rescaler, "range"))

# Cluster centers:

crs$kmeans$centers

# Within cluster sum of squares:

crs$kmeans$withinss

# Generate a discriminant coordinates plot.

cluster::clusplot(na.omit(crs$dataset[crs$train, intersect(crs$input, crs$numeric)]), crs$kmeans$cluster, color=TRUE, shade=TRUE, main='Discriminant Coordinates iris')

#=======================================================================

# Decision Tree 

# The 'rpart' package provides the 'rpart' function.

library(rpart, quietly=TRUE)

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# Build the Decision Tree model.

crs$rpart <- rpart(Species ~ .,
                   data=crs$dataset[crs$train, c(crs$input, crs$target)],
                   method="class",
                   parms=list(split="information"),
                   control=rpart.control(usesurrogate=0, 
                                         maxsurrogate=0),
                   model=TRUE)

# Generate a textual view of the Decision Tree model.

print(crs$rpart)
printcp(crs$rpart)
cat("\n")

#=======================================================================

# Plot the resulting Decision Tree. 

# We use the rpart.plot package.

fancyRpartPlot(crs$rpart, main="Decision Tree iris $ Species")

#=======================================================================

# Build a Random Forest model using the traditional approach.

set.seed(crv$seed)

crs$rf <- randomForest::randomForest(Species ~ .,
                                     data=crs$dataset[crs$train, c(crs$input, crs$target)], 
                                     ntree=500,
                                     mtry=2,
                                     importance=TRUE,
                                     na.action=randomForest::na.roughfix,
                                     replace=FALSE)

# Generate textual output of the 'Random Forest' model.

crs$rf

# List the importance of the variables.

rn <- round(randomForest::importance(crs$rf), 2)
rn[order(rn[,3], decreasing=TRUE),]

# Plot the OOB ROC curve.

library(verification)
aucc <- verification::roc.area(as.integer(as.factor(crs$dataset[crs$train, crs$target]))-1,
                               crs$rf$votes[,2])$A
verification::roc.plot(as.integer(as.factor(crs$dataset[crs$train, crs$target]))-1,
                       crs$rf$votes[,2], main="")
legend("bottomright", bty="n",
       sprintf("Area Under the Curve (AUC) = %1.3f", aucc))
title(main="OOB ROC Curve Random Forest iris",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#=======================================================================

# Score the validation dataset. 

# Obtain probability scores for the Decision Tree model on iris [validate].

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$validate, c(crs$input)],
                  type="class")

# Obtain probability scores for the Random Forest model on iris [validate].

crs$pr <- predict(crs$rf, newdata=na.omit(crs$dataset[crs$validate, c(crs$input)]))

# Obtain cluster number for the KMeans model on iris [validate].

crs$pr <- predict(crs$kmeans, crs$dataset[crs$validate, c(crs$input)])

# Extract the relevant variables from the dataset.

sdata <- subset(crs$dataset[crs$validate, ], select=c("Species"))

# Output the combined data.

write.csv(cbind(sdata, crs$pr), file="C:\Users\榛勯泟鎭掔殑灏忔柊2021-Pro13\Documents\iris_validate_score_idents.csv", row.names=FALSE)