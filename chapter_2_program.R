# Predictive Model for Los Angeles Dodgers Promotion and Attendance (R)

##Below lines are calling the needed libraries. 
##car stands for Companion to Applied Regression. This package isn’t used to perform Applied Regression techniques, it compliments these techniques.
##lattice:Its goal is produce powerful multipanel plots and it is flexible enough to handle most nonstandard requirements 
library(car)  # special functions for linear regression
library(lattice)  # graphics package

## Below lines are creating the dataset for all dodger home games and checking structure of the data frame by printing it using print function
# read in data and create a data frame called dodgers
dodgers <- read.csv("dodgers.csv")
print(str(dodgers))  # check the structure of the data frame

##These lines below creating a new variable by recoding day_of_week variable. 
##Factors represent a very efficient way to store character values.
# define an ordered day-of-week variable 
# for plots and data summaries
dodgers$ordered_day_of_week <- with(data=dodgers,
  ifelse ((day_of_week == "Monday"),1,
  ifelse ((day_of_week == "Tuesday"),2,
  ifelse ((day_of_week == "Wednesday"),3,
  ifelse ((day_of_week == "Thursday"),4,
  ifelse ((day_of_week == "Friday"),5,
  ifelse ((day_of_week == "Saturday"),6,7)))))))
dodgers$ordered_day_of_week <- factor(dodgers$ordered_day_of_week, levels=1:7,
labels=c("Mon", "Tue", "Wed", "Thur", "Fri", "Sat", "Sun"))

##These lines are used to build chart with plot command using dodgers data. We are labeling the x axis as "Day of Week" and y-axis as "Attendence" with the xlab and ylab commands; It will be in violet color as set by col option.
# exploratory data analysis with standard graphics: attendance by day of week
with(data=dodgers,plot(ordered_day_of_week, attend/1000, 
xlab = "Day of Week", ylab = "Attendance (thousands)", 
col = "violet", las = 1))

##Here we are creating a table to check bobbleheads are used mostly on which day of the week
# when do the Dodgers use bobblehead promotions
with(dodgers, table(bobblehead,ordered_day_of_week)) # bobbleheads on Tuesday

##These lines here are used to establish a check for each month that the dodgers play. 
##A number and label is assigned to each month.
# define an ordered month variable 
# for plots and data summaries
dodgers$ordered_month <- with(data=dodgers,
  ifelse ((month == "APR"),4,
  ifelse ((month == "MAY"),5,
  ifelse ((month == "JUN"),6,
  ifelse ((month == "JUL"),7,
  ifelse ((month == "AUG"),8,
  ifelse ((month == "SEP"),9,10)))))))
dodgers$ordered_month <- factor(dodgers$ordered_month, levels=4:10, ##The function factor is used to encode a vector as a factor 
                                ##levels: is an optional vector of the unique values that x might have taken
labels = c("April", "May", "June", "July", "Aug", "Sept", "Oct")) ##labels are assigned to the levels

##These lines create a plot with the dodgers game data with month and attendence. col option sets the color to light blue.
# exploratory data analysis with standard R graphics: attendance by month 
with(data=dodgers,plot(ordered_month,attend/1000, xlab = "Month", 
ylab = "Attendance (thousands)", col = "light blue", las = 1))

##Below piece of code is trying to create a scatterplot with the xyplot command with attendence and temperature on the same axis while skies and day_night on the other. 
##The fill function is used to fill in missing values.
# exploratory data analysis displaying many variables
# looking at attendance and conditioning on day/night
# the skies and whether or not fireworks are displayed
library(lattice) # used for plotting 
# let us prepare a graphical summary of the dodgers data
group.labels <- c("No Fireworks","Fireworks")
group.symbols <- c(21,24)
group.colors <- c("black","black") 
group.fill <- c("black","red")
xyplot(attend/1000 ~ temp | skies + day_night, 
    data = dodgers, groups = fireworks, pch = group.symbols, ##pch specifies which plotting symbol will be shown in the plot.
 ##Lune 71: This controls the physical aspect ratio of the panels, which is usually the same for all the panels.
  ##It can be specified as a ratio (vertical size/horizontal size) or as a character string. In the latter case, legitimate values are "fill" (the default) which tries to make the panels as big as possible to fill the available space; fill: serves the purpose of bg for certain values of pch
    aspect = 1, cex = 1.5, col = group.colors, fill = group.fill, 
    layout = c(2, 2), type = c("p","g"),
    strip=strip.custom(strip.levels=TRUE,strip.names=FALSE, style=1),
    xlab = "Temperature (Degrees Fahrenheit)", ##giving labels for x axis
    ylab = "Attendance (thousands)", ##giving labels for y axis
       ##key is a list that is defining legend for the plot
    key = list(space = "top", 
        text = list(rev(group.labels),col = rev(group.colors)),
        points = list(pch = rev(group.symbols), col = rev(group.colors),
        fill = rev(group.fill))))  

##This piece of code is preparing a graphical summary that compares the relationship between attendence and whether the game is at day or night.
##The panel command is establishing the data as cross-sectional time series data.
# attendance by opponent and day/night game
group.labels <- c("Day","Night")
group.symbols <- c(1,20)
group.symbols.size <- c(2,2.75)
bwplot(opponent ~ attend/1000, data = dodgers, groups = day_night, 
    xlab = "Attendance (thousands)",
    panel = function(x, y, groups, subscripts, ...) ##The actual plotting is done by the function specified by the panel argument. 
       {panel.grid(h = (length(levels(dodgers$opponent)) - 1), v = -1)
        panel.stripplot(x, y, groups = groups, subscripts = subscripts, 
        cex = group.symbols.size, pch = group.symbols, col = "darkblue")
       },
    key = list(space = "top", ##key is a list that is defining legend for the plot
    text = list(group.labels,col = "black"),
    points = list(pch = group.symbols, cex = group.symbols.size, 
    col = "darkblue")))

##Multiple functions have been used to serve the purpose to employe training and test regimen for model validation.
##The set. seed() function sets the starting number used to generate a sequence of random numbers – it ensures that you get the same result if you start with that same seed each time you run the same process.
##The str function below displayed the internal structure of the training/test dataset.
# employ training-and-test regimen for model validation
set.seed(1234) # set seed for repeatability of training-and-test split
training_test <- c(rep(1,length=trunc((2/3)*nrow(dodgers))),
rep(2,length=(nrow(dodgers) - trunc((2/3)*nrow(dodgers)))))
dodgers$training_test <- sample(training_test) # random permutation 
dodgers$training_test <- factor(dodgers$training_test, 
  levels=c(1,2), labels=c("TRAIN","TEST"))
dodgers.train <- subset(dodgers, training_test == "TRAIN")
print(str(dodgers.train)) # check training data frame
dodgers.test <- subset(dodgers, training_test == "TEST")
print(str(dodgers.test)) # check test data frame

##This part specifies a simple model that includes month, day of week and bobblehead entered last as predictor variables. 
# specify a simple model with bobblehead entered last
my.model <- {attend ~ ordered_month + ordered_day_of_week + bobblehead}

##lm function is used to fit linear models. It can be used to carry out regression, single stratum analysis of variance and analysis of covariance.
# fit the model to the training set
train.model.fit <- lm(my.model, data = dodgers.train)

##summary is a generic function used to produce result summaries of the results of model fitting function.
# summary of model fit to the training set
print(summary(train.model.fit))

##Below line shows the predictions coming from the training regiment.
# training set predictions from the model fit to the training set
dodgers.train$predict_attend <- predict(train.model.fit) 

##Below we see that training set is used to test the predictions from the test set
# test set predictions from the model fit to the training set
dodgers.test$predict_attend <- predict(train.model.fit, 
  newdata = dodgers.test)

##Below lines are used when predicting data out-of-sample. 
##cat is used for producing output in user-defined functions. It converts its arguments to character vectors, concatenates them to a single character vector, appends the given sep =  string(s) to each element and then outputs them.
# compute the proportion of response variance
# accounted for when predicting out-of-sample
cat("\n","Proportion of Test Set Variance Accounted for: ",
round((with(dodgers.test,cor(attend,predict_attend)^2)),
  digits=3),"\n",sep="")

##The rbind function is used to combine several vectors, matrices and/or data frames by rows. 
# merge the training and test sets for plotting
dodgers.plotting.frame <- rbind(dodgers.train,dodgers.test)

## Below part establishes a predictive model that best encapsulates data and business question to make it viewable and easily understandable.
# generate predictive modeling visual for management
group.labels <- c("No Bobbleheads","Bobbleheads")
group.symbols <- c(21,24)
group.colors <- c("black","black") 
group.fill <- c("black","red")  
xyplot(predict_attend/1000 ~ attend/1000 | training_test, 
       data = dodgers.plotting.frame, groups = bobblehead, cex = 2,
       pch = group.symbols, col = group.colors, fill = group.fill, 
       layout = c(2, 1), xlim = c(20,65), ylim = c(20,65), 
       aspect=1, type = c("p","g"),
       panel=function(x,y, ...)
            {panel.xyplot(x,y,...)
             panel.segments(25,25,60,60,col="black",cex=2)
            },
       strip=function(...) strip.default(..., style=1),
       xlab = "Actual Attendance (thousands)", 
       ylab = "Predicted Attendance (thousands)",
       key = list(space = "top", 
              text = list(rev(group.labels),col = rev(group.colors)),
              points = list(pch = rev(group.symbols), 
              col = rev(group.colors),
              fill = rev(group.fill))))            
        
# use the full data set to obtain an estimate of the increase in
# attendance due to bobbleheads, controlling for other factors 
my.model.fit <- lm(my.model, data = dodgers)  # use all available data
print(summary(my.model.fit))
# tests statistical significance of the bobblehead promotion
# type I anova computes sums of squares for sequential tests
print(anova(my.model.fit))  


##This converts the argument into character strings
## It uses the my.model command with round function to round off values from the previous coefficients.
cat("\n","Estimated Effect of Bobblehead Promotion on Attendance: ",
round(my.model.fit$coefficients[length(my.model.fit$coefficients)],
digits = 0),"\n",sep="")

##plot function is used to provide diagnostic plots
# standard graphics provide diagnostic plots
plot(my.model.fit)

##below part is used for additional model diagnostics but this time from the car package
##residualPlots: plots the residuals versus each term in a mean function and versus fitted values
##marginalModelPlots: These plots allow for the comparison of the fitted model with a nonparametric or semiparametric model fit. The user may precisely specify how the alternative fit is computed. 
# additional model diagnostics drawn from the car package
library(car)
residualPlots(my.model.fit)
marginalModelPlots(my.model.fit)
print(outlierTest(my.model.fit))

# Suggestions for the student:
# Examine regression diagnostics for the fitted model.
# Examine other linear predictors and other explanatory variables.
# See if you can improve upon the model with variable transformations.



