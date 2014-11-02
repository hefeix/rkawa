(require 'list-lib)
(require rkawa)

(define (run-plot thunk)
  (r/dev.on)
  (thunk))

(define (listing-1-1)
  (r &{age <- c(1,3,5,2,11,9,3,9,12,3)
       weight <- c(4.4,5.3,7.2,5.2,8.5,7.3,6.0,10.4,10.2,6.1)
       mean_weight <- mean(weight)
       sd_weight <- sd(weight)
       cor_age_weight <- cor(age, weight)}))

(define (listing-3-1)
  (r &{data(mtcars)
       attach(mtcars)
       plot(wt, mpg)
       abline(lm(mpg~wt))
       title("Regression of MPG on Weight")}))

;; Comparing Drug A and Drug B response by dose
(define (listing-3-2)
  (r &{dose  <- c(20, 30, 40, 45, 60)
       drugA <- c(16, 20, 27, 40, 60)
       drugB <- c(15, 18, 25, 31, 40)
       par(lwd=2, cex=1.5, font.lab=2)
       plot(dose, drugA, type="b",
                  pch=15, lty=1, col="red", ylim=c(0, 60),
                  main="Drug A vs. Drug B",
                  xlab="Drug Dosage", ylab="Drug Response")
       lines(dose, drugB, type="b",
                   pch=17, lty=2, col="blue")
       abline(h=c(30), lwd=1.5, lty=2, col="grey")
       library(Hmisc)
       minor.tick(nx=3, ny=3, tick.ratio=0.5)
       legend("topleft", inset=.05, title="Drug Type", c("A","B"),
              lty=c(1,2), pch=c(15, 17), col=c("red", "blue"))
       par(opar)
       TRUE}))

(define (listing-3-3)
  (r &{n <- 10
       mycolors <- rainbow(n)
       pie(rep(1,n), labels=mycolors, col=mycolors)}))

;; An Example of Custom Axes
(define (listing-3-4)
  (r &{opar <- par(no.readonly=TRUE)
       x <- c(1:10)
       y <- x
       z <- 10/x
       par(mar=c(5, 4, 4, 8) + 0.1)
       plot(x, y, type="b",
               pch=21, col="red",
               yaxt="n", lty=3, xlab="", ylab="")
       lines(x, z, type="b",
                pch=22, col="blue", lty=2)
       axis(2, at=x, labels=x, col.axis="red", las=0)
       axis(4, at=z, labels=round(z,digits=2),
               col.axis="blue", las=2, cex.axis=0.7, tck=-.01)
       mtext("z=10/x", side=4, line=3, cex.lab=1,las=2, col="blue")
       title("An Example of Creative Axes",
             xlab="X values",
             ylab="Y=X")
       par(opar)
       TRUE}))

;; Example of labeling points
(define (listing-3-5)
  (r &{data(mtcars)
       attach(mtcars)
       plot(wt, mpg,
                main="Milage vs. Car Weight",
                xlab="Weight", ylab="Mileage",
                pch=18, col="blue")
       text(wt, mpg,
                row.names(mtcars),
                cex=0.6, pos=4, col="red")}))

;; View font families
(define (listing-3-6)
  (r &{opar <- par(no.readonly=TRUE)
       par(cex=1.5)
       plot(1:7,1:7,type="n")
       text(3,3,"Example of default text")
       text(4,4,family="mono","Example of mono-spaced text")
       text(5,5,family="serif","Example of serif text")
       par(opar)
       TRUE}))

;; Combining graphs
(define (listing-3-7)
  (r &{opar <- par(no.readonly=TRUE)
       par(mfrow=c(2,2))
       data(mtcars)
       attach(mtcars)
       plot(wt, mpg, main="Scatterplot of wt vs. mpg")
       plot(wt, disp, main="Scatterplot of wt vs disp")
       hist(wt, main="Histogram of wt")
       boxplot(wt, main="Boxplot of wt")
       par(opar)
       TRUE}))

(define (listing-3-8)
  (r &{opar <- par(no.readonly=TRUE)
       par(mfrow=c(3,1))
       hist(wt)
       hist(mpg)
       hist(disp)
       par(opar)
       TRUE}))

(define (listing-3-9)
  (r &{layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
       hist(wt)
       hist(mpg)
       hist(disp)
       TRUE}))

(define (listing-3-10)
  (r &{layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE),
                    widths=c(3,1), heights=c(1,2))
       hist(wt)
       hist(mpg)
       hist(disp)
       TRUE}))

(define (listing-3-11)
  (r &{opar <- par(no.readonly=TRUE)
       par(fig=c(0,0.8,0,0.8))
       plot(mtcars$wt, mtcars$mpg,
                       xlab="Miles Per Gallon",
                       ylab="Car Weight")
       par(fig=c(0,0.8,0.55,1), new=TRUE)
       boxplot(mtcars$wt, horizontal=TRUE, axes=FALSE)
       par(fig=c(0.65,1,0,0.8),new=TRUE)
       boxplot(mtcars$mpg, axes=FALSE)
       mtext("Enhanced Scatterplot", side=3, outer=TRUE, line=-3)
       par(opar)
       TRUE}))

;; Comment -> #| comment |#
;; & -> &amp;
(define (listing-4-1)
  (r &{manager <- c(1,2,3,4,5)
       date <- c("10/24/08","10/28/08","10/1/08","10/12/08","5/1/09")
       gender <- c("M","F","F","M","F")
       age <- c(32,45,25,39,99)
       q1 <- c(5,3,3,3,2)
       q2 <- c(4,5,5,3,2)
       q3 <- c(5,2,5,4,1)
       q4 <- c(5,5,5,NA,2)
       q5 <- c(5,5,2,NA,1)
       leadership <- data.frame(manager,date,gender,age,q1,q2,q3,q4,q5, stringsAsFactors=FALSE)
       attach(leadership)

       #| Recoding variables |#
       leadership$agecat[age > 75] <- "Elder"
       leadership$agecat[age > 45 &amp; age <= 75] <- "Middle Aged"
       leadership$agecat[age <= 45] <- "Young"

       #| Renaming variables with the reshape package |#
       library(reshape)
       rename(leadership,
              c(manager="managerID", date="testDate"))

       #| Applying the is.na() function |#
       is.na(leadership[, 6:10])

       #| recode 99 to missing for the variable age |#
       leadership[age == 99, "age"] <- NA
       leadership

       #| Using na.omit() to delete incomplete observations |#
       na.omit(leadership)

       #| Converting character values to dates |#
       strDates <- c("01/05/1965", "08/16/1975")
       dates <- as.Date(strDates, "%m/%d/%Y")
       mydates <- as.Date(c("2007-06-22", "2004-02-13"))
       mydates

       #| Calculations with with dates |#
       startdate <- as.Date("2004-02-13")
       enddate   <- as.Date("2009-06-22")
       enddate - startdate

       #| Date functions and formatted printing |#
       today <- Sys.Date()
       format(today, format="%B %d %Y")
       dob <- as.Date("1956-10-10")
       format(dob, format="%A")

       #| Converting from one data type to another |#
       a <- c(1,2,3)
       a
       is.numeric(a)
       is.vector(a)
       a <- as.character(a)
       a
       is.numeric(a)
       is.vector(a)
       is.character(a)

       #| Sorting a dataset |#
       leadership[order(age),]
       leadership[order(gender, age),]
       leadership[order(gender, -age),]

       #| Selecting variables |#
       leadership[, c(6:10)]
       myvars <- c("q1", "q2", "q3", "q4", "q5")
       leadership[myvars]
       myvars <- paste("q", 1:5, sep="")
       leadership[myvars]

       #| Dropping variables |#
       myvars <- names(leadership) %in% c("q3", "q4")
       leadership[!myvars]
       leadership[c(-7,-8)]

       #| Selecting observations |#
       leadership[1:5,]
       leadership[which(leadership$gender=="M" &amp; leadership$age > 30),]
       leadership[which(gender == 'M' &amp; age > 30),]

       #| Selecting observations based on dates |#
       leadership$date <- as.Date(leadership$date, "%m/%d/%y")
       startdate <- as.Date("2009-01-01")
       enddate   <- as.Date("2009-10-31")
       leadership[leadership$date >= startdate &amp; leadership$date <= enddate,]

       #| Using the subset() function |#
       subset(leadership, age >= 35 | age < 24,
                          select=c(q1, q2, q3, q4))
       subset(leadership, gender=="M" &amp; age > 25,
                          select=gender:q4)}))

(define (listing-5-1)
  (r &{Student <- c("John Davis","Angela Williams","Bullwinkle Moose",
                    "David Jones","Janice Markhammer","Cheryl Cushing",
                    "Reuven Ytzrhak","Greg Knox","Joel England","Mary Rayburn")
       math <- c(502, 600, 412, 358, 495, 512, 410, 625, 573, 522)
       science <- c(95, 99, 80, 82, 75, 85, 80, 95, 89, 86)
       english <- c(25, 22, 18, 15, 20, 28, 15, 30, 27, 18)
       roster <- data.frame(Student, math, science, english,
                                     stringsAsFactors=FALSE)

       options(digits=2)
       z <- scale(roster[,2:4])
       score <- apply(z, 1, mean)
       roster <- cbind(roster, score)
       y <- quantile(score, c(.8,.6,.4,.2))
       roster$grade[score >= y[1]] <- "A"
       roster$grade[score < y[1] &amp; score >= y[2]] <- "B"
       roster$grade[score < y[2] &amp; score >= y[3]] <- "C"
       roster$grade[score < y[3] &amp; score >= y[4]] <- "D"
       roster$grade[score < y[4]] <- "F"
       name <- strsplit((roster$Student), " ")
       lastname <- sapply(name, "[", 2)
       firstname <- sapply(name, "[", 1)
       roster <- cbind(firstname,lastname, roster[,-1])
       roster <- roster[order(lastname,firstname),]
       roster}))

(define (listing-5-2)
  (r &{options(digits=3)
       data(mtcars)
       attach(mtcars)
       aggdata <- aggregate(mtcars, by=list(cyl,gear), FUN=mean, na.rm=TRUE)
       aggdata}))

;; Simple and horizontal bar plot
(define (listing-6-1)
  (r &{opar <- par(no.readonly=TRUE)
       library(vcd)
       counts <- table(Arthritis$Improved)
       barplot(counts, main="Simple Bar Plot",
                       xlab="Improvement", ylab="Frequency", horiz=TRUE)
       par(opar)
       TRUE}))

;; Stack and grouped bar plot
(define (listing-6-2 #!optional (grouped "TRUE"))
  (r &{opar <- par(no.readonly=TRUE)
       library(vcd)
       counts <- table(Arthritis$Improved, Arthritis$Treatment)
       barplot(counts,
               main="Grouped Bar Plot",
               xlab="Treatment", ylab="Frequency",
               col=c("red", "yellow", "green"),
               legend=rownames(counts), beside=&[grouped])}))

;; Mean bar plot
(define (listing-6-3)
  (r &{data(mtcars)
       attach(mtcars)
       mean_mpg <- aggregate(mpg, by=list(carb), FUN=mean)
       mean_mpg <- mean_mpg[order(mean_mpg$x),]
       barplot(mean_mpg$x, names.arg=mean_mpg$Group.1)
       title("Mean MPG")}))

;; Fitting labels in bar plots
(define (listing-6-4)
  (r &{opar <- par(no.readonly=TRUE)
       par(las=2)
       par(mar=c(5,8,4,2))
       library(vcd)
       counts <- table(Arthritis$Improved)
       barplot(counts, main="Treatment Outcome", horiz=TRUE, cex.names=0.8,
                       names.arg=c("No Improvement", "Some Improvement",
                                   "Marked Improvement"))
       par(opar)
       TRUE}))

;; Spinograms
(define (listing-6-5)
  (r &{library(vcd)
       attach(Arthritis)
       counts <- table(Treatment,Improved)
       spine(counts, main="Spinogram Example")
       detach(Arthritis)
       TRUE}))

;; Pie charts
(define (listing-6-6)
  (r &{slices <- c(10, 12, 4, 16, 8)
       lbls <- c("US", "UK", "Australia", "Germany", "France")
       layout(matrix(c(1,2,3,4), 2, 2, byrow=TRUE))

       pie(slices, labels=lbls,
                   main="Simple Pie Chart")

       pct <- round(slices/sum(slices)*100)
       lbls <- paste(lbls, pct)
       lbls <- paste(lbls,"%", sep="")
       pie(slices, labels=lbls, col=rainbow(length(lbls)),
                   main="Pie Chart with Percentages")

       library(plotrix)
       pie3D(slices, labels=lbls, explode=0.1,
                     main="3D Pie Chart")

       data(state)
       mytable <- table(state.region)
       lbls <- paste(names(mytable), "\n", mytable, sep="")
       pie(mytable, labels=lbls,
                    main="Pie Chart from a dataframe\n (with sample sizes)")
       TRUE}))

;; Fan plots
(define (listing-6-7)
  (r &{library(plotrix)
       slices <- c(10, 12,4, 16, 8)
       lbls <- c("US", "UK", "Australia", "Germany", "France")
       fan.plot(slices, labels=lbls, main="Fan Plot")
       TRUE}))

;; Colored Histogram with Rug Plot, Frame, and Specified Number of Bins
(define (listing-6-8)
  (r &{data(mtcars)
       hist(mtcars$mpg,
            freq=FALSE,
            breaks=12,
            col="red",
            xlab="Miles Per Gallon",
            main="Histogram, rug plot, density curve")
       rug(jitter(mtcars$mpg))
       lines(density(mtcars$mpg), col="blue", lwd=2)
       TRUE}))

;; Histogram with Superimposed Normal Curve (Thanks to Peter Dalgaard)
(define (listing-6-9)
  (r &{data(mtcars)
       x <- mtcars$mpg
       h <- hist(x, breaks=12, col="red",
                    xlab="Miles Per Gallon",
                    main="Histogram with normal curve and box")
       xfit <- seq(min(x), max(x), length=40)
       yfit <- dnorm(xfit, mean=mean(x), sd=sd(x))
       yfit <- yfit*diff(h$mids[1:2])*length(x)
       lines(xfit, yfit, col="blue", lwd=2)
       box()
       TRUE}))

;; Kernel density plot
(define (listing-6-10)
  (r &{data(mtcars)
       d <- density(mtcars$mpg)
       plot(d)
       ploygon(d, col="red", border="blue")
       rug(mtcars$mpg, col="brown")
       TRUE}))

;; Comparing kernel density plots
(define (listing-6-11)
  (r &{par(lwd=2)
       library(sm)
       data(mtcars)
       attach(mtcars)

       cyl.f <- factor(cyl, levels=c(4, 6, 8),
                            labels=c("4 cylinder", "6 cylinder", "8 cylinder"))

       sm.density.compare(mpg, cyl, xlab="Miles Per Gallon")
       title(main="MPG Distribution by Car Cylinders")
       detach(mtcars)
       par(lwd=1)
       TRUE}))

;; Parallel box plots
(define (listing-6-12)
  (r &{boxplot(mpg~cyl, data=mtcars,
               main="Car Milage Data",
               xlab="Number of Cylinders",
               ylab="Miles Per Gallon")
       TRUE}))
