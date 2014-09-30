library(ggplot2)

data(USArrests)
state.coordinates <- read.table("stateCoord.txt")

# Clean the hyphens from the state names in the state coordinates file
rownames(state.coordinates) <- gsub("-", " ", rownames(state.coordinates))

# "d" is for "data".
d <- merge(USArrests, state.coordinates, by="row.names")
if (nrow(d) != nrow(USArrests)) {
  stop("The merge between the arrests and state locations failed.")
}

# There is a positive correlation between murder and assault rates.
ggplot(d) + geom_point(aes(x=Murder, y=Assault), size=4)

# There is an outlier in the relationship between urban populationa and
# rate of rape.  Color it by hand.
d$outlier <- with(d, ifelse(Rape > 0.75 * UrbanPop, "Outlier", "Normal"))
ggplot(d) +
  geom_point(aes(x=UrbanPop, y=Rape, color=outlier),
             size=4)

# What is that outlier?  It is Alaska, those jerks.
ggplot(d) +
  geom_text(aes(x=UrbanPop, y=Rape, label=as.character(Row.names),
                color=outlier))

############
# Run regressions of rate of rape on urban population.

# A function to fit a regression by hand for comparison with lm().
# x: The regressors
# y: The response
RegressionByHand <- function(x, y) {
  bhat <- solve(t(x) %*% x) %*% (t(x) %*% y)
  y.fitted <- x %*% bhat
  residuals <- y - y.fitted
  return(list(bhat=bhat, y.fitted=y.fitted, residuals=residuals))
}

hand.reg <- RegressionByHand(x=as.matrix(cbind(1, d$UrbanPop), ncol=2),
                             y=as.matrix(d$Rape, ncol=1))

# Then using lm:
regression <- summary(lm(Rape ~ UrbanPop, d))

# Of course they are the same:
print(cbind(hand.reg$bhat, regression$coefficients[,"Estimate"]))
print(sum(abs(regression$residuals - hand.reg$residuals)))

# The variance of the residuals increases with the prediction.  This is
# probably due to the response being constrained to be positive.  See
# below.
ggplot() +
  geom_point(aes(x=hand.reg$y.fitted, y=hand.reg$residuals)) +
  geom_hline(aes(y.intercept=0)) +
  xlab("Fitted") + ylab("Residuals")

# The raw data with the fitted line.
ggplot(d) +
  geom_point(aes(x=UrbanPop, y=Rape, color=outlier),
             size=4) +
  geom_abline(aes(intercept=hand.reg$bhat[1], slope=hand.reg$bhat[2]),
              color="blue")

# Run the regression with no outlier.
d.trim <- subset(d, outlier == "Normal")
hand.reg.trim <- RegressionByHand(x=as.matrix(cbind(1, d.trim$UrbanPop), ncol=2),
                                  y=as.matrix(d.trim$Rape, ncol=1))

# The raw data with both fitted lines, with and without the outlier.
# I'll put the pretty title and legend for (6) in here.
ggplot(d) +
  geom_point(aes(x=UrbanPop, y=Rape, color=outlier), size=4) +
  geom_abline(aes(intercept=hand.reg$bhat[1], slope=hand.reg$bhat[2],
              color="With Alaska (outlier)"), lwd=2) +
  geom_abline(aes(intercept=hand.reg.trim$bhat[1], slope=hand.reg.trim$bhat[2],
              color="Without Alaska (outlier)"), lwd=2) +
  ggtitle(paste("Linear fit to Rape / 100k people vs Urban Population %",
                "with and without Alaska (an outlier)", sep="\n")) +
  xlab("Urban Population %") + ylab("Rapes per 100k people") +
  scale_color_manual(name="Color meanings",
                    values=c("Normal" = "black", "Outlier" = "orange",
                             "With Alaska (outlier)" = "blue",
                             "Without Alaska (outlier)" = "red"))

# The fit does not look great, especially the heteroskedasticity.
# The increase in the residuals' variance as a function of the fitted value
# could be because the response is constrained to be positive.
log.reg <- RegressionByHand(x=as.matrix(cbind(1, log(d.trim$UrbanPop)), ncol=2),
                            y=as.matrix(log(d.trim$Rape), ncol=1))

# Note that there is less heteroskedasticity in log space and the fit looks better.
ggplot() +
  geom_point(aes(x=log.reg$y.fitted, y=log.reg$residuals)) +
  geom_hline(aes(y.intercept=0)) +
  xlab("Fitted") + ylab("Residuals of the log regression")

# The raw data with the fitted line from the log regression.  Note that if
# Alaska is an outlier on the log scale, then Rhode Island is, too.
ggplot(d) +
  geom_text(aes(x=log(UrbanPop), y=log(Rape), color=outlier, label=Row.names),
             size=4) +
  geom_abline(aes(intercept=log.reg$bhat[1], slope=log.reg$bhat[2]),
              color="blue") +
  ggtitle("Linear fit to log(Rape) / 100k people vs log(Urban Population %)") +
  xlab("log(Urban Population %)") + ylab("log(Rapes per 100k people)")

# However, the R2 doesn't increase very much.
var(hand.reg.trim$y.fitted) / (var(hand.reg.trim$y.fitted) + var(hand.reg.trim$residuals))
var(log.reg$y.fitted) / (var(log.reg$y.fitted) + var(log.reg$residuals))


