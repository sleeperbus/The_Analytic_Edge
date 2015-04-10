boston = read.csv("boston.csv")
str(boston)
summary(boston)
formula = MEDV ~ LON + LAT + CRIM + ZN + INDUS + CHAS + NOX + RM +
    AGE + DIS + RAD + TAX + PTRATIO

summary(boston$MEDV)
plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV > 21.2], boston$LAT[boston$MEDV > 21.2],
       col="yellow", pch=19)
points(boston$LON[boston$MEDV > 25], boston$LAT[boston$MEDV > 25],
       col="purple", pch=19)

boxplot(boston$MEDV ~ boston$CHAS)
summary(boston$MEDV[boston$CHAS==0])
IQR(boston$MEDV[boston$CHAS==0])
