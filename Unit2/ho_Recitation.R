library(ggplot2)
library(corrplot)

nba = read.csv("NBA_train.csv", stringsAsFactors=F)
str(nba)

# 플레이오프 진출에 영향을 가장 미치는 요소는?
M = cor(nba[, 3:ncol(nba)])
corrplot(M)
# W 즉 승수가 가장 큰 영향을 미치고...

# 플레이오프에 진출하려면 몇 게임이나 이겨야 하는가?
ggplot(data=nba, aes(x=SeasonEnd, y=W)) +
  geom_point(aes(color=as.factor(Playoffs)))
tapply(nba$W, nba$Playoffs, mean, na.rm=T)
# 평균적으로 50게임을 이겨야 하지만, 그래프를 볼 때 45게임 정도 이기면 될 것
# 같다.

# 그렇다면 W 에 영향을 미치는 요소는...
nba$PD = nba$PTS - nba$oppPTS
M = cor(nba[, 4:ncol(nba)])
corrplot.mixed(M)
# PD 즉 점수차가 가장 큰 영향을 미친다는 것을 알 수 있다. 0.97 정도니까 W 과 거의
# 일치한다고도 볼 수 있겠네.

# W ~ PD 간 모델링
reg_W = lm(W ~ PD, data=nba)
summary(reg_W)
# 41 + 0.03259*PD >= 45
PD = (45-41)/0.03259
# 즉 122점 이상 차이가 나야 45게임을 상대팀보다 더 이겼다고 볼 수 있다.