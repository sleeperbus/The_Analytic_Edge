Sys.setlocale("LC_ALL", "C") 
library(ggplot2)
library(corrplot)

baseball = read.csv("baseball.csv", stringsAsFactors = F)
moneyball = subset(baseball, Year < 2002)
str(moneyball)
ggplot(data=moneyball, aes(x=Year, y=W)) +
  geom_point(aes(color=as.factor(Playoffs)))

# 플레이오프에 진출하기 위해서는? 많이 이겨야 하겠지.
# 그렇다면 얼마나 이겨야 하는가?
tapply(moneyball$W, moneyball$Playoffs, mean, na.rm=T) 
# 평균적으로 95승 이상을 해야 플레이오프에 진출할 수 있다. 

# 승수에 영향을 미치는 것은 무엇인가? 물론 RS, RA 가 영향을 미치겠지만
# cor 값이 서로 반대이다. 때문에 그럴듯한 RD = RS - RA 라는 값을 사용해서 
# 모델을 만들어보자. 
moneyball$RD = moneyball$RS - moneyball$RA
reg_W = lm(W ~ RD, data=moneyball)
summary(reg_W)
# R-squared 값이 0.88 정도가 나온다. 이 정도면 꽤 괜찮다. 
# 그렇다면 95승을 내려면 RD 값은 얼마가 되어야 할까?
RD = (95 - 80.8813)/0.1058
# 80.8813 + 0.1058*RD >= 95, 즉 RD 값은 최소 134 이상이어야 한다.

# RD 값을 늘리기 위해서는 RS 는 높이고 RA 는 낮춰야 한다.
# 우선 RS 부터 생각해보자.
# RS 에 영향을 미치는 요소는 OBP, SLG, BA 세 가지가 있다.
cor(moneyball[, c("RS", "OBP", "SLG", "BA")])
# 세 가지 모두 관계는 있는 것 같다. 그럼 이 세가지를 이용해서 모델을...
reg_RS = lm(RS ~ OBP + SLG + BA, data=moneyball)
summary(reg_RS)
# BA 는 batting average 즉 타율인데 값이 마이너스로 나왔다. 이것 좀 이상한데...
# 타율이 높아야 점수가 잘 나지 않나? 
# 현재 Adjusted R-squared 는 0.93 이다. BA 를 빼봅시다.
reg_RS = lm(RS ~ OBP + SLG, data=moneyball)
summary(reg_RS)
# Adjusted R-squared 는 0.9294 로 줄기는 했지만 큰 차이는 나지 않는다.

# 오클랜드A 의 2001년도 데이터를 봅시다.
# 2001년의 OBP = 0.339, SLG = 0.430 이라고 했을 때
RS = -804.63 + 2737.77*0.339 + 1584.91*0.430
# 즉, RS 는 805가 된다.

# RA 를 구해봅시다. RA 에 영향을 미치는 것은 OOBP, OSLG 등이다.
reg_RA = lm(RA ~ OOBP + OSLG, data=moneyball)
summary(reg_RA)
# 2001년의 OOBP = 0.307, OSLG = 0.373 이라고 한다면 
RA = -837.38 + 2913.60*0.307 + 1514.29*0.373
# RA 는 622 가 나온다.

# 그렇다면 RD = RS - RA = 805 - 622 = 183 
# 승수를 구해보자.
W = 80.88 + 0.1057*183
# 약 100승이 나온다.


# 플레이오프에 진출한 팀의 플레이오프 순위와 정규시즌 내 승수와의 관계
playoffs = subset(moneyball, Playoffs == 1)
summary(playoffs)
plot(playoffs$RankPlayoffs, playoffs$W)
cor(playoffs$RankPlayoffs, playoffs$W)
cor(playoffs$RankSeason, playoffs$RankPlayoffs)


# Quiz
teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 = c(94, 88, 95, 88, 93, 94, 98, 97, 93, 94)
wins2013 = c(97, 97, 92, 93, 92, 96, 94, 96, 92, 90)
cor(teamRank, wins2012)
cor(teamRank, wins2013)
