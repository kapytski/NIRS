# ранговый критерий тенденций для пропорций
# по Алво и Бертло
# источники:
# "Tests non-parametriques de tendance sur les proportions"
# "Nonparametric Tests of Trend for Proportions"

alvo.proportions.trend.test<-function(x,alternative="increasing")
{
  # находим доли
  m<-x[,1];n<-x[,2]
  # m<-rev(m);n<-rev(n)
  p<-m/n
  # находим полный объём и среднюю вероятность
  N<-sum(n);p.av<-sum(m)/N

  # --------------------------------------------
  # находим вспомогательне величины:
  # sum.n:
  sum.n<-rep(NA,length(n))
  sum.n[1]<-0
  for(i in 2:(length(n))) sum.n[i]<-sum.n[i-1]+n[i-1]
  sum.n

  # g:
  g<-sum.n+(n+1)/2

  # --------------------------------------------
  # считаем значение статистики Спирмена

  # числитель
  num<-sum(n*(g-(N+1)/2)*(p-p.av))

  # знаменатель
  denum<-sum(n*(g-(N+1)/2)^2)

  # находим значение статистики S
  s<-num/sqrt(denum*p.av*(1-p.av))

  # --------------------------------------------
  # находим значение уровня значимости
  p<-pnorm(s,lower.tail = switch(alternative,"increasing"=F,"decreasing"=T))
  return(c(s=round(s,3),p=round(p,4)))
}
