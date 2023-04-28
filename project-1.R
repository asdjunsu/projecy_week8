#1-2. (adelie 만)

install.packages('palmerpenguins')
install.packages('dplyr')
install.packages('car')
install.packages('ggcorrplot')
install.packages('ggplot2')
library(ggplot2)
library('palmerpenguins')
library(dplyr)
library(car)
library(pgirmess)
library(ggcorrplot)


df <- penguins
str(df)
dim(df)  # 344, 8
sum(is.na(df))
df %>% filter(!is.na(df$bill_length_mm)) -> m_df
dim(m_df)

set.seed(2023)
nums<- sample(1:342, 182, replace = FALSE)
pang <- m_df[nums, ]
dim(pang)
pang
adelie <- filter(pang, species == "Adelie")
plot(adelie[3:6])


#ggplot(adelie, aes(x = bill_length_mm, y = bill_depth_mm)) +
  #geom_point() +
  #labs(title = "Adelie Penguin Bill Length vs. Depth")

adelie_cor=round(cor(adelie[, c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")]),2)
adelie_cor
ggcorrplot(adelie_cor,lab=T)

# bill_depth_mm and species
plot(bill_depth_mm  ~ island, data = adelie,
     main = 'adelie: island and bill depth',
     xlab = 'Island name',
     ylab = 'bill depth',
     barwidth = 3,
     col = 'orange',
     lwd = 2)

# 정규성 검정 -> 정규분포를 따르는가를 보는 것. 
# H0: 정규분포를 따른다.(p-value > 0.05)
# H1: 정규분포를 따르지않는다.(p-value < 0.05)
qqnorm(adelie$bill_depth_mm , main='bill depth')
qqline(adelie$bill_depth_mm )
shapiro.test(adelie$bill_depth_mm )

# 등분산성 검정 -> 분산이 동질한가를 보는 것. 
# H0: 분산이 동질하다.(p-value > 0.05)
# H1: 분산이 동질하지 않다.(p-value < 0.05)
leveneTest(adelie$bill_depth_mm, adelie$island)

# 분산분석
# H0: 섬별로 아델리 펭귄의 부리의 크기는 같다.(p-value > 0.05)
# H1: 섬별로 아델리 펭귄의 부리의 크기가 다르다다.(p-value < 0.05)
anova <- aov(bill_depth_mm ~ island, data = adelie)
summary(anova)
TukeyHSD(anova)


# bill_length_mm and species
plot(bill_length_mm ~ island, data = adelie,
     main = 'adelie: island and bill length',
     xlab = 'Island name',
     ylab = 'bill depth',
     barwidth = 3,
     col = 'orange',
     lwd = 2)

# 정규성 검정 -> 정규분포를 따르는가를 보는 것. 
# H0: 정규분포를 따른다.(p-value > 0.5)
# H1: 정규분포를 따르지않는다.(p-value < 0.5)
qqnorm(adelie$bill_length_mm,main='bill length')
qqline(adelie$bill_length_mm)
shapiro.test(adelie$bill_length_mm)

# 등분산성 검정 -> 분산이 동질한가를 보는 것. 
# H0: 분산이 동질하다.(p-value > 0.5)
# H1: 분산이 동질하지 않다.(p-value < 0.5)
leveneTest(adelie$bill_depth_mm, adelie$island)

# 분산분석
# H0: 섬별로 아델리 펭귄의 부리 크기는 같다.(p-value > 0.05)
# H1: 섬별로 아델리 펭귄의 부리 크기는 다르다.(p-value < 0.5)
anova <- aov(bill_length_mm ~ island, data = adelie)


# 정규분포를 따르고, 
#분산이 동질하기 때문에 anova 사용

summary(anova)
TukeyHSD(anova)


# flipper_length_mm and species
plot(flipper_length_mm ~ island, data = adelie,
     main = 'adelie: island and flipper length',
     xlab = 'Island name',
     ylab = 'flipper length',
     barwidth = 3,
     col = 'orange',
     lwd = 2)


# 정규성 검정 -> 정규분포를 따르는가를 보는 것. 
# H0: 정규분포를 따른다.(p-value > 0.5)
# H1: 정규분포를 따르지않는다.(p-value < 0.5)
qqnorm(adelie$flipper_length_mm,main='flipper length')
qqline(adelie$flipper_length_mm)
shapiro.test(adelie$flipper_length_mm)#사피로 테스트 수행

# 등분산성 검정 -> 분산이 동질한가를 보는 것. 
# H0: 분산이 동질하다.(p-value > 0.5)
# H1: 분산이 동질하지 않다.(p-value < 0.5)
leveneTest(adelie$flipper_length_mm, adelie$island)#3개의 섬에 대한 레반 테스트 수
anova <- aov(flipper_length_mm ~ island, data = adelie)
summary(anova)
TukeyHSD(anova)


