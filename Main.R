library(dplyr)
library(ggplot2)
library("data.table")

df_test_original <- as.data.frame(fread("RawDataset_0708_last.csv", encoding="UTF-8"))
#names(df_test_original)

df_test <- df_test_original %>% 
  rename("timestamp" = "타임스탬프") %>% 
  rename("class" = "몇 반인지 입력해주세요(개인정보 제공에 동의하지 않으신다면 '개인정보 제공에 동의하지 않음'에 체크해주세요.)") %>% 
  rename("name" = "이름을 입력해주세요(개인정보 제공에 동의하지 않으신다면 빈칸으로 남겨두시면 됩니다.)") %>% 
  rename("first_mid" = "2016년(중학교 1학년) 1월부터 6월까지 일주일에 영어학원을 몇 번 갔는지 입력해주세요. (영어학원을 다니지 않았다면 0을 체크해주세요.)") %>% 
  rename("first_last" = "2016년(중학교 1학년) 7월부터 12월까지 일주일에 영어학원을 몇 번 갔는지 입력해주세요. (영어학원을 다니지 않았다면 0을 체크해주세요.)") %>% 
  rename("second_mid" = "2017년(중학교 2학년) 1월부터 6월까지 일주일에 영어학원을 몇 번 갔는지 입력해주세요. (영어학원을 다니지 않았다면 0을 체크해주세요.)") %>% 
  rename("second_last" = "2017년(중학교 2학년) 7월부터 12월까지 일주일에 영어학원을 몇 번 갔는지 입력해주세요. (영어학원을 다니지 않았다면 0을 체크해주세요.)") %>% 
  rename("third_mid" = "2018년(중학교 3학년) 1월부터 6월까지 일주일에 영어학원을 몇 번 갔는지 입력해주세요. (영어학원을 다니지 않았다면 0을 체크해주세요.)") %>% 
  rename("third_last" = "2018년(중학교 3학년) 6월부터 12월까지 일주일에 영어학원을 몇 번 갔는지 입력해주세요. (영어학원을 다니지 않았다면 0을 체크해주세요.)") %>% 
  rename("score" = "이번 2020학년도 6월 고2모의고사 영어 성적을 입력해주세요.(숫자만 입력해주세요. ex.50)")

df_test$first_mid <- ifelse(df_test$first_mid == "다녔지만 몇 번 갔는지 기억나지 않음.", 2.5, df_test$first_mid)
df_test$first_mid <- ifelse(df_test$first_mid == "다녔는지 다니지 않았는지 기억나지 않음.", 0, df_test$first_mid)

df_test$first_last <- ifelse(df_test$first_last == "다녔지만 몇 번 갔는지 기억나지 않음.", 2.5, df_test$first_last)
df_test$first_last <- ifelse(df_test$first_last == "다녔는지 다니지 않았는지 기억나지 않음.", 0, df_test$first_last)

df_test$second_mid <- ifelse(df_test$second_mid == "다녔지만 몇 번 갔는지 기억나지 않음.", 2.5, df_test$second_mid)
df_test$second_mid <- ifelse(df_test$second_mid == "다녔는지 다니지 않았는지 기억나지 않음.", 0, df_test$second_mid)

df_test$second_last <- ifelse(df_test$second_last == "다녔지만 몇 번 갔는지 기억나지 않음.", 2.5, df_test$second_last)
df_test$second_last <- ifelse(df_test$second_last == "다녔는지 다니지 않았는지 기억나지 않음.", 0, df_test$second_last)

df_test$third_mid <- ifelse(df_test$third_mid == "다녔지만 몇 번 갔는지 기억나지 않음.", 2.5, df_test$third_mid)
df_test$third_mid <- ifelse(df_test$third_mid == "다녔는지 다니지 않았는지 기억나지 않음.", 0, df_test$third_mid)

df_test$third_last <- ifelse(df_test$third_last == "다녔지만 몇 번 갔는지 기억나지 않음.", 2.5, df_test$third_last)
df_test$third_last <- ifelse(df_test$third_last == "다녔는지 다니지 않았는지 기억나지 않음.", 0, df_test$third_last)

df_test$first_mid <- as.numeric(df_test$first_mid)
df_test$first_last <- as.numeric(df_test$first_last)
df_test$second_mid <- as.numeric(df_test$second_mid)
df_test$second_last <- as.numeric(df_test$second_last)
df_test$third_mid <- as.numeric(df_test$third_mid)
df_test$third_last <- as.numeric(df_test$third_last)

df_test <- df_test %>% 
  mutate(total_education = first_mid*27 + first_last*26 + second_mid*26 + second_last*26 + third_mid*26 + third_last*26 )

summary(df_test)


#df_for_csv <- df_test %>% select(c(class, name, score, total_education)) %>% arrange(score)


#write.csv(df_for_csv, "D:/GitHubs/BackUp/R/2020_0706_사회문화수행평가/Sociology_Culture_DataAnalysis/df_for_csv.csv")

#df_csv_filtered <- df_for_csv %>% 

#write.csv(df_csv_filtered, "D:/GitHubs/BackUp/R/2020_0706_사회문화수행평가/Sociology_Culture_DataAnalysis/df_csv_filtered.csv")


df_for_last <- df_test %>% select(c(score, total_education)) %>% arrange(score)

#여기서부터 정규성검정(Normality Test)
library(MASS)
#귀무가설을 기각하고 대립가설이 채택된다면(p<0.01 신뢰도95%) 해탕 데이터셋은 정규분포를 따르지 않는 것이다.
#귀무가설:데이터셋이 정규분포를 따른다

#성적에 대한 정규성검정
head(df_for_last)
shapiro.test(df_for_last$score)

shapiro.test(df_for_last$total_education)
#total_education의 p-value값이 0.01보다 낮게 나왔으므로 정규분포를 따르지 않음 -> 스피어만 상관계수

#스피어만 상관계수를 구하는 과정
attach(df_for_last)
cor.test(score, total_education, method = "spearman")
plot(score, total_education, xlab="6월 모의고사 점수", ylab="중학교 3년동안 총 학원을 간 횟수", pch=18, cex=3)

#https://ordo.tistory.com/23
# lm(x~y)
m1 <- lm(total_education~score)
abline(m1, col="blue", lwd=4)
summary(m1)
cor.test(total_education, score)