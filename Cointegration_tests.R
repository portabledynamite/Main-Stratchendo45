
####coint_with_1lag####
data444 <- read_excel("Desktop/data44.xlsx")
con_with_coint123 <- NULL
qwer1 <- NULL

for (i in 1:137){
  data455 <- filter(data444, Country_name == unique(data444$Country_name)[i])
  GDP_ts <- ts(log(data455$GDP), start = 1980, frequency = 1, end = 2020)
  LE_ts <-  ts(log(data455$LE), start = 1980, frequency = 1, end = 2020)
  vkgh <- coint.test(LE_ts, as.matrix(data.frame(GDP_ts)), nlag = 1, output = TRUE)
  qwer1 <- c(vkgh[1,3], qwer1)
  if (vkgh[1,3] <= 0.05) {
    con_with_coint123 <- c(con_with_coint123, i)} else {}
}
coint_with_1lag <- cbind(unique(data444$Country_name), qwer1)
colnames(coint_with_1lag) <- (c("Country", "Engle-Granger test with 1 lag LE~GDP"))
View(coint_with_1lag)

####coint_with_2lag####
data444 <- read_excel("Desktop/data44.xlsx")
con_with_coint123 <- NULL
qwer1 <- NULL

for (i in 1:137){
  data455 <- filter(data444, Country_name == unique(data444$Country_name)[i])
  GDP_ts <- ts(log(data455$GDP), start = 1980, frequency = 1, end = 2020)
  LE_ts <-  ts(log(data455$LE), start = 1980, frequency = 1, end = 2020)
  vkgh <- coint.test(LE_ts, as.matrix(data.frame(GDP_ts)), nlag = 2, output = TRUE)
  qwer1 <- c(vkgh[1,3], qwer1)
  if (vkgh[1,3] <= 0.05) {
    con_with_coint123 <- c(con_with_coint123, i)} else {}
}
coint_with_2lag <- cbind(unique(data444$Country_name), qwer1)
colnames(coint_with_2lag) <- (c("Country", "Engle-Granger test with 2 lags LE~GDP"))
View(coint_with_2lag)

####coint_with_3lag####
data444 <- read_excel("Desktop/data44.xlsx")
con_with_coint123 <- NULL
qwer1 <- NULL

for (i in 1:137){
  data455 <- filter(data444, Country_name == unique(data444$Country_name)[i])
  GDP_ts <- ts(log(data455$GDP), start = 1980, frequency = 1, end = 2020)
  LE_ts <-  ts(log(data455$LE), start = 1980, frequency = 1, end = 2020)
  vkgh <- coint.test(LE_ts, as.matrix(data.frame(GDP_ts)), nlag = 3, output = TRUE)
  qwer1 <- c(vkgh[1,3], qwer1)
  if (vkgh[1,3] <= 0.05) {
    con_with_coint123 <- c(con_with_coint123, i)} else {}
}
coint_with_3lag <- cbind(unique(data444$Country_name), qwer1)
colnames(coint_with_3lag) <- (c("Country", "Engle-Granger test with 3 lags LE~GDP"))
View(coint_with_3lag)

####coint_with_4lag####
data444 <- read_excel("Desktop/data44.xlsx")
con_with_coint123 <- NULL
qwer1 <- NULL

for (i in 1:137){
  data455 <- filter(data444, Country_name == unique(data444$Country_name)[i])
  GDP_ts <- ts(log(data455$GDP), start = 1980, frequency = 1, end = 2020)
  LE_ts <-  ts(log(data455$LE), start = 1980, frequency = 1, end = 2020)
  vkgh <- coint.test(LE_ts, as.matrix(data.frame(GDP_ts)), nlag = 4, output = TRUE)
  qwer1 <- c(vkgh[1,3], qwer1)
  if (vkgh[1,3] <= 0.05) {
    con_with_coint123 <- c(con_with_coint123, i)} else {}
}
coint_with_4lag <- cbind(unique(data444$Country_name), qwer1)
colnames(coint_with_4lag) <- (c("Country", "Engle-Granger test with 4 lags LE~GDP"))
View(coint_with_4lag)

####coint_with_5lag####
data444 <- read_excel("Desktop/data44.xlsx")
con_with_coint123 <- NULL
qwer1 <- NULL

for (i in 1:137){
  data455 <- filter(data444, Country_name == unique(data444$Country_name)[i])
  GDP_ts <- ts(log(data455$GDP), start = 1980, frequency = 1, end = 2020)
  LE_ts <-  ts(log(data455$LE), start = 1980, frequency = 1, end = 2020)
  vkgh <- coint.test(LE_ts, as.matrix(data.frame(GDP_ts)), nlag = 5, output = TRUE)
  qwer1 <- c(vkgh[1,3], qwer1)
  if (vkgh[1,3] <= 0.05) {
    con_with_coint123 <- c(con_with_coint123, i)} else {}
}
coint_with_5lag <- cbind(unique(data444$Country_name), qwer1)
colnames(coint_with_5lag) <- (c("Country", "Engle-Granger test with 5 lags LE~GDP"))
View(coint_with_5lag)

####coint_with_6lag####
data444 <- read_excel("Desktop/data44.xlsx")
con_with_coint123 <- NULL
qwer1 <- NULL

for (i in 1:137){
  data455 <- filter(data444, Country_name == unique(data444$Country_name)[i])
  GDP_ts <- ts(log(data455$GDP), start = 1980, frequency = 1, end = 2020)
  LE_ts <-  ts(log(data455$LE), start = 1980, frequency = 1, end = 2020)
  vkgh <- coint.test(LE_ts, as.matrix(data.frame(GDP_ts)), nlag = 6, output = TRUE)
  qwer1 <- c(vkgh[1,3], qwer1)
  if (vkgh[1,3] <= 0.05) {
    con_with_coint123 <- c(con_with_coint123, i)} else {}
}
coint_with_6lag <- cbind(unique(data444$Country_name), qwer1)
colnames(coint_with_6lag) <- (c("Country", "Engle-Granger test with 6 lags LE~GDP"))
View(coint_with_6lag)

####coint_with_7lag####
data444 <- read_excel("Desktop/data44.xlsx")
con_with_coint123 <- NULL
qwer1 <- NULL

for (i in 1:137){
  data455 <- filter(data444, Country_name == unique(data444$Country_name)[i])
  GDP_ts <- ts(log(data455$GDP), start = 1980, frequency = 1, end = 2020)
  LE_ts <-  ts(log(data455$LE), start = 1980, frequency = 1, end = 2020)
  vkgh <- coint.test(LE_ts, as.matrix(data.frame(GDP_ts)), nlag = 7, output = TRUE)
  qwer1 <- c(vkgh[1,3], qwer1)
  if (vkgh[1,3] <= 0.05) {
    con_with_coint123 <- c(con_with_coint123, i)} else {}
}
coint_with_7lag <- cbind(unique(data444$Country_name), qwer1)
colnames(coint_with_7lag) <- c("Country", "Engle-Granger test with 7 lags LE~GDP")
View(coint_with_7lag)


####результаты с ECM коэффициентами####
#log(GDP total) ~ log(LE) + log(POP) 
data44 <- read_excel("Desktop/data44.xlsx")
country_list <- unique(data44$Country_name)
table_1 <- as.table(rep(1, times = 6), ncol = 6, byrow = true)
data44$GDP <- log(data44$GDP)
data44$Population <- log(data44$Population)
data44$GDP_per_capita <- log(data44$GDP_per_capita)
data44$LE <- log(data44$LE)
for (i in 1:137) {
  print(i)
  data45 <- filter(data44, Country_name == unique(data44$Country_name)[i])
  mod_ecm_by_country <- ecm(data45$GDP, as.matrix(data45[, -c(1, 2, 3, 5, 7)]))
  table_1 <- rbind(table_1, c(mod_ecm_by_country$coefficients[,1], mod_ecm_by_country$coefficients[,4]))
}
table_1 <- table_1[-1,]
colnames(table_1) <- c("LE", "POP", "ECM", "pvalue1", "pvalue2", "pvalue3")
table_2 <- as.data.frame(cbind(table_1, country_list))
table_3 <- table_2[c("country_list", "LE", "pvalue1","POP", "pvalue2","ECM", "pvalue3")]

ECM_GDP <- mutate(table_3, pvalue1 = case_when(pvalue1 < 0.01 ~ 3, pvalue1 < 0.05 ~ 2, pvalue1 < 0.1 ~ 1, TRUE ~ 0),
                  pvalue2 = case_when(pvalue2 < 0.01 ~ 3, pvalue2 < 0.05 ~ 2, pvalue2 < 0.1 ~ 1, TRUE ~ 0), 
                  pvalue3 = case_when(pvalue3 < 0.01 ~ 3, pvalue3 < 0.05 ~ 2, pvalue3 < 0.1 ~ 1, TRUE ~ 0))

pval1 <- ifelse(ECM_GDP$pvalue1 == 3, "***", ifelse(ECM_GDP$pvalue1 == 2, "**", ifelse(ECM_GDP$pvalue1 == 1, "*", "")))
pval2 <- ifelse(ECM_GDP$pvalue2 == 3, "***", ifelse(ECM_GDP$pvalue2 == 2, "**", ifelse(ECM_GDP$pvalue2 == 1, "*", "")))
pval3 <- ifelse(ECM_GDP$pvalue3 == 3, "***", ifelse(ECM_GDP$pvalue3 == 2, "**", ifelse(ECM_GDP$pvalue3 == 1, "*", "")))
ECM_GDP <- data.frame(cbind(ECM_GDP$country_list, ECM_GDP$LE, pval1, ECM_GDP$POP, pval2, ECM_GDP$ECM, pval3))
colnames(ECM_GDP) <- c("Country", "LE", "pvalue","POP", "pvalue","ECM", "pvalue")
                           
                           
####ECM в другую сторону####
#log(LE) ~ log(GDP total) + log(POP) 
data44 <- read_excel("Desktop/data44.xlsx")
country_list <- unique(data44$Country_name)
table_1 <- as.table(rep(1, times = 6), ncol = 6, byrow = true)
data44$GDP <- log(data44$GDP)
data44$Population <- log(data44$Population)
data44$GDP_per_capita <- log(data44$GDP_per_capita)
data44$LE <- log(data44$LE)
for (i in 1:137) {
  print(i)
  data45 <- filter(data44, Country_name == unique(data44$Country_name)[i])
  mod_ecm_by_country <- ecm(data45$LE, as.matrix(data45[, -c(1, 2, 4, 5, 7)]))
  table_1 <- rbind(table_1, c(mod_ecm_by_country$coefficients[,1], mod_ecm_by_country$coefficients[,4]))
}
table_1 <- table_1[-1,]
colnames(table_1) <- c("GDP", "POP", "ECM", "pvalue1", "pvalue2", "pvalue3")
table_2 <- as.data.frame(cbind(table_1, country_list))
table_3 <- table_2[c("country_list", "GDP", "pvalue1","POP", "pvalue2","ECM", "pvalue3")]

ECM_LE <- mutate(table_3, pvalue1 = case_when(pvalue1 < 0.01 ~ 3, pvalue1 < 0.05 ~ 2, pvalue1 < 0.1 ~ 1, TRUE ~ 0),
                  pvalue2 = case_when(pvalue2 < 0.01 ~ 3, pvalue2 < 0.05 ~ 2, pvalue2 < 0.1 ~ 1, TRUE ~ 0), 
                  pvalue3 = case_when(pvalue3 < 0.01 ~ 3, pvalue3 < 0.05 ~ 2, pvalue3 < 0.1 ~ 1, TRUE ~ 0))

pval1 <- ifelse(ECM_LE$pvalue1 == 3, "***", ifelse(ECM_LE$pvalue1 == 2, "**", ifelse(ECM_LE$pvalue1 == 1, "*", "")))
pval2 <- ifelse(ECM_LE$pvalue2 == 3, "***", ifelse(ECM_LE$pvalue2 == 2, "**", ifelse(ECM_LE$pvalue2 == 1, "*", "")))
pval3 <- ifelse(ECM_LE$pvalue3 == 3, "***", ifelse(ECM_LE$pvalue3 == 2, "**", ifelse(ECM_LE$pvalue3 == 1, "*", "")))
ECM_LE <- data.frame(cbind(ECM_LE$country_list, ECM_LE$GDP, pval1, ECM_LE$POP, pval2, ECM_LE$ECM, pval3))
colnames(ECM_LE) <- c("Country", "GDP", "pvalue","POP", "pvalue","ECM", "pvalue")



####coint_with_1lag в обратную сторону####
data444 <- read_excel("Desktop/data44.xlsx")
con_with_coint123 <- NULL
qwer1 <- NULL

for (i in 1:137){
  data455 <- filter(data444, Country_name == unique(data444$Country_name)[i])
  GDP_ts <- ts(log(data455$GDP), start = 1980, frequency = 1, end = 2020)
  LE_ts <-  ts(log(data455$LE), start = 1980, frequency = 1, end = 2020)
  vkgh <- coint.test(GDP_ts, as.matrix(data.frame(LE_ts)), nlag = 1, output = TRUE)
  qwer1 <- c(vkgh[1,3], qwer1)
  if (vkgh[1,3] <= 0.05) {
    con_with_coint123 <- c(con_with_coint123, i)} else {}
}
coint_with_1lag_back <- cbind(unique(data444$Country_name), qwer1)
colnames(coint_with_1lag_back) <- (c("Country", "Engle-Granger test with 1 lag GDP~LE"))
View(coint_with_1lag_back)

####coint_with_2lag####
data444 <- read_excel("Desktop/data44.xlsx")
con_with_coint123 <- NULL
qwer1 <- NULL

for (i in 1:137){
  data455 <- filter(data444, Country_name == unique(data444$Country_name)[i])
  GDP_ts <- ts(log(data455$GDP), start = 1980, frequency = 1, end = 2020)
  LE_ts <-  ts(log(data455$LE), start = 1980, frequency = 1, end = 2020)
  vkgh <- coint.test(GDP_ts, as.matrix(data.frame(LE_ts)), nlag = 2, output = TRUE)
  qwer1 <- c(vkgh[1,3], qwer1)
  if (vkgh[1,3] <= 0.05) {
    con_with_coint123 <- c(con_with_coint123, i)} else {}
}
coint_with_2lag_back <- cbind(unique(data444$Country_name), qwer1)
colnames(coint_with_2lag_back) <- (c("Country", "Engle-Granger test with 2 lags GDP~LE"))
View(coint_with_2lag_back)

####coint_with_3lag####
data444 <- read_excel("Desktop/data44.xlsx")
con_with_coint123 <- NULL
qwer1 <- NULL

for (i in 1:137){
  data455 <- filter(data444, Country_name == unique(data444$Country_name)[i])
  GDP_ts <- ts(log(data455$GDP), start = 1980, frequency = 1, end = 2020)
  LE_ts <-  ts(log(data455$LE), start = 1980, frequency = 1, end = 2020)
  vkgh <- coint.test(GDP_ts, as.matrix(data.frame(LE_ts)), nlag = 3, output = TRUE)
  qwer1 <- c(vkgh[1,3], qwer1)
  if (vkgh[1,3] <= 0.05) {
    con_with_coint123 <- c(con_with_coint123, i)} else {}
}
coint_with_3lag_back <- cbind(unique(data444$Country_name), qwer1)
colnames(coint_with_3lag_back) <- (c("Country", "Engle-Granger test with 3 lags GDP~LE"))
View(coint_with_3lag_back)

####coint_with_4lag####
data444 <- read_excel("Desktop/data44.xlsx")
con_with_coint123 <- NULL
qwer1 <- NULL

for (i in 1:137){
  data455 <- filter(data444, Country_name == unique(data444$Country_name)[i])
  GDP_ts <- ts(log(data455$GDP), start = 1980, frequency = 1, end = 2020)
  LE_ts <-  ts(log(data455$LE), start = 1980, frequency = 1, end = 2020)
  vkgh <- coint.test(GDP_ts, as.matrix(data.frame(LE_ts)), nlag = 4, output = TRUE)
  qwer1 <- c(vkgh[1,3], qwer1)
  if (vkgh[1,3] <= 0.05) {
    con_with_coint123 <- c(con_with_coint123, i)} else {}
}
coint_with_4lag_back <- cbind(unique(data444$Country_name), qwer1)
colnames(coint_with_4lag_back) <- (c("Country", "Engle-Granger test with 4 lags GDP~LE"))
View(coint_with_4lag_back)

####coint_with_5lag####
data444 <- read_excel("Desktop/data44.xlsx")
con_with_coint123 <- NULL
qwer1 <- NULL

for (i in 1:137){
  data455 <- filter(data444, Country_name == unique(data444$Country_name)[i])
  GDP_ts <- ts(log(data455$GDP), start = 1980, frequency = 1, end = 2020)
  LE_ts <-  ts(log(data455$LE), start = 1980, frequency = 1, end = 2020)
  vkgh <- coint.test(GDP_ts, as.matrix(data.frame(LE_ts)), nlag = 5, output = TRUE)
  qwer1 <- c(vkgh[1,3], qwer1)
  if (vkgh[1,3] <= 0.05) {
    con_with_coint123 <- c(con_with_coint123, i)} else {}
}
coint_with_5lag_back <- cbind(unique(data444$Country_name), qwer1)
colnames(coint_with_5lag_back) <- (c("Country", "Engle-Granger test with 5 lags GDP~LE"))
View(coint_with_5lag_back)

####coint_with_6lag####
data444 <- read_excel("Desktop/data44.xlsx")
View(data444)
con_with_coint123 <- NULL
qwer1 <- NULL

for (i in 1:137){
  data455 <- filter(data444, Country_name == unique(data444$Country_name)[i])
  GDP_ts <- ts(log(data455$GDP), start = 1980, frequency = 1, end = 2020)
  LE_ts <-  ts(log(data455$LE), start = 1980, frequency = 1, end = 2020)
  vkgh <- coint.test(GDP_ts, as.matrix(data.frame(LE_ts)), nlag = 6, output = TRUE)
  qwer1 <- c(vkgh[1,3], qwer1)
  if (vkgh[1,3] <= 0.05) {
    con_with_coint123 <- c(con_with_coint123, i)} else {}
}
coint_with_6lag_back <- cbind(unique(data444$Country_name), qwer1)
colnames(coint_with_6lag_back) <- (c("Country", "Engle-Granger test with 6 lags GDP~LE"))
View(coint_with_6lag_back)

####coint_with_7lag####
data444 <- read_excel("Desktop/data44.xlsx")
View(data444)
con_with_coint123 <- NULL
qwer1 <- NULL

for (i in 1:137){
  data455 <- filter(data444, Country_name == unique(data444$Country_name)[i])
  GDP_ts <- ts(log(data455$GDP), start = 1980, frequency = 1, end = 2020)
  LE_ts <-  ts(log(data455$LE), start = 1980, frequency = 1, end = 2020)
  vkgh <- coint.test(GDP_ts, as.matrix(data.frame(LE_ts)), nlag = 7, output = TRUE)
  qwer1 <- c(vkgh[1,3], qwer1)
  if (vkgh[1,3] <= 0.05) {
    con_with_coint123 <- c(con_with_coint123, i)} else {}
}
coint_with_7lag_back <- cbind(unique(data444$Country_name), qwer1)
colnames(coint_with_7lag_back) <- c("Country", "Engle-Granger test with 7 lags GDP~LE")
View(coint_with_7lag_back)


####общая таблица с резами тестов####
comparative_table <- cbind(coint_with_1lag, coint_with_2lag[,2], coint_with_3lag[,2], 
                           coint_with_4lag[,2], coint_with_5lag[,2], coint_with_6lag[,2], 
                           coint_with_7lag[,2], ECM_GDP[,c(6,7)], ECM_LE[,c(6,7)], coint_with_1lag_back[,2], 
                           coint_with_2lag_back[,2], coint_with_3lag_back[,2], coint_with_4lag_back[,2], 
                           coint_with_5lag_back[,2], coint_with_6lag_back[,2], coint_with_7lag_back[,2])
colnames(comparative_table) <- c("Country", "EG test with 1 lags", "EG test with 2 lags",
                                 "EG test with 3 lags","EG test with 4 lags","EG test with 5 lags",
                                 "EG test with 6 lags","EG test with 7 lags", "ECM GDP coef", "ECM GDP pval", 
                                 "ECM LE coef", "ECM LE pval", "EG test with 1 lags back", "EG test with 2 lags back", 
                                 "EG test with 3 lags back", "EG test with 4 lags back",  "EG test with 5 lags back", 
                                 "EG test with 6 lags back",  "EG test with 7 lags back")
View(comparative_table)
write.xlsx(comparative_table, file = "/Users/mariammaloan/Desktop/comparative_table.xlsx")


####графики для сомнительных резов теста####
{
  
  par(mfrow = c(3,3))
  dataasdf <- filter(data444,  data444$Country_name ==  "Angola")
  plot1 <- ggplot(data = dataasdf, aes( x = log(LE), y = log(GDP))) + geom_point() +geom_smooth(method = "lm") +  ggtitle(dataasdf$Country_name)

  dataasdf <- filter(data444,  data444$Country_name ==  "Antigua and Barbuda")
  plot2 <- ggplot(data = dataasdf, aes( x = log(LE), y = log(GDP))) + geom_point() +geom_smooth(method = "lm") +  ggtitle(dataasdf$Country_name)
  
  dataasdf <- filter(data444,  data444$Country_name ==  "Argentina")
  plot3 <- ggplot(data = dataasdf, aes( x = log(LE), y = log(GDP))) + geom_point() +geom_smooth(method = "lm") +  ggtitle(dataasdf$Country_name)
  
  dataasdf <- filter(data444,  data444$Country_name ==  "Bahamas, The")
  plot4 <- ggplot(data = dataasdf, aes( x = log(LE), y = log(GDP))) + geom_point() +geom_smooth(method = "lm") +  ggtitle(dataasdf$Country_name)
  
  dataasdf <- filter(data444,  data444$Country_name ==  "Bahrain")
  plot5 <- ggplot(data = dataasdf, aes( x = log(LE), y = log(GDP))) + geom_point() +geom_smooth(method = "lm") +  ggtitle(dataasdf$Country_name)
  
  dataasdf <- filter(data444,  data444$Country_name ==  "Bangladesh")
  plot6 <- ggplot(data = dataasdf, aes( x = log(LE), y = log(GDP))) + geom_point() +geom_smooth(method = "lm") +  ggtitle(dataasdf$Country_name)
  
  dataasdf <- filter(data444,  data444$Country_name ==  "Barbados")
  plot7 <- ggplot(data = dataasdf, aes( x = log(LE), y = log(GDP))) + geom_point() +geom_smooth(method = "lm") +  ggtitle(dataasdf$Country_name)
  
  dataasdf <- filter(data444,  data444$Country_name ==  "Benin")
  plot8 <- ggplot(data = dataasdf, aes( x = log(LE), y = log(GDP))) + geom_point() +geom_smooth(method = "lm") +  ggtitle(dataasdf$Country_name)
  
  dataasdf <- filter(data444,  data444$Country_name ==  "Cabo Verde")
  plot9 <- ggplot(data = dataasdf, aes( x = log(LE), y = log(GDP))) + geom_point() +geom_smooth(method = "lm") +  ggtitle(dataasdf$Country_name)
  
  dataasdf <- filter(data444,  data444$Country_name ==  "Cameroon")
  plot10 <- ggplot(data = dataasdf, aes( x = log(LE), y = log(GDP))) + geom_point() +geom_smooth(method = "lm") +  ggtitle(dataasdf$Country_name)
  
  dataasdf <- filter(data444,  data444$Country_name ==  "Chad")
  plot11 <- ggplot(data = dataasdf, aes( x = log(LE), y = log(GDP))) + geom_point() +geom_smooth(method = "lm") +  ggtitle(dataasdf$Country_name)
  
  dataasdf <- filter(data444,  data444$Country_name ==  "Chile")
  plot12 <- ggplot(data = dataasdf, aes( x = log(LE), y = log(GDP))) + geom_point() +geom_smooth(method = "lm") +  ggtitle(dataasdf$Country_name)

  dataasdf <- filter(data444,  data444$Country_name ==  "Comoros")
  plot13 <- ggplot(data = dataasdf, aes( x = log(LE), y = log(GDP))) + geom_point() +geom_smooth(method = "lm") +  ggtitle(dataasdf$Country_name)
  
  dataasdf <- filter(data444,  data444$Country_name ==  "Cyprus")
  plot14 <- ggplot(data = dataasdf, aes( x = log(LE), y = log(GDP))) + geom_point() +geom_smooth(method = "lm") +  ggtitle(dataasdf$Country_name)

  dataasdf <- filter(data444,  data444$Country_name ==  "Fiji")
  plot15 <- ggplot(data = dataasdf, aes( x = log(LE), y = log(GDP))) + geom_point() +geom_smooth(method = "lm") +  ggtitle(dataasdf$Country_name)

  dataasdf <- filter(data444,  data444$Country_name ==  "Gambia, The")
  plot16 <- ggplot(data = dataasdf, aes( x = log(LE), y = log(GDP))) + geom_point() +geom_smooth(method = "lm") +  ggtitle(dataasdf$Country_name)
  
  dataasdf <- filter(data444,  data444$Country_name ==  "Germany")
  plot17 <- ggplot(data = dataasdf, aes( x = log(LE), y = log(GDP))) + geom_point() +geom_smooth(method = "lm") +  ggtitle(dataasdf$Country_name)
  
  dataasdf <- filter(data444,  data444$Country_name ==  "Ghana")
  plot18 <- ggplot(data = dataasdf, aes( x = log(LE), y = log(GDP))) + geom_point() +geom_smooth(method = "lm") +  ggtitle(dataasdf$Country_name)
  
  dataasdf <- filter(data444,  data444$Country_name ==  "Guinea-Bissau")
  plot19 <- ggplot(data = dataasdf, aes( x = log(LE), y = log(GDP))) + geom_point() +geom_smooth(method = "lm") +  ggtitle(dataasdf$Country_name)
  
  dataasdf <- filter(data444,  data444$Country_name ==  "Haiti")
  plot20 <- ggplot(data = dataasdf, aes( x = log(LE), y = log(GDP))) + geom_point() +geom_smooth(method = "lm") +  ggtitle(dataasdf$Country_name)
  
  dataasdf <- filter(data444,  data444$Country_name ==  "Iceland")
  plot21 <- ggplot(data = dataasdf, aes( x = log(LE), y = log(GDP))) + geom_point() +geom_smooth(method = "lm") +  ggtitle(dataasdf$Country_name)
  
  dataasdf <- filter(data444,  data444$Country_name ==  "Korea, Rep.")
  plot22 <- ggplot(data = dataasdf, aes( x = log(LE), y = log(GDP))) + geom_point() +geom_smooth(method = "lm") +  ggtitle(dataasdf$Country_name)
  
  dataasdf <- filter(data444,  data444$Country_name ==  "Malaysia")
  plot23 <- ggplot(data = dataasdf, aes( x = log(LE), y = log(GDP))) + geom_point() +geom_smooth(method = "lm") +  ggtitle(dataasdf$Country_name)
  
  dataasdf <- filter(data444,  data444$Country_name ==  "Mali")
  plot24 <- ggplot(data = dataasdf, aes( x = log(LE), y = log(GDP))) + geom_point() +geom_smooth(method = "lm") +  ggtitle(dataasdf$Country_name)
  
  dataasdf <- filter(data444,  data444$Country_name ==  "Malta")
  plot25 <- ggplot(data = dataasdf, aes( x = log(LE), y = log(GDP))) + geom_point() +geom_smooth(method = "lm") +  ggtitle(dataasdf$Country_name)
  
  dataasdf <- filter(data444,  data444$Country_name ==  "New Caledonia")
  plot26 <- ggplot(data = dataasdf, aes( x = log(LE), y = log(GDP))) + geom_point() +geom_smooth(method = "lm") +  ggtitle(dataasdf$Country_name)
  
  dataasdf <- filter(data444,  data444$Country_name ==  "Nicaragua")
  plot27 <- ggplot(data = dataasdf, aes( x = log(LE), y = log(GDP))) + geom_point() +geom_smooth(method = "lm") +  ggtitle(dataasdf$Country_name)
  
  dataasdf <- filter(data444,  data444$Country_name ==  "Niger")
  plot28 <- ggplot(data = dataasdf, aes( x = log(LE), y = log(GDP))) + geom_point() +geom_smooth(method = "lm") +  ggtitle(dataasdf$Country_name)
  
  dataasdf <- filter(data444,  data444$Country_name ==  "Norway")
  plot29 <- ggplot(data = dataasdf, aes( x = log(LE), y = log(GDP))) + geom_point() +geom_smooth(method = "lm") +  ggtitle(dataasdf$Country_name)
  
  dataasdf <- filter(data444,  data444$Country_name ==  "Portugal")
  plot30 <- ggplot(data = dataasdf, aes( x = log(LE), y = log(GDP))) + geom_point() +geom_smooth(method = "lm") +  ggtitle(dataasdf$Country_name)

  dataasdf <- filter(data444,  data444$Country_name ==  "Saudi Arabia")
  plot31 <- ggplot(data = dataasdf, aes( x = log(LE), y = log(GDP))) + geom_point() +geom_smooth(method = "lm") +  ggtitle(dataasdf$Country_name)
  
  dataasdf <- filter(data444,  data444$Country_name ==  "St. Lucia")
  plot32 <- ggplot(data = dataasdf, aes( x = log(LE), y = log(GDP))) + geom_point() +geom_smooth(method = "lm") +  ggtitle(dataasdf$Country_name)
  
  dataasdf <- filter(data444,  data444$Country_name ==  "St. Vincent and the Grenadines")
  plot33 <- ggplot(data = dataasdf, aes( x = log(LE), y = log(GDP))) + geom_point() +geom_smooth(method = "lm") +  ggtitle(dataasdf$Country_name)
  
  dataasdf <- filter(data444,  data444$Country_name ==  "Suriname")
  plot34 <- ggplot(data = dataasdf, aes( x = log(LE), y = log(GDP))) + geom_point() +geom_smooth(method = "lm") +  ggtitle(dataasdf$Country_name)
  
  dataasdf <- filter(data444,  data444$Country_name ==  "Sweden")
  plot35 <- ggplot(data = dataasdf, aes( x = log(LE), y = log(GDP))) + geom_point() +geom_smooth(method = "lm") +  ggtitle(dataasdf$Country_name)
  
  dataasdf <- filter(data444,  data444$Country_name ==  "Togo")
  plot36 <- ggplot(data = dataasdf, aes( x = log(LE), y = log(GDP))) + geom_point() +geom_smooth(method = "lm") +  ggtitle(dataasdf$Country_name)
  
  dataasdf <- filter(data444,  data444$Country_name ==  "United Kingdom")
  plot37 <- ggplot(data = dataasdf, aes( x = log(LE), y = log(GDP))) + geom_point() +geom_smooth(method = "lm") +  ggtitle(dataasdf$Country_name)
  
  dataasdf <- filter(data444,  data444$Country_name ==  "Zimbabwe")
  plot38 <- ggplot(data = dataasdf, aes( x = log(LE), y = log(GDP))) + geom_point() +geom_smooth(method = "lm") +  ggtitle(dataasdf$Country_name)
  
  
  grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, ncol=3, nrow= 3)
  grid.arrange(plot10, plot11, plot12,plot13, plot14, plot15,plot16,plot17, plot18, ncol=3, nrow= 3)
  grid.arrange(plot19, plot20, plot21, plot22,plot23, plot24, plot25,plot26,plot27, ncol=3, nrow= 3)
  grid.arrange(plot28, plot29, plot30,  plot31, plot32,  plot33, plot34,  plot35,plot36, ncol=3, nrow= 3)
  grid.arrange(plot37, plot38, plot11, plot12,ncol=3, nrow= 3)
  }

data444 <- data.frame(read_excel("Desktop/data44.xlsx"))
Data1 <- data.frame(read_excel("/Users/mariammaloan/Desktop/mean 2013-2020.xlsx"))
Data123 <- filter(Data1, Country %in% data444$Country_name)
groups_of_con <- data.frame(read_excel("Desktop/countries with cointegration 3 groups.xlsx"))
View(groups_of_con)
group1 <- ifelse(Data123$Country %in% groups_of_con$countries.with.cointegration.1.group, 1, 0)
group2 <- ifelse(Data123$Country %in% groups_of_con$countries.with.cointegration.2.group, 1, 0)
group3 <- ifelse(Data123$Country %in% groups_of_con$countries.with.cointegration.3.group, 1, 0)
Data123$group1 <- group1
Data123$group2 <- group2
Data123$group3 <- group3

Data123$Population <- log(Data123$Population)
Data123$GDP <- log(Data123$GDP)
Data123$CO2 <- log(Data123$CO2)
Data123$GDP.per.capita <- log(Data123$GDP.per.capita)
datasummary_balance(~group1, Data123[, -c(1,15, 16)], output = "gt", dinm_statistic = "p.value",  
                    fmt = function(x) {round(x, 2)}) 
datasummary_balance(~group2, Data123[, -c(1,14, 16)], output = "gt", dinm_statistic = "p.value", 
                    fmt = function(x) {round(x, 2)}) 
datasummary_balance(~group3, Data123[, -c(1,14, 15)], output = "gt", dinm_statistic = "p.value", 
                    fmt = function(x) {round(x, 2)}) 

#графики для коинт и некоинт
data444 <- data.frame(read_excel("Desktop/data44.xlsx"))
groups_of_con <- data.frame(read_excel("Desktop/countries with cointegration 3 groups.xlsx"))

group1 <- ifelse(data444$Country_name %in% groups_of_con$countries.with.cointegration.1.group, 1, 0)
data444$group1 <- group1

coint_for_plots <- filter(data444, data444$group1 == 1)
coint_for_plots$LE <- log(coint_for_plots$LE)
coint_for_plots$GDP <- log(coint_for_plots$GDP)

ggplot(data = coint_for_plots, aes(x = LE, y = GDP)) + geom_point(aes(col = as.factor(Country_name)), 
                                                                  show.legend = FALSE)+ theme_bw()




data444 <- data.frame(read_excel("Desktop/data44.xlsx"))
Data1$GDP <- log(Data1$GDP)
summary(Data1)
View(Data123)

Data1 <- read_excel("/Users/mariammaloan/Desktop/mean 2013-2020.xlsx")
Data7 <- data.frame(Data1)
Data8 <- na.omit(Data7)
View(Data8)



