

library(tidyverse)


customer<-read_csv("super_market_customer_info.csv")
receipt<-read_csv("super_market_receipt.csv")
weather<-read_csv("weather.csv")

names(customer)
#[1] "customer_id" "gender"      "generation"  "occupation" 
#[5] "reg_month"   "reg_day"  

names(receipt)
#[1] "log_month"             "log_day"              
#[3] "log_time"              "customer_id"          
#[5] "pos_large_class_code"  "pos_large_class_name" 
#[7] "pos_middle_class_code" "item"                 
#[9] "sales"                 "amount"       

names(weather)
#[1] "年月日"   "平均気温" "最高気温" "最低気温" "降水量"   "平均風速"


head(receipt,20)

receipt_squeeze <- receipt %>% select(pos_large_class_name,sales,amount) # %>% group_by(log_month)
summary(receipt_squeeze)


cus_and_rec <- left_join(receipt,customer,key="customer_id") #%>% group_by(customer_id)
#joinの対象を間違えるとNAが発生する

head(cus_and_rec)

#install.packages("skimr")
library(skimr)
skim(cus_and_rec)


cus_and_rec %>% filter(pos_large_class_name=="パン") %>%
             summarise(SALES_MEAN=mean(sales),
                          SALES_SD=sd(sales),
                          SALES_MED=median(sales),
                          AMOUNT_MEAN=mean(amount),
                          AMOUNT_SD=sd(amount),
                          AMOUNT_MED=median(amount),
                          count=n())

################

#欠損の確認
#cus_and_rec[is.na(cus_and_rec$sales),]
#cus_and_rec[!is.na(cus_and_rec$sales),]
library(lubridate)


log_mon_gen<- cus_and_rec %>% group_by(log_month,gender) %>% summarise(SUM_sales=sum(sales))

g <- ggplot(log_mon_gen, aes(x = ymd(log_month), y = SUM_sales, group = gender, color = as.factor(gender)))
g <- g + geom_line()
plot(g)


log_mon_gene<- cus_and_rec %>% group_by(log_month,generation) %>% summarise(SUM_sales=sum(sales))

g <- ggplot(log_mon_gene, aes(x = ymd(log_month), y = SUM_sales, group = generation, color = as.factor(generation)))
g <- g + geom_line()
plot(g)




##
item_cross <- cus_and_rec %>% group_by(pos_large_class_name,gender,generation) %>% summarise(SUM_sales=sum(sales))
item_cross$gen_gene <-paste0(item_cross$gender,item_cross$generation)



item_cross%>%group_by(generation)%>%summarise(SUM=sum(SUM_sales))%>%arrange(SUM)
item_cross%>%group_by(gender)%>%summarise(SUM=sum(SUM_sales))%>%arrange(SUM)



#install.packages("ggthemes")
library(ggsci)
library(ggthemes)


item_cross_gender <- item_cross %>% group_by(pos_large_class_name,gender) %>% summarise(SUM_sales=sum(SUM_sales))

g <- ggplot(item_cross_gender, aes(x = pos_large_class_name, y = SUM_sales, fill = gender))
#g <- g + geom_bar(stat = "identity")#積み上げで見たいならこちら
g <- g + geom_bar(stat = "identity",position = "dodge")#相対的に比較する場合こちら
g <- g + scale_fill_nejm()

g <- g+theme_fivethirtyeight()+scale_colour_fivethirtyeight()
g <- g+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot(g)



item_cross_generation <- item_cross %>% group_by(pos_large_class_name,generation) %>% summarise(SUM_sales=sum(SUM_sales))

g <- ggplot(item_cross_generation, aes(x = pos_large_class_name, y = SUM_sales, fill = as.factor(generation)))
g <- g + geom_bar(stat = "identity",position = "dodge")
g <- g + scale_fill_nejm()
g <- g+theme_fivethirtyeight()+scale_colour_fivethirtyeight()
g <- g+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot(g)




####時系列をもっと細かく

log_mon_gen_gene <- cus_and_rec %>% group_by(log_month,gender,generation) %>% summarise(SUM_sales=sum(sales))
log_mon_gen_gene$gen_gene <- paste0(log_mon_gen_gene$gender,log_mon_gen_gene$generation)

g <- ggplot(log_mon_gen_gene, aes(x = ymd(log_month), y = SUM_sales, group = gen_gene, color = as.factor(gen_gene)))
g <- g + geom_line()
plot(g)



##一旦40代に絞る

cross_40 <- item_cross %>% filter(generation==40)

g <- ggplot(cross_40, aes(x = pos_large_class_name, y = SUM_sales, fill = gender))
#g <- g + geom_bar(stat = "identity")#積み上げで見たいならこちら
g <- g + geom_bar(stat = "identity",position = "dodge")#相対的に比較する場合こちら
g <- g + scale_fill_nejm()

g <- g+theme_fivethirtyeight()+scale_colour_fivethirtyeight()
g <- g+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot(g)



####



cus_and_rec$value <- cus_and_rec$amount * cus_and_rec$sales

sales_par_unit <- cus_and_rec %>% filter(gender=="F") %>% group_by(pos_large_class_name,generation) %>% summarise(SUM_value=sum(value),count=n()) 

sales_par_unit$spu <- sales_par_unit$SUM_value / sales_par_unit$count

g <- ggplot(sales_par_unit, aes(x = pos_large_class_name, y = spu, fill = as.factor(generation)))
g <- g + geom_bar(stat = "identity",position = "dodge")#相対的に比較する場合こちら
g <- g + scale_fill_nejm()
g <- g+theme_fivethirtyeight()+scale_colour_fivethirtyeight()
g <- g+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot(g)






##########################






cus_and_rec <- left_join(receipt,customer,key="customer_id") #%>% group_by(customer_id)


user_come <- cus_and_rec %>% group_by(customer_id) %>% summarise(count=n()) %>% arrange(count)

hist(user_come$count)

max(user_come$count)

hist(user_come$count[user_come$count<100])



user_come_U20 <- user_come[user_come$count<20,]
user_come_O20 <- user_come[user_come$count>=20,]






item_U20 <- cus_and_rec %>% filter(customer_id %in% user_come_U20$customer_id)
item_O20 <- cus_and_rec %>% filter(customer_id %in% user_come_O20$customer_id)



item_U20_sum <- item_U20 %>% group_by(pos_large_class_name) %>% summarise(amount=sum(amount)) %>% mutate(type="U20")
item_O20_sum <- item_O20 %>% group_by(pos_large_class_name) %>% summarise(amount=sum(amount)) %>% mutate(type="O20")


item_U20_O20_bind <- rbind(item_U20_sum,item_O20_sum)




g <- ggplot(item_U20_O20_bind, aes(x = pos_large_class_name, y = amount, fill = as.factor(type)))
g <- g + geom_bar(stat = "identity",position = "dodge")#相対的に比較する場合こちら
g <- g + scale_fill_nejm()
g <- g+theme_fivethirtyeight()+scale_colour_fivethirtyeight()
g <- g+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot(g)


#常連客でもたばこと米と雑貨はあまり購入しない
#ABC分析の一環として、20回以下の来店は切り捨てしてみるか？



cus_and_rec_count <- left_join(cus_and_rec,user_come,key="coustomer_id")
cus_and_rec_count$value <-cus_and_rec_count$amount * cus_and_rec_count$sales
cus_and_rec_count_del <- cus_and_rec_count %>% select(-log_day,-log_time,-pos_large_class_code,-item,-sales,-amount,-pos_middle_class_code)



library(rpart)
library(rpart.plot)
library(partykit)

rpart_model <- 
cus_and_rec_count_del %>%
  rpart(
    formula =  value~ .,
    data = .
  )


par(xpd = NA)
plot(as.party(rpart_model))



