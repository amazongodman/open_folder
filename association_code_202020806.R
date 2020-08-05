#20200803 market busket

setwd("C:\\Users\\0030602\\Desktop\\MB")

#https://www.kaggle.com/philippsp/exploratory-analysis-instacart



library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)
library(stringr)
library(DT)

orders <- fread('orders.csv')
products <- fread('products.csv')
order_products <- fread('order_products__train.csv')
order_products_prior <- fread('order_products__prior.csv')
aisles <- fread('aisles.csv')
departments <- fread('departments.csv')




orders
#どの注文明細が訓練でテストで事前なのかを表している
#dowは曜日、

products
#商品名の結びつき

order_products
#顧客の今回の注文

order_products_prior
#顧客の前回の注文 reorderedは再注文したかどうかのフラグ
#愛用しているか

aisles
#商品の分類名

departments 
#商品の売り場区分



kable(head(orders,12))

glimpse(orders)

#キャラクターを数字にしたり、ファクターに変換する
orders <- orders %>% mutate(order_hour_of_day = as.numeric(order_hour_of_day), eval_set = as.factor(eval_set))
products <- products %>% mutate(product_name = as.factor(product_name))
aisles <- aisles %>% mutate(aisle = as.factor(aisle))
departments <- departments %>% mutate(department = as.factor(department))

#購入時間帯の可視化
orders %>% 
  ggplot(aes(x=order_hour_of_day)) + 
  geom_histogram(stat="count",fill="red")

#購入曜日の可視化
orders %>% 
  ggplot(aes(x=order_dow)) + 
  geom_histogram(stat="count",fill="red")


#再注文はどの周期なのか
orders %>% 
  ggplot(aes(x=days_since_prior_order)) + 
  geom_histogram(stat="count",fill="red")
#7日が多いね。あとは30日以上前


#再注文率って高いのかな？再注文回数の分布
orders %>% filter(eval_set=="prior") %>% count(order_number) %>% ggplot(aes(order_number,n)) + geom_line(color="red", size=1)+geom_point(size=2, color="red")



#何個くらい一度に購入するのか
order_products %>% 
  group_by(order_id) %>% 
  summarize(n_items = last(add_to_cart_order)) %>%
  ggplot(aes(x=n_items))+
  geom_histogram(stat="count",fill="red") + 
  geom_rug()+
  coord_cartesian(xlim=c(0,80))



#一番の売れ筋商品

tmp <- order_products %>% 
  group_by(product_id) %>% 
  summarize(count = n()) %>% 
  top_n(10, wt = count) %>%
  left_join(select(products,product_id,product_name),by="product_id") %>%
  arrange(desc(count)) 


kable(tmp)




#bar plot にする
tmp %>% 
  ggplot(aes(x=reorder(product_name,-count), y=count))+
  geom_bar(stat="identity",fill="red")+
  theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())


#同じ商品を頼むってのがあるのか
tmp <- order_products %>% 
  group_by(reordered) %>% 
  summarize(count = n()) %>% 
  mutate(reordered = as.factor(reordered)) %>%
  mutate(proportion = count/sum(count))

kable(tmp)
#59%

#barにする
tmp %>% 
  ggplot(aes(x=reordered,y=count,fill=reordered))+
  geom_bar(stat="identity")

#一番再注文されるアイテムは
tmp <-order_products %>% 
  group_by(product_id) %>% 
  summarize(proportion_reordered = mean(reordered), n=n()) %>% 
  filter(n>40) %>% 
  top_n(10,wt=proportion_reordered) %>% 
  arrange(desc(proportion_reordered)) %>% 
  left_join(products,by="product_id")

kable(tmp)

tmp %>% 
  ggplot(aes(x=reorder(product_name,-proportion_reordered), y=proportion_reordered))+
  geom_bar(stat="identity",fill="red")+
  theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())+coord_cartesian(ylim=c(0.85,0.95))

#どうやらラクトースの無い牛乳やオレンジジュースが売れてる





#一番最初にカートに入れるものは？
tmp <- order_products %>% 
  group_by(product_id, add_to_cart_order) %>% 
  summarize(count = n()) %>% mutate(pct=count/sum(count)) %>% 
  filter(add_to_cart_order == 1, count>10) %>% 
  arrange(desc(pct)) %>% 
  left_join(products,by="product_id") %>% 
  select(product_name, pct, count) %>% 
  ungroup() %>% 
  top_n(10, wt=pct)

kable(tmp)


tmp %>% 
  ggplot(aes(x=reorder(product_name,-pct), y=pct))+
  geom_bar(stat="identity",fill="red")+
  theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())+coord_cartesian(ylim=c(0.4,0.7))
#タオルが一番最初にカートに突っ込まれる



order_products %>% 
  left_join(orders,by="order_id") %>% 
  group_by(days_since_prior_order) %>%
  summarize(mean_reorder = mean(reordered)) %>%
  ggplot(aes(x=days_since_prior_order,y=mean_reorder))+
  geom_bar(stat="identity",fill="red")


#再注文はその日のうちが最も注文され、時間がたつと、同じ商品は再注文されなくなる？



#注文数と再注文数の関係性分析
#過去に沢山購入された商品は、再注文が多くなるが、天井効果もある

order_products %>% 
  group_by(product_id) %>% 
  summarize(proportion_reordered = mean(reordered), n=n()) %>%
  ggplot(aes(x=n,y=proportion_reordered))+
  geom_point()+
  geom_smooth(color="red")+
  coord_cartesian(xlim=c(0,2000))

#有機栽培はうれるのか

products <- products %>% 
  mutate(organic=ifelse(str_detect(str_to_lower(products$product_name),'organic'),"organic","not organic"), organic= as.factor(organic))

tmp <- order_products %>% 
  left_join(products, by="product_id") %>% 
  group_by(organic) %>% 
  summarize(count = n()) %>% 
  mutate(proportion = count/sum(count))

kable(tmp)

tmp %>% 
  ggplot(aes(x=organic,y=count, fill=organic))+
  geom_bar(stat="identity")



tmp <- order_products %>% left_join(products,by="product_id") %>% group_by(organic) %>% summarize(mean_reordered = mean(reordered))

kable(tmp)

#再注文のうちで有機が含まれるのは多いのか

tmp %>% 
  ggplot(aes(x=organic,fill=organic,y=mean_reordered))+geom_bar(stat="identity")



#ツリーマップで部門と製品区分を可視化する

#install.packages("treemap")
library(treemap)

tmp <- products %>% group_by(department_id, aisle_id) %>% summarize(n=n())
tmp <- tmp %>% left_join(departments,by="department_id")
tmp <- tmp %>% left_join(aisles,by="aisle_id")

tmp2<-order_products %>% 
  group_by(product_id) %>% 
  summarize(count=n()) %>% 
  left_join(products,by="product_id") %>% 
  ungroup() %>% 
  group_by(department_id,aisle_id) %>% 
  summarize(sumcount = sum(count)) %>% 
  left_join(tmp, by = c("department_id", "aisle_id")) %>% 
  mutate(onesize = 1)




treemap(tmp2,index=c("department","aisle"),vSize="onesize",vColor="department",palette="Set3",title="",sortID="-sumcount", border.col="#FFFFFF",type="categorical", fontsize.legend = 0,bg.labels = "#FFFFFF")



#商品の数　種類
treemap(tmp,index=c("department","aisle"),vSize="n",title="",palette="Set3",border.col="#FFFFFF")



#売り上げに対してのtree
treemap(tmp2,index=c("department","aisle"),vSize="sumcount",title="",palette="Set3",border.col="#FFFFFF")





#いつも同じ商品を買う顧客をさがす
#習慣的に購入する人をさがすために


tmp <- order_products_prior %>% 
  group_by(order_id) %>% 
  summarize(m = mean(reordered),n=n()) %>% 
  right_join(filter(orders,order_number>2), by="order_id")

tmp2 <- tmp %>% 
  filter(eval_set =="prior") %>% 
  group_by(user_id) %>% 
  summarize(n_equal = sum(m==1,na.rm=T), percent_equal = n_equal/n()) %>% 
  filter(percent_equal == 1) %>% 
  arrange(desc(n_equal))

datatable(tmp2, class="table-condensed", style="bootstrap", options = list(dom = 'tp'))


#一番再注文してくれている人のidが分かったので絞ってみる
#絞って何を購入しているのかを確認する

uniqueorders <- filter(tmp, user_id == 99753)$order_id
tmp <- order_products_prior %>% 
  filter(order_id %in% uniqueorders) %>% 
  left_join(products, by="product_id")

datatable(select(tmp,-aisle_id,-department_id,-organic), style="bootstrap", class="table-condensed", options = list(dom = 'tp'))

#どうやらオーガニック牛乳を買っている様子



tmp <- orders %>% filter(user_id==99753, eval_set == "train")
tmp2 <- order_products %>%  
  filter(order_id == tmp$order_id) %>% 
  left_join(products, by="product_id")

datatable(select(tmp2,-aisle_id,-department_id,-organic), style="bootstrap", class="table-condensed", options = list(dom = 't'))




















install.packages("RColorBrewer")
#Author Vivek Mangipudi : 

require(tidyverse)
require(plyr)
require(dplyr)

require(readr)
require(arules)
require(arulesViz)


require(splitstackshape) # this is a really cool package I discovered today while trying to reshape data
require(RColorBrewer)




mydata<-read.csv("order_products__prior.csv", nrows=20000)
products<-read_csv("products.csv")




#Prior Orders
(mydata[1:10,])
####Products
products[1:10,]




#picking up just product id and order id, later joining with products
mydata<-mydata[,1:2] #picking up first two rows
mydata<-merge(mydata,products,by="product_id") #merging
mydata<-arrange(mydata, order_id) # ascending order
mydata<-mydata[,c(2,3)] #dropping other columns
mydata[1:10,] # sneak peek




#drop columns if you want, as we will be dealing with only order id and product name/id. 
# will be choosing product name for sake of readability.
#Splitting the data :
dt <- split(mydata$product_name, mydata$order_id)




# Converting data to a class of transactions
dt2 = as(dt,"transactions")
#summary of this new dt2, uncomment if you want to,

#summary(dt2) # t
# sneak peek into  dt2 uncomment below line on your local machine, here this thing crashes which is a bummer.
#inspect(dt2)





#Let us look at frequencies:, uncomment if you want
#itemFrequency(dt2, type = "relative")
#Plotting 
itemFrequencyPlot(dt2,topN=20,type="absolute")


#Creating rules using apriori algorithm:

#rules = apriori(dt2, parameter=list(support=0.001, confidence=0.8))
#plot(rules,control=list(col=brewer.pal(11,"Spectral")),main="")
rules = apriori(dt2, parameter=list(support=0.001, confidence=0.8, minlen = 3))
#rules = apriori(dt2, parameter=list(support=0.001, confidence=0.8, maxlen = 4))
plot(rules,control=list(col=brewer.pal(11,"Spectral")),main="")






#converting rules into the omnipotent & omnipresent data frame
rules3 = as(rules, "data.frame")


inspect( subset( rules, subset = rhs %pin% "Banana" ))



#Look at rules
options(digits=2)
inspect(head(sort(rules, by ="lift"),5))



# Get Summary Information
summary(rules)

# Sort by Lift
rules<-sort(rules, by="lift", decreasing=TRUE)
plot(rules,control=list(col=brewer.pal(11,"Spectral")),main="")













# Remove Unnecessary Rules
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
rules.pruned <- rules[!redundant]
rules<-rules.pruned

#Clean Rules
rules3$rules=gsub("\\{", "", rules3$rules)
rules3$rules=gsub("\\}", "", rules3$rules)
rules3$rules=gsub("\"", "", rules3$rules)

#Split the rule
library(splitstackshape)
Rules4=cSplit(rules3, "rules","=>")
names(Rules4)[names(Rules4) == 'rules_1'] <- 'LHS'
Rules5=cSplit(Rules4, "LHS",",")
Rules6=subset(Rules5, select= -c(rules_2))
names(Rules6)[names(Rules6) == 'rules_3'] <- 'RHS'















# What are customers likely to buy before they purchase "Banana"
rules<-apriori(data=dt, parameter=list(supp=0.001,conf = 0.8), 
               appearance = list(default="lhs",rhs="Banana"),
               control = list(verbose=F))

rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:10])
library("RColorBrewer")
#Scatter plot of rules
plot(rules,control=list(col=brewer.pal(11,"Spectral")),main="")

#Rules with high lift typically have low support.

#The most interesting rules reside on the support/confidence border which can be clearly seen in this plot.



#Plot graph-based visualisation:

subrules2 <- head(sort(rules, by="lift"), 10)

plot(subrules2, method="graph",control=list(type="items",main=""))
























