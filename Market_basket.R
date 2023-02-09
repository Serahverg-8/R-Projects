

#install.packages('arules')
#install.packages('arulesViz')

library(arules)
library(arulesViz)
library(dplyr)

#on looking at the dataset, we see transactions. Not an ideal dataset. SO we need to use read.transactions
#skip =1 is to skip the rows.Here skip first row
read.transactions('C:\\Users\\SV077490\\OneDrive - Cerner Corporation\\Desktop\\D-O\\Projects-R\\Market Basket\\market_basket.csv',sep=",",format = 'basket',rm.duplicates = T,skip = 1)-> basket
transactions <- nrow(basket)

basket = basket[,-1]
summary(basket)


#To find total number of transaction:
  #row * column * density  from summary
items_purchased <- 18840 * 26228 *  0.0007227956 

basket %>% head(n=5) %>% inspect

#expand plot screen
itemFrequencyPlot(x=basket,type='absolute',topN=10,horiz=T)

rule1=basket%>% 
  apriori(parameter = list(supp=0.005, conf=0.8))%>% sort(by='confidence')
summary(rule1)
rule1%>% head(n=5)%>% inspect


plot(rule1,engine='htmlwidget',jitter=0)
plot(rule1,method = 'two-key',engine ='htmlwidget' )
plot(rule1,method = 'graph',engine ='htmlwidget' )


#task2
  rule2=basket%>% 
  apriori(parameter = list(supp=0.009, conf=0.3))%>% sort(by='confidence')
summary(rule2)

plot(rule2,engine='htmlwidget',jitter=0)
plot(rule2,method = 'two-key',engine ='htmlwidget' )
plot(rule2,method = 'graph',engine ='htmlwidget' )

#task3
  rule3=basket%>% 
  apriori(parameter = list(supp=0.02, conf=0.5))%>% sort(by='confidence')
summary(rule3)

plot(rule3,engine='htmlwidget',jitter=0)
plot(rule3,method = 'two-key',engine ='htmlwidget' )
plot(rule3,method = 'graph',engine ='htmlwidget' )