

#last name: Sodagartojgi
#firt name: Zahra
#Homework 2, math 327

#QUESTION 5

n<-20
p<-0.25

#a) p(x <= 6)

pbinom(6,20,0.25)

#b) p(x==6)

dbinom(6,20,0.25)

#c) p(x>=6)

pbinom(6,20,0.25, lower.tail = FALSE)


#QUESTION 6

Cars<-read_excel("C:/Users/zahra/Downloads/Cars.xlsx")

cars_raw<-Cars

library(dplyr)

#a) aggregated data

cars_agg<-dplyr::count(cars_raw,cars_raw$buying,cars_raw$maint,cars_raw$doors,cars_raw$persons,cars_raw$lug_boot,cars_raw$safety, cars_raw$Class)

#b) cross-tabulated data

x_tab<- xtabs(n ~ cars_raw$buying+cars_raw$maint , data=cars_agg)


#QUESTION 7
#create a contigency table (cross tabulated table)

table1<-table(cars_raw$lug_boot,cars_raw$safety)
table1

table2<-addmargins(table1)
table2

#Question 8

#proportion 
table_prop<-round(prop.table(table1),4)
table_prop

#row proportion
table_row_prop<-round(prop.table(table1,margin=1),4)
table_row_prop

#col proportion
table_col_prop<-round(prop.table(table1,margin=2),4)
table_col_prop


#Question 9: same as 7
#QUESTION 10

library(ggplot2)
ggplot(cars_agg,aes(fill=cars_raw$maint,y=n,x=cars_raw$buying))+geom_bar(position = "dodge",stat="identity")






