dummie_Account.Balance <- predict(dummyVars(~ Account.Balance, data = dota), newdata = dota)
dummie_Payment.Status.of.Previous.Credit <- predict(dummyVars(~ Payment.Status.of.Previous.Credit, data = dota), newdata = dota)
dummie_Purpose <- predict(dummyVars(~ Purpose, data = dota), newdata = dota)
dummie_Value.Savings.Stocks <- predict(dummyVars(~ Value.Savings.Stocks, data = dota), newdata = dota)
dummie_Length.of.current.employment <- predict(dummyVars(~ Length.of.current.employment, data = dota), newdata = dota)
dummie_Sex...Marital.Status <- predict(dummyVars(~ Sex...Marital.Status, data = dota), newdata = dota)
dummie_Guarantors <- predict(dummyVars(~ Guarantors, data = dota), newdata = dota)
dummie_Foreign.Worker <- predict(dummyVars(~ Foreign.Worker, data = dota), newdata = dota)


dota2 = dota



dota2 = dota2[,-c(1,3,4,6,7,9,10,13)]
dota2 = cbind(dota2,dummie_Account.Balance,dummie_Foreign.Worker,dummie_Guarantors,dummie_Length.of.current.employment,dummie_Payment.Status.of.Previous.Credit,dummie_Purpose,dummie_Sex...Marital.Status,dummie_Value.Savings.Stocks)
dota2 = dota2[,-6]
dota3 = dota2
dota3 = dota3[,-6]
dota4 = dota3
dota2_y = dota2[dota2$predict_rpart_tst == "Y",]
dota2_n = dota2[dota2$predict_rpart_tst == "N",]
dota2_n = dota2_n[,-6]
dota2_y = dota2_y[,-6]
a= vector()

for(i in 1:nrow(dota2_y))
  {
    a[i]=sqrt(sum((dota2_n[1,]-dota2_y[i,])^2))
  }

x= order(a)
dota2_y[x[1],]


for (i in 1:nrow(dota2))
{
  if(identical(dota3[i,],dota2_y[x[1],]))
  {
    print(i)
    z = i
  }
}

print(dota[z,])


#with decostand

library(vegan)
dota4 = decostand(dota4,method = "range")
dota2_y2 = dota2_y
dota2_y2 = decostand(dota2_y2,method = "range")

for(i in 1:nrow(dota2_y))
{
  a[i]=sqrt(sum((dota2_n[1,]-dota2_y[i,])^2))
}

x= order(a)
dota2_y[x[1],]


for (i in 1:nrow(dota2))
{
  if(identical(dota4[i,],dota2_y2[x[1],]))
  {
    print(i)
    b = i
  }
}

print(dota[b,])
