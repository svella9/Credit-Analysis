print("Percentage observations Having")

print("Bad credits and good credits are")
for(i in (table(c))){
  print((i/1000)*100)
}

print(" Account Balance as \n No Account, None, Below 200DM(<), Above 200DM(>=)")
for(i in (table(ab))){
  print((i/1000)*100)
}


print(" Payment status of previous credits- Delayed,Other credits,
	Paid up, No Problem with Currrent credits, Previous Credits Paid")
for(i in (table(pspc))){
  cat(sprintf("\"%0.2f\"\t",(i/1000)*100))
  
}

print(" Savings/Stock value- None,Below 100DM, (100,500),(500,1000),Above 1000")
for(i in (table(vss))){
  cat(sprintf("\"%0.2f\"\t",(i/1000)*100))
}


print("Length of current Employment-  Unemployed, < 1 year, [1,4),[4,7),Above 7")
for(i in (table(lce))){
  cat(sprintf("\"%0.2f\"\t",(i/1000)*100))
}

print("Installment %-  Above 35%, [25,35), [20,25),Below 20%")
for(i in (table(ipc))){
  cat(sprintf("\"%0.2f\"\t",(i/1000)*100))
}

print("Occupation- Unemployed unskilled, Unskilled Permanent resident,Skilled,Executive")
for(i in (table(o))){
  cat(sprintf("\"%0.2f\"\t",(i/1000)*100))
}

print("Sex and Martial- Status Male Divirced,Male Single, Male married/widowed, Female")
for(i in (table(sms))){
  cat(sprintf("\"%0.2f\"\t",(i/1000)*100))
}

print("Duration in Current Addresss < 1 year, [1,4),[4,7),Above 7")
for(i in (table(dca))){
  cat(sprintf("\"%0.2f\"\t",(i/1000)*100))
}

print("Type of appartment Free, Rented, Owned")
for(i in (table(toa))){
  cat(sprintf("\"%0.2f\"\t",(i/1000)*100))
}

print("Most valuable asset None, Car, Life Insurence, Real Estate")
for(i in (table(mva))){
  cat(sprintf("\"%0.2f\"\t",(i/1000)*100))
}

print("No of Credits At Bank 1, 2 or 3, 4 or 5, Above")
for(i in (table(ncab))){
  cat(sprintf("\"%0.2f\"\t",(i/1000)*100))
}

print("Gurantator- None, Co-applicat,Gurantator")
for(i in (table(g))){
  cat(sprintf("\"%0.2f\"\t",(i/1000)*100))
}

print("Concurrent Credits- Other banks, Dept Stores, None")
for(i in (table(cc))){
  cat(sprintf("\"%0.2f\"\t",(i/1000)*100))
}

print("No of Dependents-  >= 3, < 3")
for(i in (table(nod))){
  cat(sprintf("\"%0.2f\"\t",(i/1000)*100))
}

print("Telephone-  Yes, No")
for(i in (table(tp))){
  cat(sprintf("\"%0.2f\"\t",(i/1000)*100))
}

print("Foreign Worker- Yes, No")
for(i in (table(fw))){
  cat(sprintf("\"%0.2f\"\t",(i/1000)*100))
}






CrossTable(c,acb, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

