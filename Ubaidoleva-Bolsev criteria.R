#################
##Bolsev - Ubaidoleva Criteria
#################
#The main function is BUC ( x, P, pop.mean, pop.var ), where 
#x - a numerical vector
#P - significance level ( 95 %, 90 %, etc)
#pop.mean - population mean. If set to FALSE - the mean of the vector
#pop.var - population variance. If set to FALSE - the variance of the vector
#BUC() main function is to give the outliers in the given vector based on the method that was
#thought of by M.Ubaidulojeva and L.Bolsev. 
##
#The function Ztransf() is a function that transforms x into a standart vector. 

library(data.table)

##### Take any R function (say lm) and look at its code (you can do that by simply 
##### executing lm command in R console). That's what a tidy code should look like.
##### As you will see, all the assignments are done with "<-" and not "=" (you can 
##### read more about that here 
##### http://stackoverflow.com/questions/1741820/assignment-operators-in-r-and)

Ztransf=function (x,mean,var) 
  ### better: Ztransf <- function (x, mean, var)
{
  
v    = n-2
### what is n here? better code: v <- n - 2

z    = numeric()

#### it is a very bad practice to initiate an empty vector and then add values one
#### by one to it, because each time the vector will have to reinitialised and all 
#### the values written to it again, which would take huge amounts of time if you have
#### to do it sufficiently many times

n    = length(x)
  
#######################
  
if((mean == FALSE)&&(var==FALSE))
{    
    
  mean = mean(x)   
  s    = var(x)
  ### take a look at the standard scale function in R
    
    
  for ( i in 1:n )
  {
    z[i]=n*(1 - pt(((x[i]-mean)/sqrt(s))*sqrt(v/(v+1-((x[i]-mean)/sqrt(s))^2)),v))
  }
  ### avoid loops whenever you can - they are slow. the same thing can be done with
  ### z <- n*(1 - pt(scale(x)*sqrt(v/(v + 1 - scale(x)^2)), v))
  ### (check to make sure it really gives the same thing)
}
  
######################
  
else if ((mean!=FALSE)&&(var==FALSE))
{
    
  var=0
  for ( i in 1:n )
  {
    var=var + (x[i] -mean)^2
  }
    
  var=var/n
    
  for ( i in 1:n )
  {
    z[i]=n*(1-(pt(((x[i]-mean)/sqrt(var))*sqrt(v/(v+1-(x[i]-mean)/sqrt(s))),v+1)))
  }

}
  
#####################
  
else if ((mean!=FALSE)&&(var!=FALSE))
{ 
    
  for ( i in 1:n ) 
  {
    z[i]=n*(1 - pnorm((x[i]-mean)/sqrt(var)))
  }

}
  
######################
  
else 
{ 
    
  for ( i in 1:n ) 
  {
      
    z[i]=n*(1-pnorm(sqrt(n/(n-1))*(x[i]-mean(x))/sqrt(var)))
      
  }
    
}
#####################
  z
}


BUC=function(x,P,pop.mean,pop.var)
{
  
z=Ztransf(abs(x),pop.mean,pop.var)
  
A=matrix(c(z,x),ncol=2,nrow=(length(x)))
Sorted.matrix <- data.table(A,key="V1")
zT=Sorted.matrix$"V1"
xT=Sorted.matrix$"V2"
  
z1=numeric()
  
  
for( i in 1:length(zT))
{
  z1[i]=zT[i]/i
}
  
a=sapply(z1, function(z1) z1<= P/2 )*1
outlier=numeric() 
  
for( i in 1:length(a))
{
  if(a[i]==1)
  { 
    outlier[i]=xT[i] 
  }
}  
  
outlier=outlier[!is.na(outlier)]
if(length(outlier)==0) outlier=0
output=list( z, zT, x,xT, Sorted.matrix, outlier)  
names(output)<-c("Z values","Sorted Z values","Original values","Sorted original values","Sorted matrix","Outliers")
output

}

x=rnorm(50,mean=0,sd=2)

y=c(1,1,2,1,1,3)

x[10]=6
a=BUC(x,0.95,FALSE,FALSE)
a


