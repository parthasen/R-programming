###############
# 1.) indicator functions using functions outer(), sapply() and vapply()

#a.) Indicator function using outer() 

#===================
#Function outer example 
x = 1:9            # Lets create vectors x and y. 
y = 2:5 

outer (x,y,"^")   
#The function outer() takes two values x and y ( arrays, vectors ) and applies the function 
#which we assigned ( in our case - ^ which means we want to raise the x elements by the powers
#that are equal to the y elements.

#As we can see from the output table, the row names are the names of the x elements
#and the collumn names are the names of y vector. Because we did not assign any names
#to these vectors the default is showing their position in the vecotr ( [1,] , [,3], etc)
             
names(x) <- x
names(y) <- y 

#To avoid confusion we assign names that are equal to the values in the vector.

outer(y,x,"^")
#This output is much more clear.

#===================

I1=function(x,y) 
{
  
  z=numeric()
  n=length(x)
  m=length(y)
  
  names(x) <- x
  names(y) <- y
  
  A=outer(x,y,">")    ##Here we are using the function outer() checking which x
                      ##elements are greater than y
  
  A=A*1               ##We transform the logical matrix A into a numerical one
  
  for(i in 1:n)       ##With this loop we sum each rows of the matrix A. Why do we do  
                      ##this can be easily understood when interpreting the realisation of I1()
  {
  
    z[i]=sum(A[i,])
    
  }
  
  z
}

I1(c(1:6),c(1:4))

##Here we have 2 vectors - x=(1,2,3,4,5,6) and y=(1,2,3,4). The output says that the first value 
##x is strictly better ( > ) than '0' elements in the y vector. The second value is strictly 
##better than 1 value in y and so on. 

#b.) Indicator using sapply.

#==============
#Function sapply() example 

B=matrix(c(1:20),ncol=5,nrow=4)
B
#Here we have created a B matrix with some numbers. If we wanted to apply a certain function to 
#all of the elements in the matrix using a loop it would be tedious. We can use sapply(x,FUN,..),
#where x is a matrix, data.frame, vector, etc. and the FUN is the function which we apply to all
#of the elemnts in the matrix 

sapply(B,function(x) x^2)

#As we can see from the output window we have easily and successfully squared all of the elements 
#in matrix B. 

#=============

I2=function(x,y)
{

  A=sapply(y,function(y) y<x)  
  
  #This row means that we take every element from y and check whether the values are lesser 
  #than every element in x 
  A=A*1
  z=rowSums(A)
  z

}

I2(c(1:6),c(1:4))

#c.) Using vapply()

I3= function ( x,y ){
  
  A=vapply(y,function(y) y<x, numeric(length(x)) ) 
  A*1
  z=rowSums(A)
  z
  
}

## vapply(x,FUN,FUN.value) is very similar to sapply(). Vapply() needs an additional argument 
##to check whether the output is correct. Think of FUN.value argument as a fail-safe switch.

######################

#2 a.) Submatrix consisting of the collums of the original matrix that did not have NA values 

matA = function (x){
 
 x1=(!is.na(x))*1
 ex=which(x1==0,arr.ind=TRUE)[,"col"]
 x=x[,-ex]
 x

}

A=matrix(seq(1,12,1),ncol=3,nrow=4)
A[1,2]=NA
matA(A)

#b.) Now we need to delete collumns AND rows with NA values

matB = function (x){
  
  x1=(!is.na(x))*1
  exC=which(x1==0,arr.ind=TRUE)[,"col"]
  exR=which(x1==0,arr.ind=TRUE)[,1]
  
  x=x[-exR,-exC]
  x
}

B=matrix(rnorm(30), ncol=5, nrow=6 )
B[1,1]=NA
B[5,4]=NA
B

matB(B)

#####################
## Empirical Copula 
#a.)
#input u, v are in [0,1], xVec,yVec - any vectors with same length

empCopula=function(u,v,xVec,yVec) {
  
  n=length(xVec)
  r=rank(xVec)
  s=rank(yVec)
  
  I1=sapply(r,function(r) {(r/(n+1))<=u})
  I2=sapply(s,function(s) {(s/(n+1))<=v})
  
  I=I1*I2
  C=(1/n)*sum(I)
  
  out=list(I1,I2,I,C)
  names(out) <- c("I1","I2","I1&I2","Empirical Copula") 
  out
  
}


empCopula(0.8,0.5,x,y)

x=rnorm(6)
y=rnorm(6)

u=2

#b.)
## Generalized empirical copula, where u and v are vectors with the same length and all 
## the coordinates are in [0,1]

empCopulaG=function(u,v,xVec,yVec) {
  
  m=length(u)
  n=length(xVec)
  
  I1=matrix(nrow=m,ncol=n)
  I2=matrix(nrow=m,ncol=n)
 

  r=rank(xVec)
  s=rank(yVec)
  
  for( i in 1:m )
   {
  
  I1[i,]=sapply(r,function(r) {(r/(n+1))<=u[i]})
  I2[i,]=sapply(s,function(s) {(s/(n+1))<=v[i]})
  
   }
  
  I=I1*I2
  rowSums(I)/n
  
}

empCopulaG(c(0.8,0.9),c(0.5,0.3),x,y)

u=c(0.8,0.9)
r=rank(x)

