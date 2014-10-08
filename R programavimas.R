##
## Exercises are from file "rexercises.pdf" 
##

##
#WARM UP
##


#1. Create the vectors:
#(a) (1; 2; 3; : : : ; 19; 20)
#(b) (20; 19; : : : ; 2; 1)
#(c) (1; 2; 3; : : : ; 19; 20; 19; 18; : : : ; 2; 1)
#(d) (4; 6; 3) and assign it to the name tmp.
#For parts (e), (f) and (g) look at the help for the function rep.
#(e) (4; 6; 3; 4; 6; 3; : : : ; 4; 6; 3) where there are 10 occurrences of 4.
#(f) (4; 6; 3; 4; 6; 3; : : : ; 4; 6; 3; 4) where there are 11 occurrences of 4, 10 occurrences of 6 and 10 occur-
#  rences of 3.
#(g) (4; 4; : : : ; 4; 6; 6; : : : ; 6; 3; 3; : : : ; 3) where there are 10 occurrences of 4, 20 occurrences of 6 and 30
#occurrences of 3.

Answers 

#a) 

x=seq(1,20,1)

#b)

x1=rev(x)

#c)

C=c(x,x1)

#d)

tmp=c(4,6,3)

#e)

y=rep(tmp,10)

#f)

y1=rep(tmp,11)
y1=y1[-c(length(y1),length(y1)-1)]
y1

#g)

g=c(rep(4,10),rep(6,20),rep(3,30))

###########################################
#2. Create a vector of the values of e^x*cos(x) at x = 3; 3.1; 3.2; ... ; 6.

x=seq(3,6,0.1)

b=cos(x)

c=exp(x)

anwr=b*c

plot(anwr,type="l",lwd=3)

###########################################
##3 Simple Functions
##########################################

#1.a) 
tmpFn1 = function (x) {

  n=length(x)
  a=numeric(n)
  
  for ( i in 1:n ) 
    {
    
    a[i]=x[i]^i  
    
    }
  a

}


tmpFn2 = function (x){
  
  n=length(x)
  a=numeric(n)
  
  for ( i in 1:n ) 
  {
    
    a[i]=(x[i]^i)/i  
    
  
  }
  
  a

}

x=1:5
tmpFn1(x)
tmpFn2(x)

#b.)

tmpFn3 = function (x,n){
  
    a=numeric()
    
  for( i in 1: n ) 
    {
    
    a[i]= (x^i)/i
    
    }
  
    b=0
  
  for(i in 1:n)
  {
    
    b=b+a[i]
    
  }
  
  c=b+1
  
  c
}

tmpFn3(2,10)

#2.)

###########################

avg=function(x){
  
  n=length(x)
  a=numeric()
  
  for ( i  in 1:n) 
    {
    
    a[i]=(x[i]+x[i+1]+x[i+2])/3  
    
    }
  
  a=a[!is.na(a)]
  a
}

avg(c(1:5,6:1))

###########################
#3.)

tmpFn = function (x) {
  n=length(x)
  a=numeric()
  
  for( i in 1:n )
  {
  
    if ( x[i] < 0 ) {a[i]=x[i]^2 + 2*x[i] +3 }
    else if ( (0 <= x[i]) &&(x[i]<2)) a[i]=x[i]+3
    else a[i]=x[i]^2 +4*x[i] -7
  
  }
a
}

a=-3:3

plot(tmpFn(a),type="l")

##########################


#4.)

B = matrix( c(2, 4, 3, 1, 5, 7,-1,-2,-3,-1,-2,-5), 
     nrow=3, 
     ncol=4) 

f=function(A){
  nrows=length(A[,1])
  ncols=length(A[1,])
  
  for ( i in 1:nrows){
    for( j in 1:ncols){
      if(A[i,j]%%2 != 0 ) A[i,j] = A[i,j]*2 
      else A[i,j] = A[i,j]
      
      
    }
    
}

A
}

f(B)

##########################

#5.) 

f1=function ( n , k ) { 
  A=matrix (
    nrow = n,
    ncol = n )
  
  diag(A)=k
  
  A[!is.finite(A)] <- 0
  
  
  for ( i in 1:(n%/%2)){
    for( j in 1:(n%/%2)){
      if( A[i,j] == 2 ) 
        {
        
        A[i,j+1]=1 
        A[i+1,j]=1   
        }
      
      else A[i,j] = A[i,j]
      }
    }
  
  for ( i in (n%/%2):n){
    for( j in (n%/%2):n){
      if( A[i,j] == 2 ) 
      {
        
        A[i,j-1]=1 
        A[i-1,j]=1   
      }
      
      else A[i,j] = A[i,j]
    }
  }
  
  A
}

f1(10,2)

############################
