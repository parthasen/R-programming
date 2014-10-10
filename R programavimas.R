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

##Vectorize!
tmpFn1 <- function(x)x^(1:length(x))
tmpFn2 <- function(x) {
  i<-1:length(x)
  x^i/i
}
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

#6) Reduction 

quadrant = function ( x ) 
{
  a = x%%360 
  
  if((a>=0)&&(a<90)) cat( ' Kampas',x,'yra 1 ketvirtyje ' )  
     else if ((a>=90)&&(a<180)) cat( ' Kampas',x,'yra 2 ketvirtyje ' )  
        else if((a>=180)&&(a<270)) cat( ' Kampas',x,'yra 3 ketvirtyje ' )  
           else cat( ' Kampas',x,'yra 4 ketvirtyje ' )  
}
  
  
quadrant(1760)

############################

#7)
integ = function (x){    ### additional function that gives the integer part of the number 
  
  x1=as.character(x)
  n = nchar(x1)
  y=strsplit(x1,'')
  y1=y[[1]]
  
  for ( i in 1:n ){
    if((y1[i]==".") || (y1[i]==",")) { 
    a = substr(x1,start=1,stop=(i-1))
    break}
    else a=x1
    
  }
  a1=as.numeric(a)
  
   a1
}

weekday = function(day,month,year) { 
  
  if       (month==2) {m=12
                       year= year - 1}
  
  else if  (month==1) {m=11
                       year= year -1}
  
  else                m= month - 2  
  
  
  
  year1=as.character(year) 
  c=as.numeric(substr(year1,start=1,stop=2))
  y=as.numeric(substr(year1,start=3,stop=4))


 
  
  index = (integ(2.6*m - 0.2) + day + y +integ(y/4) + integ(c/4) -2*c)%%7 
  

  weekdays = c( "monday","tuesday","Wednesday","thursday","friday","saturday","sunday")
  cat (" On ", day,"/",month,"/",year, " the weekday is" , weekdays[index])
  
}


weekday(10,10,2014)

##############################
#8.)
#a.)

testloop=function (n){
    
  a=numeric()
  
  for( i in 2:(n)) 
    { 
     
    a[i]=i-1 + 2/(i-1)
     
    }
  a=a[!is.na(a)]
  a
}

testloop(10)

#b.)

testloop2 = function ( y ) {
  n=length(y)
  a=numeric()
  
  for( i in 1:n ) 
    {
    
    a[i]=exp(i)
    
    }
   
    a1=cumsum(a)
  
    a1[n]
}


testloop2(c(1:5))

##############################
#9.)a
quadmap=function(start,rho,niter){
  
  a=numeric(niter)
  a[1]=start
  
  
  for ( i in 2: niter ){
    
    a[i]=rho*a[i-1]*(1 - a[i-1])
    
  } 
  
  a
  
}
quadmap(0.0008,2,20)
tmp <- quadmap(start=0.95, rho=2.99, niter=500)
plot(tmp,type="l")
#9.)b

iter=function(start,rho){
  a=numeric()
  a[1]=start
  i=1
    repeat{ i=i+1
            a[i]=rho*a[i-1]*(1-a[i-1])
            if(abs(a[i] - a[i-1])<0.02) break
    }
  i-1
  
}

iter(0.95,2.99)

##############################
#10.) ACF for 1 and 2 lags function 

ACF = function ( x ) {
  
  n=length(x)
  r2=numeric()
  mn=mean(x)
  vard=0
  skait1=0
  skait2=0
  
  for ( i in 1:n) {
    
    vard = vard + (x[i] - mn)^2
    
  }
  
  for ( i in 2:n) {
    
    skait1= skait1+(x[i]-mn)*(x[i-1]-mn)
    
  }
  
  for ( i in 3:n) {
    
    skait2= skait2+(x[i]-mn)*(x[i-2]-mn)
    
  }
  
  
output=list(skait1/vard,skait2/vard)

for ( i in output){
  print(i)
}

}

ACF(c(1:5))

acf(c(1:5))

###

#b) generalized ACF ( harder )

GACF = function (x,k) 
{
  
  n=length(x)
  a=numeric(k)
  vard=0
  skait=0
  
  for ( i in 1:n) 
  {
    
    vard = vard + (x[i] - mn)^2
    
  }

  for(j in 1:k) 
  {
    for(i in j:n)
    {
  
     skait=skait + (x[i]-mn)*(x[i-(j-1)]-mn)
      
    }
 
  a[j]=skait/vard    
  skait=0

  }
  a
}

GACF(c(1:6),6)        ## check whether answers are good 

acf(c(1:6))           ## integrated function in R 

###################################
