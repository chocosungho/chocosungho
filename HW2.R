#1.

#1)

x=c(17,16,20,24,22,15,21,18)

x[x>=20]

#2)

y=c()

for(i in 1:length(x))
  
{
  
  if(x[i]>=20)
    
    y[i]=100
  
  else
    
    y[i]=x[i]
  
}

y

#2.

#1)

x=diag(3:7)

for(i in 1:5)
  
{for (j in 1:5)
  
{
  
  if (x[i,j]==0)
    
    x[i,j]=-1
  
}}

x

#2)

y=x[,-5]

y

#3)

yinfo=c(nrow(y),ncol(y))    

yinfo

#4)

for(i in 1:5)
  
{for (j in 1:4)
  
{
  
  if (y[i,j]==-1)
    
    y[i,j]=0
  
}}

y



#3.

#1)

setwd("C:/Users/LSH")

A=read.table("x.txt",header =T, sep=",",stringsAsFactors= FALSE)

rdata=data.frame(A)

rdata

#2)

#is.na()

#3)

b=c()

for (i in (1:4))
  
  if (!(is.na.data.frame(rdata[i,2])) & !(is.na.data.frame(rdata[i,3])))
    
    b=c(b,i)

b

#4)

rdata1=rdata[b,]

rdata1



#4.

temp=list(c(TRUE,FALSE),diag(1,1),seq(0,1,length=100),1,2,3,4)

temp

temp[[2]]=NULL

temp[[3]]

length(temp)  



#5.

a1= -1:2

a2=1:2

a1+a2



a1= -(1:2)

a2=1:2

a1+a2

a1=matrix(0,2,2)

a2=c(3,4)

a1+a2

a1=matrix(1:4,2,2)

a1[a1>2]=0

a1

a1=1:5

a1[-1]-a1[-length(a1)]













#1.

a=vector(length=20)

a[1]=1

a[2]=3

for(i in 3:20)
  
{
  
  a[i]=0.9*a[i-1]-0.1*a[i-2]+1
  
  
  
  
  
}

a[20]

#2.

for(i in 3:20)
  
{
  
  a[i]=a[i]=0.9*a[i-1]-0.1*a[i-2]+1
  
  if (a[i]>4)
    
  {print(i)
    
    break
    
  }
  
  
  
}

#3.

a=matrix(runif(100),50,5)

v=c(rep(0,50))

for(i in 1:50)
  
{
  
  for(j in 1:5)
    
    v[i]=v[i]+a[i,j]
  
}

v

#4.

tmp=rep(0,10)

a <- 10:1

a

idx=1

for(j in a)
  
{
  
  if(j<5)
    
  {
    
    tmp[idx] <-a[j]
    
    idx <- idx +1
    
  }
  
}

tmp



#5.



x=matrix(runif(5000),1000,5)

print(x[1,])

sid=c(sample(1:10,1000,replace=TRUE))



#6-1.

m.mat=matrix(,10,5)



for (i in 1:10)
  
{
  
  m=rep(0,5)
  
  n=0
  
  for (j in 1:1000)
    
  {
    
    if (sid[j]==i)
      
    {
      
      m=m+x[j,]
      
      n=n+1
      
    }
    
  }
  
  m.mat[i,]=m/n
  
}



m.mat



#6-2.



idist=matrix(rep(0,10000),1000,10)

for(i in 1:1000)
  
  for(j in 1:10)
    
    idist[i,j]=sum((x[i,]*m.mat[j,]))/(sqrt(sum(x[i,]**2))*sqrt(sum(m.mat[j,]**2)))



head(idist)



#7.



ivec=rep(0,1000)

for (i in 1:1000)
  
  for (j in 1:10)
    
    if (idist[i,j]==min(idist[i,]))
      
      ivec[i]=j

head(ivec)







x

rowsum.default((x[1,]*m.mat[2,])
               
               ?rowSums
               
               
               
               #8.
               
               #1)
               
               set.seed(1)
               
               a=list()
               
               for(i in 1:1000)
                 
               {
                 
                 x=rpois(1,4)+1
                 
                 x=min(x,10)
                 
                 a[[i]]=sample(1:10,x)
                 
               }
               
               m=c(rep(0,9))
               
               for(i in 1:1000)
                 
               {
                 
                 for (j in 2:10)
                   
                 {
                   
                   if (length(a[[i]])==j)
                     
                     m[j-1]=m[j-1]+1
                   
                 }
                 
               }
               
               m
               
               
               
               #2)
               
               points=c(rep(0,10))               
               
               for(i in 1:1000)
                 
               {
                 
                 for(j in 1:10)
                   
                 {    if(length(a[[i]])==2|length(a[[i]])==3)
                   
                 {
                   
                   if (a[[i]][1]==j)
                     
                     points[j]=points[j]+1
                   
                 }
                   
                   else if(length(a[[i]])==4|length(a[[i]])==5|length(a[[i]])==6)
                     
                   {
                     
                     if (a[[i]][1]==j)
                       
                       points[j]=points[j]+2
                     
                     else if (a[[i]][2]==j)
                       
                       points[j]=points[j]+1
                     
                   }
                   
                   else if(length(a[[i]])>6)
                     
                   {
                     
                     if (a[[i]][1]==j)
                       
                       points[j]=points[j]+3
                     
                     else if (a[[i]][2]==j)
                       
                       points[j]=points[j]+2
                     
                     else if (a[[i]][3]==j)
                       
                       points[j]=points[j]+1
                     
                   }
                   
                 }
                 
               }
               
               points
               
               max(points)
               
               #따라서 9번이 1등
               
               
               
               #9.
               
               #1)
               
               set.seed(1)
               
               m1=10
               
               m2=5
               
               num=0
               
               
               
               for (i in 1:4)
                 
                 if(rbinom(1,1,1/2)==0)
                   
                 {   m1=m1-1
                 
                 }else
                   
                 {   m1=m1+1}
               
               m1
               
               #2)
               
               set.seed(1)
               
               m1=10
               
               m2=5
               
               num=0
               
               while (m1!=0&m2!=0)
                 
                 if(rbinom(1,1,1/2)==0)
                   
                 {   m1=m1-1
                 
                 m2=m2+1
                 
                 num=num+1
                 
                 }else
                   
                 {   m1=m1+1
                 
                 m2=m2-1
                 
                 num=num+1}
               
               num
               
               m1
               
               m2
               
               
               
               #3)
               
               testfunction=function(m2)
                 
               {
                 
                 x=0
                 
                 m3=0
                 
                 for (k in 1:200)
                   
                 {
                   
                   set.seed(k)
                   
                   m1 = 10
                   
                   m3=m2
                   
                   num = 0
                   
                   while (m1!=0&m3!=0)
                     
                   {
                     
                     if(rbinom(1,1,1/2)==0)
                       
                     {
                       
                       m1=m1-1
                       
                       m3=m3+1
                       
                       num=num+1
                       
                     }else{
                       
                       m1=m1+1
                       
                       m3=m3-1
                       
                       num=num+1}
                     
                   }
                   
                   if (m3==0)
                     
                     x=x+1
                   
                 }
                 
                 return (x)
                 
               }
               
               testfunction(m2=5)
               
               
               
               
               
               
               
               #10.
               
               
               
               testfunction(m2=10)/200
               
               testfunction(m2=15)/200
               
               testfunction(m2=20)/200
               
               testfunction(m2=25)/200
               