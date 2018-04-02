library(numbers)
library(compositions)

##### Question 1 #####

prime_numbers = Primes(40,50)
d=prime_numbers[1]              #41


##### Question 2 #####

s = c(3,25,10,2,6,16,37,8)
s_sum = sum(s)                  #107

##### Question 3 #####

P= c(1,0,1,1,0,0,1,0)
new=s*P
new_sum=sum(new)                #52

##### Question 4 #####
squared=new_sum^2

new_prime=Primes(new_sum,squared)
n=new_prime[1]

##### Question 5 #####

m = modinv(d, n)

##### Question 6 #####

sik = function(vec){
  vec_sum=sum(vec)
  return(round(runif(1,min=vec_sum, max=vec_sum^1.1)))
}

##### Question 7 #####

s1= vector(mode= 'integer', length=8)
s1[1] = 1
s1[2] = 2
rand=sik(s1)

for (i in 3:length(s1)) {
  s1[i] = sik(s1)
}
print(s1)

##### Question 8 #####

s1_sum=s1*P
s1_sum=sum(s1_sum)
increased_s1=round(s1_sum*1.1)
print(increased_s1)
n2=Primes(s1_sum+1,increased_s1)
print(n2)
n2=n2[1]
print(s1_sum)
print(n2)
d2=Primes(40, 50)
d2=d2[2]
m2=modinv(d2,n2)
new_s=vector(mode= 'integer', length=8)

gk=function(s,m,n){
  for (i in 1:length(s)){
    new_s[i]=mod(s[i]*m,n)
  }
  return(new_s)
}

#tests=c(2,3,7,14,30,57,120,251)
#test=gk(tests,41,491)
#print(test)
gen_knap=gk(s1,m2,n2)
print(gen_knap)


##### Question 9 #####
#the private key is the s1.
pk=modinv(m2,n2)
temp=vector(mode= 'integer', length=8)

encrypt_knapsack=function(g,p){
  p=unlist(strsplit(p, ""))
  tempVal=p
  tempVal=c(tempVal)
  tempVal=gsub(pattern = ", ", replacement = "", x = tempVal, fixed = TRUE)
  tempVal=strtoi(tempVal, base=2)
  
  temp=g*tempVal
  temp_sum=sum(temp)
  
  
  return(temp_sum)
}

C=encrypt_knapsack(gen_knap, "11100011") #170

##### Question 10 #####

new_q10_s=vector(mode= 'integer', length=8)
q10_s=c(5,6,13,31,69,127,285,684)
added=0
q10_s=c(2,3,7,14,30,57,120,251)
added=0

for (i in length(q10_s):1){
  if((q10_s[i]+added)>193){
    new_q10_s[i]=0
  } else {
    added = q10_s[i] + added
    new_q10_s[i]=1
  }
}
