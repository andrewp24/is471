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

new_prime=Primes(new_sum,new_sum^2)
n=new_prime[14]                 #109

##### Question 5 #####

m = modinv(d, n)                #8

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
print(s1)                     #1 2 3 7 16 33 93 221

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
print(gen_knap)               #5 10 15 35 80 58 37 35

##### Question 9 #####

encrypt_knapsack=function(g,p){
  p=unlist(strsplit(p, ""))
  
  product=as.integer(g)*as.integer(p)
  encoded=sum(product)
  return(encoded)
}

C = encrypt_knapsack(gen_knap, "11100011") #374
#test below
#C = encrypt_knapsack(c(171, 325, 137, 86, 360, 344, 363, 334), "11100011")
#C = encrypt_knapsack(c(82, 123, 287, 83, 248, 373, 10, 471), "10010110")

##### Question 10 #####

new_q10_s=vector(mode= 'integer', length=8)
q10_s=c(5,6,13,31,69,127,285,684)
added=0
#testing below
#q10_s=c(2,3,7,14,30,57,120,251)
#added=0

for (i in length(q10_s):1){
  if((q10_s[i]+added)>235){
    new_q10_s[i]=0
  } else {
    added = q10_s[i] + added
    new_q10_s[i]=1
  }
}
print(new_q10_s * q10_s) # 0 6 0 31 69 127 0 0
#added has the value 233


##### Question 11 #####

#C=encrypted (which is the sum.)
#genknap= general knapsack
#sik is the private key
#public key is the general knapsack
#should output 11100011

decrypt_knapsack = function(s, C, d, n){
  #s is the pk (superincreasing)
  print(s)
  print(C)
  print(d)
  print(n)
  m=modinv(d, n)
  checkVal = mod(C * m, n)
  print(checkVal)
  decrypted = c()
  
  for (i in length(s):1){
    
    if(checkVal < s[i]){
      decrypted = c(0, decrypted)
    } else {
      checkVal = checkVal - s[i]
      decrypted = c(1, decrypted)
    }
  }
  return(decrypted)
}
decrypted=decrypt_knapsack(s1, C, d2, n2) # 0 1 1 1 0 0 1 0
