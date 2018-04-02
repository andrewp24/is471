library(numbers)

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
  return(round(runif(1,min=vec_sum, max=vec_sum^2)))
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

gk=function(s,m,n){
  
}