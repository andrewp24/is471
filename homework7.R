library(numbers)
library(compositions)

##### Question 1 #####

encrypt = function(M, e, N){
  modpower(M, e, N)
}

p = Primes(150,170)[1] #151
q = Primes(290,350)[1] #293
phi = (p-1)*(q-1)     #43800
e = round(runif(1, 1, phi))
N = p*q #44243
i = 0
while (i < phi){
  if (coprime(e, phi)){
    print(e)
    print("good")
    C=encrypt(8, e, N)
    i=phi
    
  } else{
    print(e)
    print("bad")
    
    e=e+1
    
  }
  i=i+1
}

print(C) #23726
                #because e is random, pretty much everything will be different from the comments

##### Question 2 #####

decrypt = function(C, d, N){
  modpower(C, d, N)
}

d = modinv(e,phi) #39353

M = decrypt(C, d, N) #65, 116, 116, 97, 99 ...

##### Question 3 #####

M = "Attack at 1300 hours"
M = utf8ToInt(M)
encrypted_m = c()
for (i in 1:length(M)){
  encrypted_m[i] = encrypt(M[i], e, N)
}

print(encrypted_m)

##### Question 4 #####
decrypted_m = c()

for (i in 1:length(encrypted_m)){
  decrypted_m[i] = decrypt(encrypted_m[i], d, N)
}
print(decrypted_m)
decrypted_m = intToUtf8(decrypted_m)

print(decrypted_m) #Attack at 1300 hours

##### Question 5 #####

g = 8
p = 23
a = 5
b = 10 

aToB = modpower(g, a, p) # 16
bToA = modpower(g, b, p) # 3
shared_key = modpower(g, a*b, p) # 13

##### Question 6 #####

message = utf8ToInt("Meet me at noon")

enc = bitwXor(message, shared_key)
print(intToUtf8(enc)) #@hhy-`h-ly-cbbc

##### Question 7 #####

i=1
while (i <= p){
  test = modpower(g, i, p)
  if (test == aToB){
    foundA = i
    i = p+1
  }
  i = i+1
}
print(foundA) # 5, which is the same as the a we used. If I used the which() function after making a vector inside the if, I could get all the possible answers.
trudyA = modpower(g, foundA, p) #16

