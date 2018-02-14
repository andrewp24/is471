########## Question 1 ##########

#a
l <- utf8ToInt('S')
key <- 5
new_l <- l-key
modded <- mod(new_l, 128)
q1a <- intToUtf8(modded) # answer is N

#b
l <- utf8ToInt('Y')
key <- 2
new_l <- l-key
modded <- mod(new_l, 128)
q1b <- intToUtf8(modded) # answer is w

#c
l <- utf8ToInt('Y')
key <- 12
new_l <- l-key
modded <- mod(new_l, 128)
q1c <- intToUtf8(modded) #answer is M

#d
l <- utf8ToInt('C')
key <- 4
new_l <- l-key
modded <- mod(new_l, 128)
q1d <- intToUtf8(modded) #We get a '?'. You could substract 65 from the integar by mod 91 and that would fix this, but it is a hardcode fix.

########## Question 2 ##########

#a
l<-utf8ToInt('C')
key<-5
unencrypt_int<- l-65-key
modded<-mod(unencrypt_int,26)
new_int<-modded+65
q2a <-intToUtf8(new_int) #answer is X

#b
l<-utf8ToInt('A')
key<-2
unencrypt_int<- l-65-key
modded<-mod(unencrypt_int,26)
new_int<-modded+65
q2b <-intToUtf8(new_int) #answer is Y

#c
l<-utf8ToInt('H')
key<-12
unencrypt_int<- l-65-key
modded<-mod(unencrypt_int,26)
new_int<-modded+65
q2c <-intToUtf8(new_int) #answer is V

########## Question 3 ##########

#a
l<-utf8ToInt('C')
key<-9
encrypt_int<- l-65+key
modded<-mod(encrypt_int,26)
new_int<-modded+65
q3a <-intToUtf8(new_int) #answer is L

#b
l<-utf8ToInt('J')
key<-10
encrypt_int<- l-65+key
modded<-mod(encrypt_int,26)
new_int<-modded+65
q3b <-intToUtf8(new_int) #answer is T

#c
l<-utf8ToInt('D')
key<-19
encrypt_int<- l-65+key
modded<-mod(encrypt_int,26)
new_int<-modded+65
q3c <-intToUtf8(new_int) #answer is W

#d
l<-utf8ToInt('Z')
key<-12
encrypt_int<- l-65+key
modded<-mod(encrypt_int,26)
new_int<-modded+65
q3d <-intToUtf8(new_int) #answer is L

########## Question 4 ##########

toEncrypt<- "ANDREW"
key<-5
xor_encrypt<- function(toEncrypt, key){
  for (elem in toEncrypt){
    uToI<-utf8ToInt(elem)
    bInt<-bitwXor(uToI,key)
    encryptedL<-intToUtf8(bInt)
    print(encryptedL)
  }
}
xor_encrypt(toEncrypt, key) #answer will be DKAW@R

toDecrypt<- "DKAW@R"
key<-5
xor_decrypt<- function(toDecrypt, key){
  for (elem in toDecrypt){
    uToI<-utf8ToInt(elem)
    bInt<-bitwXor(uToI,key)
    encryptedL<-intToUtf8(bInt)
    print(encryptedL)
  }
}
xor_decrypt(toDecrypt, key) #answer will be ANDREW

########## Question 5 ##########
toEncryptC<-"ANDREW"
keyC<-10
caesar_encrypt<- function(toEncryptC, keyC){
  for (elemC in toEncryptC)
  uToIC<-utf8ToInt(elemC)
  encrypt_int<- uToIC-65+keyC
  modded<-mod(encrypt_int,26)
  new_int<-modded+65
  encryptedLC<-intToUtf8(new_int)
  print(encryptedLC)
}
encryptedC<-caesar_encrypt(toEncryptC, keyC) #answer will be KXNBOG

toDecryptC<-"KXNBOG"
keyC<-10
caesar_decrypt<- function(toDecryptC, keyC){
  for (elemC in toDecryptC)
    uToIC<-utf8ToInt(elemC)
    encrypt_int<- uToIC-65-keyC
    modded<-mod(encrypt_int,26)
    new_int<-modded+65
    decryptedLC<-intToUtf8(new_int)
    print(decryptedLC)
}
decryptedC<-caesar_decrypt(toDecryptC, keyC) #answer will be ANDREW