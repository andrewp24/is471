library(compositions)
library(seqinr)


##### Question 1 ######


s11_function = function(binary_val){
  numbers = c(5,2,1,6,3,4,7,0)
  s11=binary(numbers) # [1] "101" "010" "001" "110" "011" "100" "111" "000"
  
  numbers2 = c(1,4,6,2,0,7,5,3)
  s12=binary(numbers2) # [1] "001" "100" "110" "010" "000" "111" "101" "011"
  
  b=unlist(strsplit(as.character(binary_val), ""))
  tempVal=b
  tempVal=c(tempVal[2:4])
  tempVal=paste(tempVal[1:3])
  tempVal=toString(c(tempVal[1], tempVal[2], tempVal[3]))
  tempVal=gsub(pattern = ", ", replacement = "", x = tempVal, fixed = TRUE)
  tempVal=strtoi(tempVal, base=2)
 #print("tempVal is below")
 #print(tempVal)
  
  if (b[1]==0) {
    tempRet= s11[tempVal+1]
  } else {
    tempRet= s12[tempVal+1]
  }
  
  return(tempRet)
}
s11_function("1011")

s12_function = function(binary_val){
  numbers = c(4,0,6,5,7,1,3,2)
  s11=binary(numbers) # [1] "100" "000" "110" "101" "111" "001" "011" "010"
  
  numbers2 = c(5,3,0,7,6,2,1,4)
  s12=binary(numbers2) # [1] "001" "100" "110" "010" "000" "111" "101" "011"
  
  b=unlist(strsplit(as.character(binary_val), ""))
  tempVal=b
  tempVal=c(tempVal[2:4])
  tempVal=paste(tempVal[1:3])
  tempVal=toString(c(tempVal[1], tempVal[2], tempVal[3]))
  tempVal=gsub(pattern = ", ", replacement = "", x = tempVal, fixed = TRUE)
  tempVal=strtoi(tempVal, base=2)
 #print("tempVal is below")
 #print(tempVal)
  
  if (b[1]==0) {
    tempRet= s11[tempVal+1]
  } else {
    tempRet= s12[tempVal+1]
  }
  
  return(tempRet)
}
s12_function("1011")

##### Question 2

expand = function(binaryVec){
  
  swap(binaryVec[3], binaryVec[4])
  ev = vector(mode="character", length = 8)
  ev = append(binaryVec[1:6], binaryVec[3:4], after = 2)
  return(ev)
}

round1 = function(binaryNum, key, enOrDe){
  binaryNum=unlist(strsplit(binaryNum, ""))
  keyNum=unlist(strsplit(key, ""))
  
 #print("split below")
  l0=binaryNum[1:6]
  r0=binaryNum[7:12]
 #print(l0)
 #print(r0)
  
 #print("keys below")
  k1=keyNum[1:8]
  k2=keyNum[2:9]
 #print(k1)
 #print(k2)
  
 #print("extended below")
  el0=expand(l0)
  er0=expand(r0)
 #print(el0)
 #print(er0)
  
  #use k1 if encrypting, k2 if decrypting.
  if (enOrDe == "E"){
    xord=bitwXor(as.integer(er0),as.integer(k1))
  } else {
    xord=bitwXor(as.integer(er0),as.integer(k2))
  }
 
 #print(xord)
  
 #print("blocks below")
  block1=xord[1:4]
  block2=xord[5:8]
 #print(block1)
 #print(block2)
  
 #print("e and f below")
  e=s11_function(block1)
  f=s12_function(block2)
 #print(e)
 #print(f)
  
  fr0=paste(e,f, sep = "")
 #print("fr0 below")
 #print(fr0)
  
 #print("new l0")
  l0=toString(l0)
  l0=gsub(",", "", toString(l0))
  l0=gsub(" ", "", toString(l0))
 #print(l0)
 #print("new r0")
  r0=toString(r0)
  r0=gsub(",", "", toString(r0))
  r0=gsub(" ", "", toString(r0))
 #print(r0)
  
  #print("binary l0 below")
  #print(binary(l0, mb=5))

  #print("l0 binary below")
  l0=unlist(strsplit(l0,""))
 #print(l0)
  fr0=unlist(strsplit(fr0,""))
 #print(fr0)
  
  #r1=bitwXor(as.integer(fr0),as.integer(l0))
  r1=bitwXor(as.integer(l0),as.integer(fr0))
 #print("r1 below")
  r1=gsub(" ", "", toString(r1))
  r1=gsub(",", "", toString(r1))
 #print(r1)
  l1=unlist(strsplit(as.character(r0), ""))

  
  l1=gsub(",", "", toString(l1))
  l1=gsub(" ", "", toString(l1))
 #print("r1 and l1 below")
 #print(r1)
 #print(l1)
  
  
  return(paste(l1, r1, sep = ""))
}

test = round1("000001010111", "001101100", "E") #010111100011

##### Question 3 ######

round2 = function(binaryNum, key, enOrDe){
  binaryNum=unlist(strsplit(binaryNum, ""))
  keyNum=unlist(strsplit(key, ""))
  
  #print("split below")
  l0=binaryNum[1:6]
  r0=binaryNum[7:12]
  #print(l0)
  #print(r0)
  
  #print("keys below")
  k1=keyNum[1:8]
  k2=keyNum[2:9]
  #print(k1)
  #print(k2)
  
  #print("extended below")
  el0=expand(l0)
  er0=expand(r0)
  #print(el0)
  #print(er0)
  
  #print("xor below")
  #use k2 if encrypting, k1 if decrypting.
  if (enOrDe == "E"){
    xord=bitwXor(as.integer(er0),as.integer(k2))
  } else {
    xord=bitwXor(as.integer(er0),as.integer(k1))
  }
  #print(xord)
  
  #print("blocks below")
  block1=xord[1:4]
  block2=xord[5:8]
  #print(block1)
  #print(block2)
  
  #print("e and f below")
  e=s11_function(block1)
  f=s12_function(block2)
  #print(e)
  #print(f)
  
  fr0=paste(e,f, sep = "")
  #print("fr0 below")
  #print(fr0)
  
  #print("new l0")
  l0=toString(l0)
  l0=gsub(",", "", toString(l0))
  l0=gsub(" ", "", toString(l0))
  #print(l0)
  #print("new r0")
  r0=toString(r0)
  r0=gsub(",", "", toString(r0))
  r0=gsub(" ", "", toString(r0))
  #print(r0)
  
  #print("binary l0 below")
  #print(binary(l0, mb=5))
  
  #print("l0 binary below")
  l0=unlist(strsplit(l0,""))
  #print(l0)
  fr0=unlist(strsplit(fr0,""))
  #print(fr0)
  
  #r2=bitwXor(as.integer(fr0),as.integer(l0))
  r2=bitwXor(as.integer(l0),as.integer(fr0))
  #print("r1 below")
  r2=gsub(" ", "", toString(r2))
  r2=gsub(",", "", toString(r2))
  #print(r1)
  l2=unlist(strsplit(as.character(r0), ""))
  
  
  l2=gsub(",", "", toString(l2))
  l2=gsub(" ", "", toString(l2))
  #print("r1 and l1 below")
  #print(r1)
  #print(l1)
  
  
  return(paste(r2, l2, sep = ""))
}

test2 = round2("000001010111", "001101100", "E") #101011010111

##### Question 4 ######

message="Wire $2000 to Bob"
message=utf8ToInt(message)      #87 105 114 101 ....
message=binary(message,mb=11)   #000001010111 ....
k=binary(108,mb=8)
e=vector(mode="character")

for (i in 1:length(message)) {
  e1=round1(message[i],k,"E")
  e2=round2(e1,k,"E")
  e[i]=e2
}
e # the encrypted message
print(unbinary(e))              #3811 2484 2334 2341 2835  929 2527  475  475  475 2835 2168 3018 2835  630 3018  598
print(intToUtf8(unbinary(e)))   #\u0ee3\u09b4?????????????????UUU???\u0878?????????????


##### Question 5 #####2
q5="Wire $2000 to Bob"
q5=utf8ToInt(q5)
q5=binary(q5,mb=11)             #000001010111 ....
k=binary(108,mb=8)              #001101100
d=vector(mode="character")

for (i in 1:length(q5)) {
  e1=round1(q5[i],k, "E")
  print(e1)
  e2=round2(e1,k, "E")
  print(e2)
  d1=round1(e2,k,"D")
  d2=round2(d1,k,"D")
  d[i]=d2
}
d # the encrypted message
print(unbinary(d))              #87 105 114 101  32  36  50  48  48  48  32 116 111  32  66 111 98
print(intToUtf8(unbinary(d)   #Wire $2000 to Bob8
3
## testing output
print(round1('000001010111', "001101100", 'D')) #010111101011
print(round2('000001010111', "001101100", 'D')) #100011010111