##### Question 1 #####

letter_A = "A"
letter_t = "t"
letter_at = "@"
int_17 = 17
int_9 = 9
letter_amp = "&"

int_A = utf8ToInt(letter_A)     #65
int_t = utf8ToInt(letter_t)     #116
int_at = utf8ToInt(letter_at)   #64
int_amp = utf8ToInt(letter_amp) #38

binary(int_A)   #1000001
binary(int_t)   #1110100
binary(int_at)  #1000000
binary(int_17)  #10001
binary(int_9)   #1001
binary(int_amp) #100110

##### Question 2 #####

unbinary('00010110')  #22
unbinary("00011101")  #29
unbinary("01011011")  #91
unbinary("01100101")  #101
unbinary("00001010")  #10
unbinary("01100100")  #100

##### Question 3 #####

seed = "MHID9870"
#a
int_seed = utf8ToInt(seed)  #77 72 73 68 57 56 55 48
#b
b_int_seed = binary(int_seed, mb=7) #chr [1:8]
#c
strsplit_b_int_seed = strsplit(b_int_seed, "") #8 times chr [1:8]
#d
unlist_b_int_seed = unlist(strsplit_b_int_seed) #chr [1:64]
#e
b_as_int = as.integer(c(unlist_b_int_seed)) #int [1:64]

##### Question 4 #####

x = vector(mode="integer", length = 19) #int [1:19] 0 1 0 0 1 1 0 1 0 1 ...
for (i in 1:19){
  x[i] = b_as_int[i]
}

y = vector(mode="integer", length = 22) #int [1:22] 0 1 0 0 1 0 1 0 0 0 ...
for (i in 1:22){
  y[i] = b_as_int[i+19]
}

z = vector(mode="integer", length = 23) #int [1:23] 0 1 1 1 0 0 0 0 0 1 ...
for (i in 1:23){
  z[i] = b_as_int[i+41]
}

k = vector(mode="integer", length = 128) #int [1:128] 0 0 0 0 0 0 0 0 0 ...

##### Question 5 #####

print(x[9])
print(y[11])
print(z[11])

my_mode = function(v){
  freq_count = table(v)
  max_idx = which.max(freq_count)
  m = names(max_idx)
  m
}
m = my_mode(c(x[9], y[11], z[11])) # answer: 1

##### Question 6 #####
#if x9 is equal to my_mode which is 1, then it will step

#setting copies of the vectors
x2 = x
y2 = y
z2 = z

if (x[9]==m){
  #x step
  #xor the following x[14], x[17], x[18], x[19]
  # xor the first two and then the last two, then xor the results of those two
  xorx1=bitwXor(x[14], x[17])
  xorx2=bitwXor(x[18], x[19])
  xorx3=bitwXor(xorx1, xorx2)
  #xor3 will be the new first value
  
  #should be able to make a function for these
  i=1
  for(elemx in x){
    
    if(i-1!=0){
      x2[i] = x[i-1]
    }
    i=i+1
  }
  x2[1]=xorx3
}

if (y[11]==m){
  #y steps
  #xor the following: y[21], y[22]
  xory=bitwXor(y[21], y[22])
  #xory will be the new first value
  
  i=1
  for(elemy in y){
    
    if(i-1!=0){
      y2[i] = y[i-1]
    }
    i=i+1
  }
  y2[1]=xory
}

if (z[11]==m){
  #z steps
  #xor the following z[8], z[21], x[22], x[23]
  # xor the first two and then the last two, then xor the results of those two
  xorz1=bitwXor(z[8], z[21])
  xorz2=bitwXor(z[22], z[23])
  xorz3=bitwXor(xorz1, xorz2)
  #xor3 will be the new first value
  
  i=1
  for(elemz in z){
    
    if(i-1!=0){
      z2[i] = z[i-1]
    }
    i=i+1
  }
  z2[1]=xorz3
  
}
#printing the original x,y,z and new x2,y2,z2 side by side
print(x) # 0 1 0 0 1 1 0 1 0 1 0 0 1 0 0 0 0 1 0
print(x2) # 0 1 0 0 1 1 0 1 0 1 0 0 1 0 0 0 0 1 0 #care about this one
print(y) # 0 1 0 0 1 0 1 0 0 0 1 0 0 0 0 1 1 1 0 0 1 0
print(y2) # 1 0 1 0 0 1 0 1 0 0 0 1 0 0 0 0 1 1 1 0 0 1 #care about this one
print(z) # 0 1 1 1 0 0 0 0 0 1 1 0 1 1 1 0 0 1 1 0 0 0 0
print(z2) # 0 0 1 1 1 0 0 0 0 0 1 1 0 1 1 1 0 0 1 1 0 0 0 # care about this one


##### Question 7 #####

#last values of each vector will be xor'd
xorlast1=bitwXor(x2[19], y2[22])
keystreamBit=bitwXor(xorlast1, z2[23]) # answer is 1

##### Question 8 #####
#function for the above

a51_function <- function(seed, keyLength) {
  
  seedInt = utf8ToInt(seed)
  binarySeed = binary(seedInt, mb=7)
  strsplitBinarySeed = strsplit(binarySeed, "")
  unlistBinarySeed = unlist(strsplitBinarySeed)
  binaryAsInt = as.integer(c(unlistBinarySeed))
  
  x = vector(mode="integer", length = 19)
  for (i in 1:19){
    x[i] = binaryAsInt[i]
  }
  
  y = vector(mode="integer", length = 22)
  for (i in 1:22){
    y[i] = binaryAsInt[i+19]
  }
  
  z = vector(mode="integer", length = 23)
  for (i in 1:23){
    z[i] = binaryAsInt[i+41]
  }
  
  k = vector(mode="integer", length = keyLength)
  j = 1
  for (elemk in k){
  
    my_mode = function(v){
      freq_count = table(v)
      max_idx = which.max(freq_count)
      m = names(max_idx)
      m
    }
    m = my_mode(c(x[9], y[11], z[11]))
    
    #if x9 is equal to my_mode which is 1, then it will step
    
    #setting copies of the vectors
    x2 = x
    y2 = y
    z2 = z
    
    if (x[9]==m){
      #x step
      #xor the following x[14], x[17], x[18], x[19]
      # xor the first two and then the last two, then xor the results of those two
      xorx1=bitwXor(x[14], x[17])
      xorx2=bitwXor(x[18], x[19])
      xorx3=bitwXor(xorx1, xorx2)
      #xor3 will be the new first value
      
      #should be able to make a function for these
      i=1
      for(elemx in x){
        
        if(i-1!=0){
          x2[i] = x[i-1]
        }
        i=i+1
      }
      x2[1]=xorx3
    }
    if (y[11]==m){
      #y steps
      #xor the following: y[21], y[22]
      xory=bitwXor(y[21], y[22])
      #xory will be the new first value
      
      i=1
      for(elemy in y){
        
        if(i-1!=0){
          y2[i] = y[i-1]
        }
        i=i+1
      }
      y2[1]=xory
    }
    if (z[11]==m){
      #z steps
      #xor the following z[8], z[21], x[22], x[23]
      # xor the first two and then the last two, then xor the results of those two
      xorz1=bitwXor(z[8], z[21])
      xorz2=bitwXor(z[22], z[23])
      xorz3=bitwXor(xorz1, xorz2)
      #xor3 will be the new first value
      
      i=1
      for(elemz in z){
        
        if(i-1!=0){
          z2[i] = z[i-1]
        }
        i=i+1
      }
      z2[1]=xorz3
      
    }
    #last values of each vector will be xor'd
    xorlast1=bitwXor(x2[19], y2[22])
    keystreamBit=bitwXor(xorlast1, z2[23]) # answer is 1
    print(keystreamBit)
    k[j] = keystreamBit
    j = j+1
    
    #resetting the x,y,z to have new sets
    x = x2
    y = y2
    z = z2
  
  }
  return(k)
}

keystream = a51_function("johnsons", 64)
print(keystream) # 0 0 0 1 0 1 1 0 0 1 1 0 0 0 1 1 0 1 1 1 0 1 0 1 1 0 0 0 1 0 1 0 0 1 1 1 0 1 0 0 1 1 0 1 0 0 1 1 0 1 1 0 1 1 1 1 0 1 0 1 0 0 0 1

##### Question 9 #####
install.packages('seqinr')
library(seqinr)

S = vector(mode = "integer", length = 5)
S[1] = 1
S[2] = 2
S[3] = 3
S[4] = 4
S[5] = 5

swapped = swap(S[4], S[1]) #num [1:5] 4 2 3 1 5

##### Question 10 #####

rc4_function = function(seed, keyLength){
  stateTable = vector(mode = "integer", length = 256)
  keyTable = vector(mode = "integer", length = 256)
  
  j = 1
  for (i in 1:256){
    stateTable[i] = i
    keyTable[i] = mod(seed[mod(i,keyLength)], 256)
  } 
  
  
}

rc4_function("Complete", 256)

library(numbers)