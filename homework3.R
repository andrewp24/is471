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
my_mode(c(x[9], y[11], z[11])) # answer: 1

##### Question 6 #####

if

