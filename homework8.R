library('digest')
##### Question 1 #####
block0 = list()

block0$index = 1
block0$timestamp = Sys.time()
block0$data = "Block#1"
block0$previous_hash = "0000"


##### Question 2 #####

hasher = function(block) {
  hashed = digest(block, 'sha256')
  return(hashed)
}

##### Question 3 #####

block0$new_hash = hasher(block0)

##### Quesiton 4 #####
new_block = list()

newBlock = function(previous_block, transaction_str) {
  new_block$index = previous_block$index+1
  new_block$timestamp = Sys.time()
  new_block$data = transaction_str
  new_block$previous_hash = hasher(previous_block)
  new_block$new_hash = hasher(newBlock)
  return(new_block)
}

block1 = newBlock(block0, "Author=M.Twain, royalty=10%")

##### Question 5 #####

blockchain = list()
blockchain[[1]] = block0

##### Question 6 #####

blockchain[[2]] = block1

##### Question 7 #####

block2 = newBlock(block1, "Publisher=ABCPublisher, return=20%")

block3 = newBlock(block2, "Retailer=Amazon,cost=$120")

blockchain[[3]] = block2
blockchain[[4]] = block3

print(blockchain)

##### Question 8 #####

block0 = list()

block0$index = 1
block0$timestamp = Sys.time()
block0$data = "Block#1"
block0$previous_hash = "0000"
block0$nonce = round(runif(1, min = 1, max = 2147483647))

hasher = function(block) {
  hashed = digest(block, 'sha256')
  return(hashed)
}
new_hash = hasher(block0)
vec_hash = unlist(strsplit(new_hash, ""))

while(vec_hash[1] != "0" || vec_hash[2] != "0"){
  block0$nonce = round(runif(1, min = 1, max = 2147483647))
  new_hash = hasher(block0)
  vec_hash = unlist(strsplit(new_hash, ""))
  print(new_hash)
  block0$new_hash = new_hash
}

new_block = list()

newBlock = function(previous_block, transaction_str) {
  new_block$index = previous_block$index+1
  new_block$timestamp = Sys.time()
  new_block$data = transaction_str
  new_block$previous_hash = hasher(previous_block)
  new_block$nonce = round(runif(1, min = 1, max = 2147483647))
  
  new_hash = hasher(block0)
  vec_hash = unlist(strsplit(new_hash, ""))
  
  while(vec_hash[1] != "0" || vec_hash[2] != "0"){
    new_block$nonce = round(runif(1, min = 1, max = 2147483647))
    new_hash = hasher(new_block)
    vec_hash = unlist(strsplit(new_hash, ""))
    print(new_hash)
    new_block$new_hash = new_hash
  }
  return(new_block)
}

new_blockchain = list()

new_blockchain[[1]] = block0

block1 = newBlock(block0, "Author=M.Twain, royalty=10%")
new_blockchain[[2]] = block1

block2 = newBlock(block1, "Publisher=ABCPublisher, return=20%")
block3 = newBlock(block2, "Retailer=Amazon,cost=$120")

new_blockchain[[3]] = block2
new_blockchain[[4]] = block3

print(new_blockchain)
