rm(list=ls())
#####################################################################################################################
#Simulating CEU.edu

## 1)
#generate a keypair
keypair <- PKI.genRSAkey(bits = 2048L)

#save them into pem format
prv.pem <- PKI.save.key(keypair, private=TRUE)
pub.pem <- PKI.save.key(keypair, private=FALSE)


# Write the keys to a file
write(pub.pem, file="id_rsa.pub") # Save Public key -> this key should be shared
write(prv.pem, file="id_rsa")     # Save Private key

## 2)
#Sharing public key among the team

##################################
# Wait for the encrypted message
##################################

## 5)
#get message from visitor, decrypt it and print it out

#reading in the given file:
read.binfile <- file("encrypted_message_file.dat", "rb")
read.encrypted.message <- readBin(read.binfile, raw(), n=999999999) # 'n' says how many bytes
close(read.binfile)

#loading in CEU.edu's private key (in memory stored could be used as well)
prv.pem.loaded <- scan("id_rsa", what='list', sep='\n') 
#extract it from pem format
prv.key.loaded <- PKI.load.key(prv.pem.loaded) 

#decrypting message
decrypted.message <- rawToChar(PKI.decrypt(read.encrypted.message, prv.key.loaded))

#printing out message
print(decrypted.message)

#####################################################################################################################
#Simulating visitor

## 3)
#creating a message and encrypting it with the CEU.edu's public key

#loading in CEU's public key:
pub.pem.loaded <- scan("id_rsa.pub", what='list', sep='\n')

#extracting it from pem format
pub.key.loaded <- PKI.load.key(pub.pem.loaded) 

#Encrypting the message with the CEU.edu's public key
encrypted.message <- PKI.encrypt(charToRaw("This is the message that we are encrypting!"), pub.key.loaded)

## 4) 
#Writing this message in to file
write.binfile <- file("encrypted_message_file.dat", "wb")
writeBin(encrypted.message, write.binfile)
close(write.binfile)

# the encrypted_message_file.dat is writen to the working directory

#####################################################################################################################
#Simulating CEU.edu



