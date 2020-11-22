rm(list=ls())
library(digest)


encrypt <- function(s){
  hashed.message = digest(s, algo="sha256")
  return(hashed.message)
  # Return with the SHA256 hash value of `v`
} 

init.table <- function(){
  #' Create and return empty dataframe with these columns:
  #' user.name : character
  #' password: character
  df <- data.frame('user.name'=character(), 'password'=character()) 
  return(df)
}

add.or.update.user <- function(df, user.name, password){
  #' Add a new record to `df` setting the `user.name` and the `password`
  #' 
  #' If there is a record with the same username, makes sure you update the row
  #' instead of creating a duplicate
  #' 
  #' return with the updated DataFrame
  
  already_exists = FALSE
  
  for( i in seq_along(df$user.name)){
    if (df$user.name[i] == user.name ){
      df$password[i] <- encrypt(password)
      already_exists = TRUE
    }
  }
  
  if (!already_exists){
   df = rbind(df,list('user.name'=user.name, 'password'=encrypt(password)))
  }
  
  return(df)
}

authenticate.user <- function (df, user.name, password){
  for(i in seq_along(df$user.name)){
    if(df$user.name[i] == user.name & df$password[i] == encrypt(password)){
      return(TRUE)
    } 
  }
  return(FALSE)
}

# Example execution

user.df <- init.table()
user.df <- add.or.update.user(user.df, "example_user", "example_password")
user.df <- add.or.update.user(user.df, "example_user", "example_password_2")

# EVALUATE THE RESULTS
# These all must be TRUE
print(nrow(user.df[user.df$password == "example_password_2",]) == 0)
print(nrow(user.df[user.df$user.name == "example_user",]) == 1)
print(authenticate.user(user.df, "example_user", "example_password_2"))
print(!authenticate.user(user.df, "example_user", "example_password"))

