# Recode the education questions asked in questionnaires K and N

df$education_k <- NA_integer_

# No qualification
replace <- which(df$k6280 == 1)
df$education_k[replace] <- 1

# CSE
replace <- which(df$k6281 == 1)
df$education_k[replace] <- 1

# Vocational
replace <- which(df$k6284 == 1 | df$k6285 == 1 | df$k6288 == 1 | df$k6295 == 1)
df$education_k[replace] <- 2

# O-level
replace <- which(df$k6282 == 1)
df$education_k[replace] <- 3

# A-level
replace <- which(df$k6283 == 1 | df$k6286 == 1 | df$k6287 == 1 | df$k6289 == 1 | df$k6290 == 1 | df$k6291 == 1)
df$education_k[replace] <- 4

# Degree
replace <- which(df$k6292 == 1)
df$education_k[replace] <- 5

# It was felt that a lot of mothers with no educational qualifications  left this whole 
# question blank. Therefore,  all women who did not respond to any of the questions 
# were coded to 1 
tmp_edu <- df %>% select(k6281,k6282,k6283,k6284,k6285,k6286,k6287,k6288,k6289,k6290,k6291,k6292,k6280,k6295)
replace <- which(apply(tmp_edu, 1, function(row) all(row == -1)))
df$education_k[replace] <- 1

table(df$education_k)

df$education_n <- NA_integer_

# No qualification
# No qualification
replace <- which(df$n4012 == 1)
df$education_n[replace] <- 1

# CSE
replace <- which(df$n4000 == 1)
df$education_n[replace] <- 1

# Vocational
replace <- which(df$n4003 == 1 | df$n4004 == 1 | df$n4007 == 1 | df$n4015 == 1)
df$education_n[replace] <- 2

# O-level
replace <- which(df$n4001 == 1)
df$education_n[replace] <- 3

# A-level
replace <- which(df$n4002 == 1 | df$n4005 == 1 | df$n4006 == 1 | df$n4008 == 1 | df$n4009 == 1 | df$n4010 == 1)
df$education_n[replace] <- 4

# Degree
replace <- which(df$n4011 == 1)
df$education_n[replace] <- 5

# It was felt that a lot of mothers with no educational qualifications  left this whole 
# question blank. Therefore,  all women who did not respond to any of the questions 
# were coded to 1 
tmp_edu <- df %>% select(n4000,n4001,n4002,n4003,n4004,n4005,n4006,n4007,n4008,
                         n4009,n4010,n4011,n4012,n4015)

replace <- which(apply(tmp_edu, 1, function(row) all(row == -1)))
df$education_n[replace] <- 1
table(df$education_n)
