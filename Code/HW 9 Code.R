## HW 9 ## 

# Danny Szydlowski and Tyler Butts # 

## Generate a time series of abundance 
## for a population growing according to the discrete time logistic growth model

r = 0.2 
K = 100
time = seq(from=1,to=50)
N = array(dim = c(1, length(time))); N[1] = 5
  for (t in time[2:length(time)]){
    N[t] = N[t-1] + r*N[t-1]*(1-N[t-1]/K)
  }
N

############### Q1 Plot the Data ##############################
plot(x = seq(1, 50), 
          y = N,
          type = 'o', pch = 20, cex = 2)


############## Q2 - generate a model predicted time series of abundance #########################
# Pretend you don't know the true r or K and adjust until model fits data # 
dat = data.frame(x = x, y = as.vector(N))

plot(x, y, data = dat)


# Approximate 
# model 1
r = 0.5 
f = function(x, r, K, N0) {K / (1 + (K/N0 - 1) * exp(-r*x))}
plot(x, y)


pstart = c(r=r, K=max(y), N0=y[1])
mod1 = nls(y ~ f(x, r, K, N0), start = pstart, trace = T)
# model converged 
summary(mod1)
lines(x, f(x, r=r, K=max(y), N0=y[1]), col = 'blue')

# model 2 
r = 2

pstart = c(r=r, K=max(y), N0=y[1])
mod2 = nls(y ~ f(x, r, K, N0), start = pstart, trace = T)
lines(x, f(x, r=r, K=max(y), N0=y[1]), col = 'green')

# model converged 
summary(mod2)

# model 3
r = .1

pstart = c(r=r, K=max(y), N0=y[1])
mod3 = nls(y ~ f(x, r, K, N0), start = pstart, trace = T)
lines(x, f(x, r=r, K=max(y), N0=y[1]), col = 'purple')

# model 4 (true value )
r = .2

pstart = c(r=r, K=max(y), N0=y[1])
mod4 = nls(y ~ f(x, r, K, N0), start = pstart, trace = T)
lines(x, f(x, r=r, K=max(y), N0=y[1]), col = 'black')

# model converged 
summary(mod4)

legend('bottomright', legend = c('model 1', 'model 2', 'model 3', 'model 4'), lty = 1,
       col = c('blue', 'green', 'purple', 'black'))

################### Q3 ##############################
#Create a function that returns the negative log likelihood of the 
#model parameters (r, K) given the data (Nt)
#.Use optim() or a grid search and the function you just created to 
#estimate the parameters of your model - remember to add the 
#variance as a third parameter to be estimated (or use its analytical 
 #                                              MLE: SSE/n)

dat

r=0.2
K = 100
time = dat$x

# Get standard Deviation # 
sigma = sd(dat$y)
sigma # 35. 37235 

mean = mean(dat$y)
mean


nll = function(r, k, sigma){
  -sum(dnorm(x=dat$y, mean=mean, sd = sigma, log = T))
}

# Make the function for optim # 
func = function(par){
  r = par[1] 
  K = par[2] 
  sigma = par[3] 
  mean = mean
  nll_val = nll(r, K, sigma)
}

# optim()
model = optim(par = c(1,2,3), fn=func)
optim_coefficients = model$par

# Get the coefficients 
optim_coefficients

# r = -13.89
# K = 2. 66 
# sigma = 34.995

## Answer: Model coefficients seem incredibly off? ## 

############# Q4 ############################
#Add observation error ( Nt + rnorm(mean=0, sd=?) ) to your data
#.Use optim() or a grid search and the function you just created to 
        #estimate the parameters of your model based on this new data - 
        #remember to add the variance as a third parameter to be estimated 
        #(or use its analytical MLE: SSE/n)
# Q4. How well can you estimate the model parameters now?  Is there 
        #any evidence of correlation in your parameter estimates (e.g., a ridge in the likelihood surface)?

# Add observation error 

y_err = dat$y + rnorm(length(time), mean=mean, sd=sigma)

nll_v2 = function(r, k, sigma){
  -sum(dnorm(x=y_err, mean=mean, sd = sigma, log = T))
}

fun_v2 = function(par){
  r = par[1] 
  K = par[2]
  sigma = par[3]
  mean = mean 
  nll = nll_v2(r, K, sigma)
}

# optim part 2 # 
model_part2 = optim(par=c(1,2,3), fn=fun_v2)
optim_coefficients_v2 = model_part2$par
optim_coefficients_v2

# r = -40.76 
# K = 3.39 
# sigma = 91.545 

# Again these are very wrong, the negative r is confusing me
# ... Unsure how to test for correlation in the parameter estimates# 

############### Q5 ######################### 
#Start your model with observation error at 5% of K
#Use optim() or a grid search and the function you just created to 
       #estimate the parameters of your model based on this new data - 
       #remember to add the variance as a third parameter to be estimated 
       #(or use its analytical MLE: SSE/n)

# Observation error 5% of K = 5
y_err2 = dat$y + 5

nll_v2 = function(r, k, sigma){
  -sum(dnorm(x=y_err2, mean=mean, sd = sigma, log = T))
}

fun_v2 = function(par){
  r = par[1] 
  K = par[2]
  sigma = par[3]
  mean = mean 
  nll = nll_v2(r, K, sigma)
}

# optim part 2 # 
model_part2 = optim(par=c(1,2,3), fn=fun_v2)
optim_coefficients_v2 = model_part2$par
optim_coefficients_v2

# r = -14.59
# K = 3.28 
# sigma = 35.39 

# Okay these actually look better, R and K and way off but sigma is close to true 

##################### Q6 ################## 

# Again these are very wrong, the negative r is confusing me
# ... Unsure how to test for correlation in the parameter estimates# 


# Observation error 90% of K = 5
y_err3 = dat$y + 90

nll_v3 = function(r, k, sigma){
  -sum(dnorm(x=y_err3, mean=mean, sd = sigma, log = T))
}

fun_v3 = function(par){
  r = par[1] 
  K = par[2]
  sigma = par[3]
  mean = mean 
  nll = nll_v3(r, K, sigma)
}

# optim part 2 # 
model_part2 = optim(par=c(1,2,3), fn=fun_v3)
optim_coefficients_v2 = model_part2$par
optim_coefficients_v2

# r = -43.44
# K = 3.81 
# sigma = 96.69 

# Paramaters are back to being very inaccurate again # 

# Okay these actually look better, R and K and way off but sigma is close to true 