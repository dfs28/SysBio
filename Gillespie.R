#### Simulate gillespie

#function to get waiting time
wait_time <- function(r_t) {
  
  -log(runif(1))/r_t
}

#Function to get choice of which process
choose_reaction <- function(all_probs) {
  #Function to choose reaction
  
  #Sample from the distribution
  sample(1:length(all_probs), 1, prob = all_probs/sum(all_probs))
}

f_x <- function(lambda1, x2, K, n) {
  #Function to give f(x2)
  
  lambda1*(K^n/ (K^n + x2^n))
}

g_x <- function(lambda2, x1, K, n) {
  #Function to give g(x1)
  
  lambda2*(x1^n/ (K^n + x1^n))
  
}

#Make a function to return bits and set amount of time in each state
simulate_gillespie <- function(lambda1, lambda2, beta1, beta2, K, n, functionx, gunctionx, cycles = 10000) {
  
  #Set up some storage
  states <- data.frame(x1 = rep(0, times = 1000), 
                       x2 = rep(0, times = 1000))
  
  x1 <- 0
  x2 <- 0
  xs <- data.frame(x1 = rep(NA, times = cycles),
                   x2 = rep(NA, times = cycles),
                   time = rep(NA, times = cycles))
  
  for (i in 1:cycles) {
    #Loop over this a few times
    
    func_x <- functionx(lambda1, x2, K, n)
    gunc_x <- gunctionx(lambda2, x1, K, n)
    
    #Choose wait time
    time <- wait_time(func_x + gunc_x + beta1*x1 + beta2*x2)
    
    #Choose reaction 
    rxn <- choose_reaction(c(func_x, beta1*x1, gunc_x, beta2*x2))
    
    #Update time in state prior to change
    if (i %in% 1000:cycles) {
      #Only save after stationarity
      
      states$x1[x1] <- states$x1[x1] + time
      states$x2[x2] <- states$x2[x2] + time
    }
    
    xs$x1[i] <- x1
    xs$x2[i] <- x2
    
    if (i == 1) {xs$time[i] <- time
    } else {xs$time[i] <- xs$time[i - 1] + time}
    
    switch(rxn, 
           '1' = {x1 <- x1 + 1}, 
           '2' = {if (x1 != 0) {x1 <- x1 - 1}},
           '3' = {x2 <- x2 + 1}, 
           '4' = {if (x2 != 0) {x2 <- x2 - 1}})
    
  }
  
  #Update time in state after change
  states$x1[x1] <- states$x1[x1] + time
  states$x2[x2] <- states$x2[x2] + time
  
  return(list(xs, states))
  
}

gs <- simulate_gillespie(100, 1000, 1, 1, 20, 3, f_x,  g_x, 100000)


#Make some plots
ggplot(gs[[1]]) + geom_line(aes(x = time, y = x1, col = 'red', alpha = 0.4)) + 
                  geom_line(aes(x = time, y = x2, col = 'blue', alpha = 0.4)) + theme_bw()

ggplot(gs[[2]]) + geom_line(aes(x = 1:1000, y = x1, col = 'red')) + 
                  geom_line(aes(x = 1:1000, y = x2, col = 'blue')) + theme_bw() + xlim(c(1, 110))


plot(xs$time, xs$x, type = 'l')
plot(gs[[2]][which(gs[[2]]!= 0)])


#Calculate eta
calc_eta <- function(input) {
  #Function to calculate eta
  
  sd(input)^2/mean(input)^2
}

calc_eta(gs[[1]]$x1)
1/mean(gs[[1]]$x1)
calc_eta(gs[[1]]$x2)
1/mean(gs[[1]]$x2)
  
### Run some different functions for f and g of x


f_x1 <- function(lambda1, x2, K, n) {
  #Function to give f(x2)
  
  lambda1/(x2 + 1)^n
}

g_x1 <- function(lambda2, x1, K, n) {
  #Function to give g(x1)
  
  lambda2*x1^n + 1
  
}

gs <- simulate_gillespie(100, 1000, 1, 1, 20, 3, f_x1,  g_x1, 10000)


#### Simulate gillespie for III

#Make a function to return bits and set amount of time in each state
simulate_production <- function(r, cycles = 10000) {
  
  #Set up some storage
  states <- data.frame(x = rep(0, times = 100000))
  
  x <- 0
  xs <- data.frame(x = rep(NA, times = cycles),
                   time = rep(NA, times = cycles))
  
  while (x < 10) {
    
    #Choose wait time
    rt <- r(x)
    
    time <- wait_time(rt)
  
    xs$x[i] <- x
    
    if (i == 1) {xs$time[i] <- time
    } else {xs$time[i] <- xs$time[i - 1] + time}
    
    x <- x + 1
    
  }
  
  #Update time in state after change
  states$x[x] <- states$x[x] + time
  
  return(list(xs, states))
  
}

r1 <- function(x) {
  1  
}

r2 <- function(x) {
  10/(5 + x)
}

r3 <- function(x) {
  1 + x^3/1000
}

sim1 <-  simulate_production(r1)
ggplot(sim1[[1]]) + geom_line(aes(x = time, y = x, col = 'red', alpha = 0.4)) + theme_bw() 
ggplot() + geom_smooth(aes(x = 0:11, y = cumsum(r3(0:11)) - 1, col = as.factor('c'))) + 
  geom_smooth(aes(x = 0:11, y = cumsum(r2(0:11)) - 2, col = as.factor('b'))) + 
  geom_smooth(aes(x = 0:11, y = 0:11, col = as.factor('a'))) + theme_bw() + 
  labs(col = 'Function') + xlab('t') + ylab('x') + geom_hline(yintercept = 10, col = 'black', linetype = 'dashed') + 
  geom_hline(yintercept = 9, col = 'black', linetype = 'dashed') + 
  geom_hline(yintercept = 11, col = 'black', linetype = 'dashed') 
  


##### Gillespie for a2 ----

#Make a function to return bits and set amount of time in each state
simulate_gillespie2 <- function(lambda, beta, p, cycles = 10000) {
  
  #Set up some storage
  states <- data.frame(x = rep(0, times = 100000))
  
  x <- 0
  xs <- data.frame(x = rep(NA, times = cycles),
                   time = rep(NA, times = cycles))
  
  for (i in 1:cycles) {
    
    #Choose wait time
    time <- wait_time(lambda + beta*x)
    
    #Choose reaction 
    rxn <- choose_reaction(c(lambda, beta*x))
    
    #Update time in state prior to change
    if (i %in% 1000:cycles) {
      
      #Only save after stationarity
      states$x[x] <- states$x[x] + time
    }
    
    xs$x[i] <- x
    
    if (i == 1) {xs$time[i] <- time
    } else {xs$time[i] <- xs$time[i - 1] + time}
    
    switch(rxn, 
           '1' = {x <- x + cpp_geom(p)}, 
           '2' = {if (x != 0) {x <- x - 1}})
    
  }
  
  #Update time in state after change
  states$x[x] <- states$x[x] + time
  
  return(list(xs, states))
  
}

gs2 <- simulate_gillespie2(10, 2, 0.5, 100000)
gs3 <- simulate_gillespie2(10, 2, 0.1, 100000)
ggplot(gs3[[1]]) + geom_line(aes(x = time, y = x, col = 'red', alpha = 0.4)) + theme_bw() 

ggplot(gs3[[2]]) + geom_col(aes(x = 1:1e5, y = x, fill = 'red')) + theme_bw() + xlim(c(1, 200)) #Do normalised prob

calc_eta(gs2[[1]]$x)*mean(gs2[[1]]$x)
1/mean(gs2[[1]]$x)

calc_eta(gs3[[1]]$x)*mean(gs3[[1]]$x)

dans_geom <- function(p) {
  
  t <- 0
  prob <- 1
  while (prob > p) {
    prob <- runif(1)
    t <- t + 1
  }
  t
}
mean(replicate(1000000, cpp_geom(0.1)))


