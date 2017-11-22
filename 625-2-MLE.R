library(ggplot2)
library(purrr)


#############Plot Likelihood functions###########
l_true <- function(lambda, x){(lambda^length(x)*exp(-lambda*sum(x)))/max(lambda^length(x)*exp(-lambda*sum(x)))}
l_censored <- function(lambda, x, censor_time){
  if(length(censor_time) == 1) {censor_time = rep(censor_time, length(x))}
  (lambda^length(x[x < censor_time])*exp(-lambda*sum(x[x < censor_time])))*exp(-lambda*sum(censor_time[censor_time <= x]))/max(lambda^length(x[x < censor_time])*exp(-lambda*sum(x[x < censor_time]))*exp(-lambda*sum(censor_time[censor_time <= x])))
  }

n <- 100

#generate survival times and censored times

rsurv <- function(dist_function, n, censor_time, parameter, ...){
  surv_times <- dist_function(n, parameter, ...)
  censor_times <- replace(surv_times, surv_times > censor_time, censor_time)
  list(surv_times, censor_times)
}

#generate random censoring times

rsurv_rand <- function(dist_function, n, parameter, ...){
  surv_times <- dist_function(n, parameter, ...)
  foo <- runif(n, mean(surv_times)-sd(surv_times), mean(surv_times)+sd(surv_times))
  censor_times <- replace(surv_times, surv_times > )
}

s_times <- rexp(n,1)
foo <- runif(n, 0.5, 1.5)
c_times <- s_times
c_times[which(foo < c_times)]<-foo[which(foo < c_times)]

#generate informative censoring times
s_times <- rexp(n,1)
i_times <- 0
i_times[which(s_times <1)]<-runif(length(which(s_times <1)), 0 ,1)
i_times[which(s_times >= 1)]<-runif(length(which(s_times >= 1)), 1 ,2)
c_times <- s_times
c_times[which(i_times < c_times)]<-i_times[which(i_times < c_times)]

######generate 500 survival and censoring times######
s_list <- list()
c_list <- list()
obs_list <- list()

for(i in 1:50){
  s_times <- rexp(n,1)
  c_times <- s_times
  c_times[which(c_times > 1)] <- 1
  s_list[[i]]<-s_times
  c_list[[i]]<-c_times
}


#random censoring
for(i in 1:50){
  s_times <- rexp(n,1)
  foo <- runif(100, 0.5, 1.5)
  c_times <- s_times
  c_times[which(foo < c_times)]<-foo[which(foo < c_times)]
  s_list[[i]]<-s_times
  c_list[[i]]<-c_times
  obs_list[[i]]<-foo
}

#informative censoring
for(i in 1:50){
  s_times <- rexp(n,1)
  i_times <- 0
  i_times[which(s_times <1)] <- runif(length(which(s_times <1)), 0 ,1)
  i_times[which(s_times >= 1)] <- runif(length(which(s_times >= 1)), 1 ,2)
  c_times <- s_times
  c_times[which(i_times < c_times)]<-i_times[which(i_times < c_times)]
  s_list[[i]] <- s_times
  c_list[[i]] <- c_times
  obs_list[[i]] <- i_times
}

###calculate mean or sd of MLE's###
sd(map_dbl(s_list, ~length(.x)/sum(.x)))
sd(map2_dbl(c_list, obs_list, ~(length(.x)-length(which(.x==.y)))/sum(.x)))

##########Create Lists of Stat_function calls and plot all 1500 curves###########
mle1.plots <- map(s_list, ~stat_function(fun = l_true, args = list(.x)))
mle2.plots <- map(c_list, ~stat_function(fun = l_true, col = "red", args = list(.x)))
mle3.plots <- map2(s_list, obs_list, ~stat_function(fun = l_censored, col = "green", args = list(.x, .y)))

plots <- c(mle2.plots, mle3.plots, mle1.plots)



samplist <- list()
for(i in 1:50){
  samplist[[i]] <- list(s_list[[i]], obs_list[[i]])
}

multifun_getplots <- function(func, samples_list, ...){
  map(samples_list, ~stat_function(fun = func, args = list(.x),...))
}

mle1.plots <- multifun_getplots(l_censored, samplist, col = "green")
mle2.plots <- multifun_getplots(l_true, c_list, col = "red")
mle3.plots <- multifun_getplots(l_true, s_list)


ggplot(data = data.frame(x=c(0,3)), mapping = aes(x = x)) +
  xlab("MLE estimates of lambda")+
  ylab("Scaled Likelihood")+
  plots
