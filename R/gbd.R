
library(deSolve)

gbd <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {

    dS1 <- -beta1 * S1 * I1
    dI1 <-  beta1 * S1 * I1 - gamma * I1
    dR1 <- gamma * I1

    dS2 <- -(beta2 * S2 * I2 + beta12 * S2 * I1)
    dI2 <-  (beta2 * S2 * I2) + (beta12 * S2 * I1) - (gamma * I2)
    dR2 <- gamma * I2

    return(list(c(dS1, dI1, dR1,dS2, dI2, dR2 )))
  })
}



run_gbd<-function(
  beta1=0.5,
  beta2=0.2,
  gamma= 0.2,
  beta12 =0.01,
  S1=0.99,I1=0.01, R1=0,
  S2=1,I2=0, R2=0,
  days=60
)
{
  parameters <- c(beta1 = beta1,beta2=beta2, beta12=beta12,gamma=gamma)
  init<- c(S1 = S1, I1 = I1,R1 = R1, S2 = S2, I2 = I2,R2 = R2)
  times<- seq(0, days, by = 1)
  out <- ode(y = init, times = times, func = gbd, parms = parameters)
  d <- as.data.frame(out)
  d
}


run_sim<-function (
gamma = 0.2,
days1 = 100,
R01 = 2.5,
R02 =1,
R012 =1,
R11 =2.5,
R12 = 1,
R112 = 1,
days2 = 100
)
{

beta2<- R02*gamma
beta1 <- R01*gamma
beta12 <- R012*gamma

sd<-run_gbd(beta1 = beta1,beta2=beta2, beta12=beta12,gamma=gamma,days=days1)

beta2<- R12*gamma
beta1 <- R11*gamma
beta12 <- R112*gamma

n<-length(sd$S1)
sd2<-run_gbd(
  S1=sd$S1[n],I1=sd$I1[n], R1=sd$R1[n],
  S2=sd$S2[n],I2=sd$I2[n], R2=sd$R2[n],
  beta1 = beta1,
  beta2=beta2,
  beta12=beta12,
  gamma=gamma,
  days=days2)
sd2<-sd2[-1,]
sd<-rbind(sd,sd2)
n<-length(sd$S1)
sd$time<-1:n
return(sd)
}
