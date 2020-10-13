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



