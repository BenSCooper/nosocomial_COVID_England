#  Calculate what proportion of transmissions  result from x% of primary cases (for fig S7)

# Do this by simulation
N<- 1000000 #  number of samples

# Take parameters from  model P1.1.1 with outcome 2: Probable and definite healthcare associated infections.
# Table S6 - considering transmission to patients from nosocomially infected patients

a0<-0.08
b<-1.07
phi0 <-0.28
k0<-0.11
#  mu for 1 primary infections
m <- a0+ b
phi<-phi0+ k0

transmissions<-rnbinom(N,size=phi, mu=m)
transmissions<-sort(transmissions,decreasing = T)
cumproportionoftransmissions<-cumsum(transmissions)/(sum(transmissions))
Y<-cumproportionoftransmissions[N*0.2]
X<-findInterval(0.8,cumproportionoftransmissions)/N 

print("Outcome:probable and definite; Model  P1.1.1 ")
print(paste(100*Y,"% of transmissions result from 20% of infections"))
print(paste("80% of transmissions result from ",100*X, "% of infections"))

par(mfrow=c(2,1))

plot(c(1:N)/N, cumproportionoftransmissions, type ='l', xlab="Proportion of infections (ranked)", ylab="Proportion of transmission",
        main="Probable and definite healthcare associated infection")

# Now same for model P1.1.0
a0<-0.08
b<-0.60
phi0 <-0.32
k0<-0.10
#  mu for 1 primary infections
m <- a0+ b
phi<-phi0+ k0

transmissions<-rnbinom(N,size=phi, mu=m)
transmissions<-sort(transmissions,decreasing = T)
cumproportionoftransmissions<-cumsum(transmissions)/(sum(transmissions))
Y<-cumproportionoftransmissions[N*0.2]
X<-findInterval(0.8,cumproportionoftransmissions)/N 

print("Outcome:probable and definite; Model  P1.1.0 ")
print(paste(100*Y,"% of transmissions result from 20% of infections"))
print(paste("80% of transmissions result from ",100*X, "% of infections"))

lines(c(1:N)/N, cumproportionoftransmissions,  col="blue")
# Now same for model P1.0.0
a0<-0.06
b<-0.66
phi0 <-0.33
k0<-0.10
#  mu for 1 primary infections
m <- a0+ b
phi<-phi0+ k0

transmissions<-rnbinom(N,size=phi, mu=m)
transmissions<-sort(transmissions,decreasing = T)
cumproportionoftransmissions<-cumsum(transmissions)/(sum(transmissions))
Y<-cumproportionoftransmissions[N*0.2]
X<-findInterval(0.8,cumproportionoftransmissions)/N 

print("Outcome:probable and definite; Model  P1.0.0 ")
print(paste(100*Y,"% of transmissions result from 20% of infections"))
print(paste("80% of transmissions result from ",100*X, "% of infections"))

lines(c(1:N)/N, cumproportionoftransmissions, col="red")

abline(h=0.8, col="grey", lty=3)
abline(v=0.2, col="grey", lty=3)


legend("bottomright", lty=1, col=c("black", "red", "blue"),legend = c("Model P1.1.1", "Model P1.1.0", "Model P1.0.0"))
#  See, for example, Lloyd-Smith et al 2005 (SI) for corresponding caluclations without simulation




#  Now repeat above for the model with only definite healthcare associated infections
# Take parameters from  model P1.1.1 with outcome 3: Definite healthcare associated infections.
# Table S9 - considering transmission to patients from nosocomially infected patients

a0<-0.04
b<-0.56
phi0 <-0.34
k0<-0.07
#  mu for 1 primary infections
m <- a0+ b
phi<-phi0+ k0

transmissions<-rnbinom(N,size=phi, mu=m)
transmissions<-sort(transmissions,decreasing = T)
cumproportionoftransmissions<-cumsum(transmissions)/(sum(transmissions))
Y<-cumproportionoftransmissions[N*0.2]
X<-findInterval(0.8,cumproportionoftransmissions)/N 

print("Outcome: definite; Model  P1.1.1 ")
print(paste(100*Y,"% of transmissions result from 20% of infections"))
print(paste("80% of transmissions result from ",100*X, "% of infections"))


plot(c(1:N)/N, cumproportionoftransmissions, type ='l', xlab="Proportion of infections (ranked)", ylab="Proportion of transmission",
     main="Definite healthcare associated infection")

# Now same for model P1.1.0, Table S10
a0<-0.04
b<-1.02
phi0 <-0.37
k0<-0.07
#  mu for 1 primary infections
m <- a0+ b
phi<-phi0+ k0

transmissions<-rnbinom(N,size=phi, mu=m)
transmissions<-sort(transmissions,decreasing = T)
cumproportionoftransmissions<-cumsum(transmissions)/(sum(transmissions))
Y<-cumproportionoftransmissions[N*0.2]
X<-findInterval(0.8,cumproportionoftransmissions)/N 

print("Definite; Model  P1.1.0 ")
print(paste(100*Y,"% of transmissions result from 20% of infections"))
print(paste("80% of transmissions result from ",100*X, "% of infections"))

lines(c(1:N)/N, cumproportionoftransmissions,  col="blue")
# Now same for model P1.0.0, Table S11
a0<-0.03
b<-0.63
phi0 <-0.35
k0<-0.06
#  mu for 1 primary infections
m <- a0+ b
phi<-phi0+ k0

transmissions<-rnbinom(N,size=phi, mu=m)
transmissions<-sort(transmissions,decreasing = T)
cumproportionoftransmissions<-cumsum(transmissions)/(sum(transmissions))
Y<-cumproportionoftransmissions[N*0.2]
X<-findInterval(0.8,cumproportionoftransmissions)/N 

print("Outcome:definite; Model  P1.0.0 ")
print(paste(100*Y,"% of transmissions result from 20% of infections"))
print(paste("80% of transmissions result from ",100*X, "% of infections"))

lines(c(1:N)/N, cumproportionoftransmissions, col="red")

abline(h=0.8, col="grey", lty=3)
abline(v=0.2, col="grey", lty=3)


legend("bottomright", lty=1, col=c("black", "blue", "red"),legend = c("Model P1.1.1", "Model P1.1.0", "Model P1.0.0"))

