#single sampling
N = 2000 ; n= 25 ; c=2
p=c(0,0.01,0.02,0.04,0.06,0.08,0.10,0.12,0.14,0.16,0.18,0.20)
#probabbility of acceptance
pa=pbinom(c,n,p)
#Average Outgoing Quality (AOQ)
AOQ=(pa*p*(N-n))/N
# Average Total Inspection
ATI= n+(1-pa)*(N-n)
result = data.frame (Incoming_p = p, pa = round(pa,4),AOQ= round(AOQ,4), ATI = round(ATI,2))
print(result)
#par(mfrow=c(1,3))
plot(p,pa,type="b",pch=19, xlab ="Fraction Defective (p)", ylab= "Probability of acceptance (pa)", main ="OC Curve")
plot(p,AOQ,type="b",pch=19, xlab ="Fraction Defective (p)", ylab= "AOQ", main ="AOQ Curve")
plot(p,ATI,type="b",pch=19, xlab ="Fraction Defective (p)", ylab= "ATI", main ="ATI Curve")




#Double sampling plan
n1 <- 3 ; n2 <- 6 ; c1 <- 0 ; c2 <- 1
p <- c(0, 0.05, 0.1, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
pa <- numeric(length(p))
asn <- numeric(length(p))
for(i in 1:length(p)){
  pp <- p[i]
  # Acceptance in first sample
  p_accept1 <- pbinom(c1, n1, pp)
  # Continue probability
  p_continue <- pbinom(c2, n1, pp) - pbinom(c1, n1, pp)
  # Acceptance after second sample
  p_accept2 <- 0
  
  for(x in (c1 + 1):c2){
    p_accept2 <- p_accept2 +
      dbinom(x, n1, pp) * pbinom(c2 - x, n2, pp)
  }
  # Total probability of acceptance
  pa[i] <- p_accept1 + p_accept2
  # Average Sample Number
  asn[i] <- n1 + n2 * p_continue
}
results <- data.frame(p = p, pa = round(pa, 6), asn = round(asn, 4))
results
plot(p,pa, type = "b", main = "OC Curve")
plot(p,asn, type = "b", main = "ASN Curve")

