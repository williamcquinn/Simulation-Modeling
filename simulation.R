industry <- 5000000
sens <- 1.25 * 5000000

sens_prob <- 1 / sens
crash_prob <- 1 / industry


n <- 1000000
#set.seed(200)

b <- 47
bc <- 56.4

m200 <- 15
m200c <- 78.9

m300 <- 24
m300c <- 88.5

b_5 <- matrix(nrow = n, ncol = 5)
m200_5 <- matrix(nrow = n, ncol = 5)
m300_5 <- matrix(nrow = n, ncol = 5)

create <- function(n, freq, flights, crash_prob) {
  mat <- matrix(nrow = n, ncol = 5)
  for(i in 1:5) {
    losses <- rbinom(n, round(342 * flights * freq), crash_prob)  
    for (j in 1:n) {
      mat[j, i] <- losses[j]
    }
  }
  return(mat)
}

# 1 in 5M chance of a crash
#b_5 <- create(n, 6, 47, crash_prob)
#m200_5 <- create(n, 2.25, 15, crash_prob)
#m300_5 <- create (n, 2, 24, crash_prob)

# 25% safer crash rates
b_5 <- create(n, 6, 47, sens_prob)
m200_5 <- create(n, 2.25, 15, sens_prob)
m300_5 <- create (n, 2, 24, sens_prob)

inc_dam <- matrix(nrow = n, ncol = 5)
for(inc in 1:5) {
  minc <- runif(n, 1, 5)
  for (inc2 in 1:n) {
    inc_dam[inc2, inc] <- minc[inc2]
  }
}

#RCNC1
#0.45% fleet + 10% losses + 20% cumulative profit rebate @ end of 5yrs.
rcnc1_b_fixed <- (0.45 / 100) * b * bc
rc1_200_fixed <- (0.45 / 100) * m200 * m200c
rc1_300_fixed <- (0.45 / 100) * m300 * m300c
rc1_fixed <- rcnc1_b_fixed + rc1_200_fixed + rc1_300_fixed

rcnc1_b_damages <- (0.1) * b_5[1:n, 1] * bc
rc1_200_damages <- (0.1) * m200_5[1:n, 1] * m200c
rc1_300_damages <- (0.1) * m300_5[1:n, 1] * m300c
rc1_dam <- rcnc1_b_damages + rc1_200_damages + rc1_300_damages + (0.1 * inc_dam[1:n, 1])

rcnc1_b_rebate <- 0.2 * ((5 * rcnc1_b_fixed) - (sum(b_5) * bc * 0.1))
rc1_200_rebate <- 0.2 * ((5 * rc1_200_fixed) - (sum(m200_5) * m200c * 0.1))
rc1_300_rebate <- 0.2 * ((5 * rc1_300_fixed) - (sum(m300_5) * m300c * 0.1))
rc_reb <- rcnc1_b_rebate + rc1_200_rebate + rc1_300_rebate

rcnc1 <- rc1_fixed + rc1_dam
rcnc1_5 <- (5 * rc1_fixed) - max(rc_reb, 0) + ((mean(inc_dam) + (mean(b_5) * bc) + 
                                  (mean(m200_5) * m200c) + (mean(m300_5) * m300c)) * 0.1)
#rcnc1_5 <- mean(5 * rcnc1) - max(rc_reb, 0)
#mean(rcnc1 > 37)
#mean(rcnc1_5)

#RCNC2
#0.10% fleet + min(90% total losses, 1% insured fleet value)

rcnc2_b_fixed <- (0.1 / 100) * b * bc
rc2_200_fixed <- (0.1 / 100) * m200 * m200c
rc2_300_fixed <- (0.1 / 100) * m300 * m300c
rc2_fixed <- rcnc2_b_fixed + rc2_200_fixed + rc2_300_fixed

#losses
rc2_dam <- (0.9) * ((b_5[1:n, 1] * bc) + (m200_5[1:n, 1] * m200c) + 
                      (m300_5[1:n, 1] * m300c) + inc_dam[1:n, 1])

rc2_dam_5 <- (0.9) * (((b_5) * bc) + ((m200_5)* m200c) + 
                        ((m300_5)* m300c) + (inc_dam))
  
#insured fleet value
rcnc2_b_fleet <- (0.01) * b * bc
rc2_200_fleet <- (0.01) * m200 * m200c
rc2_300_fleet <- (0.01) * m300 * m300c
rc2_fleet <- rcnc2_b_fleet + rc2_200_fleet + rc2_300_fleet

rcnc2 <- rc2_fixed + pmin(rc2_dam, rc2_fleet)
rcnc2_5 <- (5 * rc2_fixed) + pmin(rc2_dam_5, (5 * rc2_fleet))
#mean(rcnc2_5)
#mean(rcnc2 > 37)

#CTC
#$13mil annually + 10% losses up to 80mil + anything over 80mil
ctc_fixed <- 13

ctc_damages_1 <- ((b_5[1:n, 1] * bc) + (m200_5[1:n, 1] * m200c) + (m300_5[1:n, 1] * m300c) + inc_dam[1:n, 1])
ctc_dam_1 <- ifelse(ctc_damages_1 > 80, ((0.1 * ctc_damages_1) + (ctc_damages_1 - 80)),
                  ((0.1) * ctc_damages_1))

ctc_dam <- replicate(n, 0)
for(col in 1:5) {
  ctc_damages <- ((b_5[1:n, col] * bc) + (m200_5[1:n, col] * m200c) + 
                    (m300_5[1:n, col] * m300c) + inc_dam[1:n, col])
  ctc_dams <- ifelse(ctc_damages > 80, ((0.1 * ctc_damages) + (ctc_damages - 80)),
                    ((0.1) * ctc_damages))
  ctc_dam <- ctc_dam + ctc_dams
}
#ctc_dam

ctc <- ctc_fixed + ctc_dam_1

ctc_5 <- ctc_dam + (5 * ctc_fixed)
#mean(ctc > 37)

#HIC
#0.165% fleet + losses upto $24 mil + 3.5% cumulative profits rebate @ end of 5yrs.
hic_b_fixed <- (0.165 / 100) * b * bc
hic_200_fixed <- (0.165 / 100) * m200 * m200c
hic_300_fixed <- (0.165 / 100) * m300 * m300c
hic_fixed <- hic_b_fixed + hic_200_fixed + hic_300_fixed

hic_damages_1 <- ((b_5[1:n, 1] * bc) + (m200_5[1:n, 1] * m200c) + (m300_5[1:n, 1] * m300c) + inc_dam[1:n, 1])
hic_dam_1 <- ifelse(hic_damages_1 > 24.0, 24.0, hic_damages_1)

hic_dam <- replicate(n, 0)
for(col_h in 1:5) {
  hic_damages <- ((b_5[1:n, col_h] * bc) + (m200_5[1:n, col_h] * m200c) + 
                    (m300_5[1:n, col_h] * m300c) + inc_dam[1:n, col_h])
  hic_dams <- ifelse(hic_damages > 24, 24, hic_damages)
  hic_dam <- hic_dam + hic_dams
}

hic_reb <- 0.035 * ((5 * hic_fixed) - mean(hic_dam))

hic <- hic_fixed + hic_dam_1
hic_5 <- hic_dam - hic_reb + (5 * hic_fixed)
#mean(hic > 37)

#comparative
cat('RCNC1 cost year 1:', mean(rcnc1))
cat('RCNC2 cost year 1:', mean(rcnc2))
cat('CTC cost year 1:', mean(ctc))
cat('HIC cost year 1:', mean(hic))

#standard deviations
cat('RCNC1 sd year 1:', sd(rcnc1))
cat('RCNC2 sd year 1:', sd(rcnc2))
cat('CTC sd year 1:', sd(ctc))
cat('HIC sd year 1:', sd(hic))

cat('RCNC1 per year:', mean(rcnc1 > 37))
cat('RCNC2 per year:', mean(rcnc2 > 37))
cat('CTC per year:', mean(ctc > 37))
cat('HIC per year:', mean(hic > 37))

##FOR 5 YEARS
cat('RCNC1 after 5 years:', mean(rcnc1_5))
cat('RCNC2 after 5 years:', mean(rcnc2_5))
cat('CTC after 5 years:', mean(ctc_5))
cat('HIC after 5 years:', mean(hic_5))
