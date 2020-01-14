# Model OC

ERR <- function(k){
  
  # Natural Spline Verison
  
  vper <- c(k[1],k[2])
  Q <- quantile(outdoor.clear$UVA.340,vper/100)
  bk <- c(-50,300)
  bkh <- c(45,150)
  
  model.outdoor.clear <- lm(cbind(dE, dGloss60, dHaze) ~ M618 + M626 + Humidity + UnderGlass + ns(UVA.340,knot=Q, Boundary.knots = bk) + 
                              ns(UVA.340,knot=Q, Boundary.knots = bk):M618 +ns(UVA.340,knot=Q, Boundary.knots = bk):M626 + 
                              ns(UVA.340,knot=Q, Boundary.knots = bkh):Humidity +
                              ns(UVA.340,knot=Q, Boundary.knots = bk):UnderGlass + ns(UVA.340,knot=Q, Boundary.knots = bkh):UnderGlass:Humidity , data=outdoor.clear)
  
rse <- sqrt(deviance(model.outdoor.clear)/df.residual(model.outdoor.clear))

error <- rse[[1]]/max(outdoor.clear$dE, na.rm = TRUE) + rse[[2]]/abs(min(outdoor.clear$dGloss60, na.rm = TRUE)) + rse[[3]]/max(outdoor.clear$dHaze, na.rm = TRUE)

error
  
}

nlm(ERR, p = c(91,92), stepmax = 1, steptol = 0.00001, print.level = 2, iterlim = 100)
nlm(ERR, p = c(91,92), stepmax = 1, steptol = 0.00001, print.level = 2, ndigit = 7, iterlim = 1000)

#nls.lm(par = 90,lower = c(85), upper = c(100), fn = ERR)

# Model OW

ERR <- function(k){
  
  # Natural Spline Verison
  
  vper <- c(k[1],k[2])
  knot_Q <- quantile(outdoor.white$UVA.340,vper/100)
  
  model.outdoor.white <- lm(cbind(dE, dGloss60) ~ M238 + MMM6 + UL + U2 + M243 + Humidity + 
                              ns(UVA.340,knot=knot_Q) + ns(UVA.340,knot=knot_Q):M243 +
                              ns(UVA.340,knot=knot_Q):M238 + ns(UVA.340,knot=knot_Q):U2 + ns(UVA.340,knot=knot_Q):UL +
                              ns(UVA.340,knot=knot_Q):MMM6 + ns(UVA.340,knot=knot_Q):Humidity , data=outdoor.white)
  
  rse <- sqrt(deviance(model.outdoor.white)/df.residual(model.outdoor.white))
  
  error <- rse[[1]]/max(outdoor.white$dE, na.rm = TRUE) + rse[[2]]/abs(min(outdoor.white$dGloss60, na.rm = TRUE))
  
  error
  
}

nlm(ERR, p = c(89,92), stepmax = 1, print.level = 2, ndigit = 4, iterlim = 1000)

# Model IC

ERR <- function(k){
  
  # Natural Spline Verison
  
  vper <- c(k[1],k[2])
  Q <- quantile(indoor.clear$UVA.340,vper/100)
  bk <- c(50,175)
  
  model.indoor.clear <- lm(cbind(dE, dGloss60, dHaze) ~ M618 + M626 + Moisture + Chamber.Temp + ns(UVA.340,knot=Q, Boundary.knots = bk) + 
                             ns(UVA.340,knot=Q,Boundary.knots = bk):M618 +ns(UVA.340,knot=Q,Boundary.knots = bk):M626 + 
                             ns(UVA.340,knot=Q,Boundary.knots = bk):Moisture +
                             ns(UVA.340,knot=Q,Boundary.knots = bk):Chamber.Temp , data=indoor.clear)
  summary(model.indoor.clear)
  
  rse <- sqrt(deviance(model.indoor.clear)/df.residual(model.indoor.clear))
  
  error <- rse[[1]]/max(indoor.clear$dE, na.rm = TRUE) + rse[[2]]/abs(min(indoor.clear$dGloss60, na.rm = TRUE)) + rse[[3]]/max(indoor.clear$dHaze, na.rm = TRUE)
  
  error
  
}

#nlm(ERR, p = c(91,93), stepmax = 1, print.level = 2, ndigit = 5, iterlim = 1000)
nlm(ERR, p = c(86,87), stepmax = 1, print.level = 2, ndigit = 4, iterlim = 1000)

# Model IW

ERR <- function(k){
  
  # Natural Spline Verison
  
  vper <- c(k[1],k[2])
  knot_Q <- quantile(indoor.white$UVA.340,vper/100)
  
  model.indoor.white <- lm(cbind(dE, dGloss60) ~ M238 + MMM6 + UL + U2 + M243 + Moisture+ Chamber.Temp + ns(UVA.340,knot=knot_Q) +       ns(UVA.340,knot=knot_Q):M243 +
                             ns(UVA.340,knot=knot_Q):M238 + ns(UVA.340,knot=knot_Q):U2 + ns(UVA.340,knot=knot_Q):UL +
                             ns(UVA.340,knot=knot_Q):MMM6 + ns(UVA.340,knot=knot_Q):Moisture +
                             ns(UVA.340,knot=knot_Q):Chamber.Temp , data=indoor.white)
  summary(model.indoor.white)
  
  rse <- sqrt(deviance(model.indoor.white)/df.residual(model.indoor.white))
  
  error <- rse[[1]]/max(indoor.white$dE, na.rm = TRUE) + rse[[2]]/abs(min(indoor.white$dGloss60, na.rm = TRUE))
  
  error
  
}

nlm(ERR, p = c(80,91), stepmax = 1, print.level = 2, ndigit = 5, iterlim = 1000)

