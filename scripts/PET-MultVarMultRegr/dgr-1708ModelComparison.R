summary(lm(cbind(dE, dGloss60, dHaze) ~ M618 + M626 + Moisture + Chamber.Temp + ns(UVA.340,knot=Q, Boundary.knots = bk) , data=indoor.clear))

gam.indoor.clear <- gam(dE ~ s(UVA.340, k = 4 ,bs = "cr", by = as.factor(Material)) + ti(UVA.340,Chamber.Temp,k = 3) + ti(UVA.340, Wet.Time,k=4), data = indoor.clear)
summary(gam.indoor.clear)

gam.indoor.clear <- gam(dGloss60 ~ s(UVA.340, k = 4 ,bs = "cr", by = as.factor(Material)) + ti(UVA.340,Chamber.Temp,k = 3) + ti(UVA.340, Wet.Time,k=4), data = indoor.clear)
summary(gam.indoor.clear)

gam.indoor.clear <- gam(dHaze ~ s(UVA.340, k = 4 ,bs = "cr", by = as.factor(Material)) + ti(UVA.340,Chamber.Temp,k = 3) + ti(UVA.340, Wet.Time,k=4), data = indoor.clear)
summary(gam.indoor.clear)

