
T <- 15

P <- 1013.25

H <- .50

W <- .004633

aiRthermo::densityMoistAir(P = P * 100L, Temp = T + 273.15, w = W, consts = aiRthermo::export_constants())

masscor::airDensity(Temp = T, p = P, h = H * 100, unitsENV = c("deg.C", "hPa", "%"), x_CO2 = 4e-04, model = "CIMP2007") * 10^3

bigleaf::air.density(Tair = T, pressure = P / 10)