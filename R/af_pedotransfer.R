# Pedotransfer functions for hydraulic properties of soil
# (adapted from Wosten et al., 1998)
# @author DHarja


#     Return Theta sat (Total saturated porosity) in m3 water m-3 soil
#
#     @param clay = Percentage of clay
#     @param bulkDensity
#     @param silt = Percentage of silt
#     @param organicMatter = Percentage of organic matter (1.7*C-Organic)
#     @param topSoil = Top Soil ? (Type 1 for top soil, 0 for sub soil)
pt.thetaSat.temp <- function(clay,
                             bulkDensity,
                             silt,
                             organicMatter,
                             topSoil) {
  return (
    0.7919
    + 0.00169 * clay
    - 0.29619 * bulkDensity
    - 0.00000149 * silt * silt
    + 0.0000821 * organicMatter * organicMatter
    + 0.02427 / clay
    + 0.01113 / silt
    + 0.01472 * log(silt)
    - 0.0000733 * organicMatter * clay
    - 0.000619 * bulkDensity * clay
    - 0.001183 * bulkDensity * organicMatter
    - 0.0001664 * topSoil * silt
  )
}

# CEC (required when using Tomasella-Hodnett)    3.97
# pH (required when using Tomasella-Hodnett)	4.6
pt.thetaSat.tropic <- function(clay, sand, bulkDensity, cec, ph) {
  return((
    81.799 + (0.099 * clay) - (31.42 * bulkDensity) + (0.018 * cec) + (0.451 *
                                                                         ph) - (0.0005 * sand * clay)
  ) / 100)
}

pt.thetaResid.tropic <- function(clay, sand, cec, ph) {
  return((
    22.733 - (0.164 * sand) + (0.235 * cec) - (0.831 * ph) + (0.0018 * (clay ^
                                                                          2)) + (0.0026 * sand * clay)
  ) / 100)
}

pt.thetaResid.temp <- function() {
  return(0)
}

#     * Return Ksat (saturated conductivity)	in	cm d-1
#     *
#     * @param clay = Percentage of clay
#     * @param bulkDensity
#     * @param silt = Percentage of silt
#     * @param organicMatter = Percentage of organic matter (1.7*C-Organic)
#     * @param topSoil = Top Soil ? (Type 1 for top soil, 0 for sub soil)
pt.kSat <- function(clay,
                    bulkDensity,
                    silt,
                    organicMatter,
                    topSoil) {
  return (
    exp(
      7.755
      + 0.0352 * silt
      + 0.93 * topSoil
      - 0.967 * bulkDensity * bulkDensity
      - 0.000484 * clay * clay
      - 0.000322 * silt * silt
      + 0.001 / silt
      - 0.0748 / organicMatter
      - 0.643 * log(silt)
      - 0.01398 * bulkDensity * clay
      - 0.1673 * bulkDensity * organicMatter
      + 0.02986 * topSoil * clay
      - 0.03305 * topSoil * silt
    )
  )
}

#     * @param clay = Percentage of clay
#     * @param bulkDensity
#     * @param silt = Percentage of silt
#     * @param organicMatter = Percentage of organic matter (1.7*C-Organic)
#     * @param topSoil = Top Soil ? (Type 1 for top soil, 0 for sub soil)
pt.alpha.temp <- function(clay,
                          bulkDensity,
                          silt,
                          organicMatter,
                          topSoil) {
  return(
    exp(
      -14.96
      + 0.03135 * clay
      + 0.0351 * silt
      + 0.646 * organicMatter
      + 15.29 * bulkDensity
      - 0.192 * topSoil
      - 4.671 * bulkDensity * bulkDensity
      - 0.000781 * clay * clay
      - 0.00687 * organicMatter * organicMatter
      + 0.0449 / organicMatter
      + 0.0663 * log(silt)
      + 0.1482 * log(organicMatter)
      - 0.04546 * bulkDensity * silt
      - 0.4852 * bulkDensity * organicMatter
      + 0.00673 * topSoil * clay
    )
  )
}

pt.alpha.tropic <- function(silt, organicC, cec, ph) {
  return(exp((
    -2.294 - (3.526 * silt) + (2.44 * organicC) - (0.076 * cec) - (11.331 *
                                                                     ph) + (0.019 * (silt ^ 2))
  ) / 100) * 10)
}
#     * @param clay = Percentage of clay
#     * @param bulkDensity
#     * @param silt = Percentage of silt
#     * @param organicMatter = Percentage of organic matter (1.7*C-Organic)
pt.lambda  <- function(clay, bulkDensity, silt, organicMatter) {
  return(((10 * (
    exp(
      0.0202
      + 0.0006193 * clay * clay
      - 0.001136 * organicMatter * organicMatter
      - 0.2316 * log(organicMatter)
      - 0.03544 * bulkDensity * clay
      + 0.00283 * bulkDensity * silt
      + 0.0488 * bulkDensity * organicMatter
    )
  ))
  - 10)
  / (1 + (
    exp(
      0.0202
      + 0.0006193 * clay * clay
      - 0.001136 * organicMatter * organicMatter
      - 0.2316 * log(organicMatter)
      - 0.03544 * bulkDensity * clay
      + 0.00283 * bulkDensity * silt
      + 0.0488 * bulkDensity * organicMatter
    )
  )))
}

#     * @param clay = Percentage of clay
#     * @param bulkDensity
#     * @param silt = Percentage of silt
#     * @param organicMatter = Percentage of organic matter (1.7*C-Organic)
#     * @param topSoil = Top Soil ? (Type 1 for top soil, 0 for sub soil)
pt.n.temp <- function(clay,
                      bulkDensity,
                      silt,
                      organicMatter,
                      topSoil) {
  return (
    exp(
      -25.23
      - 0.02195 * clay
      + 0.0074 * silt
      - 0.194 * organicMatter
      + 45.5 * bulkDensity
      - 7.24 * bulkDensity * bulkDensity
      + 0.0003658 * clay * clay
      + 0.002885 * organicMatter * organicMatter
      - 12.81 / bulkDensity
      - 0.1524 / silt
      - 0.01958 / organicMatter
      - 0.2876 * log(silt)
      - 0.0709 * log(organicMatter)
      - 44.6 * log(bulkDensity)
      - 0.02264 * bulkDensity * clay
      + 0.0896 * bulkDensity * organicMatter
      + 0.00718 * topSoil * clay
    )
    + 1
  )
}

pt.n.tropic <- function(clay, silt, sand, organicC, ph) {
  return (exp((
    62.986 - (0.833 * clay) - (0.592 * organicC) + (0.593 * ph) + (0.007 * (clay ^
                                                                              2)) - (0.014 * silt * sand)
  ) / 100))
}
#     * Return BulkDensity
#     * @param meanParticleSize = Median particle size of sand
#     * @param clay = Percentage of clay
#     * @param silt = Percentage of silt
#     * @param organicMatter = Percentage of organic matter (1.7*C-Organic)
#     * @param topSoil = Top Soil ? (Type 1 for top soil, 0 for sub soil)
#     * @return BulkDensity
pt.bulkDensity <- function(meanParticleSize,
                           clay,
                           silt,
                           organicMatter,
                           topSoil) {
  if ((clay + silt) < 50) {
    return(
      1 / (
        -1.984
        + 0.01841 * organicMatter
        + 0.032 * topSoil
        + 0.00003576 * (clay + silt) * (clay + silt)
        + 67.5 / meanParticleSize
        + 0.424 * log(meanParticleSize)
      )
    )
  }
  return(1 / (
    0.603 + 0.003975 * clay + 0.00207 * organicMatter * organicMatter + 0.01781 *
      log(organicMatter)
  ))
}

pt.pF <- function(theta, thetaSat, alpha, n) {
  p <- ((log(1 / alpha) * n) + log((theta / thetaSat) ^ (-n / (n - 1)) - 1)) /
    (n * (log(2) + log(5)))
  if (is.infinite(p) || is.nan(p)) {
    return(0)
  }
  return(p)
}

pt.p <- function(pF) {
  return(-(10 ^ pF))
}

pt.vanGenuchten <- function(rootDiameter, rootDensity) {
  if (rootDensity <= 0)
    return(0)
  return(max(0.001, -3 / 8 - 0.5 * log(
    rootDiameter * 0.5 * sqrt(pi * rootDensity)
  )))
}

pt.organicMatter <- function(organicC) {
  return(organicC * 1.7)
}

pt.sand <- function(clay, silt) {
  return(100 - clay - silt)
}

pt.theta <- function(p, thetaSat, alpha, n, thetaResid) {
  return(thetaResid + ((thetaSat - thetaResid) / (1 + (abs(
    alpha * p
  )) ^ n) ^ (1 - 1 / n)))
}

pt.theta.temp <- function(p,
                          clay,
                          bulkDensity,
                          silt,
                          organicC,
                          topSoil) {
  organicMatter <- pt.organicMatter(organicC)
  thetaSat <- pt.thetaSat.temp(clay, bulkDensity, silt, organicMatter, topSoil)
  alpha <- pt.alpha.temp(clay, bulkDensity, silt, organicMatter, topSoil)
  n <- pt.n.temp(clay, bulkDensity, silt, organicMatter, topSoil)
  thetaResid <- 0
  theta <- pt.theta(p, thetaSat, alpha, n, thetaResid)
  return(theta)
}

pt.theta.tropic <- function(p,
                            clay,
                            bulkDensity,
                            silt,
                            organicC,
                            cec,
                            ph) {
  sand <- pt.sand(clay, silt)
  thetaSat <- pt.thetaSat.tropic(clay, sand, bulkDensity, cec, ph)
  alpha <- pt.alpha.tropic(silt, organicC, cec, ph)
  n <- pt.n.tropic(clay, silt, sand, organicC, ph)
  thetaResid <- pt.thetaResid.tropic(clay, sand, cec, ph)
  theta <- pt.theta(p, thetaSat, alpha, n, thetaResid)
  return(theta)
}

pt.conductivity <- function(p, kSat, alpha, lambda, n) {
  return(kSat * ((((
    1 + ((abs(alpha * p)) ^ n)
  ) ^ (
    1 - 1 / n
  )) - ((
    abs(alpha * p)
  ) ^ (
    n - 1
  ))) ^ 2)
  / ((1 + ((
    abs(alpha * p)
  ) ^ n)) ^ ((1 - 1 / n) * (lambda + 2))))
}

pt.conductivity.temp <- function(p,
                                 clay,
                                 bulkDensity,
                                 silt,
                                 organicC,
                                 topSoil) {
  organicMatter <- pt.organicMatter(organicC)
  kSat <- pt.kSat(clay, bulkDensity, silt, organicMatter, topSoil)
  alpha <- pt.alpha.temp(clay, bulkDensity, silt, organicMatter, topSoil)
  lambda <- pt.lambda(clay, bulkDensity, silt, organicMatter)
  n <- pt.n.temp(clay, bulkDensity, silt, organicMatter, topSoil)
  return(pt.conductivity(p, kSat, alpha, lambda, n))
}

pt.conductivity.tropic <- function(p,
                                   clay,
                                   bulkDensity,
                                   silt,
                                   organicC,
                                   topSoil,
                                   cec,
                                   ph) {
  organicMatter <- pt.organicMatter(organicC)
  kSat <- pt.kSat(clay, bulkDensity, silt, organicMatter, topSoil)
  alpha <- pt.alpha.tropic(silt, organicC, cec, ph)
  lambda <- pt.lambda(clay, bulkDensity, silt, organicMatter)
  sand <- pt.sand(clay, silt)
  n <- pt.n.tropic(clay, silt, sand, organicC, ph)
  return(pt.conductivity(p, kSat, alpha, lambda, n))
}

pt.phi.temp <- function(p,
                        clay,
                        bulkDensity,
                        silt,
                        organicC,
                        topSoil) {
  phi <- c()
  for (x in p) {
    v <- integrate(
      pt.conductivity.temp,
      -Inf,
      x,
      clay,
      bulkDensity,
      silt,
      organicC,
      topSoil,
      abs.tol = 10
    )
    phi <- append(phi, v$value)
  }
  return(phi)
}

pt.phi.tropic <- function(p,
                          clay,
                          bulkDensity,
                          silt,
                          organicC,
                          topSoil,
                          cec,
                          ph) {
  phi <- c()
  for (x in p) {
    v <- integrate(
      pt.conductivity.tropic,
      -Inf,
      x,
      clay,
      bulkDensity,
      silt,
      organicC,
      topSoil,
      cec,
      ph,
      abs.tol = 10
    )
    phi <- append(phi, v$value)
  }
  return(phi)
}