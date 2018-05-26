
#' @export
###from Hydromad
snow.sim <-
  function(E,P, Tmax, Tmin, kd, kf, rcap, Tmelt = 0,
           cr = 1, cs = 1, LSWE_0 = 0, ISWE_0 = SWE) {

    ## check values
    stopifnot(0 <= kd)
    stopifnot(0 <= kf)
    stopifnot(0 <= cr)
    stopifnot(0 <= cs)
    stopifnot(0 <= rcap)
    Tmin <- min(Tmax, Tmin)
    
    
    ## rainfall or snowfall
    fr <- (E - Tmin) / (Tmax - Tmin)
    fr <- pmax(pmin(fr, 1), 0)
    Prain <- fr * cr * P
    Psnow <- (1-fr) * cs * P
    SWE <- Sdischarge <- P * 0
    LSWEprev <- LSWE_0
    ISWEprev <- ISWE_0
    for (t in seq(1, length(P))) {
      ## Melt (degree day model)
      melt <- min(max(kd*(E[t]-Tmelt),0),ISWEprev)
      ##Freezing (degree day model)
      freeze <- min(max(kf*(Tmelt-E[t]),0),LSWEprev)
      ##Mass balance for the snowpack
      ##
      ## Ice in the snowpack
      ISWE <- ISWEprev+Psnow[t]+freeze-melt
      ## Water in the snowpack
      LSWE <- min(rcap*ISWE,LSWEprev+Prain[t]+melt-freeze)
      ##Rain/melt is snowmelt discharge when there is snow on the ground,
      ##and rainfall in snow-free periods.
      Sdischarge[t] <- max(Prain[t]+melt-freeze-(rcap*ISWE-LSWEprev),0)
      SWE[t] <- LSWE+ISWE
      ISWEprev <- ISWE
      LSWEprev <- LSWE
    }
    
    return(Sdischarge)
  }
    