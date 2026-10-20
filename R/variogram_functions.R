# Shared final-study calculation helpers.
shape <- function(h, a, model) {
    t <- h/a
    switch(model, spherical = ifelse(t < 1, 1.5 * t - 0.5 * t^3, 1), exponential = 1 - exp(-t), 
        gaussian = 1 - exp(-t^2))
}
sph95 <- uniroot(function(t) 1.5 * t - 0.5 * t^3 - 0.95, c(0, 1), tol = 1e-12)$root
range95 <- function(a, model) a * switch(model, spherical = sph95, exponential = -log(0.05), gaussian = sqrt(-log(0.05)))
fixed_range <- function(loga, h, g, w, model) {
    a <- exp(loga)
    q <- shape(h, a, model)
    qbar <- sum(w * q)
    gbar <- sum(w * g)
    varq <- sum(w * (q - qbar)^2)
    cand <- list(c(nugget = max(0, gbar), partial = 0))
    if (sum(w * q * q) > 0) 
        cand[[length(cand) + 1L]] <- c(nugget = 0, partial = max(0, sum(w * q * g)/sum(w * q * q)))
    if (varq > 1e-14) {
        partial <- sum(w * (q - qbar) * (g - gbar))/varq
        nugget <- gbar - partial * qbar
        if (nugget >= 0 && partial >= 0) 
            cand[[length(cand) + 1L]] <- c(nugget = nugget, partial = partial)
    }
    obj <- vapply(cand, function(b) sum(w * (g - b[1] - b[2] * q)^2), numeric(1))
    k <- which.min(obj)
    list(a = a, nugget = unname(cand[[k]][1]), partial = unname(cand[[k]][2]), objective = obj[k])
}
fit_one <- function(z, model, weighting, year, map) {
    h <- z$mean_distance_m
    g <- z$semivariance
    w <- if (weighting == "pairs_over_distance_squared") 
        z$pairs/h^2
    else rep(1, nrow(z))
    w <- w/sum(w)
    grid <- seq(log(1), log(3000), length.out = 1500)
    scores <- vapply(grid, function(l) fixed_range(l, h, g, w, model)$objective, numeric(1))
    k <- which.min(scores)
    lo <- max(1, k - 1)
    hi <- min(length(grid), k + 1)
    op <- optimize(function(l) fixed_range(l, h, g, w, model)$objective, c(grid[lo], grid[hi]), 
        tol = 1e-11)
    candidates <- lapply(c(grid[k], op$minimum), fixed_range, h = h, g = g, w = w, model = model)
    b <- candidates[[which.min(vapply(candidates, `[[`, numeric(1), "objective"))]]
    total <- b$nugget + b$partial
    zero_partial <- b$partial <= max(g) * 1e-08
    boundary_range <- b$a <= 1.0001 || b$a >= 2999.9
    underresolved <- if (model == "spherical") 
        b$a <= h[2]
    else range95(b$a, model) <= h[1]
    range_identified <- !zero_partial && !boundary_range && !underresolved
    near <- which(scores <= b$objective * 1.01 + max(g)^2 * 1e-12)
    profile_lo <- if (length(near)) 
        exp(min(grid[near]))
    else b$a
    profile_hi <- if (length(near)) 
        exp(max(grid[near]))
    else b$a
    rmse <- sqrt(mean((g - b$nugget - b$partial * shape(h, b$a, model))^2))
    flag <- c(if (zero_partial) "ZERO_PARTIAL_SILL_RANGE_UNIDENTIFIED", if (boundary_range) "RANGE_SEARCH_BOUNDARY", 
        if (underresolved) "RANGE_BELOW_LAG_RESOLUTION", if (range_identified && range95(b$a, model) > 
            max(h)) "PRACTICAL_RANGE_EXTRAPOLATES_BEYOND_OBSERVED_LAGS", if (b$nugget <= max(g) * 
            1e-10) "NUGGET_ZERO_BOUNDARY")
    if (!length(flag)) 
        flag <- "INTERIOR_FIT"
    data.frame(year = year, map = map, model = model, weighting = weighting, bins = nrow(z), nugget = b$nugget, 
        partial_sill = b$partial, total_sill = total, fitted_scale_a_m = b$a, range_parameter_a_m = if (range_identified) 
            b$a
        else NA_real_, practical_range95_m = if (range_identified) 
            range95(b$a, model)
        else NA_real_, finite_sill_range_m = if (range_identified && model == "spherical") 
            b$a
        else NA_real_, nugget_to_total_sill = if (total > 0) 
            b$nugget/total
        else NA_real_, total_sill_to_nugget = if (b$nugget > max(g) * 1e-10) 
            total/b$nugget
        else NA_real_, inverse_ratio_status = if (b$nugget <= max(g) * 1e-10) 
            "UNDEFINED_ZERO_NUGGET"
        else "DEFINED", weighted_mean_squared_residual = b$objective, unweighted_bin_rmse = rmse, 
        max_empirical_lag_m = max(h), range_identified = range_identified, objective_1pct_scale_a_low_m = profile_lo, 
        objective_1pct_scale_a_high_m = profile_hi, flag = paste(flag, collapse = ";"), units = "nugget/sills: (Mg/ha)^2; ranges: m", 
        stringsAsFactors = FALSE)
}
