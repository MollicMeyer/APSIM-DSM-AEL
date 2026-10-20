# Shared final-study calculation helpers.
if (nzchar(Sys.getenv("APSIM_R_LIB"))) .libPaths(c(normalizePath(Sys.getenv("APSIM_R_LIB"), winslash = "/", 
    mustWork = TRUE), .libPaths()))
suppressPackageStartupMessages(library(jsonlite))
need <- function(a, n) {
    if (is.null(a[[n]])) 
        stop("Missing --", n)
    a[[n]]
}
normal <- function(p, mustWork = TRUE) normalizePath(p, winslash = "/", mustWork = mustWork)
md5 <- function(p) unname(tools::md5sum(p))
writej <- function(x, p) write_json(x, p, pretty = TRUE, auto_unbox = TRUE, digits = NA, null = "null")
walk <- function(x) c(list(x), unlist(lapply(x$Children, walk), recursive = FALSE))
type_is <- function(x, t) identical(x[["$type"]], paste0(t, ", Models"))
node <- function(x, t) {
    z <- Filter(function(z) type_is(z, t), walk(x))
    stopifnot(length(z) == 1)
    z[[1]]
}
map_nodes <- function(x, f) {
    x <- f(x)
    if (length(x$Children)) 
        x$Children <- lapply(x$Children, function(z) map_nodes(z, f))
    x
}
vec <- function(x) as.numeric(unlist(x))
mask_allowed <- function(x) map_nodes(x, function(z) {
    if (type_is(z, "Models.Soils.Physical")) 
        z$AirDry <- NULL
    if (type_is(z, "Models.Climate.Weather")) 
        z$FileName <- "<weather>"
    if (type_is(z, "Models.Report")) 
        z$VariableNames <- Filter(function(v) !grepl(" as AD_", v, fixed = TRUE), z$VariableNames)
    z
})
airdry <- function(p) {
    th <- vec(p$Thickness)
    b <- cumsum(th)
    t <- c(0, head(b, -1))
    factor <- (0.5 * pmax(0, pmin(b, 150) - t) + 0.8 * pmax(0, pmin(b, 300) - pmax(t, 150)) + pmax(0, 
        b - pmax(t, 300)))/th
    vec(p$LL15) * factor
}
normcols <- function(z) {
    names(z) <- make.names(names(z), unique = FALSE)
    stopifnot(!anyDuplicated(names(z)))
    z
}
cols <- function(z, n) {
    v <- grep(paste0("^", n, "[.][0-9]+[.]$"), names(z), value = TRUE)
    stopifnot(length(v) > 0)
    v[order(as.integer(sub(paste0("^", n, "[.]([0-9]+)[.]$"), "\\1", v)))]
}
eq <- function(a, b, tol = 1e-07) stopifnot(length(a) == length(b), all(is.finite(a)), all(is.finite(b)), 
    max(abs(a - b)) < tol)
