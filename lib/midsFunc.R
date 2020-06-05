my.glm.mids <- function(formula, family = binomial(link = "logit"), data, ...) {
  call <- match.call()
  if (!is.mids(data)) {
    stop("The data must have class mids")
  }
  analyses <- as.list(1:data$m)
  for (i in 1:data$m) {
    data.i <- mice::complete(data, i)
    analyses[[i]] <- do.call("glm", list(formula = quote(formula), family = quote(family), data = quote(data.i), ...))
  }
  #analyses <- lapply(seq_len(data$m), function(i) glm(formula, 
  #                                                    family = family, data = complete(data, i), ...))
  object <- list(
    call = call, call1 = data$call, nmis = data$nmis,
    analyses = analyses
  )
  oldClass(object) <- c("mira", "glm", "lm")
  return(object)
}

my.coxph.mids <- function(formula, data, ...) {
  call <- match.call()
  if (!is.mids(data)) {
    stop("The data must have class mids")
  }
  analyses <- as.list(1:data$m)
  for (i in 1:data$m) {
    data.i <- mice::complete(data, i)
    analyses[[i]] <- do.call("coxph", list(formula = quote(formula), data = quote(data.i), ...))
  }
  object <- list(
    call = call, call1 = data$call, nmis = data$nmis,
    analyses = analyses
  )
  oldClass(object) <- c("mira", "coxph")
  return(object)
}

# from: https://stackoverflow.com/questions/41794649/can-mice-handle-crr-fine-gray-model
my.crr.mids <- function (time, event, xvars, data, subset, ...) {
    call <- match.call()
    if (!is.mids(data)) 
      stop("The data must have class mids")
    analyses <- as.list(1:data$m)
    for (i in 1:data$m) {
      data.i <- mice::complete(data, i)
      data.i <- data.i %>% filter(!!subset)
      analyses[[i]] <- crr(ftime=data.i[[time]], fstatus=data.i[[event]],  
                           cov1=data.i[, xvars], 
                           failcode=1, cencode=0, variance=TRUE)
    }
    object <- list(call = call, call1 = data$call, nmis = data$nmis, 
                   analyses = analyses)
    oldClass(object) <- c("mira", "matrix")
    return(object)
}

vcov.crr <- function(object, ...) object$var # or getS3method('vcov','coxph')
coef.crr <- function(object, ...) object$coef

tidy.crr <- function(x, ...) {
  co = coef(x)
  data.frame(term = names(co), 
             estimate = unname(co), 
             std.error=sqrt(diag(x$var)), 
             stringsAsFactors = FALSE)
}

glance.crr <- function(x, ...){ }

