## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse  = TRUE,
  comment   = "#>",
  fig.width = 6,
  fig.height = 4.5
)

## ----eval = FALSE-------------------------------------------------------------
# install.packages("wnpmle")

## ----eval = FALSE-------------------------------------------------------------
# # install.packages("remotes")
# remotes::install_github("abellach/wnpmle")

## ----quick--------------------------------------------------------------------
library(wnpmle)

bdata <- bladder_prep()
fit_bladder <- wnpmle_fit(
  Surv(time, status) ~ treat + num + size,
  data  = bdata,
  id    = "id",
  model = "boxcox",
  rho   = 1,
  tau   = 59
)
fit_bladder

## ----readmission-data---------------------------------------------------------
rdata <- readmission_prep(tau = 1460)
head(rdata)
table(rdata$status)

## ----loglik-plot, eval = FALSE------------------------------------------------
# plot_loglik(
#   Surv(time, status) ~ chemo + sex + dukes + charlson,
#   data     = rdata,
#   id       = "id",
#   tau      = 1460,
#   rho_grid = seq(0.05, 1.2, by = 0.05),
#   r_grid   = seq(0.05, 1.2, by = 0.05)
# )

## ----loglik-figure, echo = FALSE, out.width = "85%", fig.align = "center"-----
knitr::include_graphics("loglik_readmission.png")

## ----optimize, eval = FALSE---------------------------------------------------
# f <- function(p) wnpmle_fit(Surv(time, status) ~ chemo + sex + dukes + charlson,
#                             data = rdata, id = "id", model = "boxcox", rho = p,
#                             tau = 1460, se = "none")$loglik
# optimize(f, interval = c(0.2, 1), maximum = TRUE)

## ----fit-readmission----------------------------------------------------------
fit_rd <- wnpmle_fit(
  Surv(time, status) ~ chemo + sex + dukes + charlson,
  data  = rdata,
  id    = "id",
  model = "boxcox",
  rho   = 0.85,
  tau   = 1460,
  se    = "sandwich_adj"
)
summary(fit_rd)

## ----compare------------------------------------------------------------------
fit_gl <- wnpmle_fit(Surv(time, status) ~ chemo + sex + dukes + charlson,
                     data = rdata, id = "id", model = "boxcox", rho = 1,
                     tau = 1460, se = "none")
fit_po <- wnpmle_fit(Surv(time, status) ~ chemo + sex + dukes + charlson,
                     data = rdata, id = "id", model = "log", rho = 1,
                     tau = 1460, se = "none")
sapply(list(boxcox_0.85 = fit_rd, ghosh_lin = fit_gl, prop_odds = fit_po), AIC)

## ----baseline-----------------------------------------------------------------
bl <- baseline(fit_rd)
plot(bl$time, bl$Lambda, type = "s", lwd = 2,
     xlab = "Days since surgery", ylab = expression(hat(Lambda)(t)),
     ylim = range(c(bl$lower, bl$upper), na.rm = TRUE))
lines(bl$time, bl$lower, type = "s", lty = 2, col = "grey50")
lines(bl$time, bl$upper, type = "s", lty = 2, col = "grey50")

## ----predict------------------------------------------------------------------
newdat <- data.frame(chemo    = c("NonTreated", "Treated"),
                     sex      = "Male",
                     dukes    = "C",
                     charlson = "0")
pred <- predict(fit_rd, newdata = newdat, times = seq(0, 1460, by = 7))
head(pred)

plot(pred$time, pred$mu_1, type = "n",
     xlab = "Days since surgery",
     ylab = "Marginal mean number of readmissions",
     ylim = range(pred[, -1]))
polygon(c(pred$time, rev(pred$time)), c(pred$lower_1, rev(pred$upper_1)),
        col = adjustcolor("black", 0.12), border = NA)
polygon(c(pred$time, rev(pred$time)), c(pred$lower_2, rev(pred$upper_2)),
        col = adjustcolor("firebrick", 0.15), border = NA)
lines(pred$time, pred$mu_1, type = "s", lwd = 2)
lines(pred$time, pred$mu_2, type = "s", lwd = 2, lty = 2, col = "firebrick")
legend("topleft", legend = c("No chemotherapy", "Chemotherapy"),
       lty = c(1, 2), col = c("black", "firebrick"), lwd = 2, bty = "n")

