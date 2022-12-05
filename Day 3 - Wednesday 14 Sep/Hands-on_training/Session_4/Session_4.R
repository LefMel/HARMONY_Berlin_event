## ----setup, include=FALSE-----------------------------------------------------
options(htmltools.dir.version = FALSE)


## ---- eval = FALSE------------------------------------------------------------
## remotes::install_github("AurMad/STOCfree")


## -----------------------------------------------------------------------------
library(STOCfree)


## ---- message=FALSE-----------------------------------------------------------
library(tidyverse)
library(ggplot2)


## ---- echo = FALSE------------------------------------------------------------
herdBTM %>% 
  filter(Test == "BTM_ODR")


## -----------------------------------------------------------------------------
glimpse(herdBTM)


## -----------------------------------------------------------------------------
hbm <- herdBTM %>% 
  filter(Test == "BTM_ODR") %>% 
  select(Farm, DateOfTest, TestResult)

hbm


## -----------------------------------------------------------------------------
sfd <- STOCfree_data(
  test_data = hbm,
  test_herd_col = "Farm",
  test_date_col = "DateOfTest",
  test_res_col = "TestResult"
)


## -----------------------------------------------------------------------------
str(sfd, max.level = 1)


## -----------------------------------------------------------------------------
show_tests(sfd)


## ---- eval = FALSE------------------------------------------------------------
## remotes::install_github("AurMad/betadistapp")


## ---- eval = FALSE------------------------------------------------------------
## betadistapp::shiny_beta()


## -----------------------------------------------------------------------------
sfd <- set_priors_tests(
  x = sfd,
  Se_a = 80,
  Se_b = 20,
  Sp_a = 99,
  Sp_b = 1
)


## -----------------------------------------------------------------------------
show_tests(sfd)


## ---- fig.height = 6, fig.width = 9, fig.align='center'-----------------------
plot_priors_tests(sfd)


## ---- echo=FALSE, fig.align='center', fig.height=7, fig.width=12--------------
par(mfrow = c(1, 2))
curve(log(x / (1 - x)), 
      from = 0, to = 1,
      xlab = "p",
      ylab = "logit(p)",
      main = "logit(p) as a function of p")
abline(h = 0, lty = 2, col = "red")
abline(v = .5, lty = 2, col = "red")

curve(exp(x) / (1 + exp(x)),
      from = -5, to = 5,
      xlab = "logit(p)",
      ylab = "p",
      main = "p as a function of logit(p)")
abline(v = 0, lty = 2, col = "red")
abline(h = .5, lty = 2, col = "red")


## ---- eval = FALSE------------------------------------------------------------
## remotes::install_github("AurMad/logitnormdistapp")


## ---- eval = FALSE------------------------------------------------------------
## logitnormdistapp::shiny_logitnorm()


## -----------------------------------------------------------------------------
sfd <- set_priors_status_dyn(sfd, 
                             logit_pi1_mean = -3,  logit_pi1_sd = 1, 
                             logit_tau1_mean = -3, logit_tau1_sd = 1, 
                             logit_tau2_mean = 3,  logit_tau2_sd = 1)


## ---- fig.height=6.5, fig.align='center'--------------------------------------
plot_priors_status_dyn(sfd)


## ---- eval = FALSE------------------------------------------------------------
## sfm_stan <- STOCfree_Stan(sfd,
##                       n_chains = 3,
##                       n_iter = 1000,
##                       n_thin = 1,
##                       out_path = "STOCfree_Stan_1")


## ---- eval = FALSE------------------------------------------------------------
## param <- extract_STOCfree_param(sfm_stan)


## -----------------------------------------------------------------------------
param <- read_STOCfree_param("STOCfree_Stan_1")


## ---- fig.height = 6, fig.align='center'--------------------------------------
plot(param, parameter = "Se", type = "traceplot")


## ---- fig.height = 6, fig.align='center'--------------------------------------
plot(param, parameter = "Se", type = "density")


## -----------------------------------------------------------------------------
summary(param)


## ---- eval = FALSE------------------------------------------------------------
## pred <- extract_STOCfree_pred(sfm_stan)


## -----------------------------------------------------------------------------
pred <- read_STOCfree_pred("STOCfree_Stan_1")


## ---- fig.align='center', fig.height=6----------------------------------------
plot(pred)


## ---- fig.align='center', fig.height=6----------------------------------------
plot(pred, type = "individual")


## ---- fig.align='center', fig.height=6----------------------------------------
plot(pred, herd = c("FR001", "FR002"), type = "individual", legend = TRUE)


## -----------------------------------------------------------------------------
summary(pred, herd = paste0("FR00", 1:9))


## -----------------------------------------------------------------------------
as_tibble(summary(pred)) %>% 
  filter(`97.5%` <= .1) 

