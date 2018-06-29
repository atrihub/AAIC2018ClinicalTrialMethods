options(digits = 3, stringsAsFactors = FALSE, width = 150)

library(ggplot2)
library(gridExtra)
library(tidyverse)
library(nlme)
library(mvtnorm)

# ggplot theme and palette settings
theme_set(theme_bw(base_size = 12))
cbbPalette <- c('#0072B2', '#D55E00', '#CC79A7', '#000000', '#E69F00', '#56B4E9', '#009E73', '#F0E442')
scale_colour_discrete <- function(...) scale_colour_manual(..., values=cbbPalette)
scale_fill_discrete <- function(...) scale_fill_manual(..., values=cbbPalette)

## ------------------------------------------------------------------------
set.seed(20180111)
dd <- data.frame(
    group = 'placebo',
    effect = 'significant effect',
    ADAS.ch = round(rnorm(n=285, mean=3.75, sd=8.5), digits=0)) %>% 
  bind_rows(data.frame(
    group = 'active',
    effect = 'significant effect',
    ADAS.ch = round(rnorm(n=285, mean=1.75, sd=8.5), digits=0))) %>% 
  bind_rows(data.frame(
    group = 'placebo',
    effect = 'no effect',
    ADAS.ch = round(rnorm(n=285, mean=3.75, sd=8.5), digits=0))) %>% 
  bind_rows(data.frame(
    group = 'active',
    effect = 'no effect',
    ADAS.ch = round(rnorm(n=285, mean=3.75, sd=8.5), digits=0))) %>%
  mutate(
    group = factor(group, levels = c('placebo', 'active')),
    effect = factor(effect, levels = c('no effect', 'significant effect')))

t0 <- with(subset(dd, effect=='no effect'), t.test(ADAS.ch ~ group)) 
t1 <- with(subset(dd, effect=='significant effect'), t.test(ADAS.ch ~ group)) 

ann_text <- data.frame(
    group = unique(sort(dd$group))[1],
    effect = unique(sort(dd$effect))[1],
    ADAS.ch = -28,
    label = paste0("Mean group diff.:", round(with(t0, estimate[2] - estimate[1]), 2), "\n", 'p=', round(t0$p.value, 3))) %>% 
  bind_rows(data.frame(
    group = unique(sort(dd$group))[1],
    effect = unique(sort(dd$effect))[2],
    effect = unique(dd$effect)[2],
    ADAS.ch = -28,
    label = paste0("Mean group diff.:", round(with(t1, estimate[2] - estimate[1]), 2), "\n", 'p<0.001')))

ggplot(dd, aes(x=group, y=ADAS.ch, color=group)) +
  geom_violin(aes(fill=group), alpha=.1) +
  geom_boxplot(outlier.colour=NA, width=.5) +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.3, alpha=0.1) +
  facet_wrap(~effect) +
  ylab('3-year ADAS-cog change') +
  xlab('') +
  geom_text(data=ann_text, aes(label=label), nudge_x=0.1) +
  ylim(c(-35, 30)) +
  theme(legend.position='none')


## ----echo=TRUE-----------------------------------------------------------
power.t.test(delta=2, sd=8.5, power=0.80, sig.level=0.05)

## ------------------------------------------------------------------------
power <- seq(0.25, 0.95, by=0.01)
delta <- unlist(lapply(power, function(p) power.t.test(power=p, sd=8.5, sig.level=0.05, n=285)$delta))
SD <- unlist(lapply(power, function(p) power.t.test(power=p, delta=2, sig.level=0.05, n=285, sd=NULL)$sd))
N <- unlist(lapply(power, function(p) power.t.test(power=p, delta=2, sd=8.5, sig.level=0.05)$n))
alpha <- unlist(lapply(power, function(p) power.t.test(power=p, delta=2, sd=8.5, sig.level=NULL, n=285)$sig.level))
p1 <- qplot(delta, power, geom='line', ylim=c(0.25,1)) + geom_vline(xintercept=2, linetype='dashed') + geom_hline(yintercept=0.80, linetype='dashed')
p2 <- qplot(SD, power, geom='line', ylim=c(0.25,1)) + geom_vline(xintercept=8.5, linetype='dashed') + geom_hline(yintercept=0.80, linetype='dashed')
p3 <- qplot(N, power, geom='line', ylim=c(0.25,1)) + geom_vline(xintercept=285, linetype='dashed') + geom_hline(yintercept=0.80, linetype='dashed')
p4 <- qplot(alpha, power, geom='line', ylim=c(0.25,1)) + geom_vline(xintercept=0.05, linetype='dashed') + geom_hline(yintercept=0.80, linetype='dashed')
marrangeGrob(list(p1, p2, p3, p4), nrow=2, ncol=2, top='')

## ----echo=FALSE----------------------------------------------------------
set.seed(20180111)
pvals <- rep(NA, 100000)

## ----echo=TRUE, eval=FALSE-----------------------------------------------
## for(i in 1:100000){
##   placebo <- rnorm(n=285, mean=0, sd=8.5)
##   active <- rnorm(n=285, mean=-2, sd=8.5)
##   pvals[i] <- t.test(placebo, active)$p.value
## }
## sum(pvals<0.05)/100000

## ----eval=FALSE----------------------------------------------------------
## save(pvals, file='ttest_power.rdata')

## ------------------------------------------------------------------------
load('ttest_power.rdata')
cat('Power:', sum(pvals<0.05)/100000)

## ----echo=FALSE----------------------------------------------------------
set.seed(20180111)
pvals <- rep(NA, 100000)

## ----echo=TRUE, eval=FALSE-----------------------------------------------
## for(i in 1:100000){
##   placebo <- rnorm(n=285, mean=0, sd=8.5)
##   active <- rnorm(n=285, mean=0, sd=8.5)
##   pvals[i] <- t.test(placebo, active)$p.value
## }
## sum(pvals<0.05)/100000

## ----eval=FALSE----------------------------------------------------------
## save(pvals, file='ttest_alpha.rdata')

## ------------------------------------------------------------------------
load('ttest_alpha.rdata')
cat('Type I error:', sum(pvals<0.05)/100000)

## ----echo=FALSE----------------------------------------------------------
set.seed(20180111)
pvals <- rep(NA, 100000)

## ----echo=TRUE, eval=FALSE-----------------------------------------------
## for(i in 1:100000){
##   placebo <- round(rnorm(n=285, mean=0, sd=8.5), digits=0)
##   active <- round(rnorm(n=285, mean=-2, sd=8.5), digits=0)
##   pvals[i] <- t.test(placebo, active)$p.value
## }
## sum(pvals<0.05)/100000

## ----eval=FALSE----------------------------------------------------------
## save(pvals, file='ttest_round_power.rdata')

## ------------------------------------------------------------------------
load('ttest_round_power.rdata')
cat('Power:', sum(pvals<0.05)/100000)

## ----echo=FALSE----------------------------------------------------------
set.seed(20180111)
pvals <- rep(NA, 100000)

## ----echo=TRUE, eval=FALSE-----------------------------------------------
## for(i in 1:100000){
##   placebo <- round(rnorm(n=285, mean=0, sd=8.5), digits=0)
##   active <- round(rnorm(n=285, mean=0, sd=8.5), digits=0)
##   pvals[i] <- t.test(placebo, active)$p.value
## }
## sum(pvals<0.05)/100000

## ----eval=FALSE----------------------------------------------------------
## save(pvals, file='ttest_round_alpha.rdata')

## ------------------------------------------------------------------------
load('ttest_round_alpha.rdata')
cat('Type I error:', sum(pvals<0.05)/100000)

## ----echo=FALSE----------------------------------------------------------
set.seed(20180111)
pvals <- rep(NA, 10000)
pvalsBinary <- rep(NA, 10000)

## ----echo=TRUE, eval=FALSE-----------------------------------------------
## cutPoint <- -2
## for(i in 1:10000){
##   dd <- data.frame(
##       ADAS.ch = round(rnorm(n=285, mean=0, sd=8.5), digits=0),
##       group = 'placebo') %>%
##     bind_rows(data.frame(
##       ADAS.ch = round(rnorm(n=285, mean=-2, sd=8.5), digits=0),
##       group = 'active'))
##   pvals[i] <- with(dd, t.test(ADAS.ch~group))$p.value
##   pvalsBinary[i] <- with(dd, chisq.test(ADAS.ch<=cutPoint, group))$p.value
## }
## sum(pvalsBinary<0.05)/10000
## sum(pvals<0.05)/10000

## ----eval=FALSE----------------------------------------------------------
## save(pvals, pvalsBinary, file='ttest_binary_power.rdata')

## ------------------------------------------------------------------------
load('ttest_binary_power.rdata')
cat('Power for binary outcome:', sum(pvalsBinary<0.05)/10000)
cat('Power for continuous outcome:', sum(pvals<0.05)/10000)

## ----echo=FALSE----------------------------------------------------------
set.seed(20180111)
pvals <- rep(NA, 10000)
pvalsBinary <- rep(NA, 10000)

## ----echo=TRUE, eval=FALSE-----------------------------------------------
## cutPoint <- -2
## for(i in 1:10000){
##   dd <- data.frame(
##       ADAS.ch = round(rnorm(n=447, mean=0, sd=8.5), digits=0),
##       group = 'placebo') %>%
##     bind_rows(data.frame(
##       ADAS.ch = round(rnorm(n=447, mean=-2, sd=8.5), digits=0),
##       group = 'active'))
##   pvalsBinary[i] <- with(dd, chisq.test(ADAS.ch<=cutPoint, group))$p.value
##   pvals[i] <- with(dd, t.test(ADAS.ch~group))$p.value
## }
## sum(pvals<0.05)/10000
## sum(pvalsBinary<0.05)/10000

## ----eval=FALSE----------------------------------------------------------
## save(pvals, pvalsBinary, file='ttest_binary_large_power.rdata')

## ------------------------------------------------------------------------
load('ttest_binary_large_power.rdata')
cat('Power for binary outcome:', sum(pvalsBinary<0.05)/10000)
cat('Power for continuous outcome:', sum(pvals<0.05)/10000)

## ----echo=FALSE, eval=FALSE----------------------------------------------
## ####################################################
## ### pilot estimates are from a model fit to ADNI ###
## ####################################################
## # library(ADNIMERGE) # available from loni.usc.edu
## fit_adni_lme <- lme(ADAS13 ~ PTGENDER + scale(AGE,scale=FALSE) + M, data=adnimerge,
##   random=~M|RID, subset = M<=36 & DX.bl=='LMCI', na.action=na.omit)
## summary(fit_adni)
## 
## adni_mci <- filter(adnimerge, M!=0 & M<=36 & !is.na(ADAS13) & !is.na(ADAS13.bl) & DX.bl=='LMCI') %>%
##   arrange(M) %>%
##   mutate(
##     ADAS13.ch = ADAS13 - ADAS13.bl,
##     m = as.factor(M),
##     visNo = as.numeric(m))
## 
## fit_adni_mmrm <- gls(ADAS13.ch ~ -1 + ADAS13.bl + PTGENDER + scale(AGE, scale=FALSE) + m,
##   data=adni_mci,
##   correlation = corSymm(form = ~ visNo | RID),
##   weights = varIdent(form = ~ 1 | m) )
## summary(fit_adni_mmrm)
## 
## fit_adni_mmrm2 <- gls(ADAS13.ch ~ -1 + scale(ADAS13.bl, scale=FALSE) + m,
##   data=adni_mci,
##   correlation = corCompSymm(form = ~ visNo | RID),
##   weights = varIdent(form = ~ 1 | m) )
## summary(fit_adni_mmrm2)
## 
## with(filter(adni_mci, M==36), summary(ADAS13.ch))

## ----mmrm_parameters_mean, echo=FALSE------------------------------------
Beta <- c(
     'ADAS13.bl'=-0.07, # ADAS13 change per unit baseline ADAS13
            'm6'= 0.90, # worsening at month 6 in pbo
           'm12'= 1.30, # worsening at month 12 in pbo
           'm18'= 2.90, # worsening at month 18 in pbo
           'm24'= 4.25, # worsening at month 24 in pbo
           'm30'= 5.50, # worsening at month 30 in pbo
           'm36'= 6.70, # worsening at month 36 in pbo
     'm6:active'=-0.05, # relative improvement at month 6 with treatment
    'm12:active'=-0.10, # relative improvement at month 12 with treatment
    'm18:active'=-0.50, # relative improvement at month 18 with treatment
    'm24:active'=-1.00, # relative improvement at month 24 with treatment
    'm30:active'=-1.50, # relative improvement at month 30 with treatment
    'm36:active'=-2.00) # relative improvement at month 36 with treatment

## ----mmrm_parameters_other, echo=FALSE-----------------------------------
# other design parameters
followup <- c(6, 12, 18, 24, 30, 36)
n <- 485 # per group
attrition_rate <- c(0.00, 0.15, 0.15, 0.20, 0.25, 0.30)

# var-cov parameters:
# standard deviation scale parameter:
SD <- 5
# hetergeneous variance weights:
vv <- diag(c(1.00, 1.00, 1.25, 1.30, 1.50, 2.00))
# correlation matrix
cc <- matrix(0.60, nrow=length(followup), ncol=length(followup))
diag(cc) <- 1

## ----mmrm_design---------------------------------------------------------
# set seed so that simulation is reproducible
set.seed(20170714)

# simulate subject specific data
subjects <- data.frame(
  id = 1:(2*n),
  ADAS13.bl = round(rnorm(2*n, mean=0, sd=18.7)),
  active = c(rep(0,n), rep(1,n)),
  censor = rep(
    x = c(followup, max(followup)+1),
    times = c(ceiling(c(attrition_rate[1], diff(attrition_rate, lag=1))*n),
      n - sum(ceiling(c(attrition_rate[1], diff(attrition_rate, lag=1))*n)))))

design <- right_join(subjects,
  expand.grid(id = 1:(2*n), month=followup)) %>%
  mutate(
    group = factor(active, 0:1, c('placebo', 'active')),
    missing = ifelse(month>=censor, 1, 0),
    m = as.factor(month),
    visNo = as.numeric(m)) %>%
  arrange(id, month)
pboTerms <- c('ADAS13.bl', 'm6', 'm12', 'm18', 'm24', 'm30', 'm36')
design <- cbind(design, model.matrix(~ -1+m, data = design)) %>% 
  mutate(
    ADAS13.ch.noResidual = (model.matrix(~ ADAS13.bl+
       m6+m12+m18+m24+m30+m36+
      (m6+m12+m18+m24+m30+m36):active, 
      data = design)[, names(Beta)] %*% Beta)[,1],
    ADAS13.ch.noResidual.noTxEffect = (model.matrix(~ ADAS13.bl+
       m6+m12+m18+m24+m30+m36, 
      data = design)[, pboTerms] %*% Beta[pboTerms])[,1],
    m = as.factor(month),
    visNo = as.numeric(m))

## ------------------------------------------------------------------------
trial <- arrange(design, id, month) %>%    ## WARNING: data must be properly sorted by subject and time 
  mutate(                                  ## prior to appending residuals
    residual = as.numeric(t(
      rmvnorm(length(unique(design$id)), mean=rep(0,nrow(vv)), sigma=SD^2*vv%*%cc%*%vv))),
    ADAS13.ch = round(ADAS13.ch.noResidual + residual, digits = 0)) %>%
  filter(!missing)

## ----spaghetti_plot, fig.width=6, fig.height=6/2-------------------------
ggplot(filter(trial, id %in% sample(1:(2*n),size=100)), aes(x=month, y=ADAS13.ch, group=id, color=group)) +
  geom_line(alpha=0.25) +
#  geom_smooth(aes(group = NULL), method = 'loess', size = 2) +
  scale_x_continuous(breaks=c(0,followup), lim=c(0,36)) +
  theme(legend.position=c(0.1, 0.8)) +
  ylab('ADAS13 change from baseline')

## ----echo=TRUE-----------------------------------------------------------
16/(2/10)^2 / (1-0.30)
power.t.test(delta=2, sd=10, power=0.80)$n / (1-0.30)

## ----echo=TRUE-----------------------------------------------------------
followup <- c(6,12,18,24,30,36)
cc <- matrix(0.6, nrow=length(followup), ncol=length(followup))
diag(cc) <- 1
attrition_rate <- c(0, 15, 15, 20, 25, 30)/100
longpower::power.mmrm(Ra=cc, ra=1-attrition_rate, sigmaa=10, delta=2, power=0.80)

## ----mmrm_parameters_mean, eval=FALSE, echo=TRUE-------------------------
## Beta <- c(
##      'ADAS13.bl'=-0.07, # ADAS13 change per unit baseline ADAS13
##             'm6'= 0.90, # worsening at month 6 in pbo
##            'm12'= 1.30, # worsening at month 12 in pbo
##            'm18'= 2.90, # worsening at month 18 in pbo
##            'm24'= 4.25, # worsening at month 24 in pbo
##            'm30'= 5.50, # worsening at month 30 in pbo
##            'm36'= 6.70, # worsening at month 36 in pbo
##      'm6:active'=-0.05, # relative improvement at month 6 with treatment
##     'm12:active'=-0.10, # relative improvement at month 12 with treatment
##     'm18:active'=-0.50, # relative improvement at month 18 with treatment
##     'm24:active'=-1.00, # relative improvement at month 24 with treatment
##     'm30:active'=-1.50, # relative improvement at month 30 with treatment
##     'm36:active'=-2.00) # relative improvement at month 36 with treatment

## ----mmrm_parameters_other, eval=FALSE, echo=TRUE------------------------
## # other design parameters
## followup <- c(6, 12, 18, 24, 30, 36)
## n <- 485 # per group
## attrition_rate <- c(0.00, 0.15, 0.15, 0.20, 0.25, 0.30)
## 
## # var-cov parameters:
## # standard deviation scale parameter:
## SD <- 5
## # hetergeneous variance weights:
## vv <- diag(c(1.00, 1.00, 1.25, 1.30, 1.50, 2.00))
## # correlation matrix
## cc <- matrix(0.60, nrow=length(followup), ncol=length(followup))
## diag(cc) <- 1

## ------------------------------------------------------------------------
set.seed(20170714)
pvals <- rep(NA, 1000)
txEffect <- rep(NA, 1000)

## ----mmrm_sim_pow, echo=TRUE, eval=FALSE---------------------------------
## for(i in 1:1000){
##   trial <- arrange(design, id, month) %>%
##     mutate(
##       residual = as.numeric(t(
##         rmvnorm(length(unique(design$id)), mean=rep(0,nrow(vv)), sigma=SD^2*vv%*%cc%*%vv))),
##       ADAS13.ch = round(ADAS13.ch.noResidual + residual, digits = 0)) %>%
##     filter(!missing)
## 
##   trial_fit <- gls(ADAS13.ch ~ -1+ADAS13.bl+
##       (m6+m12+m18+m24+m30+m36)+
##       (m6+m12+m18+m24+m30+m36):active,
##     data=trial, correlation = corCompSymm(form = ~ visNo | id),
##     weights = varIdent(form = ~ 1 | m))
##   pvals[i] <- summary(trial_fit)$tTable['m36:active','p-value']
##   txEffect[i] <- summary(trial_fit)$tTable['m36:active','Value']
## }
## sum(pvals<0.05)/1000
## summary(txEffect)

## ----eval=FALSE----------------------------------------------------------
## save(pvals, txEffect, file='mmrm_power.rdata')

## ------------------------------------------------------------------------
load('mmrm_power.rdata')
txEffect.pow <- txEffect
cat('Power for MMRM:', sum(pvals<0.05)/1000)

## ------------------------------------------------------------------------
set.seed(20170714)
pvals <- rep(NA, 1000)
txEffect <- rep(NA, 1000)

## ----mmrm_sim_alpha, echo=TRUE, eval=FALSE-------------------------------
## for(i in 1:1000){
##   trial <- arrange(design, id, month) %>%
##     mutate(
##       residual = as.numeric(t(
##         rmvnorm(length(unique(design$id)), mean=rep(0,nrow(vv)), sigma=SD^2*vv%*%cc%*%vv))),
##       ADAS13.ch = round(ADAS13.ch.noResidual.noTxEffect + residual, digits = 0)) %>%
##     filter(!missing)
## 
##   trial_fit <- gls(ADAS13.ch ~ -1+ADAS13.bl+
##       (m6+m12+m18+m24+m30+m36)+
##       (m6+m12+m18+m24+m30+m36):active,
##     data=trial, correlation = corCompSymm(form = ~ visNo | id),
##     weights = varIdent(form = ~ 1 | m))
##   pvals[i] <- summary(trial_fit)$tTable['m36:active','p-value']
##   txEffect[i] <- summary(trial_fit)$tTable['m36:active','Value']
## }
## sum(pvals<0.05)/1000
## summary(txEffect)

## ----eval=FALSE----------------------------------------------------------
## save(pvals, txEffect, file='mmrm_alpha.rdata')

## ------------------------------------------------------------------------
load('mmrm_alpha.rdata')
txEffect.alpha <- txEffect
cat('Type I error for MMMR:', sum(pvals<0.05)/1000)

## ------------------------------------------------------------------------
pd <- data.frame(
    Simulation = 'Power',
    Effect = txEffect.pow) %>% 
  bind_rows(data.frame(
    Simulation = 'Type I error',
    Effect = txEffect.alpha)) %>% 
  mutate(Simulation = factor(Simulation, levels = c('Type I error', 'Power')))
ggplot(pd, aes(x=Simulation, y=Effect, color=Simulation)) +
  geom_violin(aes(fill=Simulation), alpha=.1) +
  geom_boxplot(outlier.colour=NA, width=.5) +
  ylab('Estimated group contrast') +
  xlab('') +
  theme(legend.position='none')

## ------------------------------------------------------------------------
set.seed(20170714)
pvals <- rep(NA, 1000)
txEffect <- rep(NA, 1000)

## ----mmrm_sim_mnar_pow, echo=TRUE, eval=FALSE----------------------------
## for(i in 1:1000){
##   trial <- arrange(design, id, month) %>%
##     mutate(
##       residual = as.numeric(t(
##         rmvnorm(length(unique(design$id)), mean=rep(0,nrow(vv)), sigma=SD^2*vv%*%cc%*%vv))),
##       ADAS13.ch = round(ADAS13.ch.noResidual + residual, digits = 0)) %>%
##     filter(ADAS13.ch<12)
## 
##   trial_fit <- gls(ADAS13.ch ~ -1+ADAS13.bl+
##       (m6+m12+m18+m24+m30+m36)+
##       (m6+m12+m18+m24+m30+m36):active,
##     data=trial, correlation = corCompSymm(form = ~ visNo | id),
##     weights = varIdent(form = ~ 1 | m))
##   pvals[i] <- summary(trial_fit)$tTable['m36:active','p-value']
##   txEffect[i] <- summary(trial_fit)$tTable['m36:active','Value']
## }
## sum(pvals<0.05)/1000
## summary(txEffect)

## ----eval=FALSE----------------------------------------------------------
## save(pvals, txEffect, file='mmrm_mnar_pow.rdata')

## ------------------------------------------------------------------------
load('mmrm_mnar_pow.rdata')
cat('Power:', sum(pvals<0.05)/1000)
# summary(txEffect)

## ----echo=TRUE-----------------------------------------------------------
summary(txEffect) - -2

## ------------------------------------------------------------------------
set.seed(20170714)
pvals <- rep(NA, 1000)
txEffect <- rep(NA, 1000)

## ----mmrm_sim_mnar_alpha, echo=TRUE, eval=FALSE--------------------------
for(i in 1:1000){
  trial <- arrange(design, id, month) %>%
    mutate(
      residual = as.numeric(t(
        rmvnorm(length(unique(design$id)), mean=rep(0,nrow(vv)), sigma=SD^2*vv%*%cc%*%vv))),
      ADAS13.ch = round(ADAS13.ch.noResidual.noTxEffect + residual, digits = 0)) %>%
    filter(ADAS13.ch<12)

  trial_fit <- gls(ADAS13.ch ~ -1+ADAS13.bl+
      (m6+m12+m18+m24+m30+m36)+
      (m6+m12+m18+m24+m30+m36):active,
    data=trial, correlation = corCompSymm(form = ~ visNo | id),
    weights = varIdent(form = ~ 1 | m))
  pvals[i] <- summary(trial_fit)$tTable['m36:active','p-value']
  txEffect[i] <- summary(trial_fit)$tTable['m36:active','Value']
}
sum(pvals<0.05)/1000
summary(txEffect)

## ----eval=FALSE----------------------------------------------------------
## save(pvals, txEffect, file='mmrm_mnar_alpha.rdata')

## ------------------------------------------------------------------------
load('mmrm_mnar_alpha.rdata')
cat('Type I error:', sum(pvals<0.05)/1000)
# summary(txEffect)

## ------------------------------------------------------------------------
set.seed(20170714)
pvals <- rep(NA, 1000)
txEffect <- rep(NA, 1000)

## ----mmrm_sim_mnartol_pow, echo=TRUE, eval=FALSE-------------------------
## for(i in 1:1000){
##   trial <- arrange(design, id, month) %>%
##     mutate(
##       residual = as.numeric(t(
##         rmvnorm(length(unique(design$id)), mean=rep(0,nrow(vv)), sigma=SD^2*vv%*%cc%*%vv))),
##       ADAS13.ch = round(ADAS13.ch.noResidual + residual, digits = 0)) %>%
##     filter(ADAS13.ch<18 | active==0)
## 
##   trial_fit <- gls(ADAS13.ch ~ -1+ADAS13.bl+
##       (m6+m12+m18+m24+m30+m36)+
##       (m6+m12+m18+m24+m30+m36):active,
##     data=trial, correlation = corCompSymm(form = ~ visNo | id),
##     weights = varIdent(form = ~ 1 | m))
##   pvals[i] <- summary(trial_fit)$tTable['m36:active','p-value']
##   txEffect[i] <- summary(trial_fit)$tTable['m36:active','Value']
## }
## sum(pvals<0.05)/1000
## summary(txEffect)

## ----eval=FALSE----------------------------------------------------------
## save(pvals, txEffect, file='mmrm_mnartol_pow.rdata')

## ------------------------------------------------------------------------
load('mmrm_mnartol_pow.rdata')
cat('Power:', sum(pvals<0.05)/1000)
# summary(txEffect)

## ----echo=TRUE-----------------------------------------------------------
summary(txEffect) - -2

## ------------------------------------------------------------------------
set.seed(20170714)
pvals <- rep(NA, 1000)
txEffect <- rep(NA, 1000)

## ----mmrm_sim_mnartol_alpha, echo=TRUE, eval=FALSE-----------------------
## for(i in 1:1000){
##   trial <- arrange(design, id, month) %>%
##     mutate(
##       residual = as.numeric(t(
##         rmvnorm(length(unique(design$id)), mean=rep(0,nrow(vv)), sigma=SD^2*vv%*%cc%*%vv))),
##       ADAS13.ch = round(ADAS13.ch.noResidual.noTxEffect + residual, digits = 0)) %>%
##     filter(ADAS13.ch<18 | active==0)
## 
##   trial_fit <- gls(ADAS13.ch ~ -1+ADAS13.bl+
##       (m6+m12+m18+m24+m30+m36)+
##       (m6+m12+m18+m24+m30+m36):active,
##     data=trial, correlation = corCompSymm(form = ~ visNo | id),
##     weights = varIdent(form = ~ 1 | m))
##   pvals[i] <- summary(trial_fit)$tTable['m36:active','p-value']
##   txEffect[i] <- summary(trial_fit)$tTable['m36:active','Value']
## }
## sum(pvals<0.05)/1000
## summary(txEffect)

## ----eval=FALSE----------------------------------------------------------
## save(pvals, txEffect, file='mmrm_mnartol_alpha.rdata')

## ------------------------------------------------------------------------
load('mmrm_mnartol_alpha.rdata')
cat('Type I error:', sum(pvals<0.05)/1000)
# summary(txEffect)

