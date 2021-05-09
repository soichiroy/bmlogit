
## load data 
library(ccesMRPprep)
library(emlogit)
cces <-  get_cces_dataverse("cumulative", year_subset = 2019)

## generate population via CCES 
cov_df <- cces %>% select(gender, race, educ) %>% 
  mutate(race = ifelse(race >= 5, 5, race))
cov_mat <- model.matrix(~factor(gender) + factor(race) + factor(educ), cov_df)


n_cov <- ncol(cov_mat)
n_cat <- 4
coefs  <- cbind(0, matrix(runif(n_cov * (n_cat - 1), -0.2, 0.2), nrow = n_cov))

eXb <- exp(cov_mat %*% coefs)
prY <- eXb / rowSums(eXb)

Y <- apply(prY, 1, function(x) sample(1:n_cat, size = 1, prob = x))


Y_mat <- model.matrix(~as.factor(Y)-1)

fit <- emlogit(Y_mat, cov_mat, control = list(intercept = FALSE))

plot(as.vector(coef(fit)), as.vector(coefs))
abline(0, 1)

eXb_hat <- exp(cov_mat %*% coef(fit)); prY_hat <- eXb_hat / rowSums(eXb_hat)
plot(as.vector(prY), as.vector(prY_hat))
abline(0, 1)


# ## sampling data p(S = 1 | X, Y)
intercept <- 500 / nrow(cces)
XY_mat <- cbind(cov_mat, Y_mat[,-1])
coef_sample <- c(0.01, runif(ncol(XY_mat)-1, 0, 0.2))
prS <- 1 / (1 + exp(-XY_mat %*% coef_sample))
S <- rbinom(nrow(XY_mat), size = 1, prob = prS)
hist(prS)



## stratified sampling 
# cov_df <- cov_df %>% mutate(Y = factor(Y))
# group_id <- cov_df %>% group_by (gender, race, educ, Y) %>% group_indices()
  
