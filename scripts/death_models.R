# survival models

max_time <- 150 # let's say days
timestep <- 0.1
samples <- 5000
time <- seq(0, max_time / timestep)
death_models <- list()

# constant death rate
death_models[["constant"]] <- rep(0.02, length(time)) * timestep

# linearly increasing death rate
death_models[["linear"]] <- seq(0, .1, length.out = length(time)) * timestep


observed_deaths <- matrix(nrow = samples, ncol = length(death_models))
for (m in 1:length(death_models)) {
    for (i in 1:samples) {
        survival <- rbinom(max(time), 1, 1 - death_models[[m]])
        observed_deaths[i, m] <- which(survival == 0)[1] * timestep
        observed_deaths <- ifelse(is.na(observed_deaths), Inf, observed_deaths)
        colnames(observed_deaths) <- names(death_models)
    }
}


par(mfrow = c(2, 2))
plot(time, death_models[["constant"]])
hist(observed_deaths[, 1], xlim = c(0, max_time))
plot(time, death_models[["linear"]])
hist(observed_deaths[, 2], xlim = c(0, max_time))
