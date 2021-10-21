# survival models

max_time <- 150 # let's say days
timestep <- 0.1
samples <- 5000
time <- seq(0, max_time / timestep)  # adapt observation frequency
death_models <- list()

# constant death rate
death_models[["constant"]] <- rep(0.02, length(time)) * timestep

# linearly increasing death rate
death_models[["linear"]] <- seq(0, .1, length.out = length(time)) * timestep


survival_time <- matrix(nrow = samples, ncol = length(death_models))
for (m in 1:length(death_models)) {
    for (i in 1:samples) {
        survival <- rbinom(max(time), 1, 1 - death_models[[m]])
        survival_time[i, m] <- which(survival == 0)[1]
        survival_time <- ifelse(is.na(survival_time), Inf, survival_time)
        colnames(survival_time) <- names(death_models)

    }
}

survival_time <- survival_time * timestep  # convert time back to days

par(mfrow = c(2, 2))
plot(time, death_models[["constant"]])
hist(survival_time[, 1], xlim = c(0, max_time))
plot(time, death_models[["linear"]])
hist(survival_time[, 2], xlim = c(0, max_time))
