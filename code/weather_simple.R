source("./packages.R")

# 
cond_inversion <- function(pr_x_y, pr_y, pr_x_not_y, pr_not_y) {
    a = pr_x_y * pr_y
    b = pr_x_not_y * pr_not_y
    a / (a + b)
}

# weather
x_states <- matrix(c(0.6, 0.4,
                     0.3, 0.7),
                   dimnames = list(c("sunny", "rainy"), 
                                   c("sunny", "rainy")),
                   byrow = TRUE, nrow = 2)
weather <- data.frame(
    weather = c("sunny", "rainy"),
    sunny = c(.6, .3),
    rainy = c(.4, .7)
)

# activities
x_emissions <- matrix(c(0.6, 0.3, 0.1,
                        0.1, 0.4, 0.5),
                      dimnames = list(c("sunny", "rainy"),
                                      c("walk", "shop", "clean")),
                      byrow = TRUE, nrow = 2)
activities <- data.frame(
    activity = c("walk", "shop", "clean"),
    sunny = c(0.6, 0.3, 0.1),
    rainy = c(01., 0.4, 0.5)
)

# long-run state probs.
simulate_weather <- function(weather, start_state = NA, n) {
    
    iterate_state <- function(weather, state) {
        t_prob <- weather[weather$weather == state, 2:3]
        t_states <- as.character(weather$weather)
        state <- sample(t_states, 1, prob = t_prob)
        return(state)
    }
    
    if (is.na(start_state)) start_state <- weather$weather[1]
    state <- start_state
    out   <- vector(mode = "character", length = n+1)
    out[1] <- state
    for (i in seq(n)) {
        state <- iterate_state(weather, state)
        out[i+1] <- state
    }
    return(out)
}


# Q1 - does the starting state matter in the long run?
n.each = 100000
weather_sunny <- simulate_weather(weather, start_state = "sunny", n = n.each)
weather_rainy <- simulate_weather(weather, start_state = "rainy", n = n.each)

run_sims <- function(weather, start_state, n.runs, n.each) {
    
    for (i in seq(n.runs)) {
        tab <- simulate_weather(weather, start_state, n = n.each) %>%
            table()
    }

}

start_sunny <- table(weather_sunny) %>% 
    data.frame() %>%
    rename(weather = weather_sunny,
           freq = Freq) %>%
    mutate(prob = freq/n.runs)

start_rainy <- table(weather_rainy) %>% 
    data.frame() %>%
    rename(weather = weather_rainy,
           freq = Freq) %>%
    mutate(prob = freq/n.runs)
# yes, these converge over time

# Q2 - what are the starting probabilites for each state?



# rain: 57%, sun: 43%

