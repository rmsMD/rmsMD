library(ggplot2)
library(hexSticker)
library(rms)

set.seed(1234)
n <- 500

# Simulate predictors
age <- round(rnorm(n, mean = 50, sd = 12), 1)
bmi <- round(rnorm(n, mean = 25, sd = 4), 1)
sex <- factor(sample(c("Male", "Female"), n, replace = TRUE))
smoking <- factor(
        sample(c("Never", "Former", "Current"), n, replace = TRUE),
        levels = c("Never", "Former", "Current")
)

# S-shaped relationship for BMI
lengthstay <- 15 + 7 * (1 / (1 + exp(-(bmi - 26)/1.5))) + 0.3 * age + rnorm(n, 0, 2)

sim_data <- data.frame(age, bmi, sex, smoking, lengthstay)

# Fit an RCS model for BMI
dd <- datadist(sim_data)
options(datadist = "dd")
fit <- ols(lengthstay ~ rcs(bmi, 5), data = sim_data)

# Make the sticker plot using your ggrmsMD function (assumes it is loaded)
p <- ggrmsMD(
        modelfit = fit,
        data = sim_data,
        xlabs = list(bmi = "BMI"),
        ylab = "Length of stay",
        titles = list(bmi = NULL),
        combined = TRUE,
        shade_inferior = "none",
        noeffline = FALSE
) +
        theme_void() +
        theme_transparent()

p

# Make the hex sticker
sticker(
        subplot = p,
        package = "rmsMD",
        p_size = 20,
        s_x = 1, s_y = 0.8,
        s_width = 1.2, s_height = 1,
        h_fill = "#f4f7fa",
        h_color = "#26507B",
        p_color = "#26507B",
        filename = "man/figures/hex-rmsMD.png"
)
