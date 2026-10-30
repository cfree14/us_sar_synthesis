library(ggplot2)

# Parameters
K <- 1000       # Carrying capacity
r <- 0.06      # Intrinsic growth rate per year
N0 <- 2        # Initial population
mnpl_prop <- 0.75

# Solve for shape parameter that places MNPL at 75% of K
theta <- uniroot(
  function(theta) (1 + theta)^(-1 / theta) - mnpl_prop,
  interval = c(0.01, 100)
)$root

theta
MNPL <- K * mnpl_prop

# Population growth: exact continuous-time theta-logistic solution
growth <- data.frame(year = seq(0, 300, by = 0.1))

growth$N <- K / (
  1 + ((K / N0)^theta - 1) * exp(-r * theta * growth$year)
)^(1 / theta)

# Surplus production across population sizes
production <- data.frame(N = seq(2, K, by = 1))

production$surplus <- with(
  production,
  r * N * (1 - (N / K)^theta)
)

# Maximum surplus production
Pmax <- r * MNPL * (1 - (MNPL / K)^theta)


# Plot data
################################################################################

# Theme
my_theme <-  theme(# Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot 1: population growth from 2 whales toward carrying capacity
g1 <- ggplot(growth, aes(x = year, y = N)) +
  geom_line(linewidth = 1, color = "steelblue") +
  geom_hline(yintercept = K, linetype = "dashed", color = "grey50") +
  geom_hline(yintercept = MNPL, linetype = "dotted", color = "grey50") +
  scale_y_continuous(breaks = c(2, 250, 500, 750, 1000)) +
  labs(
    x = "Year",
    y = "Population size",
    title = "Population growth"
  ) +
  theme_bw() + my_theme 

# Plot 2: surplus production curve
g2 <- ggplot(production, aes(x = N, y = surplus)) +
  geom_line(linewidth = 1, color = "steelblue") +
  geom_vline(xintercept = MNPL, linetype = "dotted", color = "grey50") +
  annotate("point", x = MNPL, y = Pmax, size = 3) +
  scale_x_continuous(breaks = c(2, 250, 500, 750, 1000)) +
  labs(
    x = "Population size",
    y = "Surplus production",
    title = "Surplus production"
  ) +
  theme_bw() + my_theme 


gridExtra::grid.arrange(g1, g2, nrow=1)
