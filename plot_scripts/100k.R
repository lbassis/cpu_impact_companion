library(ggplot2)
library(dplyr)
library(RColorBrewer)

library(ggplot2)
library(dplyr)

#### XEON ####
# 100k single
df_getrf <- read.csv('../outputs/xeon_performance/getrf_100k.csv', sep=";", header=TRUE) |> select(Function, n, threads, gpus, nb, time) |> group_by(nb) |> arrange(time) |> slice(0:(n()-1)) |> ungroup()
df_potrf <- read.csv('../outputs/xeon_performance/potrf_100k.csv', sep=";", header=TRUE) |> select(Function, n, threads, gpus, nb, time) |> group_by(nb) |> arrange(time) |> slice(0:(n()-1)) |> ungroup()
xeon_100k_single <- bind_rows(df_getrf, df_potrf)
xeon_100k_single$precision = 'Single'

# 100k double
df_getrf <- read.csv('../outputs/xeon_performance/getrf_100k_double.csv', sep=";", header=TRUE) |> select(Function, n, threads, gpus, nb, time) |> group_by(nb) |> arrange(time) |> slice(0:(n()-1)) |> ungroup()
df_potrf <- read.csv('../outputs/xeon_performance/potrf_100k_double.csv', sep=";", header=TRUE) |> select(Function, n, threads, gpus, nb, time) |> group_by(nb) |> arrange(time) |> slice(0:(n()-1)) |> ungroup()
xeon_100k_double <- bind_rows(df_getrf, df_potrf)
xeon_100k_double$precision = 'Double'

xeon_df <- bind_rows(xeon_100k_single, xeon_100k_double) |> filter(threads == 7)

#### i9 ####
# 100k single
df_getrf <- read.csv('../outputs/i9_performance/getrf_100k.csv', sep=";", header=TRUE) |> select(Function, n, threads, gpus, nb, time) |> group_by(nb) |> arrange(time) |> slice(0:(n()-1)) |> ungroup()
df_potrf <- read.csv('../outputs/i9_performance/potrf_100k.csv', sep=";", header=TRUE) |> select(Function, n, threads, gpus, nb, time) |> group_by(nb) |> arrange(time) |> slice(0:(n()-1)) |> ungroup()
i9_100k_single <- bind_rows(df_getrf, df_potrf)
i9_100k_single$precision = 'Single'

# 100k double
df_getrf <- read.csv('../outputs/i9_performance/getrf_100k_double.csv', sep=";", header=TRUE) |> select(Function, n, threads, gpus, nb, time) |> group_by(nb) |> arrange(time) |> slice(0:(n()-1)) |> ungroup()
df_potrf <- read.csv('../outputs/i9_performance/potrf_100k_double.csv', sep=";", header=TRUE) |> select(Function, n, threads, gpus, nb, time) |> group_by(nb) |> arrange(time) |> slice(0:(n()-1)) |> ungroup()
i9_100k_double <- bind_rows(df_getrf, df_potrf)
i9_100k_double$precision = 'Double'

i9_df <- bind_rows(i9_100k_single, i9_100k_double) |> filter(threads==23)

xeon_df$CPU <- 'Xeon'
i9_df$CPU <- 'i9'
df <- bind_rows(xeon_df, i9_df)
df$Function = ifelse((df$Function == 'dgetrf_nopiv' | df$Function == 'sgetrf_nopiv'), 'getrf_nopiv', 'potrf')

meu_estilo <- function() {
    list(
        theme_bw(base_size = 14),
        theme(
            legend.title = element_blank(),
            plot.margin = unit(c(0, 0, 0, 0), "cm"),
            legend.spacing = unit(1, "mm"),
            legend.position = "inside",
            legend.position.inside = c(0.85,0.6),
            legend.justification = "left",
            legend.box.spacing = unit(0, "pt"),
            legend.box.margin = margin(0, 0, 0, 0),
            axis.text.x = element_text(angle=90, vjust=1, hjust=1)    
        ))
}

df_100k <- df |> filter(n == 100000) |> group_by(nb) |> arrange(time) |> slice(0:(n()-1)) |> ungroup()

label_func <- c('LU Factorization', 'Cholesky Factorization')
names(label_func) <- c('getrf_nopiv', 'potrf')

df_100k$precision = factor(df_100k$precision, levels=c('Single', 'Double'))
df_100k |>
    group_by(Function, precision, threads, nb, CPU) |>
    summarize(count = n(), avg_t = mean(time), sd = sd(time), se = 3*sd/sqrt(count)) |>
    ggplot(aes(x = nb, y = avg_t, color = factor(CPU),
               ymin = avg_t - se,
               ymax = avg_t + se,
               )) +
    geom_point(size=2) +
    geom_errorbar(width=40) +
    ylim(0, NA) +
    labs(x="Block size", y="Average execution time (s)", color="Application") +
    scale_x_continuous(breaks=unique(df$nb)) +
    scale_colour_brewer(palette="Set1") +
    facet_grid(Function~precision, scales='free_y', labeller=labeller(Function=label_func)) +
    meu_estilo() -> plot
plot
ggsave('100k.pdf', width=8, height=5, plot)
