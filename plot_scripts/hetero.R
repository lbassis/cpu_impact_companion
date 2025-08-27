library(ggplot2)
library(dplyr)

df_getrf <- read.csv('../outputs/i9_performance/comparing_homo_hetero_getrf.csv', sep=";", header=TRUE) |> select(Function, nb, time, mode)
df_potrf <- read.csv('../outputs/i9_performance/comparing_homo_hetero_potrf.csv', sep=";", header=TRUE) |> select(Function, nb, time, mode)

df <- bind_rows(df_getrf, df_potrf)

label_func <- c('Double Precision LU', 'Double Precision Cholesky')
names(label_func) <- c('dgetrf_nopiv', 'dpotrf')

df$mode = ifelse(df$mode == 'hetero', 'Heterogeneous models', 'Homogeneous models')

meu_estilo <- function() {
    list(
        theme_bw(base_size = 14),
        theme(
            legend.title = element_blank(),
            plot.margin = unit(c(0, 0, 0, 0), "cm"),
            legend.spacing = unit(1, "mm"),
            legend.position = "inside",
            legend.position.inside = c(0.55,0.8),
            legend.justification = "left",
            legend.box.spacing = unit(0, "pt"),
            legend.box.margin = margin(0, 0, 0, 0),
            axis.text.x = element_text(angle=90, vjust=1, hjust=1)    
        ))
}

df |>
    filter(Function == 'dpotrf' | Function == 'dgetrf_nopiv') |>
    group_by(Function, nb, mode) |>
    summarize(count = n(), avg_t = mean(time), sd = sd(time)) |>
    ggplot(aes(fill = mode, x = factor(nb), y = avg_t,
               ymin = avg_t - sd,
               ymax = avg_t + sd,
               )) +
    geom_col(position=position_dodge(width=.9)) +
    geom_errorbar(position=position_dodge(width=.9), width=0.35) +
    ylim(0, NA) +
    labs(x="Block size", y="Average execution time (s)", color="Performance models") +
    facet_wrap(~Function, labeller=labeller(Function=label_func)) +
    meu_estilo() +
    scale_fill_brewer(palette="Set2") -> plot

ggsave('hetero.pdf', width=8, height=4, plot)
