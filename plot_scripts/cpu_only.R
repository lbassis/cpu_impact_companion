library(ggplot2)
library(dplyr)

xeon_getrf <- read.csv('../outputs/xeon_performance/getrf_cpu.csv', sep=";", header=TRUE) |> select(Function, nb, time)
xeon_potrf <- read.csv('../outputs/xeon_performance/potrf_cpu.csv', sep=";", header=TRUE) |> select(Function, nb, time)

i9_getrf <- read.csv('../outputs/i9_performance/getrf_cpu.csv', sep=";", header=TRUE) |> select(Function, threads, nb, time) |> filter(threads == 24)
i9_potrf <- read.csv('../outputs/i9_performance/potrf_cpu.csv', sep=";", header=TRUE) |> select(Function, threads, nb, time) |> filter(threads == 24)

# ignore worst case
xeon <- bind_rows(xeon_getrf |> group_by(nb) |> arrange(time) |> slice(0:(n()-1)) |> ungroup(),
                  xeon_potrf |> group_by(nb) |> arrange(time) |> slice(0:(n()-1)) |> ungroup())
xeon$cpu = 'Xeon'

i9 <- bind_rows(i9_getrf |> group_by(nb) |> arrange(time) |> slice(0:(n()-1)) |> ungroup(),
                i9_potrf |> group_by(nb) |> arrange(time) |> slice(0:(n()-1)) |> ungroup())
i9$cpu = 'i9'

df <- bind_rows(xeon, i9)


label_func <- c('Single Precision LU Factorisation', 'Single Precision Cholesky Factorisation')
names(label_func) <- c('sgetrf_nopiv', 'spotrf')

meu_estilo <- function() {
    list(
        theme_bw(base_size = 14),
        theme(
            legend.title = element_blank(),
            plot.margin = unit(c(0, 0, 0, 0), "cm"),
            legend.spacing = unit(1, "mm"),
            legend.position = "inside",
            legend.position.inside = c(0.88,0.8),
            legend.justification = "left",
            legend.box.spacing = unit(0, "pt"),
            legend.box.margin = margin(0, 0, 0, 0),
            axis.text.x = element_text(angle=90, vjust=1, hjust=1)    
        ))
}

df |>
    filter(nb > 128) |>
    group_by(Function, nb, cpu) |>
    summarize(count = n(), avg_t = mean(time), sd = sd(time)) |>
    ggplot(aes(color = cpu, x = nb, y = avg_t,
               ymin = avg_t - sd,
               ymax = avg_t + sd,
               )) +
    geom_point() +
    geom_errorbar() +
    ylim(0, NA) +
    labs(x="Block size", y="Average execution time (s)", color="Operation") +
    scale_x_continuous(breaks=unique(df$nb)) +
    meu_estilo() +
    facet_wrap(~Function, labeller=labeller(Function=label_func)) +
    scale_colour_brewer(palette="Set1") -> plot

ggsave("cpu_only.pdf", width=10, height=4, plot)
