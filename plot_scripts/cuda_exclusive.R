library(ggplot2)
library(dplyr)

xeon_getrf <- read.csv('../outputs/xeon_performance/getrf_gpu.csv', sep=";", header=TRUE) |> select(Function, threads, nb, time)
xeon_potrf <- read.csv('../outputs/xeon_performance/potrf_gpu.csv', sep=";", header=TRUE) |> select(Function, threads, nb, time)

i9_getrf <- read.csv('../outputs/i9_performance/getrf_gpu.csv', sep=";", header=TRUE) |> select(Function, threads, nb, cudacore, time)
i9_potrf <- read.csv('../outputs/i9_performance/potrf_gpu.csv', sep=";", header=TRUE) |> select(Function, threads, nb, cudacore, time)

xeon <- bind_rows(xeon_getrf |> group_by(nb) |> arrange(time) |> slice(0:(n()-1)) |> ungroup(),
                  xeon_potrf |> group_by(nb) |> arrange(time) |> slice(0:(n()-1)) |> ungroup())
xeon$cpu = 'Xeon'

i9 <- bind_rows(i9_getrf |> group_by(nb) |> arrange(time) |> slice(0:(n()-1)) |> ungroup(),
                i9_potrf |> group_by(nb) |> arrange(time) |> slice(0:(n()-1)) |> ungroup())
i9$cpu = 'i9'

df <- bind_rows(xeon, i9)		    
label_func <- c('Single Precision LU', 'Single Precision Cholesky')
names(label_func) <- c('sgetrf_nopiv', 'spotrf')

df$cuda <- ifelse(df$threads == 7 | df$threads == 23, "CUDA exclusive thread", "No CUDA exclusive thread")

meu_estilo <- function() {
    list(
        theme_bw(base_size = 14),
        theme(
            legend.title = element_blank(),
            plot.margin = unit(c(0, 0, 0, 0), "cm"),
            legend.spacing = unit(1, "mm"),
            legend.position = "inside",
            legend.position.inside = c(0.55,0.81),
            legend.justification = "left",
            legend.box.spacing = unit(0, "pt"),
            legend.box.margin = margin(0, 0, 0, 0),
            axis.text.x = element_text(angle=90, vjust=1, hjust=1)    
        ))
}

df |>
    filter(nb > 128 & cpu == 'Xeon' & Function == 'sgetrf_nopiv') |>
    group_by(Function, threads, nb, cuda, cpu) |>
    summarize(count = n(), avg_t = mean(time), sd = sd(time), se = 3*sd/sqrt(count)) |>
    ggplot(aes(x = nb, y = avg_t, color = factor(cuda),
               ymin = avg_t - se,
               ymax = avg_t + se,
               )) +
    geom_point(size=2) +
    geom_errorbar(width=20) +
    ylim(0, NA) +
    labs(x="Block size", y="Average execution time (s)", color="Application") +
    scale_x_continuous(breaks=unique(df$nb)) +
    scale_colour_brewer(palette="Dark2") +
    meu_estilo() -> plot


ggsave('cuda_exclusive.pdf', width=6, height=4, plot)
