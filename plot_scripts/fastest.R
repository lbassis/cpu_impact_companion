library(ggplot2)
library(dplyr)
library(RColorBrewer)

#### XEON ####
                                        # só cpu single
df_getrf <- read.csv('../outputs/xeon_performance/getrf_cpu.csv', sep=";", header=TRUE) |> select(Function, n, threads, gpus, nb, time) |> group_by(nb) |> arrange(time) |> slice(0:(n()-1)) |> ungroup()
df_potrf <- read.csv('../outputs/xeon_performance/potrf_cpu.csv', sep=";", header=TRUE) |> select(Function, n, threads, gpus, nb, time) |> group_by(nb) |> arrange(time) |> slice(0:(n()-1)) |> ungroup()
xeon_cpu_single <- bind_rows(df_getrf, df_potrf)
xeon_cpu_single$precision <- 'Single'

                                        # só cpu double
df_getrf <- read.csv('../outputs/xeon_performance/getrf_cpu_double.csv', sep=";", header=TRUE) |> select(Function, n, threads, gpus, nb, time) |> group_by(nb) |> arrange(time) |> slice(0:(n()-1)) |> ungroup()
df_potrf <- read.csv('../outputs/xeon_performance/potrf_cpu_double.csv', sep=";", header=TRUE) |> select(Function, n, threads, gpus, nb, time) |> group_by(nb) |> arrange(time) |> slice(0:(n()-1)) |> ungroup()
xeon_cpu_double <- bind_rows(df_getrf, df_potrf)
xeon_cpu_double$precision <- 'Double'

                                        # gpu single
df_getrf <- read.csv('../outputs/xeon_performance/getrf_gpu.csv', sep=";", header=TRUE) |> select(Function, n, threads, gpus, nb, time) |> group_by(nb) |> arrange(time) |> slice(0:(n()-1)) |> ungroup()
df_potrf <- read.csv('../outputs/xeon_performance/potrf_gpu.csv', sep=";", header=TRUE) |> select(Function, n, threads, gpus, nb, time) |> group_by(nb) |> arrange(time) |> slice(0:(n()-1)) |> ungroup()
xeon_gpu_single <- bind_rows(df_getrf, df_potrf)
xeon_gpu_single$precision = 'Single'

                                        # gpu double
df_getrf <- read.csv('../outputs/xeon_performance/getrf_gpu_double.csv', sep=";", header=TRUE) |> select(Function, n, threads, gpus, nb, time) |> group_by(nb) |> arrange(time) |> slice(0:(n()-1)) |> ungroup()
df_potrf <- read.csv('../outputs/xeon_performance/potrf_gpu_double.csv', sep=";", header=TRUE) |> select(Function, n, threads, gpus, nb, time) |> group_by(nb) |> arrange(time) |> slice(0:(n()-1)) |> ungroup()
xeon_gpu_double <- bind_rows(df_getrf, df_potrf)
xeon_gpu_double$precision = 'Double'

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


xeon_df <- bind_rows(xeon_cpu_single, xeon_cpu_double, xeon_gpu_single, xeon_gpu_double, xeon_100k_single, xeon_100k_double) |> filter(threads == 7)

#### i9 ####
                                        # só cpu single
df_getrf <- read.csv('../outputs/i9_performance/getrf_cpu.csv', sep=";", header=TRUE) |> select(Function, n, threads, gpus, nb, time) |> group_by(nb) |> arrange(time) |> slice(0:(n()-1)) |> ungroup()
df_potrf <- read.csv('../outputs/i9_performance/potrf_cpu.csv', sep=";", header=TRUE) |> select(Function, n, threads, gpus, nb, time) |> group_by(nb) |> arrange(time) |> slice(0:(n()-1)) |> ungroup()
i9_cpu_single <- bind_rows(df_getrf, df_potrf)
i9_cpu_single$precision <- 'Single'

                                        # só cpu double
df_getrf <- read.csv('../outputs/i9_performance/getrf_cpu_double.csv', sep=";", header=TRUE) |> select(Function, n, threads, gpus, nb, time) |> group_by(nb) |> arrange(time) |> slice(0:(n()-1)) |> ungroup()
df_potrf <- read.csv('../outputs/i9_performance/potrf_cpu_double.csv', sep=";", header=TRUE) |> select(Function, n, threads, gpus, nb, time) |> group_by(nb) |> arrange(time) |> slice(0:(n()-1)) |> ungroup()
i9_cpu_double <- bind_rows(df_getrf, df_potrf)
i9_cpu_double$precision <- 'Double'

                                        # gpu single
df_getrf <- read.csv('../outputs/i9_performance/getrf_gpu.csv', sep=";", header=TRUE) |> select(Function, n, threads, gpus, nb, time) |> group_by(nb) |> arrange(time) |> slice(0:(n()-1)) |> ungroup()
df_potrf <- read.csv('../outputs/i9_performance/potrf_gpu.csv', sep=";", header=TRUE) |> select(Function, n, threads, gpus, nb, time) |> group_by(nb) |> arrange(time) |> slice(0:(n()-1)) |> ungroup()
i9_gpu_single <- bind_rows(df_getrf, df_potrf)
i9_gpu_single$precision = 'Single'

                                        # gpu double
df_getrf <- read.csv('../outputs/i9_performance/getrf_gpu_double.csv', sep=";", header=TRUE) |> select(Function, n, threads, gpus, nb, time) |> group_by(nb) |> arrange(time) |> slice(0:(n()-1)) |> ungroup()
df_potrf <- read.csv('../outputs/i9_performance/potrf_gpu_double.csv', sep=";", header=TRUE) |> select(Function, n, threads, gpus, nb, time) |> group_by(nb) |> arrange(time) |> slice(0:(n()-1)) |> ungroup()
i9_gpu_double <- bind_rows(df_getrf, df_potrf)
i9_gpu_double$precision = 'Double'

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

i9_df <- bind_rows(i9_cpu_single, i9_cpu_double, i9_gpu_single, i9_gpu_double, i9_100k_single, i9_100k_double) |> filter(threads==23)

xeon_df$CPU <- 'Xeon'
i9_df$CPU <- 'i9'
df <- bind_rows(xeon_df, i9_df)
df$Function = ifelse((df$Function == 'dgetrf_nopiv' | df$Function == 'sgetrf_nopiv'), 'getrf_nopiv', 'potrf')

label_func <- c('LU Factorization', 'Cholesky Factorization')
names(label_func) <- c('getrf_nopiv', 'potrf')

label_n <- c('n = 32k', 'n = 100k')
names(label_n) <- c(32768, '100000')

df |>
  group_by(Function, threads, gpus, nb, precision, CPU) |>
  summarise(best_time = min(time)) -> min_times

min_sgetrf = min_times |> filter(Function == 'getrf_nopiv' & precision == 'Single')
min_sgetrf[which.min(min_sgetrf$best_time),]

min_dgetrf = min_times |> filter(Function == 'getrf_nopiv' & precision == 'Double')
min_dgetrf[which.min(min_dgetrf$best_time),]

min_spotrf = min_times |> filter(Function == 'potrf' & precision == 'Single')
min_spotrf[which.min(min_spotrf$best_time),]

min_dpotrf = min_times |> filter(Function == 'potrf' & precision == 'Double')
min_dpotrf[which.min(min_dpotrf$best_time),]

    meu_estilo <- function() {
     list(
       theme_bw(base_size = 14),
       theme(
         legend.title = element_blank(),
         plot.margin = unit(c(0, 0, 0, 0), "cm"),
         legend.spacing = unit(1, "mm"),
         legend.position = "inside",
         legend.position.inside = c(0.55,0.83),
         legend.justification = "left",
         legend.box.spacing = unit(0, "pt"),
         legend.box.margin = margin(0, 0, 0, 0),
         axis.text.x = element_text(vjust=1, hjust=1)    
       ))
    }

df |>
  filter((nb == 1024 & precision == 'Double') | (nb == 2048 & precision == 'Single') | (nb == 1500 & precision == 'Single' & n == 100000)) |>
  group_by(Function, n, threads, gpus, nb, precision, CPU) |>
  summarise(best = min(time)) |>
  ggplot(aes(x=factor(precision, levels=c('Single', 'Double')), y=best, fill=CPU)) +
  geom_bar(stat='identity', width=0.5, position='dodge') +
  facet_grid(n~Function, scales="free_y", labeller=labeller(Function=label_func, n=label_n)) +
  labs(x="Precision", y="Fastest execution time (s)", color="CPU") +
  meu_estilo() +  scale_fill_brewer(palette="Set1") -> plot

ggsave('fastest.pdf', height=3, width=7, plot)
