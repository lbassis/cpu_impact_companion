library(ggplot2)
library(dplyr)
library(RColorBrewer)

df_xeon <- read.csv('../outputs/xeon_performance/max_error_models/models.csv', sep=";", header=TRUE) |> select(op, max_error, core_type, mean, dev) |> filter (core_type == 0)
df_xeon$cpu = 'Xeon'
df_xeon$op = factor(df_xeon$op, levels=c("sgemm", "strsm", "splgsy", "sgetrf_nopiv"))

df_i9 <- read.csv('../outputs/i9_performance/max_error_models/models.csv', sep=";", header=TRUE) |> select(op, max_error, core_type, mean, dev) |> filter (core_type == 0)
df_i9$op = factor(df_i9$op, levels=c("sgemm", "strsm", "splgsy", "sgetrf_nopiv"))
df_i9$cpu = 'i9'
df <- bind_rows(df_xeon, df_i9)

meu_estilo <- function() {
    list(
        theme_bw(base_size = 14),
        theme(
            legend.title = element_blank(), 
            plot.margin = unit(c(0, 0, 0, 0), "cm"),
            legend.spacing = unit(1, "mm"),
            legend.position = "inside",
            legend.position.inside = c(0.85,0.85),
            legend.justification = "left",
            legend.box.spacing = unit(0, "pt"),
            legend.box.margin = margin(0, 0, 0, 0),
            axis.text.x = element_text(angle=90, vjust=1, hjust=1)    
        ))
}

ggplot() +
    geom_point(data=df_xeon, size=4,
               aes(x=max_error, y=mean/1000,
                   color = cpu,
                   )) +
    geom_jitter(data=df_i9, alpha=0.3, width=1, size=3, aes(x=max_error, y=mean/1000, color=cpu)) +
    facet_wrap(~op, nrow=1) +
    ylim(0, NA) +
    labs(x="Calibration threshold (%)", y="Average execution time (ms)", color="CPU") +
    meu_estilo() + scale_colour_brewer(palette="Set1") -> plot

ggsave("max_error.pdf", width=8, height=5, plot)
