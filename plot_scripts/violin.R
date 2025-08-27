library("dplyr")
library("arrow")
library("ggplot2")
library("stringr")

core_order <- c(sprintf("CPU%s",seq(0,22)), "CUDA0_0")

df <- read_parquet("./../outputs/parquet/chameleon_stesting_tupi5_getrf_nopiv_1_homo/application.parquet")
df <- df[0,]

exps = list.dirs(path = "./../outputs/parquet", full.names = TRUE)
exps <- exps[ grepl("chameleon", exps) ]

for (exp in exps) {
    file = paste0(exp, "/application.parquet");
    temp_df <- read_parquet(file) |> filter (ResourceId != 'tate')
    filename <- str_replace(str_split_i(file, "/", 5), "_nopiv", "")
    filename
    setup <- unlist(str_split(str_split_i(filename, "_", 6), "\\."))[1]
    precision <- str_split_i(filename, "_", 2)
    node <- str_split_i(filename, "_", 3)
    operation <- str_split_i(filename, "_", 4)
    temp_df$setup <- setup
    temp_df$precision <- ifelse(precision == "dtesting", "double", "single")
    temp_df$operation <- operation
    temp_df$node <- node
    df <- rbind(df, temp_df)
}
df$ResourceId <- factor(df$ResourceId, level=core_order)              

df
pcores <- c(sprintf("CPU%s", seq(0,6)))
ecores <- c(sprintf("CPU%s", seq(7,22)))
nocuda <- c(sprintf("CPU%s", seq(0, 22)))

meu_estilo <- function() {
    list(
        theme_bw(base_size = 14),
        theme(
            legend.title = element_blank(),
            plot.margin = unit(c(0, 0, 0, 0), "cm"),
            legend.spacing = unit(1, "mm"),
            legend.position = "inside",
            legend.position.inside = c(0.8,0.83),
            legend.justification = "left",
            legend.box.spacing = unit(0, "pt"),
            legend.box.margin = margin(0, 0, 0, 0),
            axis.text.x = element_text(angle=90, vjust=1, hjust=1)    
        ))
}

df |>
    filter(Value == 'dgemm' & setup == 'hetero' & ResourceId %in% nocuda) |>
    ggplot(aes(x=ResourceId, y=Duration)) +
    geom_violin(width=1) +
    ylim(0, NA) +
    labs(x="CPU Workers", y="Average execution time (ms)") +
    meu_estilo() -> plot

ggsave("violin.pdf", plot, width=8, height=3.5)
