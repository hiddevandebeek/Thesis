

# Analysis 3



# Analysis 4
## complex
models <- c("model_normal", "model_sparse")
data <- readRDS("analysis/data/analysis/5pred/covar/1normal/auc09_ev05.rds")
p50_1 <- plot_win_percentage_by_sample_size(data, models, "model_normal") + theme(legend.position = "none")
p50_auc_1 <- plot_average_auc_by_model_and_sample_size(data, models, "model_normal", min = 0.87, max = 0.90)  + theme(legend.position = "none")

models <- c("model_interaction", "model_sparse")
data <- readRDS("analysis/data/analysis/5pred/covar/1interaction/auc09_ev05.rds")
p50_2 <- plot_win_percentage_by_sample_size(data, models, "model_interaction")
p50_auc_2 <- plot_average_auc_by_model_and_sample_size(data, models, "model_interaction", min = 0.87, max = 0.90)
ggsave("plots_4/normal_1.png", p50_1 + p50_2, width = 16.1, height = 6.5, dpi = 200)
ggsave("plots_4/normal_auc_1.png", p50_auc_1 + p50_auc_2, width = 16.1, height = 6.5, dpi = 200)

models <- c("model_normal", "model_minimal", "model_sparse2", "model_sparse")
data <- readRDS("analysis/data/analysis/5pred/covar/2normal/auc09_ev05.rds") 
p50_1 <- plot_win_percentage_by_sample_size(data, models, "model_normal") + theme(legend.position = "none")
p50_auc_1 <- plot_average_auc_by_model_and_sample_size(data, models, "model_normal", min = 0.87, max = 0.90)  + theme(legend.position = "none")

models <- c("model_interaction", "model_minimal", "model_sparse", "model_interactionsparse1")
data <- readRDS("analysis/data/analysis/5pred/covar/2normalinteraction/auc09_ev05.rds")
p50_2 <- plot_win_percentage_by_sample_size(data, models, "model_interaction")  + theme(legend.position = "none")
p50_auc_2 <- plot_average_auc_by_model_and_sample_size(data, models, "model_interaction", min = 0.87, max = 0.90)  + theme(legend.position = "none")

models <- c("model_2interaction", "model_minimal", "model_interactionsparse1", "model_interactionsparse2")
data <- readRDS("analysis/data/analysis/5pred/covar/2interaction/auc09_ev05.rds")
p50_3 <- plot_win_percentage_by_sample_size(data, models, "model_2interaction")
p50_auc_3 <- plot_average_auc_by_model_and_sample_size(data, models, "model_2interaction", min = 0.87, max = 0.90)
ggsave("plots_4/normal_2.png", p50_1 + p50_2 + p50_3, width = 22.8, height = 6.5, dpi = 200)
ggsave("plots_4/normal_auc_2.png", p50_auc_1 + p50_auc_2 + p50_auc_3, width = 22.8, height = 6.5, dpi = 200)


