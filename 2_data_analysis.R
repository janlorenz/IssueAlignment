library(tidyverse)
library(gt)
library(arrow)
library(pals)
library(patchwork)

# Data notes: ESS Waves 1 (2002) and 5 (2010) do not have euftf, so they are not in
# ESS Wave 11 is 2023 (not 2022) because of COVID19_Pandemic

normalize_pc <- function(value) -sign(value[3])*length(value)*value/sum(abs(value))
formatpercentage <- function(x) paste0(round(100*x, digits = 1), "%")
ess <- read_parquet("ESS1-11.parquet")
attitudenames <- c("freehms", "gincdif", "lrscale", "impcntr", "euftf")
attitudedesc <- c(#"Homosexual", "Equality", "Political Right", "Immigrants", "EU")
 "Pro gay/lesbian",
 "Pro redistribution",
 "Political right",
 "Pro immigrants",
 "Pro EU")
# These are the countries which have data in all years covered
cntrynames <- c("BE","CH","DE","ES","FI","FR","GB","HU","IE","NL","NO","PL","PT","SE","SI")

essPCA <- ess |> 	filter(cntry %in% c("BE","CH","DE","ES","FI","FR","GB","HU","IE","NL","NO","PL","PT","SE","SI")) |> 
 select(all_of(attitudenames)) |> 
 prcomp(center = T, scale. = T)
ess_all <- essPCA$rotation |> as_tibble() |> 
	add_column(topic = factor(attitudenames), .before = 1) |> 
	add_column(topic_desc = factor(attitudedesc), .before = 2) |> 
	pivot_longer(starts_with("PC")) |> 
	arrange(name) |> 
	mutate(value = normalize_pc(value), .by = name) |> 
	mutate(expVar = essPCA$sdev^2/5,  .by = c(topic_desc, topic)) |>
	mutate(name = paste0(name, " (", formatpercentage(expVar), ")"))
# ESS by year
ess_y <- ess |> 
	filter(cntry %in% c("BE","CH","DE","ES","FI","FR","GB","HU","IE","NL","NO","PL","PT","SE","SI")) |> 
 nest(.by = year) |> 
	mutate(pca = data |> map(\(x) x |> select(all_of(attitudenames)) |> prcomp(center = T, scale. = T)),
								pca_varexp = pca |> map(\(x) x$sdev^2/5), 
								pca_pcs = map2(pca, pca_varexp, 
																							\(x,y) x$rotation |> as_tibble() |> 	
																								add_column(topic = factor(attitudenames), .before = 1) |> 
																								add_column(topic_desc = factor(attitudedesc), .before = 2) |> 
																								pivot_longer(starts_with("PC")) |> 
																								arrange(name) |> 
																								mutate(value = normalize_pc(value), .by = name) |> 
																								mutate(expVar = y,  .by = c(topic_desc, topic)) |>
																								mutate(name = paste0(name, " (", formatpercentage(expVar), ")")) ))
# ESS by year and country
ess_yc <- ess |> nest(.by = c(year, cntry)) |> 
	mutate(pca = data |> map(\(x) x |> select(all_of(attitudenames)) |> prcomp(center = T, scale. = T)),
								pca_varexp = pca |> map(\(x) x$sdev^2/5), 
								pca_pcs = map2(pca, pca_varexp, 
																							\(x,y) x$rotation |> as_tibble() |> 	
																								add_column(topic = factor(attitudenames), .before = 1) |> 
																								add_column(topic_desc = factor(attitudedesc), .before = 2) |> 
																								pivot_longer(starts_with("PC")) |> 
																								arrange(name) |> 
																								mutate(value = normalize_pc(value), .by = name) |> 
																								mutate(expVar = y,  .by = c(topic_desc, topic)) |>
																								mutate(name = paste0(name, " (", formatpercentage(expVar), " explained Variance)")) ))
save(ess_all, ess_y, ess_yc, file = "PCAs")


# Figure

PC_plot <- function(data, title) data |> 
 ggplot(aes(y = topic_desc |> fct_rev(), x = value, fill = value)) + 
 geom_vline(xintercept = c(-1, 1), color = "darkgray") +
 geom_col() +
 scale_fill_gradient2(low = "red", mid = "black", high="green", midpoint = 0, limits = c(-2,2), oob = scales::squish) +
 facet_wrap(~name, ncol = 5) + 
 labs(x = "",y="") +#, title = title) +
 guides(fill = "none") +
 theme_bw()

## ESS all 2002, 2023
ess_y <- ess |> 
 filter(cntry %in% c("BE","CH","DE","ES","FI","FR","GB","HU","IE","NL","NO","PL","PT","SE","SI")) |> 
 nest(.by = year) |> 
 mutate(pca = data |> map(\(x) x |> select(all_of(attitudenames)) |> prcomp(center = T, scale. = T)),
        pca_varexp = pca |> map(\(x) x$sdev^2/5), 
        pca_pcs = map2(pca, pca_varexp, 
                       \(x,y) x$rotation |> as_tibble() |> 	
                        add_column(topic = factor(attitudenames), .before = 1) |> 
                        add_column(topic_desc = factor(attitudedesc), .before = 2) |> 
                        pivot_longer(starts_with("PC")) |> 
                        arrange(name) |> 
                        mutate(value = normalize_pc(value), .by = name) |> 
                        mutate(expVar = y,  .by = c(topic_desc, topic)) |>
                        mutate(name = paste0(name, " (", formatpercentage(expVar), ")")) ))
ESS04 <- ess_y |> filter(year == 2004) |> pull(pca_pcs) |> first() |> PC_plot() + labs(tag = "2004")
ESS23 <- ess_y |> filter(year == 2023) |> pull(pca_pcs) |> first() |> PC_plot() + labs(tag = "2023") 


## Plot Time
ess_y_expVarP1 <- ess_y |> 
 mutate(`PC1 explained Variance` = pca_varexp |> map_dbl(\(x) x[1]),
        cntry = "All") |> 
 select(Year = year, cntry, `PC1 explained Variance` )
ess_y_PC = ess_y |> select(year, pca_pcs) |> unnest(cols = c(pca_pcs))
BOTTOM <- ess_yc |> select(cntry, year, pca_pcs) |> unnest(cols = c(pca_pcs)) |> 
 filter(word(name, 1) == "PC1") |> 
 mutate(PC = word(name, 1)) |> 
 select(cntry, year, PC, expVar) |> distinct() |> 
 pivot_wider(names_from = year, values_from = expVar) |> 
 na.omit() |>
 pivot_longer(`2004`:`2023`, names_to = "Year", values_to = "PC1 explained Variance") |> 
 mutate(Year = as.numeric(Year)) |> 
 ggplot(aes(Year, `PC1 explained Variance`, color = cntry)) +
 #geom_point() +
 geom_line() +
 geom_point(size = 0.5) +
 geom_line(data = ess_y_expVarP1, linewidth = 1.5, color = "black") +
 geom_point(data = ess_y_expVarP1, size = 2.5, color = "black") +
 #	facet_wrap(~PC, nrow=1) +
 scale_color_manual(values = cols25(16) |> unname()) +
 scale_x_continuous(breaks=c(2004,2006,2008,2012,2014,2016,2018,2020,2023), minor_breaks = c()) +
 scale_y_continuous(labels = scales::percent, limits = c(0.2,0.43)) +
 guides(linewidth = "none", color = guide_legend(title = "Country")) +
 theme_minimal() + labs(caption = "Black thick line = PCAs for all countries")

TOP <- ESS04 / ESS23

combine_1 =  (TOP) & plot_annotation(title = "A \t Principle components of PCA for all countries") & theme(plot.title = element_text(hjust = 0))
combine_2 =  (BOTTOM) & plot_annotation(title = "B \t Issue alignment by variance explained of PC1") & theme(plot.title = element_text(hjust = 0))

wrap_elements(combine_1) / wrap_elements(combine_2) + plot_layout(heights = c(3.5,4))
ggsave("IssueAlignment.pdf", height = 10*0.8, width = 8*0.8)
