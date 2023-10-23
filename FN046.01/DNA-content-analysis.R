## --- READ ME --- ##

# copy the file name of the Image Analyst output file and paste it below
Image_Analyst_output_file_name <- "IAoutput_23-0823_noE05"

# copy the file name of the Image Analyst output file and paste it below
plate_template_name <- "plate-template_noE05"

# control and senescent labels
CTL_label <- "CTL"
SEN_label <- "IR"

# control and senescent colors
CTL_color <- "#1B9E77"
SEN_color <- "#7974BA"

# indicate was morphological parameter was measured
morphological_parameter <- "Nuclear_Area"

# optional: assess cell viability and senescence marker changes after a treatment?
assess_cell_viability_and_senescence_markers_changes <- TRUE
cell_viability_variable <- "Serum" # must be perfect match to variable name entered in plate template


# Indicate label #/number for DAPI, EdU, and SA-B-Gal;
## Note: the default label #s below should be correct if you followed the image analysis protocol correctly
DAPI_label_number <- 1
EdU_label_number <- 2
SABGal_label_number <- 3

# Indicate desired quantile values (0-1) for threshold calculation
SABGal_threshold_quantile <- 0.95
EdU_threshold_quantile <- 0.99

# character sizes
size_legend_title <- 20
size_legend_text <- 18
size_axis_title <- 20
size_axis_text <- 18
size_facets_text <- 20

# resolution
resolution_dpi <- 150

## --- READ ME --- ##

# libraries 

library(tidyverse)
library(beepr)
library(readxl)


library(plater)
# library(stringi)
library(beepr)
library(openxlsx)
library(knitr)
library(scales)

## ggplot themes

old_theme <- theme_set(theme_bw())
theme_update(
  legend.title = element_text(size=size_legend_title), #change legend title font size
  legend.text = element_text(size=size_legend_text), #change legend text font size
  axis.title = element_text(size=size_axis_title),
  axis.text = element_text(size=size_axis_text),
  strip.text = element_text(size=size_facets_text)
)


# Define functions folder path and load_function  ----------------------------------

## Enter folder path as it appears in Windows Eplorer
functions_folder_path <- "C:/Users/ffran/AppData/Local/R/functions"

## generate function to load functions

load_function <- function(function_file_name, folder_path = functions_folder_path) {
  
  function_file_name <- if (str_detect(function_file_name, ".R")) {
    function_file_name
  } else { 
    str_c(function_file_name, ".R")}
  
  function_path <- str_c(functions_folder_path, "/", function_file_name)
  
  source(function_path)
  
}

# Tidying IA output: START --------------------------------------------------

load_function("tidy_IAoutput")

tidied_IAoutput <- tidy_IAoutput("IAoutput_Mock", subdirectory_name = "FN047.01")

# Tidying IA output: END --------------------------------------------------

## creating graph folder

graphs_folder <- str_c("Analysis_", Image_Analyst_output_file_name,"_", Sys.time()) %>%
  str_replace_all(pattern = " ", replacement = "_") %>%
  str_replace_all(pattern = ":", replacement = "") %>%
  str_replace_all(pattern = ".xlsx", replacement = "")

dir.create(graphs_folder)


df <- tidy_data6

treatment_variable <- cell_viability_variable %>%
  tolower() %>%
  str_replace_all(., "[()]", "")

# renaming variable corresponding to the treatment variable entered in plate-template
new_col_names <- colnames(df) %>%
  str_replace(., tolower(treatment_variable), "treatment_variable")

colnames(df) <- new_col_names

# changing treatment variable into a factor

df$treatment_variable <- as.factor(df$treatment_variable)

df <- df %>%
  mutate(treatment_variable = fct_relevel(treatment_variable, "FS", "SS_5.0", "SS_0.5", "SS_0.0"))


# graphs settings ---------------------------------------------------------

## ggplot themes

old_theme <- theme_set(theme_bw())
theme_update(
  legend.title = element_text(size=12), #change legend title font size
  legend.text = element_text(size=10) #change legend text font size
)

## colors

palette1_named <- setNames(object = scales::hue_pal()(length((unique(df$condition)))),
                           nm = (sort(unique(df$condition)))
)

## condition colors
color_scale_conditions <- scale_color_brewer(palette = "Set1", limits = unique(df$condition))

## png function

default_png <- function(filename = file_path, width, height) {
  ggsave(filename = filename, device = "png", width = width * (resolution_dpi/72), height = height * (resolution_dpi/72), units = "px", dpi = resolution_dpi)
}

# graphs ------------------------------------------------------------------

# checking if df contains variable serum and modifies facet_grid accordingly

serum_check <- grepl("serum", colnames(df), ignore.case = TRUE) %>% any() # if TRUE, will facet_grid plots based on serum

# filtering out background wells
df_signal <- df

color_scale_conditions_signal <- scale_color_brewer(palette = "Set1", limits = unique(df_signal$condition))

fill_scale_conditions_signal <- scale_fill_brewer(palette = "Set1", limits = unique(df_signal$condition))

# grouping data by well

grouped_by_well <- if (serum_check == TRUE) {
  group_by(df_signal, well, condition, serum, treatment_variable)
} else {
  group_by(df_signal, well, condition, treatment_variable)
}

df_summary <- summarise(grouped_by_well,
                        tot_count = n()
)

df_average <- summarise(group_by(df_summary, condition, treatment_variable),
                        avg_count = mean(tot_count))

df_summary <- left_join(df_summary, df_average)

# histogram DNA content SENESCENT untreated

df_signal_SEN_only <- df_signal %>%
  filter(., condition == SEN_label)

DAPI_threshold_SEN <- 14000000

file_path <- str_c(getwd(),"/",graphs_folder,"/DNA-content-histogram_SEN.png", sep = "") # gets string with full path and file name for plot

ggplot(df_signal_SEN_only,
       aes(x = DAPI,
           fill = condition)) +
  geom_vline(xintercept = DAPI_threshold_SEN) +
  geom_histogram(binwidth = quantile(df_signal_SEN_only$DAPI, 0.99)/100,
                 fill = SEN_color) +
  facet_grid(seeding_density ~ treatment_variable) +
  labs(y = "Counts",
       x = "DNA Content (DAPI staining, A.U.)",
       fill = "Condition") +
  scale_x_continuous(limits = c(0,quantile(df_signal_SEN_only$DAPI, 0.99)))

default_png(width = 900, height = 900)

# histogram DNA content CONTROL untreated

df_signal_CTL_only <- df_signal %>%
  filter(., condition == CTL_label)

DAPI_threshold_CTL <- 16000000

file_path <- str_c(getwd(),"/",graphs_folder,"/DNA-content-histogram_CTL.png", sep = "") # gets string with full path and file name for plot

ggplot(df_signal_CTL_only,
       aes(x = DAPI,
           fill = condition)) +
  geom_vline(xintercept = DAPI_threshold_CTL) +
  geom_histogram(binwidth = quantile(df_signal_CTL_only$DAPI, 0.99)/100,
                 fill = CTL_color) +
  facet_grid(seeding_density ~ treatment_variable) +
  labs(y = "Counts",
       x = "DNA Content (DAPI staining, A.U.)",
       fill = "Condition") +
  scale_x_continuous(limits = c(0,quantile(df_signal_CTL_only$DAPI, 0.99)))

default_png(width = 900, height = 900)

# scatterplot Area vs DNA content

ggplot(df_signal,
       aes(x = DAPI,
           y = Nuclear_Area,
           color = condition)) +
  geom_vline(xintercept = DAPI_threshold_CTL) +
  geom_point(alpha = 1/3) +
  facet_grid(condition ~ treatment_variable) +
  color_scale_conditions_signal +
  labs(y = str_replace_all(morphological_parameter, "_", " "),
       x = "DNA Content (DAPI staining, A.U.)",
       color = "Condition") +
  scale_x_continuous(limits = c(0,quantile(df_signal_CTL_only$DAPI, 0.99))) +
  guides(color = guide_legend(override.aes = list(size = 3, alpha = 1)))

# dfs for stacked bar plots

df_signal <- df_signal %>%
  mutate(DAPI_threshold = if_else(condition == CTL_label, DAPI_threshold_CTL, DAPI_threshold_SEN))

df_summary_DAPI <- summarise(group_by(df_signal, condition, treatment_variable, seeding_density, well, DAPI_threshold),
                             tot_count = n(),
                             G1_count = sum(DAPI <= DAPI_threshold),
                             G2_count = sum(DAPI > DAPI_threshold),
                             G1_percent = G1_count / tot_count,
                             G2_percent = G2_count / tot_count)

df_summary_DAPI_SEN_only <- df_summary_DAPI %>%
  filter(., condition == SEN_label)

df_summary_DAPI_SEN_only_stacked_barplot <- df_summary_DAPI_SEN_only %>%
  select(!("tot_count":"G2_count")) %>%
  rename(., Low = G1_percent, High = G2_percent) %>%
  pivot_longer(c("Low","High"), names_to = "DNA_content", values_to = "Proportion")

df_summary_DAPI_SEN_only_stacked_barplot_Low <- df_summary_DAPI_SEN_only_stacked_barplot %>%
  filter(DNA_content == "Low")

df_summary_DAPI_CTL_only <- df_summary_DAPI %>%
  filter(., condition == CTL_label)

df_summary_DAPI_CTL_only_stacked_barplot <- df_summary_DAPI_CTL_only %>%
  select(!("tot_count":"G2_count")) %>%
  rename(., Low = G1_percent, High = G2_percent) %>%
  pivot_longer(c("Low","High"), names_to = "DNA_content", values_to = "Proportion")

df_summary_DAPI_CTL_only_stacked_barplot_Low <- df_summary_DAPI_CTL_only_stacked_barplot %>%
  filter(DNA_content == "Low")


# stacked bar plot function

plot_G1_G2_percent_stacked <- function(df, df_low, color) {
  ggplot(data = df_low,
         aes(x = treatment_variable,
             y = Proportion)) +
    geom_bar(data = df,
             aes(x = treatment_variable,
                 y = Proportion,
                 fill = DNA_content),
             position = "fill",
             stat = "identity"
             ) +
    geom_point(fill = color,
               color = "black",
               size = 3,
               shape = 21) +
    facet_grid(seeding_density ~ condition) +
    labs(y = "% of Cells",
         x = treatment_variable) +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0,1),
                       breaks = seq(0,1,length.out = 11)) +
    scale_fill_manual(values = c("darkgrey","lightgrey")) +
    guides(fill = guide_legend(title = "DNA content"))
}

# plot % of SENESCENT cells with high vs low DNA content 

file_path <- str_c(getwd(),"/",graphs_folder,"/percentages-high-low-DNA-content_Sen.png", sep = "") # gets string with full path and file name for plot

plot_G1_G2_percent_stacked(df_summary_DAPI_SEN_only_stacked_barplot, df_summary_DAPI_SEN_only_stacked_barplot_Low, SEN_color)

default_png(width = 300, height = 600)

# plot % of CONTROL cells with high vs low DNA content

file_path <- str_c(getwd(),"/",graphs_folder,"/percentages-high-low-DNA-content_Ctl.png", sep = "") # gets string with full path and file name for plot

plot_G1_G2_percent_stacked(df_summary_DAPI_CTL_only_stacked_barplot, df_summary_DAPI_CTL_only_stacked_barplot_Low, CTL_color)

default_png(width = 300, height = 600)

# Notes -------------------------------------------------------------------

