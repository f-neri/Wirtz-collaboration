## --- READ ME --- ##

# copy the file name of the Image Analyst output file and paste it below
Image_Analyst_output_file_name <- "IA-output_Plate2_230803"

# copy the file name of the Image Analyst output file and paste it below
plate_template_name <- "plate-template_2"

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
library(readxl)
library(plater)
library(stringi)
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
# Tidying IA output: START --------------------------------------------------

# check if file name exists

Image_Analyst_output_file_name <- if (str_detect(Image_Analyst_output_file_name, ".xlsx")) {Image_Analyst_output_file_name
} else {str_c(Image_Analyst_output_file_name, ".xlsx")}

if (file.exists(Image_Analyst_output_file_name)) {
  str_c(Image_Analyst_output_file_name, " exists within the working directory")
} else {
  beep(1)
  Sys.sleep(2)
  stop(
    str_c(Image_Analyst_output_file_name, " does not exist within the working directory.

Ensure you copy-pasted the Image Analyst ouput excel (.xlsx) file into the working directory
and that such file name was entered correctly into the \"IMPORTANT: READ ME section\" "))
}

# import IA-output file
IA_output_unadjusted <- read_xlsx(Image_Analyst_output_file_name, skip = 1, na = "NA")

# adjust new IA output to match older IA output (removing extra columns, changing "Plot Name" to "Name")

cols_to_remove <- c("Folder Name", "Base Name", "Position Name", "Frame")

cols_to_keep <- colnames(IA_output_unadjusted)[!(colnames(IA_output_unadjusted) %in% cols_to_remove)]

IA_output <- IA_output_unadjusted[,cols_to_keep]

if (any(colnames(IA_output) %in% "Plot Name")) {
  colnames(IA_output)[grep("Plot Name", colnames(IA_output))] <- "Name"
}

# change column names so they're all "OBJ#"

col_names <- colnames(IA_output)

adjusted_col_names <- col_names[-(1:2)] %>%
  grep(pattern = "[.][.][.]", value = TRUE) %>%
  str_sub(start = 4) %>%
  as.numeric() %>%
  -2 %>%
  str_c("OBJ", .)

final_col_names <- col_names %>%
  grep(pattern = "[.][.][.]", invert = TRUE, value = TRUE) %>%
  c(., adjusted_col_names)

colnames(IA_output) <- final_col_names

# tidy the dataset

## pivot_longer()

### creating character vector with all OBJ# present in the data frame header

OBJ_vec <- colnames(IA_output)[-(1:2)]

### pivoting
tidy_data1 <-  pivot_longer(IA_output, all_of(OBJ_vec),
               names_to = "cell_ID",
               values_to = "Signal_Intensity")

## splitting "Name" Column into well # and the parameter measured
tidy_data2 <- mutate(tidy_data1,
                     well = sub(".+ - ([0-9]+ )*", "", tidy_data1$Name),
                     Measured_Parameter = sub(" - .*", "", tidy_data1$Name)
)

tidy_data2$well <- ifelse(nchar(tidy_data2$well) == 2, sub(pattern = "(.)(.)", replacement = "\\10\\2", tidy_data2$well), tidy_data2$well)

## eliminating "Name" and "Channel" columns, rearranging tibble
tidy_data3 <- select(tidy_data2, -(Name:Channel)) %>%
  select(well, cell_ID,everything())

## pivot_wider()
tidy_data4 <- tidy_data3 %>%
  pivot_wider(names_from = Measured_Parameter,
              values_from = Signal_Intensity)

## removing non-existing cells from each well (i.e. where signal intensities = NA)
tidy_data5 <- tidy_data4 %>% na.omit()

# importing metadata regarding conditions of each well (IR or CTL, full serum or serum-starved, different drug concentrations etc.)

## plater
file_path <- str_c(getwd(),"/",plate_template_name,".csv", sep = "") # gets string with full path

plate_metadata <- read_plate(
  file = file_path,             # full path to the .csv file
  well_ids_column = "well",    # name to give column of well IDs (optional)
  sep = ","                     # separator used in the csv file (optional)
)

## change variable names to lower case, removing parenthesis (if present)
colnames(plate_metadata) <- colnames(plate_metadata) %>%
  tolower() %>%
  str_replace_all(., "[()]", "")

## check that plate-template contains Condition variable
if (any(colnames(plate_metadata) %in% "condition") == TRUE) {} else {
  beep(1)
  Sys.sleep(2)
  stop(
    "The metadata entered in plate-template must contain the \"Condition\" variable"
  )}

## check that the variables contained in plate-template are limited to
## condition, and up to 2 additional variable (e.g. serum and/or drug treatment)
additional_variables <- colnames(plate_metadata) %>%
  .[-grep(pattern = "well|condition", .)]

if (length(additional_variables) > 2) {
  beep(1)
  Sys.sleep(2)
  stop(
    "The metadata entered in plate-template is not acceptable.

The only metadata that can be entered in the plate-template file are
\"Condition\" and up to TWO more variable (e.g. \"Serum\" and/or \"Drug_concentration\")")
}

additional_variables_check <-  if (length(additional_variables) > 0) {TRUE} else {FALSE}
multiple_additional_variables_check <- if (length(additional_variables) == 2) {TRUE} else {FALSE}

# add metadata info (conditions & serum) to table with observations (signal intensities for each cell)

## mutating join with left_join()

plate_metadata_variables <- colnames(plate_metadata)[-1]

tidy_data6 <- tidy_data5 %>%
  left_join(plate_metadata, by = "well") %>%
  select(well, cell_ID, all_of(plate_metadata_variables), everything())

# changing the column names based on the strings entered at the beginning of script (READ ME section)

## changing current variable/column names to the ones inputted in the READ ME section
colnames(tidy_data6)[str_detect(colnames(tidy_data6), "Plot of Each")] <- "Nuclear_Area"

latest_value <- colnames(tidy_data6)

param <- tibble(previous = list(DAPI_label_number, EdU_label_number, SABGal_label_number),
                new = list("DAPI","EdU","SABGal"))

update_value <- function(previous, new) {
  latest_value <<- sub(pattern = str_c(".+Label #", previous, ".*"), replacement = new, latest_value)
} # function needs "<<-" in order to update the global variable "latest_value"

map2(param$previous, param$new, update_value)

colnames(tidy_data6) <- latest_value

"completed tidying IA output"

## creating graph folder

graphs_folder <- str_c("Analysis_", Image_Analyst_output_file_name,"_", Sys.time()) %>%
  str_replace_all(pattern = " ", replacement = "_") %>%
  str_replace_all(pattern = ":", replacement = "") %>%
  str_replace_all(pattern = ".xlsx", replacement = "")

dir.create(graphs_folder)

tidy_IA_ouput_1 <- read.csv("tidy-IA-output_Plate1.csv")
tidy_IA_ouput_2 <- read.csv("tidy-IA-output_Plate2.csv")

tidy_IA_ouput <- rbind(tidy_IA_ouput_1, tidy_IA_ouput_2)


# Tidying IA output: END --------------------------------------------------


df <- tidy_IA_ouput

treatment_variable <- cell_viability_variable %>%
  tolower() %>%
  str_replace_all(., "[()]", "")

# renaming variable corresponding to the treatment variable entered in plate-template
new_col_names <- colnames(df) %>%
  str_replace(., tolower(treatment_variable), "treatment_variable")

colnames(df) <- new_col_names

# changing treatment variable into a factor

df$treatment_variable <- as.factor(df$treatment_variable)


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

DAPI_threshold_CTL <- 14000000

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
  geom_vline(xintercept = DAPI_threshold_SEN) +
  geom_point(alpha = 1/3) +
  facet_grid(condition ~ treatment_variable) +
  color_scale_conditions_signal +
  labs(y = str_replace_all(morphological_parameter, "_", " "),
       x = "DNA Content (DAPI staining, A.U.)",
       color = "Condition") +
  scale_x_continuous(limits = c(0,quantile(df_signal_SEN_only$DAPI, 0.99))) +
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

