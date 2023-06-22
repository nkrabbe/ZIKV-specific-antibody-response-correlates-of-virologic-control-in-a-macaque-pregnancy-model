library(readxl)
library(ggplot2)
library(tidyverse)
library(ggpubr)

###
# Sample OD450 reading to calibrator OD450 reading ratio- IgM Binding ELISA
###
sample_OD450_calibrator_OD450_ratio <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\IgM ELISA\\Final Raw Data, figures, and code\\IgM Binding ELISA Data (Final).xlsx", sheet= 1, col_names = TRUE)
sample_OD450_calibrator_OD450_ratio$Virologic_Control_Group = as.character(sample_OD450_calibrator_OD450_ratio$Virologic_Control_Group)
sample_OD450_calibrator_OD450_ratio$Virologic_Control_Group <- factor(sample_OD450_calibrator_OD450_ratio$Virologic_Control_Group, levels = c('Controller', 'Non-Controller'))
sample_OD450_calibrator_OD450_ratio$Timepoint_DPI = as.character(sample_OD450_calibrator_OD450_ratio$Timepoint_DPI)
sample_OD450_calibrator_OD450_ratio$Timepoint_DPI <- factor(sample_OD450_calibrator_OD450_ratio$Timepoint_DPI, levels = c('Pre-Infection', '2-4', '7-10', '13-16', '18-22', '27-35', '50-60', '80-90'))
sample_OD450_calibrator_OD450_ratio$Sample_to_Calibrator_Ratio = as.numeric(sample_OD450_calibrator_OD450_ratio$Sample_to_Calibrator_Ratio)
sample_OD450_calibrator_OD450_ratio_figure <-
  ggplot() + 
  geom_boxplot(data = sample_OD450_calibrator_OD450_ratio, aes(x= Timepoint_DPI, y= Sample_to_Calibrator_Ratio, color = Virologic_Control_Group), fill = 'white', outlier.colour =  'white') +
  geom_point(data = sample_OD450_calibrator_OD450_ratio, aes(x= Timepoint_DPI, y= Sample_to_Calibrator_Ratio, color = Virologic_Control_Group), position = position_jitterdodge(seed = 123, jitter.width = 0.35), size= 2, shape= 16) +
  theme_classic() + 
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = guide_legend('Virologic Control Group')) + 
  scale_x_discrete(name = 'Timepoint (DPI)', expand = c(0,0)) + 
  scale_y_continuous(name = 'IgM Sample to Calibrator Ratio', expand = c(0,0), limits = c(0,13.2), breaks = c(0, 2, 4, 6, 8, 10, 12)) + 
  geom_hline(yintercept = 0.8, linetype = 'solid') + 
  geom_hline(yintercept = 1.1, linetype = 'dotted') + 
  theme(axis.text.x= element_text(size = 20, angle = 45, hjust = 1, vjust = 1), axis.text.y = element_text(size = 20), axis.title = element_text(size = 20, face = 'bold'), aspect.ratio= 1, legend.title = element_text(size = 20, face = 'bold'), legend.text = element_text(size = 20), legend.position = 'top') +
  geom_segment(aes(x= 3.75, xend= 3.75, y= 11.8, yend= 12)) +
  geom_segment(aes(x= 4.25, xend= 4.25, y= 11.8, yend= 12)) +
  geom_segment(aes(x= 3.75, xend= 4.25, y= 12, yend= 12)) + 
  geom_text(aes(x= 4, y= 12.1, label = '**'), size = 6) +
  geom_segment(aes(x= 4.75, xend= 4.75, y= 12.6, yend= 12.8)) +
  geom_segment(aes(x= 5.25, xend= 5.25, y= 12.6, yend= 12.8)) +
  geom_segment(aes(x= 4.75, xend= 5.25, y= 12.8, yend= 12.8)) +
  geom_text(aes(x= 5, y= 12.9, label = '*'), size = 6) +
  geom_segment(aes(x= 5.75, xend= 5.75, y= 6.8, yend= 7)) +
  geom_segment(aes(x= 6.25, xend= 6.25, y= 6.8, yend= 7)) +
  geom_segment(aes(x= 5.75, xend= 6.25, y= 7, yend= 7)) + 
  geom_text(aes(x= 6, y= 7.1, label= '*'), size = 6)
sample_OD450_calibrator_OD450_ratio_figure

### Statistical analyses comparing virologic control groups at each timepoint- Mann Whitney U-Test (Wilcox test)
# Pre-infection
sample_OD450_calibrator_OD450_ratio_preinfection <- filter(sample_OD450_calibrator_OD450_ratio, Timepoint_DPI == 'Pre-Infection')
sample_OD450_calibrator_OD450_ratio_preinfection_controller <- filter(sample_OD450_calibrator_OD450_ratio_preinfection, Virologic_Control_Group == 'Controller')
sample_OD450_calibrator_OD450_ratio_preinfection_controller_quantile <- quantile(sample_OD450_calibrator_OD450_ratio_preinfection_controller$Sample_to_Calibrator_Ratio, probs= c(0.25, 0.75), na.rm= TRUE)
sample_OD450_calibrator_OD450_ratio_preinfection_controller_quantile
sample_OD450_calibrator_OD450_ratio_preinfection_non_controller <- filter(sample_OD450_calibrator_OD450_ratio_preinfection, Virologic_Control_Group == 'Non-Controller')
sample_OD450_calibrator_OD450_ratio_preinfection_non_controller_quantile <- quantile(sample_OD450_calibrator_OD450_ratio_preinfection_non_controller$Sample_to_Calibrator_Ratio, probs= c(0.25, 0.75), na.rm= TRUE)
sample_OD450_calibrator_OD450_ratio_preinfection_non_controller_quantile
wilcox.test(Sample_to_Calibrator_Ratio~Virologic_Control_Group, data = sample_OD450_calibrator_OD450_ratio_preinfection, exact = FALSE)
# 2-4 DPI
sample_OD450_calibrator_OD450_ratio_2.4dpi <- filter(sample_OD450_calibrator_OD450_ratio, Timepoint_DPI == '2-4')
sample_OD450_calibrator_OD450_ratio_2.4dpi_controller <- filter(sample_OD450_calibrator_OD450_ratio_2.4dpi, Virologic_Control_Group == 'Controller')
sample_OD450_calibrator_OD450_ratio_2.4dpi_controller_quantile <- quantile(sample_OD450_calibrator_OD450_ratio_2.4dpi_controller$Sample_to_Calibrator_Ratio, probs= c(0.25, 0.75), na.rm= TRUE)
sample_OD450_calibrator_OD450_ratio_2.4dpi_controller_quantile
sample_OD450_calibrator_OD450_ratio_2.4dpi_non_controller <- filter(sample_OD450_calibrator_OD450_ratio_2.4dpi, Virologic_Control_Group == 'Non-Controller')
sample_OD450_calibrator_OD450_ratio_2.4dpi_non_controller_quantile <- quantile(sample_OD450_calibrator_OD450_ratio_2.4dpi_non_controller$Sample_to_Calibrator_Ratio, probs= c(0.25, 0.75), na.rm= TRUE)
sample_OD450_calibrator_OD450_ratio_2.4dpi_non_controller_quantile
wilcox.test(Sample_to_Calibrator_Ratio~Virologic_Control_Group, data = sample_OD450_calibrator_OD450_ratio_2.4dpi, exact = FALSE)
# 7-10 DPI
sample_OD450_calibrator_OD450_ratio_7.10dpi <- filter(sample_OD450_calibrator_OD450_ratio, Timepoint_DPI == '7-10')
sample_OD450_calibrator_OD450_ratio_7.10dpi_controller <- filter(sample_OD450_calibrator_OD450_ratio_7.10dpi, Virologic_Control_Group == 'Controller')
sample_OD450_calibrator_OD450_ratio_7.10dpi_controller_quantile <- quantile(sample_OD450_calibrator_OD450_ratio_7.10dpi_controller$Sample_to_Calibrator_Ratio, probs= c(0.25, 0.75), na.rm= TRUE)
sample_OD450_calibrator_OD450_ratio_7.10dpi_controller_quantile
sample_OD450_calibrator_OD450_ratio_7.10dpi_non_controller <- filter(sample_OD450_calibrator_OD450_ratio_7.10dpi, Virologic_Control_Group == 'Non-Controller')
sample_OD450_calibrator_OD450_ratio_7.10dpi_non_controller_quantile <- quantile(sample_OD450_calibrator_OD450_ratio_7.10dpi_non_controller$Sample_to_Calibrator_Ratio, probs= c(0.25, 0.75), na.rm= TRUE)
sample_OD450_calibrator_OD450_ratio_7.10dpi_non_controller_quantile
wilcox.test(Sample_to_Calibrator_Ratio~Virologic_Control_Group, data = sample_OD450_calibrator_OD450_ratio_7.10dpi, exact = FALSE)
# 13-16 DPI
sample_OD450_calibrator_OD450_ratio_13.16dpi <- filter(sample_OD450_calibrator_OD450_ratio, Timepoint_DPI == '13-16')
sample_OD450_calibrator_OD450_ratio_13.16dpi_controller <- filter(sample_OD450_calibrator_OD450_ratio_13.16dpi, Virologic_Control_Group == 'Controller')
sample_OD450_calibrator_OD450_ratio_13.16dpi_controller_quantile <- quantile(sample_OD450_calibrator_OD450_ratio_13.16dpi_controller$Sample_to_Calibrator_Ratio, probs= c(0.25, 0.75), na.rm= TRUE)
sample_OD450_calibrator_OD450_ratio_13.16dpi_controller_quantile
sample_OD450_calibrator_OD450_ratio_13.16dpi_non_controller <- filter(sample_OD450_calibrator_OD450_ratio_13.16dpi, Virologic_Control_Group == 'Non-Controller')
sample_OD450_calibrator_OD450_ratio_13.16dpi_non_controller_quantile <- quantile(sample_OD450_calibrator_OD450_ratio_13.16dpi_non_controller$Sample_to_Calibrator_Ratio, probs= c(0.25, 0.75), na.rm= TRUE)
sample_OD450_calibrator_OD450_ratio_13.16dpi_non_controller_quantile
wilcox.test(Sample_to_Calibrator_Ratio~Virologic_Control_Group, data = sample_OD450_calibrator_OD450_ratio_13.16dpi, exact = FALSE)
# 18-22 DPI
sample_OD450_calibrator_OD450_ratio_18.22dpi <- filter(sample_OD450_calibrator_OD450_ratio, Timepoint_DPI == '18-22')
sample_OD450_calibrator_OD450_ratio_18.22dpi_controller <- filter(sample_OD450_calibrator_OD450_ratio_18.22dpi, Virologic_Control_Group == 'Controller')
sample_OD450_calibrator_OD450_ratio_18.22dpi_controller_quantile <- quantile(sample_OD450_calibrator_OD450_ratio_18.22dpi_controller$Sample_to_Calibrator_Ratio, probs= c(0.25, 0.75), na.rm= TRUE)
sample_OD450_calibrator_OD450_ratio_18.22dpi_controller_quantile
sample_OD450_calibrator_OD450_ratio_18.22dpi_non_controller <- filter(sample_OD450_calibrator_OD450_ratio_18.22dpi, Virologic_Control_Group == 'Non-Controller')
sample_OD450_calibrator_OD450_ratio_18.22dpi_non_controller_quantile <- quantile(sample_OD450_calibrator_OD450_ratio_18.22dpi_non_controller$Sample_to_Calibrator_Ratio, probs= c(0.25, 0.75), na.rm= TRUE)
sample_OD450_calibrator_OD450_ratio_18.22dpi_non_controller_quantile
wilcox.test(Sample_to_Calibrator_Ratio~Virologic_Control_Group, data = sample_OD450_calibrator_OD450_ratio_18.22dpi, exact = FALSE)
# 27-35 DPI
sample_OD450_calibrator_OD450_ratio_27.35dpi <- filter(sample_OD450_calibrator_OD450_ratio, Timepoint_DPI == '27-35')
sample_OD450_calibrator_OD450_ratio_27.35dpi_controller <- filter(sample_OD450_calibrator_OD450_ratio_27.35dpi, Virologic_Control_Group == 'Controller')
sample_OD450_calibrator_OD450_ratio_27.35dpi_controller_quantile <- quantile(sample_OD450_calibrator_OD450_ratio_27.35dpi_controller$Sample_to_Calibrator_Ratio, probs= c(0.25, 0.75), na.rm= TRUE)
sample_OD450_calibrator_OD450_ratio_27.35dpi_controller_quantile
sample_OD450_calibrator_OD450_ratio_27.35dpi_non_controller <- filter(sample_OD450_calibrator_OD450_ratio_27.35dpi, Virologic_Control_Group == 'Non-Controller')
sample_OD450_calibrator_OD450_ratio_27.35dpi_non_controller_quantile <- quantile(sample_OD450_calibrator_OD450_ratio_27.35dpi_non_controller$Sample_to_Calibrator_Ratio, probs= c(0.25, 0.75), na.rm= TRUE)
sample_OD450_calibrator_OD450_ratio_27.35dpi_non_controller_quantile
wilcox.test(Sample_to_Calibrator_Ratio~Virologic_Control_Group, data = sample_OD450_calibrator_OD450_ratio_27.35dpi, exact = FALSE)
# 50-60 DPI
sample_OD450_calibrator_OD450_ratio_50.60dpi <- filter(sample_OD450_calibrator_OD450_ratio, Timepoint_DPI == '50-60')
sample_OD450_calibrator_OD450_ratio_50.60dpi_controller <- filter(sample_OD450_calibrator_OD450_ratio_50.60dpi, Virologic_Control_Group == 'Controller')
sample_OD450_calibrator_OD450_ratio_50.60dpi_controller_quantile <- quantile(sample_OD450_calibrator_OD450_ratio_50.60dpi_controller$Sample_to_Calibrator_Ratio, probs= c(0.25, 0.75), na.rm= TRUE)
sample_OD450_calibrator_OD450_ratio_50.60dpi_controller_quantile
sample_OD450_calibrator_OD450_ratio_50.60dpi_non_controller <- filter(sample_OD450_calibrator_OD450_ratio_50.60dpi, Virologic_Control_Group == 'Non-Controller')
sample_OD450_calibrator_OD450_ratio_50.60dpi_non_controller_quantile <- quantile(sample_OD450_calibrator_OD450_ratio_50.60dpi_non_controller$Sample_to_Calibrator_Ratio, probs= c(0.25, 0.75), na.rm= TRUE)
sample_OD450_calibrator_OD450_ratio_50.60dpi_non_controller_quantile
wilcox.test(Sample_to_Calibrator_Ratio~Virologic_Control_Group, data = sample_OD450_calibrator_OD450_ratio_50.60dpi, exact = FALSE)
# 80-90 DPI
sample_OD450_calibrator_OD450_ratio_80.90dpi <- filter(sample_OD450_calibrator_OD450_ratio, Timepoint_DPI == '80-90')
sample_OD450_calibrator_OD450_ratio_80.90dpi_controller <- filter(sample_OD450_calibrator_OD450_ratio_80.90dpi, Virologic_Control_Group == 'Controller')
sample_OD450_calibrator_OD450_ratio_80.90dpi_controller_quantile <- quantile(sample_OD450_calibrator_OD450_ratio_80.90dpi_controller$Sample_to_Calibrator_Ratio, probs= c(0.25, 0.75), na.rm= TRUE)
sample_OD450_calibrator_OD450_ratio_80.90dpi_controller_quantile
sample_OD450_calibrator_OD450_ratio_80.90dpi_non_controller <- filter(sample_OD450_calibrator_OD450_ratio_80.90dpi, Virologic_Control_Group == 'Non-Controller')
sample_OD450_calibrator_OD450_ratio_80.90dpi_non_controller_quantile <- quantile(sample_OD450_calibrator_OD450_ratio_80.90dpi_non_controller$Sample_to_Calibrator_Ratio, probs= c(0.25, 0.75), na.rm= TRUE)
sample_OD450_calibrator_OD450_ratio_80.90dpi_non_controller_quantile
wilcox.test(Sample_to_Calibrator_Ratio~Virologic_Control_Group, data = sample_OD450_calibrator_OD450_ratio_80.90dpi, exact = FALSE)
# summary stats
summary_stats <- sample_OD450_calibrator_OD450_ratio %>%
  group_by(Timepoint_DPI, Virologic_Control_Group) %>%
  summarise(median= median(Sample_to_Calibrator_Ratio, na.rm= TRUE), IQR(Sample_to_Calibrator_Ratio, na.rm= TRUE), count= n())

###
# Percentage of animals in each virologic control group that are IgM positive at each timepoint
###
IgM_percent_positive <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\IgM ELISA\\Final Raw Data, figures, and code\\IgM Binding ELISA Data (Final).xlsx", sheet = 2, col_names = TRUE)
IgM_percent_positive$Virologic_Control_Group = as.character(IgM_percent_positive$Virologic_Control_Group)
IgM_percent_positive$Virologic_Control_Group <- factor(IgM_percent_positive$Virologic_Control_Group, levels = c('Controller', 'Non-Controller'))
IgM_percent_positive$Timepoint_DPI = as.character(IgM_percent_positive$Timepoint_DPI)
IgM_percent_positive$Timepoint_DPI <- factor(IgM_percent_positive$Timepoint_DPI, levels = c('Pre-Infection', '2-4', '7-10', '13-16', '18-22', '27-35', '50-60', '80-90'))
IgM_percent_positive$Percent_Positive = as.numeric(IgM_percent_positive$Percent_Positive)
IgM_percent_positive_figure <-
  ggplot(data = IgM_percent_positive, aes(x= Timepoint_DPI, y= Percent_Positive, color = Virologic_Control_Group, fill = Virologic_Control_Group)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.25) + 
  theme_classic() +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) + 
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) + 
  guides(color = 'none', fill = 'none') +
  scale_x_discrete(name = 'Timepoint (DPI)', expand = c(0,0)) + 
  scale_y_continuous(name = 'IgM-Positive Dams (%)', expand = c(0,0), limits= c(0, 101), breaks= c(0, 25, 50, 75, 100)) +
  theme(axis.text.x = element_text(size = 20, angle = 45, hjust = 1, vjust = 1), axis.text.y = element_text(size = 20), axis.title = element_text(size = 20, face = 'bold'), aspect.ratio = 1)
IgM_percent_positive_figure

### Statistical analyses for percentage of IgM positive animals in each group at each timepoint
# Pre-infection
IgM_percent_positive_preinfection <- filter(IgM_percent_positive, Timepoint_DPI == 'Pre-Infection')
IgM_percent_positive_preinfection_controller <- filter(IgM_percent_positive_preinfection, Virologic_Control_Group == 'Controller')
IgM_percent_positive_preinfection_controller_quantile <- quantile(IgM_percent_positive_preinfection_controller$Percent_Positive, probs= c(0.25, 0.75), na.rm= TRUE)
IgM_percent_positive_preinfection_controller_quantile
IgM_percent_positive_preinfection_non_controller <- filter(IgM_percent_positive_preinfection, Virologic_Control_Group == 'Non-Controller')
IgM_percent_positive_preinfection_non_controller_quantile <- quantile(IgM_percent_positive_preinfection_non_controller$Percent_Positive, probs= c(0.25, 0.75), na.rm= TRUE)
IgM_percent_positive_preinfection_non_controller_quantile
# 2-4 DPI
IgM_percent_positive_2.4dpi <- filter(IgM_percent_positive, Timepoint_DPI == '2-4')
IgM_percent_positive_2.4dpi_controller <- filter(IgM_percent_positive_2.4dpi, Virologic_Control_Group == 'Controller')
IgM_percent_positive_2.4dpi_controller_quantile <- quantile(IgM_percent_positive_2.4dpi_controller$Percent_Positive, probs= c(0.25, 0.75), na.rm= TRUE)
IgM_percent_positive_2.4dpi_controller_quantile
IgM_percent_positive_2.4dpi_non_controller <- filter(IgM_percent_positive_2.4dpi, Virologic_Control_Group == 'Non-Controller')
IgM_percent_positive_2.4dpi_non_controller_quantile <- quantile(IgM_percent_positive_2.4dpi_non_controller$Percent_Positive, probs= c(0.25, 0.75), na.rm= TRUE)
IgM_percent_positive_2.4dpi_non_controller_quantile
# 7-10 dpi
IgM_percent_positive_7.10dpi <- filter(IgM_percent_positive, Timepoint_DPI == '7-10')
IgM_percent_positive_7.10dpi_controller <- filter(IgM_percent_positive_7.10dpi, Virologic_Control_Group == 'Controller')
IgM_percent_positive_7.10dpi_controller_quantile <- quantile(IgM_percent_positive_7.10dpi_controller$Percent_Positive, probs= c(0.25, 0.75), na.rm= TRUE)
IgM_percent_positive_7.10dpi_controller_quantile
IgM_percent_positive_7.10dpi_non_controller <- filter(IgM_percent_positive_7.10dpi, Virologic_Control_Group == 'Non-Controller')
IgM_percent_positive_7.10dpi_non_controller_quantile <- quantile(IgM_percent_positive_7.10dpi_non_controller$Percent_Positive, probs= c(0.25, 0.75), na.rm= TRUE)
IgM_percent_positive_7.10dpi_non_controller_quantile
# 13-16 dpi
IgM_percent_positive_13.16dpi <- filter(IgM_percent_positive, Timepoint_DPI == '13-16')
IgM_percent_positive_13.16dpi_controller <- filter(IgM_percent_positive_13.16dpi, Virologic_Control_Group == 'Controller')
IgM_percent_positive_13.16dpi_controller_quantile <- quantile(IgM_percent_positive_13.16dpi_controller$Percent_Positive, probs= c(0.25, 0.75), na.rm= TRUE)
IgM_percent_positive_13.16dpi_controller_quantile
IgM_percent_positive_13.16dpi_non_controller <- filter(IgM_percent_positive_13.16dpi, Virologic_Control_Group == 'Non-Controller')
IgM_percent_positive_13.16dpi_non_controller_quantile <- quantile(IgM_percent_positive_13.16dpi_non_controller$Percent_Positive, probs= c(0.25, 0.75), na.rm= TRUE)
IgM_percent_positive_13.16dpi_non_controller_quantile
# 18-22 dpi
IgM_percent_positive_18.22dpi <- filter(IgM_percent_positive, Timepoint_DPI == '18-22')
IgM_percent_positive_18.22dpi_controller <- filter(IgM_percent_positive_18.22dpi, Virologic_Control_Group == 'Controller')
IgM_percent_positive_18.22dpi_controller_quantile <- quantile(IgM_percent_positive_18.22dpi_controller$Percent_Positive, probs= c(0.25, 0.75), na.rm= TRUE)
IgM_percent_positive_18.22dpi_controller_quantile
IgM_percent_positive_18.22dpi_non_controller <- filter(IgM_percent_positive_18.22dpi, Virologic_Control_Group == 'Non-Controller')
IgM_percent_positive_18.22dpi_non_controller_quantile <- quantile(IgM_percent_positive_18.22dpi_non_controller$Percent_Positive, probs= c(0.25, 0.75), na.rm= TRUE)
IgM_percent_positive_18.22dpi_non_controller_quantile
# 27-35 dpi
IgM_percent_positive_27.35dpi <- filter(IgM_percent_positive, Timepoint_DPI == '27-35')
IgM_percent_positive_27.35dpi_controller <- filter(IgM_percent_positive_27.35dpi, Virologic_Control_Group == 'Controller')
IgM_percent_positive_27.35dpi_controller_quantile <- quantile(IgM_percent_positive_27.35dpi_controller$Percent_Positive, probs= c(0.25, 0.75), na.rm= TRUE)
IgM_percent_positive_27.35dpi_controller_quantile
IgM_percent_positive_27.35dpi_non_controller <- filter(IgM_percent_positive_27.35dpi, Virologic_Control_Group == 'Non-Controller')
IgM_percent_positive_27.35dpi_non_controller_quantile <- quantile(IgM_percent_positive_27.35dpi_non_controller$Percent_Positive, probs= c(0.25, 0.75), na.rm= TRUE)
IgM_percent_positive_27.35dpi_non_controller_quantile
# 50-60 dpi
IgM_percent_positive_50.60dpi <- filter(IgM_percent_positive, Timepoint_DPI == '50-60')
IgM_percent_positive_50.60dpi_controller <- filter(IgM_percent_positive_50.60dpi, Virologic_Control_Group == 'Controller')
IgM_percent_positive_50.60dpi_controller_quantile <- quantile(IgM_percent_positive_50.60dpi_controller$Percent_Positive, probs= c(0.25, 0.75), na.rm= TRUE)
IgM_percent_positive_50.60dpi_controller_quantile
IgM_percent_positive_50.60dpi_non_controller <- filter(IgM_percent_positive_50.60dpi, Virologic_Control_Group == 'Non-Controller')
IgM_percent_positive_50.60dpi_non_controller_quantile <- quantile(IgM_percent_positive_50.60dpi_non_controller$Percent_Positive, probs= c(0.25, 0.75), na.rm= TRUE)
IgM_percent_positive_50.60dpi_non_controller_quantile
# 80-90 dpi
IgM_percent_positive_80.90dpi <- filter(IgM_percent_positive, Timepoint_DPI == '80-90')
IgM_percent_positive_80.90dpi_controller <- filter(IgM_percent_positive_80.90dpi, Virologic_Control_Group == 'Controller')
IgM_percent_positive_80.90dpi_controller_quantile <- quantile(IgM_percent_positive_80.90dpi_controller$Percent_Positive, probs= c(0.25, 0.75), na.rm= TRUE)
IgM_percent_positive_80.90dpi_controller_quantile
IgM_percent_positive_80.90dpi_non_controller <- filter(IgM_percent_positive_80.90dpi, Virologic_Control_Group == 'Non-Controller')
IgM_percent_positive_80.90dpi_non_controller_quantile <- quantile(IgM_percent_positive_80.90dpi_non_controller$Percent_Positive, probs= c(0.25, 0.75), na.rm= TRUE)
IgM_percent_positive_80.90dpi_non_controller_quantile

###
# Combine figures into a single multi-panel figure
###
tiff("IgM Binding ELISA Data (Final).tiff", units = 'in', height= 8, width = 10, res= 300)
ggarrange(sample_OD450_calibrator_OD450_ratio_figure, IgM_percent_positive_figure, labels = c('A', 'B'), nrow= 1, ncol= 2, common.legend = TRUE, legend = 'top', align = 'hv')
dev.off()

