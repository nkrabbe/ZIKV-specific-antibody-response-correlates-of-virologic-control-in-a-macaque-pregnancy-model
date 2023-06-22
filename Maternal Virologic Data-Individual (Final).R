library(readxl)
library(tidyverse)
library(ggplot2)
library(scales)
library(ggthemes)
library(reshape2)
library(stringr)
library(dplyr)
library(plotrix)
library(stats)
library(gtable)
library(grid)
library(multipanelfigure)
library(ggpubr)
library(cowplot)
library(gridExtra)

###
# Plasma vRNA kinetics over the course of pregnancy (Individual)
###
plasma_vRNA_loads_individual <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\Maternal Viral Characteristics\\Final Raw Data, figures, and code\\Maternal Virologic Data (Final).xlsx", sheet = 1, col_names = TRUE)
plasma_vRNA_loads_individual$Days_Post_Infection = as.numeric(plasma_vRNA_loads_individual$Days_Post_Infection)
plasma_vRNA_loads_individual$Animal_ID <- factor(plasma_vRNA_loads_individual$Animal_ID, levels = c("044-101", "044-102", "044-103", "044-104", "044-109", "044-110", "044-112", "044-114", "044-116", "044-117", "044-118", "044-122", "044-126", "044-127", "044-130", "044-131", "044-132", "044-133"))
# Set all plasma vRNA loads below the LLoD (150 copies/mL) equal to the LLoD
plasma_vRNA_loads_individual$Plasma_vRNA_Load_Copies <- (ifelse(plasma_vRNA_loads_individual$Plasma_vRNA_Load_Copies < 150, 150, plasma_vRNA_loads_individual$Plasma_vRNA_Load_Copies))
# Graph the plasma vRNA loads over the course of pregancny 
plasma_vRNA_loads_individual_figure <- 
  ggplot(data = plasma_vRNA_loads_individual, aes(x = Days_Post_Infection, y = Plasma_vRNA_Load_Copies, color = Animal_ID)) + 
  geom_line(size = 0.5) + geom_point() + theme_classic() + 
  scale_x_continuous(name = "DPI", limits = c(-10, 130), breaks = c(-10, 0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130), expand = c(0,0)) + 
  annotation_logticks(base = 10, sides = 'l', scaled = TRUE, short = unit(-0.5, "cm"), mid = unit(-0.5, "cm"), long = unit(-0.5, "cm")) + 
  scale_y_log10(name = "Plasma vRNA (copies/mL)", breaks = c(100, 1000, 10000, 100000, 1000000), labels = trans_format("log10", math_format(10^.x)), expand = c(0,0), limits = c(100,10^6.1)) + 
  scale_color_manual(values = c("#7b0000", "#ad0000", "#FF0000", "#ff5d5d", "#ffa3a3", "#ffd2d2", "#000088", "#3434fb", "#6c6cff", "#9c9cff", "#d0d0ff", "#0ef000", "#c1c000", "#fffe04", "#ffd991", "#c7ad7c", "#6ca7a2", "#9c7a99"),
                     breaks = c("044-101", "044-102", "044-103", "044-104", "044-109", "044-110", "044-112", "044-114", "044-116", "044-117", "044-118", "044-122", "044-126", "044-127", "044-130", "044-131", "044-132", "044-133")) + 
  theme(axis.title= element_text(size = 20, face = "bold"), axis.text.x = element_text(size = 20, angle = 90, hjust = 1, vjust = 0.5), axis.text.y = element_text(size = 20), legend.text = element_text(size= 20), legend.title = element_text(size = 20, face = 'bold'), legend.position = 'top') + 
  theme(aspect.ratio = 1) + geom_hline(yintercept = 150, linetype = "dashed", linecolor = "black") + guides(color = guide_legend('Dam ID'))
plasma_vRNA_loads_individual_figure

###
# Area under the curve values of the viral load kinetics curves (individual)
###
AUC_indiviual <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\Maternal Viral Characteristics\\Final Raw Data, figures, and code\\Maternal Virologic Data (Final).xlsx", sheet = 2, col_names= TRUE)
AUC_indiviual$Animal_ID <- factor(AUC_indiviual$Animal_ID, levels = c("044-101", "044-102", "044-103", "044-104", "044-109", "044-110", "044-112", "044-114", "044-116", "044-117", "044-118", "044-122", "044-126", "044-127", "044-130", "044-131", "044-132", "044-133"))
AUC_individual_figure <-
  ggplot() +
  geom_bar(data = AUC_indiviual, aes(x= Animal_ID, y= Area_Under_Curve, color = Animal_ID, fill = Animal_ID), stat = 'identity', width = 0.25) + 
  theme_classic() + 
  scale_x_discrete(name = 'Dam ID') + 
  annotation_logticks(base = 10, sides = 'l', scaled = TRUE, short = unit(-0.5, "cm"), mid = unit(-0.5, "cm"), long = unit(-0.5, "cm")) + 
  scale_y_log10(name = "AUC", breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)), expand = c(0,0), limits = c(1,10^6.2)) +
  scale_color_manual(values = c("#7b0000", "#ad0000", "#FF0000", "#ff5d5d", "#ffa3a3", "#ffd2d2", "#000088", "#3434fb", "#6c6cff", "#9c9cff", "#d0d0ff", "#0ef000", "#c1c000", "#fffe04", "#ffd991", "#c7ad7c", "#6ca7a2", "#9c7a99"),
                     breaks = c("044-101", "044-102", "044-103", "044-104", "044-109", "044-110", "044-112", "044-114", "044-116", "044-117", "044-118", "044-122", "044-126", "044-127", "044-130", "044-131", "044-132", "044-133")) +
  scale_fill_manual(values = c("#7b0000", "#ad0000", "#FF0000", "#ff5d5d", "#ffa3a3", "#ffd2d2", "#000088", "#3434fb", "#6c6cff", "#9c9cff", "#d0d0ff", "#0ef000", "#c1c000", "#fffe04", "#ffd991", "#c7ad7c", "#6ca7a2", "#9c7a99"),
                    breaks = c("044-101", "044-102", "044-103", "044-104", "044-109", "044-110", "044-112", "044-114", "044-116", "044-117", "044-118", "044-122", "044-126", "044-127", "044-130", "044-131", "044-132", "044-133")) +
  theme(axis.title= element_text(size = 20, face = "bold"), axis.text.x = element_text(size = 20, angle = 90, hjust= 1, vjust= 0.5), axis.text.y = element_text(size = 20)) + 
  theme(aspect.ratio = 1) + guides(color= 'none', fill= 'none')
AUC_individual_figure

###
# Timing of peak plasma vRNA load (individual)
###
peak_plasma_vRNA_timing <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\Maternal Viral Characteristics\\Final Raw Data, figures, and code\\Maternal Virologic Data (Final).xlsx", sheet= 3, col_names = TRUE)
peak_plasma_vRNA_timing$Peak_vRNA_Load_DPI = as.character(peak_plasma_vRNA_timing$Peak_vRNA_Load_DPI)
peak_plasma_vRNA_timing$Peak_vRNA_Load_DPI <- factor(peak_plasma_vRNA_timing$Peak_vRNA_Load_DPI, levels = c(2, 3, 4, 5, 6))
peak_plasma_vRNA_timing_figure <-
  ggplot(data= peak_plasma_vRNA_timing, aes(x= Peak_vRNA_Load_DPI, y= Number_of_Animals, fill= Animal_ID)) +
  geom_bar(stat= 'identity', width = 0.25) +
  theme_classic() + 
  scale_x_discrete(name = 'Timing of Peak Plasma vRNA Load (DPI)') +
  scale_y_continuous(name= 'Number of Animals', expand= c(0,0), limits= c(0, 12), breaks = c(0, 2, 4, 6, 8, 10, 12)) + 
  theme(axis.title= element_text(size = 20, face = "bold"), axis.text = element_text(size = 20)) + 
  theme(aspect.ratio = 1) + 
  scale_fill_manual(values = c("#7b0000", "#ad0000", "#FF0000", "#ff5d5d", "#ffa3a3", "#ffd2d2", "#000088", "#3434fb", "#6c6cff", "#9c9cff", "#d0d0ff", "#0ef000", "#c1c000", "#fffe04", "#ffd991", "#c7ad7c", "#6ca7a2", "#9c7a99"),
                    breaks = c("044-101", "044-102", "044-103", "044-104", "044-109", "044-110", "044-112", "044-114", "044-116", "044-117", "044-118", "044-122", "044-126", "044-127", "044-130", "044-131", "044-132", "044-133")) + 
  guides(fill = 'none')
peak_plasma_vRNA_timing_figure

###
# Peak plasma vRNA load (individual)
###
peak_plasma_vRNA_load_individual <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\Maternal Viral Characteristics\\Final Raw Data, figures, and code\\Maternal Virologic Data (Final).xlsx", sheet = 4)
peak_plasma_vRNA_load_individual$Animal_ID = as.character(peak_plasma_vRNA_load_individual$Animal_ID)
peak_plasma_vRNA_load_individual$Animal_ID <- factor(peak_plasma_vRNA_load_individual$Animal_ID, levels = c("044-101", "044-102", "044-103", "044-104", "044-109", "044-110", "044-112", "044-114", "044-116", "044-117", "044-118", "044-122", "044-126", "044-127", "044-130", "044-131", "044-132", "044-133"))
peak_plasma_vRNA_load_individual_figure <-
  ggplot() +
  geom_bar(data = peak_plasma_vRNA_load_individual, aes(x= Animal_ID, y= Peak_Plasma_vRNA_Load_Copies, color = Animal_ID, fill = Animal_ID), stat = 'identity', width = 0.25) + 
  theme_classic() + 
  scale_x_discrete(name = 'Dam ID') + 
  annotation_logticks(base = 10, sides = 'l', scaled = TRUE, short = unit(-0.5, "cm"), mid = unit(-0.5, "cm"), long = unit(-0.5, "cm")) + 
  scale_y_log10(name = "Peak vRNA Load (copies/mL)", breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000), labels = trans_format("log10", math_format(10^.x)), expand = c(0,0), limits = c(1,10^6.1)) +
  scale_color_manual(values = c("#7b0000", "#ad0000", "#FF0000", "#ff5d5d", "#ffa3a3", "#ffd2d2", "#000088", "#3434fb", "#6c6cff", "#9c9cff", "#d0d0ff", "#0ef000", "#c1c000", "#fffe04", "#ffd991", "#c7ad7c", "#6ca7a2", "#9c7a99"),
                     breaks = c("044-101", "044-102", "044-103", "044-104", "044-109", "044-110", "044-112", "044-114", "044-116", "044-117", "044-118", "044-122", "044-126", "044-127", "044-130", "044-131", "044-132", "044-133")) +
  scale_fill_manual(values = c("#7b0000", "#ad0000", "#FF0000", "#ff5d5d", "#ffa3a3", "#ffd2d2", "#000088", "#3434fb", "#6c6cff", "#9c9cff", "#d0d0ff", "#0ef000", "#c1c000", "#fffe04", "#ffd991", "#c7ad7c", "#6ca7a2", "#9c7a99"),
                    breaks = c("044-101", "044-102", "044-103", "044-104", "044-109", "044-110", "044-112", "044-114", "044-116", "044-117", "044-118", "044-122", "044-126", "044-127", "044-130", "044-131", "044-132", "044-133")) +
  theme(axis.title= element_text(size = 20, face = "bold"), axis.text.x = element_text(size = 20, angle = 90, hjust= 1, vjust= 0.5), axis.text= element_text(size = 20)) + 
  theme(aspect.ratio = 1) + guides(color = 'none', fill = 'none')
peak_plasma_vRNA_load_individual_figure

###
# Peak infectious virus titers (individual)
###
peak_plasma_infectious_virus_titer_individual <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\Maternal Viral Characteristics\\Final Raw Data, figures, and code\\Maternal Virologic Data (Final).xlsx", sheet = 5, col_names = TRUE)
peak_plasma_infectious_virus_titer_individual$Animal_ID = as.character(peak_plasma_infectious_virus_titer_individual$Animal_ID)
peak_plasma_infectious_virus_titer_individual$Animal_ID <- factor(peak_plasma_infectious_virus_titer_individual$Animal_ID, levels = c('044-101', '044-102', '044-103', '044-104', '044-109', '044-110', '044-112', '044-114', '044-116', '044-117', '044-118', '044-122', '044-126', '044-127', '044-130', '044-131', '044-132', '044-133'))
peak_plasma_infectious_virus_titer_individual$Peak_Infectious_Virus_Titer_PFU = as.numeric(peak_plasma_infectious_virus_titer_individual$Peak_Infectious_Virus_Titer_PFU)
peak_plasma_infectious_virus_titer_individual_figure <-
  ggplot() +
  geom_bar(data = peak_plasma_infectious_virus_titer_individual, aes(x= Animal_ID, y= Peak_Infectious_Virus_Titer_PFU, color = Animal_ID, fill = Animal_ID), stat = 'identity', width = 0.25) + 
  theme_classic() + 
  scale_x_discrete(name = 'Dam ID') + 
  annotation_logticks(base = 10, sides = 'l', scaled = TRUE, short = unit(-0.5, "cm"), mid = unit(-0.5, "cm"), long = unit(-0.5, "cm")) + 
  scale_y_log10(name = "Peak Infectivity (PFU/mL)", breaks = c(1, 10, 100, 1000), labels = trans_format("log10", math_format(10^.x)), expand = c(0,0), limits = c(1,10^3.2)) +
  scale_color_manual(values = c("#7b0000", "#ad0000", "#FF0000", "#ff5d5d", "#ffa3a3", "#ffd2d2", "#000088", "#3434fb", "#6c6cff", "#9c9cff", "#d0d0ff", "#0ef000", "#c1c000", "#fffe04", "#ffd991", "#c7ad7c", "#6ca7a2", "#9c7a99"),
                     breaks = c("044-101", "044-102", "044-103", "044-104", "044-109", "044-110", "044-112", "044-114", "044-116", "044-117", "044-118", "044-122", "044-126", "044-127", "044-130", "044-131", "044-132", "044-133")) +
  scale_fill_manual(values = c("#7b0000", "#ad0000", "#FF0000", "#ff5d5d", "#ffa3a3", "#ffd2d2", "#000088", "#3434fb", "#6c6cff", "#9c9cff", "#d0d0ff", "#0ef000", "#c1c000", "#fffe04", "#ffd991", "#c7ad7c", "#6ca7a2", "#9c7a99"),
                    breaks = c("044-101", "044-102", "044-103", "044-104", "044-109", "044-110", "044-112", "044-114", "044-116", "044-117", "044-118", "044-122", "044-126", "044-127", "044-130", "044-131", "044-132", "044-133")) +
  theme(axis.title= element_text(size = 20, face = "bold"), axis.text.x = element_text(size = 20, angle = 90, hjust= 1, vjust= 0.5), axis.text.y = element_text(size = 20)) + 
  theme(aspect.ratio = 1) + guides(color= 'none', fill= 'none') +
  geom_text(aes(x=2, y= 1.2, label= 'ND'), size= 3.5) +
  geom_text(aes(x= 3, y= 1.2, label= 'NA'), size= 3.5) + 
  geom_text(aes(x= 4, y= 1.2, label= 'NA'), size= 3.5) +
  geom_text(aes(x= 5, y= 1.2, label= 'ND'), size= 3.5) + 
  geom_text(aes(x= 6, y= 1.2, label= 'ND'), size= 3.5) + 
  geom_text(aes(x= 8, y= 1.2, label = 'ND'), size= 3.5) + 
  geom_text(aes(x= 12, y= 1.2, label = 'NA'), size= 3.5) + 
  geom_text(aes(x= 14, y= 1.2, label= 'ND'), size= 3.5) +
  geom_text(aes(x= 17, y= 1.2, label= 'ND'), size= 3.5)
peak_plasma_infectious_virus_titer_individual_figure

###
# Duration of plasma vRNA burden (individual)
###
plasma_vRNA_burden_duration_individual <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\Maternal Viral Characteristics\\Final Raw Data, figures, and code\\Maternal Virologic Data (Final).xlsx", sheet = 6, col_names = TRUE)
plasma_vRNA_burden_duration_individual$Plasma_vRNA_Burden_Duration_DPI = as.character(plasma_vRNA_burden_duration_individual$Plasma_vRNA_Burden_Duration_DPI)
plasma_vRNA_burden_duration_individual$Plasma_vRNA_Burden_Duration_DPI <- factor(plasma_vRNA_burden_duration_individual$Plasma_vRNA_Burden_Duration_DPI, levels = c(4, 5, 6, 7, 8, 9, 10, 28, 29, 31, 45, 52))
plasma_vRNA_burden_duration_individual_figure <- 
  ggplot(data = plasma_vRNA_burden_duration_individual, aes(x = Plasma_vRNA_Burden_Duration_DPI, y = Number_of_Animals, fill = Animal_ID)) + 
  geom_bar(stat = 'identity', width= 0.25) + 
  theme_classic() + 
  scale_x_discrete(name = "Duration of Plasma vRNA Burden (DPI)") + 
  scale_y_continuous(name = "Number of Animals", expand = c(0,0), limits = c(0, 5.05), breaks = c(1, 2, 3, 4, 5)) + 
  theme(axis.title= element_text(size = 20, face = "bold"), axis.text.x = element_text(size = 20, angle= 90, hjust= 1, vjust= 0.5), axis.text.y = element_text(size= 20)) + 
  theme(aspect.ratio = 1) + 
  scale_fill_manual(values = c("#7b0000", "#ad0000", "#FF0000", "#ff5d5d", "#ffa3a3", "#ffd2d2", "#000088", "#3434fb", "#6c6cff", "#9c9cff", "#d0d0ff", "#0ef000", "#c1c000", "#fffe04", "#ffd991", "#c7ad7c", "#6ca7a2", "#9c7a99"),
                    breaks = c("044-101", "044-102", "044-103", "044-104", "044-109", "044-110", "044-112", "044-114", "044-116", "044-117", "044-118", "044-122", "044-126", "044-127", "044-130", "044-131", "044-132", "044-133")) + 
  guides(fill = 'none')
plasma_vRNA_burden_duration_individual_figure

###
# Duration of plasma infectious virus burden (individual)
###
plasma_infectious_virus_duration_individual <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\Maternal Viral Characteristics\\Final Raw Data, figures, and code\\Maternal Virologic Data (Final).xlsx", sheet = 7, col_names = TRUE)
plasma_infectious_virus_duration_individual$Plasma_Infectious_Virus_Duration_DPI = as.character(plasma_infectious_virus_duration_individual$Plasma_Infectious_Virus_Duration_DPI)
plasma_infectious_virus_duration_individual$Plasma_Infectious_Virus_Duration_DPI <- factor(plasma_infectious_virus_duration_individual$Plasma_Infectious_Virus_Duration_DPI, levels = c(0, 2, 3, 4, 5, 6, 7, 15, NA))
plasma_infectious_virus_duration_individual_figure <-
  ggplot(data= plasma_infectious_virus_duration_individual, aes(x= Plasma_Infectious_Virus_Duration_DPI, y= Number_of_Animals, fill= Animal_ID)) +
  geom_bar(stat= 'identity', width= 0.25) + 
  theme_classic() +
  scale_x_discrete(name= 'Plasma Infectious Virus Duration (DPI)') +
  scale_y_continuous(name= 'Number of Animals', expand= c(0,0), limits= c(0,5), breaks= c(0, 1, 2, 3, 4, 5)) +
  scale_fill_manual(values = c('#7b0000', '#ad0000', '#ff0000', '#ff5d5d', '#ffa3a3', '#ffd2d2', '#000088', '#3434fb', '#6c6cff', '#9c9cff', '#d0d0ff', '#0ef000', '#c1c000', '#fffe04', '#ffd991', '#c7ad7c', '#6ca7a2', '#9c7a99'),
                    breaks = c('044-101', '044-102', '044-103', '044-104', '044-109', '044-110', '044-112', '044-114', '044-116', '044-117', '044-118', '044-122', '044-126', '044-127', '044-130', '044-131', '044-132', '044-133')) + 
  theme(axis.title= element_text(size= 20, face = 'bold'), axis.text = element_text(size= 20), aspect.ratio = 1) + 
  guides(fill = 'none')
plasma_infectious_virus_duration_individual_figure

###
# Maternal-Fetal Interface (MFI) vRNA burden (individual)
###
mfi_vRNA_burden_individual <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\Maternal Viral Characteristics\\Final Raw Data, figures, and code\\Maternal Virologic Data (Final).xlsx", sheet = 8)
mfi_vRNA_burden_individual$Percent_vRNA_Positive_Cotyledons = as.numeric(mfi_vRNA_burden_individual$Percent_vRNA_Positive_Cotyledons)
mfi_vRNA_burden_individual$Animal_ID = as.character(mfi_vRNA_burden_individual$Animal_ID)
mfi_vRNA_burden_individual$Animal_ID <- factor(mfi_vRNA_burden_individual$Animal_ID, levels = c("044-101", "044-102", "044-103", "044-104", "044-109", "044-110", "044-112", "044-114", "044-116", "044-117", "044-118", "044-122", "044-126", "044-127", "044-130", "044-131", "044-132", "044-133"))
mfi_vRNA_burden_individual$Maternal_Fetal_Interface_Tissue = as.character(mfi_vRNA_burden_individual$Maternal_Fetal_Interface_Tissue)
mfi_vRNA_burden_individual$Maternal_Fetal_Interface_Tissue <- factor(mfi_vRNA_burden_individual$Maternal_Fetal_Interface_Tissue, levels = c('Placenta', 'Decidua', 'Chorionic Plate', 'Total'))
mfi_vRNA_burden_individual_figure <-
  ggplot() + 
  geom_point(data= mfi_vRNA_burden_individual, aes(x= Animal_ID, y= Percent_vRNA_Positive_Cotyledons, color = Animal_ID, shape= Maternal_Fetal_Interface_Tissue, stroke= 1), size = 4) + 
  theme_classic() +
  scale_x_discrete(name= 'Dam ID') +
  geom_text(aes(x= 5, y= 0, label= 'NT'), size= 3.5) +
  geom_text(aes(x= 16, y= 0, label = 'NT'), size= 3.5) +
  scale_y_continuous(name = "Cotyledons vRNA+ (%)", expand = c(0,0), limits = c(-1,51), breaks = c(0, 10, 20, 30, 40, 50)) + 
  scale_color_manual(values = c("#7b0000", "#ad0000", "#FF0000", "#ff5d5d", "#ffa3a3", "#ffd2d2", "#000088", "#3434fb", "#6c6cff", "#9c9cff", "#d0d0ff", "#0ef000", "#c1c000", "#fffe04", "#ffd991", "#c7ad7c", "#6ca7a2", "#9c7a99"),
                     breaks = c("044-101", "044-102", "044-103", "044-104", "044-109", "044-110", "044-112", "044-114", "044-116", "044-117", "044-118", "044-122", "044-126", "044-127", "044-130", "044-131", "044-132", "044-133")) +
  scale_shape_manual(values= c(0, 1, 2, 3),
                     breaks= c('Placenta', 'Decidua', 'Chorionic Plate', 'Total')) +
  theme(axis.title= element_text(size = 20, face = "bold"), axis.text.x = element_text(size = 20, angle = 90, hjust= 1, vjust= 0.5), axis.text.y = element_text(size = 20), legend.title = element_text(size= 20, face = 'bold'), legend.text = element_text(size = 20)) + 
  theme(aspect.ratio = 1) + 
  guides(color = 'none', shape= guide_legend('MFI Tissue'))
mfi_vRNA_burden_individual_figure

###
# combine figures into a single multipanel figure
###
tiff('Maternal Virologic Data-Individual (Final).tiff', units = 'in', width= 12, height = 24, res= 300)
ggarrange(plasma_vRNA_loads_individual_figure, AUC_individual_figure, peak_plasma_vRNA_timing_figure, peak_plasma_vRNA_load_individual_figure, peak_plasma_infectious_virus_titer_individual_figure, plasma_vRNA_burden_duration_individual_figure, plasma_infectious_virus_duration_individual_figure, mfi_vRNA_burden_individual_figure, labels = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'), nrow= 4, ncol= 2, common.legend = TRUE, legend = 'top', align = 'hv')
dev.off()
