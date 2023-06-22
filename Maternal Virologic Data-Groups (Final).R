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
# Duration of plasma vRNA burden (based on virologic control group)
### 
plasma_vRNA_burden_duration_group <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\Maternal Viral Characteristics\\Maternal Virologic Data (Final).xlsx", sheet = 7, col_names = TRUE)
plasma_vRNA_burden_duration_group$Plasma_vRNA_Burden_Duration_DPI = as.numeric(plasma_vRNA_burden_duration_group$Plasma_vRNA_Burden_Duration_DPI)
plasma_vRNA_burden_duration_group$Virologic_Control_Group <- factor(plasma_vRNA_burden_duration_group$Virologic_Control_Group, levels = c("Controller", "Non-Controller"))
plasma_vRNA_burden_duration_group_figure <- ggplot() + 
  geom_boxplot(data= plasma_vRNA_burden_duration_group, aes(x= Virologic_Control_Group, y= Plasma_vRNA_Burden_Duration_DPI, color = Virologic_Control_Group), fill = "white") + 
  geom_point(data = plasma_vRNA_burden_duration_group, aes(x= Virologic_Control_Group, y= Plasma_vRNA_Burden_Duration_DPI, color= Virologic_Control_Group), position = position_jitterdodge(seed= 123, jitter.width = 0.75, jitter.height = 0), size = 2.5) + 
  theme_classic() + 
  scale_x_discrete((name = "Virologic Control Group")) + 
  scale_y_continuous(name = "Duration of Plasma vRNA Burden (DPI)", expand = c(0,0), limits = c(0, 60), breaks = c(0, 10, 20, 30, 40, 50, 60)) + 
  theme(axis.title = element_text(size = 20, face = "bold"), axis.text = element_text(size = 20), legend.title = element_text(face= 'bold', size = 20), legend.text = element_text(size= 20), legend.position = 'top') + 
  theme(aspect.ratio = 1) + 
  scale_color_manual(values = c("#000000", "#b5b5b5" ), breaks = c("Controller", "Non-Controller")) + 
  guides(color = guide_legend("Virologic Control Group")) + geom_line(data = tibble(x = c(1,2), y = c(56, 56)), aes(x=x, y=y), inherit.aes = FALSE) + geom_line(data = tibble(x = c(1,1), y = c(56, 54)), aes(x=x, y=y), inherit.aes = FALSE) + geom_line(data = tibble(x = c(2,2), y = c(56, 54)), aes(x=x, y=y), inherit.aes = FALSE) + geom_text(data = tibble(x = c(1.5, 1.5), y = c(58, 58)), aes(x=x, y=y, label = "***"), inherit.aes = FALSE, size = 8)
plasma_vRNA_burden_duration_group_figure
# Statistical analyses for duration of plasma vRNA burden (Controllers vs.Non-Controllers)- Mann Whitney U-Test (Wilcox test)
plasma_vRNA_burden_duration_group_controller <- filter(plasma_vRNA_burden_duration_group, Virologic_Control_Group == 'Controller')
plasma_vRNA_burden_duration_group_non_controller <- filter(plasma_vRNA_burden_duration_group, Virologic_Control_Group == 'Non-Controller')
plasma_vRNA_burden_duration_group_controller_quantile <- quantile(plasma_vRNA_burden_duration_group_controller$Plasma_vRNA_Burden_Duration_DPI, probs = c(0.25, 0.75))
plasma_vRNA_burden_duration_group_controller_quantile
plasma_vRNA_burden_duration_group_non_controller_quantile <- quantile(plasma_vRNA_burden_duration_group_non_controller$Plasma_vRNA_Burden_Duration_DPI, probs = c(0.25, 0.75))
plasma_vRNA_burden_duration_group_non_controller_quantile
summary_stats <- plasma_vRNA_burden_duration_group %>%
  group_by(Virologic_Control_Group) %>% 
  summarise(median = median(Plasma_vRNA_Burden_Duration_DPI), IQR= IQR(Plasma_vRNA_Burden_Duration_DPI), count= n())
wilcox.test(Plasma_vRNA_Burden_Duration_DPI~Virologic_Control_Group, data = plasma_vRNA_burden_duration_group, exact = FALSE)

###
# Plasma infectious virus duration (by virologic control group)
###
plasma_infectious_virus_duration_group <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\Maternal Viral Characteristics\\Maternal Virologic Data (Final).xlsx", sheet = 6, col_names = TRUE)
plasma_infectious_virus_duration_group$Plasma_Infectious_Virus_Duration_DPI = as.numeric(plasma_infectious_virus_duration_group$Plasma_Infectious_Virus_Duration_DPI)
plasma_infectious_virus_duration_group$Virologic_Control_Group <- factor(plasma_infectious_virus_duration_group$Virologic_Control_Group, levels = c("Controller", "Non-Controller"))
plasma_infectious_virus_duration_group_figure <- ggplot() + 
  geom_boxplot(data= plasma_infectious_virus_duration_group, aes(x= Virologic_Control_Group, y= Plasma_Infectious_Virus_Duration_DPI, color = Virologic_Control_Group), fill = "white", outlier.colour = 'white') + 
  geom_point(data = plasma_infectious_virus_duration_group, aes(x= Virologic_Control_Group, y= Plasma_Infectious_Virus_Duration_DPI, color= Virologic_Control_Group), position = position_jitterdodge(seed= 123, jitter.width = 0.75, jitter.height = 0), size = 2.5) + 
  theme_classic() + 
  scale_x_discrete((name = "Virologic Control Group")) + 
  scale_y_continuous(name = "Plasma Infectious Virus Duration (DPI)", expand = c(0,0), limits = c(-1, 16), breaks = c(0, 5, 10, 15)) + 
  theme(axis.title= element_text(size = 20, face = "bold"), axis.text = element_text(size = 20)) + 
  theme(aspect.ratio = 1) + 
  scale_color_manual(values = c("#000000", "#b5b5b5" ), breaks = c("Controller", "Non-Controller")) + 
  guides(color = 'none')
plasma_infectious_virus_duration_group_figure

# Statistical analyses for plasma infectious virus duration (Controllers vs.Non-Controllers)- Mann Whitney U-Test (Wilcox test)
plasma_infectious_virus_duration_group_controller <- filter(plasma_infectious_virus_duration_group, Virologic_Control_Group == 'Controller')
plasma_infectious_virus_duration_group_non_controller <- filter(plasma_infectious_virus_duration_group, Virologic_Control_Group == 'Non-Controller')
plasma_infectious_virus_duration_group_controller_quantile <- quantile(plasma_infectious_virus_duration_group_controller$Plasma_Infectious_Virus_Duration_DPI, probs = c(0.25, 0.75))
plasma_infectious_virus_duration_group_controller_quantile
plasma_infectious_virus_duration_group_non_controller_quantile <- quantile(plasma_infectious_virus_duration_group_non_controller$Plasma_Infectious_Virus_Duration_DPI, probs = c(0.25, 0.75), na.rm = TRUE)
plasma_infectious_virus_duration_group_non_controller_quantile
summary_stats <- plasma_infectious_virus_duration_group_controller %>%
  group_by(Virologic_Control_Group) %>% 
  summarise(median = median(Plasma_Infectious_Virus_Duration_DPI, na.rm = TRUE), IQR= IQR(Plasma_Infectious_Virus_Duration_DPI, na.rm = TRUE), count= n())
wilcox.test(Plasma_Infectious_Virus_Duration_DPI~Virologic_Control_Group, data = plasma_infectious_virus_duration_group, exact = FALSE)

###
# Peak plasma vRNA loads (by virologic control group)
### 
peak_plasma_vRNA_group <-  read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\Maternal Viral Characteristics\\Maternal Virologic Data (Final).xlsx", sheet = 4, col_names = TRUE)
peak_plasma_vRNA_group$Virologic_Control_Group = as.character(peak_plasma_vRNA_group$Virologic_Control_Group)
peak_plasma_vRNA_group$Virologic_Control_Group <- factor(peak_plasma_vRNA_group$Virologic_Control_Group, levels = c("Controller", "Non-Controller"))
peak_plasma_vRNA_group_figure <- 
  ggplot() + 
  geom_boxplot(data= peak_plasma_vRNA_group, aes(x= Virologic_Control_Group, y= Peak_Plasma_vRNA_Load_Copies, color = Virologic_Control_Group), fill = "white", outlier.color = "white") + 
  geom_point(data= peak_plasma_vRNA_group, aes(x= Virologic_Control_Group, y= Peak_Plasma_vRNA_Load_Copies, color = Virologic_Control_Group), position = position_jitterdodge(seed = 123, jitter.width = 0.75), size = 2.5) + 
  theme_classic() + 
  scale_x_discrete(name = "Virologic Control Group") + 
  annotation_logticks(base = 10, sides = 'l', scaled = TRUE, short = unit(-0.5, "cm"), mid = unit(-0.5, "cm"), long = unit(-0.5, "cm")) + 
  scale_y_log10(name = "Peak vRNA Load (Copies/mL)", breaks = c(1000, 10000, 100000, 1000000), labels = trans_format("log10", math_format(10^.x)), expand = c(0,0), limits = c(1e3, 10^6.5)) + 
  theme(axis.title= element_text(size = 20, face = "bold"), axis.text = element_text(size = 20)) + 
  theme(aspect.ratio = 1) + scale_fill_manual(values = c("#000000", "#b5b5b5"), breaks = c("Controller", "Non-Controller")) + scale_color_manual(values = c("#000000", "#b5b5b5"), breaks = c("Controller", "Non-Controller")) + 
  guides(color = 'none', fill = 'none') + geom_line(data = tibble(x = c(1,2), y = c(2000000, 2000000)), aes(x=x, y=y), inherit.aes = FALSE) + geom_line(data = tibble(x = c(1,1), y = c(2000000, 1500000)), aes(x=x, y=y), inherit.aes = FALSE) + geom_line(data = tibble(x = c(2,2), y = c(2000000, 1500000)), aes(x=x, y=y), inherit.aes = FALSE) + geom_text(data = tibble(x = c(1.5, 1.5), y = c(2300000, 2300000)), aes(x=x, y=y, label = "**"), inherit.aes = FALSE, size = 8)
peak_plasma_vRNA_group_figure

# Statistical analyses for peak plasma vRNA load (Controllers vs.Non-Controllers)- Mann Whitney U-Test (Wilcox test)
peak_plasma_vRNA_group_controller <- filter(peak_plasma_vRNA_group, Virologic_Control_Group == 'Controller')
peak_plasma_vRNA_group_non_controller <- filter(peak_plasma_vRNA_group, Virologic_Control_Group == 'Non-Controller')
peak_plasma_vRNA_group_controller_quantile <- quantile(peak_plasma_vRNA_group_controller$Peak_Plasma_vRNA_Load_Copies, probs= c(0.25, 0.75))
peak_plasma_vRNA_group_controller_quantile
peak_plasma_vRNA_group_non_controller_quantile <- quantile(peak_plasma_vRNA_group_non_controller$Peak_Plasma_vRNA_Load_Copies, probs= c(0.25, 0.75))
peak_plasma_vRNA_group_non_controller_quantile
summary_stats <-
  peak_plasma_vRNA_group %>%
  group_by(Virologic_Control_Group) %>%
  summarize(median= median(Peak_Plasma_vRNA_Load_Copies), IQR= IQR(Peak_Plasma_vRNA_Load_Copies), count= n())
wilcox.test(Peak_Plasma_vRNA_Load_Copies~Virologic_Control_Group, data = peak_plasma_vRNA_group, exact = FALSE)

###
# vRNA-to-Infectious virus ratios (by virologic control group)
###
vRNA_infectivity_ratio_group <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\Maternal Viral Characteristics\\Maternal Virologic Data (Final).xlsx", sheet = 5, col_names = TRUE)
vRNA_infectivity_ratio_group$Peak_vRNA_to_Infectious_Virus_Ratio = as.numeric(vRNA_infectivity_ratio_group$Peak_vRNA_to_Infectious_Virus_Ratio)
vRNA_infectivity_ratio_group$Virologic_Control_Group <- factor(vRNA_infectivity_ratio_group$Virologic_Control_Group, levels = c("Controller", "Non-Controller"))
vRNA_infectivity_ratio_group_figure <- ggplot() + 
  geom_boxplot(data= vRNA_infectivity_ratio_group, aes(x= Virologic_Control_Group, y= Peak_vRNA_to_Infectious_Virus_Ratio, color = Virologic_Control_Group), fill = "white", outlier.colour = 'white') + 
  geom_point(data = vRNA_infectivity_ratio_group, aes(x= Virologic_Control_Group, y= Peak_vRNA_to_Infectious_Virus_Ratio, color= Virologic_Control_Group), position = position_jitterdodge(seed= 123, jitter.width = 0.75, jitter.height = 0), size = 2.5) + 
  theme_classic() + 
  scale_x_discrete((name = "Virologic Control Group")) + 
  scale_y_continuous(name = "vRNA:Infectious Virus Ratio", expand = c(0,0), limits = c(-100, 2500), breaks = c(0, 500, 1000, 1500, 2000, 2500)) + 
  theme(axis.title= element_text(size = 20, face = "bold"), axis.text = element_text(size = 20)) + 
  theme(aspect.ratio = 1) + 
  scale_color_manual(values = c("#000000", "#b5b5b5" ), breaks = c("Controller", "Non-Controller")) + 
  guides(color = 'none')
vRNA_infectivity_ratio_group_figure

# Statistical analyses for peak vRNA to infectious virus ratio (Controllers vs.Non-Controllers)- Mann Whitney U-Test (Wilcox test)
vRNA_infectivity_ratio_group_controller <- filter(vRNA_infectivity_ratio_group, Virologic_Control_Group == 'Controller')
vRNA_infectivity_ratio_group_non_controller <- filter(vRNA_infectivity_ratio_group, Virologic_Control_Group == 'Non-Controller')
vRNA_infectivity_ratio_group_controller_quantile <- quantile(vRNA_infectivity_ratio_group_controller$Peak_vRNA_to_Infectious_Virus_Ratio, probs = c(0.25, 0.75), na.rm = TRUE)
vRNA_infectivity_ratio_group_controller_quantile
vRNA_infectivity_ratio_group_non_controller_quantile <- quantile(vRNA_infectivity_ratio_group_non_controller$Peak_vRNA_to_Infectious_Virus_Ratio, probs = c(0.25, 0.75), na.rm = TRUE)
vRNA_infectivity_ratio_group_non_controller_quantile
summary_stats <- vRNA_infectivity_ratio_group %>%
  group_by(Virologic_Control_Group) %>% 
  summarise(median = median(Peak_vRNA_to_Infectious_Virus_Ratio, na.rm = TRUE), IQR= IQR(Peak_vRNA_to_Infectious_Virus_Ratio, na.rm = TRUE), count= n())
wilcox.test(Peak_vRNA_to_Infectious_Virus_Ratio~Virologic_Control_Group, data = vRNA_infectivity_ratio_group, exact = FALSE)

###
# Area under the curve values (by virologic control group)
###
AUC_group <-  read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\Maternal Viral Characteristics\\Maternal Virologic Data (Final).xlsx", sheet = 2, col_names = TRUE)
AUC_group$Virologic_Control_Group = as.character(AUC_group$Virologic_Control_Group)
AUC_group$Virologic_Control_Group <- factor(AUC_group$Virologic_Control_Group, levels = c("Controller", "Non-Controller"))
AUC_group_figure <- 
  ggplot() +
  geom_boxplot(data= AUC_group, aes(x= Virologic_Control_Group, y= Area_Under_Curve, color = Virologic_Control_Group), fill = "white", outlier.color = "white") + 
  geom_point(data= AUC_group, aes(x= Virologic_Control_Group, y= Area_Under_Curve, color = Virologic_Control_Group), position = position_jitterdodge(seed = 123, jitter.width = 0.75), size = 2.5) + 
  theme_classic() + 
  scale_x_discrete(name = "Virologic Control Group") + 
  annotation_logticks(base = 10, sides = 'l', scaled = TRUE, short = unit(-0.5, "cm"), mid = unit(-0.5, "cm"), long = unit(-0.5, "cm")) + 
  scale_y_log10(name = "AUC", breaks = c(1000, 10000, 100000, 1000000), labels = trans_format("log10", math_format(10^.x)), expand = c(0,0), limits = c(1e3, 10^6.5)) + 
  theme(axis.title= element_text(size = 20, face = "bold"), axis.text = element_text(size = 20)) + 
  theme(aspect.ratio = 1) + scale_fill_manual(values = c("#000000", "#b5b5b5"), breaks = c("Controller", "Non-Controller")) + scale_color_manual(values = c("#000000", "#b5b5b5"), breaks = c("Controller", "Non-Controller")) + 
  guides(color = 'none', fill = 'none') + geom_line(data = tibble(x = c(1,2), y = c(2000000, 2000000)), aes(x=x, y=y), inherit.aes = FALSE) + geom_line(data = tibble(x = c(1,1), y = c(2000000, 1500000)), aes(x=x, y=y), inherit.aes = FALSE) + geom_line(data = tibble(x = c(2,2), y = c(2000000, 1500000)), aes(x=x, y=y), inherit.aes = FALSE) + geom_text(data = tibble(x = c(1.5, 1.5), y = c(2300000, 2300000)), aes(x=x, y=y, label = "**"), inherit.aes = FALSE, size = 8)
AUC_group_figure

# Statistical analyses for AUC values (Controllers vs.Non-Controllers)- Mann Whitney U-Test (Wilcox test)
AUC_group_controller <- filter(AUC_group, Virologic_Control_Group == 'Controller')
AUC_group_non_controller <- filter(AUC_group, Virologic_Control_Group == 'Non-Controller')
AUC_group_controller_quantile <- quantile(AUC_group_controller$Area_Under_Curve, probs = c(0.25, 0.75), na.rm = TRUE)
AUC_group_controller_quantile
AUC_group_non_controller_quantile <- quantile(AUC_group_non_controller$Area_Under_Curve, probs = c(0.25, 0.75), na.rm = TRUE)
AUC_group_non_controller_quantile
summary_stats <- AUC_group %>%
  group_by(Virologic_Control_Group) %>% 
  summarise(median = median(Area_Under_Curve, na.rm = TRUE), IQR= IQR(Area_Under_Curve, na.rm = TRUE), count= n())
wilcox.test(Area_Under_Curve~Virologic_Control_Group, data = AUC_group, exact = FALSE)

###
# Maternal Fetal Interface (MFI) vRNA burden (by virologic control group)
###
mfi_vRNA_burden_group <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\Maternal Viral Characteristics\\Maternal Virologic Data (Final).xlsx", sheet = 8, col_names = TRUE)
mfi_vRNA_burden_group$Percent_vRNA_Positive_Cotyledons = as.numeric(mfi_vRNA_burden_group$Percent_vRNA_Positive_Cotyledons)
mfi_vRNA_burden_group$Virologic_Control_Group = as.character(mfi_vRNA_burden_group$Virologic_Control_Group)
mfi_vRNA_burden_group$Virologic_Control_Group <- factor(mfi_vRNA_burden_group$Virologic_Control_Group, levels = c("Controller", "Non-Controller"))
mfi_vRNA_burden_group_figure <- 
  ggplot(data = mfi_vRNA_burden_group, aes(x = Maternal_Fetal_Interface_Tissue, y = Percent_vRNA_Positive_Cotyledons)) + 
  geom_boxplot(fill = "white", outlier.color = "white", aes(color = Virologic_Control_Group)) + 
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.5, aes(color = Virologic_Control_Group, fill = Virologic_Control_Group), position = position_dodge(0.75)) + 
  theme_classic() + scale_y_continuous(name = "Cotyledons vRNA+ (% Positive)", limits = c(0, 60), breaks = c(0, 15, 30, 45, 60)) + 
  scale_x_discrete(name = "Maternal-Fetal Interface Tissue") + 
  scale_fill_manual(values = c("#000000", "#b5b5b5"), breaks = c("Controller", "Non-Controller")) + 
  scale_color_manual(values = c("#000000", "#b5b5b5"), breaks = c("Controller", "Non-Controller")) + 
  guides(color = 'none', fill = 'none') + 
  theme(axis.title= element_text(size = 20, face = "bold"), axis.text.x = element_text(size= 20, angle= 45, hjust= 1), axis.text.y = element_text(size = 20), aspect.ratio = 1) + 
  geom_vline(xintercept = 3.5, linetype = "dashed", linecolor = "black") + 
  geom_segment(aes(x= 1.75, xend= 2.25, y= 55, yend= 55), color = 'black') +
  geom_segment(aes(x= 1.75, xend= 1.75, y= 55, yend= 54), color = 'black') +
  geom_segment(aes(x= 2.25, xend= 2.25, y= 55, yend= 54), color = 'black') +
  geom_text(aes(x=2, y= 56, label = '**'), size= 6) + 
  geom_segment(aes(x= 3.75, xend= 4.25, y= 55, yend= 55), color = 'black') +
  geom_segment(aes(x= 3.75, xend= 3.75, y= 55, yend= 54), color = 'black') +
  geom_segment(aes(x= 4.25, xend= 4.25, y= 55, yend= 54), color = 'black') +
  geom_text(aes(x=4, y= 56, label = '**'), size= 6)
mfi_vRNA_burden_group_figure

# Statistical analyses for MFI vRNA burden (Controllers vs.Non-Controllers)- Mann Whitney U-Test (Wilcox test)
mfi_vRNA_burden_group_controller <- filter(mfi_vRNA_burden_group, Virologic_Control_Group == "Controller")
mfi_vRNA_burden_group_controller_chorionic <- filter(mfi_vRNA_burden_group_controller, Maternal_Fetal_Interface_Tissue == 'Chorionic Plate')
mfi_vRNA_burden_group_controller_chorionic_quantile <- quantile(mfi_vRNA_burden_group_controller_chorionic$Percent_vRNA_Positive_Cotyledons, probs = c(0.25, 0.75), na.rm = TRUE)
mfi_vRNA_burden_group_controller_chorionic_quantile
mfi_vRNA_burden_group_controller_decidua <- filter(mfi_vRNA_burden_group_controller, Maternal_Fetal_Interface_Tissue == 'Decidua')
mfi_vRNA_burden_group_controller_decidua_quantile <- quantile(mfi_vRNA_burden_group_controller_decidua$Percent_vRNA_Positive_Cotyledons, probs = c(0.25, 0.75), na.rm = TRUE)
mfi_vRNA_burden_group_controller_decidua_quantile
mfi_vRNA_burden_group_controller_placenta <- filter(mfi_vRNA_burden_group_controller, Maternal_Fetal_Interface_Tissue == 'Placenta')
mfi_vRNA_burden_group_controller_placenta_quantile <- quantile(mfi_vRNA_burden_group_controller_placenta$Percent_vRNA_Positive_Cotyledons, probs = c(0.25, 0.75), na.rm = TRUE)
mfi_vRNA_burden_group_controller_placenta_quantile
mfi_vRNA_burden_group_controller_total <- filter(mfi_vRNA_burden_group_controller, Maternal_Fetal_Interface_Tissue == 'Total')
mfi_vRNA_burden_group_controller_total_quantile <- quantile(mfi_vRNA_burden_group_controller_total$Percent_vRNA_Positive_Cotyledons, probs = c(0.25, 0.75), na.rm = TRUE)
mfi_vRNA_burden_group_controller_total_quantile
mfi_vRNA_burden_group_noncontroller <- filter(mfi_vRNA_burden_group, Virologic_Control_Group == 'Non-Controller')
mfi_vRNA_burden_group_noncontroller_chorionic <- filter(mfi_vRNA_burden_group_noncontroller, Maternal_Fetal_Interface_Tissue == 'Chorionic Plate')
mfi_vRNA_burden_group_noncontroller_chorionic_quantile <- quantile(mfi_vRNA_burden_group_noncontroller_chorionic$Percent_vRNA_Positive_Cotyledons, probs = c(0.25, 0.75), na.rm = TRUE)
mfi_vRNA_burden_group_noncontroller_chorionic_quantile
mfi_vRNA_burden_group_noncontroller_decidua <- filter(mfi_vRNA_burden_group_noncontroller, Maternal_Fetal_Interface_Tissue == 'Decidua')
mfi_vRNA_burden_group_noncontroller_decidua_quantile <- quantile(mfi_vRNA_burden_group_noncontroller_decidua$Percent_vRNA_Positive_Cotyledons, probs = c(0.25, 0.75), na.rm = TRUE)
mfi_vRNA_burden_group_noncontroller_decidua_quantile
mfi_vRNA_burden_group_noncontroller_placenta <- filter(mfi_vRNA_burden_group_noncontroller, Maternal_Fetal_Interface_Tissue == 'Placenta')
mfi_vRNA_burden_group_noncontroller_placenta_quantile <- quantile(mfi_vRNA_burden_group_noncontroller_placenta$Percent_vRNA_Positive_Cotyledons, probs = c(0.25, 0.75), na.rm = TRUE)
mfi_vRNA_burden_group_noncontroller_placenta_quantile
mfi_vRNA_burden_group_noncontroller_total <- filter(mfi_vRNA_burden_group_noncontroller, Maternal_Fetal_Interface_Tissue == 'Total')
mfi_vRNA_burden_group_noncontroller_total_quantile <- quantile(mfi_vRNA_burden_group_noncontroller_total$Percent_vRNA_Positive_Cotyledons, probs = c(0.25, 0.75), na.rm = TRUE)
mfi_vRNA_burden_group_noncontroller_total_quantile
summary_stats <-
  mfi_vRNA_burden_group %>%
  group_by(Virologic_Control_Group, Maternal_Fetal_Interface_Tissue) %>%
  summarize(median = median(Percent_vRNA_Positive_Cotyledons, na.rm = TRUE), IQR = IQR(Percent_vRNA_Positive_Cotyledons, na.rm = TRUE), count = n())
chorionic_plate <- filter(mfi_vRNA_burden_group, Maternal_Fetal_Interface_Tissue == 'Chorionic Plate')
wilcox.test(Percent_vRNA_Positive_Cotyledons~Virologic_Control_Group, data = chorionic_plate, exact = FALSE, na.rm = TRUE)
decidua <- filter(mfi_vRNA_burden_group, Maternal_Fetal_Interface_Tissue == 'Decidua')
wilcox.test(Percent_vRNA_Positive_Cotyledons~Virologic_Control_Group, data = decidua, exact = FALSE, na.rm = TRUE)
placenta <- filter(mfi_vRNA_burden_group, Maternal_Fetal_Interface_Tissue == 'Placenta')
wilcox.test(Percent_vRNA_Positive_Cotyledons~Virologic_Control_Group, data = placenta, exact = FALSE, na.rm = TRUE)
total <- filter(mfi_vRNA_burden_group, Maternal_Fetal_Interface_Tissue == 'Total')
wilcox.test(Percent_vRNA_Positive_Cotyledons~Virologic_Control_Group, data = total, exact = FALSE, na.rm = TRUE)

###
# Combine figures into a multipanel figure with labels
###
tiff("Maternal Virologic Data-Groups (Final).tiff", units = 'in', width = 18, height = 16, res = 300)
ggarrange(plasma_vRNA_burden_duration_group_figure, plasma_infectious_virus_duration_group_figure, peak_plasma_vRNA_group_figure, vRNA_infectivity_ratio_group_figure, AUC_group_figure, mfi_vRNA_burden_group_figure, labels= c('A', 'B', 'C', 'D', 'E', 'F'), nrow= 2, ncol = 3, align= 'hv', common.legend = TRUE, legend = 'top')
dev.off()
