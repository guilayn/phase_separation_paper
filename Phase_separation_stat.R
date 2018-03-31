################.################
#### PHASE SEPARATION STATS ####
################.################

#clean variables ###################
rm(list=ls(all=TRUE)) 

#### 1.PREP ####
#1.1 loading libraries####
libraries=c("Hmisc","colorspace","corrplot","pvclust","mixOmics","RColorBrewer",
            "FactoMineR","missMDA","FactoMineR","ggplot2", "tidyr",
            "gplots","scales","grid","plyr","gridExtra",
            "devtools","factoextra","ggrepel", "viridis",
            "WriteXLS","dendextend","cluster","reshape2","readxl","pvclust","pls",
            "sjstats")

install_this=libraries[which(lapply(libraries, require, character.only = TRUE) == F)]
install.packages(install_this)

###1.2 ADJUST THE FOLDER###############################################################################################################################

data_wd="C:/Users/JT5363/OneDrive - Suez Environnement/THESE-FELIPE/01_DIGESTATE_CHARAC/"
file_name="2017_11 Phase separation compilation.xlsx"
wd="C:/Users/JT5363/OneDrive - Suez Environnement/THESE-FELIPE/01_DIGESTATE_CHARAC/2018_01 Phase separation stats"
setwd(wd)

dir.create(paste0(wd,"/Initial_Boxplots"))
dir.create(paste0(wd,"/Mass_distribution"))
dir.create(paste0(wd,"/Efficiency_histogram"))
dir.create(paste0(wd,"/Efficiency_ttest"))
dir.create(paste0(wd,"/Corr_matrix"))
#dir.create(paste0(wd,"/HCA"))
dir.create(paste0(wd,"/PCA"))
dir.create(paste0(wd,"/PLS_EFF"))
dir.create(paste0(wd,"/Linear_corr"))
dir.create(paste0(wd,"/New_corr"))
dir.create(paste0(wd,"/ERRORS"))
dir.create(paste0(wd,"/Validation"))


#### 1.3 Importing data ####
data_sep_raw = read_excel(path = paste0(data_wd,file_name),
                      sheet = "All data",
                      col_names = T,
                      na = "NA",
                      skip=2)

data_distrib_clean = read_excel(path = paste0(data_wd,file_name),
                      sheet = "Clean_m_distrib",
                      col_names = T,
                      na = "NA",
                      range="D1:S15")
data_distrib_clean_low = read_excel(path = paste0(data_wd,file_name),
                                sheet = "Low_m_distrib",
                                col_names = T,
                                na = "NA",
                                range="D1:S15")
data_distrib_clean_high = read_excel(path = paste0(data_wd,file_name),
                                sheet = "High_m_distrib",
                                col_names = T,
                                na = "NA",
                                range="D1:S15") ##### No balance of Mn

data_PCA_short = read_excel(path = paste0(data_wd,file_name),
                            sheet = "PCA_equip_data",
                            col_names = T,
                            na = "NA")
data_PCA_extra_raw_FM = read_excel(path = paste0(data_wd,file_name),
                               sheet = "PCA_extra_raw_FM",
                               col_names = T,
                               na = "NA")
                              # n_max = 43) ### NOT USING AFIFI's incomplete data
data_PCA_extra_raw_DM = read_excel(path = paste0(data_wd,file_name),
                               sheet = "PCA_extra_raw_DM",
                               col_names = T,
                               na = "NA")
                              # n_max = 43)
data_PCA_extra_all_FM = read_excel(path = paste0(data_wd,file_name),
                            sheet = "PCA_extra_all_FM",
                            col_names = T,
                            na = "NA")
                           # n_max = 43) ### NOT USING AFIFI's incomplete data
data_PCA_extra_all_DM = read_excel(path = paste0(data_wd,file_name),
                               sheet = "PCA_extra_all_DM",
                               col_names = T,
                               na = "NA")
                               #n_max = 43) 
data_PLS = read_excel(path = paste0(data_wd,file_name),
                                   sheet = "PLS_data",
                                   col_names = T,
                                   na = "NA")
                                   #n_max = 43) 
data_PLS_polymer = read_excel(path = paste0(data_wd,file_name),
                      sheet = "PLS_polymer",
                      col_names = T,
                      na = "NA")
data_equip_group = read_excel(path = paste0(data_wd,file_name),
                                     sheet = "Group_equip",
                                     col_names = T,
                                     na = "NA",
                                     range="A1:B65")
data_feed_group = read_excel(path = paste0(data_wd,file_name),
                              sheet = "Group_feed",
                              col_names = T,
                              na = "NA",
                              range="B1:C65")
data_errors = read_excel(path = paste0(data_wd,file_name),
                             sheet = "ERRORS",
                             col_names = T,
                             na = "NA",
                             range="A3:N67")

data_efficiency = read_excel(path = paste0(data_wd,file_name),
                         sheet = "Efficiency_data",
                         col_names = T,
                         na = "NA",
                         range="A1:Z65")

data_validation = read_excel(path = paste0(data_wd,file_name),
                             sheet = "Validation",
                             col_names = T,
                             na = "NA",
                             range="A1:I249")

list_distrib_data = list(All = data_distrib_clean,
                         Low = data_distrib_clean_low,
                         High = data_distrib_clean_high)


list_PCA_data = list(PCA_efficiency=data_efficiency)
  #PCA_short=data_PCA_short,
                     #PCA_extra_FM=data_PCA_extra_raw_FM,
                     #PCA_extra_DM=data_PCA_extra_raw_DM,
                     #PCA_extra_all_FM=data_PCA_extra_all_FM,
                      #PCA_extra_all_DM=data_PCA_extra_all_DM)
#### 1.3.1 check that ids are different ####
table(colnames(data_sep_raw))
table(colnames(data_distrib_clean))

#### 1.3.2 check class of columns ####
which(lapply(data_sep_raw, class) != "numeric")
#### 1.3.3 force into given class ####
#selec_cols = c(x,x,x,x)
#data_sep_raw[, selec_cols] <- sapply(data_sep_raw[, selec_cols], as.numeric)

#### VALIDATION OF METHOD ####
sapply(data_validation, class)
psf_index = which(data_validation$Fraction == "SF" & data_validation$Element == "FM") 
data_xy_val_all = data.frame(x=data_validation$Calculated,y=data_validation$Given)
data_xy_val_all = data.frame(x=data_validation$Calculated,y=data_validation$Given)
data_xy_val_psf = data_xy_val_all[psf_index,]
data_xy_val_all = data_xy_val_all[which(data_validation$Fraction == "SF"),]
list_val = list(data_xy_val_psf,data_xy_val_all)
data_xy_val_combined = rbind(data.frame(group="All",data_xy_val_all),data.frame(group="pSF,DM",data_xy_val_psf))
for (i in 1:length(list_val)) {
  a_to_zero = F
  data_xytemp=list_val[[i]]
  if (i == 1){
    data_val_lincorr = lm_eqn_bydataframe(data_xytemp,data_xytemp[,1],data_xytemp[,2],a_to_0 = a_to_zero)
    rownames(data_val_lincorr)="pSF,DM"
    test=summary(lm(data=(data_xytemp),as.matrix(data_xytemp[,2]) ~ as.matrix(data_xytemp[,1])))
  }
  if (i > 1) {
    data_val_lincorr = rbind(data_val_lincorr,lm_eqn_bydataframe(data_xytemp,data_xytemp[,1],data_xytemp[,2],a_to_0 = a_to_zero))
    rownames(data_val_lincorr)[i]="all"
  }
  if (i == length(list_val)) {
  
  plot_val = ggplot(data=data_xy_val_combined, aes(x=data_xy_val_combined[,2], y=data_xy_val_combined[,3],shape=data_xy_val_combined$group, color=data_xy_val_combined$group))  +   
    geom_point() +
    stat_smooth(data=as.data.frame(data_xy_val_combined),method = "lm", formula= (y ~ x), show.legend = T) +
    theme_bw() +
    theme(legend.position = "bottom",legend.direction = "vertical")
  pdf(paste0(wd,"/Validation/","Validation_corr.pdf"), width = 7, height = 4)
  print(plot_val)
  dev.off()
  }
}
WriteXLS(data_val_lincorr,paste0(wd,"/Validation/","Validation_stat.xlsx"))


#### PLOT1: ERRORS ON MASS BALANCES ####
data_errors_melt=melt(data_errors,id.vars = c("ID","Eff_group"))
selected_err_var = c("VS","Ash","TN","TAN","Nor","P","K","S","Ca","Mg")
data_errors_melt=data_errors_melt[data_errors_melt$variable %in% selected_err_var,]

plot_errors=ggplot(data=data_errors_melt, 
                   aes(x=variable, y=value))+ geom_boxplot()+#geom_dotplot(binaxis = "y",stackdir = "center") +
  labs(title="Boxplots for mass balance errors",x="Variable",y="Errors") +
  stat_summary(fun.data = mean.n, aes(shape="Mean"), colour = "black", geom="point") +
  stat_summary(fun.data = give.n, geom = "text", aes(shape="Nb. of ind."), position=position_dodge(.9), fun.y = median,show.legend=FALSE, size=3 ) +
  scale_shape_manual("", values=c("Mean"="x","Nb. of ind."="N")) +
  geom_hline(yintercept=0.1, color="red",linetype="dashed") +
  geom_hline(yintercept=-0.1, color="red",linetype="dashed") +
  theme_bw() +
  theme(legend.position = "bottom")

pdf(paste0(wd,"/ERRORS/","ERRORS_singleplot.pdf"), width = 7, height = 4)
plot(plot_errors)
dev.off()

# AFTER: TESTANDO SE MEDIA ERROS SIGNIF MAIOR OU MENOR QUE QUE 0 ####
for (j in 1:length(selected_err_var)) {
  if (j == 1) {
    table_wilcox_test_err = data.frame(0,0,0,0,0,0,0,0,0,0,0)
    colnames(table_wilcox_test_err)=c("Var","Shapiro","Option","PV (t-test","PV (Wilcox)","N","median (%)","IQR (%)","mean (%)","SD (%)","SD by IQR")
  }
  i = selected_err_var[j]
  print(i)
  line_table = nrow(table_wilcox_test_err)
  if (j > 1) {line_table = line_table+1}
  table_wilcox_test_err[line_table,1] = i
  table_wilcox_test_err[line_table+1,1] = i
  table_wilcox_test_err[line_table+2,1] = i
  
  table_wilcox_test_err[line_table,3] = "Diff."
  table_wilcox_test_err[line_table+1,3] = "Greater"
  table_wilcox_test_err[line_table+2,3] = "Less"
  data_err_temp = data_errors_melt[data_errors_melt$variable == i,4]
  data_err_temp = data_err_temp[!is.na(data_err_temp)]
  
  shapiro_err_test=shapiro.test(data_err_temp)
  qqnorm(data_err_temp,main = i) 
  qqline(data_err_temp)
  
  ttest_err_diff = t.test(data_err_temp, mu = 0) #2sided is default
  ttest_err_greater = t.test(data_err_temp, mu = 0, alternative= "greater")
  ttest_err_less = t.test(data_err_temp, mu = 0, alternative = "less")
  
  wilcoxtest_err_diff = wilcox.test(data_err_temp, mu = 0) #2sided is default
  wilcoxtest_err_greater = wilcox.test(data_err_temp, mu = 0, alternative= "greater")
  wilcoxtest_err_less = wilcox.test(data_err_temp, mu = 0, alternative = "less")
  
  table_wilcox_test_err[line_table,2] = shapiro_err_test$p.value
  
  table_wilcox_test_err[line_table,4] = round(wilcoxtest_err_diff$p.value,3)
  table_wilcox_test_err[line_table+1,4] = round(wilcoxtest_err_greater$p.value,3)
  table_wilcox_test_err[line_table+2,4] = round(wilcoxtest_err_less$p.value,3)
  
  table_wilcox_test_err[line_table,5] = round(wilcoxtest_err_diff$p.value,3)
  table_wilcox_test_err[line_table+1,5] = round(wilcoxtest_err_greater$p.value,3)
  table_wilcox_test_err[line_table+2,5] = round(wilcoxtest_err_less$p.value,3)
  
  table_wilcox_test_err[line_table,6] = length(data_err_temp)
  table_wilcox_test_err[line_table,7] = round(100*median(data_err_temp),1)
  table_wilcox_test_err[line_table,8] = round(100*IQR(data_err_temp),1)
  table_wilcox_test_err[line_table,9] = round(100*mean(data_err_temp),1)
  table_wilcox_test_err[line_table,10] = round(100*sd(data_err_temp),1)
  table_wilcox_test_err[line_table,11] = table_wilcox_test_err[line_table,8]/1.349
}
WriteXLS(table_wilcox_test_err,paste0(wd,"/ERRORS/","ERRORS_mass_distrib.xlsx"))


#### 2.2 PLOT 2: EFFICIENCY BOXPLOTS BY EQUIP, DM AND OTHER VARS ####
data_boxplot_eff = cbind.data.frame(data_efficiency$`ID separation`,
                                    data_efficiency$`Sep. Abbrev`,
                                    data_efficiency[,13:26])
colnames(data_boxplot_eff)[1:2]=c("ID","Sep._Abbrev")
data_boxplot_eff_melt = melt(data_boxplot_eff, 
                             id.vars = c(1,2), 
                             measure.vars = 3:16)
equip_greater_than = names(which(summary(data_boxplot_eff$Sep._Abbrev)>3))
#equip_greater_than = equip_greater_than[-7]
data_boxplot_eff_melt=data_boxplot_eff_melt[data_boxplot_eff_melt$Sep._Abbrev %in% equip_greater_than,]
adapt_var1 = function(x) {return(substr(x,1,regexpr("_",x)-1))}
adapt_var2 = function(x) {return(substr(x,regexpr("_",x)+1,nchar(as.character(x))))}
data_boxplot_eff_melt=mutate(data_boxplot_eff_melt,variable1=adapt_var1(variable))
data_boxplot_eff_melt=mutate(data_boxplot_eff_melt,variable2=adapt_var2(variable))
data_boxplot_eff_melt$variable = factor(data_boxplot_eff_melt$variable, unique(data_boxplot_eff_melt$variable) )
data_boxplot_eff_melt$variable1 = factor(data_boxplot_eff_melt$variable1, unique(data_boxplot_eff_melt$variable1) )
data_boxplot_eff_melt$variable2 = factor(data_boxplot_eff_melt$variable2, unique(data_boxplot_eff_melt$variable2) )

plot=ggplot(data=data_boxplot_eff_melt, aes(x=variable2, y=value))

give.n <- function(x){
  return(c(y = 1.02*as.numeric(quantile(x,prob=0.75)), label = length(x)))}
mean.n <- function(x){
  return(c(y = mean(x), label = round(mean(x),2)))} 

pdf(paste0(wd,"/Initial_Boxplots/","Efficiency_singleplot.pdf"), width = 7, height = 4)
plot(plot + 
       geom_boxplot(aes(fill=variable1)) +
       stat_summary(fun.data = give.n, geom = "text", aes(shape="Nb. of ind.",group=variable1), position=position_dodge(.9), fun.y = median,show.legend=FALSE, size=3 ) +
       stat_summary(fun.y = mean, aes(shape="Mean",group=variable1), position=position_dodge(.9), colour = "black", geom="point") +
      facet_wrap(~Sep._Abbrev ,scales="free",nrow = 1, ncol = NULL) + #,labeller=labeller(.cols=labelsVAR))
      labs(title="Boxplots for sep. equip.",x="Eff. indicator",y="Values in different scale") +
      theme(plot.title = element_text(size = rel(1.5), colour = "black"),
             legend.title=element_blank(),
             legend.position="bottom",
             legend.direction="horizontal") + 
      guides(fill = guide_legend(nrow = 2)) +
      scale_shape_manual("", values=c("Mean"="x","Nb. of ind."="N")) +
       theme_bw()
     #+ stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, colour = "red")
)
dev.off()




#### 1.3 TESTING CORRELATIONS BETWEEN EFF1 and EFF2 ####
data_eff_corr = cbind.data.frame(data_efficiency$`ID separation`,
                                    data_efficiency$`Sep. Abbrev`,
                                    data_efficiency[,13:26])
colnames(data_eff_corr)[1:2]=c("ID","Sep._Abbrev")

effxy_DM = data_eff_corr[,3:4]
effxy_VS = data_eff_corr[,5:6]
effxy_TN = data_eff_corr[,7:8]
effxy_TAN = data_eff_corr[,9:10]
effxy_Norg = data_eff_corr[,11:12]
effxy_P = data_eff_corr[,13:14]
effxy_K = data_eff_corr[,15:16]
list_effxy = list(effxy_DM,effxy_VS,effxy_TN,effxy_TAN,effxy_Norg,effxy_P,effxy_K)
effxy_all = data.table::rbindlist(list_effxy)
colnames(effxy_all)=c("Eff1","Eff2")
list_effxy = list(effxy_all,effxy_DM,effxy_VS,effxy_TN,effxy_TAN,effxy_Norg,effxy_P,effxy_K)

for (i in 1:length(list_effxy)) {
  data_xytemp=list_effxy[[i]]
  if (i == 1){
    data_eff_lincorr = lm_eqn_bydataframe(data_xytemp,data_xytemp[,1],data_xytemp[,2])
    rownames(data_eff_lincorr)="all"
  }
  if (i > 1) {
    data_eff_lincorr = rbind(data_eff_lincorr,lm_eqn_bydataframe(data_xytemp,data_xytemp[,1],data_xytemp[,2]))
    rownames(data_eff_lincorr)[i]=paste0(colnames(data_eff_corr)[2*i-1],",",colnames(data_eff_corr)[2*i])
  }
}

#data_eff_lincorr=mutate(data_eff_lincorr,pSF1=(1+1/b))
#data_eff_lincorr=mutate(data_eff_lincorr,pSF2 =(1+1/(a-1)))

##### 1. STATISTICS ####
#### 1.1 CORRELATION MATRIX ####
#### 1.1.1 Preparing data ####

data_sep_raw_numeric = data_sep_raw[,which(matrix(lapply(data_sep_raw, class)) == "numeric")]
data_sep_raw_numeric = data_sep_raw_numeric[,-1]
data_sep_raw_numeric_SP = data_sep_raw_numeric[data_sep_raw$`Sep. Abbrev`=="SP",]
data_sep_raw_numeric_C = data_sep_raw_numeric[data_sep_raw$`Sep. Abbrev`=="C",]
data_sep_raw_numeric_Low = data_sep_raw_numeric[data_sep_raw$Eff_group=="Low",]
data_sep_raw_numeric_High = data_sep_raw_numeric[data_sep_raw$Eff_group=="High",]
list_numeric_data= list (all.eqp_all.var = data_sep_raw_numeric,
                         SP_all.var = data_sep_raw_numeric_SP,
                         C_all.var = data_sep_raw_numeric_C,
                         Low.eff_all.var = data_sep_raw_numeric_Low,
                         High.eff_all.var = data_sep_raw_numeric_High)
#### Starting corrmatrix LOOP ####
for (i in 1:length(list_numeric_data)) {
data_corr = as.data.frame(list_numeric_data[[i]])
name_data_corr = names(list_numeric_data)[i]


#### 1.1.2 Corrmatrix calculation #####
matrrcor=rcorr(as.matrix(data_corr), type=c("pearson","spearman"))

#### 1.1.3 exporting excel #####

WriteXLS(list(as.data.frame(matrrcor$r),as.data.frame(matrrcor$P),as.data.frame(matrrcor$n)),
         ExcelFileName = paste(wd,"/Corr_matrix/",name_data_corr,"_correlation_matrix.xlsx",sep=""),
         SheetNames = c("r2","p_value","n"), perl = "perl",
         verbose = FALSE, Encoding = c("UTF-8", "latin1", "cp1252"),
         row.names = TRUE, col.names = TRUE,
         AdjWidth = FALSE, AutoFilter = TRUE, BoldHeaderRow = TRUE,
         na = "",
         FreezeRow = 1, FreezeCol = 0,
         envir = parent.frame())

#### 1.1.4 plotting correlation matrix ####
col = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

pdf(paste0(wd,"/Corr_matrix/",name_data_corr,"_corrmatrix_pv01.pdf"))
corrplot(matrrcor$r, method="color", #col=col(200),
         type="upper", #order="hclust", hclust.method = "average",
         addCoef.col = "black", # Ajout du coefficient de corr?lation
         number.cex= 7/ncol(matrrcor$r),
         tl.col="black", tl.srt=45, #Rotation des etiquettes de textes
         tl.cex = 9/ncol(matrrcor$r), cl.cex = 9/ncol(matrrcor$r),
         # Combiner avec le niveau de significativit?
         p.mat = matrrcor$P, sig.level = 0.01, insig = "blank",
         # Cacher les coefficients de corr?lation sur la diagonale
         diag=FALSE,
         na.label="NA", na.label.col="grey")
title("p-value<0.01", line=2)
dev.off()

pdf(paste0(wd,"/Corr_matrix/",name_data_corr,"_corrmatrix_pv05.pdf"))
corrplot(matrrcor$r, method="color", #col=col(200),
         type="upper", #order="hclust", hclust.method = "average",
         addCoef.col = "black", # Ajout du coefficient de corr?lation
         number.cex= 7/ncol(matrrcor$r),
         tl.col="black", tl.srt=45, #Rotation des etiquettes de textes
         tl.cex = 9/ncol(matrrcor$r), cl.cex = 9/ncol(matrrcor$r),
         # Combiner avec le niveau de significativit?
         p.mat = matrrcor$P, sig.level = 0.05, insig = "blank",
         # Cacher les coefficients de corr?lation sur la diagonale
         diag=FALSE,
         na.label="NA", na.label.col="grey")
title("p-value<0.05", line=2)
dev.off()
} #end of for loop

#### 1.2 DETERMINING EQUIPMENT GROUPS ####
#### 1.2.1 PLOT 3: Histograms of efficiency ####
data_hist=data.frame("Eff1,DM"=data_sep_raw$Eff1,"Eff2,DM"=data_sep_raw$Eff2)
data_hist=melt(data_hist)
eff1 = data_hist[data_hist$variable=="Eff1.DM",]
eff2 = data_hist[data_hist$variable=="Eff2.DM",]
den1=density(eff1$value)
den2=density(eff2$value)
den1min = data.frame(x=den1$x,y=den1$y)
den2min = data.frame(x=den2$x,y=den2$y)
range1=c(0.4,0.6)
range2=c(0.5,0.75)
den1minred = den1min[(den1min < range1[2] & den1min > range1[1] ),]
den2minred = den2min[(den2min < range2[2] & den2min > range2[1] ),]
den1min_y = min(den1minred[,2],na.rm = T)
den2min_y = min(den2minred[,2],na.rm = T)
den1min_x = den1minred$x[which(den1minred$y == den1min_y)]
den2min_x = den2minred$x[which(den2minred$y == den2min_y)]
vlines=data.frame(Var=c(paste0("x=",round(den1min_x,2)),paste0("x=",round(den2min_x,2))),xmin = c(den1min_x,den2min_x),ymin=c(den1min_y,den2min_y))
colnames(vlines)[1]="Vert. lines"
# 1.2.1.1 PLOTS ###
pdf(paste0(wd,"/Efficiency_histogram/","Histogram_Eff1.pdf"))
ggplot(eff1, aes(value, fill = variable)) + geom_histogram(col="black", alpha = 0.5)+
labs(title = "Separation efficiency histogram",x="1 - TSliq/TSraw", y="Count")
dev.off()

pdf(paste0(wd,"/Efficiency_histogram/","Histogram_TS.to.SF.pdf"))
ggplot(eff2, aes(value, fill = variable)) + geom_histogram(col="black", alpha = 0.5) +
labs(title = "Separation efficiency histogram",x="TS distribution to SF (w/w)", y="Count")
dev.off()

pdf(paste0(wd,"/Efficiency_histogram/","Histogram_overlapped.pdf"))
ggplot(data_hist, aes(x=value)) +
  geom_histogram(data=eff1,col="black",aes(fill = eff1$variable[1]),alpha = 0.2) +
  geom_histogram(data=eff2,col="black",aes(fill = eff2$variable[1]),alpha = 0.2) +
  labs(title = "Separation efficiency histograms",x="Value", y="Count") + 
  scale_fill_manual(name="Variable",values=c("#ff3860","#00b6c9"))#,labels=as.character(unique(data_hist$variable)))
dev.off()

pdf(paste0(wd,"/Efficiency_histogram/","Density_overlapped.pdf"))
ggplot(data_hist, aes(value, fill = variable)) + geom_density(alpha = 0.2) +
  labs(title = "Separation efficiency densities",x="Value", y="Density") +
  geom_vline(data=vlines,aes(xintercept=xmin, colour=`Vert. lines`, linetype=`Vert. lines`),
             alpha = 0.8,size=1)+
  scale_fill_discrete(name="Density \n\ curves") +
  theme_bw()

dev.off()



#### Testing significance of groups ####
funct_eff = function(x,limit) {
  (if (is.na(x)) { result=NA 
  } else if (x > limit) {
    result="High"
  } else {
    result="Low" } )
  return(result) 
}    
## Comparing mean and sd of groups through eff1 or eff2 ####
groups_eff1 = apply(matrix(as.numeric(data_sep_raw$Eff1)),1, function(x) funct_eff(x,limit=0.5370558))
groups_eff2 = apply(matrix(as.numeric(data_sep_raw$Eff2)),1, function(x) funct_eff(x,limit=0.6197546))

data_groups_eff1 = data.frame(value=data_sep_raw$Eff1,group=groups_eff1)
data_groups_eff2 = data.frame(value=data_sep_raw$Eff2,group=groups_eff2)

mean_eff1groups=ddply(.data = data_groups_eff1, ~group, summarize, mean_eff1=mean(value))
mean_eff2groups=ddply(.data = data_groups_eff2, ~group, summarize, mean_eff2=mean(value))
sd_eff1groups=ddply(.data = data_groups_eff1, ~group, summarize, sd_eff1=sd(value))
sd_eff2groups=ddply(.data = data_groups_eff2, ~group, summarize, sd_eff2=sd(value))

summary_eff1groups = cbind(mean_eff1groups,sd_eff1=sd_eff1groups[,2])
summary_eff2groups = cbind(mean_eff2groups,sd_eff2=sd_eff2groups[,2])
summary_eff12_groups = cbind(summary_eff1groups,summary_eff2groups[2:3])

index_exclude_ID_eff1eff2 = which(!(groups_eff1==groups_eff2))

WriteXLS(summary_eff12_groups,paste0(wd,"/Efficiency_histogram/","Eff_groups_means_sd.xlsx"))

groups_eff12 = groups_eff1[-index_exclude_ID_eff1eff2]

data_groups = data.frame(value_eff1=data_sep_raw$Eff1[-index_exclude_ID_eff1eff2],value_eff2=data_sep_raw$Eff2[-index_exclude_ID_eff1eff2],group=groups_eff12)

# 1.2.1.2 t-test  of groups ###
# first verifying normality #

for (i in 1:2){
print(i)
  data_low_eff = as.numeric(data_groups[data_groups$group == "Low",][[i]])
data_high_eff = data_groups[data_groups$group == "High",][[i]]
shapiro_low_eff = shapiro.test(data_low_eff)
print(shapiro_low_eff)
shapiro_high_eff = shapiro.test(data_high_eff)
print(shapiro_high_eff)
hist(data_low_eff)
hist(data_high_eff)

# t-test and wilcox test
print(var.test(data_low_eff,data_high_eff)) #pv > 0.05 -> can assume homogenous variances
t_test_groups = t.test(data_low_eff,data_high_eff)
print(t_test_groups)
print(wilcox.test(data_low_eff,data_high_eff))
# ANOVA
#groups_model_eff = lm(formula = value_eff ~ group, data = data_groups_eff1)
#summary(groups_model_eff1)
#anova(groups_model_eff1)
#confint(groups_model_eff1)
}


#como ler: 
# 1) se o p-value > 0.15, nao invalida que o modelo nao é normal: pvalue do ttest pode ser usado
# 2) p < 0.10; teste de adequaçao invalida o modelo normal: atençao ao usar o pv do ttest
# 3) entre 0.10 e 0.15 (caso do exemplo): zona de perigo, melhor usar outro teste


#### 2.1 Overal distribution ####
# it's necessary to transform into a factor to keep original order in plots #

for (i in 1:length(list_distrib_data)) {
  data_distrib = as.data.frame(list_distrib_data[[i]])
  name_data_distrib = names(list_distrib_data)[i]
  
  
  data_distrib_melt = melt(data_distrib, id = colnames(data_distrib)[1:4] )
  data_distrib_melt$value = 100*data_distrib_melt$value
  data_distrib_melt$Variable = factor(data_distrib_melt$Variable, rev(unique(data_distrib_melt$Variable)) )
  
  data_distrib_SF_av = data_distrib_melt[data_distrib_melt$variable == "SF av.",]
  data_distrib_SF_up = data_distrib_SF_av$value + data_distrib_melt[data_distrib_melt$variable == "SF sd",]$value
  data_distrib_SF_low = data_distrib_SF_av$value - data_distrib_melt[data_distrib_melt$variable == "SF sd",]$value
  data_distrib_SF_min = data_distrib_melt[data_distrib_melt$variable == "SF min",]$value
  data_distrib_SF_max = data_distrib_melt[data_distrib_melt$variable == "SF max",]$value
  
  data_distrib_LF_av = data_distrib_melt[data_distrib_melt$variable == "LF av.",]
  data_distrib_LF_up = data_distrib_LF_av$value + data_distrib_melt[data_distrib_melt$variable == "LF sd",]$value
  data_distrib_LF_low = data_distrib_LF_av$value - data_distrib_melt[data_distrib_melt$variable == "LF sd",]$value
  data_distrib_LF_min = data_distrib_melt[data_distrib_melt$variable == "LF min",]$value
  data_distrib_LF_max= data_distrib_melt[data_distrib_melt$variable == "LF max",]$value
  
  #### 1A.1 PLOT 1 (+PLOT 4) : OVERALL MASS BALANCES (AND BY EFF GROUPS) ####
  pdf(paste0(wd,"/Mass_distribution/",name_data_distrib,"_eff_mass_balance.pdf"))
  print(
    ggplot(data=data_distrib_melt, 
           aes(x = Variable, y = value, fill = variable)) + 
      geom_bar(data = data_distrib_SF_av, colour= "black",
               stat = "identity",
               position = "identity") +
      geom_bar(data = data_distrib_LF_av, colour= "black",
               stat = "identity",
               position = "identity",
               mapping = aes(y = -value)) +
      geom_errorbar(data = data_distrib_LF_av,
                    mapping = aes(y = -data_distrib_LF_av$value,
                                  ymin = -data_distrib_LF_low,
                                  ymax = -data_distrib_LF_up)) +
      geom_errorbar(data = data_distrib_SF_av,
                    mapping = aes(y = data_distrib_SF_av$value,
                                  ymin = data_distrib_SF_low,
                                  ymax = data_distrib_SF_up)) +
      geom_text(data = data_distrib_SF_av, label= data_distrib_SF_av$n,
                mapping = aes (y = rep(99,nrow(data_distrib)))) +
      geom_text(data = data_distrib_LF_av, label= round(data_distrib_LF_av$value,0),
                mapping = aes (y = -0.5*data_distrib_LF_av$value), color = "white") +
      geom_text(data = data_distrib_SF_av, label= round(data_distrib_SF_av$value,0),
                mapping = aes (y = 0.5*data_distrib_SF_av$value), color = "white") +
      geom_segment(data=data_distrib_SF_av,
                mapping = aes(x=as.numeric(Variable)-0.45,xend=as.numeric(Variable)+0.45,
                              y=data_distrib_SF_min, yend=data_distrib_SF_min,
                              linetype="Min.",color="Min."),size=0.8) +
      geom_segment(data=data_distrib_SF_av,
                   mapping = aes(x=as.numeric(Variable)-0.45,xend=as.numeric(Variable)+0.45,
                                 y=data_distrib_SF_max, yend=data_distrib_SF_max,
                                 linetype="Max.",color="Max."),size=0.8) +
      geom_segment(data=data_distrib_SF_av,
                   mapping = aes(x=as.numeric(Variable)-0.45,xend=as.numeric(Variable)+0.45,
                                 y=-data_distrib_LF_min, yend=-data_distrib_LF_min,
                                 linetype="Min.",color="Min."),size=0.8) +
      geom_segment(data=data_distrib_SF_av,
                   mapping = aes(x=as.numeric(Variable)-0.45,xend=as.numeric(Variable)+0.45,
                                 y=-data_distrib_LF_max, yend=-data_distrib_LF_max,
                                 linetype="Max.",color="Max."),size=0.8) +
      scale_color_manual(values=c("Red","Green")) +
      #geom_point(data=data_distrib_LF_min, aes(y=data_distrib_SF_min,shape="Min. and max."),size=3)+
       # scale_shape_manual(values="red") +
      coord_flip() +
      scale_y_continuous(labels = abs, limits = c(-100,100)) +
      scale_fill_manual(labels = c("Liquid \n\ Fraction.","Solid \n\ Fraction"),values = c("#6fb3ca","#ca866f")) + 
      labs(title = paste0("Mass balance. Efficiency group: ",name_data_distrib),
           y="Distribution (%)", x="Variable", fill="Fraction")+
      theme_bw()
  )
  dev.off()
}

#### PLOT: GROUPS AND CHOICE OF EQUIPMENT ####
data_equip_group_plot = as.data.frame(table(data_equip_group))
data_equip_group_plot=data_equip_group_plot[which(data_equip_group_plot$Freq != 0),]
data_equip_group_plot$Eff_group = factor(data_equip_group_plot$Eff_group, levels = c("Low","High","Inconsistent"))

data_equip_group_plot = data_equip_group_plot[order(data_equip_group_plot$Sep.Abbrev),]
data_equip_group_plot = data_equip_group_plot[order(data_equip_group_plot$Eff_group),]
data_equip_group_plot = data_equip_group_plot[rev(1:nrow(data_equip_group_plot)),]

data_equip_group_plot <- ddply(data_equip_group_plot, "Eff_group",
                          transform, label_ypos=cumsum(Freq)-0.5*Freq)
data_equip_group_plot <- ddply(data_equip_group_plot, "Eff_group",
                          transform, percent=round(100*Freq/sum(Freq),0))
data_equip_group_plot$label_plotg=paste0("n=",data_equip_group_plot$Freq,". ",data_equip_group_plot$percent,"%")

colnames(data_equip_group_plot)[1:3] = c("Sep. Equipment","Efficiency category","N. observations")

plot_equip = ggplot(data_equip_group_plot, aes(x=`Efficiency category`, y=`N. observations`,fill=`Sep. Equipment`)) +
  geom_bar(stat="identity") +
  geom_text(aes(y=label_ypos, label=label_plotg), color="black", size=3.5) +
  scale_fill_brewer(palette="Paired")+  #, guide=F) + #TO REMOVE THE LEGEND FROM PLOT!!
  theme_bw() +
  theme(legend.position="bottom")

pdf(paste0(wd,"/Group_eff_equip.pdf"),width = 5.4, height = 5.4)
print(plot_equip)
dev.off()  

#### PLOT: FEED AND CHOICE OF EQUIPMENT ####

data_feed_group_plot = data.frame(table(data_feed_group))
data_feed_group_plot=data_feed_group_plot[which(data_feed_group_plot$Freq != 0),]
data_feed_group_plot$Group = factor(data_feed_group_plot$Group, levels = c("Low","High","Inconsistent"))

data_feed_group_plot = data_feed_group_plot[order(data_feed_group_plot$Feed),]
data_feed_group_plot = data_feed_group_plot[order(data_feed_group_plot$Group),]
data_feed_group_plot = data_feed_group_plot[rev(1:nrow(data_feed_group_plot)),]


colnames(data_feed_group_plot)[3]="N"
data_feed_group_plot <- ddply(data_feed_group_plot, "Group",
                         transform, label_ypos=cumsum(N)-0.5*N)
data_feed_group_plot <- ddply(data_feed_group_plot, "Group",
                         transform, percent=round(100*N/sum(N),0))
data_feed_group_plot$label_plotg=paste0("n=",data_feed_group_plot$N,". ",data_feed_group_plot$percent,"%")


colnames(data_feed_group_plot)[1:3] = c("Efficiency category","Anaerobic digestion \n\ feedstock","N. observations")

plot_feed = ggplot(data_feed_group_plot, 
                   aes(x=`Efficiency category`, 
                       y=`N. observations`,fill=`Anaerobic digestion \n\ feedstock`)) +
  geom_bar(stat="identity") +
  geom_text(aes(y=label_ypos, 
                label=data_feed_group_plot$label_plotg), 
                color="black", size=3.5) +
  scale_fill_brewer(palette="Paired")+#, guide=F) + #TO REMOVE THE LEGEND FROM PLOT!!
  theme_bw() +
  theme(legend.position="bottom")
  

pdf(paste0(wd,"/Group_eff_feed.pdf"),width = 5.4, height = 5.4)
print(plot_feed)
dev.off() 

pdf(paste0(wd,"/Group_eff_feed NO LEGEND.pdf"),width = 5.4, height = 5.4)
print(plot_feed_nolegend)
dev.off() 
#### 1.2.3 PLOT: PCA OF SEPARATION EFFICIENCY ####
# estimating ncp and replacing NA


for (i in 1:length(list_PCA_data)) {
data_PCA = as.data.frame(list_PCA_data[[i]])
name_data_PCA = names(list_PCA_data)[i]
data_PCA_numeric = data_PCA[,10:ncol(data_PCA)]
data_PCA_numeric = sapply(data_PCA_numeric, as.numeric)
NNA=2
data_PCA=data_PCA[which(rowSums(is.na(data_PCA_numeric))<=NNA),]
data_PCA_numeric=data_PCA_numeric[which(rowSums(is.na(data_PCA_numeric))<=NNA),]

sapply(data_PCA_numeric, class)
#groups_temp = apply(matrix(as.numeric(data_PCA$`1-Tsliq/Tsraw`)),1,funct_eff)

#nb=estim_ncpPCA(as.matrix(data_PCA_numeric), scale=TRUE, method.cv = "Kfold")    #estimation of ncp (=nb$ncp)
dataPCA_complete=imputePCA(as.matrix(data_PCA_numeric), ncp=5)
# running PCA ####
dataPCA_complete_obs=dataPCA_complete$completeObs
PCA_separation = PCA(dataPCA_complete_obs, scale.unit = TRUE,ncp=5,graph = T)
dataPCA_complete_obs=as.data.frame(dataPCA_complete_obs)
# PCA output EXCEL ####
eigenvalues = as.data.frame(PCA_separation$eig)
loading_matrix=as.data.frame(PCA_separation$var$coord)
PCcorr=dimdesc(PCA_separation, axes=c(1,2,3))
PC1corr=as.data.frame(PCcorr$Dim.1$quanti)
PC2corr=as.data.frame(PCcorr$Dim.2$quanti)
PC3corr=as.data.frame(PCcorr$Dim.3$quanti)
aka=as.data.frame(PCA_separation$ind$coord)
write_PCA_data=c("data_PCA","dataPCA_complete_obs","eigenvalues","loading_matrix","PC1corr","PC2corr","PC3corr","aka")
WriteXLS(write_PCA_data, 
         ExcelFileName = paste0(wd,"/PCA/",name_data_PCA,"_output_data.xls",sep=""), 
         SheetNames = write_PCA_data, perl = "perl",
         verbose = FALSE, Encoding = c("UTF-8", "latin1", "cp1252"),
         row.names = TRUE, col.names = TRUE,
         AdjWidth = FALSE, AutoFilter = TRUE, BoldHeaderRow = TRUE,
         na = "",
         FreezeRow = 1, FreezeCol = 0,
         envir = parent.frame())

# PCA plots ###
pdf(paste0(wd,"/PCA/",name_data_PCA,"_eigen.pdf"))
print(fviz_screeplot(PCA_separation, ncp=10))
dev.off()

# PLOTING BY DIGESTATE TYPOLOGY #
pdf(paste0(wd,"/PCA/",name_data_PCA,"_biplotPC12_group_efficiency.pdf"))
print(fviz_pca_biplot(PCA_separation, label="var", 
                   axes = c(1,2), #choose PC
                   habillage=as.factor(groups_temp),
                   addEllipses=T, ellipse.level=0.95, repel = T)

        #+ geom_text_repel(aes(x=PCA_separation$ind$coord[,1], 
          #                 y=PCA_separation$ind$coord[,2],
           #                 label=data_PCA_equip$`Sep. Abbrev`))
)
dev.off()

pdf(paste0(wd,"/PCA/",name_data_PCA,"_biplotPC13_group_efficiency.pdf"))
print(fviz_pca_biplot(PCA_separation, label="var", 
                      axes = c(1,3), #choose PC
                      habillage=as.factor(data_PCA$`Sep. Abbrev`),
                      addEllipses=T, ellipse.level=0.95, repel = T)
      
      #+ geom_text_repel(aes(x=PCA_separation$ind$coord[,1], 
      #                 y=PCA_separation$ind$coord[,2],
      #                 label=data_PCA_equip$`Sep. Abbrev`))
)
dev.off()
}

#### Statistics for predicting efficiency ####
# handing data ####
CENT_stat = data.frame("")
SP_stat = data.frame("")
col_selected = c(48,52,124,132,148,156)
use_label=F
clean_selec = T
#stat_all_table_final = as.data.frame(matrix(nrow=length(col_selected),ncol=3))
PREFIXO="All_nolabel"
exclude_ID = ""#c("PS23","PS25","PS17")
selected_equip = c(
  "SP+SV"
 ,"SP"
  ,"SP+SV+C"
  ,"C"
  # ,"SP+C(LF)"
 # ,"RD+SP(SF)"
#  ,"FP"
 # ,"SE.or.RS"
 ,"VS"
#  ,"SP+C"
  #,"RS(lab)"
 # ,"RD"
#  ,"PS+SP(SF)"
)
for (i in 1:length(col_selected)) {

COL=col_selected[i]
data_mult_scatplot = data_sep_raw[,c(1,8,9,11,12,13,17,COL)]  #48:SF, 52:DM to SF, 124=TN to SF
data_mult_scatplot$Polymer2[is.na(data_mult_scatplot$Polymer2)]="NA."
data_mult_scatplot=data_mult_scatplot[data_mult_scatplot$`Sep. Abbrev` %in% selected_equip,]
#data_mult_scatplot$`Sep. Abbrev`[!(data_mult_scatplot$`Sep. Abbrev` %in% selected_equip)]="Other"
data_outliers = data_mult_scatplot[(data_mult_scatplot$`ID separation` %in% exclude_ID),]
data_mult_scatplot=data_mult_scatplot[!(data_mult_scatplot$`ID separation` %in% exclude_ID),]
data_mult_scatplot=data_mult_scatplot[!(is.na(as.matrix(data_mult_scatplot[,8]))),]


colnames(data_mult_scatplot)[c(7)]=c("DM")

lm_eqn_bymodel = function(modelxxx) {
  
  m <- modelxxx
  
  ###ADJUST IF NEEDED
  aaaa = format(coef(m)[1], digits = 2) 
  bbbb = format(coef(m)[2], digits = 2)
  R2222 = format(summary(m)$r.squared, digits = 2)
  pvvvv = signif(summary(m)$coef[2,4], 2)
  eqqqq = paste0("y=",bbbb,"*x","+(",aaaa,"). r2=",R2222," ,p.value=",pvvvv)
  return(eqqqq)
}
lm_eqn_bydata = function(dataf) {
  if(nrow(dataf)>2){
    m <- lm(as.matrix(dataf[,8])~as.matrix(dataf[,7]),data=dataf)
    ###ADJUST IF NEEDED
    aaa = format(coef(m)[1], digits = 2) 
    bbb = format(coef(m)[2], digits = 2)
    R222 = format(summary(m)$r.squared, digits = 2)
    pvvv = signif(summary(m)$coef[2,4], 2)
    nnnn = nrow(m$model)
    eqqq = paste0("y=",bbb,"*x","+(",aaa,"). r2=",R222," ,p.value=",pvvv,". n=",nnnn)
  }
  else eqqq=paste0("n=",nrow(dataf))
  return(eqqq)}

# PLOTING LINEAR CORRELATION FOR ALL, WITH GROUPS ONLY DISPLAYD
#data_mult_scatplot = melt(data_mult_scatplot, id=c(1:5))
x_temp = as.matrix(data_mult_scatplot[,7])
y_temp = as.matrix(data_mult_scatplot[,8])
x_out = as.matrix(data_outliers[,7])
y_out = as.matrix(data_outliers[,8])

n_pol_options = length(unique(rbind(data_mult_scatplot,data_outliers)$Polymer2)) 

model_temp = lm(y_temp~x_temp,data=data_mult_scatplot)
name_corr = paste("x.",colnames(x_temp),"_y.",colnames(y_temp))
plot1 = ggplot(aes(x=x_temp,y=y_temp, color=factor(Polymer2)), 
                     data=data_mult_scatplot)  +  
  stat_smooth(aes(group=1),method = "lm", formula= (y ~ x)) +
  geom_point()+ #,pch=21  
  geom_point(aes(shape=factor(data_mult_scatplot$`Sep. Abbrev`)),size = 4) +
  scale_color_manual(name="Polymer",values= brewer.pal(name="Dark2",n_pol_options)) +
  scale_shape(name="Equip",solid=T) +
  labs(title = paste("Adj R2 = ",signif(summary(model_temp)$adj.r.squared, 2),
                     "Intercept =",signif(model_temp$coef[[1]],2 ),
                     " Slope =",signif(model_temp$coef[[2]], 2),
                     " P =",signif(summary(model_temp)$coef[2,4], 2)),
       x=colnames(x_temp),y=colnames(y_temp)) +
  theme_bw() +
  theme(legend.position = "bottom",legend.direction = "vertical")
if (use_label){
  plot1 = plot1 +
    geom_text_repel(aes(label=data_mult_scatplot$`ID separation`))
}
if (nrow(data_outliers)>0) {
plot1 = plot1 +
  geom_point(data=data_outliers, aes(x=x_out, y=y_out,fill="Outlier"),size=3,pch=21)+
  scale_fill_manual(values="red") +
  geom_text_repel(data=data_outliers,aes(x=as.matrix(x_out),y=as.matrix(y_out),label=data_outliers$`ID separation`)) 
}
pdf(paste0(wd,"/New_corr/",PREFIXO,"_",name_corr,"_linearcorr_all.pdf"),width = 5.4, height = 5.4)
print(plot1)
dev.off()

# PLOTING LINEAR CORRELATION FOR EACH GROUP

function_text = ddply(data_mult_scatplot, c("Polymer2"),lm_eqn_bydata)
scale_label = paste0(function_text$Polymer2,". ",function_text$V1)

plot2 = ggplot(aes(x=x_temp, y=y_temp,color=Polymer2), data=as.data.frame(data_mult_scatplot))  +   
  geom_point() +
  
  scale_color_manual(name="Polymer",values = brewer.pal(name="Dark2",n_pol_options), 
                     labels = scale_label) +
  stat_smooth(data=as.data.frame(data_mult_scatplot),method = "lm", formula= (y ~ x), show.legend = T) +
  labs(x=colnames(x_temp),y=colnames(y_temp)) +
  theme_bw() +
  theme(legend.position = "bottom",legend.direction = "vertical") 
if (use_label){
  plot2 = plot2 +
    geom_text_repel(aes(label=data_mult_scatplot$`ID separation`))
}
if (nrow(data_outliers)>0) {
  plot2 = plot2 +
    geom_point(data=data_outliers, aes(x=x_out, y=y_out,fill="Outlier"),size=3,pch=21)+
    scale_fill_manual(values="red") +
    geom_text_repel(data=data_outliers,aes(x=as.matrix(x_out),y=as.matrix(y_out),label=data_outliers$`ID separation`))
}
pdf(paste0(wd,"/New_corr/",PREFIXO,"_",name_corr,"_linearcorr_group.pdf"),width = 5.4, height = 5.4)
print(plot2)
dev.off()

}



#### NOT USING ####


for (i in c("All","SP","C","Low","High")) {
select_criteria = i
data_PLS_select = as.data.frame(data_PLS[-30:-41,])
rownames(data_PLS_select)=as.character(data_PLS_select$ID_separation)
if (i == "All") {
  data_PLS_select = data_PLS_select[-30:-41,]
} else if (nchar(i)<=2) {
data_PLS_select = data_PLS_select[data_PLS_select$Sep._Abbrev == i,]

} else if (nchar(i)>2) {
data_PLS_select= data_PLS_select[data_PLS_select$Eff_group == i,]
} 

data_PLS_select_X = data_PLS_select$DM #[,c(5:8)]
data_PLS_select_Y = data_PLS_select[,c(9:13)]
linesn = round(nrow(data_PLS_select)*0.3,0)
data_x_cal = data_PLS_select_X[-1:-linesn]#,]
data_x_val = data_PLS_select_X[1:linesn]#,]
data_y_cal = data_PLS_select_Y[-1:-linesn,]
data_y_val = data_PLS_select_Y[1:linesn,]
data_cal = data.frame(data_x_cal,data_y_cal)
data_val = data.frame(data_x_val,data_y_val)
  
pls_temp = pls(X = data_x_cal[-8], Y = data_y_cal[-8,])
pls_validation = predict(pls_temp, ncomp = 3, newdata = data_x_val,type="score")
pls_validation$predict

model_mlr = lm(as.matrix(data_y_cal) ~ as.matrix(data_x_cal))


for (j in 1:ncol(data_PLS_select_X)) {
  print(paste0("j=",j))
  for (k in 1:ncol(data_PLS_select_Y)) {
    print(paste0("k=",k))
    data_temp = cbind.data.frame(data_PLS_select_X[,j],data_PLS_select_Y[,k])
    label_x=names(data_temp)[1]
    label_y=names(data_temp)[2]
    model_temp = lm(data_temp[,1] ~ data_temp[,2])
    pdf(paste0(wd,"/Linear_corr/",select_criteria,"x.",label_x,"y.",label_y,"_scatterplot.pdf"))
    print(ggplot(data = data_temp, aes(x=data_temp[,1],y=data_temp[,2])) + geom_point() +
    labs(title = paste("Adj R2 = ",signif(summary(model_temp)$adj.r.squared, 5),
                       "Intercept =",signif(model_temp$coef[[1]],5 ),
                       " Slope =",signif(model_temp$coef[[2]], 5),
                       " P =",signif(summary(model_temp)$coef[2,4], 5)),
         x=label_x,y=label_y) +
      stat_smooth(method = "lm"))
    dev.off()
    if (j == 1) {
      data_temp2 = data.frame(data_temp,data_select_polpol)
      print(ggplot(aes(x=data_temp[,1], y=data_temp[,2],color=Polymer2), data=data_temp)  +  
        geom_point() +
        stat_smooth(method = "lm", formula= (y ~ x)) +
        # scale_color_manual(name="Equip.",values= brewer.pal(name="Set2",length(table(data_mult_scatplot$Sep.categ))))+
        #scale_color_manual(name="Polymer",values= brewer.pal(name="RdBu",length(table(data_mult_scatplot$Polymer)))) +
        #geom_text(x = mean, y = 300, label = lm_eqn(df), parse = TRUE) +
        labs(title = paste("a"),
             x="Digestate Dry Matter (%)",y="Solid fraction mass (%)") +
        theme_bw())
    }
  }
}
}







#### NOT USING: PLS Polymer ####
data_PLS_polymer_comp = imputePCA(as.matrix(data_PLS_polymer[,5:14]))
data_PLS_polymer_comp = data_PLS_polymer_comp$completeObs

data_PLS_polymer_comp = data_PLS_polymer_comp[data_PLS_polymer$Sep._Abbrev=="C",]
X_PLS_pol = data_PLS_polymer_comp #data_PLS_polymer_comp[,5:14]
Y_PLS_pol = data_PLS_polymer[,4]
Y_PLS_pol = data_PLS_polymer[data_PLS_polymer$Sep._Abbrev=="C",4]

plsda_polym = plsda(X_PLS_pol,factor(as.matrix(Y_PLS_pol)))
plotVar(plsda_polym)

PCA_polym = PCA(data_PLS_polymer_comp, scale.unit = TRUE,ncp=5,graph = T)

fviz_pca_biplot(PCA_polym, label="var", 
                      axes = c(1,2), #choose PC
                      habillage=as.factor(as.matrix(Y_PLS_pol)),
                      addEllipses=T, ellipse.level=0.95, repel = T)
      
      #+ geom_text_repel(aes(x=PCA_separation$ind$coord[,1], 
      #                 y=PCA_separation$ind$coord[,2],
      #                 label=data_PCA_equip$`Sep. Abbrev`))

dev.off()



