################.################
#### PHASE SEPARATION STATS ####
################.################

#clean variables ###################
rm(list=ls(all=TRUE)) 

#### 0.PREP ####
#0.1 loading libraries####
libraries=c("Hmisc","colorspace","ggbiplot","corrplot","pvclust","mixOmics","RColorBrewer",
            "FactoMineR","missMDA","FactoMineR","ggplot2", "tidyr",
            "gplots","scales","grid","plyr","gridExtra",
            "devtools","factoextra","ggrepel", "viridis",
            "WriteXLS","dendextend","cluster","reshape2","readxl","pvclust","pls")

install_this=libraries[which(lapply(libraries, require, character.only = TRUE) == F)]
install.packages(install_this)

###0.2 ADJUST THE FOLDER###############################################################################################################################

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

#### 0.3 Importing data ####
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
                                     range="H1:J13")
data_feed_group = read_excel(path = paste0(data_wd,file_name),
                              sheet = "Group_feed",
                              col_names = T,
                              na = "NA",
                              range="G1:I11")
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

list_distrib_data = list(All = data_distrib_clean,
                         Low = data_distrib_clean_low,
                         High = data_distrib_clean_high)


list_PCA_data = list(PCA_efficiency=data_efficiency)
  #PCA_short=data_PCA_short,
                     #PCA_extra_FM=data_PCA_extra_raw_FM,
                     #PCA_extra_DM=data_PCA_extra_raw_DM,
                     #PCA_extra_all_FM=data_PCA_extra_all_FM,
                      #PCA_extra_all_DM=data_PCA_extra_all_DM)
#### 0.3.1 check that ids are different ####
table(colnames(data_sep_raw))
table(colnames(data_distrib_clean))

#### 0.3.2 check class of columns ####
which(lapply(data_sep_raw, class) != "numeric")
#### 0.3.3 force into given class ####
#selec_cols = c(x,x,x,x)
#data_sep_raw[, selec_cols] <- sapply(data_sep_raw[, selec_cols], as.numeric)



#### 1A. General plots ####
#### 1A.1 Overal distribution ####
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
data_distrib_LF_av = data_distrib_melt[data_distrib_melt$variable == "LF av.",]
data_distrib_LF_up = data_distrib_LF_av$value + data_distrib_melt[data_distrib_melt$variable == "LF sd",]$value
data_distrib_LF_low = data_distrib_LF_av$value - data_distrib_melt[data_distrib_melt$variable == "LF sd",]$value

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
  coord_flip() +
  scale_y_continuous(labels = abs, limits = c(-100,100)) +
  scale_fill_manual(labels = c("Liquid Fra.","Solid Fra."),values = c("#6fb3ca","#ca866f")) + 
  labs(title = paste0("Mass balance. Efficiency group: ",name_data_distrib),
       y="Distribution (%)", x="Variable", fill="Fraction")
)
dev.off()
}

#### 1A.2 PLOT 2: EFFICIENCY BOXPLOTS BY EQUIP, DM ONLY ####
data_boxplot_eff = cbind.data.frame(data_efficiency$`ID separation`,
                                    data_efficiency$`Sep. Abbrev`,
                                    data_efficiency[,13:26])
colnames(data_boxplot_eff)[1:2]=c("ID","Sep._Abbrev")
data_boxplot_eff_melt = melt(data_boxplot_eff, 
                             id.vars = c(1,2), 
                             measure.vars = 3:16)
equip_greater_than = names(which(summary(data_boxplot_eff$Sep._Abbrev)>2))
data_boxplot_eff_melt=data_boxplot_eff_melt[data_boxplot_eff_melt$Sep._Abbrev %in% equip_greater_than,]
adapt_var1 = function(x) {return(substr(x,1,regexpr("_",x)-1))}
adapt_var2 = function(x) {return(substr(x,regexpr("_",x)+1,nchar(as.character(x))))}
data_boxplot_eff_melt=mutate(data_boxplot_eff_melt,variable1=adapt_var1(variable))
data_boxplot_eff_melt=mutate(data_boxplot_eff_melt,variable2=adapt_var2(variable))
data_boxplot_eff_melt$variable = factor(data_boxplot_eff_melt$variable, rev(unique(data_boxplot_eff_melt$variable)) )

plot=ggplot(data=data_boxplot_eff_melt, aes(x=variable2, y=value))+geom_boxplot(aes(fill=variable1))

give.n <- function(x){
  return(c(y = 1.02*as.numeric(quantile(x,prob=0.75)), label = length(x)))}
mean.n <- function(x){
  return(c(y = mean(x), label = round(mean(x),2)))} 

pdf(paste0(wd,"/Initial_Boxplots/","Efficiency_singleplot.pdf"), width = 7, height = 4)
plot(plot + 
      facet_wrap(~Sep._Abbrev ,scales="free",nrow = 1, ncol = NULL) + #,labeller=labeller(.cols=labelsVAR))
      labs(title="Boxplots for sep. equip.",x="Eff. indicator",y="Values in different scale") +
      theme(plot.title = element_text(size = rel(1.5), colour = "black"),
             legend.title=element_blank(),
             legend.position="bottom",
             legend.direction="horizontal") + 
      guides(fill = guide_legend(nrow = 2)) +
      stat_summary(fun.data = give.n, geom = "text", aes(shape="Nb. of ind."), fun.y = median,show.legend=FALSE, size=3 ) +
      stat_summary(fun.data = mean.n, aes(shape="Mean"), colour = "black", geom="point") +
      scale_shape_manual("", values=c("Mean"="x","Nb. of ind."="N"))
     #+ stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, colour = "red")
)
dev.off()
#### 1B.2 PLOT 2: EFFICIENCY BOXPLOTS BY EQUIP, OTHER VARS ####


#### PLOT: GROUPS AND CHOICE OF EQUIPMENT ####
data_equip_group = data_equip_group[order(data_equip_group$Equip),]
data_equip_group = data_equip_group[order(data_equip_group$Group),]
data_equip_group = data_equip_group[rev(1:nrow(data_equip_group)),]

data_equip_group <- ddply(data_equip_group, "Group",
                   transform, label_ypos=cumsum(N)-0.5*N)
data_equip_group <- ddply(data_equip_group, "Group",
                          transform, percent=round(100*N/sum(N),0))
label_plotg=paste0("n=",data_equip_group$N,". ",data_equip_group$percent,"%")

plot_equip = ggplot(data_equip_group, aes(x=Group, y=N,fill=Equip)) +
geom_bar(stat="identity") +
geom_text(aes(y=label_ypos, label=label_plotg), color="black", size=3.5) +
  scale_fill_brewer(palette="Paired") +
theme(legend.position="right")+
  theme_minimal()

pdf(paste0(wd,"/Group_eff_equip.pdf"),width = 5.4, height = 5.4)
print(plot_equip)
dev.off()  

#### PLOT: FEED AND CHOICE OF EQUIPMENT ####
data_feed_group = data_feed_group[order(data_feed_group$Feed),]
data_feed_group = data_feed_group[order(data_feed_group$Group),]
data_feed_group = data_feed_group[rev(1:nrow(data_feed_group)),]

data_feed_group <- ddply(data_feed_group, "Group",
                          transform, label_ypos=cumsum(N)-0.5*N)
data_feed_group <- ddply(data_feed_group, "Group",
                          transform, percent=round(100*N/sum(N),0))
label_plotg=paste0("n=",data_feed_group$N,". ",data_feed_group$percent,"%")

plot_feed = ggplot(data_feed_group, aes(x=Group, y=N,fill=Feed)) +
  geom_bar(stat="identity") +
  geom_text(aes(y=label_ypos, label=label_plotg), color="black", size=3.5) +
  scale_fill_brewer(palette="Paired") +
  theme(legend.position="right")+
  theme_minimal()

pdf(paste0(wd,"/Group_eff_feed.pdf"),width = 5.4, height = 5.4)
print(plot_feed)
dev.off() 
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
data_hist=data.frame("1-TSliq/TSraw"=data_sep_raw$`1-Tsliq/Tsraw`,"DM to SF"=data_sep_raw$DM_to_SF)
colnames(data_hist)=c("1-TSliq/TSraw","DM to SF")
data_hist=melt(data_hist)
eff1 = data_hist[data_hist$variable=="1-TSliq/TSraw",]
eff2 = data_hist[data_hist$variable=="DM to SF",]
# 1.2.1.1 PLOTS ####
pdf(paste0(wd,"/Efficiency_histogram/","Histogram_1-TS.TS.pdf"))
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
  labs(title = "Separation efficiency densities",x="Value", y="Density")
dev.off()




### testing significance of groups ###
funct_eff = function(x) {
  (if (is.na(x)) { result=NA 
  } else if (x > 0.5) {
    result="High"
  } else {
    result="Low" } )
  return(result) 
}    ## CREATED FOR "1 - TS/TS"
groups = apply(matrix(as.numeric(data_sep_raw$`1-Tsliq/Tsraw`)),1,funct_eff)
data_groups = data.frame(value=data_sep_raw$`1-Tsliq/Tsraw`,group=groups)
#data_groups = data_groups[-56,]

# 1.2.1.2 t-test  of groups ####
# first verifying normality #
ddply(.data = data_groups, ~group, summarize, mean=mean(value))
data_low = data_groups[data_groups$group == "Low",]$value
data_high = data_groups[data_groups$group == "High",]$value
shapiro_low_eff = shapiro.test(data_low)
shapiro_high_eff = shapiro.test(data_high)
hist(data_low)
hist(data_high)
# t-test and wilcox test
var.test(data_low,data_high) #pv > 0.05 -> can assume homogenous variances
t_test_groups = t.test(data_low,data_high)
wilcox.test(data_low,data_high)
# ANOVA
groups_model = lm(formula = value ~ group, data = data_groups)
summary(groups_model)
anova(groups_model)
confint(groups_model)

#como ler: 
# 1) se o p-value > 0.15, nao invalida que o modelo nao é normal: pvalue do ttest pode ser usado
# 2) p < 0.10; teste de adequaçao invalida o modelo normal: atençao ao usar o pv do ttest
# 3) entre 0.10 e 0.15 (caso do exemplo): zona de perigo, melhor usar outro teste

#### 1.2.2 PLOT 4: MASS BALANCES FOR EFFICIENCY GROPS ####
## in the same loop as plot 1 ###


#### ERRORS ON MASS BALANCES ####
data_errors_melt=melt(data_errors,id.vars = c("ID","Eff_group"))
selected_err_var = c("VS","TN","TAN","P","K")
data_errors_melt=data_errors_melt[data_errors_melt$variable %in% selected_err_var,]

plot_errors=ggplot(data=data_errors_melt, 
                   aes(x=variable, y=value))+ geom_boxplot()+#geom_dotplot(binaxis = "y",stackdir = "center") +
  labs(title="Boxplots for mass balance errors",x="Variable",y="Errors") +
  stat_summary(fun.data = mean.n, aes(shape="Mean"), colour = "black", geom="point") +
  scale_shape_manual("", values=c("Mean"="x","Nb. of ind."="N")) +
  geom_hline(yintercept=0.1, color="red") +
  geom_hline(yintercept=-0.1, color="red") +
  theme(legend.position = "bottom")

pdf(paste0(wd,"/ERRORS/","ERRORS_singleplot.pdf"), width = 7, height = 4)
plot(plot_errors)
dev.off()

#TESTANDO SE MEDIA ERROS SIGNIF MAIOR OU MENOR QUE QUE 0 #
for (j in 1:length(selected_err_var)) {
  if (j == 1) {
  table_wilcox_test_err = data.frame(0,0,0,0,0,0)
  colnames(table_wilcox_test_err)=c("Var","Option","PV (Wilcox)","N","mean (%)","SD (%)")
  }
  i = selected_err_var[j]
  print(i)
  line_table = nrow(table_wilcox_test_err)
  if (j > 1) {line_table = line_table+1}
  table_wilcox_test_err[line_table,1] = i
  table_wilcox_test_err[line_table+1,1] = i
  table_wilcox_test_err[line_table+2,1] = i
  
  table_wilcox_test_err[line_table,2] = "Diff."
   table_wilcox_test_err[line_table+1,2] = "Greater"
  table_wilcox_test_err[line_table+2,2] = "Less"
  data_err_temp = data_errors_melt[data_errors_melt$variable == i,4]
  data_err_temp = data_err_temp[!is.na(data_err_temp)]
  
  wilcoxtest_err_diff = wilcox.test(data_err_temp, mu = 0) #2sided is default
  wilcoxtest_err_greater = wilcox.test(data_err_temp, mu = 0, alternative= "greater")
 # print(wilcoxtest_err_greater)
  wilcoxtest_err_less = wilcox.test(data_err_temp, mu = 0, alternative = "less")
 # print(wilcoxtest_err_less)
  table_wilcox_test_err[line_table,3] = round(wilcoxtest_err_diff$p.value,3)
  table_wilcox_test_err[line_table+1,3] = round(wilcoxtest_err_greater$p.value,3)
  table_wilcox_test_err[line_table+2,3] = round(wilcoxtest_err_less$p.value,3)
  table_wilcox_test_err[line_table,4] = length(data_err_temp)
  table_wilcox_test_err[line_table,5] = round(100*mean(data_err_temp),1)
  table_wilcox_test_err[line_table,6] = round(100*sd(data_err_temp),1)
  }
WriteXLS(table_wilcox_test_err,paste0(wd,"/ERRORS/","ERRORS_singleplot.xlsx"))

#### 1.2.3 PLOT 5: PCA OF SEPARATION EFFICIENCY ####
# estimating ncp and replacing NA


for (i in 1:length(list_PCA_data)) {
data_PCA = as.data.frame(list_PCA_data[[i]])
name_data_PCA = names(list_PCA_data)[i]
data_PCA_numeric = data_PCA[,-1:-3]
data_PCA_numeric = sapply(data_PCA_numeric, as.numeric)
groups_temp = apply(matrix(as.numeric(data_PCA$`1-Tsliq/Tsraw`)),1,funct_eff)

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
write_PCA_data=c(name_data_PCA,"dataPCA_complete_obs","eigenvalues","loading_matrix","PC1corr","PC2corr","PC3corr","aka")
WriteXLS(write_PCA_data, 
         ExcelFileName = paste(wd,"/PCA/",name_data_PCA,"_output_data.xls",sep=""), 
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
                      habillage=as.factor(groups_temp),
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



