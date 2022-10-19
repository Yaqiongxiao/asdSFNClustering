# function to plot ROI and clinical test data across clusters 

clusters_plot <- function(ROI_clinic_clusters, savepath) {

	# DESCRIPTION
	# This function plots boxplot graph by cluster/index, with invidual points 
	# showing percentage signal change values (ROI variables) or clinical scores 
	# (clinical variables)   
	# 
	# INPUT ARGUMENTS
	# dat = data frame to process
	# ROI_var = a string denoting ROI variables
	# clinic_var = a string denoting clinical variables
	#
	# OUTPUT
	# a boxplot graph with inidividual values/scores
	# 
	# load libraries
	require(ggplot2)
	
	# reorganize data file for plotting
	ROI_clinic_clusters_long <- gather(ROI_clinic_clusters, test, values, LHfrontal:RHtemporal)
	head(ROI_clinic_clusters_long)
	
	ROI_clinic_clusters_long$index <- as.factor(ROI_clinic_clusters_long$index)
	ROI_clinic_clusters_long$test <- factor(ROI_clinic_clusters_long$test, 
					 levels = c("LHfrontal","RHfrontal","LHtemporal",
					 	   "RHtemporal"))
	
	## barplots of %signal changes
	
	ROI_clinic_clusters_long_run <- ROI_clinic_clusters_long
	tmp <- ROI_clinic_clusters_long_run

	p_ROIs_barplot <- ggplot(tmp, aes(x = test, y = values, group = index, fill = index)) + 
		geom_bar(width = 0.8, stat = "summary",fun = "mean", color = "black", position = position_dodge(width = 0.8)) + 
		geom_errorbar(width = 0.6, stat = "summary", fun.data = "mean_se", position = position_dodge(width = 0.8)) +
		labs(y = "% Signal Change [Speech vs. Rest]", x = "", title = "ROI activation") +
		guides(fill = "none") +
		theme(legend.title = element_text(colour="black", size=14, face="bold"),
		      legend.text = element_text(colour="black", size=14, face="bold")) +
		theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
		theme(axis.text.x = element_blank(),
		      axis.ticks.x = element_blank(),
		      axis.text.y = element_text(size = 10, face = "bold"),
		      axis.title.y = element_text(size = 10, face = "bold")) +
		scale_fill_hue(name = "Cluster") + 
		theme(panel.background = element_blank(),
		      panel.grid = element_blank(),
		      panel.border = element_blank(),
		      axis.line = element_line(colour = "black")) + # remove background
		coord_cartesian(ylim=c(-0.04,0.1)) + 
		scale_y_continuous(breaks = seq(-0.04,0.1, 0.02))	
	
	print(p_ROIs_barplot)	
	ggsave(paste0(savepath,"/boxplot_ROIs.png"), width = 4, height = 3,units = c("in"), dpi = 200)

	## organize data for plotting clinical data
	kk <- colnames(dplyr::select(ROI_clinic_clusters, contains("final")))
	
	#ROI_clinic_clusters_long <- gather(ROI_clinic_clusters, test, values, kk[1]:kk[length(kk)])
	ROI_clinic_clusters_long <- gather(ROI_clinic_clusters, test, values, kk[1]:kk[6])
	head(ROI_clinic_clusters_long)
	
	ROI_clinic_clusters_long$index <- as.factor(ROI_clinic_clusters_long$index)
	
	ROI_clinic_clusters_long$test <- factor(ROI_clinic_clusters_long$test,
						levels =c("final_ados_CoSoTot","final_ados_RRTot",          
						"final_vine_AdapBehav_DomStd", "final_mullen_RLT",
						"final_mullen_ELT","final_mullen_ELC_Std"))
	
	# plot clinical data
	
	titles <- c("ADOS SA", "ADOS RRB", "Vineland ABC","Mullen RLT",
		    "Mullen ELT","Mullen ELC")

	for (i in 1:length(titles)) {
		test <- levels(ROI_clinic_clusters_long$test)[i]
		tmp <- ROI_clinic_clusters_long[ROI_clinic_clusters_long$test == test, ]
		#tmp <- tmp[tmp$values != 0, ]
		
		p_clinic <- ggplot(tmp, aes(x = index, y = values, fill = index)) +
			geom_boxplot(width = 0.9,outlier.shape = NA) +
			geom_point(aes(col = index), color = "black", size = 3, alpha = 0.6, position = position_jitterdodge(dodge.width = 1)) +
			labs(y = " ", x = "Cluster") + # title = titles[i]
			guides(fill = "none") +
			theme(legend.title = element_text(colour="black", size=14, face="bold"),
			      legend.text = element_text(colour="black", size=14, face="bold")) +
			theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
			theme(axis.text.y = element_text(size = 16, face = "bold"),
			      axis.title.y = element_text(size = 16, face = "bold")) +
			theme( axis.text.x = element_blank(),  axis.title.x = element_blank(),
			       axis.ticks.x = element_blank()) + 
			theme(panel.background = element_blank(),
			      panel.grid = element_blank(),
			      panel.border = element_blank(),
			      axis.line = element_line(colour = "black"))
		
	print(p_clinic)	
	ggsave(paste0(savepath,"/boxplot_",test,".png"), width = 4, 
	       height = 3,units = c("in"), dpi = 200)
	}
	
	
	return(list(nn,ROI_clinic_clusters,ROI_clinic_clusters_long_run))
}
