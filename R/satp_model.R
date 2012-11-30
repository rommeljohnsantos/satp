library(ggplot2)
library(plyr)
library(reshape2)
library(MSBVAR)
library(zoo)

# Normal-IW prior pdf
BVAR.informed <- szbvar(pak_t_diff_ts, p=14, z=NULL, lambda0=0.6,
												lambda1=0.2, lambda3=1, lambda4=0.2, lambda5=0,
												mu5=0, mu6=0, nu=ncol(pak_t_diff_ts)+1, qm=4, 
												prior=0, posterior.fit=F)

irf.informed <- mc.irf(BVAR.informed, 30, 1000)

ggirf <- function(x){
	
	var_names <- rev(expand.grid(
		"response" = attr(x, "eqnames"),
		"impulse" = attr(x, "eqnames"), 
		stringsAsFactors = F))
	
	x2 <- lapply(1:dim(irf.flat)[2], function(i){
		cbind(melt(cbind(t(x[,i,]),var_names), id.vars = c("impulse", "response"), 
							 variable.name = "iteration"), "lag" = i)
	})
	
	df <- do.call("rbind", x2)
	df$impulse <- factor(df$impulse, levels = attr(x, "eqnames"))
	df$response <- factor(df$response, levels = attr(x, "eqnames"))
	
	ggplot(df, aes(x = lag, y = value, group = iteration)) +
		theme_bw() +
		geom_line(alpha = .1, color = "dodgerblue", size = .5) + 
		facet_grid(response~impulse)
}

################################################################################
# Plot the impulse responses -- by various methods
################################################################################

# Repsonses for the reference prior BVAR using the different error band
# construction methods.


informed.impulse.percentile <- plot(irf.flat, method=c("Percentile"), 
																		probs=c(0.025,0.16,0.84,0.975), varnames=colnames(pak_t_diff_ts))
title(main="Quantiles Method", outer=T, line=5)


informed.impulse.percentile <- plot(irf.informed, method=c("Percentile"), 
																		probs=c(0.025,0.16,0.84,0.975), varnames=colnames(pak_t_diff_ts))
title(main="Quantiles Method", outer=T, line=5)

informed.impulse.eigen.full <- plot(irf.informed, method=c("Sims-Zha3"), component=1,
																		probs=c(0.025,0.16,0.84,0.975), varnames=colnames(pak_t_diff_ts))
title(
	main="Stacked Eigen Decomposition Method, Accounting for Correlated Repsonses",
	outer=T, line=5)
