library(ggplot2)
library(plyr)
library(reshape2)
library(MSBVAR)
library(zoo)

root <- gsub("(?<=(:|~)/)[[:print:]]+", "", getwd(), perl=T)
locs <- grep("Dropbox/Code$", list.dirs(root,recursive=T), value=T)

data_path <- grep('pak_output.csv',
							 list.files(path = locs[1], 
							 					 all.files = T, 
							 					 full.names = T, 
							 					 recursive = T),
							 value=T)

pak <- read.csv(data_path, as.is = T)

pak <- pak[, c("date", "province", "district",
								"p_p", "p_n", "m_p", "m_n", "u_p", "u_n", "d_a")]

pak$date <- as.Date(pak$date)

# max(pak$date) - as.Date("2005-01-01")

t_template <- data.frame("date" = rev(seq(as.Date('2005-01-01'), 
																					to = max(pak$date), by = 'days')))

pak_t <- merge(t_template, pak, by = c("date"), all.x = T, all.y = T)
pak_t[is.na(pak_t)] <- 0

pak_t <- ddply(pak_t, .variables = "date", function(df)colSums(df[,4:10]))

pakt_melt <- melt(pak_t, 
									id.vars = c("date"), 
									variable.name = "event", 
									value.name = "frequency")

pak_t_diff <- cbind("date" = pak_t$date[-1],
										as.data.frame(apply(pak_t[,grepl("^._.$", colnames(pak_t))], MARGIN = 2,
																				diff)))

# tpd_template <- expand.grid(
# 	"date" = rev(seq(as.Date('2005-01-01'), to = max(pak2$date), by = 'days')),
# 	"loc" = unique(paste(pak2$province, pak2$district, sep = "_"))
# 	)
# tpd_template$province <- gsub("_\\w+$", "", tpd_template$loc)
# tpd_template$district <- gsub("^\\w+_", "", tdp_template$loc)
# tpd_template <- tdp_template[,!grepl("^loc$", colnames(tdp_template))]
# 
# pak_tpd <- merge(tpd_template, pak2, by = c("date", "province", "district"), 
# 							all.x = T, all.y = T)
# pak_tpd[is.na(pak_tpd)] <- 0
# 
# pak_tdp_melt <- melt(pak_tdp, 
# 		 id.vars = c("date", "province", "district"), 
# 		 variable.name = "event", 
# 		 value.name = "frequency")
# 
# pak_tdp_melt$province <- factor(pak_tdp_melt$province, 
# 														 levels = c("fata", "kp", "balochistan", "punjab",
# 														 					 "sindh", "contestedareas"))
# 
# sort(table(pak_tdp_melt[pak_tdp_melt$frequency > 0,"province"]), decreasing = T)
# sort(table(pak_tdp_melt[pak_tdp_melt$frequency > 0,"district"]), decreasing = T)
# 
# ggplot(pak_tdp_melt[pak_tdp_melt$frequency > 0,], 
# 			 aes(x=date)) +
# 			 	geom_histogram(binwidth = 30) +
# 			 	facet_grid(event~province) + 
# 			 	theme_bw()
# 
# pak_tdp_a <- ddply(pak_tdp, .variables = c("date"), .fun = function(x){
# 	out <- colSums(x[,4:10], na.rm = T)
# 	}, .progress = "text")

# ggplot(melt(pak_t_diff, id.vars = "date", variable.name = "event", 
# 						value.name = "frequency"), aes(x = date, y = frequency, color = event)) + 
# 	geom_line(size = 1) +
# 	scale_color_manual(values = c("darkblue", "blue", "darkred", "red", "green", "yellow", "black")) + 
# 	facet_grid(event~.)


################################################################################
# Select lags and hyperparamers based on forecast accuracy
################################################################################

pak_t_diff_z <- zoo(apply(pak_t[,grepl("^._.$", colnames(pak_t))], 
													MARGIN = 2,
													diff), 
										order.by = pak_t$date[-1])

pak_t_diff_z1 <- as.ts(pak_t_diff_z[index(pak_t_diff_z) < as.Date("2012-01-01"),])
pak_t_diff_z2 <- as.ts(pak_t_diff_z[index(pak_t_diff_z) >= as.Date("2012-01-01"),])

lambda0 <- c(0.6)
lambda1 <- c(0.15)
lambda3 <- c(0.5, 1.0, 2.0)
lambda4 <- c(.15)

priors <- expand.grid(lambda0, lambda1, lambda3, lambda4)

p.max <- 7
p.min <- 1

results <- matrix(0, nrow=((p.max - p.min + 1)*nrow(priors)), ncol=8)
colnames(results) <- c("lambda0", "lambda1", "lambda3", "lambda4", 
											 "Lag","RMSE","MAE","Pearsons")

pb <- txtProgressBar(min = 0, max = nrow(results), style = 3)
k <- 0

for (p in p.min:p.max){ 
	for(i in 1:nrow(priors)){ 
		prior.tmp <- data.matrix(priors[i,])
		
		# Estimate the BVAR for the respective prior
		tmp <- szbvar(pak_t_diff_ts_z1, 
									p, 
									z=NULL, 
									lambda0 = prior.tmp[1],
									lambda1 = prior.tmp[2],
									lambda3 = prior.tmp[3],
									lambda4 = prior.tmp[4],
									lambda5 = 0,
									mu5 = 0,
									mu6 = 0, 
									nu = ncol(y),
									qm = 4,
									prior = 0,
									posterior.fit = F)
		
		forecast <-uc.forecast(tmp, nsteps = nrow(pak_t_diff_z2),
													 burnin=1000, gibbs=2000)[[1]]
		
		forecast_mean <- t(sapply(1:dim(forecast)[2], function(i){
			colMeans(forecast[,i,])}))
		
		
		# Compute the posterior summaries
		
		
		
		
		
		k <- k+1
		setTxtProgressBar(pb, k)
		results[k,] <- c(prior.tmp, 0, 0, 0, p, marg.llf, marg.post, bic,
										 aic)
	}
}

################################################################################
# Select lags and hyperparamers based on Information Criteria
################################################################################

pak_t_diff_z <- zoo(apply(pak_t[,grepl("^._.$", colnames(pak_t))], 
													MARGIN = 2,
													diff), 
										order.by = pak_t$date[-1])

pak_t_diff_ts <- as.ts(pak_t_diff_z)


lambda0 <- c(0.6)
lambda1 <- c(0.15)
lambda3 <- c(0.5, 1.0, 2.0)
lambda4 <- c(.15)

priors <- expand.grid(lambda0, lambda1, lambda3, lambda4)

p.max <- 7
p.min <- 1

results <- matrix(0, nrow=((p.max - p.min + 1)*nrow(priors)), ncol=12)
colnames(results) <- c("lambda0", "lambda1", "lambda3", "lambda4", 
											 "mu5", "mu6", "v0", 
											 "Lag","ML","POST","BIC","AIC")

pb <- txtProgressBar(min = 0, max = nrow(results), style = 3)
k <- 0

for (p in p.min:p.max){ 
	for(i in 1:nrow(priors)){ 
		prior.tmp <- data.matrix(priors[i,1:4])
		
		# Estimate the BVAR for the respective prior
		tmp <- szbvar(pak_t_diff_ts, 
									p, 
									z=NULL, 
									lambda0 = prior.tmp[1],
									lambda1 = prior.tmp[2],
									lambda3 = prior.tmp[3],
									lambda4 = prior.tmp[4],
									lambda5 = 0,
									mu5 = 0,
									mu6 = 0, 
									nu = ncol(y),
									qm = 4,
									prior = 0,
									posterior.fit = T)
		
		# Compute the posterior summaries
		marg.llf <- tmp$marg.llf        
		bic <- 2*marg.llf + prod(dim(tmp$Bhat))*(nrow(y)-p)
		aic <- -2*marg.llf + 2*prod(dim(tmp$Bhat))
		marg.post <- tmp$marg.post
		k <- k+1
		setTxtProgressBar(pb, k)
		results[k,] <- c(prior.tmp, 0, 0, 0, p, marg.llf, marg.post, bic,
										 aic)
	}
}

results[order(results[,10], decreasing=T),]
results[order(results[,11], decreasing=F),]
results[order(results[,12], decreasing=F),]











