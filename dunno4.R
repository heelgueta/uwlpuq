# Fit the 2PL IRT model for biocf
irt_model_biocf <- ltm::ltm(irt_data_biocf ~ z1, IRT.param = TRUE)

# Model fit
print(summary(irt_model_biocf))

# Item fit (using residuals)
item_fit_biocf <- residuals(irt_model_biocf, type = "Q1")
print(item_fit_biocf)

# Plot Item characteristic curves (ICC)
plot(irt_model_biocf, type = "ICC", item = 1)
plot(irt_model_biocf, type = "ICC")

factor_scores_biocf <- ltm::factor.scores(irt_model_biocf,return.MIvalues = T)
factor_scores_biocf$score.dat
ltm::factor.scores(irt_model_biocf,return.MIvalues = T)$score.dat$var.zvalues.MI


idf_with_scores <- merge(idf, ltm::factor.scores(irt_model_biocf, return.MIvalues = TRUE)$score.dat, by = c("wlim01", "wlim02", "wlim03", "wlim04"))
hist(idf_with_scores$z1)
hist(idf_with_scores$biocf)
hist(idf_with_scores$humcf)
unique(idf_with_scores$z1)
unique(idf_with_scores$biocf)
cor(idf_with_scores$z1,idf_with_scores$biocf)
cor(idf_with_scores$biocf,idf_with_scores$natcf)
cor(idf_with_scores$z1,idf_with_scores$natcf)

# Fit the 2PL IRT model for humcf
irt_model_humcf <- ltm::ltm(irt_data_humcf ~ z1, IRT.param = TRUE)

# Model fit
print(summary(irt_model_humcf))

# Item fit (using residuals)
item_fit_humcf <- residuals(irt_model_humcf, type = "Q1")
print(item_fit_humcf)

# Plot Item characteristic curves (ICC)
plot(irt_model_humcf, type = "ICC")
