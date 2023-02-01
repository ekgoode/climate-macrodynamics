################################################################################
# This program creates all figures (excluding maps)
#
# Author: Greg Casey, Stephie Fried, Ethan Goode
# Last Edited: 1/13/2023
################################################################################

################################################################################
# Section 0: Pull-in boot-strap results
################################################################################
tfp_le_boot <- read.csv(here('data','output','Result_bootstrap_tfp_le.csv'))

world_tfp_le_ci <- tfp_le_boot %>%
  filter(country == 'China' & boots == 1) %>%
  select(country, year_vec, world_lower95, world_upper95) %>%
  mutate(year_vec = as.character(year_vec))

tfp_le_joined <- right_join(resultsAll_model_reg2, world_tfp_le_ci, by=c('year_vec','country'))

################################################################################
# Figure 6 - Impact of Climate Change on World GDP per Capita
################################################################################
 worldY_fig1 <- ggplot(data=tfp_le_joined,aes(x=as.numeric(year_vec))) +
   geom_ribbon(aes(ymin=world_lower95,ymax=world_upper95), fill="grey") +
   geom_line(aes(y=world_Y_diff), color = "red", size = 1.2) +
   geom_segment(aes(x=2011,xend=2100,y=0,yend=0), linetype = 'dashed', size = 0.5) +
   labs(x = "Year",
        y = "Difference in GDP per Capita (percent)",
        title = "") +
   xlim(2011,2100) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(2010,2100, by = 10), limits = c(2010, 2104)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(-7,1, by = 1), limits = c(-7.2, 1)) +
   theme_ekg() +
   theme(
     axis.text.x = element_text(size = 22),
     axis.text.y = element_text(size = 22),
     axis.title.x = element_text(size = 24),
     axis.title.y = element_text(size = 24),
     plot.margin = margin(0, 0, 0, 0, "pt")
   )
 ggsave(filename = here('results','figures', 'worldY_level_effects_model.eps'), plot = worldY_fig1, units = "in", height = 8, width = 10, dpi = 300, device = "eps")

 ################################################################################
 # Figure 8: Comparison of the Impact of Climate Change on World GDP per capita
 ################################################################################
results_plot_3 <- resultsAll_model_reg3 %>%
   rename('world_Y_diff3' = 'world_Y_diff')
 
 results_plot_2 <- resultsAll_model_reg2 %>%
   rename('world_Y_diff2' = 'world_Y_diff')
 
 colors <- c('Growth and level effects' = '#6baed6', 'Level effects' = '#08519c')
 
 worldY_compare <- ggplot(data=resultsAll_model_reg1,aes(x=as.numeric(year_vec))) +
  geom_line(data=results_plot_3, aes(y=world_Y_diff3, color = 'Growth and level effects'), size = 1.2, linetype = 'dashed') +
  geom_line(data=results_plot_2, aes(y=world_Y_diff2, color = 'Level effects'), size = 1.2, linetype = 'solid') +
  geom_segment(aes(x=2010,xend=2100,y=0,yend=0), linetype = 'solid', size = 0.5) +
   scale_color_manual(values=colors) +
  labs(x = "Year",
       y = "Difference in GDP per Capita (percent)",
       title = "") +
  theme_ekg() +
  scale_x_continuous(expand = c(0, 0), breaks = seq(2010,2100, by = 10), limits = c(2010, 2104)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(-5,15, by = 5), limits = c(-5.5, 15)) +
  theme(
    axis.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 22),
    axis.title.x = element_text(size = 24),
    axis.title.y = element_text(size = 24),
    legend.position = c(0.3,0.8),
    legend.background = element_blank(),
    legend.key = element_rect(fill=NA),
    legend.key.size = unit(1, "cm"),
    legend.text = element_text(size = 20)
    ) 
ggsave(filename = here('results','figures', 'worldY_effects_comparison.eps'), plot = worldY_compare, units = "in", height = 8, width = 10, dpi = 300, device = "eps")

################################################################################
# Figure 2: Optimal Temperature
################################################################################

#Pull betas
beta_1gph <- as.numeric(tfp_regs[[3]][tfp_regs$var == 'dtemp'])
beta_2gph <- as.numeric(tfp_regs[[3]][tfp_regs$var == 'dtemp2'])

#Instantiate vectors for data
optTempgph <- c()
temp_vec <- c()

#Set annotation text size
annotate_size <- 6

#Loop over desired range of temperatures
for (temps in -5:35) {
  yvalit <- beta_1gph*temps + (beta_2gph*(temps^2))
  optTempgph <- c(optTempgph,yvalit)
  temp_vec <- c(temp_vec, temps)
}

#Convert to df
fig4_df <- as.data.frame(cbind(temp_vec, optTempgph))
v_colors =  rev(viridis(1, option = 'viridis'))
#Plot
fig4 <- ggplot() +
  geom_line(data=fig4_df,aes(x=temp_vec, y=optTempgph),  size = 1.2, color='black') +
  geom_vline(xintercept=df_sim$temp[(df_tfp$countryname == 'United States' & df_tfp$year == 2010)]) +
  geom_segment(aes(x=df_sim$temp[(df_tfp$countryname == 'United States' & df_tfp$year == 2010)]-1,y=0.05,xend=df_sim$temp[(df_tfp$countryname == 'United States' & df_tfp$year == 2010)],yend=0.05), arrow = arrow(length = unit(0.3, "cm")), size = 0.75) +
  annotate("text", x=df_sim$temp[(df_tfp$countryname == 'United States' & df_tfp$year == 2010)]-2, y= 0.05, label = 'US', size = annotate_size) +
  geom_vline(xintercept=df_sim$temp[(df_tfp$countryname == 'France' & df_tfp$year == 2010)]) +
  geom_segment(aes(x=df_sim$temp[(df_tfp$countryname == 'France' & df_tfp$year == 2010)]-1,y=0.03,xend=df_sim$temp[(df_tfp$countryname == 'France' & df_tfp$year == 2010)],yend=0.03), arrow = arrow(length = unit(0.3, "cm")), size = 0.75) +
  annotate("text", x=df_sim$temp[(df_tfp$countryname == 'France' & df_tfp$year == 2010)]-2.75, y= 0.03, label = 'France', size = annotate_size) +
  geom_vline(xintercept=df_sim$temp[(df_tfp$countryname == 'Ethiopia' & df_tfp$year == 2010)]) +
  geom_segment(aes(x=df_sim$temp[(df_tfp$countryname == 'Ethiopia' & df_tfp$year == 2010)]-1,y=0.04,xend=df_sim$temp[(df_tfp$countryname == 'Ethiopia' & df_tfp$year == 2010)],yend=0.04), arrow = arrow(length = unit(0.3, "cm")), size = 0.75) +
  annotate("text", x=df_sim$temp[(df_tfp$countryname == 'Ethiopia' & df_tfp$year == 2010)]-3, y= 0.04, label = 'Ethiopia', size = annotate_size) +
  geom_vline(xintercept=df_sim$temp[(df_tfp$countryname == 'China' & df_tfp$year == 2010)]) +
  geom_segment(aes(x=df_sim$temp[(df_tfp$countryname == 'China' & df_tfp$year == 2010)]+1,y=0.02,xend=df_sim$temp[(df_tfp$countryname == 'China' & df_tfp$year == 2010)],yend=0.02), arrow = arrow(length = unit(0.3, "cm")), size = 0.75) +
  annotate("text", x=df_sim$temp[(df_tfp$countryname == 'China' & df_tfp$year == 2010)]+2.7, y= 0.02, label = 'China', size = annotate_size) +
  geom_vline(xintercept=df_sim$temp[(df_tfp$countryname == 'India' & df_tfp$year == 2010)]) +
  geom_segment(aes(x=df_sim$temp[(df_tfp$countryname == 'India' & df_tfp$year == 2010)]+1,y=0.03,xend=df_sim$temp[(df_tfp$countryname == 'India' & df_tfp$year == 2010)],yend=0.03), arrow = arrow(length = unit(0.3, "cm")), size = 0.75) +
  annotate("text", x=df_sim$temp[(df_tfp$countryname == 'India' & df_tfp$year == 2010)]+2.3, y= 0.03, label = 'India', size = annotate_size) +
  geom_vline(xintercept=df_sim$temp[(df_tfp$countryname == 'Brazil' & df_tfp$year == 2010)]) +
  geom_segment(aes(x=df_sim$temp[(df_tfp$countryname == 'Brazil' & df_tfp$year == 2010)]+.75,y=0.045,xend=df_sim$temp[(df_tfp$countryname == 'Brazil' & df_tfp$year == 2010)],yend=0.05), arrow = arrow(length = unit(0.3, "cm")), size = 0.75) +
  annotate("text", x=df_sim$temp[(df_tfp$countryname == 'Brazil' & df_tfp$year == 2010)]+2, y= 0.042, label = 'Brazil', size = annotate_size) +
  labs(x = "Temperature (Degrees C)",
       y = "",
       title = "") +
  xlim(-5,35) +
  theme_ekg() +
  theme(
    axis.text.x = element_text(size = 24),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(size = 28),
    axis.title.y = element_text(size = 28),
    legend.position = "none"
  )
ggsave(filename = here('results','figures', 'optTemp_standard.eps'), plot = fig4, units = "in", height = 8, width = 10, dpi = 300, device = "eps")


################################################################################
# Figure(s) C1: Marginal Effects
################################################################################

#Pull betas
beta_1gph <- as.numeric(tfp_regs[[4]][tfp_regs$var == 'dtemp'])
beta_2gph <- as.numeric(tfp_regs[[4]][tfp_regs$var == 'dtemp2'])
gamma_1gph <- as.numeric(tfp_regs[[4]][tfp_regs$var == 'temp'])
gamma_2gph <- as.numeric(tfp_regs[[4]][tfp_regs$var == 'temp2'])

#Instantiate vectors for data
optTempgph <- c()
optTempgph2 <- c()
temp_vec <- c()

#Set annotation text size
annotate_size <- 6

#Loop over desired range of temperatures
for (temps in -5:35) {
  yvalit <- beta_1gph + (2*beta_2gph*(temps))
  yvalit2 <-  gamma_1gph + (2*gamma_2gph*(temps))
  optTempgph <- c(optTempgph,yvalit)
  optTempgph2 <- c(optTempgph2,yvalit2)
  temp_vec <- c(temp_vec, temps)
}

#Convert to df
fig5_df <- as.data.frame(cbind(temp_vec, optTempgph, optTempgph2))
v_colors =  rev(viridis(1, option = 'viridis'))

#Plot
fig5 <- ggplot(data=fig5_df) +
  geom_line(aes(x=temp_vec, y=optTempgph),  size = 1.2, color='black') +
  labs(x = "Temperature (Degrees C)",
       y = expression(beta[1] + 2 * beta[2] * "T"),
       title = "Marginal Level Effects") +
  scale_y_continuous(limits=c(-0.02,0.015),
                     breaks = seq(-0.02,0.015,by=0.005),
                     expand = c(0,0)) +
  xlim(-5,35) +
  theme_ekg() +
  theme(
    axis.text.x = element_text(size = 24),
    axis.text.y = element_text(size = 24),
    axis.title.x = element_text(size = 28),
    axis.title.y = element_text(size = 28),
    plot.title = element_text(hjust = 0.5, size=28),
    legend.position = "none"
  )
fig5 <- shift_axis(fig5,0) + theme(axis.line.x=element_blank())
ggsave(filename = here('results','figures', 'marginal_effects_dTemp.eps'), plot = fig5, units = "in", height = 8, width = 10, dpi = 300, device = "eps")

#Plot
fig6 <- ggplot(data=fig5_df) +
  geom_line(aes(x=temp_vec, y=optTempgph2),  size = 1.2, color='black') +
  geom_hline(yintercept = 0, size = 1) +
  labs(x = "Temperature (Degrees C)",
       y = expression(gamma[1] + 2 * gamma[2] * "T"),
       title = "Marginal Growth Effects") +
  scale_y_continuous(limits=c(-0.0005,0.0020),
                     breaks = seq(-0.0005,0.0020,by=0.0005),
                     expand = c(0,0)) +
  xlim(-5,35) +
  theme_ekg() +
  theme(
    axis.text.x = element_text(size = 24),
    axis.text.y = element_text(size = 24),
    axis.title.x = element_text(size = 28),
    axis.title.y = element_text(size = 28),
    plot.title = element_text(hjust = 0.5, size=28),
    legend.position = "none"
  )
fig6 <- shift_axis(fig6,0) + theme(axis.line.x=element_blank())
ggsave(filename = here('results','figures', 'marginal_effects_Temp.eps'), plot = fig6, units = "in", height = 8, width = 10, dpi = 300, device = "eps")


################################################################################
# Figure(s) C2/3: Time Fixed Effects
################################################################################
tfe_tfp <- tfp_regs %>%
  filter(str_detect(var,".year")) %>%
  mutate(year = as.numeric(substr(var,1,4)))

tfe_gdppc <- gdppc_regs %>%
  filter(str_detect(var,".year")) %>%
  mutate(year = as.numeric(substr(var,1,4)))

#Get ymin & ymax for y-axis scale on graphs 
ymin_fig7 <- round(min(as.numeric(tfe_tfp$reg1), na.rm = TRUE),4)
ymax_fig7 <- round(max(as.numeric(tfe_tfp$reg1), na.rm = TRUE),4)
ymin_fig8 <- round(min(as.numeric(tfe_tfp$reg2), na.rm = TRUE),4)
ymax_fig8 <- round(max(as.numeric(tfe_tfp$reg2), na.rm = TRUE),4)
ymin_fig9 <- round(min(as.numeric(tfe_tfp$reg3), na.rm = TRUE),4)
ymax_fig9 <- round(max(as.numeric(tfe_tfp$reg3), na.rm = TRUE),4)
ymin_fig10 <- round(min(as.numeric(tfe_gdppc$reg1), na.rm = TRUE),4)
ymax_fig10 <- round(max(as.numeric(tfe_gdppc$reg1), na.rm = TRUE),4)
ymin_fig11 <- round(min(as.numeric(tfe_gdppc$reg2), na.rm = TRUE),4)
ymax_fig11 <- round(max(as.numeric(tfe_gdppc$reg2), na.rm = TRUE),4)
ymin_fig12 <- round(min(as.numeric(tfe_gdppc$reg3), na.rm = TRUE),4)
ymax_fig12 <- round(max(as.numeric(tfe_gdppc$reg3), na.rm = TRUE),4)

fig7 <- ggplot(data=tfe_tfp) +
  geom_point(aes(x=year, y=as.numeric(reg1)),size = 3) +
  labs(x = "Temperature (Degrees C)",
       y = "Time fixed effects",
       title =  "Growth Effects") +
  scale_y_continuous(limits=c(-0.035,0.025),
                     breaks = seq(-0.035,0.025,by=0.01),
                     expand = c(0,0.001)) +
  theme_ekg() +
  theme(
    axis.text.x = element_text(size = 24),
    axis.text.y = element_text(size = 24),
    axis.title.x = element_text(size = 28),
    axis.title.y = element_text(size = 28),
    plot.title = element_text(hjust = 0.5, size=28),
    legend.position = "none"
  )
ggsave(filename = here('results','figures', 'tfe_tfp_reg1.eps'), plot = fig7, units = "in", height = 8, width = 10, dpi = 300, device = "eps")


fig8 <- ggplot(data=tfe_tfp) +
  geom_point(aes(x=year, y=as.numeric(reg2)),size=3) +
  labs(x = "Temperature (Degrees C)",
       y = "Time fixed effects",
       title = "Level Effects") +
  scale_y_continuous(limits=c(-0.025,0.035),
                     breaks = seq(-0.025,0.035,by=0.01),
                     expand = c(0,0.001)) +
  theme_ekg() +
  theme(
    axis.text.x = element_text(size = 24),
    axis.text.y = element_text(size = 24),
    axis.title.x = element_text(size = 28),
    axis.title.y = element_text(size = 28),
    plot.title = element_text(hjust = 0.5, size=28),
    legend.position = "none"
  )
ggsave(filename = here('results','figures', 'tfe_tfp_reg2.eps'), plot = fig8, units = "in", height = 8, width = 10, dpi = 300, device = "eps")


fig9 <- ggplot(data=tfe_tfp) +
  geom_point(aes(x=year, y=as.numeric(reg3)),size=3) +
  labs(x = "Temperature (Degrees C)",
       y = "Time fixed effects",
       title = "Growth Effects and Level Effects") +
  scale_y_continuous(limits=c(-0.035,0.025),
                     breaks = seq(-0.035,0.025,by=0.01),
                     expand = c(0,0.001)) +
  theme_ekg() +
  theme(
    axis.text.x = element_text(size = 24),
    axis.text.y = element_text(size = 24),
    axis.title.x = element_text(size = 28),
    axis.title.y = element_text(size = 28),
    plot.title = element_text(hjust = 0.5, size=28),
    legend.position = "none"
  )
ggsave(filename = here('results','figures', 'tfe_tfp_reg3.eps'), plot = fig9, units = "in", height = 8, width = 10, dpi = 300, device = "eps")

fig10 <- ggplot(data=tfe_gdppc) +
  geom_point(aes(x=year, y=as.numeric(reg1)),size = 3) +
  labs(x = "Temperature (Degrees C)",
       y = "Time fixed effects",
       title = "Level Effects") +
  scale_y_continuous(limits=c(-0.035,0.025),
                     breaks = seq(-0.035,0.025,by=0.01),
                     expand = c(0,0.001)) +
  theme_ekg() +
  theme(
    axis.text.x = element_text(size = 24),
    axis.text.y = element_text(size = 24),
    axis.title.x = element_text(size = 28),
    axis.title.y = element_text(size = 28),
    plot.title = element_text(hjust = 0.5, size=28),
    legend.position = "none"
  )
ggsave(filename = here('results','figures', 'tfe_gdppc_reg1.eps'), plot = fig10, units = "in", height = 8, width = 10, dpi = 300, device = "eps")


fig11 <- ggplot(data=tfe_gdppc) +
  geom_point(aes(x=year, y=as.numeric(reg2)),size=3) +
  labs(x = "Temperature (Degrees C)",
       y = "Time fixed effects",
       title = "Growth Effects") +
  scale_y_continuous(limits=c(-0.035,0.025),
                     breaks = seq(-0.035,0.025,by=0.01),
                     expand = c(0,0.001)) +
  theme_ekg() +
  theme(
    axis.text.x = element_text(size = 24),
    axis.text.y = element_text(size = 24),
    axis.title.x = element_text(size = 28),
    axis.title.y = element_text(size = 28),
    plot.title = element_text(hjust = 0.5, size=28),
    legend.position = "none"
  )
ggsave(filename = here('results','figures', 'tfe_gdppc_reg2.eps'), plot = fig11, units = "in", height = 8, width = 10, dpi = 300, device = "eps")


fig12 <- ggplot(data=tfe_gdppc) +
  geom_point(aes(x=year, y=as.numeric(reg3)),size=3) +
  labs(x = "Temperature (Degrees C)",
       y = "Time fixed effects",
       title = "Growth and Level Effects") +
  scale_y_continuous(limits=c(-0.025,0.025),
                     breaks = seq(-0.025,0.025,by=0.01),
                     expand = c(0,0.001)) +
  theme_ekg() +
  theme(
    axis.text.x = element_text(size = 24),
    axis.text.y = element_text(size = 24),
    axis.title.x = element_text(size = 28),
    axis.title.y = element_text(size = 28),
    plot.title = element_text(hjust = 0.5, size=28),
    legend.position = "none"
  )
ggsave(filename = here('results','figures', 'tfe_gdppc_reg3.eps'), plot = fig12, units = "in", height = 8, width = 10, dpi = 300, device = "eps")

################################################################################
# Figure 5 - Decomposition of Climate Impacts by country 
################################################################################
#Compute the N most populated countries in 2010
npop <- df_tfp %>% filter(year==2010) %>% arrange(desc(Pop))
npop <- head(npop, n=25L)

namespop <- unique(npop$countryname)

#Recode names
namespop[namespop == 'Russian Federation'] <- 'Russia'
namespop[namespop == 'Iran, Islamic Rep.'] <- 'Iran'
namespop[namespop == 'Egypt, Arab Rep.'] <- 'Egypt'
namespop[namespop == 'Korea, Rep.'] <- 'South Korea'

#Recode names in results table
results_plot_bar <- results_model_reg2
results_plot_bar$country <- recode(results_plot_bar$country,
                                     'UK' = 'United Kingdom',
                                     'USA' = 'United States',
                                     'Democratic Republic of the Congo'  = 'Congo, Dem. Rep.',
                                     "Russian Federation" = "Russia",
                                     "Iran, Islamic Rep." = "Iran",
                                     "Egypt, Arab Rep." = "Egypt",
                                     'Korea, Rep.' = 'South Korea'
)

reshape_decomposition <- function(df, names) {
  dfKcon <- df %>%
    filter(country %in% names) %>%
    select(country,Kcontribution) %>%
    mutate(decompose = 'Capital',
           Kcontribution = as.numeric(Kcontribution)*100) %>%
    rename(contribution = Kcontribution)
  
  dfAcon <- df %>%
    filter(country %in% names) %>%
    select(country,Acontribution) %>%
    mutate(decompose = 'TFP',
           Acontribution = as.numeric(Acontribution)*100) %>%
    rename(contribution = Acontribution)
  
  dfreturn <- as.data.frame(rbind(dfKcon, dfAcon)) %>%
    group_by(country) %>%
    mutate(Ypos=sum(contribution),
           Ypos=replace(Ypos,which(Ypos<0),NA),
           Yneg=sum(contribution),
           Yneg=replace(Yneg,which(Yneg>=0),NA))

    return(dfreturn)
  
}

dfKcon <- results_plot_bar %>%
  filter(country %in% namespop) %>%
  select(country,Kcontribution) %>%
  mutate(decompose = 'Capital',
         Kcontribution = as.numeric(Kcontribution)*100) %>%
  rename(contribution = Kcontribution)

dfAcon <- results_plot_bar %>%
  filter(country %in% namespop) %>%
  select(country,Acontribution) %>%
  mutate(decompose = 'TFP',
         Acontribution = as.numeric(Acontribution)*100) %>%
  rename(contribution = Acontribution)

df_r2col <- reshape_decomposition(results_plot_bar, namespop)
df_r3col <- reshape_decomposition(results_plot_bar, 'World')
decomposition_chart <- function(df) {
  df$decompose <- reorder(df$decompose, df$contribution)
  df$decompose <- factor(df$decompose, levels=rev(levels(df$decompose)))
  
  plot <- ggplot(data = df) +
    geom_col(aes(x = fct_reorder(country,contribution), y = contribution, fill=decompose)) +
    geom_text(aes(x=country,
                  y=Yneg,
                  label=paste(sprintf("%0.1f",round(Yneg,1)),sep="")),
                              hjust = 1.1, size = 5,
                              position = position_dodge(width = 1)
                              ) +
    geom_text(aes(x=country,
                  y=Ypos,
                  label=paste(round(Ypos,1),sep="")),
              hjust = -0.1, size = 5,
              position = position_dodge(width = 1)
    ) +
    coord_flip() +
    labs(y='Log difference in GDP per capita',
         x='',
         title='') +
    scale_fill_manual(values=c("Capital" = "#6baed6",
                               "TFP" = "#08519c")) +
    scale_y_continuous(expand = expansion(mult = 0.15)) +
    theme_ekg() +
    theme(
      axis.text.x = element_text(size = 18),
      axis.text.y = element_text(size = 18),
      axis.title.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      legend.title = element_blank(),
      legend.text = element_text(size=18),
      legend.position = c(0.15,0.8),
      legend.background = element_blank()
    )
 
 return(plot)
}

decompositionFig2 <- decomposition_chart(df_r2col)

ggsave(filename = here('results','figures', 'Ydecomposition_reg2.eps'), plot = decompositionFig2, units = "in", height = 8, width = 10, dpi = 300, device = "eps")

################################################################################
# Figure 7 - Decomposition of Climate Impacts
################################################################################
reshape_world_decomposition <- function(df) {
  dfKcon <- df %>%
    select(Kcontribution, year_vec) %>%
    mutate(decompose = 'Capital',
           Kcontribution = as.numeric(Kcontribution)*100) %>%
    rename(contribution = Kcontribution)
  
  dfAcon <- df %>%
    select(Acontribution, year_vec) %>%
    mutate(decompose = 'TFP',
           Acontribution = as.numeric(Acontribution)*100) %>%
    rename(contribution = Acontribution)
  
  dfreturn <- as.data.frame(rbind(dfKcon, dfAcon)) %>%
    group_by(year_vec) %>%
    mutate(Ypos=sum(contribution),
           Ypos=replace(Ypos,which(Ypos<0),NA),
           Yneg=sum(contribution),
           Yneg=replace(Yneg,which(Yneg>=0),NA))
  
  return(dfreturn)
  
}

df_worlddmgs_plt <- reshape_world_decomposition(results_world_reg2)

plt_world_dmgs <- function(df) {
  df$decompose <- reorder(df$decompose, df$contribution)
  df$decompose <- factor(df$decompose, levels=rev(levels(df$decompose)))
  
  plot <- ggplot(data = df) +
    geom_col(aes(x = year_vec, y = contribution, fill=decompose),width = 0.75) +
    labs(y='Percent change in GDP per capita',
         x='Year',
         title='') +
    scale_fill_manual(values=c("Capital" = "#6baed6",
                               "TFP" = "#08519c")) +
    scale_y_continuous(limits=c(-4,1),
                       breaks = seq(-4,1,by=1),
    ) + 
    scale_x_discrete(breaks=seq(2010,2100,by=10))+
    theme_ekg() +
    theme(
      axis.text.x = element_text(size = 22),
      axis.text.y = element_text(size = 22),
      axis.title.x = element_text(size = 24),
      axis.title.y = element_text(size = 24),
      legend.title = element_blank(),
      legend.text = element_text(size=18),
      legend.position = c(0.15,0.7),
      legend.background = element_blank(),
      plot.margin=unit(c(0,10,0,0),"mm")
    )
  
  return(plot)
}


world_dmgs <- plt_world_dmgs(df_worlddmgs_plt)
ggsave(filename = here('results','figures', 'worldYdecomposition_reg2.eps'), plot = world_dmgs, units = "in", height = 8, width = 10, dpi = 300, device = "eps")

################################################################################
# Figure C4/5: Fraction of Statistically Significant Trends
################################################################################

#Load in estimates from stata
tfp_regs_qt <- read.csv(here('data','output','regtable1_quadratic_trends.csv'))
tfp_regs_lt <- read.csv(here('data','output','regtable1_linear_trends.csv'))
tfp_regs_90 <- read.csv(here('data','output','regtable1_1990_dummy.csv'))
tfp_regs_region <- read.csv(here('data','output','regtable1_region_fe.csv'))

#Delete unnecessary rows
tfp_regs_lt = tfp_regs_lt[-seq(1,nrow(tfp_regs_lt),2),]
tfp_regs_qt = tfp_regs_qt[-seq(1,nrow(tfp_regs_qt),2),]
tfp_regs_90 = tfp_regs_90[-seq(1,nrow(tfp_regs_90),2),]
tfp_regs_region = tfp_regs_region[-seq(1,nrow(tfp_regs_region),2),]

#Remove weird characters
tfp_regs_lt[] <- lapply(tfp_regs_lt, function(x) gsub("=", "", x, fixed = TRUE))
tfp_regs_lt[] <- lapply(tfp_regs_lt, function(x) gsub("\"", "", x, fixed = TRUE))
tfp_regs_qt[] <- lapply(tfp_regs_qt, function(x) gsub("=", "", x, fixed = TRUE))
tfp_regs_qt[] <- lapply(tfp_regs_qt, function(x) gsub("\"", "", x, fixed = TRUE))
tfp_regs_90[] <- lapply(tfp_regs_90, function(x) gsub("=", "", x, fixed = TRUE))
tfp_regs_90[] <- lapply(tfp_regs_90, function(x) gsub("\"", "", x, fixed = TRUE))
tfp_regs_region[] <- lapply(tfp_regs_region, function(x) gsub("=", "", x, fixed = TRUE))
tfp_regs_region[] <- lapply(tfp_regs_region, function(x) gsub("\"", "", x, fixed = TRUE))

#rename columns
tfp_regs_lt <- tfp_regs_lt %>%
  rename(
    'var' = 'X.',
    'reg1' = 'X..1.',
    'reg2' = 'X..2.',
    'reg3' = 'X..3.') %>%
  filter(str_detect(var,"X_y"))

#rename columns
tfp_regs_qt <- tfp_regs_qt %>%
  rename(
    'var' = 'X.',
    'reg1' = 'X..1.',
    'reg2' = 'X..2.',
    'reg3' = 'X..3.') %>%
  filter(str_detect(var,"X_y"))

#rename columns
tfp_regs_90 <- tfp_regs_90 %>%
  rename(
    'var' = 'X.',
    'reg1' = 'X..1.',
    'reg2' = 'X..2.',
    'reg3' = 'X..3.') %>%
  filter(str_detect(var,"#1.post90"))

#rename columns
tfp_regs_region <- tfp_regs_region %>%
  rename(
    'var' = 'X.',
    'reg1' = 'X..1.',
    'reg2' = 'X..2.',
    'reg3' = 'X..3.') %>%
  filter(str_detect(var,".region_factor"))

#Create list of data frames to loop over
df_list <-list(tfp_regs_lt,tfp_regs_qt,tfp_regs_90,tfp_regs_region)

#Create labels for newly created variables
label <- c('lt','qt','90','region')

#Compute fraction of time-trends that are significant
for (i in 1:length(df_list)) {
  for (reg in 1:3) {
    reg.sig90 <- sum(grepl("*",df_list[[i]][[reg+1]], fixed = TRUE))
    reg.sig95 <- sum(grepl("**",df_list[[i]][[reg+1]], fixed = TRUE))
    reg.frac90 <-reg.sig90/nrow(df_list[[i]])
    reg.frac95 <- reg.sig95/nrow(df_list[[i]])
    
    assign(paste0('reg',as.character(reg),'.frac90.',label[i]), reg.frac90)
    assign(paste0('reg',as.character(reg),'.frac95.',label[i]), reg.frac95)
    
  }
}


row1 <- c("Linear\n(1)","95% Confidence",reg1.frac95.lt)
row2 <- c("Linear\n(1)", "90% Confidence",reg1.frac90.lt)
row3 <- c("Quadratic\n(1)","95% Confidence",reg1.frac95.qt)
row4 <- c("Quadratic\n(1)", "90% Confidence",reg1.frac90.qt)
row5 <- c("Linear\n(2)", "95% Confidence",reg2.frac95.lt)
row6 <- c("Linear\n(2)", "90% Confidence",reg2.frac90.lt)
row7 <- c("Quadratic\n(2)", "95% Confidence",reg2.frac95.qt)
row8 <- c("Quadratic\n(2)", "90% Confidence",reg2.frac90.qt)
row9 <- c("Linear\n(3)", "95% Confidence",reg3.frac95.lt)
row10 <- c("Linear\n(3)", "90% Confidence",reg3.frac90.lt)
row11 <- c("Quadratic\n(3)", "95% Confidence",reg3.frac95.qt)
row12 <- c("Quadratic\n(3)", "90% Confidence",reg3.frac90.qt)
row13 <- c("1990 Dum.\n(1)", "95% Confidence", reg1.frac95.90)
row14 <- c("1990 Dum.\n(1)", "90% Confidence", reg1.frac90.90)
row15 <- c("1990 Dum.\n(2)", "95% Confidence", reg2.frac95.90)
row16 <- c("1990 Dum.\n(2)", "90% Confidence", reg2.frac90.90)
row17 <- c("1990 Dum.\n(3)", "95% Confidence", reg3.frac95.90)
row18 <- c("1990 Dum.\n(3)", "90% Confidence", reg3.frac90.90)
row19 <- c("Reg.-Yr. \n(1)", "95% Confidence", reg1.frac95.region)
row20 <- c("Reg.-Yr. \n(1)", "90% Confidence", reg1.frac90.region)
row21 <- c("Reg.-Yr. \n(2)", "95% Confidence", reg2.frac95.region)
row22 <- c("Reg.-Yr. \n(2)", "90% Confidence", reg2.frac90.region)
row23 <- c("Reg.-Yr. \n(3)", "95% Confidence", reg3.frac95.region)
row24 <- c("Reg.-Yr. \n(3)", "90% Confidence", reg3.frac90.region)

df_sigcount_ttrends <- as.data.frame(rbind(row1,row2,row3,row4,row5,row6,row7,row8,row9
                                   ,row10,row11,row12)) %>%
  mutate(V3 = as.numeric(V3)
  ) %>%
  rename('label' = V1,
         'cl' = V2,
         'val' = V3
  ) 

df_sigcount_fetrends <- as.data.frame(rbind(row13,row14,row15,row16,row17,row18,row19,row20
                                      , row21, row22, row23, row24)) %>%
  mutate(V3 = as.numeric(V3)
  ) %>%
  rename('label' = V1,
         'cl' = V2,
         'val' = V3
  ) 

sigHist_tt <- ggplot(data=df_sigcount_ttrends, aes(x=label, fill=cl)) +
  geom_bar(stat = "identity", aes(y=val),position=position_dodge()) +
  labs(y='Fraction significant',
       x = '',
       title = '') +
  scale_fill_manual(values=c('#6baed6','#08519c'))+
  scale_y_continuous(limits=c(0,0.4),
                     breaks = seq(0,0.4,by=0.1), expand = c(0, 0)
  ) + 
  theme(
    axis.text.x = element_text(size = 35),
    axis.text.y = element_text(size = 35),
    axis.title.x = element_text(size = 35),
    axis.title.y = element_text(size = 35),
    legend.title = element_blank(),
    legend.text = element_text(size=35),
    legend.position = c(0.2,0.8),
    legend.background = element_blank()
  ) +
  theme_ekg()

sigHist_fe <- ggplot(data=df_sigcount_fetrends, aes(x=label, fill=cl)) +
  geom_bar(stat = "identity", aes(y=val),position=position_dodge()) +
  labs(y='Fraction significant',
       x = '',
       title = '') +
  scale_fill_manual(values=c('#6baed6','#08519c'))+
  scale_y_continuous(limits=c(0,0.4),
                     breaks = seq(0,0.4,by=0.1), expand = c(0, 0)
  ) + 
  theme(
    axis.text.x = element_text(size = 35),
    axis.text.y = element_text(size = 35),
    axis.title.x = element_text(size = 35),
    axis.title.y = element_text(size = 35),
    legend.title = element_blank(),
    legend.text = element_text(size=35),
    legend.position = c(0.2,0.8),
    legend.background = element_blank()
  ) +
  theme_ekg()

ggsave(filename = here('results','figures', 'TrendSig1.eps'), plot = sigHist_tt, units = "in", height = 8, width = 18, dpi = 300, device = "eps")
ggsave(filename = here('results','figures', 'TrendSig2.eps'), plot = sigHist_fe, units = "in", height = 8, width = 18, dpi = 300, device = "eps")

################################################################################
# Figure C6: The Response of TFP Growth to a Temperature Shock
################################################################################

plot_irf <- function(filename) {

  irf_df <- read.csv(here('data','output',filename))
  irf_df <- irf_df %>%
    mutate(upper90 = beta + 1.645*se,
           lower90 = beta - 1.645*se,
           upper95 = beta + 1.960*se,
           lower95 = beta - 1.960*se)
  
  irf <- ggplot(data=irf_df,aes(x=as.numeric(horizon))) +
    geom_ribbon(aes(ymin=lower95,ymax=upper95), fill="grey") +
    geom_line(aes(y=beta), color = "red", size = 1.2) +
    geom_segment(aes(x=0,xend=20,y=0,yend=0), linetype = "dashed", size = 0.5) +
    labs(x = "Years since the temperature shock",
         y = "Percent",
         title = "") +
    scale_x_continuous(expand = c(0, 0), breaks = seq(0,20, by = 5), limits = c(0, 21)) +
    scale_y_continuous(expand = c(0, 0),breaks = seq(-0.008, by = 0.004), limits = c(-0.0085,0.0085)) + 
    theme_ekg() +
    theme(
      axis.text.x = element_text(size = 24),
      axis.text.y = element_text(size = 24),
      axis.title.x = element_text(size = 28),
      axis.title.y = element_text(size = 28),
      plot.title = element_text(hjust = 0.5, size=28)
      )

  return(irf)
}

irf8_tfp <- plot_irf('tfp8_irf_lp.csv') + ggtitle('Panel a: country with annual\n average temperature 8°C')
ggsave(filename = here('results','figures', 'tfp8_irf.eps'), plot = irf8_tfp, units = "in",  height = 8, width = 10, dpi = 300, device = "eps")
ggsave(filename = here('results','figures', 'tfp8_irf.png'), plot = irf8_tfp, units = "in", height = 8, width = 10, dpi = 300, device = "png")

irf13_tfp <- plot_irf('tfp13_irf_lp.csv') + ggtitle('Panel b: country with annual\n average temperature 13°C')
ggsave(filename = here('results','figures', 'tfp13_irf.eps'), plot = irf13_tfp, units = "in",  height = 8, width = 10, dpi = 300, device = "eps")
ggsave(filename = here('results','figures', 'tfp13_irf.png'), plot = irf13_tfp, units = "in", height = 8, width = 10, dpi = 300, device = "png")

irf18_tfp <- plot_irf('tfp18_irf_lp.csv') + ggtitle('Panel c: country with annual\n average temperature 18°C')
ggsave(filename = here('results','figures','tfp18_irf.eps'), plot = irf18_tfp, units = "in",  height = 8, width = 10, dpi = 300, device = "eps")
ggsave(filename = here('results','figures', 'tfp18_irf.png'), plot = irf18_tfp, units = "in", height = 8, width = 10, dpi = 300, device = "png")





