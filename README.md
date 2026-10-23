# Sublinear_growth
Codes and filter output for: Sublinear growth and its relation to temporal stability in populations of Poecilia reticulata 

Verena Haring1* (verena.haring@uni-graz.at)
Bernat Corominas Murtra2(bernat.corominas-murtra@uni-graz.at)
Lina Mühlbauer1(lina.muehlbauer@mailbox.org)
Ronald D. Bassar3 (rdb0057@auburn.edu)
Tim Coulson4 (tim.coulson@biology.ox.ac.uk)
David N. Reznick5 (david.reznick@ucr.edu)
Joseph Travis6 (travis@bio.fsu.edu)
Andrew D. Letten7 (a.letten@uq.edu.au)
Onofrio Mazzarisi8 (onofrio.mazzarisi@pm.me)
Adam Thomas Clark1(adam.clark@uni-graz.at)

Affiliations:
1 Department of Biology, University of Graz, Holteigasse 6, 8010 Graz, Austria
2 Department of Biology, University of Graz, Universitätsplatz 2, 8010 Graz, Austria
3 Department of Biological Sciences, Auburn University, AL, USA
4 Department of Biology, University of Oxford, Oxford, UK
5  Evolution, Ecology, Organismal Biology Department, University of California Riverside,   Riverside, CA, USA
6 Department of Biological Science, Florida State University
7 School of the Environment, The University of Queensland, Brisbane, Australia
8 The National Institute of Oceanography and Applied Geophysics (OGS), Trieste, Italy & The Abdus Salam International Centre for Theoretical Physics (ICTP), Trieste, Italy

*Correspondence:
Verena Haring
Department of Biology, University of Graz, Holteigasse 6, 8010 Graz, Austria
Email: verena.haring@uni-graz.at


################################################################################

###### 1. Datafile ######
Timeseries P. reticulata: data.csv

###### 2. Methods ######

#### 2.1 Particle Filter ####
Updated particle filter functions: Functions.R

#### 2.1.1 Get true trajectory ####
Stochastic state space models fitted using a particle filter to separate the
sources of variability for each stream (CA,LL,TY,UL). The following codes computes
the true system state of each time series as well as mean values for observation error,
process noise and deterministic variation of each stream.

Run_Particle_Filter_CA.R\n
Run_Particle_Filter_LL.R\n
Run_Particle_Filter_TY.R\n
Run_Particle_Filter_UL.R\n

The MCMC chains are saved as:\n
\n
out_EDM_CA_TOTAL_varProc.RData\n
out_EDM_2_CA_TOTAL_varProc.RData\n
\n
out_EDM_LL_TOTAL_varProc.RData\n
\n
out_EDM_TY_TOTAL_varProc.RData\n
out_EDM_2_TY_TOTAL_varProc.RData\n
out_EDM_3_TY_TOTAL_varProc.RData\n
out_EDM_4_TY_TOTAL_varProc.RData\n
\n
out_EDM_UL_TOTAL_varProc.RData\n
out_EDM_2_UL_TOTAL_varProc.RData\n
out_EDM_3_UL_TOTAL_varProc.RData\n
\n
#Note: For CA,TY and UL, extended runs were needed to reach convergence. 

The filter outputs/true system states are saved as:

filterout_CA.rda
filterout_LL.rda
filterout_TY.rda
filterout_UL.rda

#### 2.1.2 Extract Parameter Distributions ####

The following codes extract the whole parameter distribution of each stream:

Particle_Filter_getsamples_CA.R
Data used: out_EDM_2_CA_TOTAL_varProc.RData

Particle_Filter_getsamples_LL.R
Data used: out_EDM_LL_TOTAL_varProc.RData

Particle_Filter_getsamples_TY.R
Data used: out_EDM_4_TY_TOTAL_varProc.RData

Particle_Filter_getsamples_UL.R
Data used: out_EDM_3_UL_TOTAL_varProc.RData

The parameter distrubion are saved as:

filterout_CA_whole.rda
filterout_LL_whole.rda
filterout_TY_whole.rda
filterout_UL_whole.rda

#### 2.2 Theta Fitting ####

The true trajectory of each stream is used to get temporal estimates for theta and
the growth rate related parameter alpha.

The following codes are used to receive temporal estimates for theta and alpha:
Theta_fitting_CA.R
Data used: filterout_CA.rda

Theta_fitting_LL.R
Data used: filterout_LL.rda

Theta_fitting_TY.R
Data used: filterout_TY.rda

Theta_fitting_UL.R
Data used: filterout_UL.rda

Theta and alpha estimates are saved as:

Theta_est_CA.rda
Theta_est_LL.rda
Theta_est_TY.rda
Theta_est_UL.rda

###### 3. Figures Main Text ######

Code generating Figures 1A,C-D:
Fig1ACD.R

Codes/Files generating Figure 2E:
Fig2E.R
Data used: data.csv

Codes/Files generating Figure 3:
Create_dataframe_Fig_3.R
Fig3.R
Data used:
filterout_CA_whole.rda
filterout_LL_whole.rda
filterout_TY_whole.rda
filterout_UL_whole.rda
Parameter distributions saved as:
df_Variability_Sources_Fig_3.csv


Codes/Files generating Figure 4:
Fig4.R
Data used:
Theta_est_CA.rda
Theta_est_LL.rda
Theta_est_TY.rda
Theta_est_UL.rda

###### 4. Supplement ######

#### 4.1 Figures ####
Codes/Files generating Fig S1:
FigS1.R
Data loaded from Theta_fitting_X.R: 
CA_r2_plots.rda
LL_r2_plots.rda
TY_r2_plots.rda
UL_r2_plots.rda

Codes/Files generating Fig S2:
FigS2.R
Data used:
filterout_CA_whole.rda
filterout_LL_whole.rda
filterout_TY_whole.rda
filterout_UL_whole.rda

Codes/Files generating Fig S3:
FigS3.R
Data used: 
Theta_est_CA.rda
Theta_est_LL.rda
Theta_est_TY.rda
Theta_est_UL.rda
filterout_CA.rda
filterout_LL.rda
filterout_TY.rda
filterout_UL.rda

Codes/Files generating Fig S4:
FigS4.R
Data used from Theta_fitting_X.R: 
dNNdt_N_CA.rda
dNNdt_N_LL.rda
dNNdt_N_TY.rda
dNNdt_N_UL.rda

#### 4.2 Tables ####

Codes/Files to generate Table S1-2:
TabS1.R
TabS2.R

Data used:
filterout_CA_whole.rda
filterout_LL_whole.rda
filterout_TY_whole.rda
filterout_UL_whole.rda

Codes/Files to generate Table S3:
TabS3.R
Data used: df_Variability_Sources_Fig_3.csv

Codes/Files to generate Table S4-5:
TabS4.R
TabS5.R
Data used: 
out_EDM_2_CA_TOTAL_varProc.RData
out_EDM_LL_TOTAL_varProc.RData
out_EDM_4_TY_TOTAL_varProc.RData
out_EDM_3_UL_TOTAL_varProc.RData

Codes/Files to generate Table S6:
Create_df_var_alpha_theta.R
Data used:
Theta_est_CA.rda
Theta_est_LL.rda
Theta_est_TY.rda
Theta_est_UL.rda
TabS6.R
Data used:
Var_alpha_theta.csv

Codes/Files to generate Table S7:
TabS7.R
Data used:
Theta_est_CA.rda
Theta_est_LL.rda
Theta_est_TY.rda
Theta_est_UL.rda
