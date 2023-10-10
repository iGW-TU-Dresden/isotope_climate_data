# isotope_climate_data
GLEAM (ET), MSWEP (P) gridded datasets and isotopic point data compiled from SISAL are collected and used for plotting.

For data collecting...
1. Global Land Evaporation Amsterdam Model (GLEAM): go to --->　https://www.gleam.eu/ and check more about it. I used FileZilla to download the files.
Host: sftp://hydras.ugent.be; Port: 2225; Username: gleamuser; Password: v36_GLEAM2022#aw 
2. Multi-Source Weighted-Ensemble Precipitation (MSWEP):  go to --->  https://drive.google.com/drive/folders/1Kok05OPVESTpyyan7NafR-2WwuSJ4TO9 and download the nc. files.
3. Speleothem Isotopes Synthesis and AnaLysis Working Group (SISAL): it is given by the group member.
4. Global Network of Isotopes in Precipitation (GNIP, IAEA): go to ----> https://nucleus.iaea.org/wiser/index.aspx to download the sites nearby your cave.
5. Reconstructed precipitaiton isotopes: Please check the supplementary data from Allen, S. T., Jasechko, S., Berghuijs, W. R., Welker, J. M., Goldsmith, G. R., & Kirchner, J. W. (2019). Global sinusoidal seasonality in precipitation isotopes. Hydrology and Earth System Sciences, 23(8), 3423-3436.

find_allvar.R is the script for combining all the above data together in one csv. with 6 columes (site id, site name, year, month, P, AET, PET, d18O, d2H)

For plotting...

There are two files can be the example of how other format we can use for understanding these data.  Cathedral Cave is a cave located in SE-Australia.
ttest_Cathedral.xlsx: combines the isotopic data from IAEA, SISAL and sine-curve into the climate data from GLEAM, MSWEP and IAEA. 
Cathedral_3plots.R: includes the basic three plots for visualizing the data and calculating the correlation and cross-correlation of the variables among all sites.

