# The Future Airport Project

## Lay abstract
This dashboard shows the amount of climate change expected at airports in the 21st century according to the latest generation of climate models. It allows you to pick any of the world's ~900 largest airports, choose one of four climate scenarios from most to least optimistic, and see how much change is predicted to happen in the air temperature, pressure, and humidity at that location up to the year 2100.

## Methodology
A worldwide population of 907 commercial airports was identified as having processed one million or more passengers in 2019 (pre-COVID). An R Shiny dashboard was built to visualize the amount of longitudinal change predicted at those airports in the climatic variables of surface air temperature (*tas*), pressure (*ps*), and humidity (*hurs*) between the years 2015 and 2100 under four shared socioeconomic pathways (SSP1–2.6, SSP2–4.5, SSP3–7.0, and SSP5–8.5). The dashboard uses simulation data from the Max Planck Institute Earth System Model version 1.2 High Resolution (MPI-ESM1-2-HR) climate model (Müller et al., 2018). This model is part of the Coupled Model Intercomparison Project Phase 6 (CMIP6) overseen by the World Climate Research Programme (WCRP)'s Working Group on Coupled Modelling (WGCM). Binary NetCDF files corresponding to the three climate variables and four SSPs were downloaded from the Earth System Grid Federation (ESGF) (Cinquini et al., 2014) and parsed in R. The six-hourly time series for *tas*, *ps*, and *hurs* were extracted for each Earth grid cell found to contain the coordinates of one or more of the population airports. The data from the starting year of the model (2015) to the present day were not subjected to reanalysis and are presented as-is. The minimum, 25th percentile (lower quartile), mean, 50th percentile (median), 75th percentile (upper quartile), and maximum statistics were aggregated annually for every airport and climate scenario (SSP). The results show a clear warming signal in the *tas* time series, a positive correlation with the amount of radiative forcing in W/m² predicted by each SSP, and an uneven geographical distribution of warming over the 21st century, with the highest increase located in and around the Siberian plateau.

## Further reading
This dashboard is an extension of my doctoral research available at https://commons.erau.edu/edt/720/ and https://github.com/TheAviationDoctor/PhD. You may contact me in relation to this work at thomas -at- pellegr -dot- in.

## License
This code is offered under the GPL-3.0 license.

## References
* Cinquini, L., Crichton, D., Mattmann, C., Harney, J., Shipman, G., Wang, F.,
Ananthakrishnan, R., Miller, N., Denvil, S., Morgan, M., Pobre, Z., Bell, G. M.,
Doutriaux, C., Drach, R., Williams, D., Kershaw, P., Pascoe, S., Gonzalez, E.,
Fiore, S., & Schweitzer, R. (2014). The Earth system grid federation: An open
infrastructure for access to distributed geospatial data. Future Generation
Computer Systems, 36, 400–417. https://doi.org/f5478t
* Müller, W. A., Jungclaus, J. H., Mauritsen, T., Baehr, J., Bittner, M., Budich, R., Bunzel,
F., Esch, M., Ghosh, R., Haak, H., Ilyina, T., Kleine, T., Kornblueh, L., Li, H.,
Modali, K., Notz, D., Pohlmann, H., Roeckner, E., Stemmler, I., … Marotzke, J.
(2018). A Higher‐Resolution Version of the Max Planck Institute Earth System
Model (MPI‐ESM1.2‐HR). Journal of Advances in Modeling Earth Systems,
10(7), 1383–1413. https://doi.org/gdqdnr