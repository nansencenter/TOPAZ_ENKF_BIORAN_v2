# TOPAZ EnKF with BGC assimilation

### main update

Following updates are for making TOPAZ EnKF flexible for hycom configuration options:

- hycom configuration files, ```regional.grid.(a,b)``` and ```regional.depth.(a,b)```, are read through new modules:
```
m_get_mod_cnfg.F90
m_io_hycom.F90
```

#### notes:

- This change removes requirement of ```depth*.uf``` file to read ```depth``` information both in ```EnKF``` and ```prepobs```.
- ```depth``` read from ```regional.depth.(a,b)``` is used for making land mask in ```Prep_Routines/m_get_def_wet_point.F90``` without using ```meanssh.uf```
- configuration-specific ```meanssh.uf``` and ```re_sla.nc``` are still required for SLA data prepobs.
- BGC observations: ```SCHL, CHL, POC, NIT, SIL, PHO, OXY``` are NOT ready to be assimilated.

### TODO:

- read ```meanssh.nc``` only for SLA assimilation.
- register ```SCHL, CHL, POC, NIT, SIL, PHO, OXY```.
- add ```m_global_analysis.F90``` for ecoregion-dependent parameter estimation.
- add analysis masks based on mixder layer depth, sea ice concentration and surface nitrate concentration.
