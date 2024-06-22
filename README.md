# TOPAZ EnKF with BGC assimilation

### main update

Following updates are for making TOPAZ EnKF flexible for hycom configuration options:

- Hycom configuration files, ```regional.grid.(a,b)``` and ```regional.depth.(a,b)```, are read through a new module:
```
get_mod_cnfg
```
- ```depth``` is used for making land mask in ```Prep_Routines``` without using ```meanssh.nc```

### Notes:

- configuration specific ```meanssh.nc``` is still required for SLA assimilation.

### TODO

- read ```meanssh.nc``` only for SLA assimilation
