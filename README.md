# TOPAZ EnKF with BGC assimilation

### main update

- Hycom configuration files, ```regional.grid.(a,b)``` and ```regional.depth.(a,b)```, are read through a new module:
```
get_mod_cnfg
```

- ```depth``` is used for making land mask in ```Prep_Routines``` without using ```meanssh.nc```

### TODO

- read ```meanssh.nc``` for SLA assimilation
