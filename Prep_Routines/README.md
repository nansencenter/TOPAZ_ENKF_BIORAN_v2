## Useful links

- [ICES unit conversions](https://www.ices.dk/data/tools/Pages/Unit-conversions.aspx)

- [Units for Carbon Dioxide Concentrations and Emissions](https://acsess.onlinelibrary.wiley.com/doi/pdfdirect/10.2134/asaspecpub53.appendix2)

- [NEMO-ERSEM and POLCOMS-ERSEM: User Guides](https://confluence.ecmwf.int/pages/viewpage.action?pageId=283550184)

- [TEOS 10](https://repository.oceanbestpractices.org/bitstream/handle/11329/286/TEOS-10_Manual.pdf?sequence=1&isAllowed=y)

## Unit conversion factors

|name  |factor  |description   |unit            |name in hycproj|
|------|--------|--------------|----------------|---------------|
|C_N   |6.625   |Redfield ratio|[mmol_C/mmol_N] |`C2NIT`        |
|C_Si  |6.625   |Redfield ratio|[mmol_C/mmol_Si]|`C2SIL`        |
|C_P   |106.0   |Redfield ratio|[mmol_C/mmol_P] |`C2PHO`        |
|C_Cmg |12.01   |Transfer unit |[mg_C/mmol_C]   |`ccar`         |

Note: The constant elemental stoichiometry of phytoplankton (C:N:Si:P = 106:16:16:1) is known as the Redfield ratio. From the Redfield ratio, the conversion factors, C_N=106/16=6.625, C_Si=106/16=6.625, C_P=106/1=106.0, are derived.

## Unit list of FABM-ECOSMO state variables for assimilation

### Conversion factors from observation to HYCOM daily/restart

Listed are the conversion factors used in:

```bash
Prep_Routines/
   
```

for matching observed variable unit to HYCOM restart data unit. 

### Conversion factors from HYCOM daily/restart to CMEMS

Listed are the conversion factors used in:

```bash
MSCPROGS/
  src/
    Hyc2proj/
       p_hyc2proj.F90
       m_bio_conversions.F90
```

for CMEMS products dissemination.

|name     |parameter                   |unit HYCOM         |unit CMEMS         |conversion factor   | 
|---------|----------------------------|-------------------|-------------------|--------------------| 
|ECO_diac |Diatom chlorophyll-a (Chl-a)|mg Chl m-3         |mg Chl m-3         |`1.0`               |  
|ECO_flac |Flagellate Chl-a            |mg Chl m-3         |mg Chl m-3         |`1.0`               |
|ECO_cclc |Coccolithophores Chl-a      |mg Chl m-3         |mg Chl m-3         |`1.0`               |
|ECO_dia  |Diatom biomass              |mg C m-3           |mmol C m-3         |`/ccar`             |
|ECO_fla  |Flagellate biomass          |mg C m-3           |mmol C m-3         |`/ccar`             |
|ECO_ccl  |Coccolithophores biomass    |mg C m-3           |mmol C m-3         |`/ccar`             |
|ECO_micr |Micro Zooplankton biomass   |mg C m-3           |mmol C m-3         |`/ccar`             |
|ECO_meso |Meso Zooplankton biomass    |mg C m-3           |mmol C m-3         |`/ccar`             |
|ECO_no3  |Nitrate                     |mg C m-3           |mmol m-3           |`/ccar/C2NIT`       |
|ECO_sil  |Silicate                    |mg C m-3           |mmol m-3           |`/ccar/C2SIL`       |
|ECO_pho  |Phosphate                   |mg C m-3           |mmol m-3           |`/ccar/C2PHO`       |
|ECO_oxy  |Dissolved oxygen            |mmol m-3 (umol L-1)|mmol m-3 (umol L-1)|`1.0`               |
|CO2_c    |Dissolved Inorganic Carbon  |mmol m-3           |mole m-3           |`/1000.0`           |
|CO2_pH   |pH of sea water             |non                |non                |`1.0`               |
|CO2_pCO2 |partial pressure CO2        |umol mol-1 (ppm)   |Pa                 |`/10.1325`          |
