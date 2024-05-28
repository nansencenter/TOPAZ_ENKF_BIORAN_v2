## Useful links

[ICES unit conversions](https://www.ices.dk/data/tools/Pages/Unit-conversions.aspx)

[Units for Carbon Dioxide Concentrations and Emissions](https://acsess.onlinelibrary.wiley.com/doi/pdfdirect/10.2134/asaspecpub53.appendix2)

## Constants from [TEOS 10](https://repository.oceanbestpractices.org/bitstream/handle/11329/286/TEOS-10_Manual.pdf?sequence=1&isAllowed=y)



## Unit conversion factors

|name  |factor  |description   |unit            |name in hycproj|
|------|--------|--------------|----------------|---------------|
|C_N   |6.625   |Redfield ratio|[mmol_C/mmol_N] |`C2NIT`        |
|C_Si  |6.625   |Redfield ratio|[mmol_C/mmol_Si]|`C2SIL`        |
|C_P   |106.0   |Redfield ratio|[mmol_C/mmol_P] |`C2PHO`        |
|C_Cmg |12.01   |Transfer unit |[mg_C/mmol_C]   |`ccar`         |

Note: The constant elemental stoichiometry of phytoplankton (C:N:Si:P = 106:16:16:1) is known as the Redfield ratio. 

## Unit list of FABM-ECOSMO state variables for assimilation

|parameter|unit HYCOM |unit CMEMS |conversion factor   | 
|---------|-----------|-----------|--------------------| 
|ECO_diac |mg Chl m-3 |mg Chl m-3 |`1.0`               |  
|ECO_flac |mg Chl m-3 |mg Chl m-3 |`1.0`               |
|ECO_cclc |mg Chl m-3 |mg Chl m-3 |`1.0`               |
|ECO_dia  |mg C m-3   |mmol C m-3 |`/ccar`             |
|ECO_fla  |mg C m-3   |mmol C m-3 |`/ccar`             |
|ECO_ccl  |mg C m-3   |mmol C m-3 |`/ccar`             |
|ECO_micr |mg C m-3   |mmol C m-3 |`/ccar`             |
|ECO_meso |mg C m-3   |mmol C m-3 |`/ccar`             |
|ECO_no3  |mg C m-3   |mole m-3   |`/ccar/C2NIT`       |
|ECO_sil  |mg C m-3   |mole m-3   |`/ccar/C2SIL`       |
|ECO_pho  |mg C m-3   |mole m-3   |`/ccar/C2PHO`       |
|ECO_oxy  |mmol m-3   |mmol m-3   |`1.0`               |
|CO2_c    |mmol m-3   |mole m-3   |`/1000.0`           |
|CO2_pH   |mmol m-3   |mole m-3   |`/1000.0`           |
|CO2_pCO2 |umol m-3   |Pa         |`/10.1325`          |
