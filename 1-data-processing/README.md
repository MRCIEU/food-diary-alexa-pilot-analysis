

# Data processing



## Data downloads

The web survey data is downloaded from REDCap (choose options 'CSV / Microsoft Excel (raw data)').

The alexa data is downloaded from the Amazon AWS Maria DB. See `https://github.com/MRCIEU/food-diary-alexa-pilot-aws/' for database export commands.




## Map web food form entries to alexa entries

```bash
Rscript matchDiaries.R
```


