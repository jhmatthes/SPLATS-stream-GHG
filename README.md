# SPLATS-stream-GHG
Repository for the stream GHG concentrations at Harvard Forest for the SPLATS (Soils-PLants-ATmosphere-Streams) methane project.

This set of code converts raw concentration measurements made with the LI-7810 in a closed loop mode into concentrations of aqueous CO2 & CH4 measured from streams using the headspace equilibration method. 

Order of operations are: 
1. Aggregate stream temperature and meteorological data necessary for converting equilibrated headspace concentrations to aqueous in-stream concentrations. 
2. Use the raw LI-7810 output data to calculate the concentration measured in the headspace sample. The code assumes that that a unique "Remark" is created within the LI-7810 software that corresponds to the stream headspace sample ID and that the "Remark" tag accurately corresponds to the time period when the sample was circulating through the LI-7810 closed loop.  
3. Use properties of aqueous chemistry (water temp, pressure, etc) to convert the headspace concentrations to aqueous concentrations for in-stream conditions.
