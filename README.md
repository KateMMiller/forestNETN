# forestNETN
Package for importing, joining and querying NETN forest database tables

For this package to work, the version of R (32 vs 64 bit) must match the version of Access, which is 32-bit for the NETN forest database. You can either have the master NETN forest database named "NETNFVM" as a SystemDSN, or you can specify a database path. 
The importData() function is the first step to do any of the other queries in the package. 
The importData() function imports database tables from the NETN forest database, and assigns them to the global environment. 
The joinLocEvent(), joinTreeData(), joinRegenData(), joinQuadData(), and joinCWDData() source the tables from the global environment, and create common views of the data. These functions also allow you to filter by common factors, such as park, years, exotic or native species, etc.
