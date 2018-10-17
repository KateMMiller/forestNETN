# forestNETN
Package for importing, joining and querying NETN forest database tables

For this package to work, you must have the NETN forest database named "NETNFVM" as a SystemDSN using the 32X bit driver. You must use the 32 bit version of R for the ODCB driver to work in the importData() function. 
The importData() function is the first step to do any of the other queries in the package. 
The importData() function imports database tables from the NETN forest database, and assigns them to the global environment. 
The joinLocEvent(), joinTreeData(), joinRegenData(), joinQuadData(), and joinCWDData() source the tables from the global environment, and create common views of the data. These functions also allow you to filter by common factors, such as park, years, exotic or native species, etc.
