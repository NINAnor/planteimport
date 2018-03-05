CREATE EXTENSION postgres_fdw;

CREATE SERVER ninsrv16
	FOREIGN DATA WRAPPER postgres_fdw
	OPTIONS(host 'ninsrv16.nina.no', port '5432', dbname 'gisdata');
	
	
CREATE USER MAPPING FOR "jens.astrom"
	SERVER ninsrv16
	OPTIONS(user 'postgjest', password 'gjestpost');

CREATE SCHEMA darwin;
	
IMPORT FOREIGN SCHEMA lookup_tables
	FROM SERVER ninsrv16 INTO darwin;
	

SELECT * 
FROM darwin.species_names_artsdatabanken_plantae
LIMIT 100
