CREATE EXTENSION postgres_fdw;

CREATE SERVER "gisdata-db"
	FOREIGN DATA WRAPPER postgres_fdw
	OPTIONS(host 'gisdata-db.nina.no', port '5432', dbname 'gisdata');
	
	
CREATE USER MAPPING FOR "jens.astrom"
	SERVER "gisdata-db"
	OPTIONS(user 'postgjest', password 'gjestpost');

--CREATE SCHEMA darwin;
	
--IMPORT FOREIGN SCHEMA lookup_tables
--	FROM SERVER "gisdata-db" INTO darwin;


--NEW VERSION 2020

IMPORT FOREIGN SCHEMA species_names
	FROM SERVER "gisdata-db" INTO artsnavnebase;

IMPORT FOREIGN SCHEMA invasive_alien_species
	FROM SERVER "gisdata-db" INTO fremmedartslista;


/*
SELECT * 
FROM darwin.species_names_artsdatabanken_plantae
LIMIT 100

SELECT * 
FROM darwin.species_names_artsdatabanken_animalia
LIMIT 100
*/

--Triggers: UPDATED 2020

DROP TRIGGER update_insect_artsnavn
on insects.species;

DROP FUNCTION functions.update_insect_artnavn();
   
CREATE OR REPLACE FUNCTION functions.update_insect_artnavn()
  RETURNS trigger AS
$BODY$
BEGIN
CASE when NEW.sub_species IS NOT NULL THEN
NEW.latinsknavnid := pk_latinsknavnid
FROM artsnavnebase.artsnavnebase_animalia d
	WHERE NEW.genus = d.slekt
	AND NEW.species = d.art
	AND NEW.sub_species = d.underart
	AND d.hovedstatus = 'Gyldig';
	ELSE 
	NEW.latinsknavnid := pk_latinsknavnid
	FROM artsnavnebase.artsnavnebase_animalia d
	WHERE NEW.genus = d.slekt
	AND NEW.species = d.art
	AND d.underart IS NULL
	AND d.hovedstatus = 'Gyldig'; END CASE;
     RETURN NEW;
END
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;  
  
ALTER FUNCTION functions.update_insect_artnavn()
    OWNER TO ag_pgsql_radardata_admin;	
	
CREATE TRIGGER update_insect_artsnavn
  BEFORE INSERT OR UPDATE
  ON insects.species
  FOR EACH ROW
  EXECUTE PROCEDURE functions.update_insect_artnavn();

------------

DROP TRIGGER update_plants_artsnavn
on plants.species;

DROP FUNCTION functions.update_plants_artnavn();
   
CREATE OR REPLACE FUNCTION functions.update_plants_artnavn()
  RETURNS trigger AS
$BODY$
BEGIN
CASE when NEW.sub_species IS NOT NULL THEN
NEW.latinsknavnid := pk_latinsknavnid
FROM artsnavnebase.artsnavnebase_plantae d
	WHERE NEW.genus = d.slekt
	AND NEW.species = d.art
	AND NEW.sub_species = d.underart
	AND d.hovedstatus = 'Gyldig';
	ELSE 
	NEW.latinsknavnid := pk_latinsknavnid
	FROM artsnavnebase.artsnavnebase_plantae d
	WHERE NEW.genus = d.slekt
	AND NEW.species = d.art
	AND d.underart IS NULL
	AND d.hovedstatus = 'Gyldig'; END CASE;
     RETURN NEW;
END
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;  
  
ALTER FUNCTION functions.update_plants_artnavn()
    OWNER TO ag_pgsql_radardata_admin;	
	
CREATE TRIGGER update_plants_artsnavn
  BEFORE INSERT OR UPDATE
  ON plants.species
  FOR EACH ROW
  EXECUTE PROCEDURE functions.update_plants_artnavn();



--Put autorstring into separate field
UPDATE insects.species
SET autorstring = substring(species_latin, '[a-z\.]+[ ][a-z\.]+[ ](.+)')



