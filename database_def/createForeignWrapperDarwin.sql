CREATE EXTENSION postgres_fdw;

CREATE SERVER "gisdata-db"
	FOREIGN DATA WRAPPER postgres_fdw
	OPTIONS(host 'gisdata-db.nina.no', port '5432', dbname 'gisdata');
	
	
CREATE USER MAPPING FOR "jens.astrom"
	SERVER "gisdata-db"
	OPTIONS(user 'postgjest', password 'gjestpost');

CREATE SCHEMA darwin;
	
IMPORT FOREIGN SCHEMA lookup_tables
	FROM SERVER "gisdata-db" INTO darwin;
	
/*
SELECT * 
FROM darwin.species_names_artsdatabanken_plantae
LIMIT 100

SELECT * 
FROM darwin.species_names_artsdatabanken_animalia
LIMIT 100
*/

Triggers:

CREATE OR REPLACE FUNCTION functions.update_insect_artnavn()
  RETURNS trigger AS
$BODY$
BEGIN
NEW.latinsknavnid := pk_latinsknavnid
FROM darwin.species_names_artsdatabanken_animalia d
	WHERE NEW.genus = d.slekt
	AND NEW.species = d.art
	LIMIT 1;
     RETURN NEW;
END
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

  CREATE TRIGGER update_insect_artsnavn
  BEFORE INSERT OR UPDATE
  ON insects.species
  FOR EACH ROW
  EXECUTE PROCEDURE functions.update_insect_artnavn();


  
CREATE OR REPLACE FUNCTION functions.update_plants_artnavn()
  RETURNS trigger AS
$BODY$
BEGIN
NEW.latinsknavnid := pk_latinsknavnid
FROM darwin.species_names_artsdatabanken_plantae d
	WHERE NEW.genus = d.slekt
	AND NEW.species = d.art
	LIMIT 1;
     RETURN NEW;
END
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

  CREATE TRIGGER update_plants_artsnavn
  BEFORE INSERT OR UPDATE
  ON plants.species
  FOR EACH ROW
  EXECUTE PROCEDURE functions.update_plants_artnavn();
  

--OR manual

UPDATE plants.species p
SET latinsknavnid = foo.pk_latinsknavnid FROM (SELECT d.pk_latinsknavnid, p.projectid
FROM plants.species p,
darwin.species_names_artsdatabanken_plantae d
WHERE p.genus = d.slekt
AND p.species = d.art) foo
WHERE foo.projectid = p.projectid


UPDATE insects.species p
SET latinsknavnid = foo.pk_latinsknavnid FROM (SELECT d.pk_latinsknavnid, p.projectid
FROM insects.species p,
darwin.species_names_artsdatabanken_animalia d
WHERE p.genus = d.slekt
AND p.species = d.art) foo
WHERE foo.projectid = p.projectid

--Put autorstring into separate field
UPDATE insects.species
SET autorstring = substring(species_latin, '[a-z\.]+[ ][a-z\.]+[ ](.+)')



