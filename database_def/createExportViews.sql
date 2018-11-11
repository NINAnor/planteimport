SELECT 
row_number() OVER(order by sp.latinsknavnid) as "catalogNumber",
NULL as "recordNumber",
NULL as "institutionCode",
NULL as "collectionCode",
'insekter_i_containere_fra_planteimport' as "datasetName",
'insekter som fripassagerer i containere ved planteimport til Artskart' as "NINADescription",
genus || ' ' || species as "scientificName",
'HumanObservation' as "basisOfRecord",
'Animalia' as "phylum",
rike as "kingdom",
overklasse as "superClass",
klasse as "class",
orden as "order",
familie as "family",
underfamilie as "subFamily",
slekt as "genus",
art as "species",
underart as "subSpecies",
autorstreng as "ScientificNameAuthor",
NULL as "IdentifiedBy", ---!!!
NULL as "YearIdentified", ---!!!
NULL as "MonthIdentified", ---!!!
NULL as "DayIdentified", ---!!!
NULL as "TypeStatus",
NULL as "CollectorNumber",
NULL as "FieldNumber",
'Anders Endrestøl' as "Collector",
date_part('year', date_sampled) as "YearCollected",
date_part('month', date_sampled) as "MonthCollected",
date_part('day', date_sampled) as "DayCollected",
to_char(date_sampled, 'DDD') as "JulianCollected",
'33' AS "UTMsone",
ST_X(lo.geom) as "UTMost",
ST_Y(lo.geom) as "UTMnord",
sp.latinsknavnid as "LatinskNavnID"

FROM insects.container_records cr LEFT JOIN insects.species sp ON cr.species_latin = sp.species_latin
LEFT JOIN common.containers co ON cr.container = co.container
LEFT JOIN common.locations lo ON co.locality = lo.locality
LEFT JOIN darwin.species_names_artsdatabanken_animalia art ON sp.latinsknavnid = art.pk_latinsknavnid

LIMIT 100
