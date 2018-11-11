SELECT *
FROM insects.species
ORDER BY species_latin

SELECT min(date_sampled), max(date_sampled)
FROM common.containers c, insects.container_records cr,
insects.species s
WHERE cr.species_latin = s.species_latin
AND cr.container = c.container
AND s.order = 'Collembola'

"Athena sp. B"

SELECT *
FROM insects.container_records
WHERE species_latin = 'Stenus juno'


SELECT distinct(species_latin) species
FROM insects.container_records
ORDER BY species