SELECT min(container), max(container)
FROM common.containers
WHERE date_sampled > '2017-01-01'

SELECT distinct(species_latin)
FROM insects.species
WHERE species_latin = 'Diptera spp.'

SELECT t.species_latin
FROM common.temp_species t LEFT JOIN insects.species i ON t.species_latin = i.species_latin
WHERE i.species_latin IS NULL
ORDER BY species_latin

"Gabrius appendiculatus "