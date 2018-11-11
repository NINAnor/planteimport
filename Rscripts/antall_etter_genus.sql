SELECT genus, count(*) antall
FROM insects.species
GROUP BY genus
ORDER BY antall desc


SELECT genus , family,  sum(antall) antall
FROM (SELECT species_latin, sum(amount) antall
FROM insects.container_records
GROUP BY species_latin) foo,
insects.species
WHERE foo.species_latin = species.species_latin
AND "order" != 'Collembola'
GROUP BY genus, family
ORDER BY antall desc



SELECT genus,  sum(antall) antall
FROM (SELECT species_latin, sum(amount) antall
FROM plants.container_records
GROUP BY species_latin) foo,
plants.species
WHERE foo.species_latin = species.species_latin
--AND "order" != 'Collembola'
GROUP BY genus
ORDER BY antall desc

