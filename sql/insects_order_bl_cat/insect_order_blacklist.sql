SELECT DISTINCT ON(cr.species_latin) cr.species_latin, s.latinsknavnid,  s."order", s.alien, s.blacklist_cat
FROM insects.container_records cr LEFT JOIN insects.species s
ON cr.species_latin = s.species_latin
ORDER BY species_latin