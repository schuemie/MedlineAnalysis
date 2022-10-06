SELECT 
{@title} ? {	medcit.art_arttitle AS title,}
{@abstract} ? {	abstract,}
	medcit.pmid
FROM @database_schema.medcit
INNER JOIN #pmids pmids
	ON medcit.pmid = pmids.pmid
{@abstract} ? {
INNER JOIN (
	SELECT medcit_art_abstract_abstracttext.pmid,
		STRING_AGG(value, '\n' ORDER BY medcit_art_abstract_abstracttext_order) AS abstract
	FROM @database_schema.medcit_art_abstract_abstracttext
	INNER JOIN #pmids pmids
		ON medcit_art_abstract_abstracttext.pmid = pmids.pmid
	GROUP BY medcit_art_abstract_abstracttext.pmid
) abstracts
	ON abstracts.pmid = medcit.pmid
}
