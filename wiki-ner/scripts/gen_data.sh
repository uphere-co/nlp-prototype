DIR=/scratch/groups/uphere/wikidata
tail -n +2 $DIR/public_companies.csv| awk -F ',' '{print $1}' | awk -F "/" '{print $NF}' > ne.company
tail -n +2 $DIR/businesspersons.csv | awk -F ',' '{print $1}' | awk -F "/" '{print $NF}' > ne.business_person   
tail -n +2 $DIR/employees.csv       | awk -F ',' '{print $1}' | awk -F "/" '{print $NF}' >> ne.business_person

QEDIR=/opt/develset.rss/
grep -Fwf ne.business_person /opt/develset.rss/wikidata.all_entities > uid.business_person
grep -Fwf ne.company /opt/develset.rss/wikidata.all_entities    > uid.company
grep -Fwf <(cat ne.*) /opt/develset.rss/wikidata.properties > property
cat uid.* > uid

tail -n +2 orgs.csv | awk -F ',' '{print $1}' | awk -F "/" '{print $NF}'  > org_types
tail -n +2 persons.csv | awk -F ',' '{print $1}' | awk -F "/" '{print $NF}'  > person_types
tail -n +2 occupations.csv | awk -F ',' '{print $1}' | awk -F "/" '{print $NF}'  > occupation_types

grep -Fwf org_types $DIR/wikidata.items | awk -F '\t' '{print $1}' > items.org
grep -Fwf person_types $DIR/wikidata.items | awk -F '\t' '{print $1}' > items.person
grep -Fwf occupation_types $DIR/wikidata.items | awk -F '\t' '{print $1}' > items.occupation

grep -Fwf items.org $DIR/wikidata.all_entities > ne.org
grep -Fwf items.person $DIR/wikidata.all_entities > ne.person
grep -Fwf items.occupation $DIR/wikidata.all_entities > ne.occupation

#Note : it takes ~ 3 mins
grep -Fwf ne.person $DIR/wikidata.all_entities > uid.person
#Note : it takes ~ 80 s
grep -Fwf ne.org $DIR/wikidata.all_entities    > uid.org

