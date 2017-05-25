DIR=/data/groups/uphere/wikidata
tail -n +2 $DIR/public_companies.csv| awk -F ',' '{print $1}' | awk -F "/" '{print $NF}' > ne.org
tail -n +2 $DIR/businesspersons.csv | awk -F ',' '{print $1}' | awk -F "/" '{print $NF}' > ne.person   
tail -n +2 $DIR/employees.csv       | awk -F ',' '{print $1}' | awk -F "/" '{print $NF}' >> ne.person

QEDIR=/opt/develset.rss/
grep -Fwf ne.person /opt/develset.rss/wikidata.all_entities > uid.person
grep -Fwf ne.org /opt/develset.rss/wikidata.all_entities    > uid.org
grep -Fwf <(cat ne.*) /opt/develset.rss/wikidata.properties > property
cat uid.* > uid
