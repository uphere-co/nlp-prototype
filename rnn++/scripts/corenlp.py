import sys
import requests
import json

query_file = sys.argv[1]
with open(query_file, "r") as f:
    content=f.read()
r = requests.post('http://mark:9000/?properties={%22annotators%22%3A%22depparse%2Cpos%22%2C%22outputFormat%22%3A%22json%22}', data =content)
#r = requests.post('http://bill:9000/?properties={%22annotators%22%3A%22depparse%2Cpos%22%2C%22outputFormat%22%3A%22json%22}', data =content)
r.encoding='UTF-8'
#a= r.json(strict=False)
mpa = dict.fromkeys(range(32))
#a = json.loads(r.text.translate(mpa), encoding='utf-8')
a = json.loads(r.text.translate(mpa))
queries = [' '.join([token['word'] for token in sent['tokens']]) for sent in a['sentences']]
a['queries'] = queries
with open(query_file+".corenlp", "w") as f:
    f.write(json.dumps(a, indent=4))
