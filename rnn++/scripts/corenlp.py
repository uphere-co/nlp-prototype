import sys
import os
import requests
import json


query_file = sys.argv[1]
query_dir,query_basename = os.path.split(query_file)

if(len(sys.argv)>2):
    out_path,out_basename = os.path.split(sys.argv[2])
    if out_basename == '':
        out_basename = query_basename + ".corenlp"
else:
    out_path = query_dir
    out_basename = query_basename+".corenlp"
out_file = os.path.join(out_path,out_basename)

with open(query_file, "r") as f:
    #In default, max length of a query string of CoreNLP webserver is 100000
    content=f.read()[:99000]
r = requests.post('http://mark:9000/?properties={%22annotators%22%3A%22depparse%2Cpos%22%2C%22outputFormat%22%3A%22json%22}', data =content)
#r = requests.post('http://bill:9000/?properties={%22annotators%22%3A%22depparse%2Cpos%22%2C%22outputFormat%22%3A%22json%22}', data =content)
r.encoding='UTF-8'
#a= r.json(strict=False)
mpa = dict.fromkeys(range(32))
#a = json.loads(r.text.translate(mpa), encoding='utf-8')
a = json.loads(r.text.translate(mpa))
queries = [' '.join([token['word'] for token in sent['tokens']]) for sent in a['sentences']]
a['queries'] = queries
a['max_clip_len'] = 200
with open(out_file, "w") as f:
    f.write(json.dumps(a, indent=4))
