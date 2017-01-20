import sys
import json

def foo(x):
    with open('results','a') as f:
        if type(x) is str:
            sys.stdout.write(x+'\n')
        else:
            sys.stdout.write(x.encode('utf-8')+'\n')


for idx,output in enumerate(sys.stdin):
    #foo("Query: "+input_sents[idx])    
    results = json.loads(open(output.strip()).read())['results']
    for result in results:
        foo("Query: "+' '.join(result['words']))
        foo('=========================================')
        for idx, sent in zip(result['result_row_idx'],result['result_DEBUG'])[:2]:
            foo("%06s: "%idx + sent)
            foo('-------------------------')
    foo('/////////////////////////////////////////////////')
