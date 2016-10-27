import psycopg2
import pandas as pd
import numpy as np
from nltk.tokenize import sent_tokenize

def query_and_fetch_all(cur, query):
    cur.execute(query)
    return cur.fetchall()
conn = psycopg2.connect("dbname=C291145_gbi_test")
print "Encoding :", conn.encoding
cur = conn.cursor()
query_and_fetch = lambda query : query_and_fetch_all(cur, query)

tables=query_and_fetch("SELECT table_name FROM information_schema.tables WHERE table_schema = 'public';")
tables=[x[0] for x in tables]
def GetYGPtable(table):
    columns=query_and_fetch("select column_name from information_schema.columns where table_name='{TABLE_NAME}';".format(TABLE_NAME=table))
    columns=[x[0] for x in columns]
    body=query_and_fetch("SELECT * FROM {TABLE_NAME};".format(TABLE_NAME=table))
    df=pd.DataFrame(body, columns=columns)
    return df

def serialize_table(table, key):
    df=table.stack().to_frame()
    df.index.names =['row_id', 'column']
    df['table_name'] = key
    df.set_index('table_name', append=True, inplace=True)
    df=df.reorder_levels(['table_name', 'column', 'row_id']).sort_index()
    return df

dfs={}
for table in tables:
    dfs[table]=GetYGPtable(table)

exports = {'autchklist2'  :["regrqr", "guidenote"],
           'regulation'   :["enregsummary", "enmainrequire"],
           'reach_reports':['content','abstract']}
for i,(x,col) in enumerate([(x,col) for x in exports for col in exports[x]]):
    print i,x,col
    df=dfs[x][col].fillna("")
    df=df[df.str.len()>5]
    df = df.str.replace("\r\n"," ").replace("\n"," ").to_frame("row_str")
    df.index.name='row_idx'
    df['row_uid']=i
    df=df.reset_index()[['row_uid','row_idx','row_str']]
    df.to_csv("/home/jihuni/nlp-prototype/build/ygp.raw.%d.%s.%s.csv"%(i,x,col), index=False, header=None)
