#!/user/bin/python3
#bert word vector extract
#from sklearn.metrics.pairwise import cosine_similarity
from bert_serving.client import BertClient
import pandas as pd
import sys

bc = BertClient(ip='119.3.218.106')  # ip address of the remote machine

df=pd.read_csv(sys.argv[1],header=None,index_col=0)
strlist=df.index.tolist()
if  len(strlist) <= 100:
    vec=bc.encode(strlist) # each sentense one 768 vector
    re=pd.DataFrame(vec)
    re.to_csv("./tmp/"+sys.argv[2],header=None,index=0) #docker tmp path

