import networkx as nx
from collections import defaultdict

g = nx.generators.random_graphs.powerlaw_cluster_graph(20,10,0.2, seed = 0) 
#g = nx.random_graphs.barabasi_albert_graph(,3)
c=0
es = defaultdict(list)
for e in g.edges:
    s = e[0]
    t = e[1]
    if s == t:
        continue
    es[s].append(str(t))
    es[t].append(str(s))
    c+=1

#print(edges)
for e in es.keys():
    #print(e + str(edges[e]))
    print(str(e) + " "+ " ".join(set(es[e])))
print(c)
