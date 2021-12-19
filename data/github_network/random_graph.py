import networkx as nx
from collections import defaultdict

g = nx.generators.random_graphs.powerlaw_cluster_graph(1000,5,0.2, seed = 0) 

es = defaultdict(list)
for e in g.edges:
    s = e[0]
    t = e[1]
    if s == t:
        continue
    es[s].append(str(t))
    es[t].append(str(s))

#print(edges)
for e in es.keys():
    #print(e + str(edges[e]))
    print(str(e) + " "+ " ".join(set(es[e])))
