from collections import defaultdict
header = 1
edges = defaultdict(list)
with open ("musae_git_edges.csv") as f:
    for l in f:
        if header == 1:
            header = 0
            continue
        s = l[:-1].split(',')[0]
        t = l[:-1].split(',')[1]
        edges[s].append(t)
        edges[t].append(s)

#print(edges)
for e in edges.keys():
    #print(e + str(edges[e]))
    print(e + " "+ " ".join(edges[e]))
