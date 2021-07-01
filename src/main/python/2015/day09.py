#!/usr/bin/env python3

import re
import sys

import networkx as nx

if __name__ == '__main__':
    input = sys.argv[1]
    graph = nx.Graph()

    # Build the graph
    with open(input) as file:
        for line in file:
            matches = re.match(r'^(.*) to (.*) = (\d+)$', line)
            start = matches.group(1)
            end = matches.group(2)
            distance = int(matches.group(3))
            graph.add_edge(start, end, weight=distance)

    path_weights = []
    for node in graph.nodes:
        other_nodes = [n for n in graph.nodes if n != node]
        for other in other_nodes:
            # Only keep paths if they use all nodes
            paths = [path for path in nx.all_simple_paths(graph, source=node, target=other) if len(path) == len(graph.nodes)]
            path_weights.extend([nx.path_weight(graph, path, 'weight') for path in paths])

    print(f'Part 1: {min(path_weights)}')
    print(f'Part 2: {max(path_weights)}')

