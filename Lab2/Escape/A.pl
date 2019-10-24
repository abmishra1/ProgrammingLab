edge('CU', 'G1', 0).
edge('CU', 'G2', 0).
edge('CU', 'G3', 0).
edge('CU', 'G4', 0).

edge('G1', 'G5', 4).
edge('G2', 'G5', 6).
edge('G3', 'G5', 8).
edge('G4', 'G5', 9).

edge('G1', 'G6', 10).
edge('G2', 'G6', 9).
edge('G3', 'G6', 3).
edge('G4', 'G6', 5).

edge('G5', 'G6', 7).
edge('G5', 'G7', 3).
edge('G5', 'G8', 9).
edge('G5', 'G10', 4).
edge('G5', 'G11', 6).
edge('G5', 'G12', 7).

% add edge G6 -> G5
edge('G6', 'G7', 10).
edge('G6', 'G8', 2).
edge('G6', 'G10', 9).
edge('G6', 'G11', 5).
edge('G6', 'G12', 3).

edge('G7', 'G10', 2).
edge('G7', 'G11', 5).
edge('G7', 'G12', 7).
edge('G7', 'G8', 10).

edge('G8', 'G9', 3).
edge('G8', 'G10', 8).
edge('G8', 'G11', 4).
edge('G8', 'G12', 3).

edge('G10', 'G11', 2).
edge('G10', 'G12', 5).
edge('G10', 'G15', 5).

edge('G11', 'G12', 4).
edge('G11', 'G13', 5).
edge('G11', 'G15', 4).

edge('G12', 'G13', 4).
edge('G12', 'G14', 5).

edge('G13', 'G14', 4).
edge('G13', 'G15', 3).
edge('G14', 'G17', 5).
edge('G14', 'G18', 4).
edge('G17', 'G18', 8).


connected(U, V, W) :-
    edge(U, V, W);
    edge(V, U, W).

gen_path(CV, CC, CP, MINC, MINP) :-
    edge(CV, NV, W),
    NC is CC + W,
    gen_path(NV, NC, [CV|CP], MINC, MINP).