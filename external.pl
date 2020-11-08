:- module(external, [external_knowledge/2]).

% Zewnętrzna wiedza (docelowo np. Python)
external_knowledge([1,1],1).
external_knowledge([1,2],2).
external_knowledge([1,3],1).
external_knowledge([1,4],2).
external_knowledge([1,5],x).
external_knowledge([2,1],x).
external_knowledge([2,2],2).
external_knowledge([2,3],x).
external_knowledge([2,4],2).
external_knowledge([2,5],1).
external_knowledge([3,1],1).
external_knowledge([3,2],3).
external_knowledge([3,3],2).
external_knowledge([3,4],2).
external_knowledge([3,5],0).
external_knowledge([4,1],0).
external_knowledge([4,2],1).
external_knowledge([4,3],x).
external_knowledge([4,4],2).
external_knowledge([4,5],1).
external_knowledge([5,1],0).
external_knowledge([5,2],1).
external_knowledge([5,3],1).
external_knowledge([5,4],2).
external_knowledge([5,5],x).