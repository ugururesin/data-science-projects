%%% OCTAVE COMMANDS - VECTORIZATION %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

A = [1 2 3; 3 4 5]	%2x3 matrix
B = [2 2 2; 2 2 2]
V = [1; 2]
%
A = [A, V];	%appending a vector V to RHS of A
C = [A; A]; %appending A below to A and assign to C
A .* B 		%element-wise product of A and B
