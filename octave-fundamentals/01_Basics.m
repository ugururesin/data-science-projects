%%% BASIC OCTAVE COMMANDS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% BASICS
pwd			%current wd
clear		%removes all variables
PS1('>> ')	%To change the prompt as ">> "
addpath('C.\Users\uuresin\Desktop\')	%knows this path eventhough the cwd is different!


%% LOAD/SAVE DATA
load mydata.dat
load('mydata.dat')
%
save hello.mat v		%v is saved as binary
save hello.txt v -ascii %v is saved as human-readable


%% WORKSPACE VARIABLES
who		%lists all variables
whos	%lists all variables w details


%% SET DECIMALS
format long		%long decimals
format short	%short decimals


%% LOGICAL OPERATORS
1==2 		%IS EQUAL
1~=2		%IS INEQUAL
1 && 0		%AND
1 || 0 		%OR
xor(1,0)	%XOR


%% VARIABLES
a = 'hello';	%string
b = pi;			%float


%% MATRICES
A = [1 2 3; 3 4 5]	%2x3 matrix
eye(3)				%3x3 identity matrix
ones(2,3)			%2x3 matrix with all "1"
zeros(2,3)			%2x3 matrix with all "0"
%
rand(1,3)			%1x3 matrix with random numbers
randn(1,3)			%3 random numbers, std=1, mean=0
%
size(A)		%size of matrix A
size(A,1)	%first dim of matrix A
%
A = [A, V];	%appending a vector V to RHS of A
C = [A; A]; %appending A below to A and assign to C
A .* B 		%element-wise product of A and B


%% VECTORS
V = [1 2 3]	%row vector
V = [1;2;3]	%column vector
%
length(V)	%length of vector V
%
V = 1:0.1:2	%START-INCREMENT-STOP = 1.0000 1.1000 1.2000 ... 2.0000


%% PRINTING
disp(pi);	%3.1416
disp(sprintf('2 decimals of b: %0.2f', b)) %2 decimals of b: 3.14
%
A(1,2)	%element in 1st row, 2nd column
A(1,:)	%all elements in 1st row
A(:,2)	%all elements in 2nd column
%
A([1 3],:)	%all elements in 1st & 3rd rows


%% DATA OPERATIONS
A = [1 2 3; 4 5 6; 7 8 9]		%3x3 matrix
A'	%transpose of AND
%
A < 3 				%returns 1 for <3 elements && 0 for >=3 elements
find(A<3) 			%returns indices for TRUE elements
[r,c] = find(A>3)	%returns row and col indices for TRUE elements
%
sum(A)		%sums
prod(A)		%production
floor(A)	%round up
ceil(A)		%round down
magic(3)	%returns 3x3 matrix that all diagonal summations are equal!
%
max(A,[],1)		%column-wise maximums
max(A,[],2)		%row-wise maximums
max(max(A))		%the max element
val = max(A)	%max val of A
[v,i] = max(A)	%max vals and idx