%%% LINEAR ALGEBRA OPERATIONS %%%

%% MATRICES & VECTORS
s = 2											% Initialize constant s
A = [1, 2, 3; 4, 5, 6; 7, 8, 9; 10, 11, 12] 	% The ; denotes we are going back to a new row.
v = [1;2;3] 									% Initialize a vector 
[m,n] = size(A)									% Get the dimension of the matrix A where m = rows and n = columns
dim_A = size(A)									% You could also store it this way
dim_v = size(v)									% Get the dimension of the vector v 
A_23 = A(2,3)									% Now let's index into the 2nd row 3rd column of matrix A

% Initialize matrix A and B 
A = [1, 2, 4; 5, 3, 2]
B = [1, 3, 4; 1, 1, 1]

add_AB = A + B		% See how element-wise addition works 
sub_AB = A - B 		% See how element-wise subtraction works
mult_As = A * s		% See how scalar multiplication works
div_As = A / s		% Divide A by s
add_As = A + s		% What happens if we have a Matrix + scalar?
Av = A * v			% Multiply A * v

%% MATRIX MULTIPLICATION
A = [1, 2; 3, 4;5, 6] 	% Initialize a 3 by 2 matrix 
B = [1; 2]				% Initialize a 2 by 1 matrix 
mult_AB = A*B			% Resulting matrix of (3 by 2)*(2 by 1) = (3 by 1) 

A = [1,2,0;0,5,6;7,0,9] 
A_trans = A' 				% Transpose A
A_inv = inv(A)				% Take the inverse of A 
I = eye(2) 					% Initialize a 2 by 2 identity matrix
