%%% OCTAVE COMMANDS - FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

%% FUNCTIONS
function y = squareThisNumber(x)
y=x^2;
end;

function [y1,y2] = squareAndCubeThisNumber(x)
y1=x^2;
y2=x^3;
end;

function J = costFunctionJ(X, y, theta)

%X is the "design matrix" containing our training examples
%y is the class labels

m = size(X,1);				%number of training examples
preds = X*theta;			%predictions of hypothesis on all m
sqrErrs = (preds-y).^2;		%squared errors

J = 1/(2*m) * sum(sqrErrs);
end;