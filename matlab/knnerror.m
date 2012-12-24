function classerror = knnerror(train_points, train_labels, test_points, test_labels, k);

%------------------------------------------------
% K nearest neighbour (KNN) classification error
% code by Jaakko Peltonen 2008
%
% requires the knnclassify.m script
%------------------------------------------------
% 
% Computes classification error rate, when test points are
% classified based on majority vote of their k nearest
% neighbours in train points. knnclassify.m is used to do the
% classification. Can also compute leave-one-out classification
% based only on the training data.
%
% Inputs:
%---------
% train_points: matrix, the i:th row has the features of the i:th 
%               training point.
%
% train_labels: two possible formats. 
%   Format 1: a column vector where the i:th element is an integer 
%             value indicating the label of the i:th training
%             point. The labels should start from zero.
%
%   Format 2: a matrix where the i:th row has the class memberships
%             of the i:th training point. Each class membership is
%             a value from 0 to 1, where 1 means the point fully 
%             belongs to that class.
%
% test_points and test_labels: features and labels for test points.
%    See train_points and train_labels for the allowed formats. 
%
%    If you do not give test points (that is, if you give
%    empty matrices for test_points and test_labels), then the
%    method computes leave-one-out classification error based on
%    the training data only.
%
% k:           number of neighbors used to compute the classification.
%
%
% Outputs:
%---------
% classerror: KNN classification error rate.
%

% get the KNN classifications
classifications = knnclassify(train_points, train_labels, test_points, k);


% if no test points were provided, use the train labels as test
% labels for evaluating leave-one-out classification.
if isempty(test_labels), 
  test_labels = train_labels; 
end;

% if the test labels were provided in format 1,
% convert them to format 2.
if size(test_labels,2)==1,
  nClasses = max(test_labels)+1;
  test_labels2 = zeros(size(test_labels,1),nClasses);
  for i=1:size(test_labels,1),
    test_labels2(i,test_labels(i)+1) = 1;
  end;
  test_labels = test_labels2;
end;
nClasses = size(test_labels,2);

% Compute the classification error rate.
% Note: the following code makes sense only if the
% row-wise sum of classifications is 1. knnclassify.m
% should satisfy this requirement.
classerror = 0;
for k=1:size(test_labels,1),
  classerror = classerror + sum(test_labels(k,:)) - sum(test_labels(k,:).*classifications(k,:));
end;
classerror = classerror / size(test_labels,1);