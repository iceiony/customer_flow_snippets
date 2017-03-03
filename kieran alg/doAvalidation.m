function [L,predictions] = doAvalidation

fprintf('\nLoading Data...\n')
loadData;
fprintf('Data loaded\n')

fprintf('Calculating features...\n')
nDays2Predict = 14;
[theXs,theNextDays] = getfeatures(data(:,1:end-nDays2Predict));

% Load in the measure importance parameters saved by fminsearch
loadin = load('output2.mat');
wSmall = loadin.wSmall;
wBig = loadin.wBig;

fprintf('Starting predictions...\n')
[L,predictions] = predictAllShops(data,wSmall,wBig,theXs,theNextDays);

end