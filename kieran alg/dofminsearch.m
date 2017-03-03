% dofminsearch looks for measure importance parameters for small and big
% shops.


loadData;
fprintf('\nData loaded\n')

fprintf('Calculating features...\n')
nDays2Predict = 14;
[theXs,theNextDays] = getfeatures(data(:,1:end-nDays2Predict));


small = find(nanmean(data(:,end-50:end),2)<200);                 % Quite arbitrarily decided that a shop was small if it's average sales over the last 50 days was less than 200
big = find(nanmean(data(:,end-50:end),2)>=200);
dataSmall = data(small,:);
dataBig = data(big,:);

w = ones(1,7);                   % Initial measure importance parameters

% First small shops
fprintf('Starting parameter search for small shops\n')
myfun = @(pm) predictAllShops(dataSmall,pm,w,theXs(:,:,small),theNextDays(:,:,small));           % Loss function
[wSmall,L] = fminsearch(myfun,w);                                                                                                          % Implement fminsearch
fprintf('Finished small shops\n')

% Then big shops
fprintf('Starting parameter search for big shops\n')
myfun = @(pm) predictAllShops(dataBig,w,pm,theXs(:,:,big),theNextDays(:,:,big));
[wBig,L] = fminsearch(myfun,w);
fprintf('Done\n')

save('output2','wSmall','wBig')