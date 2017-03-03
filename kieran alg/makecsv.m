function [predictions] = makecsv(data)

% Save name for csv file
savename = 'pred6';

% Load in the measure importance parameters saved by fminsearch
loadin = load('output2.mat');
wSmall = loadin.wSmall;
wBig = loadin.wBig;

% Initialize some variables
nShops = size(data,1);
nDays = size(data,2);
nDays2Predict = 14;        
predictions = zeros(nShops,nDays+nDays2Predict);

% Calculatee the features
[theXs,theNextDays] = getfeatures(data);

% Determine the small shops
small = find(nanmean(data(:,end-50:end),2)<200);

% Loop through each shop and make predictions
for sh=1:nShops
    shop = data(sh,:);
    if ismember(sh,small)
        predictions(sh,:) = predictdays(shop,nDays2Predict,wSmall,[],theXs(:,:,sh),theNextDays(:,:,sh));   
    else
        predictions(sh,:) = predictdays(shop,nDays2Predict,wBig,[],theXs(:,:,sh),theNextDays(:,:,sh));
    end
end

% Convert predictions into format for csv file
realpreds = round(predictions(:,end-nDays2Predict+1:end));
realpreds = [(1:nShops)',realpreds];

% Save the csv
csvwrite([savename,'.csv'],realpreds)


end