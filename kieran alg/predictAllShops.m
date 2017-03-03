function [L,predictions] = predictAllShops(data,wSmall,wBig,theXs,theNextDays)

% predictAllShops is for validating predictions on data we have and will
% predict the last 14 days of the data input

% Set some variables
nShops = size(data,1);
nDays = size(data,2);
nDays2Predict = 14; 
predictions = zeros(nShops,nDays);

small = find(nanmean(data(:,end-50:end),2)<200);      % These shops are small
for sh=1:nShops
    shop = data(sh,1:end-nDays2Predict);
    if ismember(sh,small)
        predictions(sh,:) = predictdays(shop,nDays2Predict,wSmall,[],theXs(:,:,sh),theNextDays(:,:,sh));   
    else
        predictions(sh,:) = predictdays(shop,nDays2Predict,wBig,[],theXs(:,:,sh),theNextDays(:,:,sh));
    end
end


% Calculate loss function
L = nansum(nansum(abs((data-predictions)./(data+predictions))))/(nShops*nDays2Predict);

end