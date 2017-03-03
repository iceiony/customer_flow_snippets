function [prediction] = predictdays(shop,N,wMeas,b,theXs,theNextDays)

% Basic idea is that we look at the most recent day that we know. Look at
% what had been going on for the last two weeks (or whatever else we set
% "prePeriod" to) in terms of average sales and day-to-day change in sales,
% standard deviation of both, these things represented by z-scores etc.
% 
% Then we go back in intervals of 7 days (so we are always looking at the
% same day of the week) and see what had been going on for the two-week
% period prior to then on the same measures.
%
% With this info, we get a measure of how similar each of those previous
% days were to today, and we use that to determine a weighting of their
% contibution to the prediction.
%
% Once we have those weights, we look at what the next day's sales were on
% all previous occasions and get the weighted average. That is our
% prediction (within some constraints based on the minimum and maximum
% sales over the last week)

% Recursive function. End recursion when N is 0
if N<=0, prediction = shop; return; end

% If we have no data today, temporarily remove last week for purposes of
% prediction, and then put it back in at the end (so all the days are lined
% up properly)
appendatend=[];
while isnan(shop(end))
    appendatend = [appendatend,shop(end-6:end)];          
    shop(end-6:end)=[];
end

nDays = length(shop);                                % What day are we predicting from?
prePeriod = 13;                                                  % How long before day n to look (for local sales behaviour)
weeks = nDays : -7 : (prePeriod+1);            % Decreasing in intervals of 7 (to index our previous weeks)
lookSeg = (-prePeriod:0)';                             % Period of time beforehand to look at local sales behaviour
theDay = find(lookSeg==0);

% Get features for this week
dataSeg = shop(weeks(1)+lookSeg);                 % Segment of data to look at
M = nanmean(dataSeg);                                       % Mean sales
SD = nanstd(dataSeg);                                           % and SD
dataSegZ = (dataSeg-M)/SD;                                % Sales normalized by z-scoring
dataSegZdiff = diff(dataSegZ);                             % Day-to-day change in sales (in zscores)
MdiffPre = nanmean(dataSegZdiff);                   % Mean Day-to-day change in sales (in zscores)
SDdiffPre = nanstd(dataSegZdiff);                      % And SD
dataOnDay = dataSeg(theDay);                           % Sales on the day
dataOnDayZ = dataSegZ(theDay);                       % Sales on the day as a zscore
whichDay = weeks(1);                            % Which day is it (will be used for part of the similarity measure as distance in time)

% Measure simililarity to this week based on these measures
Y = cat(1,whichDay,dataOnDay,dataOnDayZ,M,SD,MdiffPre,SDdiffPre);

% Measure similarities of these weeks' measures to this week's measures
X = theXs(:,weeks(2:end));

% And these are the next-day changes to base the prediction on
nextDays = theNextDays(1,weeks(2:end));

Mvar = nanmean(X,2);    % Get average of each measure
SDvar =nanstd(X,[],2);      % And StdD
                                                  % ... for normalizing

% Use them to normalize predictor weeks and week to be predicted
XforPred = bsxfun(@rdivide,bsxfun(@minus,X,Mvar),SDvar)';
YforPred = ((Y-Mvar)./SDvar)';

% Weight each measure to determine how strongly it determines similarity
% (these weights have been derived with matlab's fminsearch)
XforPred = bsxfun(@times,XforPred,wMeas);
YforPred = YforPred.*wMeas;

D = (1./pdist2(XforPred,YforPred));           % Get 1 over Euclidean distance (similarity measure) between this week and predictor weeks
W = D/nansum(D);                                         % Use to define weights (for averaging each week's next-day sales)

y = nansum(nextDays(:).*W(:));           % Predicted change in sales for tomrrow (in zscores)
nextStep = y*SD*sqrt(2);                         % Transform out of z-score domain (N.B. we multiply by root 2 and don't add the mean because we got differences of z-scores. These differences already took care of the mean. And they amplified the StdD by root 2)

% impose the min and max sales over the last week as a lower and upper
% bound of the prediction to safeguard against crazy predictions
minLastWeek = nanmin(shop(end-6:end));
maxLastWeek = nanmax(shop(end-6:end));

% dataOnDayRelative = mean(dataOnDay>shop(~isnan(shop)));
% ismadDataOnDay = dataOnDayRelative < 0.02 || 0.98 < dataOnDayRelative;

thisPrediction = min([max([dataOnDay+nextStep,minLastWeek]),maxLastWeek]);          % Put in the prediction for this step
newSales = [shop,thisPrediction];

theXs = [theXs,Y];                                     % Yesterday's prediction is tomorrow's data
theNextDays = [theNextDays,y];
[prediction] = predictdays(newSales,N-1,wMeas,b,theXs,theNextDays);                              % And reiterate

% If we took data out at the beginning, put it back in again
if ~isempty(appendatend)
    prediction = [prediction(1:end-N),appendatend,prediction(end-N+1:end)];
end


end