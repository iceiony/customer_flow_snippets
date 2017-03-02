function prediction = predictdays(shop,N)

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
% prediction.


if N<=0, prediction = shop; return; end

nDays = length(shop);        % How many days total?

prePeriod = 14;          % How long before day n to look (for local sales behaviour)

week = nDays : -7 : (prePeriod+1);            % Decreasing in intervals of 7 (to index our previous weeks)
lookSeg = (-prePeriod:1)';                             % Period of time beforehand to look at local sales behaviour

preDays = find(lookSeg<=0);                     % Days strictly prior
theDay = find(lookSeg==0);                      % The day at hand (on a given week)

IDX = bsxfun(@plus,lookSeg,week);           % Actually make the indices
IDXpred = IDX(1:end-1,1);                              % Pull out the most recent week. This is what we are predicting
IDX(:,1)=[];                                                         % Remove that week from what will be used to make the prediction

% First do predictor weeks
dataSeg = shop(IDX);                                            % Pull out the local period for each week
M = nanmean(dataSeg(preDays,:,:));                 % Get the average sales for each local period
SD = nanstd(dataSeg(preDays,:,:));                     % And Std D
dataSegZ = bsxfun(@rdivide,bsxfun(@minus,dataSeg,M),SD);          % Z score them
dataSegZdiff = diff(dataSegZ);                                                                     % Get the day-to-day change in z-scored sales
MdiffPre = nanmean(dataSegZdiff(1:size(dataSegZdiff,1)-1,:,:));        % And get the average of these day to day changes (crude measure of whether sales were generally going up or down)
SDdiffPre = nanstd(dataSegZdiff(1:size(dataSegZdiff,1)-1,:,:));            % And standard deviation
dataOnDay = dataSeg(theDay,:,:);                                                               % What were the sales on the day in question? (not z scored)
dataOnDayZ = dataSegZ(theDay,:,:);                                                               % What were the sales on the day in question? (z scored)
whichDay = IDX(theDay,:,:);                                                                             % Which day was it (i.e. over whole data set. Are we day 3 or day 376?)

% Make a multivariate dataset with a vector for each week containing:
% (1) which day (3 or 367?) , (2) sales that day (not zscore) , (3) sales that day
% (zscore) , (4) average local sales in the preceding two weeks, (5) StdD of
% same, (6) Average day-to-day change in sales (zscore), (7) StdD of same
X = cat(1,whichDay,dataOnDay,dataOnDayZ,M,SD,MdiffPre,SDdiffPre);         % Put all of these things into a multivariate 
nextDay = dataSegZdiff(end,:,:);          % What was the change in sales the next day? (in z-score)


% Then do week to predict    (i.e. get the same vector for the week we want
% to predict)
dataSeg = shop(IDXpred);
M = nanmean(dataSeg(preDays));
SD = nanstd(dataSeg(preDays));
dataSegZ = (dataSeg-M)/SD;
dataSegZdiff = diff(dataSegZ);
MdiffPre = nanmean(dataSegZdiff);
SDdiffPre = nanstd(dataSegZdiff);
dataOnDay = dataSeg(theDay);
dataOnDayZ = dataSegZ(theDay);
whichDay = IDXpred(theDay);
Y = cat(1,whichDay,dataOnDay,dataOnDayZ,M,SD,MdiffPre,SDdiffPre);

Mvar = nanmean(X,2);    % Get average of each variable in vector
SDvar =nanstd(X,[],2);      % And StdD

% Use them to normalize predictor weeks and week to be predicted
XforPred = bsxfun(@rdivide,bsxfun(@minus,X,Mvar),SDvar)';
YforPred = ((Y-Mvar)./SDvar)';

D = 1./pdist2(XforPred,YforPred);           % Get 1 over Euclidean distance (similarity measure) between week to predict and predictor weeks
W = D/nansum(D);                            % Use to define weights

y = nansum(nextDay(:).*W(:));           % Predicted change in sales for next day this week (in zscores)
nextStep = y*SD*sqrt(2);                   % Transform out of z-score domain (N.B. we multiply by root 2 and don't add the mean because we got differences of z-scores. These differences already took care of the mean. And they amplified the StdD by root 2)

thisPrediction = [shop,shop(find(~isnan(shop),1,'last'))+nextStep];          % Put in the prediction for this step
prediction = predictdays(thisPrediction,N-1);                                                      % And reiterate


end