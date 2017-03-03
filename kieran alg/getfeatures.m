function [theXs,theNextDays] = getfeatures(shops)
shops=shops';

% Initialize Outputs
theXs = nan([7,size(shops)] - [0 1 0]);             % These will be the features
theNextDays = nan([1,size(shops)]-[0 1 0]);           % These will be the next-day changes in sales

% Initialize some variables
nDays = size(shops,1);       
nShops = size(shops,2);
prePeriod = 13;          % How long before day n to look (for local sales behaviour)

days = (prePeriod+1):nDays-1;            % Day numbers
lookSeg = (-prePeriod:1)';                             % Period of time around each day to look at local sales behaviour
preDays = find(lookSeg<=0);                     % Days strictly prior
theDay = find(lookSeg==0);                        % The day at hand 

% Actually make the indices
IDX = bsxfun(@plus,lookSeg,days);           
IDX = bsxfun(@plus,IDX,reshape((0:nShops-1)*nDays,[1,1,nShops]));


% Start calculating measures
dataSeg = shops(IDX);                                                                                   % Pull out the local period for each week
M = nanmean(dataSeg(preDays,:,:));                                                        % Get the average sales for each local period
SD = nanstd(dataSeg(preDays,:,:));                                                            % And Std D
dataSegZ = bsxfun(@rdivide,bsxfun(@minus,dataSeg,M),SD);          % Z score them
dataSegZdiff = diff(dataSegZ);                                                                     % Get the day-to-day change in z-scored sales
MdiffPre = nanmean(dataSegZdiff(1:size(dataSegZdiff,1)-1,:,:));        % And get the average of these day to day changes (crude measure of whether sales were generally going up or down)
SDdiffPre = nanstd(dataSegZdiff(1:size(dataSegZdiff,1)-1,:,:));            % And standard deviation
dataOnDay = dataSeg(theDay,:,:);                                                               % What were the sales on the day in question? (not z scored)
dataOnDayZ = dataSegZ(theDay,:,:);                                                               % What were the sales on the day in question? (z scored)
whichDay = IDX(theDay,:,:);                                                                             % Which day was it (i.e. over whole data set. Are we day 3 or day 376?)

% Have to reconvert back from linear indices
whichDay = bsxfun(@minus,whichDay,reshape((0:nShops-1)*nDays,[1,1,nShops]));

% Make a multivariate dataset with a vector for each week containing:
% (1) which day (3 or 367?) , (2) sales that day (not zscore) , (3) sales that day
% (zscore) , (4) average local sales in the preceding two weeks, (5) StdD of
% same, (6) Average day-to-day change in sales (zscore), (7) StdD of same,
X = cat(1,whichDay,dataOnDay,dataOnDayZ,M,SD,MdiffPre,SDdiffPre);

% What was the change in sales the next day? (in z-score)
nextDays = dataSegZdiff(end,:,:);          

% Output measures
theXs(:,days,:) = X;
theNextDays(1,days,:) = nextDays;

end