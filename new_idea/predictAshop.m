% Select a random shop and do the prediction

shop = data(3,:);         % Choose a shop

nDays2Predict = 14;        % How many days to predict?

prediction = predictdays(shop(1:end-nDays2Predict),nDays2Predict);      % Do the prediction (Take the shop's sales up to 14 days ago and predict the last 14 days)

shop2plot = shop(end-4*nDays2Predict:end);                    % Pull out a late bit of the time series for plotting
pred2plot = prediction(end-4*nDays2Predict:end);

nsamples = length(shop2plot);                                          % Get the x-axis (days relative to final "known" day)
t = (1:nsamples) - (nsamples-nDays2Predict);

%close;figure('outerposition',get(0,'screensize'),'color','w');      % Do the plotting
clf()
plot(t,shop2plot); hold on; plot(t,pred2plot,'r')
legend('Real Data','Prediction','location','northwest')
xlabel('Days (relative to final non-predicted day)','fontweight','bold')