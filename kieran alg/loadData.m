% Load in the sales data

[data] = csvread('shop_sales.csv',1,1);
% [num,txt,raw] = xlsread('shop_sales.csv');
% dasta = num(2:end,2:end);

nShops = size(data,1);
nDays = size(data,2);