function plotclass3(D,ind1,ind2,ind3,minclass);
% plotclass3(D,ind1,ind2,ind3,minclass)
%
% Plots three coordinates ind1-ind3 from a data matrix D where
% each row is a sample and the last column is a class index.
%
% The class colors are selected from a list: the lowest class
% index in the data is drawn with the first color. If you want
% a fixed coloring where a class not present in the data
% gets the first color, give the index of that class as the
% 'minclass' parameter. For example, if you have classes 1-10,
% but your data contains only classes 3-7, set minclass to 1
% to use a fixed coloring.
%
% Author: Jaakko Peltonen
% Date added: April 26, 2006
colors={'k.','r.','g.','b.','c.','m.','y.','ko','ro','go','bo','co','mo','yo',...
        'k*','r*','g*','b*','c*','m*','y*','kv','rv','gv','bv','cv','mv','yv',...
        'kd','rd','gd','bd','cd','md','yd'};
if nargin==4, minclass=min(D(:,end)); end;
maxclass=max(D(:,end));
h=ishold;

for i=1:size(D,1),
  plot3(D(i,ind1),D(i,ind2),D(i,ind3),colors{D(i,end)-minclass+1});
  hold on;
end;
if h==1, hold on; else hold off; end;
