function plotclass(D,ind1,ind2);
% Plots two coordinates ind1,ind2 from a data matrix D where
% each row is a sample and the last column is a class index.
% Author: Jaakko Peltonen
% Date added: March 29, 2006
colors={'k.','r.','g.','b.','c.','m.','y.'};
minclass=min(D(:,end));
maxclass=max(D(:,end));
h=ishold;

for i=1:size(D,1),
  plot(D(i,ind1),D(i,ind2),colors{D(i,end)-minclass+1});
  hold on;
end;
if h==1, hold on; else hold off; end;
