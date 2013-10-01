% Specify data files and import data

A = importdata('~/proj/Science13/Analyses-June2012/data.tab', '\t');
Acut = importdata('~/proj/Science13/Analyses-June2012/datacut.tab', '\t');
Az = importdata('~/proj/Science13/Analyses-June2012/dataz.tab', '\t');

X = Acut(:, 1);
Y = Acut(:, 2);
Z = Acut(:, 3);

% Full
surfc(Az(30:-1:1, 1:30));
colormap('Jet'); % Gray
shading interp; % flat, faceted
view(-20,40);

% Cut
surfc(Acut(30:-1:1, 1:20));
colormap('Gray'); % Gray
shading faceted; % flat, faceted
view(-45,35);

%Contour
surfc(Az(:, :));
colormap('Hot'); % Gray
shading interp; % flat, faceted
view(90, -90);

% Another version
surfl(Az(30:-1:1, :))
