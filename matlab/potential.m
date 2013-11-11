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


% Pretty good
surfc(Az(30:-1:1, :))
shading interp; 
colormap('Hot'); 
view(-30,45); 
 
% Gray 2D
surf(Az(30:-1:1, :));
%colormap(hot);
colormap(gray);
colormap(contrast(Az,64))
shading interp;
view(-20,40);
hold on;
contour3(Az(30:-1:1, :), 2, 'k');
colorbar;
zlim([0, max(max(Az))]);
hold off;
view(-90,90)


% Gray 3D
surf(Az(30:-1:1, :));
set(h, 'XAxisMode', 'manual')
ytick = 20:20:80; 
yticklabels = {'20', '40', '60', '80'};
set(axes_handle, 'YTick', ytick);
set(axes_handle, 'YTickLabel', yticklabels);

%colormap(hot);
colormap(gray);
colormap(contrast(Az,64))
shading interp;
view(-10,40);
hold on;
contour3(Az(30:-1:1, :), 8, 'k');
colorbar;
zlim([0, max(max(Az))]);
hold off;
ylabel('Age');
xlabel('Abundance (Log10)');
zlabel('Potential');

%n <- 59; cbind(seq(0, 30, length = n), seq(floor(min(intp$x)), round(max(intp$x)), length = n))
