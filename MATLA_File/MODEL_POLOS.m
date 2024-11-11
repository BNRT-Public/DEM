%% Programa para Verificacion de Modelos y Polos
% Boris Rojo Tanzi
% 08/10/2019

close all
clear all
clc

%% Lectura de Archivo

path = cd ;
[file{1},folder{1}] = uigetfile(fullfile(path(1:end-11),'MODEL*.dat'),'Select File');
Nfile(1) = fullfile(folder(1), file(1));

[file{2},folder{2}] = uigetfile(fullfile(folder{1},'POLOS*.dat'),'Select File');
Nfile(2) = fullfile(folder(2), file(2));

if ~isequal(file,0)
    
    %% Lectura de Archivos de MODEL_Txx.dat
    delimiter = {''};
    startRow = 8;
    endRow = 8;
    
    formatSpec = '%s%[^\n\r]';
    fileID = fopen(Nfile{1},'r');
    dataArray = textscan(fileID, formatSpec, endRow-startRow+1, ...
        'Delimiter', delimiter, 'TextType', 'string', 'HeaderLines', ...
        startRow-1, 'ReturnOnError', false, 'EndOfLine', '\r\n');
    fclose(fileID);
    
    dataArray{1} = erase(dataArray{1},' ');
    dataArray = split(dataArray{1},',');
    
    n = size(dataArray,1)-1;
    delimiter = ',';
    startRow = 9;
    formatSpec = '%f';
    for i=1:n-3
        formatSpec = strcat(formatSpec,'%f');
    end
    formatSpec = strcat(formatSpec,'%s%s%[^\n\r]');
    
    fileID = fopen(Nfile{1},'r');
    dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, ...
        'TextType', 'string', 'HeaderLines' ,startRow-1, ...
        'ReturnOnError', false, 'EndOfLine', '\r\n');
    fclose(fileID);
    
    DATA = cell2mat(dataArray(1:end-3));
    MODEL.Node = DATA(:,1);
    MODEL.Pos = DATA(:,2:4);
    MODEL.Gfr = DATA(:,5);
    MODEL.Yng = DATA(:,6);
    MODEL.Ep = DATA(:,7);
    MODEL.Er = DATA(:,8);
    MODEL.DM = strcmp(dataArray{end-2},'T');
    MODEL.FS = strcmp(dataArray{end-1},'T');
    
    %% Lectura de Archivos de POLOS_Txx.dat
    
    delimiter = ',';
    startRow = 2;
    formatSpec = '%f%f%f%f%[^\n\r]';
    
    fileID = fopen(Nfile{2},'r');
    dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, ...
        'TextType', 'string', 'HeaderLines' ,startRow-1, ...
        'ReturnOnError', false, 'EndOfLine', '\r\n');
    fclose(fileID);
    
    DATA = cell2mat(dataArray(1:end-1));
    POLOS.Pos = DATA(:,1:3);
    POLOS.Value = DATA(:,4);
end

clearvars -except 'MODEL' 'POLOS' 'Nfile';

%% Grafico de Variables

lco = 3.50E-03;
M = 50;
N = 18;
L = 8;

YNG = 14.0E+09;
GFR = 14;

%% Grafico con Datos del Modelo

fig1 = figure();
axes1 = axes('Parent',fig1);
colormap('jet');
x = MODEL.Pos(:,1);
y = MODEL.Pos(:,2);
z = MODEL.Pos(:,3);
s = 10.*ones(size(MODEL.Gfr));
c = MODEL.Gfr;
p1 = scatter3(x,y,z,s,c);
set(p1,'MarkerFaceColor','flat')
grid(axes1,'on')
box(axes1,'on')
set(axes1,'DataAspectRatio',[1 1 1]);
colorbar(axes1);

%% Grafico con Datos de los Polos

fig2 = figure();
axes2 = axes('Parent',fig2);
colormap('jet');
x = POLOS.Pos(:,1);
y = POLOS.Pos(:,2);
z = POLOS.Pos(:,3);
s = 10.*ones(size(POLOS.Value));
c = POLOS.Value;
p1 = scatter3(x,y,z,s,c);
set(p1,'MarkerFaceColor','flat')
grid(axes2,'on')
box(axes2,'on')
set(axes2,'DataAspectRatio',[1 1 1]);
colorbar(axes2);

%% Plot Model y Polos con corte

dplane = 0;

xx = POLOS.Pos(:,1);
yy = POLOS.Pos(:,2);
zz = POLOS.Pos(:,3);
ss = 15.*ones(size(POLOS.Value));
cc = POLOS.Value;
posP = ((zz>=dplane) & (zz<=(dplane+(0.6*lco))));

x = MODEL.Pos(:,1)-(M-1).*lco.*0.5;
y = MODEL.Pos(:,2)-(N-1).*lco.*0.5;
z = MODEL.Pos(:,3)-(L-1).*lco.*0.5;
s = 5.*ones(size(MODEL.Gfr));
c = MODEL.Gfr;

posM = ((z>=(unique(zz(posP))-(0.15*lco))) & (z<=(unique(zz(posP))+(0.15*lco))));

fig3 = figure();
axes3 = axes('Parent',fig3);
colormap('jet');
hold(axes3,'on')
p1 = scatter3(x(posM),y(posM),z(posM),s(posM),c(posM));
p2 = scatter3(xx(posP),yy(posP),zz(posP),ss(posP),cc(posP));

set(p1,'MarkerFaceColor','flat')
set(p2,'MarkerFaceColor','flat')
grid(axes3,'on')
box(axes3,'on')
set(axes3,'DataAspectRatio',[1 1 1]);
colorbar(axes3);


figure()
histogram(POLOS.Value)


%