%% Lectura de DESPLASAMIENTOS en DEM
% Boris Rojo Tanzi
% 24/02/2023

close all
clear all
clc

%% Lectura de Archivo

path = cd ;
[file,path] = uigetfile(fullfile(path(1:end-11),'DISPLACEMENT_*.dat'),'Select File');
Nfile = fullfile(path, file);

if ~isequal(file,0)
    DATA = readtable(Nfile);
    file_save = strcat(file(1:end-3),'mat');
    save(fullfile(path,file_save),'DATA');
    %  TIME = Tiempo de simulacion
    %  UX,UY,UZ = Desplazamiento del nodo N_xxxxx
else
    return
end

%% Plot
% 
% fig1 = figure(1);
% axes1 = axes('Parent',fig1);
% hold(axes1,'on');
% plot(DATA.TIME,table2array(DATA(:,2)),'DisplayName','Ux');
% plot(DATA.TIME,table2array(DATA(:,3)),'DisplayName','Uy');
% plot(DATA.TIME,table2array(DATA(:,4)),'DisplayName','Uz');
% plot(DATA.TIME,vecnorm(table2array(DATA(:,2:4)),2,2),'DisplayName','Ures');
% xlabel('Time');
% ylabel('Displecement');
% set(axes1,'FontSize',14);
% set(p,'LineWidth',1.5);
% legend()
% grid on
% box on
% saveas(gcf,fullfile(path,'Displ.png'))
