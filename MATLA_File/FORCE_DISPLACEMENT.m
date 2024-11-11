%% Fuerza - Desplazamiento DEM
% Boris Rojo Tanzi
% 2022/11/07

close all
clear all
clc

%% Archivos a leer

path = cd ;
[file,path] = uigetfile(fullfile(path(1:end-11),'LOAD*.dat'),'Select File');
Ffile = fullfile(path, file);
[file,path] = uigetfile(fullfile(path,'BOUND*.dat'),'Select File');
Dfile = fullfile(path, file);

clear 'file'

%% Lectura de Archivos

Force = readfileDEM(Ffile);
Displ = readfileDEM(Dfile);

%% Plot

fig1 = figure();
   axes1 = axes('Parent',fig1);
   hold(axes1,'on');
   %plot(-Displ.LOY01,Force.LOY01);
    plot(-Displ.LOY01*1000+0.6,Force.LOY01/1000);
plot(LOAD.Load(:,2),LOAD.Load(:,1))
   xlabel('Displacement');
   ylabel('Force');
   set(axes1,'FontSize',14);
   grid on
   box on
   saveas(gcf,fullfile(path,'Force-Disp.png'))