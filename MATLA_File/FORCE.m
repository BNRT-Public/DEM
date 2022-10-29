%% Programa para Verificacion de Fuerzas, Desplazamientos
% Boris Rojo Tanzi
% 03/10/2019

close all
clear all
clc

%% Lectura de Archivo

path = cd ;
[file,path] = uigetfile(fullfile(path(1:end-11),'FORCE*.dat'),'Select File');
Nfile = fullfile(path, file);

if ~isequal(file,0)
   DATA = dlmread(Nfile,',',1,0);
   
   Time = DATA(:,1);
   FZ = DATA(:,2);
   FY = DATA(:,3);
   CMOD = DATA(:,4);
   DEFL = -10.*DATA(:,5);
   
   clearvars 'DATA'
   
   fig1 = figure();
   axes1 = axes('Parent',fig1);
   hold(axes1,'on');
   p(1) = plot(1000*DEFL,-FY,'b');
   ylabel('FY');
   xlabel('DEFL');
   set(axes1,'FontSize',14);
   set(p(1),'LineWidth',1.5);
   grid(axes1,'on')
   box(axes1,'on')
end