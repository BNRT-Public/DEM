%% Programa para Verificacion de Tensiones en DEM
% Boris Rojo Tanzi
% 23/10/2018

% close all
% clear all
% clc

%% Lectura de Archivo

path = cd ;
[file,path] = uigetfile(fullfile(path(1:end-11),'STRESS*.dat'),'Select File');
Nfile = fullfile(path, file);

if ~isequal(file,0)
   DATA = dlmread(Nfile,',',1,0);
   
   Time = DATA(:,1);
   SXX = DATA(:,2);
   SYY = DATA(:,3);
   SZZ = DATA(:,4);
   SYZ = DATA(:,5);
   SXZ = DATA(:,6);
   SXY = DATA(:,7);
   EXX = DATA(:,8);
   EYY = DATA(:,9);
   EZZ = DATA(:,10);
   EYZ = DATA(:,11);
   EXZ = DATA(:,12);
   EXY = DATA(:,13);
   
   clearvars 'DATA'
   
   fig1 = figure();
   axes1 = axes('Parent',fig1);
   hold(axes1,'on');
   p(1) = plot(Time,SXX,'r');
   p(1) = plot(Time,SYY,'b');
   %p(1) = plot(Time,SZZ,'k');
   xlabel('Time');
   ylabel('Szz');
   set(axes1,'FontSize',14);
   set(p(1),'LineWidth',1.5);
   grid(axes1,'on')
   box(axes1,'on')
   
%    fig2 = figure();
%    axes2 = axes('Parent',fig2);
%    hold(axes2,'on');
%    p(1) = plot(EYY,SYY.*1e-6,'b');
%    xlabel('Exx');
%    ylabel('Szz');
%    set(axes1,'FontSize',14);
%    set(p,'LineWidth',1.5);
%    grid on
%    box on
end