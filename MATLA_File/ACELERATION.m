%% Programa para Verificacion de Aceleraciones en DEM
% Boris Rojo Tanzi
% 23/10/2018

close all
clear all
clc

%% Lectura de Archivo

path = cd ;
[file,path] = uigetfile(fullfile(path(1:end-11),'ACELERATION*.dat'),'Select File');
Nfile = fullfile(path, file);

if ~isequal(file,0)
   DATA = dlmread(Nfile,',',1,0);
   
   n = (size(DATA,2)-2)./3;
   fileID = fopen(Nfile,'r');
   formatSpec = '%s';
   for i=1:n
       formatSpec = strcat(formatSpec,'%s%s%s');
   end
   formatSpec = strcat(formatSpec,'%[^\n\r]');
   
   DATAString = textscan(fileID,formatSpec,1,'Delimiter',',','TextType',...
       'string','ReturnOnError',false,'EndOfLine','\r\n');
   fclose(fileID);
   DATAString = DATAString(2:end-1);
   
   Time = DATA(:,1);
   Acel = DATA(:,2:end-1);
   
   fig1 = figure(1);
   axes1 = axes('Parent',fig1);
   hold(axes1,'on');
   p(1) = plot(Time,Acel(:,3),'b');
   p(2) = plot(Time,Acel(:,6),'r');
   xlabel('Time');
   ylabel('Energia');
   set(axes1,'FontSize',14);
   set(p,'LineWidth',1.5);
   grid on
   box on
end