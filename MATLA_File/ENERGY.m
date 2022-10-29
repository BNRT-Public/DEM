%% Programa para Verificacion de Energia en DEM
% Boris Rojo Tanzi
% 23/10/2018
% 
close all
clear all
clc

%% Lectura de Archivo

path = cd ;
[file,path] = uigetfile(fullfile(path(1:end-11),'ENERGY*.dat'),'Select File');
Nfile = fullfile(path, file);

if ~isequal(file,0)
   DATA = dlmread(Nfile,',',1,0);
   
%  ENEX = Trabalho externo realizado sobre o modelo
%  ENIN = Energia interna total dissipada ou acumulada
%  ENCN = Energia cinetica total do modelo
%  ENEL = Energia elastica total presente no modelo
%  ENDP = Energia dissipada por amortecimento
%  ENGD = Energia dissipada pelo efeito de amolecimento (strain softening)
%         (somatoria sobre todas as barras)
%  DEN  = Diferencia entre energia externa e interna
%         (quanto menor este valor, mais preciso ? o resultado)

   Time = DATA(:,1);
   EnEx = DATA(:,2);
   EnCn = DATA(:,3);
   EnEl = DATA(:,4);
   EnGd = DATA(:,5);
   EnDp = DATA(:,6);
   EnIn = DATA(:,7);
   DEn  = DATA(:,8);
   clearvars('DATA')
   
   fig1 = figure();
   axes1 = axes('Parent',fig1);
   hold(axes1,'on');
   p(1) = plot(Time,EnCn,'b');
   p(2) = plot(Time,EnEl,'r');
   p(3) = plot(Time,EnGd,'g');
   p(4) = plot(Time,EnDp,'m');
   legend('Kinetic','Elastic','ENGD','ENDP','Location','northwest');
   xlabel('Time');
   ylabel('Energia');
   set(axes1,'FontSize',14);
   set(p,'LineWidth',1.5);
   grid on
   box on
end