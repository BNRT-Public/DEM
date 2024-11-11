%% Programa para Verificacion de Energia en DEM
% Boris Rojo Tanzi
% 28/11/2022
%
close all
clear all
clc

%% Lectura de Archivo

path = cd ;
[file,path] = uigetfile(fullfile(path(1:end-11),'ENERGY*.dat'),'Select File');
Nfile = fullfile(path, file);

if ~isequal(file,0)
    format long;
    opts = delimitedTextImportOptions("NumVariables", 11);

    opts.DataLines = [2, Inf];
    opts.Delimiter = ",";

    opts.VariableNames = ["TIME", "ENEX", "ENCN", "ENEL", "ENGD", ...
        "ENDP", "ENIN", "ENCND", "ENELD", "ENGDD", "ENDPD"];
    opts.VariableTypes = ["double", "double", "double", "double", "double", ...
        "double", "double", "double", "double", "double", "double"];
    opts.ExtraColumnsRule = "ignore";
    opts.EmptyLineRule = "read";
    opts = setvaropts(opts, ["TIME", "ENEX", "ENCN", "ENEL", "ENGD", ...
        "ENDP", "ENIN", "ENCND", "ENELD", "ENGDD", "ENDPD"], "FillValue", Inf);

    DATA = readtable(Nfile, opts);
    %  ENEX = Trabalho externo realizado sobre o modelo
    %  ENIN = Energia interna total dissipada ou acumulada
    %  ENCN = Energia cinetica total do modelo
    %  ENEL = Energia elastica total presente no modelo
    %  ENDP = Energia dissipada por amortecimento
    %  ENGD = Energia dissipada pelo efeito de amolecimento
    %  DEN  = Diferencia entre energia externa e interna

    clearvars('opts')

    fig1 = figure();
    axes1 = axes('Parent',fig1);
    hold(axes1,'on');
    p(1) = plot(DATA.TIME,DATA.ENCN,'b','DisplayName','Kinetic');
    p(2) = plot(DATA.TIME,DATA.ENEL,'r','DisplayName','Elastic');
    p(3) = plot(DATA.TIME,DATA.ENGD,'g','DisplayName','ENGD');
    p(4) = plot(DATA.TIME,DATA.ENDP,'m','DisplayName','ENDP');
    legend('Location','northwest');
    xlabel('Time');
    ylabel('Energy');
    set(axes1,'FontSize',14);
    set(p,'LineWidth',1.5);
    grid on
    box on
    saveas(gcf,fullfile(path,'Energy.png'))

    difEC = [0; diff(DATA.ENCN)./diff(DATA.TIME)];
    difEL = [0; diff(DATA.ENEL)./diff(DATA.TIME)];
    difEG = [0; diff(DATA.ENGD)./diff(DATA.TIME)];
    fig2 = figure();
    axes2 = axes('Parent',fig2);
    hold(axes2,'on');
    p(1) = plot(DATA.TIME,difEC,'b','DisplayName','Diff Kinetic');
%     p(2) = plot(DATA.TIME,difEL,'r','DisplayName','Diff Elastic');
%     p(3) = plot(DATA.TIME,difEG,'g','DisplayName','Diff ENGD');
    legend('Location','northwest');
    xlabel('Time');
    ylabel('Diff Energy');
    set(axes2,'FontSize',14);
%     set(p,'LineWidth',1.5);
    grid on
    box on
    saveas(gcf,fullfile(path,'diff_Energy.png'))
end

%% IGNACIO...
% Creo que lo que vos veias era esto...

% figure();
% plot(DATA.TIME,-difEG,'g','DisplayName','Diff ENGD');
% xlabel('Time');
% ylabel('Diff Energy');
% grid on
% box on
% ylim([-25 2])
% xlim([0 0.045])