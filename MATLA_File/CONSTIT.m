%% Programa para Verificacion de Aceleraciones en DEM
% Boris Rojo Tanzi
% 23/10/2018

close all
clear all
clc

%% Lectura de Archivo

path = cd ;
[file,path] = uigetfile(fullfile(path(1:end-11),'CONSTIT*.dat'),'Select File');
Nfile = fullfile(path, file);

if ~isequal(file,0)
   DATA = dlmread(Nfile,',',1,0);
   
   fileID = fopen(Nfile,'r');
   formatSpec = '%s';
   for i=1:size(DATA,2)-2
       formatSpec = strcat(formatSpec,'%s');
   end
   formatSpec = strcat(formatSpec,'%[^\n\r]');
   
   DATAString = textscan(fileID,formatSpec,1,'Delimiter',',','TextType',...
       'string','ReturnOnError',false,'EndOfLine','\r\n');
   fclose(fileID);
   DATAString = DATAString(2:end-1);
   
   cont = false(size(DATAString));
   NumNode = zeros(size(DATAString));
   for i=1:size(DATAString,2)
       text = strsplit(DATAString{i},':');
       NumNode(i) = str2double(text(1,2));
   end
   NumNode = unique(NumNode);
   ndata = floor(size(DATAString,2)./size(NumNode,2));
   
   Time = DATA(:,1);
   DATA = DATA(:,2:end);

   fig1 = figure();
   axes1 = axes('Parent',fig1);
   hold(axes1,'on');
   for i=1:size(NumNode,2)
   p(1) = plot(DATA(:,1+(i-1).*ndata),DATA(:,2+(i-1).*ndata),'.');
   end
  % p(2) = plot(Time,DATA(:,6),'r');
   xlabel('Deformation');
   ylabel('Force');
   set(axes1,'FontSize',14);
   set(p,'LineWidth',1.5);
   grid on
   box on
end

%%
i = 4

v = VideoWriter('constit.mp4', 'MPEG-4');
v.Quality = 10;
v.FrameRate = 10;
open(v)

x = DATA(:,1+(i-1).*ndata);
y = DATA(:,2+(i-1).*ndata);
hold on
grid on
box on
ylabel('force')
xlabel('deformacion')
plot(x,y,'.',Color=[.7 .7 .7],MarkerSize=0.2)
h = plot(nan,nan,'Color','red','LineWidth',1.8);
for k = 200001:10000:length(x)
    set(h, 'XData', x(k-200000:k), 'YData', y(k-200000:k))
    drawnow
    frame = getframe(gcf);
    f = imresize(frame.cdata,4); % resize the image
    writeVideo(v,f);
end

close(v)

%%
i=4
figure()
grid on
box on
plot(DATA(:,1+(i-1).*ndata),DATA(:,2+(i-1).*ndata),'.');
xlabel('deformacion')
ylabel('fuerza')
grid on
box on

figure()
hold on
yyaxis left
plot(Time,DATA(:,1+(i-1).*ndata),"DisplayName",'str')
plot(Time,DATA(:,3+(i-1).*ndata),"DisplayName",'ep')
plot(Time,DATA(:,4+(i-1).*ndata),"DisplayName",'er')
ylabel('deformaciones')
yyaxis right
plot(Time,DATA(:,5+(i-1).*ndata),"DisplayName",'kr')
legend()
xlabel('time')
ylabel('kr')
grid on
box on
