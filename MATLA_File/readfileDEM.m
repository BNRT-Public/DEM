function [Table]=readfileDEM(NameFile)
% readfileDEM  Add one value.
%   Table = ADDME(pathfile) adds A to itself.
%
%% Lectura de archivos de texto do DEM
% Boris Rojo Tanzi
% 2022/11/07
% v0

%NameFile = "D:\ONEDRIVE_UFRGS_NEW\OneDrive - UFRGS\DOUTORADO\ENSAIOS\NanoBeams\Models\1\FORCE_T01.dat"
fid=fopen(NameFile);
title=textscan(fid, '%[^\n]', 1);
fclose(fid);
title = string(title);
title = strrep(title,':','');
title = strrep(title,' ','');
title = strip(title);
title = split(title,',');
if (title(end)=="")
    title(end) = [];
end

opts = delimitedTextImportOptions("NumVariables", length(title));
opts.DataLines = [2, Inf];
opts.Delimiter = ",";
opts.VariableNames = title;
opts.VariableTypes = repmat("double", 1,length(title));
opts.ExtraColumnsRule = "ignore";
opts.EmptyLineRule = "read";

Table = readtable(NameFile, opts);