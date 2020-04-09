% Create for loop to line up BVR Volumes from ArcGIS with BVR water levels
% from 2009 to 2018 for daily values
% A Hounshell, 09 Apr 2020

% Load in data
[vol.data, vol.parms, vol.raw] = xlsread ('07Apr20_BVR_Volume.xlsx');
[level.data, level.parms, level.raw] = xlsread ('09Apr20_BVR_WaterLevelDaily.xlsx');

% Create for loop to add BVR volume to specific BVR water level from 2009
% to 2013
for i = 1:3750
    for j = 1:35
        if level.data(i,1) == vol.data(j,1)
            level.data(i,2) = vol.data(j,3);
        end
    end
end