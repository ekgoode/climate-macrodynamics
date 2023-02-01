%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IntuitionFigures.m
% By: Greg Casey, Stephie Fried, and Ethan Goode
% Date: Spring 2022
% Purpose: Highlight the differences between level 
% and growth effects
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear; 
close
bpath = '/path/to/folder';
figpath = append(bpath, '/results/figures/');
cd(bpath)

%% Solve model for level shock
% Parameters
g = 0.02;
alpha = 0.33;
s = 0.6;
delta = 0.99;
T = 200;

% Initialize Vectors
K1 = ones(1,T);
Y1 = ones(1,T);
A1 = ones(1,T);
gY1 = ones(1,T);
Yss1 = ones(1,T);
gYss1 = ones(1,T);
Ass1 = ones(1,T);
Kss1 = ones(1,T);

% Shock Variables
shock_per = 2;
shock_value_growth = -0.5*g;
shock_value_level = -0.1;
pp = 15;

% Starting values
A1(1) = 1;
k = (s/(delta+g)) ^ (1/(1-alpha));
K1(1) = k * A1(1);

%Run model with no shock
K0(1) = K1(1);
A0(1) = A1(1);
Y0(1) = K0(1)^alpha * A0(1)^(1-alpha);
gY0(1) = g;
for t = 2:1:pp
    K0(t) = (1+g)*K0(t-1);
    A0(t)= (1+g)*A0(t-1);
    Y0(t)= (1+g)*Y0(t-1);
    gY0(t) = g; 
end
   


% Run Model with Level Shock
for t = 1:T-1
    Y1(t) = K1(t)^alpha * A1(t)^(1-alpha);
    K1(t+1) = s * Y1(t) + (1-delta)*K1(t);
    if t == shock_per
        A1(t+1) = (1+g)*A1(t) + shock_value_level*A1(t);
    else
        A1(t+1) = (1+g)*A1(t);
    end
end

gY1(1) = g;
for t=2:T-1
   gY1(t) = Y1(t)/Y1(t-1)-1; 
end

Y_over_A = Y1./A1;
K_over_Y = K1./ Y1;

ln_A = log(A1);
ln_Y = log(Y1);
ln_K = log(K1);

% Steady state values
Yss1(1) = Y1(1);
gYss1(1) = g;
Ass1(1) = A1(1);
Kss1(1) = K1(1);

for t=2:pp
   Yss1(t) = (1+g)*Yss1(t-1);
   Ass1(t) = (1+g)*Ass1(t-1);
   gYss1(t) = g;
   Kss1(t) = Kss1(t-1)*(1+g);
end


% Re-initialize Vectors
K2 = ones(1,T);
Y2 = ones(1,T);
A2 = ones(1,T);

%Starting values
A2(1) = 1;
k2 = (s/(delta+g)) ^ (1/(1-alpha));
K2(1) = k * A2(1);

for t = 1:T-1
    Y2(t) = K2(t)^alpha * A2(t)^(1-alpha);
    K2(t+1) = s * Y2(t) + (1-delta)*K2(t);
    if t < shock_per
        A2(t+1) = (1+g)*A2(t);
    else
        A2(t+1) = (1+g+shock_value_growth)*A2(t);
    end
end

gY2(1) = g;
for t=2:T-1
   gY2(t) = Y2(t)/Y2(t-1)-1; 
end


close all

%%
% Combined plots
close all
pic = 1

%TFP
figure(1)
hold on
plot(1:pp, log(A2(1:pp).^(1/(1-alpha))),  'LineWidth',2, 'Color',[49/255,130/255,189/255])
plot(1:pp, log(A1(1:pp).^(1/(1-alpha))), '-.', 'LineWidth',2, 'Color', [158/255,202/255,225/255])
plot(1:pp, log(A0(1:pp).^(1/(1-alpha))),'--',  'LineWidth',1, 'Color', [0/255,0/255,0/255])
set(gca, 'FontSize', 20)
ylim([-0.12, 0.45])
title('ln(TFP)', 'FontSize', 15)
box off
set(gca,'yticklabel',[])
set(gca,'xticklabel',[])
xticks([shock_per+1])
yticks([])
xlabel('time', 'FontSize', 18)
legend('Growth effect', 'Level effect', 'No shock', 'Location', 'NW', 'Fontsize', 12)
legend boxoff
xticklabels({'t*'})
if pic ==1
    saveas(gcf,[bpath ,filesep, 'TFP2'],'epsc');
end

%%


%Capital
figure(2)
hold on
plot(1:pp, log(K2(1:pp)),  'LineWidth',2, 'Color',[49/255,130/255,189/255])
plot(1:pp, log(K1(1:pp)), '-.', 'LineWidth',2, 'Color', [158/255,202/255,225/255])
plot(1:pp, log(K0(1:pp)),'--',  'LineWidth',1, 'Color', [0/255,0/255,0/255])
set(gca, 'FontSize', 20)
ylim([-.82, -.47])
title('ln(Capital per Capita)', 'FontSize', 15)
box off
set(gca,'yticklabel',[])
set(gca,'xticklabel',[])
xticks([shock_per+1])
yticks([])
xlabel('time', 'FontSize', 18)
xticklabels({'t*'})
if pic ==1
    saveas(gcf,[bpath ,filesep, 'K'],'epsc');
end

%%

%Output
figure(3)
hold on
plot(1:pp, log(Y2(1:pp)),  'LineWidth',2, 'Color',[49/255,130/255,189/255])
plot(1:pp, log(Y1(1:pp)), '-.', 'LineWidth',2, 'Color', [158/255,202/255,225/255])
plot(1:pp, log(Y0(1:pp)),'--',  'LineWidth',1, 'Color', [0/255,0/255,0/255])
set(gca, 'FontSize', 20)
ylim([-0.3, .05])
title('ln(Output per Capita)', 'FontSize', 15)
box off
set(gca,'yticklabel',[])
set(gca,'xticklabel',[])
xticks([shock_per+1])
yticks([])
xlabel('time', 'FontSize', 18)
xticklabels({'t*'})
if pic ==1
    saveas(gcf,[bpath ,filesep, 'Y'],'epsc');
end


%%
close all

%Growth rate of output 
figure(4)
hold on
plot(1:pp, gY2(1:pp),  'LineWidth',2, 'Color',[49/255,130/255,189/255])
plot(1:pp, gY1(1:pp), '-.', 'LineWidth',2, 'Color', [158/255,202/255,225/255])
plot(1:pp, gY0(1:pp),'--',  'LineWidth',1, 'Color', [0/255,0/255,0/255])
set(gca, 'FontSize', 20)
ylim([-0.06, 0.034])
title('Growth Rate of Output per Capita', 'FontSize', 15)
box off
set(gca,'yticklabel',[])
set(gca,'xticklabel',[])
xticks([shock_per+1])
yticks([])
xlabel('time', 'FontSize', 18)
xticklabels({'t*'})
if pic ==1
    saveas(gcf,[bpath ,filesep, 'gY'],'epsc');
end


