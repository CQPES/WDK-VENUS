% startup script to make Octave/Matlab aware of the GPML package
%
% Copyright (c) by Carl Edward Rasmussen and Hannes Nickisch 2018-08-01.

disp ('executing gpml startup script...')
mydir = fileparts ('E:\gpml-matlab-v4.2-2018-06-11\');        % where am I located
addpath (mydir);

% core folders
dirs = {'cov','doc','inf','lik','mean','prior','util'};
for d = dirs
  addpath (fullfile (mydir, d{1}))
end

% minfunc folders
dirs = {{'util','minfunc'},{'util','minfunc','compiled'}};
for d = dirs
  addpath (fullfile (mydir, d{1}{:}))
end

addpath([mydir,'/util/sparseinv'])


%Generate inputdata=load('input');
inputdata=load('trainingset');
%inputtest=load('icstest');
inputtest=load('testset');
[ndata,ndim]=size(inputdata);
[ndata1,ndim1]=size(inputtest);
nbond=ndim-1;
x=inputdata(:,1:nbond);
y=inputdata(:,ndim);
xs=inputtest(:,:);
meanfunc=[]; hyp.mean=[];
%meanfunc=@meanConst; hyp.mean=[5];
%meanfunc = {@meanSum, {@meanLinear, @meanConst}}; hyp.mean = [0.5; 0.5; 1];
%meanfunc = {@meanPoly,2}; hyp.mean = [0.5; 0.5; 0.5; 0.5];
covfunc={@covMaternard,5}; hyp.cov=[5/2; 5/2; 5/2; 1];
%covfunc={@covMaternard,5}; hyp.cov=[0.5; 0.5; 0.5; 1];
%covfunc={@covMaternard,5}; hyp.cov = log([1/4; 1/4; 1]);
%covfunc=@covSEiso;hyp.cov=[1; 0]
%covfunc=@covZero;hyp.cov=[]
%covfunc=@covNNone;hyp.cov=[1; 0]
%covfunc={@covMatern,3};hyp.cov=[0 0]
%covfunc=@covPERiso; hyp.cov=[1; 0]
%covfunc={@covMaterniso,5}; hyp.cov=[1; 0]
likfunc=@likGauss;
%hyp=struct('mean',hyp.mean,'cov',[-0.6; -0.6; 0],'lik',-1)
%hyp=struct('mean',[],'cov',[-0.6; -0.6; 0],'lik',-5)
tic;
%tStart_1 = cputime;
hyp=struct('mean',hyp.mean,'cov',hyp.cov,'lik',-1);
hyp2=minimize(hyp,@gp,-500,@infGaussLik,meanfunc,covfunc,likfunc,x,y);
[nlZ, dnlZ]=gp(hyp2,@infGaussLik,meanfunc,covfunc,likfunc,x,y);
elapsed_time_1 = toc;
%tEnd_1 = cputime - tStart_1;
%n1m1=gp(hyp2,@infGaussLik,meanfunc,covfunc,likfunc,x,y)

tic;
%tStart_2 = cputime;
fid = fopen(('output.ics1'),'w');
[ymu, ys2]=gp(hyp2,@infGaussLik,meanfunc,covfunc,likfunc,x,y,xs);
for i =1 : ndata1
fprintf(fid, '%10.4f%10.4f%10.4f%20.6f%20.6f \n', xs(i,:),ymu(i),ys2(i));
end
elapsed_time_2 = toc;
%tEnd_2 = cputime - tStart_2;

figure;
f = [ymu + 2*sqrt(ys2); flip(ymu - 2*sqrt(ys2),1)];
fill([xs(:,1); flip(xs(:,1),1)], f, [0.6 0.6 0.6], 'EdgeColor', 'none');
hold on;
plot(xs(:,1), ymu, 'r-', 'LineWidth', 2);
set(gca, 'LineWidth', 1.5, 'FontSize', 14, 'FontName', 'Times');

%plot(x(:,1),y,'ro','MarkerFaceColor','r');

xlabel('$\textit{E}_t$ (kcal/mol)', 'Interpreter', 'latex');
ylabel('ICS (\AA$^2$)', 'Interpreter', 'latex');

zlabel('ICS', 'Interpreter', 'latex');
legend('95\% Confidence Interval', 'Prediction','Location', 'Best', 'Interpreter', 'latex');


