# -*- coding: utf-8 -*-
"""
Created on Wed Apr 26 12:21:48 2017

@author: IVA
"""
#%%
# https://statcompute.wordpress.com/2015/12/09/fitting-generalized-regression-neural-network-with-python/
# http://www.lwebzem.com/cgi-bin/res/view.cgi?name=gr_nn1.cgi
# http://freeconnection.blogspot.ru/2012/10/general-regression-neural-network-grnn.html
#%%
#%%%%Simulation by Matlab R2012b
clc
clear all
%%Initialization of PSO algorithm
%Calculate the initial fitness value of each particle
for i=1:popsize
pop(i,:)=abs(rands(1,1));
v(i,:)=rands(1,1)*0.01;
spread(i)=pop(i,:);
net=newgrnn(sampleinput_train,sampleoutput_train,spread(i));
testout_sim(i,:)=sim(net,sampleinput_test);
 error(i,:)= testout_expect(i,:)- testout_sim(i,:);
 fitness(i)=mse(error(i,:));
end
%Calculate the initial best personal position of the particle and best global
position of the swarm
[bestfitness bestparticle]=min(fitness);
gbest=pop(bestparticle,:);
pbest=repmat(pop,1,1);
fitnesspbest=fitness;
fitnessgbest=bestfitness;
%%Search the optimal solution by PSO algorithm
for j=1:maxiter
%update the personal best position
%update the velocity of the particle
for i=1:popsize
 w=wmax-(wmax-wmin)*j/maxiter;
 c1=(c1f-c1i)*j/maxiter+c1i;
 c2=(c2f-c2i)*j/maxiter+c2i;
 v(i,:)= w*v(i,:)+ c1*rand*(pbest(i,:)-pop(i,:))+c2*rand*(gbestpop(i,:));
 v(i,find(v(i,:)>vmax))=vmax;
v(i,find(v(i,:)<vmin))=vmin;
%Update the position of the particle
pop(i,:)=pop(i,:)+v(i,:);
pop(i,find(pop(i,:)>popmax))=popmax;
%Update the fitness value of the particle
spread(i)=pop(i,:);
net=newgrnn(sampleinput_train, sampleoutput_train,spread(i));
 testout_sim(i,:)=sim(net, sampleinput_test);
 error(i,:)= testout_expect(i,:)- testout_sim(i,:);
 fitness(i)=mse(error(i,:));
end
%Update the best personal position of the particle
for i=1:popsize
if fitness(i)<fitnesspbest(i)
pbest(i,:)=pop(i,:);
fitnesspbest(i)=fitness(i);
end
 end
%Update the global best position of the swarm
for i=1:popsize
if fitness(i)<fitnessgbest
gbest=pop(i,:);
fitnessgbest=fitness(i);
end
end
-432-
Journal of Industrial Engineering and Management – http://dx.doi.org/10.3926/jiem.1007
best_fitness(j)= fitnessgbest;
best_spread(j)= gbest;
end
%%Construct GRNN with best spread
[gbestfitness gbestparticle]=min(best_fitness);
gbestspread= best_spread(gbestparticle);
net=newgrnn(sampleinput_train, sampleinput_train,best_spread);
testout_sim(i,:)=sim(net,sampleinput_test);

#%%
