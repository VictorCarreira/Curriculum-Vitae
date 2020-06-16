#-*- coding: utf-8 -*-
#----------------------------------------------------------#
#     Este programa tem por objeto fazer uma análise       #
# estatística dos dados provenientes do programa acoplador #
#----------------------------------------------------------#


############ Pacotes necessários ############## 
import numpy as np
import pylab as py
import scipy as sp
import pandas as pd
from pandas import Series, DataFrame
###############################################


#Lê o dado bruto
df = pd.read_csv("../inputs/BD_treinamento.txt", sep='\s+' , skiprows=2, names=('Rock', 'Code' ,'Depth(m)' ,'GR', 'RHOB','DT','SP'))

#Armazena em formato de data frame
#df=pd.DataFrame(data, columns=['Rock','Code', 'Depth(m)','GR','RHOB','DT','SP'])

#print(df)

# Corta do dado da parte não-numérica
#df=df.drop([0])#cabeçalho
df=df.drop('Rock',axis=1)#coluna nome
df=df.drop('Code',axis=1)#coluna cod
df=df.drop('Depth(m)',axis=1)#coluna prof

print(df)




print('+++++++++++ Informações Estatísticas ++++++++++++')

print('Mediana')
print(df.median())
print('Media')
print(df.mean())
print('Valor maximo')
print(df.max())
print('Valor minimo')
print(df.min())



print('informacao')
print(df.info())
#print(df.cumsum())
#print(df.min()/df.max())
#print(df.idmin()/df.idmax())

print('Mediana de SP')
#print(df.describe())
#print(df['GR'].mean())
print(df.SP.median(axis = 0))

print('++++++++++++++++++++++++++++++++++++++++++++++++')

print('++++++++++++++++++Prova Real++++++++++++++++++++')

a=np.array(df.SP)
print(a)
print(type(a))
b=np.mean(a)
print(b)

print('++++++++++++++++++++++++++++++++++++++++++')





