#-*- coding: utf-8 -*-

#-----------------------------------------------------#
# Este programa visa converter os cods sinteticos para#
#o cods real                                          #
#-----------------------------------------------------#


######################PACOTES##########################
import sys
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
sys.path.insert(0,'../modules')
from appynho_2 import plotagem as plm2
#######################################################



#Adicionando o meu Debugger:
def pause():
    programPause = input("Press the <ENTER> key to continue...")
    return
  
#----------------------------------------------------------#
# Leitura do data frame que contém os canais da perfilagem #
#----------------------------------------------------------#
df = pd.read_csv("../inputs/Sintetico/BD_convolvido.txt", sep='\s+', skiprows=1 , 
                 names=('Rock','Code' ,'Depth(m)','RHOB','GR','SP','DT'))

#--------------------------------------------------------#
# Criando os vetores e dicionários para a conversão      #
#--------------------------------------------------------#

#Arquivo de propriedades:
codereal = np.zeros(len((df['Depth(m)'])))
codesint = np.array(df['Code'])
rocksint= df['Rock']
rockreal = [0.0] * np.size(codereal, axis=0) #Esta variável precisa ser um character
prof = np.array(df['Depth(m)'])
rhob = np.array(df['RHOB'])
dt = np.array(df['DT'])
gr = np.array(df['GR'])
sp = np.array(df['SP'])


#Limites dos laço:
fim = np.size(df['Depth(m)'], axis=0)


#dicionários:

sintetico ={1:['#00d34e','Shale2'],
            2:['#82a7dd','Dolomite'],
            3:['#ff0004','Diabase'],
            4:['#ff6004','Conglomerate'],
            451:['#f4a15a','Fault Zone (Co-Cr 20%)'],
            452:['#ffddaa','Fault Zone (Co-Cr 40%)'],
            453:['#ffd9b7','Fault Zone (Co-Cr 60%)'],
            454:['#faead6','Fault Zone (Co-Cr 80%)'],
            5:['#ffbca4','Crystalline'],
            6:['#7b7b01' ,'Shale1'],
            151:['#c9f3a6','Fault Zone (Sh2-Cr 20%)'],
            152:['#f6fdb4','Fault Zone (Sh2-Cr 40%)'],
            153:['#ecdda3','Fault Zone (Sh2-Cr 60%)'],
            154:['#a8a495','Fault Zone (Sh2-Cr 80%)'],
            7:['#173c72' ,'Halite'],
            8:['#a70001','Granite'],
            9:['#ffe57b' ,'Sandstone'],
            10:['black', 'Basalt']}

real =  {    8:['#0080ef','Calciferous sandstone'],
            42:['#ffbf20','Conglomerate'],
            44:['#10ef60','Diamictite'],
            49:['#ffff40','Sandstone'],
            54:['#af2050','Siltstone'],
            57:['#40ff00' ,'Shale'],
            65:['#ff00ff' ,'Diabase'],
            66:['#f900f9','Basalt'],
            70:['#ff0000' ,'Metamorphic']}

#--------------------------------------------------------#
#                Laço da conversão                       #
#--------------------------------------------------------#

for i in range(fim):
    if  codesint[i] == 1:
        codereal[i] = 57
        rockreal[i] = 'Shale'
    if  codesint[i] == 2:
        codereal[i] = 8
        rockreal[i] = 'Calciferous_Sandstone'
    if  codesint[i] == 3:
        codereal[i] = 65
        rockreal[i] = 'Diabase'
    if  codesint[i] == 4:
        codereal[i] = 42
        rockreal[i] = 'Conglomerate'
    if  codesint[i] == 451 and codesint[i] == 452:
        codereal[i] = 42
        rockreal[i] = 'Conglomerate'
    if  codesint[i] == 453 and codesint[i] == 454:
        codereal[i] = 70
        rockreal[i] = 'Metamorphic'
    if  codesint[i] == 5:
        codereal[i] = 70
        rockreal[i] = 'Metamorphic'
    if  codesint[i] == 6:
        codereal[i] = 57
        rockreal[i] = 'Shale'
    if  codesint[i] == 151 and codesint[i] == 152:
        codereal[i] = 57
        rockreal[i] = 'Shale'
    if  codesint[i] == 153 and codesint[i] == 154:
        codereal[i] = 70
        rockreal[i] = 'Metamorphic'
    if  codesint[i] == 7:
        codereal[i] = 7
        rockreal[i] = 'Halite'
    if  codesint[i] == 8:
        codereal[i] = 80
        rockreal[i] = 'Granite'
    if  codesint[i] == 9:
        codereal[i] = 49
        rockreal[i] = 'Sandstone'
    if  codesint[i] == 10:
        codereal[i] = 66
        rockreal[i] = 'Basalt'
    #print(codereal[i], rockreal[i], i)
    #pause()




#--------------------------------------------------------#
#  Incorporando o array com codigos de rocha no no       #
# Esta info é especifica para cada caso de poço          #
#--------------------------------------------------------#

df['Code'] = codereal
df['Rock'] = rockreal


print(df)
# ------------------------------------------------------#
#           Salva o dataframe                           #
# ------------------------------------------------------#

BD_converted = pd.DataFrame(df,columns = ['Rock', 'Code', 'Depth(m)', 'RHOB', 'GR','SP','DT'])
BD_converted.to_csv('../inputs/Real/BD_converted.txt', sep=' ', index=False) 






  
