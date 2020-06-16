#-*- coding: utf-8 -*-

#-----------------------------------------------------#
# Este programa visa acoplar o arquivo convertido com #
#o arquivo *.agp                                      #
#-----------------------------------------------------#


######################PACOTES##########################
import os
import sys
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
sys.path.insert(0,'../../../modules')
from appynho_2 import plotagem as plm2
#######################################################



#Adicionando o meu Debugger:
def pause():
    programPause = input("Press the <ENTER> key to continue...")
    return
  
#----------------------------------------------------------#
# Leitura do data frame que contém os canais da perfilagem #
#----------------------------------------------------------#
df = pd.read_csv("2CB0001DASP.txt", sep='\s+', skiprows=64 ,
                 names=('Depth1(ft)','Depth2(m)' ,'CALI', 'ILD','DT','SRAT','SGR','CGR','SP','THOR','URAN','POTA'
                   'RHOB','NPHI','DRHO'))

# Retira as colunas:
df=df.drop('Depth1(ft)',axis=1) #retira a coluna da profundidade duplicada
#df=df.drop('CALI',axis=1) #retira a coluna caliper (Faz )
df=df.drop('NPHI',axis=1) #retira a coluna nphi
df=df.drop('DRHO',axis=1) #retira a coluna drho
df=df.drop('ILD',axis=1) #retira a coluna ild
df=df.drop('THOR',axis=1) #retira a canal de torio
df=df.drop('URAN',axis=1) #retira a canal de uranio
df=df.drop('POTA',axis=1) #retira a canal de pot
df=df.drop('SRAT',axis=1) #retira a canal de res
df=df.drop('CGR',axis=1) #retira a canal de gama corrigido



#Inverte as linhas do dataframe e reseta os índices:
df=df[::-1].reset_index()

#Filtra os expúrios ferramentais:
df=df[(df['RHOB'] != -999.2500) & (df['RHOB'] != -999999.9999)] 
df=df[(df['SP'] != -999.2500)   & (df[ 'SP' ] != -999999.9999)]
df=df[(df['DT'] != -999.2500)   & (df[ 'DT' ] != -999999.9999)]
df=df[(df['GR'] != -999.2500)   & (df[ 'GR' ] != -999999.9999)]
df=df[(df['CALI'] != -999.2500)   & (df[ 'CALI' ] != -999999.9999)]
#print('Classification data:',df.info())

#---------------------------------------------------------#
# Leitura do arquivo *.apg e criação do DataFrame:        #
#---------------------------------------------------------#

lito = pd.read_csv("1TP0003SCagp_mod.txt", sep='\s+', usecols=(0,2,3), 
                   index_col=False, na_values= ' ', skiprows=1, names=('Depth(m)', 'Code', 'Rock') ) 

#--------------------------------------------------------#
# Criando os vetores para a filtragem e o acoplamento    #
#--------------------------------------------------------#

#Arquivo de propriedades:
code = np.zeros(len((df['Depth1(m)'])))
rock = [0.0] * np.size(code, axis=0)#Esta variável precisa ser um character
prof = np.array(df['Depth1(m)'])
rhob = np.array(df['RHOB'])
dt = np.array(df['DT'])
gr = np.array(df['GR'])
sp = np.array(df['SP'])
#Arquivo Agp:
proflito = np.array(lito['Depth(m)'])
rocklito = np.array(lito['Rock'])
codelito = np.array(lito['Code'])
#Limites dos laços:
fim1 = len(df['Depth1(m)'])
fim2 = len(lito['Depth(m)'])


#--------------------------------------------------------#
#   Armazenando as informações nas variáveis             #
# Esta info é especifica para cada caso de poço          #
#--------------------------------------------------------#

for j in range(fim1):
    k=0
    for i in range(fim2-1):
        if prof[j] > proflito[i] and prof[j] <= proflito[i+1]:
            code[j]=codelito[k]
            rock[j] = rocklito[k]
            #print(j,i,prof[j], proflito[i], proflito[i+1],k,code[j])
            # pause()
        
        k=k+1
        # condicao extrema, ou seja, quando as profs dos perfis forem maiores que as do arquivo agp:
        if prof[j] > proflito[i+1]:
            code[j] = codelito[k]
            rock[j] = rocklito[k] 
                   
#--------------------------------------------------------#
#  Incorporando o array com codigos de rocha no no       #
# Esta info é especifica para cada caso de poço          #
#--------------------------------------------------------#
df['Code'] = code
df['Rock'] = rock


df=df.drop('index',axis=1) 

print(df)

# ------------------------------------------------------#
# Save o dataframe                                      #
# ------------------------------------------------------#

inputmodels= pd.DataFrame(df,columns = ['Rock', 'Code', 'Depth1(m)', 'RHOB', 'GR','SP','DT'])
inputmodels.to_csv('perfis_1TP0003SC.txt', sep=' ', index=False) 



#-------------------------------------------------------#
# Imgem caso seja um poco                               #
#-------------------------------------------------------#

#+++++++++++++++++++++++++++++++++++++++++++++++#
# RESUMO DAS ROCHAS ENCONTRADAS NO POCO -       #
#-----------------------------------------------#
#   8  CALCARENITO      =     6.0 M       .19 % #
#  42  CONGLOMERADO     =    29.0 M       .90 % #
#  44  DIAMICTITO       =   230.0 M      7.10 % #
#  49  ARENITO          =   838.0 M     25.88 % #
#  54  SILTITO          =   318.0 M      9.82 % #
#  57  FOLHELHO         =   648.0 M     20.01 % #
#  65  DIABASIO         =   181.0 M      5.59 % #
#  66  BASALTO          =   955.0 M     29.49 % #
#  70  METAMOR.NAO IDE. =    33.0 M      1.02 % #
#+++++++++++++++++++++++++++++++++++++++++++++++#

#Dicionário de cores do poço:


rockcolors={ 8:['#0080ef','Calciferous sandstone'],
            42:['#ffbf20','Conglomerate'],
            44:['#10ef60','Diamictite'],
            49:['#ffff40','Sandstone'],
            54:['#af2050','Siltstone'],
            57:['#40ff00' ,'Shale'],
            65:['#ff00ff' ,'Diabase'],
            66:['#f900f9','Basalt'],
            70:['#ffe57b' ,'Metamorphic']}


# Edita o tamanho do plot
padrao={'comprimento':10,
            'altura':50,
        'titulo_geral': '1TP0003SC'
}


# Desenha os plots
figure2 = plm2(5, padrao)
figure2.plot_l2(0,code,prof, rockcolors,{'titulo':'Lithology', 'descricao_y':'Depth(m)','descricao_x':'(a)'})
figure2.plot_s(1,rhob,prof,{'titulo':'RHOB \n $(g/cm^{3})$','cor':'b','alfabeto':'b','descricao_x':'(b)'})
figure2.plot_s(2,gr,prof,{'titulo':'GR \n $(ci/g$)','cor':'r','descricao_x':'(c)'})
figure2.plot_s(3,sp,prof,{'titulo':'SP\n $(mV)$', 'cor':'k','descricao_x':'(d)'})
figure2.plot_s(4,dt,prof,{'titulo':'DT \n $(\mu s/m)$','cor':'g','descricao_x':'(e)'})

figure2.legenda({'ancoragem':(-0.1, 0.09, 5.0, -0.15),'colunas':4,'ordem':[0,1,2,3,4,5,6] })
    
plt.savefig('1TP0003SC.pdf', dpi=300, bbox_inches = 'tight', transparent = True)

plt.show()
