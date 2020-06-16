#-*- coding: utf-8 -*-

#---------------------PROGRAMA-------------------------#
# Finalidade: retirar a artificialidade angular da base# 
#              de dados sintética e dos poços de clas- #
#              sificação.                              #
# Método: Convolução no domínio da frequência usando   # 
#          FFT.                                        #
#------------------------------------------------------#



####################PACOTES###########################
import numpy as np
import pylab as py
import scipy as sp
from scipy import signal
from scipy.signal import correlate
import pandas as pd
import matplotlib
import matplotlib.pyplot as plt
#######################################################


############################## LEITURA E TRATAMENTO DOS DADOS DE ENTRADA ####################################


DB= pd.read_csv("../inputs/Sintetico/BDsintetico.txt", sep='\s+' , skiprows=2, names=('Rock', 'Code' ,'Depth' ,'RHOB', 'GR','SP','DT'))
df=pd.DataFrame(DB)
#df=df.drop('Code',axis=1) #retira a coluna codigo

c1 = pd.read_csv("../inputs/Sintetico/C1.txt", sep='\s+' , 
                 skiprows=1, names=('Rock', 'Code' ,'Depth' ,'RHOB', 'GR','SP','DT'))
df1=pd.DataFrame(c1)

c2 = pd.read_csv("../inputs/Sintetico/C2.txt", sep='\s+' ,
                 skiprows=1, names=('Rock', 'Code' ,'Depth' ,'RHOB', 'GR','SP','DT'))
df2=pd.DataFrame(c2)

c3 = pd.read_csv("../inputs/Sintetico/C3.txt", sep='\s+' ,
                 skiprows=1, names=('Rock', 'Code' ,'Depth' ,'RHOB', 'GR','SP','DT'))
df3=pd.DataFrame(c3)


############################# Armazenamento de variáveis: #####################################


# Banco de dados:
rock = df.Rock
cod = np.array(df.Code)
prof = np.array(df.Depth)*10.0 # unidade em metros
RHOB = np.array(df.RHOB)
GR = np.array(df.GR)
SP = np.array(df.SP)
DT = np.array(df.DT)



# Poço C1:
rock1 = df1.Rock
cod1 = np.array(df1.Code)
prof1 = np.array(df1.Depth)*10.0 # unidade em metros
RHOB1 = np.array(df1.RHOB)
GR1 = np.array(df1.GR)
SP1 = np.array(df1.SP)
DT1 = np.array(df1.DT)

# Poço C2:
rock2 = df2.Rock
cod2 = np.array(df2.Code)
prof2 = np.array(df2.Depth)*10.0 # unidade em metros
RHOB2 = np.array(df2.RHOB)
GR2 = np.array(df2.GR)
SP2 = np.array(df2.SP)
DT2 = np.array(df2.DT)

# Poço C3:
rock3 = df3.Rock
cod3 = np.array(df3.Code)
prof3 = np.array(df3.Depth)*10.0 # unidade em metros
RHOB3 = np.array(df3.RHOB)
GR3 = np.array(df3.GR)
SP3 = np.array(df3.SP)
DT3 = np.array(df3.DT)

################################ Definindo os Dicionários: ###################################


# Bando de Dados Sintéticos
codigo={'#00d34e':'Shale 2',
        '#82a7dd' :'Dolomite',
        '#ff0004' :'Diabase',
        '#ff6004' :'Conglomerate',
        '#f4a15a' :'Fault Zone (Co-Cr  20%)',
        '#ffddaa' :'Fault Zone (Co-Cr  40%)',
        '#ffd9b7' :'Fault Zone (Co-Cr  60%)',
        '#faead6' :'Fault Zone (Co-Cr  80%)',
        '#ffbca4' :'Crystalline',
        '#7b7b01' :'Shale 1',
        '#173c72' :'Halite',
        '#a70001' :'Granite',
        '#ffe57b' :'Sandstone'}

# Poço c1
codigo1={'#00d34e':'Shale 2',
           '#82a7dd' :'Dolomite',
           '#ff0004' :'Diabase',
           '#ff6004' :'Conglomerate',
           '#ffbca4' :'Crystalline'}

# Poço c2
codigo2={'#00d34e' :'Shale 2',
           '#82a7dd' :'Dolomite',
           '#ff0004' :'Diabase',
           '#ff6004' :'Conglomerate',
           '#7b7b01' :'Shale 1',
           '#173c72' :'Halite',
           '#000000' : 'Basalt'}

# Poço c3
codigo3={'#00d34e':'Shale 2',
           '#ff6004' :'Conglomerate',
           '#ffe57b' :'Sandstone',
           #'#f4a15a' :'Fault Zone (Co-Cr 20%)',
           #'#ffddaa' :'Fault Zone (Co-Cr 40%)',
           #'#ffd9b7' :'Fault Zone (Co-Cr 60%)',
           #'#faead6' :'Fault Zone (Co-Cr 80%)',
           '#c9f3a6' :'Fault Zone (Sh1-Cr 20%)',
           '#f6fdb4' :'Fault Zone (Sh1-Cr 40%)',
           '#ecdda3' :'Fault Zone (Sh1-Cr 60%)',
           '#a8a495' :'Fault Zone (Sh1-Cr 80%)'}


################################ Aplicando a FFT e a convolução #################################

#Banco de dados:
sig_RHOB = np.copy(RHOB) # canal a ser filtrado 
win_RHOB = signal.hann(int(len(prof)*0.010)) # aqui cria o filtro 1D (janela)
filtered_RHOB = signal.convolve(sig_RHOB, win_RHOB, mode='same') / sum(win_RHOB)
print(int(len(prof)*0.010))
sig_GR = np.copy(GR) # canal a ser filtrado 
win_GR = signal.hann(int(len(prof)*0.012))# aqui cria o filtro 1D
filtered_GR = signal.convolve(sig_GR, win_GR, mode='same') / sum(win_GR)

sig_SP = np.copy(SP) # canal a ser filtrado
win_SP = signal.hann(int(len(prof)*0.012))# aqui cria o filtro 1D
filtered_SP = signal.convolve(sig_SP, win_SP, mode='same') / sum(win_SP)

sig_DT = np.copy(DT) # canal a ser filtrado 
win_DT = signal.hann(int(len(prof)*0.015))# aqui cria o filtro 1D
filtered_DT = signal.convolve(sig_DT, win_DT, mode='same') / sum(win_DT)


#Poço C1:
sig_RHOB1 = np.copy(RHOB1)
win_RHOB1 = signal.hann(15)# aqui cria o filtro 1D
filtered_RHOB1 = signal.convolve(sig_RHOB1, win_RHOB1, mode='same') / sum(win_RHOB1)

sig_GR1 = np.copy(GR1)
win_GR1 = signal.hann(25)# aqui cria o filtro 1D
filtered_GR1 = signal.convolve(sig_GR1, win_GR1, mode='same') / sum(win_GR1)

sig_SP1 = np.copy(SP1)
win_SP1 = signal.hann(25)# aqui cria o filtro 1D
filtered_SP1 = signal.convolve(sig_SP1, win_SP1, mode='same') / sum(win_SP1)

sig_DT1 = np.copy(DT1)
win_DT1 = signal.hann(30)# aqui cria o filtro 1D
filtered_DT1 = signal.convolve(sig_DT1, win_DT1, mode='same') / sum(win_DT1)



#Poço C2:
sig_RHOB2 = np.copy(RHOB2)
win_RHOB2 = signal.hann(25)# aqui cria o filtro 1D
filtered_RHOB2 = signal.convolve(sig_RHOB2, win_RHOB2, mode='same') / sum(win_RHOB2)

sig_GR2 = np.copy(GR2)
win_GR2 = signal.hann(25)# aqui cria o filtro 1D
filtered_GR2 = signal.convolve(sig_GR2, win_GR2, mode='same') / sum(win_GR2)

sig_SP2 = np.copy(SP2)
win_SP2 = signal.hann(30)# aqui cria o filtro 1D
filtered_SP2 = signal.convolve(sig_SP2, win_SP2, mode='same') / sum(win_SP2)

sig_DT2 = np.copy(DT2)
win_DT2 = signal.hann(30)# aqui cria o filtro 1D
filtered_DT2 = signal.convolve(sig_DT2, win_DT2, mode='same') / sum(win_DT2)


#Poço C3
sig_RHOB3 = np.copy(RHOB3)
win_RHOB3 = signal.hann(25)# aqui cria o filtro 1D
filtered_RHOB3 = signal.convolve(sig_RHOB3, win_RHOB3, mode='same') / sum(win_RHOB3)

sig_GR3 = np.copy(GR3)
win_GR3 = signal.hann(25) # aqui cria o filtro 1D
filtered_GR3 = signal.convolve(sig_GR3, win_GR3, mode='same') / sum(win_GR3)

sig_SP3 = np.copy(SP3)
win_SP3 = signal.hann(30)# aqui cria o filtro 1D
filtered_SP3 = signal.convolve(sig_SP3, win_SP3, mode='same') / sum(win_SP3)

sig_DT3 = np.copy(DT3)
win_DT3 = signal.hann(30) # aqui cria o filtro 1D
filtered_DT3 = signal.convolve(sig_DT3, win_DT3, mode='same') / sum(win_DT3)





############################### Aplica o ruído no dado de saída: ######################################

# Banco de dados sintético
pure_RHOB = np.linspace(-0.1, 0.1, len(filtered_RHOB)) # imagem da função densidade de probabilidade
noise_RHOB = np.random.normal(0, 0.1, pure_RHOB.shape) #Vetor ruído
noised_filtered_RHOB = filtered_RHOB + noise_RHOB # sinal original com ruído


pure_GR = np.linspace(-0.1, 0.1, len(filtered_GR)) #  imagem da função densidade de probabilidade
noise_GR = np.random.normal(0, 7, pure_GR.shape) #Vetor ruído
noised_filtered_GR = filtered_GR + noise_GR # sinal original com ruído


pure_SP = np.linspace(-0.1, 0.1, len(filtered_SP)) # imagem da função densidade de probabilidade
noise_SP = np.random.normal(0, 10, pure_SP.shape) #Vetor ruído
noised_filtered_SP = filtered_SP + noise_SP # sinal original com ruído


pure_DT = np.linspace(-0.1, 0.1, len(filtered_DT)) # imagem da função densidade de probabilidade
noise_DT = np.random.normal(0, 30, pure_DT.shape) #Vetor ruído
noised_filtered_DT = filtered_DT + noise_DT # sinal original com ruído



# Poço c1
pure_RHOB1 = np.linspace(-0.1, 0.1, len(filtered_RHOB1)) # imagem da função densidade de probabilidade
noise_RHOB1 = np.random.normal(0, 0.1, pure_RHOB1.shape) #Vetor ruído
noised_filtered_RHOB1 = filtered_RHOB1 + noise_RHOB1 # sinal original com ruído


pure_GR1 = np.linspace(-0.1, 0.1, len(filtered_GR1)) #  imagem da função densidade de probabilidade
noise_GR1 = np.random.normal(0, 7, pure_GR1.shape) #Vetor ruído
noised_filtered_GR1 = filtered_GR1 + noise_GR1 # sinal original com ruído


pure_SP1 = np.linspace(-0.1, 0.1, len(filtered_SP1)) # imagem da função densidade de probabilidade
noise_SP1 = np.random.normal(0, 10, pure_SP1.shape) #Vetor ruído
noised_filtered_SP1 = filtered_SP1 + noise_SP1 # sinal original com ruído


pure_DT1 = np.linspace(-0.1, 0.1, len(filtered_DT1)) # imagem da função densidade de probabilidade
noise_DT1 = np.random.normal(0, 30, pure_DT1.shape) #Vetor ruído
noised_filtered_DT1 = filtered_DT1 + noise_DT1 # sinal original com ruído


# Poço c2
pure_RHOB2 = np.linspace(-0.1, 0.1, len(filtered_RHOB2)) # imagem da função densidade de probabilidade
noise_RHOB2 = np.random.normal(0, 0.1, pure_RHOB2.shape) #Vetor ruído
noised_filtered_RHOB2 = filtered_RHOB2 + noise_RHOB2 # sinal original com ruído


pure_GR2 = np.linspace(-0.1, 0.1, len(filtered_GR2)) #  imagem da função densidade de probabilidade
noise_GR2 = np.random.normal(0, 7, pure_GR2.shape) #Vetor ruído
noised_filtered_GR2 = filtered_GR2 + noise_GR2 # sinal original com ruído


pure_SP2 = np.linspace(-0.1, 0.1, len(filtered_SP2)) # imagem da função densidade de probabilidade
noise_SP2 = np.random.normal(0, 10, pure_SP2.shape) #Vetor ruído
noised_filtered_SP2 = filtered_SP2 + noise_SP2 # sinal original com ruído


pure_DT2 = np.linspace(-0.1, 0.1, len(filtered_DT2)) # imagem da função densidade de probabilidade
noise_DT2 = np.random.normal(0, 30, pure_DT2.shape) #Vetor ruído
noised_filtered_DT2 = filtered_DT2 + noise_DT2 # sinal original com ruído



# Poço c3
pure_RHOB3 = np.linspace(-0.1, 0.1, len(filtered_RHOB3)) # imagem da função densidade de probabilidade
noise_RHOB3 = np.random.normal(0, 0.1, pure_RHOB3.shape) #Vetor ruído
noised_filtered_RHOB3 = filtered_RHOB3 + noise_RHOB3 # sinal original com ruído


pure_GR3 = np.linspace(-0.1, 0.1, len(filtered_GR3)) #  imagem da função densidade de probabilidade
noise_GR3 = np.random.normal(0, 7, pure_GR3.shape) #Vetor ruído
noised_filtered_GR3 = filtered_GR3 + noise_GR3 # sinal original com ruído


pure_SP3 = np.linspace(-0.1, 0.1, len(filtered_SP3)) # imagem da função densidade de probabilidade
noise_SP3 = np.random.normal(0, 10, pure_SP3.shape) #Vetor ruído
noised_filtered_SP3 = filtered_SP3 + noise_SP3 # sinal original com ruído


pure_DT3 = np.linspace(-0.1, 0.1, len(filtered_DT3)) # imagem da função densidade de probabilidade
noise_DT3 = np.random.normal(0, 30, pure_DT3.shape) #Vetor ruído
noised_filtered_DT3 = filtered_DT3 + noise_DT3 # sinal original com ruído



######################### Visualiza e avalia a filtragem #############################


#plotando os dados não convolvidos:

fig, (ax1, ax2, ax3, ax4) = plt.subplots(nrows=1, ncols=4, sharey=False)

ax1.set_ylim(bottom=min(prof3), top=max(prof3))
ax1.set_title('RHOB')
ax1.plot(RHOB3, prof3, 'b')
ax1.set_xlim(1, 3)
ax1.invert_yaxis()
ax1.grid()


ax2.set_ylim(bottom=min(prof3), top=max(prof3))
ax2.set_title('GR')
ax2.plot(GR3, prof3, 'r') 
ax2.invert_yaxis()
ax2.grid()

ax3.set_ylim(bottom=min(prof3), top=max(prof3))
ax3.set_title('SP')
ax3.plot(SP3, prof3, 'k') 
ax3.invert_yaxis()
ax3.grid()

ax4.set_ylim(bottom=min(prof3), top=max(prof3))
ax4.set_title('DT')
ax4.plot(DT3, prof3, 'g') 
ax4.invert_yaxis()
ax4.grid()


fig.subplots_adjust(hspace=0.2,wspace=0.5)

plt.title('C3 antes', fontsize='xx-large', loc='right')
plt.draw()
plt.show()


#plotando os dados convolvidos:

fig, (ax1, ax2, ax3, ax4) = plt.subplots(nrows=1, ncols=4, sharey=False)

ax1.set_ylim(bottom=min(prof3), top=max(prof3))
ax1.set_title('RHOB')
ax1.plot(noised_filtered_RHOB3, prof3, 'b')
ax1.set_xlim(1, 3)
ax1.invert_yaxis()
ax1.grid()

ax2.set_ylim(bottom=min(prof3), top=max(prof3))
ax2.set_title('GR')
ax2.plot(noised_filtered_GR3, prof3, 'r') 
ax2.invert_yaxis()
ax2.grid()

ax3.set_ylim(bottom=min(prof3), top=max(prof3))
ax3.set_title('SP')
ax3.plot(noised_filtered_SP3, prof3, 'k') 
ax3.invert_yaxis()
ax3.grid()

ax4.set_ylim(bottom=min(prof3), top=max(prof3))
ax4.set_title('DT')
ax4.plot(noised_filtered_DT3, prof3, 'g') 
ax4.invert_yaxis()
ax4.grid()


fig.subplots_adjust(hspace=0.2,wspace=0.5)

plt.title('C3 depois', fontsize='xx-large', loc='right')
plt.draw()
plt.show()



############################################### SALVA ARQUIVO DE SAÍDA #############################################

#banco = open("../inputs/Sintetico/BD_convolvido.txt","w+")

#for i in range(len(prof)):
#    banco.write(("%f %f %f %f %f\r\n" % (prof[i],noised_filtered_RHOB[i],noised_filtered_GR[i], noised_filtered_SP[i], noised_filtered_DT[i])))
#banco.close()



# salvando via pandas a base de dados:
databank = {'Litologia': rock ,
            'codigo': cod,
            'prof': prof,
            'RHOB': noised_filtered_RHOB,
            'GR': noised_filtered_GR, 
            'SP': noised_filtered_SP, 
            'DT': noised_filtered_DT}

BD_convolved = pd.DataFrame(databank, columns = ['Litologia', 'codigo', 'prof', 'RHOB', 'GR','SP','DT'])

print(BD_convolved)

BD_convolved.to_csv("../inputs/Sintetico/BD_convolvido.txt", index=False, sep=' ')




#c1 = open("../inputs/Sintetico/C1_convolvido.txt","w+")

#for i in range(len(prof1)):
#    c1.write(("%f %f %f %f %f\r\n" % (prof1[i],noised_filtered_RHOB1[i],noised_filtered_GR1[i], noised_filtered_SP1[i], noised_filtered_DT1[i])))
#c1.close()





# salvando via pandas o poço c1:
c1 = {'Litologia': rock1,
      'codigo': cod1,
      'prof': prof1,
      'RHOB': noised_filtered_RHOB1,
      'GR': noised_filtered_GR1, 
      'SP': noised_filtered_SP1, 
      'DT': noised_filtered_DT1}

C1 = pd.DataFrame(c1, columns = ['Litologia', 'codigo', 'prof', 'RHOB', 'GR','SP','DT'])

#print(C1)

C1.to_csv("../inputs/Sintetico/C1_convolvido.txt", index=False, sep=' ')




#c2 = open("../inputs/Sintetico/C2_convolvido.txt","w+")

#for i in range(len(prof2)):
#    c2.write(("%f %f %f %f %f\r\n" % (prof2[i],noised_filtered_RHOB2[i],noised_filtered_GR2[i], noised_filtered_SP2[i], noised_filtered_DT2[i])))
#c2.close()

# salvando via pandas o poço c2:
c2 = {'Litologia': rock2,
      'codigo': cod2,
      'prof': prof2,
      'RHOB': noised_filtered_RHOB2,
      'GR': noised_filtered_GR2, 
      'SP': noised_filtered_SP2, 
      'DT': noised_filtered_DT2}

C2 = pd.DataFrame(c2, columns = ['Litologia', 'codigo', 'prof', 'RHOB', 'GR','SP','DT'])

#print(C2)

C2.to_csv("../inputs/Sintetico/C2_convolvido.txt", index=False, sep=' ')




#c3 = open("../inputs/Sintetico/C3_convolvido.txt","w+")

#for i in range(len(prof3)):
#    c3.write(("%f %f %f %f %f\r\n" % (prof3[i],noised_filtered_RHOB3[i],noised_filtered_GR3[i], noised_filtered_SP3[i], noised_filtered_DT3[i])))
#c3.close()


# salvando via pandas o poço c1:
c3 = {'Litologia': rock3,
      'codigo': cod3,
      'prof': prof3,
      'RHOB': noised_filtered_RHOB3,
      'GR': noised_filtered_GR3, 
      'SP': noised_filtered_SP3, 
      'DT': noised_filtered_DT3}

C3 = pd.DataFrame(c3, columns = ['Litologia', 'codigo', 'prof', 'RHOB', 'GR','SP','DT'])

#print(C3)

C3.to_csv("../inputs/Sintetico/C3_convolvido.txt", index=False, sep= ' ')
