import math
import numpy as np
import pandas as pd 
from matplotlib import pyplot as plt
data=pd.read_csv("resultados.txt",delim_whitespace=True)
df=pd.DataFrame(data)
x=data.iloc[:,0]
y=data.iloc[:,1]
plt.plot(x,y,"-") 
plt.plot(x,y,"go")
plt.ylabel("Tiempo")
plt.title("Problema 4 inciso C")
plt.xlabel("Puntos resultandez de RK4")
plt.savefig("Resultados 4C.png")