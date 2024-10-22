# -*- coding: utf-8 -*-
"""


@author: Alexandre & Victor
"""
import string
import numpy as np
import random as rd
import os
os. chdir("C://Users//jmv//Documents//Travail//Master//r//projet_SC")

# On ouvre un texte de base (.txt)
text=open('texte1.txt',encoding='utf-8') 
text_trie=""
for ligne in text:                            # On récupére l'information et on la stocke dans une seul chaîne de caractère
    text_trie=text_trie+str(ligne)
    
#text_2=open('texte2.txt',encoding='utf-8')
#text_trie2="" 
#for ligne in text_2:                            # On récupére l'information et on la stocke dans une seul chaîne de caractère
#    text_trie2=text_trie2+str(ligne)
    
d='ula gadilba qddgitpfqrpsa q cq nilla xuabrpil q naqueiud dcub oa sqcaug xu ula gadilba dgaepba q cq fqusqpba xuabrpil'

def codage(x,code):  # la fonction codage
    x1=list(x)
    
    x2=""
    for i in range (len(x1)):
        for j in range(26):
            if  x1[i]==code[j,0] :
                x1[i]=code[j,1]
                break
    for i in range(len(x1)):
        x2+=x1[i]
    return x2
def decodage(x,code):              # la fonction decodage prend en entré un texte et un code ( tableau de taille 26 x 2)
    x1=list(x)
    
    x2=""                          ## La fonction renvoi une chaîne de caractère décodé par le code d'entré
    for i in range (len(x1)):
        for j in range(26):
            if  x1[i]==code[j,1] :
                x1[i]=code[j,0]
                break
    for i in range(len(x1)):
        x2+=x1[i]
    return x2              

def Mrpropre(text):            # Cette fonction sert à "nettoyer" le texte pour ensuite pouvoir créer la matrice de transition
    text=text.lower()          # on enleve les majuscules
    accent = ['é', 'è', 'ê', 'ë','ä', 'à', 'ù','ü', 'û', 'ç', 'ô', 'î', 'ï', 'â','-',"'",'1','2','3','4','5','6','7','8','9','0','«','»']             
    sans_accent = ['e', 'e', 'e','e', 'a','a','u', 'u', 'u', 'c', 'o', 'i', 'i', 'a','é',' ','','','','','','','','','','','','']
    i = 0
    while i < len(accent):   # on remplace les accents par des caractères sans accents et on régle le problème des -
        text = text.replace(accent[i], sans_accent[i])
        i += 1
    text = text.translate(str.maketrans("","", string.punctuation))   # Pour retirer la ponctuation
    return text
alphabet=['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z',' ']
def prob_transi(text): 
    caractere=['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z',' ']  # C'est l'ensemble des terme de base que lon considère pour les calculs de probabilités
    n=len(caractere)
    proba=np.zeros((n,n))
    Nbr=n*[0]
    Nbr[0]+=1
    x=-1
    for i in text:  # On boucle sur l'ensemble des lettres du texte
        s=0
        while s<len(caractere) and i != caractere[s]:
            s+=1
        if s!=27:  # Si la boucle tombe sur un élément du texte qui n'est pas dans caractère on passe à l'élément i+1
            proba[x,s]+=1
            Nbr[s]+=1
            x=s
    for i in range(n):
        for j in range(n):
            proba[i,j]=proba[i,j]/len(text)
    return proba
sum_a=0
a=prob_transi(Mrpropre(text_trie))
#a2=prob_transi(Mrpropre(text_trie2))
# for i in range(27):
#     for j in range(27):
#         sum_a+= a[i,j]            
#print("la somme des elt de A vaut:",sum_a)  ## On vérifie que la somme des éléments de a vaut à peu prés 1

def dlettresfr(x):  # la matrice de transition noter a n'est pas un argument de la fonction, cette variable doit exister et être globale
    x=list(x)
    i=0
    while i<26 and x[0] != code[i,0]:
        i+=1
    if i==27:
        print('les caractère ne sont pas des lettres ( pensez à retirer les accents')
    else:
        j=0
        while j<26 and x[1] != code[j,0]:
            j+=1
        if j==27:
            print('les caractère ne sont pas des lettres ( pensez à retirer les accents')
        elif x[1]==' ':
            return a[i,26]
        elif x[0]==' ':
            return float(a[26,j])
        else:
            return float(a[i,j])

#Q6:  
code=np.array([['a','q'],['b','b'],['c','c'],['d','d'],['e','a'],['f','f'],['g','g'],['h','h'],['i','i'],['j','j'],['k','k'],['l','e'],['m','m'],['n','l'],['o','o'],['p','p'],['q','x'],['r','r'],['s','s'],['t','t'],['u','u'],['v','v'],['w','w'],['x','n'],['y','y'],['z','z']])

candidat=['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z',' ']

#Q7
def prop(x):
    # choix indice au hasard
    indice1=rd.randint(0,25)
    indice2=rd.randint(0,25)
    
    while indice1==indice2:
        indice1=rd.randint(0,25)
        
    #On inverse les lettres:
    copy=x.copy()
    temp=copy[indice1,1]
    copy[indice1,1]=copy[indice2,1]
    copy[indice2,1]=temp
    return copy


def Metropolis_H2(texte_code1,c,nbr):
    taille= np.trunc(nbr/500) # On prepare la boucle d'affichage
    taille=int(taille)
    texte_code1=Mrpropre(texte_code1) # On nettoie le texte à décoder 
    texte_code=list(texte_code1)
    x=c
    stop=0
    probx=10**100 # Les probabilité étant très faible,, on initialise une probabilité à 10*100 pour éviter d'arrondir a probabilité calculer à 0
    decode=decodage(texte_code,x)
    decode=list(decode)
    for i in range(26): # On initialise la proba de X0
            if decode[0]==x[i,0]:
                probx=probx*(a[-1,i]) # proba de la première lettre
            # elif decode[-1]==x[i,0]:
            #     probx=probx*(a[i,-1]) # proba de la dernière lettre en fin de mot

    for i in range(len(decode)-1):
        probx=probx*dlettresfr(decode[i]+decode[i+1])

    while probx==0 and stop<100: ## Pour démarrer avec une probabilité non nulle on créé une boucle while.
        probx=10**100
        
        stop+=1
        x=prop(c)
        decode=decodage(texte_code,x)
        for i in range(26): # On initialise la proba de X0
            if decode[0]==x[i,0]:
                probx=probx*(a[-1,i])

        for i in range(len(texte_code)-1):
            probx=probx*dlettresfr(decode[i]+decode[i+1])

    Xn=(taille+1)*[x]
    for i in range(nbr): # debut de la boucle itérative

        if probx==0:
            print("Proba initial nulle")
            break

        c1=prop(x)
        decode=decodage(texte_code,c1)
        decode=list(decode)
        proby=10**100
        for j in range(26): # On initialise la proba de Yi
            if decode[0]==c1[j,0]:
                proby=proby*a[-1,j]

        for j in range(len(texte_code)-1):
            proby=proby*dlettresfr(decode[j]+decode[j+1])
            
        alpha=1                                #On calcul alpha
        if proby/probx<1:
            alpha=proby/probx

        if rd.random()<=alpha:                # on simule une uniforme pour accepte ou non le nouveau candidat   
            probx=proby
            x=c1
        if i%500 ==0:                         # Boucle pour afficher les résultats tous les 500 itérations
            Xn[int(i/500)]=x
            print(i/500, decodage(d,Xn[int(i/500)]))
            print('-------------------')      # Pour la lisibilité
    print('proba finale:',probx)
    Xn[-1]=x
    return Xn                                 # Le code renvoi la chaîne de Markov partiel


d='ula gadilba qddgitpfqrpsa q cq nilla xuabrpil q naqueiud dcub oa sqcaug xu ula gadilba dgaepba q cq fqusqpba xuabrpil'

code_fin=Metropolis_H2(d,code,10000)

probx=10**100
decode='une reponse approximative a la bonne question a beaucoup plus de valeur qu une reponse precise a la mauvaise question'

## On calcul ici la probabilité de notre phrase décodé
for i in range(26): 
    if decode[0]==code[i,0]:
        probx=probx*(a[-1,i])
for i in range(len(decode)-1):
    probx=probx*dlettresfr(decode[i]+decode[i+1])
print('proba vrai phrase', probx )
text.close()