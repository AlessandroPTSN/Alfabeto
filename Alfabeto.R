library(stringr)
library(ggplot2)
library(gridExtra)

#lendo banco de dados
br<- read.table("C:/Users/Neo/Desktop/br-utf8.txt", encoding="UTF-8", quote="\"", comment.char="")
head(br)
nrow(br)


#removendo palavras como \xc3 �
br2 = br[-261529,1]
br2 = br2[-261529]
br2 = br2[-261529]
br2 = br2[-261529]
br2 = br2[-261529]
br2 = br2[-261529]
br2 = br2[-261529]
br2 = br2[-261529]
br2 = br2[-261529]
br2 = br2[-261529]
br2 = br2[-261529]
br2 = br2[-261529]
#pegando apenas palavras de 5 letras
br2 = tolower(br2[nchar(br2)==5])
br2 = iconv(br2,from="UTF-8",to="ASCII//TRANSLIT")


#fazendo banco de dados de aparicao por lugar na palavra
df=data.frame(
pri=c(rep(0,26)),
seg=c(rep(0,26)),
ter=c(rep(0,26)),
qua=c(rep(0,26)),
qui=c(rep(0,26)),
letra=letters)
for(j in 1:26){
for(i in 1:5){
f=length(substr(br2, i, i)[substr(br2, i, i)==letters[j]])
df[j,i] = f 
}
}
df


#fazendo banco de dados de total de aparicao
df2=data.frame(
  total=c(rep(0,26)),
  letra=letters)
for(i in 1:26){
  df2[i,1]=sum(df[i,1:5])
}
df2=df2[order(df2$total),c(1,2)]
df2



par(mfrow=c(5,5))

for(i in 26:1){
  
#arrumando dados
  z = reshape2::melt(df[df$letra==df2[i,2],],id="letra")
  
#arrumando dados  
  zz=data.frame(
    name=as.factor(1:5) ,  
    value=as.numeric(z[,3])
  )
  
  
#fazendo gráfico  
ggplot(zz, aes(x=as.factor(name), y=value, fill = value)) + 
    geom_bar( stat = "identity") +
    scale_fill_gradient(low="blue",high="red")+ 
    labs(title = paste(toupper(df2[i,2])),subtitle = paste(df2[i,1],"Ocorrencias de letra nas palavras"),
         caption = "Source: https://www.ime.usp.br/~pf/dicios/index.html")+ 
    labs(y = "Contagem", x = "Local nas palavras")

#salvando gráfico  
png(file = gsub(" ", "", paste("C:/Users/Neo/Desktop/alfabeto/",df2[i,2],".png"), fixed = TRUE), width = 1200, height = 1200, res = 300)
grid.arrange(p)
dev.off()


}
















#Fazendo um de cada vez todos juntos
df3=df2[order(df2$total, decreasing = TRUE),c(1,2)]
df3



i=25 #mude o i

#arrumando dados
z = reshape2::melt(df[df$letra==df3[i,2],],id="letra")

#arrumando dados  
zz=data.frame(
  name=as.factor(1:5) ,  
  value=as.numeric(z[,3])
)


#fazendo gráfico  
p25 = #mude o pi
  ggplot(zz, aes(x=as.factor(name), y=value, fill = value)) + 
  geom_bar( stat = "identity") +
  scale_fill_gradient(low="blue",high="red")+ 
  labs(title = paste(toupper(df3[i,2])),subtitle = paste(df3[i,1],"Ocorrencias de letra nas palavras"),
       caption = "Source: https://www.ime.usp.br/~pf/dicios/index.html")+ 
  labs(y = "Contagem", x = "Local nas palavras")





grid.arrange(p1,p2,p3,p4,p5,
             p6,p7,p8,p9,p10,
             p11,p12,p13,p14,p15,
             p16,p17,p18,p19,p20,
             p21,p22,p23,p24,p25,
             nrow = 5, ncol = 5)


#Letra mais usada como primeira
head(df[order(df$pri, decreasing = T),6],5)
#Letra mais usada como segunda
head(df[order(df$seg, decreasing = T),6],5)
#Letra mais usada como terceira
head(df[order(df$ter, decreasing = T),6],5)
#Letra mais usada como quarta
head(df[order(df$qua, decreasing = T),6],5)
#Letra mais usada como quinta
head(df[order(df$qui, decreasing = T),6],5)
