library(MASS)
library(ggplot2)
library(psych)
####         PART 1         ####

##   1.  ##

mu <- c(2, 3, 5, 8)

R <- matrix(c(
  1,   0.2, 0.4, 0.8,
  0.2, 1,   0.5, 0.6,
  0.4, 0.5, 1,   0.7,
  0.8, 0.6, 0.7, 1
), nrow=4, byrow=TRUE)

 ## i) ##

var1=rep(sqrt(2),4)
D1=diag(var1)
Sigma1=D1%*%R%*%D1

## Correlation Y ##

cy1=rbind(c(1,-1,0,-1),c(0,0,1,0))
Sy1=cy1%*%Sigma1%*%t(cy1)
Vy1=sqrt(diag(Sy1))
Ry1=Sy1/(Vy1%*%t(Vy1))


## Correlation Z ##

cz1=rbind(c(-1,0,0,0),c(0,-1,0,1))
Sz1=cz1%*%Sigma1%*%t(cz1)
Vz1=sqrt(diag(Sz1))
Rz1=Sz1/(Vz1%*%t(Vz1))

## Correlation Y1 & Z2 ##

cyz1=rbind(c(1,-1,0,-1),c(0,-1,0,1))
Syz1=cyz1%*%Sigma1%*%t(cyz1)
Vyz1=sqrt(diag(Syz1))
Ryz1=Syz1/(Vyz1%*%t(Vyz1))

## ii) ##

var2=sqrt(c(2,3,4,5))
D2=diag(var2)
Sigma2=D2%*%R%*%D2

## Correlation Y ##

cy2=rbind(c(1,-1,0,-1),c(0,0,1,0))
Sy2=cy2%*%Sigma2%*%t(cy2)
Vy2=sqrt(diag(Sy2))
Ry2=Sy2/(Vy2%*%t(Vy2))


## Correlation Z ##

cz2=rbind(c(-1,0,0,0),c(0,-1,0,1))
Sz2=cz2%*%Sigma2%*%t(cz2)
Vz2=sqrt(diag(Sz2))
Rz2=Sz2/(Vz2%*%t(Vz2))

## Correlation Y1 & Z2 ##

cyz2=rbind(c(1,-1,0,-1),c(0,-1,0,1))
Syz2=cyz2%*%Sigma2%*%t(cyz2)
Vyz2=sqrt(diag(Syz2))
Ryz2=Syz2/(Vyz2%*%t(Vyz2))



##  2.  ##

## Setting Parameters for multivariate Normal ##

n=10000
Sigma=Sigma2
mu

## Setting seed for reproduction purposes ##

set.seed(2025)

## Taking sample of 10000 multivariate Normals ##

X=mvrnorm(n, mu, Sigma)

## Calculating sample means and covariances of X ##

X_bar=apply(X,2,mean)
X_cov=cov(X)

## Computing y1,y2,z1,z2 and creating w ## 

y1=X[,1]-X[,2]-X[,4]
y2=X[,3]
z1=-X[,1]
z2=X[,4]-X[,2]
w=cbind(y1,y2,z1,z2)

## Calculating sample means, covariances and correlation of w ##

w_bar=apply(w,2,mean)
w_cov=cov(w)
Varw=sqrt(diag(w_cov))
Rw=w_cov/(Varw%*%t(Varw))



##----------------------------------------------------------------------------##

####         PART 2         ####
diet=read.csv("dietStudy.csv")
deaths=diet$death
diet=diet[,-7]
dcov=cov(diet)


##   1.  ##

round(cor(diet),2)
pairs(diet)
KMO(diet)

##   2.  ##

## a) ##

eigd <- eigen(dcov)

## b) ##

eigd$cp = eigd$values/sum(eigd$values)
eigd$vectors=-(eigd$vectors)
d_eig=data.frame(it = 1:ncol(diet),values=eigd$values,cp=eigd$cp,vectors=eigd$vectors)
d_eig$ceig <- cumsum(d_eig$cp)

## d) ## 
d_eig$ceig=d_eig$ceig*100
head(d_eig[d_eig$ceig>80,],1)
d_eig[d_eig$values>1,]
k=length(d_eig$values)

ggplot(d_eig, aes(x = it, y = values)) +
  geom_line(color = "#2C7FB8", linewidth = 1.2) + # Γραμμή
  geom_point(color = "#2C7FB8", size = 4, shape = 19) + # Σημεία
  labs(
    title = "Scree Plot: Ιδιοτιμές ανά Συνιστώσα",
    x = "Αριθμός Συνιστώσας (Component Number)",
    y = "Ιδιοτιμή (Eigenvalue)"
  ) +
  # Προσθήκη οριζόντιας γραμμής για το Κριτήριο Kaiser
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 0.8) +
  annotate("text", x = max(d_eig$it), y = 1.05, label = "Κριτήριο Kaiser (Eigenvalue = 1)",
           color = "red", hjust = 1, vjust = 0, size = 3.5) +
  # Προσθήκη ετικετών με τις ακριβείς τιμές των ιδιοτιμών (πάνω από τα σημεία)
  geom_text(aes(label = round(values, 2)), # Ετικέτα: στρογγυλοποιημένη τιμή ιδιοτιμής
            vjust = -1.2, # Μετακίνηση ετικέτας πάνω από το σημείο
            hjust = 0.5,
            size = 3.5,
            color = "darkgreen") +
  # ΝΕΟ: Προσθήκη ετικετών με την αθροιστική διακύμανση (κάτω από τα σημεία)
  geom_text(aes(label = paste0(round(ceig, 1), "%")), # Ετικέτα: στρογγυλοποιημένο ceig με "%"
            vjust = 2.2, # Μετακίνηση ετικέτας κάτω από το σημείο (μεγαλύτερη τιμή vjust = πιο κάτω)
            hjust = 0.5,
            size = 3.5,
            color = "purple") + # Χρησιμοποιούμε διαφορετικό χρώμα για διαφοροποίηση
  # Εξασφάλιση ότι ο x-άξονας δείχνει ακέραιους αριθμούς
  scale_x_continuous(breaks = 1:k) +
  # Επιλογή ενός καθαρού, μινιμαλιστικού θέματος
  theme_minimal() +
  # Προσαρμογή του θέματος για καλύτερη εμφάνιση
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), # Κεντράρισμα και έντονη γραμματοσειρά τίτλου
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major = element_line(color = "gray90", linetype = "dotted"), # Κύριες γραμμές πλέγματος
    panel.grid.minor = element_blank() # Αφαίρεση δευτερευόντων γραμμών πλέγματος
  )

## e ##



ggplot(d_eig, aes(x = vectors.1, y = vectors.2)) +
  geom_point(color = "steelblue", size = 2) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +  # y = 0 line
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +  # x = 0 line
  coord_fixed(xlim = c(-1, 1), ylim = c(-1, 1)) +  # fixed ratio and limits
  labs(title = "Eigenvector Biplot",
       x = "Eigenvector 1",
       y = "Eigenvector 2") +
  theme_minimal()



## f ##

diet[,c("Comp1","Comp2")] <- as.matrix(diet) %*% (eigd$vectors[,1:2])
diet$death=deaths
glm_log=glm(death~Comp1+Comp2,family = binomial(link="logit"),data = diet)
summary(glm_log)

