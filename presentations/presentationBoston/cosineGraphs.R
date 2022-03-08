library(ggplot2)
library(ggforce)
library(ggthemes)


#say you start with two 2-dimensional vectors, with origin at (0,0)

a <- c(1,2)
b <- c(3,4)

#The dot product of vectors of the same length is a1b1 + a2b2 + ...
#it is a number

ab <- a %*% b

#in particular for length 2, $a \dot a$ is going to be the same as $a_1^2 + a_2^2$
a %*% a

1^2 + 2^2

#now think about a:

ggplot()+ geom_segment(aes(x = 0, y = 0, xend = a[1], yend = a[2]))+
  xlim(c(0,6))+ylim(c(0,6))+
  annotate(geom = "text", label  = "a = [1,2]", x = 1.1, y = 2.1)


#It's length is $\sqrt{a_1^2 + a_2^2}= \sqrt{a \dot a} = \sqrt{5}$
# \vert \vert v \vert \vert & = \sqrt(v \times v)

#normalized v? Scalar multiple by $\nicefrac{1}{\vert \vert v\vert \vert}

lengthA <- sqrt(a %*% a)

aNormalized <- a/lengthA
aNormalized
sqrt(aNormalized %*% aNormalized)

ggplot()+
  xlim(c(0,6))+ylim(c(0,6))+geom_segment(aes(x = 0, y = 0, xend = a[1], yend = a[2]))+
  annotate(geom = "text", label  = "a = [1,2]", x = 1.1, y = 2.1)+
  geom_segment(aes(x = 0, y = 0, xend = aNormalized[1], yend = aNormalized[2]), color = "grey")+ 
  annotate(geom = "text", label  = "a normalized = [.447,.894]", x = 1.3, y = .994)


#distance between vectors is the length of the difference
#$d(u,v) = \vert \vert u - v \vert \vert$
 
ggplot()+
  xlim(c(0,6))+ylim(c(0,6))+
  geom_segment(aes(x = 0, y = 0, xend = a[1], yend = a[2]),  arrow = arrow(length = unit(0.02, "npc")))+
  annotate(geom = "text", label  = "a = [1,2]", x = 1.1, y = 2.1)+
  geom_segment(aes(x = 0, y = 0, xend = b[1], yend = b[2]), arrow = arrow(length = unit(0.02, "npc")))+ 
  annotate(geom = "text", label  = "b = [4,5]", x = 4.1, y = 5.1)+theme_tufte(base_size = 7)


a <- c(1,2)
b <- c(4,5)

bma <- b-a


ggplot()+
  xlim(c(0,6))+ylim(c(0,6))+
  geom_segment(aes(x = 0, y = 0, xend = a[1], yend = a[2]),  arrow = arrow(length = unit(0.02, "npc")))+
  annotate(geom = "text", label  = "a = [1,2]", x = 1.1, y = 2.1)+
  geom_segment(aes(x = 0, y = 0, xend = b[1], yend = b[2]), arrow = arrow(length = unit(0.02, "npc")))+ 
  annotate(geom = "text", label  = "b = [4,5]", x = 4.1, y = 5.1)+
  geom_segment(aes(x = a[1], y = a[2], xend = b[1], yend = b[2]),
               arrow = arrow(length = unit(0.02, "npc")), color = "grey")+ 
  annotate(geom = "text", label  = "b-a = [3,3] (shifted)", x = 2.1, y = 3.8)+
  theme_tufte(base_size = 7)+
  annotate(geom = "text", label  = expression(theta), x = .5, y = .78)+
  geom_arc(aes(x0 = 0, y0 = 0, r = 1.3, 
               start = .463, end = .876))

#atan2(a[1] , a[2])
#atan2(b[1] , b[2])

\vert \vert b - a \vert \vert ^2 = \vert \vert b \vert \vert 2 + \vert \vert a \vert \vert ^2 - 
  2 \vert \vert b \vert \vert  \vert \vert a \vert \vert \mathsf{cos} \theta  \\
b \dot a = \vert \vert b\vert \vert \vert \vert a \vert \vert \cos \theta 
cos \theta & = \frac{b \dot a}{\vert \vert b\vert \vert \vert \vert a \vert \vert}

#now orthogonality: 
\math{cos} (\theta) = 90$^{\circ}$ = 0, so you want 
$\frac{b \dot a}{\vert \vert b\vert \vert \vert \vert a \vert \vert} = 0$ 
which happens only if $b \dot a= 0$.  





start <- c(x = 0, y = 0)






dat <- data.frame(
  x = start[c("x", "x")],
  y = start[c("y", "y")],
  xend = c(1, 4),
  yend = c(5, 1)
)

angles <- with(dat, atan2(xend - x, yend - y))

ggplot(dat) +
  geom_segment(aes(x, y, xend = xend, yend = yend)) +
  geom_arc(aes(x0 = start["x"], y0 = start["y"], r = 1, 
               start = angles[1], end = angles[2])) +
  coord_equal()