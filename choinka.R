library(plotrix)
library(png)
library(fields)
thick=2
thin=1
dens=20
#Sys.setlocale( "LC_ALL", "english")
QR=readPNG("~/test.png")
QR = QR[,,1]
QR = t(QR[nrow(QR):1,])

screw = function(b,c,a=1, tr_a=pi/6, tr_b=tr_a) {

	mr=sqrt(3)/2*a
	tr1 = a-mr
	tr2 = abs(tr1)*tan(tr_a)
	tr3 = abs(tr1)*tan(tr_b)
	segments(a-tr1,c,a,c-tr2)
	segments(a-tr1,b,a,b+tr3)
	segments(a/2,b+tr3,a/2,c-tr2)
	segments(a,b+tr3,a,c-tr2)
	segments(0,b,mr,b)
	segments(0,c,mr,c)
	x = seq(0,a/2,len=100)
	y = (abs(mr)-sqrt(x^2+mr^2))*tr2/abs(tr1)
	lines(3/4*a+x/2,c + y)
	lines(3/4*a-x/2,c + y)
	lines(x,c + y)
	y = -(abs(mr)-sqrt(x^2+mr^2))*tr3/abs(tr1)
	lines(3/4*a+x/2,b + y)
	lines(3/4*a-x/2,b + y)
	lines(x,b + y)

	list(
		p = t(matrix(c(
		0,b,
		-a+tr1,b,
		-a,b+tr3,
		-a,c-tr2,
		-a+tr1,c,
		0,c),nrow=2))
	)
}

win=20
for (form in c("pdf","png")) {
	if (form == "pdf") {
		#cairo_pdf("choinka.pdf",width=win,height=win)
    cairo_pdf("choinka.pdf", width=8.267, height=11.692, pointsize=8)
    he = 17
    par(lwd=1,mar=c(4,4,4,4)+2)
	} else if (form=="png") {
		
		png("choinka.png",width=600,height=600,res=50)
    he=8
		par(lwd=1,mar=c(4,4,4,4))
	}
	#par(lwd=1,mar=c(0,0,0,0))

h = 0.7*16
w = 0.9*16
h1 = 0.2*16
l=-2*16
u=he*16
u2=u-3*h
lr = w*0.55
g = 0.1*16
z=1*16

h = 12
w = 15
h1 = 3
l=-32
u=he*16
u2=u-3*h
lr = 8
g = 0.1*16
z=1*16





plot(
  NA,
  xlim=c(140,-140),
  ylim=c(l,u),
  asp=1,
  xlab="Konkurs dla zainteresowanych studentów. Wynik konkursu nie wpływa na ocene z Informatyki I, czy C++. Organizator: Łukasz Łaniewski-Wołłk",
  ylab="",
  main="KONKURS Z NAGRODAMI", cex.main=4)
grid()
#plot(NA, xlim=c(-4,4), ylim=c(-4,4))
#plot(NA, xlim=c(-4,4), ylim=c(-4,4)+15)
par(lwd=thick)

tr_a=pi/6
tr1=2*g
d = seq(0,u2,h+h1)

tr2=tr1*tan(tr_a)
ret = screw(l-h,l,w,tr_b=tr_a,tr_a=0)
p = ret$p
p[nrow(p),1]=-lr
#	p = rbind(p, t(matrix(c(
#		-lr,s-h/2,
#		-lr,u-tr2,
#		-lr+tr1,u,
#		0,u),2)))
#lines(p)
ret = screw(l-h,l,-w,tr_b=tr_a,tr_a=0)

for (s in d) {
	ret = screw(s-h/2,s+h/2,w,tr_a=tr_a)
	hg=g/2
	ret$p[c(1,nrow(ret$p)),1] = -lr
	lines(ret$p)
	ret$p[c(1,nrow(ret$p)),1] = -lr-g
	ret$p = rbind(c(-lr,s-h/2+hg),ret$p,c(-lr,s+h/2-hg))
	lines(ret$p)
	polygon(ret$p,density=dens,border=NA, lwd=thin, angle=-45)
	lines(t(matrix(c(
		-lr,s-h/2+hg,
		-lr+g,s-h/2+2*hg,
		-lr+g,s+h/2-2*hg,
		-lr,s+h/2-hg),2)),lwd=thin)
}

for (s in d[-1]) {
	r = (u-s)*0.5+lr
	p = t(matrix(c(
		0,s-h/2,
		r,s-h/2,
		r,s-h/2-h1,
		0,s-h/2-h1),2))
	lines(p)
	p[2:3,1]=-r
	p[c(1,4),1]=-lr
	lines(p)
	p[c(1,4),1]=-lr-g*1.5
	lines(p[c(4,1),])
#	lines(p)
	polygon(p,density=dens,border=NA, lwd=thin, angle=45)
}

segments(0,l-3*h,0,u+3*h,lty="18F8",lwd=thin)

segments(-lr,l+z,lr,l+z)
segments(-lr,l,-lr,u-tr2)
segments(-lr,u-tr2,-lr+tr1,u)
segments(-lr+tr1,u,+lr-tr1,u)
segments(lr,u-tr2,lr-tr1,u)
segments(-lr,u-tr2,lr,u-tr2)
segments(-lr+g,l+z,-lr+g,u-tr2/tr1*(tr1-g),lwd=thin,lty=2)
segments(-lr+g,l+z,-lr,l+z-3*g,lwd=thin,lty=2)
s = min(d-h/2)
segments(lr,l,lr,s)
segments(lr-g,l+z,lr-g,s,lwd=thin,lty=2)
segments(lr-g,l+z,lr,l+z-3*g,lwd=thin,lty=2)
s = max(d+h/2)
segments(lr,s,lr,u-tr2)
segments(lr-g,s,lr-g,u-tr2/tr1*(tr1-g),lwd=thin,lty=2)
par(lwd=thin)
legend("topleft", density=c(dens,dens), angle=c(45,-45), legend=c(
	"Wesołych",
	"Świąt"),cex=2)
legend("bottomright",
	lwd=c(thick,thin,thin,thin),
	lty=c(1,1,2,4),
	legend=c(
		"Konkurs na choinkę, ...",
		"          napisaną w C/C++.",
		"więcej na c-cfd.meil.pw.edu.pl"
	),cex=2)
image(75+(1:nrow(QR))*1.5, -90+(1:ncol(QR))*1.5, z=QR[nrow(QR):1,], col=c(1,0), add=TRUE)
dev.off()
}

