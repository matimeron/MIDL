Pro Fbeamline1, bor = bor

	on_error, 1

	cang = 5e-4
	off = 0.7

	inb = [1,-cang]
	oub = [1,cang]
	vof = [0,off]
	mof01 = [0,1]##vof
	mof11 = [1,1]##vof

	window, 0, xsiz=1280, ysi=384
	Box, [24,54], [-2,8], truasp='ylo', bor=bor
	axis, 24, -2, 0, /xaxis, xstyle=1, charsize=1.5
	axis, 24, 0, 0, yaxis=0, ystyle=1, yminor=2, charsize = 1.5
	plots, [24,28]##inb, thi=2, col = !pcol.purple
	plots, [24,54]##oub, thi=2, col = !pcol.red, line=1
	plots, [24,52.7]##oub, thi=2, col = !pcol.red

	plots,[28,28.7]##inb + mof01, thi=2, col=!pcol.purple, line=2
	plots,[28,33.3]##inb + mof01, thi=2, col=!pcol.purple, line=2
	plots,[33.3,51.2]##inb + mof11, thi=2, col=!pcol.purple, line=2
	plots,[33.3,46.2]##inb + mof11, thi=2, col=!pcol.purple

	rray = [[0,0],[5,0]]
	plots, Shape_trans(rray,15,1,39.2*inb+vof,/deg),$
		thi=2,col=!pcol.purple, line=2
	plots, Shape_trans(rray,30,1,39.2*inb+vof,/deg),$
		thi=2,col=!pcol.purple, line=2	
	plots, Shape_trans(rray,45,1,39.2*inb+vof,/deg),$
		thi=2,col=!pcol.purple, line=2
	plots, Shape_trans(rray,15,1,46.2*inb+vof,/deg),$
		thi=2,col=!pcol.purple, line=2
	plots, Shape_trans(rray,30,1,46.2*inb+vof,/deg),$
		thi=2,col=!pcol.purple, line=2
	plots, Shape_trans(rray,45,1,46.2*inb+vof,/deg),$
		thi=2,col=!pcol.purple, line=2

	foe = Shape_close([[24.5,-0.2],[27.8,-0.55],[31.3,-0.55],[36,-0.95],$
		[36,-0.4],[37.5,-0.4],[37.5,1.8],$
		[36,1.8],[26.5,1.8],[26.5,0.85],[24.5,0.85]])
	plots, foe, thi = 2, col = !pcol.dblue
	plots, [[27.5,1.8],[30.5,1.8]], thi=2, col=!pcol.white
	plots, [[26.6,1.9],[28.6,1.9]], thi=2, col=!pcol.green
	plots, [[30.5,1.9],[31.5,1.9]], thi=2, col=!pcol.green
	plots, [[33.5,1.8],[34.5,1.8]], thi=2, col=!pcol.white
	plots, [[34.5,1.9],[35.5,1.9]], thi=2, col=!pcol.green

	soe = Shape_close([[38.7,-0.85],[47.7,-0.85],[52.7,-1.25],[52.7,0.2],$
		[45.7,0.2],[45.7,0.45],[40.2,0.45],[40.2,0.2],[38.7,0.2]])
	plots, soe, thi=2,col=!pcol.dblue
	plots, [[42.5,-0.85],[45.5,-0.85]], thi = 2, col=!pcol.white
	plots, [[40.5,-0.95],[42.5,-0.95]], thi=2, col=!pcol.green
	plots, [[45.5,-0.95],[46.5,-0.95]], thi=2, col=!pcol.green

	idb = [[52.7,0.2],[52.7,5.5],[45.7,5.5],$
		[45.7,0.2],[45.7,5.5],[38.7,5.5],[38.7,0.2]]
	plots, idb, thi=2, col=!pcol.dblue
	plots, [[41.2,5.5],[43.2,5.5]], thi=2, col = !pcol.white
	plots, [[40.2,5.6],[41.2,5.6]], thi=2, col = !pcol.green
	plots, [[43.2,5.6],[44.2,5.6]], thi=2, col=!pcol.green
	plots, [[48.2,5.5],[50.2,5.5]], thi=2, col = !pcol.white
	plots, [[47.2,5.6],[48.2,5.6]], thi=2, col = !pcol.green
	plots, [[50.2,5.6],[51.2,5.6]], thi=2, col=!pcol.green

	plots, [[52.7,-1.25],[54,-1.4]], thi=2,col=!pcol.dblue, line=2
	plots, [[52.7,3.2],[54,3.2]], thi=2,col=!pcol.dblue, line=2

	Rectan, xli=0.5*[-1,1], yli=0.4*[-1,1], /no_show, shape=mono
	Rectan, xli=1.5*[-1,1], yli=0.32*[-1,1], /no_show, shape=mirror
	Circle_mm, cent=[0,0],rad=0.2,/no_show, shape=scr
	Rectan, xli=2.9*[-1,1], yli= 0.32*[-1,1], /no_show, shape=sbm
	Rectan, xli=0.5*[-1,1], yli=0.25*[-1,1], /no_show, shape=shutter
	Rectan, xli=0.45*[-1,1], yli=0.2*[-1,1], /no_show, shape=crl

	mono = Shape_trans(mono,0,1,34.3*oub-[0,0.05])
	mirror = Shape_trans(mirror,0,1,42*oub)
	scr1 = Shape_trans(scr,0,1,28*inb)
	scr2 = Shape_trans(scr,0,1,39.2*inb+vof)
	scr3 = Shape_trans(scr,0,1,46.2*inb+vof)
	sbm = Shape_trans(sbm,0,1,30.9*inb+vof)
	shutter1 = Shape_trans(shutter,0,1,36.8*oub)
	shutter2 = Shape_trans(shutter,0,1,36.8*inb+vof)
	crl1 = Shape_trans(crl,0,1,45*oub)
	crl2 = Shape_trans(crl,0,1,35.4*inb+vof)

	polyfill, mono, col = !pcol.lblue
	plots, mono, thi=2, col=!pcol.blue
	polyfill, mirror, col = !pcol.lblue
	plots, mirror, thi=2, col=!pcol.blue
	polyfill, scr1, col = !pcol.lblue
	plots, scr1, thi=2, col=!pcol.blue
	polyfill, scr2, col = !pcol.lblue
	plots, scr2, thi=2, col=!pcol.blue
	polyfill, scr3, col = !pcol.lblue
	plots, scr3, thi=2, col=!pcol.blue
	polyfill, sbm, col = !pcol.lblue
	plots, sbm, thi=2, col=!pcol.blue
	polyfill, shutter1, col = !pcol.lblue
	plots, shutter1, thi=2, col=!pcol.blue
	polyfill, shutter2, col = !pcol.lblue
	plots, shutter2, thi=2, col=!pcol.blue
	polyfill, crl1, col = !pcol.lblue
	plots, crl1, thi=2, col=!pcol.blue
	polyfill, crl2, col = !pcol.lblue
	plots, crl2, thi=2, col=!pcol.blue

	Labels, [34.3,42,36.8,45],[-1.3,-0.7,-0.9,-0.7],align=0.5,$
		['Mono','Mirrors','Shut.','CRL'],charsize=1.6,col=!pcol.pink
	Labels, [30.9,36.8,35.4],[1.2,1.2,1.2],align=0.5,$
		['Sidebounce Mono','Shut.','CRL'],charsize=1.6,col=!pcol.pink

	return
end